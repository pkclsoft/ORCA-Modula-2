(*$Segment FastFileSystem*)
IMPLEMENTATION MODULE FastFileSystem;

FROM ASCII IMPORT
  nul;
FROM GSOSInterface IMPORT 
  AccessSet, accessAttributes;
FROM MemoryManager IMPORT
  HUnLock, SetHandleSize, HLock, NewHandle, DisposeHandle, AttributeSet, 
  Attribute;
FROM M2Lib IMPORT
  UserID, ToolError, LoWORD, HighWORD, LongWORD;
FROM OrcaShell IMPORT FastFileDCB, FastFile, actSave, actAdd, actPurge, 
  actDelete, actLoad, actLoadFromMemory, actIndexedLoad, bDiskAndMemory, 
  bMayBePurged;
FROM SYSTEM IMPORT
  ADR, ADDRESS, WORD;

IMPORT GSOSInterface;

CONST
  chunkSize         = 8192; (* number of bytes to extend the file by *)

(*FORWARDS*)
PROCEDURE DisposeFile(VAR f: File); FORWARD;
PROCEDURE ExtendFile(VAR f: File); FORWARD;

PROCEDURE Close(VAR f:            File;
                    discardFile:  BOOLEAN;
                    saveToDisk:   BOOLEAN);
(*
  OPERATION:
    This procedure closes the 'file' if it is open.

    If discardFile is TRUE, then it simply disposes of any memory taken up
    by the file.

    If discardFile is FALSE, then saveToDisk is checked.  If TRUE, the memory
    file is written to disk.  Finally, the 'file' is given to FastFile, which
    leaves the file in memory, but removes it from our care.
*)
VAR
  fileSize: LONGINT;
  parms:    FastFileDCB;
BEGIN
  WITH f DO
    IF open AND (mode = writing) THEN
      IF discardFile THEN
        DisposeFile(f);
      ELSE
        (*
          Unlock the block so that FastFile may do with it as it wishes.
        *)
        HUnLock(handle);

        fileSize := VAL(LONGINT, (nextByte.a - handle^));

        WITH parms DO
          pCount := 14;

          IF saveToDisk THEN
            action := actSave;
            flags := {bDiskAndMemory, bMayBePurged};
          ELSE
            action := actAdd;
            flags := { };
          END;

          index := 0;
          filehandle := handle;
          pathName := ADR(nameString.inString);
          access := AccessSet{aaReadEnabled, aaWriteEnabled, aaRenameEnabled,
                               aaDestroyEnabled};
          fileType := 0B1H; (* object file type *)
          auxType := 0;
          storageType := 0;
          createDate := 0;
          createTime := 0;
          modDate := 0;
          modTime := 0;
          optionList := NIL;
          eof := fileSize;
          blocksUsed := 0;
        END;

        FastFile(parms);

        IF (ToolError() = 0) AND saveToDisk THEN
          parms.action := actPurge;

          FastFile(parms);
        END;

        IF ToolError() <> 0 THEN
          res := notdone;
        ELSE
          res := done;
        END;
      END;

      open := FALSE;
    ELSE
      res := notdone;
    END;
  END;
END Close;

PROCEDURE Delete(VAR f: File; filename: ARRAY OF CHAR);
(*
  OPERATION:
    Delete the specified file from the FastFile system.
  NOTE:
    This requires the file to have been loaded first.  The file is _NOT_ deleted
    from the disk.
*)
BEGIN
  WITH f DO
    IF open AND (mode = reading) THEN
      GSOSInterface.StringToGSOSInString(filename, nameString.inString);

      WITH parms DO
        pCount := 5;
        action := actDelete;
        flags := { bDiskAndMemory, bMayBePurged };
        pathName := ADR(nameString.inString);
      END;

      FastFile(parms);

      IF ToolError() = 0 THEN
        res := done;
        open := FALSE;
      END;
    ELSE
      res := notdone;
    END;
  END;
END Delete;

PROCEDURE DisposeFile(VAR f: File);
(*
  OPERATION:
    Dispose of any memory taken up by the 'file'.  This would normaly only
    be done if a compile error had occured.
*)
BEGIN
  WITH f DO
    (*
      Only try to dispose of the memory if we believe the file to be 'open'.
    *)
    IF open AND (mode = writing) THEN
      DisposeHandle(handle);

      open := FALSE;
      res := done;
    ELSE
      res := notdone;
    END;
  END;
END DisposeFile;

PROCEDURE EnsureAvailable(VAR f:    File;
                              len:  CARDINAL);
(*
  OPERATION:
    Ensures that we have at least 'len' bytes available in the file to be
    written to.
*)
VAR
  remaining:  LONGCARD;
BEGIN
  WITH f DO
    IF open AND (mode = writing) THEN
      (*
        We calculate the available space by subtracting the current byte 
        address from the address of the last available byte.
      *)
      remaining := VAL(LONGCARD, (lastByte - nextByte.a));

      (*
        Extend the file even if we have just enough.  I figure that we can save 
        some time by grabbing the extra space now, instead of finding out later
        that we have used up every last byte.
      *)
      IF remaining <= VAL(LONGCARD, len) THEN
        ExtendFile(f);
      ELSE
        res := done;
      END;
    END;
  END;
END EnsureAvailable;

PROCEDURE ExtendFile(VAR f: File);
(*
  OPERATION:
    This procedure extends the 'file' by 'chunkSize' bytes, and updates the 
    file descriptor accordingly.
*)
VAR
  oldBlock: ADDRESS;
  offset:   ADDRESS;
BEGIN
  WITH f DO
    IF open AND (mode = writing) THEN
      (*
        Save the starting address of the old block, in case the MM moves the
        file around.
      *)
      oldBlock := handle^;

      (*
        Unlock the block so that the MM can move it.
      *)
      HUnLock(handle);

      (*
        Increase the block size
      *)
      INC(size, chunkSize);
      SetHandleSize(size, handle);

      IF ToolError() <> 0 THEN
        res := notdone;
      ELSE
        res := done;
      END;

      (*
        Re-lock the block to prevent it moving when we don't want it to.
      *)
      HLock(handle);

      lastByte := handle^ + VAL(ADDRESS, size);

      (*
        Now adjust the various pointers and addresses in case the block had to
        be moved.
      *)
      IF handle^ <> oldBlock THEN
        offset := nextByte.a - oldBlock;
        nextByte.a := handle^ + offset;
      END;
    END;
  END;
END ExtendFile;

PROCEDURE GetPos(VAR f: File; VAR pos: LONGINT);
(*
  OPERATION:
    Return the current offset into the file.
*)
BEGIN
  WITH f DO
    IF open AND (mode = reading) THEN
      pos := currpos;
      res := done;
    ELSE        
      res := notdone;
    END;
  END;
END GetPos;

PROCEDURE ILoad(VAR f: File; index: CARDINAL);
(*
  OPERATION:
    Load the file specified by "index".  If the file is not loaded then
    return a response of notdone.
*)
BEGIN
  WITH f DO
    open := FALSE;
    mode := reading;
    res := notdone;
    eof := FALSE;

    WITH parms DO
      pCount := 14;
      action := actIndexedLoad;
      flags := { };
      pathName := ADR(nameString);
    END;

    FastFile(parms);

    IF ToolError() = 0 THEN
      res := done;
      open := TRUE;
      current.a := parms.filehandle^;
      currpos := 0;
    END;
  END;
END ILoad;

PROCEDURE Load(VAR f: File; filename: ARRAY OF CHAR);
(*
  OPERATION:
    Load the file specified by "filename".  If the file is not loaded then
    return a response of notdone.
*)
BEGIN
  WITH f DO
    open := FALSE;
    mode := reading;
    res := notdone;
    eof := FALSE;
    GSOSInterface.StringToGSOSInString(filename, nameString.inString);

    WITH parms DO
      pCount := 14;
      action := actLoad;
      flags := { bDiskAndMemory, bMayBePurged };
      pathName := ADR(nameString.inString);
    END;

    FastFile(parms);

    IF ToolError() = 0 THEN
      res := done;
      open := TRUE;
      current.a := parms.filehandle^;
      currpos := 0;
    END;
  END;
END Load;

PROCEDURE LookupNew(VAR f:    File;
                        name: ARRAY OF CHAR);
(*
  OPERATION:
    Allocate the initial chunk of memory for the file, and initialise the 
    RefFileID accordingly.
*)
BEGIN
  WITH f DO
    IF name[0] <> nul THEN
      (*
        The block may be virtually anywhere, but we want it locked.  We don't
        mind if it moves, but only at certain times.  At those times, we will
        explicitly unlock it.
      *)
      handle := NewHandle(chunkSize, UserID() + 0100H, 
                          AttributeSet{attrNoSpec, attrLocked}, NIL);

      IF ToolError() = 0 THEN
        open := TRUE;
        mode := writing;
        GSOSInterface.StringToGSOSInString(name, nameString.inString);
        size := chunkSize;
        nextByte.a := handle^;
        lastByte := nextByte.a + VAL(ADDRESS, chunkSize);
        res := done;
      ELSE
        open := FALSE;
        res := notdone;
      END;
    ELSE
      (*
        IF the name is a nul string, then don't bother trying to open the file
        up.
      *)
      open := FALSE;
      res := notdone;
    END;
  END;  
END LookupNew;

PROCEDURE MLoad(VAR f: File; filename: ARRAY OF CHAR);
(*
  OPERATION:
    Load the file specified by "filename".

    This call does not try and locate the file on the disk.  If the file has
    not previously been loaded into memory, then it returns a response of
    notdone;
*)
BEGIN
  WITH f DO
    open := FALSE;
    mode := reading;
    res := notdone;
    eof := FALSE;
    GSOSInterface.StringToGSOSInString(filename, nameString.inString);

    WITH parms DO
      pCount := 14;
      action := actLoadFromMemory;
      flags := { };
      pathName := ADR(nameString.inString);
    END;

    FastFile(parms);

    IF ToolError() = 0 THEN
      res := done;
      open := TRUE;
      current.a := parms.filehandle^;
      currpos := 0;
    END;
  END;
END MLoad;

PROCEDURE Purge(VAR f: File);
(*
  OPERATION:
    Mark the file as purgeable.
*)
BEGIN
  WITH f DO
    IF open AND (mode = reading) THEN
      WITH parms DO
        pCount := 5;
        action := actPurge;
        flags := { bDiskAndMemory, bMayBePurged };
        filehandle := handle;
        pathName := ADR(nameString.inString);
      END;

      FastFile(parms);

      IF ToolError() = 0 THEN
        res := done;
        open := FALSE;
      END;
    ELSE
      res := notdone;
    END;
  END;
END Purge;

PROCEDURE ReadChar(VAR f: File; VAR ch: CHAR);
(*
  OPERATION:
    Read a character from the specified file.
*)
BEGIN
  WITH f DO
    IF open AND (mode = reading) THEN
      IF currpos < parms.eof THEN
        ch := current.c^;
        INC(current.a);
        INC(currpos);
        res := done;
      ELSE
        eof := TRUE;
        res := notdone;
        ch := nul;
      END;
    ELSE
      res := notdone;
      ch := nul;
    END;          
  END;
END ReadChar;

PROCEDURE ReadWord(VAR f: File; VAR w: WORD);
(*
  OPERATION:
    Read a word from the specified file.
*)
BEGIN
  WITH f DO
    IF open AND (mode = reading) THEN
      IF currpos < parms.eof THEN
        w := current.w^;
        INC(current.a, 2);
        INC(currpos, 2);
        res := done;
      ELSE
        eof := TRUE;
        res := notdone;
        w := 0;
      END;
    ELSE
      res := notdone;
      w := 0;
    END;          
  END;
END ReadWord;

PROCEDURE SetPos(VAR f: File; pos: LONGINT);
(*
  OPERATION:
    Set the current offset into the file.
*)
BEGIN
  WITH f DO
    IF open AND (mode = reading) THEN
      IF pos < parms.eof THEN
        current.a := parms.filehandle^ + VAL(ADDRESS, pos);
        currpos := pos;
        eof := FALSE;
        res := done;
      ELSE
        res := notdone;
      END;
    ELSE
      res := notdone;
    END;
  END;
END SetPos;

PROCEDURE WriteChar(VAR f:    File;
                        char: CHAR);
(*
  OPERATION:
    Insert one character into the file.
*)
BEGIN
  WITH f DO
    IF open AND (mode = writing) THEN
      EnsureAvailable(f, 1);

      IF res = done THEN
        nextByte.c^ := char;
        INC(nextByte.a);
      END;
    END;
  END;
END WriteChar;

PROCEDURE WriteWord(VAR f:    File;
                        word: WORD);
(*
  OPERATION:
    Insert two bytes into the file.
*)
VAR
  value:  CARDINAL;
BEGIN
  WITH f DO
    IF open AND (mode = writing) THEN
      value := VAL(CARDINAL, word);
      WriteChar(f, VAL(CHAR, value MOD 256));
      WriteChar(f, VAL(CHAR, value DIV 256));
    END;
  END;
END WriteWord;

END FastFileSystem.
