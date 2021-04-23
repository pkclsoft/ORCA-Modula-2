(*$Segment EZDump*)
IMPLEMENTATION MODULE EZDump;

(*
  This module provides simple methods of dumping the memory used by the
  currently running application.
*)

FROM EZFileSystem IMPORT
  ReadNBytes, WriteNBytes, FilePosition;
FROM FileSystem IMPORT
  Lookup, WriteWord, Close, File, WriteChar, ReadWord, Response;
FROM GSOSInterface IMPORT
  GSOSNameString, GSOSOutString, getNameDCB, GetName, GSOSOutStringToString;
FROM InOut IMPORT
  WriteString, WriteHex, WriteLn;
FROM Loader IMPORT 
  GetLoadSegInfo, MemSegTableEntry;
FROM MemoryManager IMPORT
  Handle, GetHandleSize;
FROM M2Lib IMPORT
  StackTop, StackBottom, aTerminateStatus, ToolError, UserID, Display,
  CrashStackFrame, MasterID, TermProc, aTerminationProcedure;
FROM Storage IMPORT
  ALLOCATE, DEALLOCATE;
FROM Strings IMPORT
  Insert, Concat, Length;
FROM SYSTEM IMPORT
  ADDRESS, ADR, TSIZE;

TYPE
  (*
    aBlockIndex is supplied purely as a description of the format of the
    block index in the file.  The format of the index at time of generation
    is that of a linked list.
  *)
  aBlockIndex =
    RECORD
      NumOfBlocks:  CARDINAL;
      blocks:       ARRAY [0..1] OF aBlock;
    END;

  adrToCard =
    RECORD
      CASE :CARDINAL OF
          0:  a: ADDRESS;
        | 1:  l: LONGINT;
        | 2:  lo, hi: CARDINAL;
      END;
    END;

VAR
  BlockList:    pBlockListEntry;
  BlockListLen: CARDINAL;
  ChainToProc:  aTerminationProcedure;

PROCEDURE DisposeBlocks(VAR list: pBlockListEntry); FORWARD;
PROCEDURE ReadBlockList(VAR dumpFile: File; VAR OK: BOOLEAN); FORWARD;

PROCEDURE BuildListToFileInfo(dumpFile: File);
(*
  OPERATION:
    This procedure will scan through the BlockList, and calculate the file
    offsets for each block that represents data.
*)
VAR
  block:  pBlockListEntry;
  fpos:   LONGINT;
BEGIN
  (*
    Determine the file offset of the first data block.

    FilePosition returns the current file mark, telling us how many bytes
    have already been written to the file.  
    
    BlockListLen is the number of blocks in the BlockList.  Since we are 
    going to write out the BlockList BEFORE the data blocks themselves, we 
    must take that into account.
    
    The final 2 is the word value of BlockListLen which must be written
    before the BlockList so that we know how many entries to read back into
    memory when analysing the dump.
  *)
  fpos := FilePosition(dumpFile) + VAL(LONGINT, (BlockListLen * SIZE(aBlock))) +
          VAL(LONGINT, 2);

  block := BlockList;

  WHILE block <> NIL DO
    WITH block^.info DO
      IF type = data THEN
        position := fpos;
        fpos := fpos + length;
      END;
    END;

    block := block^.next;
  END;
END BuildListToFileInfo;

PROCEDURE ClosePMD(VAR dumpFile: File);
(*
  OPERATION:
    Closes the dump file, and deallocates any memory taken up by the dump data.
*)
BEGIN
  Close(dumpFile);

  WITH dumpHeader DO
    DEALLOCATE(dumpStack, StackTop - StackBottom);
  END;

  DisposeBlocks(dumpBlockList);
END ClosePMD;

PROCEDURE DisplayReason(reason: aTerminateStatus);
(*
  OPERATION:
    Displays on the screen, the reason for the termination.
*)
BEGIN
  WriteString('Termination reason: ');

  CASE reason OF
    tsOK:
      WriteString('None');
    |
    tsOverflow:
      WriteString('Overflow');
    |
    tsSubrangeExceeded:
      WriteString('Subrange exceeded');
    |
    tsOutOfMemory:
      WriteString('Out of memory');
    |
    tsUndefinedCase:
      WriteString('Undefined case label');
    |
    tsDivideByZero:
      WriteString('Divide by zero');
    |
    tsRangeError:
      WriteString('Range error');
    |
    tsHalt:
      WriteString('Program HALTed');
    |
    tsNoReturnFromFunction:
      WriteString('No RETURN from function');
    |
    tsStackOverflow:
      WriteString('Stack Overflow');
    |
    tsUnexpectedCoRoutineExit:
      WriteString('Unexpected end of Co-Routine');
    |
    tsAddressOverflow:
      WriteString('Address Overflow / NIL Pointer Dereference');
  END;

  WriteString('.');
  WriteLn;
END DisplayReason;

PROCEDURE DisposeBlocks(VAR list: pBlockListEntry);
VAR
  block, current: pBlockListEntry;
BEGIN
  block := list;

  WHILE block <> NIL DO
    current := block;
    block := block^.next;

    DISPOSE(current);
  END;
END DisposeBlocks;

PROCEDURE GetNewBlock(VAR list, b: pBlockListEntry; VAR len: CARDINAL);
(*
  OPERATION:
    This procedure is used to allocate a new BlockListEntry.  It returns a
    a pointer to the new entry which has already been added to the block
    list.
*)
BEGIN
  IF list = NIL THEN
    NEW(list);
    b := list;
    b^.next := NIL;
    len := 1;
  ELSE
    b := list;

    WHILE b^.next <> NIL DO
      b := b^.next;
    END;

    NEW(b^.next);

    b := b^.next;
    b^.next := NIL;
    INC(len);
  END;
END GetNewBlock;

PROCEDURE ObtainDynamicDataInfo;
(*
  OPERATION:
    This procedure will add to the block list, all blocks of memory that have
    been allocated by our user id (NOT the master id).
*)
VAR
  handle: Handle;
BEGIN
END ObtainDynamicDataInfo;

PROCEDURE ObtainLoaderInfo;
(*
  OPERATION:
    Uses GetLoadSegInfo to form a list of memory blocks that are related to
    load segments.  The list is later used to add details of any blocks of
    memory that have been allocated dynamically by the application, so until
    that is done, the list is not written to the dump file.
*)
VAR
  ac:       adrToCard;
  fileNum:  CARDINAL;
  segNum:   CARDINAL;
  error:    CARDINAL;
  buffer:   MemSegTableEntry;
  count:    CARDINAL;
  block:    pBlockListEntry;
  kind:     CARDINAL;

BEGIN
  count := 0;
  fileNum := 1;
  error := 0;

  WHILE error = 0 DO
    segNum := 1;

    WHILE error = 0 DO
      GetLoadSegInfo(MasterID, fileNum, segNum, ADR(buffer));

      IF ToolError() = 0 THEN
        INC(count);

        GetNewBlock(BlockList, block, BlockListLen);

        WITH block^ DO
          next := NIL;

          WITH info DO
            address := buffer.handle^;
            length := GetHandleSize(buffer.handle);

            kind := buffer.segKind MOD 0FFH;

            CASE kind OF
                000H:  type := code;
              | 001H:  type := data;
              | 012H:  type := stack;
              ELSE     type := other;
            END;

            segment := buffer.loadSeg;
            file := buffer.loadFile;
            position := 0;
          END;
        END;

(*
        WriteString('File $');
        WriteHex(buffer.loadFile, 2);
        WriteString(' Segment $');
        WriteHex(buffer.loadSeg, 2);
        WriteString(' Address $');
        ac.a := buffer.handle^;
        WriteHex(ac.lo, 4);
        WriteHex(ac.hi, 2);
        WriteString(' Size $');
        ac.l := block^.info.length;
        WriteHex(ac.lo, 4);
        WriteHex(ac.hi, 2);
        WriteString(' Kind $');
        WriteHex(buffer.segKind, 4);
        WriteLn;
*)

        INC(segNum);
      ELSE
        error := 1;
      END;
    END;

    IF (segNum > 1) OR (error = 0) THEN
      error := 0;
      INC(fileNum);
    END;
  END;
END ObtainLoaderInfo;

PROCEDURE OpenPMD(VAR dumpFile: File; name: ARRAY OF CHAR; VAR OK: BOOLEAN);
(*
  OPERATION:
    Reads the informational component of the dump file into memory for
    analysis.
*)
VAR
  ac:         adrToCard;
  read:       LONGINT;
  stackSize:  CARDINAL;
BEGIN
  Lookup(dumpFile, name, FALSE);

  IF dumpFile.res = done THEN
    WriteString('Reading PMD: <');
    WriteString(name);
    WriteString('>');
    WriteLn;

    ReadNBytes(dumpFile, SIZE(dumpHeader), ADR(dumpHeader), read);

    IF read = VAL(LONGINT, SIZE(dumpHeader)) THEN
      WITH dumpHeader DO
        stackSize := StackTop - StackBottom;

        ALLOCATE(dumpStack, stackSize);

        ReadNBytes(dumpFile, stackSize, dumpStack, read);
      END;

      IF read = VAL(LONGINT, stackSize) THEN
        ReadBlockList(dumpFile, OK);
      ELSE
        OK := FALSE;
      END;
    ELSE
      OK := FALSE;
    END;

    IF NOT OK THEN
      Close(dumpFile);

      WriteString('PMD Corrupt: <');
      WriteString(name);
      WriteString('>');
      WriteLn;
    END;
  ELSE
    WriteString('Unable to open PMD: <');
    WriteString(name);
    WriteString('>');
    WriteLn;

    OK := FALSE;
  END;
END OpenPMD;

PROCEDURE ReadBlockList(VAR dumpFile: File; VAR OK: BOOLEAN);
VAR
  block:          aBlock;
  blockPtr:       pBlockListEntry;
  read:           LONGINT;
  expectedBlocks: CARDINAL;
BEGIN
  IF dumpFile.open THEN
    dumpBlockListLen := 0;

    (*
      Read the number of blocks that should be present in the file.
    *)
    ReadWord(dumpFile, expectedBlocks);

    (*
      Get the first block
    *)
    ReadNBytes(dumpFile, SIZE(aBlock), ADR(block), read);

    (*
      Now, while we have succeeded in reading a block, add it to the list
    *)
    WHILE (read = VAL(LONGINT, SIZE(aBlock))) AND
          (dumpBlockListLen < expectedBlocks) DO
      GetNewBlock(dumpBlockList, blockPtr, dumpBlockListLen);
      blockPtr^.info := block;

      ReadNBytes(dumpFile, SIZE(aBlock), ADR(block), read);
    END;

    (*
      The number of blocks read should match the expected count.
    *)
    OK := dumpBlockListLen = expectedBlocks;
  END;
END ReadBlockList;

PROCEDURE WriteDataToFile(VAR dumpFile: File);
(*
  OPERATION:
    This procedure searches through the BlockList for any blocks that have a
    type of "Data".  Whenever one is found, the memory block described by 
    that entry is written to the dump file.
*)
VAR
  block:    pBlockListEntry;
  written:  LONGINT;
BEGIN
  block := BlockList;

  WHILE block <> NIL DO
    WITH block^ DO
      IF info.type = data THEN
        WriteNBytes(dumpFile, info.length, info.address, written);
      END;
    END;

    block := block^.next;
  END;
END WriteDataToFile;

PROCEDURE WriteListToFile(VAR dumpFile: File);
(*
  OPERATION:
    This procedure writes the entire contents of the BlockList to the dump
    file.
*)
VAR
  block:    pBlockListEntry;
  written:  LONGINT;
BEGIN
  WriteWord(dumpFile, BlockListLen);

  block := BlockList;

  WHILE block <> NIL DO
    WriteNBytes(dumpFile, SIZE(aBlock), ADR(block^.info), written);

    block := block^.next;
  END;
END WriteListToFile;

PROCEDURE WritePMD(TermAddr: ADDRESS; reason: aTerminateStatus);
(*
  OPERATION:
    Dumps the contents of the current stack to disk in a filename of the
    form "xxx.SDP", where xxx is up to 11 characters of the applications
    filename.
*)
VAR
  ac:         adrToCard;
  appName:    GSOSOutString;
  nameParms:  getNameDCB;
  filename:   ARRAY [0..36] OF CHAR;
  written:    LONGINT;
  dumpFile:   File;
BEGIN
  WriteString('Writing Post mortem dump...');
  WriteLn;

  appName.inLength := TSIZE(GSOSNameString);

  WITH nameParms DO
    pCount := 1;
    dataBuffer := ADR(appName);
  END;

  GetName(nameParms);

  IF ToolError() = 0 THEN
    GSOSOutStringToString(appName, filename);
    Insert("9:", filename, 0);

    IF Length(filename) > 11 THEN
      filename[11] := 0C;
    END;

    Concat(filename, ".SDP", filename);

    Lookup(dumpFile, filename, TRUE);

    WriteNBytes(dumpFile, 4, ADR(TermAddr), written);

    ac.a := TermAddr;
(*
    WriteString('Termination address: ');
    WriteHex(ac.hi, 2);
    WriteHex(ac.lo, 4);
    WriteLn;
*)
    WriteWord(dumpFile, StackTop);
    WriteWord(dumpFile, StackBottom);
    WriteChar(dumpFile, VAL(CHAR, reason));
    WriteWord(dumpFile, CrashStackFrame);
    WriteNBytes(dumpFile, SIZE(Display), ADR(Display), written);

    ac.lo := StackBottom;
    ac.hi := 0;

    WriteNBytes(dumpFile, (StackTop - StackBottom), ac.a, written);

    ObtainLoaderInfo;
    ObtainDynamicDataInfo;
    BuildListToFileInfo(dumpFile);
    WriteListToFile(dumpFile);
    WriteDataToFile(dumpFile);

    Close(dumpFile);
    DisposeBlocks(BlockList);
  END;

  DisplayReason(reason);

  IF ChainToProc <> VAL(aTerminationProcedure, NIL) THEN
    ChainToProc(TermAddr, reason);
  END;
END WritePMD;

BEGIN
  BlockList := NIL;
  BlockListLen := 0;
  dumpStack := NIL;
  ChainToProc := TermProc;
  TermProc := WritePMD;
END EZDump.
