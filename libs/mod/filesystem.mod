(*$Segment FileSystem*)
IMPLEMENTATION MODULE FileSystem;

FROM ASCII IMPORT
  nul;
FROM GSOSInterface IMPORT
  accessAttributes, AccessSet;
FROM M2Lib IMPORT
  ToolError, LoWORD, HighWORD, LongWORD;
FROM SYSTEM IMPORT
  ADR, ADDRESS, WORD;

IMPORT GSOSInterface;

PROCEDURE Lookup(VAR f: File; filename: ARRAY OF CHAR; new: BOOLEAN);
(*
  OPERATION:
    Open the file specified by "filename".  If "new" is TRUE, and the file
    does not exist, then create the file before opening it.  IF "new" is
    FALSE, and no file exists, then return a response of notdone.
*)
  PROCEDURE openFile;
  VAR
    openParms:  GSOSInterface.openDCB;
  BEGIN
    WITH openParms DO
      pCount := 2;
      pathName := ADR(f.nameString);
    END;

    GSOSInterface.Open(openParms);

    IF ToolError() # 0 THEN
      f.res := notdone;
    ELSE
      WITH f DO
        open := TRUE;
        res := done;
        fileRefNum := openParms.refNum;
      END;
    END;
  END openFile;

  PROCEDURE createFile;
  VAR
    createParms:  GSOSInterface.createDCB;
  BEGIN
    WITH createParms DO
      pCount := 3;
      pathName := ADR(f.nameString);
      access := AccessSet{aaReadEnabled, aaWriteEnabled, aaRenameEnabled,
                           aaDestroyEnabled};
      fileType := 00B0H;  (* SRC filetype *)
    END;

    GSOSInterface.Create(createParms);

    IF ToolError() # 0 THEN
      f.res := notdone;
    ELSE
      f.res := done;
    END;
  END createFile;

BEGIN
  WITH f DO
    open := FALSE;
    res := notdone;
    eof := FALSE;
    GSOSInterface.StringToGSOSInString(filename, nameString);

    openFile;

    IF new THEN
      IF res = notdone THEN
        createFile;

        IF res = done THEN
          openFile;
        END;
      END;
    END;
  END;
END Lookup;

PROCEDURE Close(VAR f: File);
(*
  OPERATION:
    Close the specified file.
*)
VAR
  closeParms: GSOSInterface.closeDCB;
BEGIN
  WITH f DO
    IF open THEN
      WITH closeParms DO
        pCount := 1;
        refNum := fileRefNum;
      END;
      
      GSOSInterface.Close(closeParms);

      IF ToolError() # 0 THEN
        res := notdone;
      ELSE
        open := FALSE;
        res := done;
      END;
    ELSE
      res := notdone;  (* file wasn't open! *)
    END;
  END;
END Close;

PROCEDURE Delete(VAR f: File);
(*
  OPERATION:
    Delete the specified file.
  NOTE:
    This requires the file to have been opened first.  To avoid this necessity,
    refer to the module EZFileSystem.
*)
VAR
  destroyParms: GSOSInterface.destroyDCB;
BEGIN
  WITH f DO
    IF open THEN
      Close(f);

      IF res = done THEN
        WITH destroyParms DO
          pCount := 1;
          pathName := ADR(nameString);
        END;

        GSOSInterface.Destroy(destroyParms);

        IF ToolError() # 0 THEN
          res := notdone;
        ELSE
          res := done;
        END;
      ELSE
        (* unable to close the file for some reason... *)
      END;
    ELSE
      res := notdone;  (* file wasn't open! *)
    END;
  END;
END Delete;

PROCEDURE Rename(VAR f: File; filename: ARRAY OF CHAR);
(*
  OPERATION:
    Rename the specified file to "filename".
  NOTE:
    This requires the file to have been opened first.  To avoid this necessity,
    refer to the module EZFileSystem.
*)
VAR
  changePathParms:  GSOSInterface.changePathDCB;
  newPathname:      GSOSInterface.GSOSInString;
  saveLoPos:        CARDINAL;
  saveHiPos:        CARDINAL;
BEGIN
  WITH f DO
    IF open THEN
      GetPos(f, saveHiPos, saveLoPos);

      IF res = done THEN
        Close(f);
      END;

      IF res = done THEN
        GSOSInterface.StringToGSOSInString(filename, newPathname);

        WITH changePathParms DO
          pCount := 2;
          pathName := ADR(f.nameString);
          newPathName := ADR(newPathname);
        END;

        GSOSInterface.ChangePath(changePathParms);

        IF ToolError() # 0 THEN
          res := notdone;
        ELSE
          res := done;

          (*
            We must re-open the file, and set the file-position as if nothing
            had happened.
          *)
          Lookup(f, filename, FALSE);
          SetPos(f, saveHiPos, saveLoPos);
        END;
      ELSE
        (* unable to close the file for some reason... *)
        res := notdone;
      END;
    ELSE
      res := notdone;  (* file wasn't open! *)
    END;
  END;
END Rename;

PROCEDURE SetPos(VAR f: File; highpos, lowpos: CARDINAL);
(*
  OPERATION:
    Set the current position in the specified file.
  NOTE:
    For a simpler method of setting the position (i.e. with a longint)
    refer to the module EZFileSystem.
*)
VAR
  setMarkParms: GSOSInterface.setMarkDCB;
  newMark:      LONGINT;
BEGIN
  WITH f DO
    IF open THEN
      newMark := LongWORD(highpos, lowpos);

      WITH setMarkParms DO
        pCount := 3;
        refNum := fileRefNum;
        base := 0;
        displacement := newMark;
      END;

      GSOSInterface.SetMark(setMarkParms);

      IF ToolError() # 0 THEN
        res := notdone;
      ELSE
        res := done;
        eof := FALSE;
      END;
    ELSE
      res := notdone;
    END;
  END;
END SetPos;

PROCEDURE GetPos(VAR f: File; VAR highpos, lowpos: CARDINAL);
(*
  OPERATION:
    Get the current position in the specified file.
  NOTE:
    For a simpler method of getting the position (i.e. with a longint)
    refer to the module EZFileSystem.
*)
VAR
  getMarkParms: GSOSInterface.getMarkDCB;
BEGIN
  WITH f DO
    IF open THEN
      WITH getMarkParms DO
        pCount := 2;
        refNum := fileRefNum;
      END;

      GSOSInterface.GetMark(getMarkParms);

      IF ToolError() # 0 THEN
        res := notdone;
        lowpos := 0;
        highpos := 0;
      ELSE
        res := done;

        WITH getMarkParms DO
          lowpos := LoWORD(position);
          highpos := HighWORD(position);
        END;
      END;
    ELSE
      res := notdone;
      lowpos := 0;
      highpos := 0;
    END;
  END;
END GetPos;

PROCEDURE Length(VAR f: File; VAR highpos, lowpos: CARDINAL);
(*
  OPERATION:
    Get the length (in bytes) of the specified file.
  NOTE:
    For a simpler method of getting the file length (i.e. with a longint)
    refer to the module EZFileSystem.
*)
VAR
  getEOFParms: GSOSInterface.eofDCB;
BEGIN
  WITH f DO
    IF open THEN
      WITH getEOFParms DO
        pCount := 2;
        refNum := fileRefNum;
      END;

      GSOSInterface.GetEOF(getEOFParms);

      IF ToolError() # 0 THEN
        f.res := notdone;
        lowpos := 0;
        highpos := 0;
      ELSE
        f.res := done;

        WITH getEOFParms DO
          lowpos := LoWORD(eof);
          highpos := HighWORD(eof);
        END;
      END;
    ELSE
      f.res := notdone;
      lowpos := 0;
      highpos := 0;
    END;
  END;
END Length;

PROCEDURE ReadWord(VAR f: File; VAR w: WORD);
(*
  OPERATION:
    Read two bytes from the specified file, storing them as a WORD in "w".
*)
VAR
  readParms:  GSOSInterface.readWriteDCB;
BEGIN
  WITH f DO
    IF open THEN
      WITH readParms DO
        pCount := 4;
        refNum := fileRefNum;
        dataBuffer := ADR(w);
        requestCount := SIZE(w);
      END;

      GSOSInterface.Read(readParms);

      eof := (ToolError() = 004CH);

      IF readParms.transferCount # VAL(LONGINT, SIZE(w)) THEN
        res := notdone;
        w := 0;
      ELSE
        res := done;
      END;
    ELSE
      w := 0;
      res := notdone;
    END;
  END;
END ReadWord;

PROCEDURE WriteWord(VAR f: File; w: WORD);
(*
  OPERATION:
    Write a WORD to the specified file.  They are written LSB, MSB.
*)
VAR
  writeParms:  GSOSInterface.readWriteDCB;
BEGIN
  WITH f DO
    IF open THEN
      WITH writeParms DO
        pCount := 4;
        refNum := fileRefNum;
        dataBuffer := ADR(w);
        requestCount := SIZE(w);
      END;

      GSOSInterface.Write(writeParms);

      IF ToolError() # 0 THEN
        res := notdone;
      ELSE
        res := done;
      END;
    ELSE
      res := notdone;
    END;
  END;
END WriteWord;

PROCEDURE ReadChar(VAR f: File; VAR ch: CHAR);
(*
  OPERATION:
    Read a character from the specified file.
*)
VAR
  readParms:  GSOSInterface.readWriteDCB;
BEGIN
  WITH f DO
    IF open THEN
      WITH readParms DO
        pCount := 4;
        refNum := fileRefNum;
        dataBuffer := ADR(ch);
        requestCount := SIZE(ch);
      END;

      GSOSInterface.Read(readParms);

      eof := (ToolError() = 004CH);

      IF readParms.transferCount # VAL(LONGINT, SIZE(ch)) THEN
        res := notdone;
        ch := nul;
      ELSE
        res := done;
      END;
    ELSE
      res := notdone;
      ch := nul;
    END;
  END;
END ReadChar;

PROCEDURE WriteChar(VAR f: File; ch: CHAR);
(*
  OPERATION:
    Write a character to the specified file.
*)
VAR
  writeParms:  GSOSInterface.readWriteDCB;
BEGIN
  WITH f DO
    IF open THEN
      WITH writeParms DO
        pCount := 4;
        refNum := fileRefNum;
        dataBuffer := ADR(ch);
        requestCount := SIZE(ch);
      END;

      GSOSInterface.Write(writeParms);

      IF ToolError() # 0 THEN
        res := notdone;
      ELSE
        res := done;
      END;
    ELSE
      res := notdone;
    END;
  END;
END WriteChar;

(*
  For more extended access to Modula-2 files, see the module FileUtility,
  which supplies a number of convience type procedures that simplify utility
  type access to files.  It also supplies more extensive support for GS/OS 
  while trying to maintain the access mechanism defined by this module.
*)

END FileSystem.
