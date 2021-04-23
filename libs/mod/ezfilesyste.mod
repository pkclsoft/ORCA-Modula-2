IMPLEMENTATION MODULE EZFileSystem;
(*
  This module provides a number of convience type procedures that simplify 
  utility type access to files.  It also supplies more extensive support for 
  GS/OS while trying to maintain the access mechanism defined by this module.

  WARNING:
    Use of any procedures within this module will prevent your code from 
    being portable.
*)

FROM ASCII IMPORT
  nul;
FROM FileSystem IMPORT
  File, Response;
FROM M2Lib IMPORT
  ToolError;
FROM Strings IMPORT
  Assign, Length;
FROM SYSTEM IMPORT 
  ADR, ADDRESS, TSIZE;

IMPORT GSOSInterface;

PROCEDURE FileExists(file: ARRAY OF CHAR): BOOLEAN;
(*
  OPERATION:
    Returns TRUE if the specified file exists.
*)
VAR
  info:       GSOSInterface.getFileInfoDCB;
  expand:     GSOSInterface.expandPathDCB;
  name:       GSOSInterface.GSOSInString;
  fixedName:  GSOSInterface.GSOSOutString;
BEGIN
  GSOSInterface.StringToGSOSInString(file, name);

  fixedName.inLength := TSIZE(GSOSInterface.GSOSNameString);

  WITH expand DO
    pCount := 2;
    inputPath := ADR(name);
    outputPath := ADR(fixedName);
  END;

  GSOSInterface.ExpandPath(expand);

  WITH info DO
    pCount := 2;
    pathName := ADR(fixedName.inString);
  END;

  GSOSInterface.GetFileInfo(info);

  RETURN ToolError() = 0;
END FileExists;

PROCEDURE FilePosition(VAR f: File): LONGINT;
(*
  OPERATION:
    This function procedure provides a simpler mechanism to obtaining the 
    current position with a file.  Instead of returning two cardinals, a
    single longint is returned which is easier to manipulate.
*)
VAR
  getMarkParms: GSOSInterface.getMarkDCB;
  fpos:         LONGINT;
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
        fpos := 0;
      ELSE
        res := done;

        WITH getMarkParms DO
          fpos := position;
        END;
      END;
    ELSE
      res := notdone;
      fpos := 0;
    END;
  END;

  RETURN fpos;
END FilePosition;

PROCEDURE GetFileType(    file: ARRAY OF CHAR;
                      VAR type: CARDINAL;
                      VAR aux:  LONGINT;
                      VAR OK:   BOOLEAN);
(*
  OPERATION:
    Obtain and return the filetype and auxtype of the specified file.

    If the file does not exist, then OK is returned as FALSE.
*)
VAR
  info: GSOSInterface.getFileInfoDCB;
  name: GSOSInterface.GSOSInString;
BEGIN
  GSOSInterface.StringToGSOSInString(file, name);

  WITH info DO
    pCount := 4;
    pathName := ADR(name);
  END;

  GSOSInterface.GetFileInfo(info);

  IF ToolError() = 0 THEN
    type := info.fileType;
    aux := info.auxType;
    OK := TRUE;
  ELSE
    OK := FALSE;
  END;
END GetFileType;

PROCEDURE MakeFileName(    filename:  ARRAY OF CHAR;
                       VAR FName:     ARRAY OF CHAR;
                           ext:       ARRAY OF CHAR);
(*
  OPERATION:
    This procedure generates a filename of the form:

      filename.ext

    It ensures that the path is kept, and that the first part of the filename
    (not the extension) is no more than 8 characters in length.

  NOTE:
    It assumes that FName is big enough to hold the resulting filename.
*)
VAR
  dot:    CARDINAL;
  colon:  CARDINAL;
  start:  CARDINAL;
  len:    CARDINAL;
  extlen: CARDINAL;
  maxlen: CARDINAL;
BEGIN
  extlen := Length(ext);
  len := Length(filename);
  dot := 0;
  colon := 0;
  start := len - 1;

  WHILE (start > 0) AND (colon = 0) DO
    IF (colon = 0) AND ((filename[start] = '/') OR
                        (filename[start] = ':')) THEN
      colon := start;
    END;

    IF (dot = 0) AND (filename[start] = '.') THEN
      dot := start;
    END;

    DEC(start);
  END;

  IF dot = 0 THEN
    dot := len - 1;
  ELSE
    DEC(dot);
  END;

  IF colon = 0 THEN
    len := dot + 1;
  ELSE
    len := dot - colon;
  END;

  IF extlen > 5 THEN (* 5 = "." + up to four characters, eg. ".ROOT" *)
    extlen := 5;
  END;

  (*
    maxlen is the max number of characters in the 'name' part of the filename,
    i.e. everything in the name before the dot.
  *)
  maxlen := 15 - extlen;

  (*
    If the name part is too long to allow it AND the extension, then truncate
    the name part so that we can fit the entire extension in.
  *)
  IF len > maxlen THEN
    IF colon = 0 THEN
      dot := maxlen - 1;
    ELSE
      dot := colon + maxlen;
    END;
  END;

  Assign(filename, FName);

  start := 0;

  WHILE start < extlen DO
    INC(dot);
    FName[dot] := ext[start];
    INC(start);
  END;

  IF dot < HIGH(FName) THEN
    FName[dot+1] := nul;
  END;
END MakeFileName;

PROCEDURE ReadNBytes(VAR f:       File; 
                         n:       LONGINT;
                         buffer:  ADDRESS; 
                     VAR read:    LONGINT);
(*
  OPERATION:
    Read "n" bytes from the file "f" into the buffer with address "buffer".

    The result "read" reports the actual number of bytes read.
*)
VAR
  readParms:  GSOSInterface.readWriteDCB;
BEGIN
  WITH f DO
    IF open THEN
      WITH readParms DO
        pCount := 4;
        refNum := fileRefNum;
        dataBuffer := buffer;
        requestCount := n;
      END;

      GSOSInterface.Read(readParms);

      eof := (ToolError() = 004CH);

      IF readParms.transferCount # n THEN
        res := notdone;
      ELSE
        res := done;
      END;

      read := readParms.transferCount;
    ELSE
      res := notdone;
      read := 0;
    END;
  END;
END ReadNBytes;

PROCEDURE SetFileType(    file: ARRAY OF CHAR;
                          type: CARDINAL;
                          aux:  LONGINT;
                      VAR OK:   BOOLEAN);
(*
  OPERATION:
    Set the filetype and auxtype of the specified file.

    If the file does not exist, then OK is returned as FALSE.
*)
VAR
  ginfo:  GSOSInterface.getFileInfoDCB;
  sinfo:  GSOSInterface.setFileInfoDCB;
  name:   GSOSInterface.GSOSInString;
BEGIN
  GSOSInterface.StringToGSOSInString(file, name);

  WITH ginfo DO
    pCount := 2;
    pathName := ADR(name);
  END;

  GSOSInterface.GetFileInfo(ginfo);

  IF ToolError() = 0 THEN
    WITH sinfo DO
      pCount := 4;
      pathName := ADR(name);
      access := ginfo.access;
      fileType := type;
      auxType := aux;
    END;

    GSOSInterface.SetFileInfo(sinfo);

    IF ToolError() = 0 THEN
      OK := TRUE;
    ELSE
      OK := FALSE;
    END;
  ELSE
    OK := FALSE;
  END;
END SetFileType;

PROCEDURE WriteNBytes(VAR f:       File; 
                          n:       LONGINT;
                          buffer:  ADDRESS; 
                      VAR written: LONGINT);
(*
  OPERATION:
    Write "n" bytes to the file "f" from the buffer with address "buffer".

    The result "written" reports the actual number of bytes written.
*)
VAR
  writeParms:  GSOSInterface.readWriteDCB;
BEGIN
  WITH f DO
    IF open THEN
      WITH writeParms DO
        pCount := 4;
        refNum := fileRefNum;
        dataBuffer := buffer;
        requestCount := n;
      END;

      GSOSInterface.Write(writeParms);

      IF ToolError() # 0 THEN
        res := notdone;
      ELSE
        res := done;
      END;

      written := writeParms.transferCount;
    ELSE
      res := notdone;
      written := 0;
    END;
  END;
END WriteNBytes;

END EZFileSystem.
