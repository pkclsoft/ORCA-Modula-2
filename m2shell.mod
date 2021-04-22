(*$Segment M2Shell*)
(*$RangeCheck+*)
(*$OverflowCheck+*)
IMPLEMENTATION MODULE M2Shell;
(*
  This module provides an interface to the ORCA shell for the compiler.

  Use procedures in this module to obtain the command line parameters.
*)

FROM ASCII IMPORT
  nul;
FROM EZFileSystem IMPORT
  FileExists, MakeFileName;
FROM InOut IMPORT
  Write, WriteCard, WriteHex, WriteString, WriteLn, WriteLongInt;
FROM GSOSInterface IMPORT
  GSOSOutString, GSOSOutStringToString, StringToGSOSInString, GSOSNameLength,
  GSOSNameString;
FROM M2Lib IMPORT 
  ToolError;
FROM M2LM IMPORT
  aSymbol, IdToSymbol, ChainToName, OpenObjectFile;
FROM M2SM IMPORT
  Directives, aDirective, firstError;
FROM OrcaShell IMPORT
  GetLInfoDCB, GetLInfo, SetLInfoDCB, SetLInfo, aSwitch, bCompile, aKeepStyle;
FROM Strings IMPORT
  Assign, Pos, Concat;
FROM SYSTEM IMPORT
  ADR, BYTE;

VAR
  inParms:          GetLInfoDCB;
  outParms:         SetLInfoDCB;

  noFilesToCompile: BOOLEAN;

  nextExtension:    CARDINAL;
  nextExtension2:   CARDINAL;

PROCEDURE GetFileName(VAR name: ARRAY OF CHAR);
VAR
  i:  CARDINAL;
  ch: CHAR;
BEGIN
  IF noFilesToCompile THEN
    name[0] := nul;
  ELSE
    IF SourceFile.outLength <> 0 THEN
      GSOSOutStringToString(SourceFile, name);
      SourceFile.outLength := 0;
    ELSE
      name[0] := nul;
    END;

    nextExtension := 0;
    nextExtension2 := 0;
  END;
END GetFileName;

PROCEDURE InitM2Shell;
(*
  OPERATION:
    This procedure will use the shell call "GetLInfo" to obtain the name of
    the file to be compiled.  It also sets up a number of globals that 
    describe the compiling environment:

      For example:

      . Do we compile to a disk file or a memory file?
      . Whether to generate debug code.
      . Whether to invoke the editor when an error is detected.
      . Whether to list the source.
      . Whether to produce a symbol table.
*)
VAR
  options:  ARRAY [0..16] OF CHAR;
  name:     ARRAY [0..507] OF CHAR;
  flag:       aSwitch;
BEGIN
  WITH inParms DO
    pCount := 11;
    sFile := ADR(SourceFile);
    dFile := ADR(OutputFile);
    parms := ADR(NamesList);
    iString := ADR(Switches);
  END;

  GetLInfo(inParms);

  WITH inParms DO
    DebugCode := sD IN pFlags;

    SaveToDisk := (sM IN mFlags) OR
                  (NOT (sM IN mFlags) AND NOT (sM IN pFlags));
      
    ListSource := sL IN pFlags;
    InvokeEdit := sE IN pFlags;
    ShowProgress := (sP IN pFlags) OR
                    (NOT (sP IN mFlags) AND NOT (sP IN pFlags));
    ShowProcs := ShowProgress;
    AllErrFatal := sT IN pFlags;
    PauseOnError := sW IN pFlags;

    keepStyle := kFlag;
 
    IF keepStyle = kDontSave THEN
      outFileMask := 'xxx';
      KeepObject := FALSE;
    ELSE
      GSOSOutStringToString(OutputFile, outFileMask);

      KeepObject := outFileMask[0] <> nul;
    END;
  END;

  GSOSOutStringToString(Switches, options);

  IF (Pos("-P", options) <= HIGH(options)) OR
     (Pos("-p", options) <= HIGH(options)) THEN
    ShowProcs := FALSE;
  END;

  IF (Pos("-K", options) <= HIGH(options)) OR
     (Pos("-k", options) <= HIGH(options)) THEN
    DisplayKey := TRUE;
  END;

  IF (Pos("-D", options) <= HIGH(options)) OR
     (Pos("-d", options) <= HIGH(options)) THEN
    debugMode := TRUE;
  END;

  IF (Pos("-8", options) <= HIGH(options)) THEN
    COP8asCOP5 := TRUE;
  END;

  IF debugMode THEN
    WITH inParms DO
      WriteString('pCount = <');
      WriteCard(pCount, 5);
      WriteString('>');
      WriteLn;

      WriteString('sFile.inLength = <'); 
      WriteCard(sFile^.inLength, 5);
      WriteString('>');
      WriteLn;

      WriteString('sFile.outLength = <'); 
      WriteCard(sFile^.outLength, 5);
      WriteString('>');
      WriteLn;

      WriteString('sFile.text = <');
      WriteString(sFile^.text);
      WriteString('>');
      WriteLn;

      WriteString('dFile.inLength = <'); 
      WriteCard(dFile^.inLength, 5);
      WriteString('>');
      WriteLn;

      WriteString('dFile.outLength = <'); 
      WriteCard(dFile^.outLength, 5);
      WriteString('>');
      WriteLn;

      WriteString('dFile.text = <');
      WriteString(dFile^.text);
      WriteString('>');
      WriteLn;

      WriteString('parms.inLength = <'); 
      WriteCard(parms^.inLength, 5);
      WriteString('>');
      WriteLn;

      WriteString('parms.outLength = <'); 
      WriteCard(parms^.outLength, 5);
      WriteString('>');
      WriteLn;

      WriteString('parms.text = <');
      WriteString(parms^.text);
      WriteString('>');
      WriteLn;

      WriteString('iString.inLength = <'); 
      WriteCard(iString^.inLength, 5);
      WriteString('>');
      WriteLn;

      WriteString('iString.outLength = <'); 
      WriteCard(iString^.outLength, 5);
      WriteString('>');
      WriteLn;

      WriteString('iString.text = <');
      WriteString(iString^.text);
      WriteString('>');
      WriteLn;

      WriteString('merr = <');
      WriteHex(VAL(CARDINAL, merr), 2);
      WriteString('H>');
      WriteLn;

      WriteString('merrf = <');
      WriteHex(VAL(CARDINAL, merrf), 2);
      WriteString('H>');
      WriteLn;

      WriteString('lops = <');
      WriteHex(VAL(CARDINAL, lops), 2);
      WriteString('H>');
      WriteLn;

      WriteString('kFlag = <');
      WriteHex(VAL(CARDINAL, kFlag), 2);
      WriteString('H>');
      WriteLn;

      WriteString('          ');
      FOR flag := sA TO sZ BY -1 DO
        Write(CHR((32 - ORD(flag)) + 64));
      END;
      WriteLn;

      WriteString('mFlags = <');
      FOR flag := sA TO sZ BY -1 DO
        IF flag IN mFlags THEN
          Write('*');
        ELSE
          Write('.');
        END;
      END;
      WriteString('>');
      WriteLn;

      WriteString('          ');
      FOR flag := sA TO sZ BY -1 DO
        Write(CHR((32 - ORD(flag)) + 64));
      END;
      WriteLn;

      WriteString('pFlags = <');
      FOR flag := sA TO sZ BY -1 DO
        IF flag IN pFlags THEN
          Write('*');
        ELSE
          Write('.');
        END;
      END;
      WriteString('>');
      WriteLn;

      WriteString('origin = <');
      WriteLongInt(origin, 10);
      WriteString('>');
      WriteLn;
    END;

    IF KeepObject THEN
      WriteString('KeepObject = TRUE');
    ELSE
      WriteString('KeepObject = FALSE');
    END;

    WriteLn;

    WriteString('outFileMask = <');
    WriteString(outFileMask);
    WriteString('>');
    WriteLn;
  END;
END InitM2Shell;

PROCEDURE OpenNextObjectFile(isMainModule: BOOLEAN);
(*
  OPERATION:
    Given the keep style supplied in the command line info record, open an
    object file for output.  For more detail, see page 411 or the ORCA/M
    manual.
*)
VAR
  ext:    ARRAY [0..5] OF CHAR;
  Found:  BOOLEAN;
BEGIN
  objectFileInError := FALSE;

  IF keepStyle = kDontSave THEN
    currentObjectName[0] := nul;
  ELSIF keepStyle = kSaveObject THEN
    IF (nextExtension = 0) AND isMainModule THEN
      OpenObjectFile(outFileMask, '.ROOT');
      MakeFileName(outFileMask, currentObjectName, '.ROOT');
    ELSE
      OpenObjectFile(outFileMask, '.A');
      MakeFileName(outFileMask, currentObjectName, '.A');
    END;

    INC(nextExtension);
  ELSIF keepStyle = kRootExists THEN
    IF nextExtension = 0 THEN
      OpenObjectFile(outFileMask, '.A');
      MakeFileName(outFileMask, currentObjectName, '.A');
    ELSE
      OpenObjectFile(outFileMask, '.B');
      MakeFileName(outFileMask, currentObjectName, '.B');
    END;

    INC(nextExtension);
  ELSIF keepStyle = kSearchSuffix THEN
    Found := FALSE;

    REPEAT
      INC(nextExtension);

      IF nextExtension = 27 THEN
        nextExtension := 1;
        INC(nextExtension2);
      END;

      ext[0] := '.';

      IF nextExtension2 = 0 THEN
        ext[1] := CHR(64 + nextExtension);
        ext[2] := nul;
      ELSE
        ext[1] := CHR(64 + nextExtension2);
        ext[2] := CHR(64 + nextExtension);
        ext[3] := nul;
      END;

      MakeFileName(outFileMask, currentObjectName, ext);

      Found := NOT FileExists(currentObjectName);
    UNTIL Found OR (nextExtension2 = 27);

    IF Found THEN
      OpenObjectFile(outFileMask, ext);
    ELSE
      objectFileInError := TRUE;
    END
  END;
END OpenNextObjectFile;

PROCEDURE ShutdownM2Shell(successfull: BOOLEAN; name: ARRAY OF CHAR);
(*
  OPERATION:
    This procedure sets up and executes a "SetLInfo" shell call in order to
    inform the shell as to what the compiler has achieved.

    The "successfull" parameter indicates whether the compile succeeded 
    without errors or not, and is used in setting up the "SetLInfo" call.
*)
VAR
  editNames:  ARRAY [0..GSOSNameLength-1] OF CHAR;
  chainFile:  aSymbol;
  flag:       aSwitch;
BEGIN
  WITH outParms DO
    pCount := 11;

    SourceFile := OutputFile;

    sFile := ADR(SourceFile.inString);  (* Pass the same values back *)
    dFile := ADR(OutputFile.inString);
    parms := ADR(NamesList.inString);
    iString := ADR(Switches.inString);

    merr := inParms.merr;

    IF successfull THEN
      merrf := VAL(BYTE, 0);

      lops := inParms.lops;

      IF ChainTo IN Directives THEN
        (*
          The code had a "ChainTo" directive, specifying a file to be compiled
          after the last file.
        *)
        IdToSymbol(ChainToName, chainFile);
        StringToGSOSInString(chainFile, SourceFile.inString);

        IF inParms.kFlag <> kDontSave THEN
          MakeFileName(chainFile, editNames, '');
          StringToGSOSInString(editNames, OutputFile.inString);
        ELSE
          editNames := '';
          StringToGSOSInString(editNames, OutputFile.inString);
        END;
      ELSE
        EXCL(flags, bCompile);
      END;

      origin := 0;
    ELSE
      IF InvokeEdit AND terminalError THEN
        merrf := VAL(BYTE, 128); (* invoke the editor *)
        lops := VAL(BYTE, 0);
        origin := firstError.position;

        StringToGSOSInString(name, SourceFile.inString);
        StringToGSOSInString(firstError.string, NamesList.inString);
      ELSE
        merrf := VAL(BYTE, 127); (* we don't want to invoke the editor *)
        lops := VAL(BYTE, 0);
        origin := 0;
      END;
    END;

    IF keepStyle = kDontSave THEN
      kFlag := kDontSave;
    ELSE
      kFlag := kSearchSuffix;
    END;

    mFlags := inParms.mFlags;
    pFlags := inParms.pFlags;
  END;

  SetLInfo(outParms);

  IF debugMode THEN
    IF ToolError() = 0 THEN
      WriteString('SetLInfo passed');
    ELSE
      WriteString('SetLInfo failed');
    END;
    WriteLn;

    WITH outParms DO
      WriteString('pCount = <');
      WriteCard(pCount, 5);
      WriteString('>');
      WriteLn;

      WriteString('sFile.length = <'); 
      WriteCard(sFile^.length, 5);
      WriteString('>');
      WriteLn;

      WriteString('sFile.text = <');
      WriteString(sFile^.text);
      WriteString('>');
      WriteLn;

      WriteString('dFile.length = <'); 
      WriteCard(dFile^.length, 5);
      WriteString('>');
      WriteLn;

      WriteString('dFile.text = <');
      WriteString(dFile^.text);
      WriteString('>');
      WriteLn;

      WriteString('parms.length = <'); 
      WriteCard(parms^.length, 5);
      WriteString('>');
      WriteLn;

      WriteString('parms.text = <');
      WriteString(parms^.text);
      WriteString('>');
      WriteLn;

      WriteString('iString.length = <'); 
      WriteCard(iString^.length, 5);
      WriteString('>');
      WriteLn;

      WriteString('iString.text = <');
      WriteString(iString^.text);
      WriteString('>');
      WriteLn;

      WriteString('merr = <');
      WriteHex(VAL(CARDINAL, merr), 2);
      WriteString('H>');
      WriteLn;

      WriteString('merrf = <');
      WriteHex(VAL(CARDINAL, merrf), 2);
      WriteString('H>');
      WriteLn;

      WriteString('lops = <');
      WriteHex(VAL(CARDINAL, lops), 2);
      WriteString('H>');
      WriteLn;

      WriteString('kFlag = <');
      WriteHex(VAL(CARDINAL, kFlag), 2);
      WriteString('H>');
      WriteLn;

      WriteString('          ');
      FOR flag := sA TO sZ BY -1 DO
        Write(CHR((32 - ORD(flag)) + 64));
      END;
      WriteLn;

      WriteString('mFlags = <');
      FOR flag := sA TO sZ BY -1 DO
        IF flag IN mFlags THEN
          Write('*');
        ELSE
          Write('.');
        END;
      END;
      WriteString('>');
      WriteLn;

      WriteString('          ');
      FOR flag := sA TO sZ BY -1 DO
        Write(CHR((32 - ORD(flag)) + 64));
      END;
      WriteLn;

      WriteString('pFlags = <');
      FOR flag := sA TO sZ BY -1 DO
        IF flag IN pFlags THEN
          Write('*');
        ELSE
          Write('.');
        END;
      END;
      WriteString('>');
      WriteLn;

      WriteString('origin = <');
      WriteLongInt(origin, 10);
      WriteString('>');
      WriteLn;
    END;
  END;
END ShutdownM2Shell;

BEGIN
  DebugCode := FALSE;
  SaveToDisk := TRUE;
  ListSource := FALSE;
  InvokeEdit := FALSE;
  ShowProgress := FALSE;
  ShowProcs := TRUE;
  DisplayKey := FALSE;
  AllErrFatal := FALSE;
  PauseOnError := FALSE;
  noFilesToCompile := FALSE;
  KeepObject := FALSE;
  debugMode := FALSE;
  COP8asCOP5 := FALSE;

  SourceFile.inLength := SIZE(GSOSOutString);
  OutputFile.inLength := SIZE(GSOSOutString);
  NamesList.inLength := SIZE(GSOSOutString);
  Switches.inLength := SIZE(GSOSOutString);

  outFileMask := 'xxx';
  userHasAborted := FALSE;
  terminalError := FALSE;
  objectFileInError := FALSE;
  nextExtension := 0;
  nextExtension2 := 0;
END M2Shell.
