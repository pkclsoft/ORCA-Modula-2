IMPLEMENTATION MODULE EZCommandLine;

FROM ASCII IMPORT
  ht, nul;
FROM M2Lib IMPORT
  CommandLine;
FROM OrcaShell IMPORT
  StopDCB, Stop;
FROM Strings IMPORT
  Pos, Copy, Delete, Length, Assign;
FROM SYSTEM IMPORT
  ADR;

CONST
  space = 40C;
  tab   = ht;

PROCEDURE deleteSwitch(VAR line:    ARRAY OF CHAR;
                           switch:  ARRAY OF CHAR);
(*
  OPERATION:
    Deletes the specified switch from the command line.
*)
BEGIN
  deleteWord(line, Pos(switch, line));
  trimString(line);
END deleteSwitch;

PROCEDURE deleteWord(VAR line:  ARRAY OF CHAR;
                         start: CARDINAL);
(*
  OPERATION:
    Delete's the Word begiining at the character specified by 'start' from
    the input line.
*)
VAR
  dummyWord: ARRAY [0..255] OF CHAR;
BEGIN
  getWord(line, start, dummyWord);
END deleteWord;

PROCEDURE deTabString(VAR value: ARRAY OF CHAR);
(*
  OPERATION:
    Will replace any tab characters in the specified string with spaces.
*)
VAR
  index:  CARDINAL;
  len:    CARDINAL;
BEGIN
  len := Length(value);

  IF len > 0 THEN
    DEC(len);

    FOR index := 0 TO len DO
      IF value[index] = tab THEN
        value[index] := space;
      END;
    END;
  END;
END deTabString;

PROCEDURE getCommandLine(VAR line: ARRAY OF CHAR);
(*
  OPERATION:
    Takes a copy of the shell command line, as supplied at startup in the X & Y
    registers.
*)
BEGIN
  IF CommandLine <> NIL THEN
    Assign(CommandLine^.value, line);
  ELSE
    line[0] := nul;
  END;
END getCommandLine;

PROCEDURE getParameters(VAR line: ARRAY OF CHAR);
(*
  OPERATION:
    Takes a copy of the shell command line, as supplied at startup in the X & Y
    registers. It then deletes the first word which (normally) is the actual
    command.  What is left, is the parameters.
*)
BEGIN
  IF CommandLine <> NIL THEN
    Assign(CommandLine^.value, line);
    deleteWord(line, 0);
  ELSE
    line[0] := nul;
  END;
END getParameters;

PROCEDURE getSwitchValue(VAR line:    ARRAY OF CHAR;
                             switch:  ARRAY OF CHAR;
                         VAR value:   ARRAY OF CHAR);
(*
  OPERATION:
    For a switch that also has a parameter, this procedure will return
    the value of the parameter, and will then delete the switch and it's
    parameter from the command line.
*)
VAR
  switchPosition: CARDINAL;
BEGIN
  switchPosition := Pos(switch, line);
  deleteSwitch(line, switch);
  getWord(line, switchPosition, value);
END getSwitchValue;

PROCEDURE getWord(VAR line:   ARRAY OF CHAR;
                      start:  CARDINAL;
                  VAR value:  ARRAY OF CHAR);
(*
  OPERATION:
    Will extract and return the Word in the input line beginning at
    the character pointed to by 'start'.
*)
VAR
  count:  CARDINAL;
  len:    CARDINAL;
BEGIN
  IF Length(line) > 0 THEN
    count := start;

    WHILE (line[count] = space) AND
          (Length(line) > 0) DO
      Delete(line, count, 1);
    END;

    len := Length(line);

    WHILE (count < len) AND
          (line[count] <> space) DO
      INC(count);
    END;

    count := count - start;
    Copy(line, start, count, value);
    Delete(line, start, count);
    trimString(line);
  ELSE
    value := '';
  END;
END getWord;

PROCEDURE switchPresent(VAR line:   ARRAY OF CHAR;
                            switch: ARRAY OF CHAR): BOOLEAN;
(*
  OPERATION:
    Returns a boolean value indicating whether the specified switch
    is present in the command line.
*)
BEGIN
  IF Pos(switch, line) <= HIGH(line) THEN
    RETURN TRUE
  ELSE
    RETURN FALSE;
  END;
END switchPresent;

PROCEDURE trimString(VAR value: ARRAY OF CHAR);
(*
  OPERATION:
    Will delete all leading and trailing spaces from the supplied string.
*)
BEGIN
  IF Length(value) <> 0 THEN
    deTabString(value);

    WHILE (value[0] = space) DO
      Delete(value, 0, 1);
    END;

    WHILE (value[Length(value)-1] = space) DO
      Delete(value, Length(value)-1, 1);
    END;
  END;
END trimString;

PROCEDURE UserAborted(): BOOLEAN;
(*
  OPERATION:
    Does a shell call to determine whether the user has pressed
    command-period, and returns true if so.
*)
VAR
  stopParms:  StopDCB;
BEGIN
  Stop(stopParms);

  IF stopParms.stopFlag THEN
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END UserAborted;

END EZCommandLine.
