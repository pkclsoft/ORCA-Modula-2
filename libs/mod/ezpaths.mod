(*$Keep 'EZPaths'*)
IMPLEMENTATION MODULE EZPaths;
(*
  This module supplies a number of routines that you can use to manipulate
  (forgive me) MS-DOS style paths.

  You can specify a path string by referring to an environment variable, or
  by explicitly supplying a string.

  A path is a string of characters that represent a variable number of 
  directories (each defined by a full pathname), separated by a double colon
  ("::").  The double colon is used instead of a semi-colon in order to 
  maintain compatability with HFS.

  NOTE:
    The directory walker, ".." is not recognised by this module.  It is
    assumed that a path contains only valid GSOS pathnames.

  NOTE:
    Do not mix separators within any one entry within a path.

  NOTE:
    No individual pathname may end in a separator.  eg. Use "8", not "8/".
*)

FROM EZFileSystem IMPORT FileExists;
FROM GSOSInterface IMPORT StringToGSOSInString, StringToGSOSOutString,
  GSOSNameLength, GSOSInString, GSOSOutString, GSOSNameString;
FROM OrcaShell IMPORT ReadVariableDCB, ReadVariable;
FROM Strings IMPORT Copy, Concat, Delete, Assign, Pos, Length;
FROM SYSTEM IMPORT ADR, TSIZE;


PROCEDURE FindFileInPath(VAR filename:  ARRAY OF CHAR;
                         VAR path:      ARRAY OF CHAR;
                             isEnvVar:  BOOLEAN;
                         VAR pathname:  ARRAY OF CHAR;
                         VAR OK:        BOOLEAN);
(*
  OPERATION:
    Attempts to locate the file specified by "filename" within one of the
    directories specified by "path".

    If "isEnvVar" is TRUE, then "path" is actually the name of an environment
    variable containing the path.

    If found, then the full pathname of the file is returned in "pathname",
    and OK is returned TRUE;

    If found, then OK is returned FALSE, and "pathname" is undefined.

    NOTE:
      If you are using this procedure in an application that does not 
      necessarily run under a shell such as Orca or Gnome, then don't
      call it with isEnvVar TRUE.  This prevents any shell calls being
      made.

    NOTE:
      Both "filename" and "path" are passed as VAR parameters for efficiency.
      This procedure DOES NOT alter them in any way.
*)
VAR
  thePath:      GSOSOutString;
  name:         GSOSInString;
  readVarParms: ReadVariableDCB;
  curName:      ARRAY [0..GSOSNameLength] OF CHAR;
  found:        BOOLEAN;
  pos:          CARDINAL;
BEGIN
  OK := TRUE;

  IF isEnvVar THEN
    (*
      Get the value of the specified variable.
    *)
    StringToGSOSInString(path, name);

    thePath.inLength := TSIZE(GSOSNameString);

    WITH readVarParms DO
      pCount := 3;
      varName := ADR(name);
      value := ADR(thePath);
    END;

    ReadVariable(readVarParms);

    WITH thePath DO
      IF outLength = 0 THEN
        (*
          Variable not found.
        *)
        OK := FALSE;
      ELSE
        (*
          Variable found.  Make sure that the value is terminated with a nul
          character for normal M2 string operations.
        *)
        IF outLength <= HIGH(text) THEN
          text[outLength] := 0C;
        END;
      END;
    END;
  ELSE
    (*
      So that the following code can use a common source for the path, copy
      the path to "thePath".
    *)
    StringToGSOSOutString(path, thePath);
  END;

  IF OK THEN
    found := FALSE;

    REPEAT
      (*
        If there is no path left to check, then the file doesn't exist within
        the path.
      *)
      IF Length(thePath.text) = 0 THEN
        OK := FALSE;
      ELSE
        (*
          Find a delimiter.
        *)
        pos := Pos("::", thePath.text);

        IF pos > HIGH(thePath.text) THEN
          (*
            No delimiter, so use the remains of the path for one final 
            attempt.
          *)
          Assign(thePath.text, curName);
          thePath.text[0] := 0C;
        ELSE
          (*
            Copy the next section of the path to "curName", and then remove
            it from the path.
          *)
          Copy(thePath.text, 0, pos, curName);
          Delete(thePath.text, 0, pos+2);
        END;

        (*
          Terminate the name with a separator before adding the filename.

          If the pathname already contains a colon as a separator, then use
          another colon to terminate it.  If it doesn't contain a colon,
          then use a slash (which will be converted by ExpandPath in
          FileExists).  The story here is that for ExpandPath to work,
          we don't want a mix of slashes and colons as separators.
        *)
        IF Pos(':', curName) <= HIGH(curName) THEN
          Concat(curName, ':', curName);
        ELSE
          Concat(curName, '/', curName);
        END;

        (*
          Add the filename.
        *)
        Concat(curName, filename, curName);

        (*
          Check for the files presence.
        *)
        found := FileExists(curName);
      END;

      (*
        Keep going until we either find the file, or there is no path left.
      *)
    UNTIL found OR NOT OK;

    (*
      Only set up the pathname if we have found the file.
    *)
    IF found THEN
      Assign(curName, pathname);
    END;
  END;
END FindFileInPath;

END EZPaths.
