(*$Segment GSOSInterface*)
IMPLEMENTATION MODULE GSOSInterface;

FROM ASCII IMPORT
  nul;
FROM Strings IMPORT
  Assign, Copy, Length;
FROM SYSTEM IMPORT
  TSIZE;

(*
  The following routines are found in the implementation module.  They are
  basically support routines to make life easier when using GSOS under
  Modula-2.
*)

PROCEDURE StringToGSOSInString(VAR string:    ARRAY OF CHAR;
                               VAR pathname:  GSOSInString);
(*
  OPERATION:
    Will convert a standard Modula-2 string to a GS/OS Input String.

    Note: "string" is passed VAR for efficiency reasons only.  It is not
    changed.
*)
BEGIN
  WITH pathname DO
    length := Length(string);
    Assign(string, text);
  END;
END StringToGSOSInString;

PROCEDURE StringToGSOSOutString(VAR string:   ARRAY OF CHAR;
                                VAR pathname:  GSOSOutString);
(*
  OPERATION:
    Will convert a standard Modula-2 string to a GS/OS Output String.

    Note: "string" is passed VAR for efficiency reasons only.  It is not
    changed.
*)
BEGIN
  WITH pathname DO
    inLength := TSIZE(GSOSNameString);
    StringToGSOSInString(string, inString);
  END;
END StringToGSOSOutString;

PROCEDURE GSOSInStringToString(VAR pathname:  GSOSInString;
                               VAR string:    ARRAY OF CHAR);
(*
  OPERATION:
    Will convert a GS/OS Input String to a standard Modula-2 string.

    Note: "pathname" is passed VAR for efficiency reasons only.  It is not
    changed.
*)
BEGIN
  WITH pathname DO
    IF length < GSOSNameLength THEN
      text[length] := nul;
    END;

    Copy(text, 0, length, string);
  END;
END GSOSInStringToString;

PROCEDURE GSOSOutStringToString(VAR pathname:  GSOSOutString;
                                VAR string:    ARRAY OF CHAR);
(*
  OPERATION:
    Will convert a GS/OS Output String to a standard Modula-2 string.

    Note: "pathname" is passed VAR for efficiency reasons only.  It is not
    changed.
*)
BEGIN
  WITH pathname DO
    GSOSInStringToString(inString, string);
  END;
END GSOSOutStringToString;

END GSOSInterface.
