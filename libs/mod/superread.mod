IMPLEMENTATION MODULE SuperRead;
(*
  Author  : John Cameron
*)

FROM ASCII IMPORT EOL;
FROM CharSet IMPORT aCHARSet;

PROCEDURE ReadToken( VAR Token: ARRAY OF CHAR; VAR Delim: CHAR; 
                     VAR Finish: BOOLEAN );
(*
  OPERATION:
    Uses the GetChar procedure specified in SetUp to read the next "token".
    A token is defined as a string of characters delimited by any of the
    characters given in Delimiters as specified by SetUp.

    In other words, ReadToken reads characters until it finds a character
    not contained in Delimiters. Then reads all subsequent characters into
    Token until it encounters another delimiter.

    Delim is set to the delimiter encountered. If Finish is TRUE or if we
    finished because the Token string filled up, Delim returns 0C.

    If the end of data is encountered, Finish returns TRUE.
  NOTE:
    Must define Get first. Until you do, ReadToken will always return an
    empty string.
    Delimiters defaults to aCHARSet{ EOL } - ie ReadToken will act like a
    read line.
*)
VAR
  ch: CHAR;
  n:  CARDINAL;
BEGIN
  Token[0] := 0C;

  IF Get = aGetCharProc(NIL) THEN
    RETURN;
  END;

  (*
    Skip leading delimiters
  *)
  REPEAT
    Get(Delim, Finish);
  UNTIL NOT (Delim IN Delimiters) OR Finish;

  IF Finish THEN
    RETURN;
  END;

  (*
    Build Token
  *)
  n := 0;
  REPEAT
    Token[n] := Delim;
    INC(n);
    Get(Delim, Finish);
  UNTIL (Delim IN Delimiters) OR Finish OR (n > HIGH(Token));

  IF n <= HIGH(Token) THEN
    Token[n] := 0C;
  ELSE
    Delim := 0C;
  END;
END ReadToken;

BEGIN
  Get := aGetCharProc(NIL);
  Delimiters := aCHARSet{ EOL };
END SuperRead.
 
