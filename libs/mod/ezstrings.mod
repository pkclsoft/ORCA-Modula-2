IMPLEMENTATION MODULE EZStrings;
(*
  Author  : John Cameron
*)

FROM CharSet IMPORT aCHARSet;
FROM NumberConversion IMPORT LongIntToString, StringToCard, StringToLongInt;
FROM Strings IMPORT Concat, Delete, Insert, Length;

PROCEDURE StringToRightWithFill( VAR s, a: ARRAY OF CHAR; Filler: CHAR ); FORWARD;

PROCEDURE AllSpaces( VAR A: ARRAY OF CHAR ): BOOLEAN;
(*
  OPERATION:
    Returns TRUE if the given string contains only spaces
  NOTE:
    This is not the same as a NUL string.
*)
VAR
  i: CARDINAL;
BEGIN
  FOR i := 0 TO HIGH(A) DO
    IF A[i] <> ' ' THEN
      RETURN FALSE;
    END;
  END;

  RETURN TRUE;
END AllSpaces;

PROCEDURE ArrayToString( a: ARRAY OF CHAR; VAR s: ARRAY OF CHAR );
(*
  OPERATION:
    Takes array in "a" and copies it into "s". The difference is that trailing
    blanks in "a" are not copied to "s". "s" terminates with a null
    character (0C).
  NOTE:
    "s" and "a" may be the same.

    "s" must be at least as big as "a".
*)
VAR
  i, j: INTEGER;
BEGIN
  i := HIGH(a);

  WHILE (i >= 0 ) AND (a[i] = ' ') DO
    DEC(i);
  END;

  IF i >= 0 THEN
    FOR j := 0 TO i DO
      s[j] := a[j];
    END;
  END;

  INC(i);
  IF i <= VAL(INTEGER, HIGH(s)) THEN
    s[i] := 0C;
  END;
END ArrayToString;

PROCEDURE ConvertCard( String: ARRAY OF CHAR; VAR n: CARDINAL );
(*
  OPERATION:
    Uses NumberConversion.StringToCard to convert string to a CARDINAL.

    In addition, interprets nul string as 0. (StringToCard crashes)

    Also HALTS in case of bad conversion.
*)
VAR
  OK: BOOLEAN;
BEGIN
  IF String[0] = 0C THEN
    n := 0;
  ELSE
    StringToCard( String, n, OK );

    IF NOT OK THEN
      HALT;
    END;
  END;
END ConvertCard;

PROCEDURE ConvertLongInt( String: ARRAY OF CHAR; VAR n: LONGINT );
(*
  OPERATION:
    Uses NumberConversion.StringToLongInt to convert string to a LONGINT.

    In addition, interprets nul string as 0. (StringToLongInt crashes)

    Also HALTS in case of bad conversion.
*)
VAR
  OK: BOOLEAN;
BEGIN
  IF String[0] = 0C THEN
    n := VAL(LONGINT, 0);
  ELSE
    StringToLongInt( String, n, OK );

    IF NOT OK THEN
      HALT;
    END;
  END;
END ConvertLongInt;

PROCEDURE DeleteLeadingSpaces( VAR String: ARRAY OF CHAR );
(*
  OPERATION:
    Left Justifies the string. Trailing 0c.
*)
VAR
  StrPtr:       CARDINAL;
BEGIN
  StrPtr := 0;

  (* Find first non space *)
  WHILE (StrPtr <= HIGH(String)) AND (String[StrPtr] = ' ') DO
    INC(StrPtr);
  END; (* WHILE space *)

  Delete( String, 0, StrPtr);
END DeleteLeadingSpaces;



VAR
  (*
    Factor is used in PROCEDURE LongIntToShortString.

    Factor[n] contains the info you need in order to reduce the number
    of displayable characters by n.
    For example, to represent the number 12345 in 3 characters, you need
    to save 2 characters somehow.
    Factor[2] will tell you to divide the number by 1000 (divisor) and
    display the result followed by 'T' (suffix).
    This gives 12T: the best representation of 12345 in 3 characters.
  *)
  Factor:
    ARRAY[0..7] OF 
      RECORD
        divisor:  LONGINT;
        suffix:   ARRAY [0..2] OF CHAR;
      END;

PROCEDURE LongIntToShortString( num: LONGINT; VAR Str: ARRAY OF CHAR; 
                                width: CARDINAL );
(*
  OPERATION:
    Convert the number in num to an ASCII representation filling the first
    "width" characters of the array Str.
    If the number is too big to fit unchanged into Str, it is abbreviated
    using H, T and M to stand for Hundred, Thousand and Million.
    For example, 12345 displayed in a 3 character Str would be represented
    as 12T.
  NOTE:
    This procedure is table driven, using the global pseudo-constant array, 
    Factor (see declaration above and initialization at end of this module).

    This procedure will not work properly if num is negative. It will not
    crash however.

    If the number cannot be represented even using abbreviations, Str
    is filled with ?'s.
*)
VAR
  digits, extra, i, l: CARDINAL;
  copy: LONGINT;
BEGIN
  copy := num;

  (*
    Work out how many digits there are in num
  *)
  digits := 0;

  WHILE copy <> VAL(LONGINT, 0) DO
    INC(digits);
    copy := copy DIV VAL(LONGINT, 10);
  END;

  (*
    How many extra digits are there over what we have room for in Str
  *)
  IF width > digits THEN
    extra := 0;
  ELSE
    extra := digits - width;

    IF extra > HIGH(Factor) THEN
      extra := HIGH(Factor);
    END;
  END;

  (*
    Special case for coding whole numbers of thousands
  *)
  IF (extra = 1) AND (num MOD VAL(LONGINT, 1000) = VAL(LONGINT, 0)) THEN
    extra := 2;
  END;

  WITH Factor[extra] DO
    l := Length(suffix);
    copy := num DIV divisor;

    IF (width <= l) OR 
       ((copy = VAL(LONGINT, 0)) AND (digits > 0)) THEN
      FOR i := 0 TO width-1 DO
        Str[i] := '?';
      END;

      IF width <= HIGH(Str) THEN
        Str[width] := 0C;
      END;
    ELSE
      LongIntToString( copy, Str, width-l );
      Concat( Str, suffix, Str );
    END;
  END;
END LongIntToShortString;

PROCEDURE NumericStringToArray( s: ARRAY OF CHAR; VAR a: ARRAY OF CHAR );
(*
  OPERATION:
    Takes string in "s" and copies it into "a". The difference is that "a" is
    right justified filled with '0's. 
    "s" may be terminated with a null character (0C).
  NOTE:
    "s" and "a" may be the same.

    If "a" cannot hold all the data in "s", it truncates on the right.
*)
BEGIN
  StringToRightWithFill( s, a, '0' );
END NumericStringToArray;

PROCEDURE StringToLeftArray( s: ARRAY OF CHAR; VAR a: ARRAY OF CHAR );
(*
  OPERATION:
    Takes string in "s" and copies it into "a". The difference is that "a" is
    left justified filled with blanks. 
    "s" may be terminated with a null character (0C).
  NOTE:
    "s" and "a" may be the same.

    If "a" cannot hold all the data in "s", it truncates on the right.
*)
VAR
  i:        CARDINAL;
  sLength:  CARDINAL;
BEGIN
  sLength := Length(s);

  IF sLength > HIGH(a)+1 THEN
    sLength := HIGH(a) + 1;
  END;

  IF sLength > 0 THEN
    FOR i := 0 TO sLength-1 DO
      a[i] := s[i];
    END;
  END;

  FOR i := sLength TO HIGH(a) DO
    a[i] := ' ';
  END;
END StringToLeftArray;

PROCEDURE ShortStringToLongInt( Str: ARRAY OF CHAR; VAR Num: LONGINT;
                                VAR Done: BOOLEAN );
(*
  OPERATION:
    Converts ASCII string coded with H, T, M standing for hundreds, thousands
    and millions to a long integer.
    The allowed characters are '0'..'9', 'H', 'T', 'M'. Detection of any
    illegal character will cause Done to be set FALSE and Num to 0.
*)
VAR
  Ch: CHAR;
  Factor: LONGINT;
  i: CARDINAL;
BEGIN
  Done := TRUE;
  i := 0;
  Num := VAL(LONGINT, 0);

  WHILE (i <= HIGH(Str)) AND (Str[i] <> 0C) DO
    Ch := Str[i];

    IF Ch IN aCHARSet{'0'..'9'} THEN
      Num := Num*VAL(LONGINT, 10) + VAL(LONGINT, ORD(Ch) - ORD('0'));
    ELSE
      CASE Ch OF
        'H':
          Factor := VAL(LONGINT, 100);
        |
        'T':
          Factor := VAL(LONGINT, 1000);
        |
        'M':
          Factor := VAL(LONGINT, 1000000);
        ELSE
          Done := FALSE;
      END;
      Num := Num * Factor;
    END;

    INC(i);
  END;

  IF NOT Done THEN
    Num := VAL(LONGINT, 0);
  END;
END ShortStringToLongInt;

PROCEDURE SpacedAppend( VAR Dest: ARRAY OF CHAR; AppendStr: ARRAY OF CHAR );
(*
  OPERATION:
    Dest := Dest + ' ' + AppendStr
*)
BEGIN
  Concat( Dest, ' ', Dest );
  Concat( Dest, AppendStr, Dest );
END SpacedAppend;

PROCEDURE StringToRightArray( s: ARRAY OF CHAR; VAR a: ARRAY OF CHAR );
(*
  OPERATION:
    Takes string in "s" and copies it into "a". The difference is that "a" is
    right justified filled with blanks. 
    "s" may be terminated with a null character (0C).
  NOTE:
    "s" and "a" may be the same.

    If "a" cannot hold all the data in "s", it truncates on the right.
*)
BEGIN
  StringToRightWithFill( s, a, ' ' );
END StringToRightArray;

PROCEDURE StringToRightWithFill( VAR s, a: ARRAY OF CHAR; Filler: CHAR );
(*
  OPERATION:
    Takes string in "s" and copies it into "a". The difference is that "a" is
    right justified filled with the character specified by Filler. 
    "s" may be terminated with a null character (0C).
    All spaces in "s" are replaced in "a" by Filler.
  NOTE:
    If "a" cannot hold all the data in "s", it truncates on the right.
*)
VAR
  ch:           CHAR;
  i:            CARDINAL;
  NonSpaceFill: BOOLEAN;
  Start:        CARDINAL;
  sLength:      CARDINAL;
BEGIN
  sLength := Length(s);

  IF sLength > HIGH(a)+1 THEN
    sLength := HIGH(a) + 1;
  END;

  Start := HIGH(a) + 1;
  Start := Start - sLength;

  IF Start > 0 THEN
    FOR i := 0 TO Start-1 DO
      a[i] := Filler;
    END;
  END;

  NonSpaceFill := Filler <> ' ';

  FOR i := Start TO HIGH(a) DO
    ch := s[i-Start];

    IF NonSpaceFill AND (ch = ' ') THEN
      ch := Filler;
    END;

    a[i] := ch;
  END;
END StringToRightWithFill;

BEGIN
  WITH Factor[0] DO
    divisor := VAL(LONGINT, 1);
    suffix := '';
  END;

  WITH Factor[1] DO
    divisor := VAL(LONGINT, 100);
    suffix := 'H';
  END;

  WITH Factor[2] DO
    divisor := VAL(LONGINT, 1000);
    suffix := 'T';
  END;

  WITH Factor[3] DO
    divisor := VAL(LONGINT, 100000);
    suffix := 'HT';
  END;

  WITH Factor[4] DO
    divisor := VAL(LONGINT, 1000000);
    suffix := 'M';
  END;

  WITH Factor[5] DO
    divisor := VAL(LONGINT, 1000000);
    suffix := 'M';
  END;

  WITH Factor[6] DO
    divisor := VAL(LONGINT, 100000000);
    suffix := 'HM';
  END;

  WITH Factor[7] DO
    divisor := VAL(LONGINT, 1000000000);
    suffix := 'TM';
  END;
END EZStrings.
