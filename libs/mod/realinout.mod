(*$Segment RealInOut*)
IMPLEMENTATION MODULE RealInOut;

  FROM InOut IMPORT
    termCh, Read, ReadString, Write, WriteString, WriteLn, WriteHex;

  FROM RealConversions IMPORT
    LongRealToString, StringToLongReal;

  FROM M2Lib IMPORT
    FormatLongReal;

  FROM ASCII IMPORT
    esc, can;

  CONST
    stringLength = 30;

  PROCEDURE ReadLongReal(VAR r: LONGREAL);
    VAR
      str: ARRAY [0..stringLength - 1] OF CHAR;
  BEGIN
    Done := FALSE;
    ReadString(str);

    IF (termCh <> esc) AND (termCh <> can) THEN
      StringToLongReal(str, r, Done);
    END;
  END ReadLongReal;

  PROCEDURE WriteLongReal(r: LONGREAL; length: CARDINAL);
    CONST
      minLength = 8;
      minDigits = 2;
      badMessLength = minLength + minDigits;
      badMess = "not a real"; (* must have 'badMessLength' characters *)
    VAR
      str: ARRAY [0..stringLength - 1] OF CHAR;
      digits: INTEGER;
      i: CARDINAL;
  BEGIN
    IF length < minLength + minDigits THEN
      length := minLength + minDigits;
    END;

    IF length > stringLength THEN
      FOR i := stringLength+1 TO length DO
        Write(' ');
      END;
      length := stringLength;
    END;

    digits := - INTEGER(length - minLength);
    (*LongRealToString(r, digits, length, str, Done);*)
    FormatLongReal(r, digits, length, str);

    IF NOT Done THEN
      IF length >  badMessLength THEN
        FOR i := 1 TO length - badMessLength DO
          Write(' ');
        END;
      END;
      WriteString(badMess);
    ELSE
      WriteString(str);
    END;

    Done := TRUE;
  END WriteLongReal;


  PROCEDURE WriteOct(z,n: CARDINAL);
    VAR i: CARDINAL;
        d: ARRAY [0..6] OF CARDINAL;
  BEGIN i := 0;
    REPEAT d[i] := z MOD 10B; z := z DIV 10B; i := i+1
    UNTIL i = n;
    REPEAT i := i-1; Write(CHR(d[i]+60B))
    UNTIL i = 0
  END WriteOct;

  TYPE
    LongReal = RECORD (* conversion from real to its four words *)
             CASE :CARDINAL OF
               0: R: LONGREAL;
             | 1: arr: ARRAY [0..3] OF CARDINAL;
             END
           END ;

  PROCEDURE WriteLongRealOct(x: LONGREAL);
    VAR
      u: LongReal;
  BEGIN
    u.R := x;
    WriteOct(u.arr[0], 6); Write(" ");
    WriteOct(u.arr[1], 6); Write(" ");
    WriteOct(u.arr[2], 6); Write(" ");
    WriteOct(u.arr[3], 6); Write(" ");
    Done := TRUE;
  END WriteLongRealOct;

  PROCEDURE WriteLongRealHEX(x: LONGREAL);
    VAR
      u: LongReal;
  BEGIN
    u.R := x;
    WriteHex(u.arr[0], 4); Write(" ");
    WriteHex(u.arr[1], 4); Write(" ");
    WriteHex(u.arr[2], 4); Write(" ");
    WriteHex(u.arr[3], 4); Write(" ");
    Done := TRUE;
  END WriteLongRealHEX;

END RealInOut.
