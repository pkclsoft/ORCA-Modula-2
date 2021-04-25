(*$OverflowCheck+*)
(*$RangeCheck+*)
MODULE TSTDIV4;

FROM InOut IMPORT WriteString, WriteLn;

VAR
  A, B, C:  LONGINT;
  pass:     BOOLEAN;
BEGIN
  A := 32784;
  B := 63;
  C := A DIV B;

  pass := (C = 520);

  A := -56343L;
  B := 7;
  C := A DIV B;

  pass := pass AND (C = -8049L);

  A := 523;
  B := -8342L;
  C := A DIV B;

  pass := pass AND (C = 0);

  A := -96123L;
  B := -5902L;
  C := A DIV B;

  pass := pass AND (C = 16);

  A := 1628373230;
  B := 3;
  C := A DIV B;

  pass := pass AND (C = 542791076);

  IF pass THEN
    WriteString('TstDiv4 Passed');
  ELSE
    WriteString('TstDiv4 Failed');
  END;

  WriteLn;
END TSTDIV4.
