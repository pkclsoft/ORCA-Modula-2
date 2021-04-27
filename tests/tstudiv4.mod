(*$OverflowCheck+*)
(*$RangeCheck+*)
MODULE TSTUDIV4;

FROM InOut IMPORT WriteString, WriteLn;

VAR
  A, B, C: LONGCARD;
  pass: BOOLEAN;
BEGIN
  A := 5;
  B := 6;
  C := A DIV B;

  pass := (C = 0);

  A := 3731234;
  B := 20000;
  C := A DIV B;

  pass := pass AND (C = 186);

  A := 513265;
  B := 20000;
  C := A DIV B;

  pass := pass AND (C = 25);

  A := 60125;
  B := 20000;
  C := A DIV B;

  pass := pass AND (C = 3);

  A := 34000;
  B := 35060;
  C := A DIV B;

  pass := pass AND (C = 0);

  A := 4005125675;
  B := 35060;
  C := A DIV B;

  pass := pass AND (C = 114236);

  IF pass THEN
    WriteString('TSTUDIV4 Passed');
  ELSE
    WriteString('TSTUDIV4 Failed');
  END;

  WriteLn;
END TSTUDIV4.
