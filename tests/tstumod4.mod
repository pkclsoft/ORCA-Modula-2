(*$OverflowCheck+*)
(*$RangeCheck+*)
MODULE TSTUMOD4;

FROM InOut IMPORT WriteString, WriteLn;

VAR
  A, B, C: LONGCARD;
  pass: BOOLEAN;
BEGIN
  A := 5;
  B := 6;
  C := A MOD B;

  pass := (C = 5);

  A := 3731234;
  B := 20000;
  C := A MOD B;

  pass := pass AND (C = 11234);

  A := 513265;
  B := 20000;
  C := A MOD B;

  pass := pass AND (C = 13265);

  A := 60125;
  B := 20000;
  C := A MOD B;

  pass := pass AND (C = 125);

  A := 34000;
  B := 35060;
  C := A MOD B;

  pass := pass AND (C = 34000);

  A := 4005125675;
  B := 35060;
  C := A MOD B;

  pass := pass AND (C = 11515);

  IF pass THEN
    WriteString('TSTUMOD4 Passed');
  ELSE
    WriteString('TSTUMOD4 Failed');
  END;

  WriteLn;
END TSTUMOD4.
