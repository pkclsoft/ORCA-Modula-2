(*$OverflowCheck+*)
(*$RangeCheck+*)
MODULE TSTUMOD2;

FROM InOut IMPORT WriteString, WriteLn;

VAR
  A, B, C: CARDINAL;
  pass: BOOLEAN;
BEGIN
  A := 50023;
  B := 6;
  C := A MOD B;

  pass := (C = 1);

  A := 33000;
  B := 11001;
  C := A MOD B;

  pass := pass AND (C = 10998);

  IF pass THEN
    WriteString('TSTUMOD2 Passed');
  ELSE
    WriteString('TSTUMOD2 Failed');
  END;

  WriteLn;
END TSTUMOD2.
