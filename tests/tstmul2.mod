(*$OverflowCheck+*)
(*$RangeCheck+*)
MODULE TSTMUL2;

FROM InOut IMPORT WriteLn, WriteString;

VAR
  A, B, C: INTEGER;
  pass: BOOLEAN;
BEGIN
  A := 5;
  B := 6;
  C := A * B;

  pass := (C = 30);

  A := -5;
  B := 7;
  C := A * B;

  pass := pass AND (C = -35);

  A := 5;
  B := -8;
  C := A * B;

  pass := pass AND (C = -40);

  A := -5;
  B := -9;
  C := A * B;

  pass := pass AND (C = 45);

  IF pass THEN
    WriteString('TstMul2 Passed');
  ELSE
    WriteString('TstMul2 Failed');
  END;

  WriteLn;
END TSTMUL2.

