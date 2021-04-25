(*$OverflowCheck+*)
(*$RangeCheck+*)
MODULE TSTUMUL2;

FROM InOut IMPORT WriteString, WriteLn;

VAR
  A, B, C: CARDINAL;
  pass: BOOLEAN;
BEGIN
  A := 5;
  B := 6;
  C := A * B;

  pass := (C = 30);

  A := 3;
  B := 20000;
  C := A * B;

  pass := pass AND (C = 60000);

  IF pass THEN
    WriteString('TSTUMUL2 Passed');
  ELSE
    WriteString('TSTUMUL2 Failed');
  END;

  WriteLn;
END TSTUMUL2.
