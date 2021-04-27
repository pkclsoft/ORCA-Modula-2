(*$OverflowCheck+*)
(*$RangeCheck+*)
MODULE TSTMOD2;

FROM InOut IMPORT WriteLn, WriteString;

VAR
  A, B, C: INTEGER;
  pass: BOOLEAN;
BEGIN
  A := 30;
  B := 6;
  C := A MOD B;

  pass := (C = 0);

  A := -50;
  B := 7;
  C := A MOD B;

  pass := pass AND (C = 6);

  IF pass THEN
    WriteString('TstMod2 passed');
  ELSE
    WriteString('TstMod2 failed');
  END;

  WriteLn;
END TSTMOD2.
