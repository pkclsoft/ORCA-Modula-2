(*$OverflowCheck+*)
(*$RangeCheck+*)
MODULE TSTMOD4;

FROM InOut IMPORT WriteString, WriteLn;

VAR
  A, B, C:  LONGINT;
  pass:     BOOLEAN;
BEGIN
  A := 32784L;
  B := 63;
  C := A MOD B;

  pass := (C =  24);

  A := -56343L;
  B := 7;
  C := A MOD B;

  pass := pass AND (C = 0);

  A := 1628373230;
  B := 3;
  C := A MOD B;

  pass := pass AND (C = 2);

  IF pass THEN
    WriteString('TstMod4 Passed');
  ELSE
    WriteString('TstMod4 Failed');
  END;

  WriteLn;
END TSTMOD4.
