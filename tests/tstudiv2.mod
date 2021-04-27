(*$OverflowCheck+*)
(*$RangeCheck+*)
MODULE TSTUDIV2;

FROM InOut IMPORT WriteString, WriteLn;

VAR
  A, B, C: CARDINAL;
  pass: BOOLEAN;
BEGIN
  A := 50023;
  B := 6;
  C := A DIV B;

  pass := (C = 8337);

  A := 33000;
  B := 11001;
  C := A DIV B;

  pass := pass AND (C = 2);

  IF pass THEN
    WriteString('TSTUDIV2 Passed');
  ELSE
    WriteString('TSTUDIV2 Failed');
  END;

  WriteLn;
END TSTUDIV2.
