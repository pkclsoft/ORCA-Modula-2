(*$OverflowCheck+*)
(*$RangeCheck+*)
MODULE TSTUMUL4;

FROM InOut IMPORT WriteString, WriteLn;

VAR
  A, B, C: LONGCARD;
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

  A := 5;
  B := 20000;
  C := A * B;

  pass := pass AND (C = 100000);

  A := 60125;
  B := 20000;
  C := A * B;

  pass := pass AND (C = 1202500000);

  A := 34000;
  B := 35060;
  C := A * B;

  pass := pass AND (C = 1192040000);

  A := 100000;
  B := 12;
  C := A * B;

  pass := pass AND (C = 1200000);

  A := 13;
  B := 100000;
  C := A * B;

  pass := pass AND (C = 1300000);

  IF pass THEN
    WriteString('TSTUMUL4 Passed');
  ELSE
    WriteString('TSTUMUL4 Failed');
  END;

  WriteLn;
END TSTUMUL4.
