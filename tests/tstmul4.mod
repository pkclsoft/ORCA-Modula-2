(*$OverflowCheck+*)
(*$RangeCheck+*)
MODULE TSTMUL4;

FROM InOut IMPORT WriteString, WriteLn;

VAR
  A, B, C: LONGINT;
  pass: BOOLEAN;
BEGIN
  A := 32784;
  B := 63;
  C := A * B;

  pass := (C = 2065392);

  A := -56343L;
  B := 7;
  C := A * B;

  pass := pass AND (C = -394401L);

  A := 523;
  B := -8342;
  C := A * B;

  pass := pass AND (C = -4362866L);

  A := -5902;
  B := -96123L;
  C := A * B;

  pass := pass AND (C = 567317946L);

  IF pass THEN
    WriteString('TstMul4 Passed');
  ELSE
    WriteString('TstMul4 Failed');
  END;

  WriteLn;
END TSTMUL4.
