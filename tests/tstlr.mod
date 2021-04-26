MODULE TestLR;

FROM InOut IMPORT WriteString, WriteLn;
FROM RealInOut IMPORT WriteLongReal, WriteLongRealHEX;

VAR
  rv:   LONGREAL;
  cnt:  CARDINAL;
BEGIN
  WriteString('Max(LONGREAL) = ');
  WriteLongRealHEX(MAX(LONGREAL));
  WriteLn;

  WriteString('Min(LONGREAL) = ');
  WriteLongRealHEX(MIN(LONGREAL));
  WriteLn;

  rv := 1.5L;

  FOR cnt := 0 TO 17 DO
    WriteString('rv = ');
    WriteLongRealHEX(rv);
    WriteLongReal(rv, 20);
    WriteLn;

    rv := rv + 0.012345L;
  END;
END TestLR.
