MODULE TestLR;

FROM InOut IMPORT WriteString, WriteLn;
FROM RealInOut IMPORT WriteLongReal, WriteLongRealOct;

VAR
  rv:   LONGREAL;
  cnt:  CARDINAL;
BEGIN
  WriteString('Max(LONGREAL) = ');
  WriteLongReal(MAX(LONGREAL), 20);
  WriteLn;

  WriteString('Min(LONGREAL) = ');
  WriteLongReal(MIN(LONGREAL), 20);
  WriteLn;

  rv := MAX(LONGREAL) / 10.00001L;

  FOR cnt := 0 TO 17 DO
    WriteString('rv = ');
    WriteLongReal(rv, 20);
    WriteLn;

    rv := rv + 0.012345L;
  END;
END TestLR.
