MODULE TestR;

FROM InOut IMPORT WriteString, WriteLn, WriteFixPt;
FROM RealConversion IMPORT RealToString;
VAR
  rv:   REAL;
  cnt:  CARDINAL;
  str:  ARRAY [0..100] OF CHAR;
  ok :  BOOLEAN;
BEGIN
  WriteString('Max(REAL) = ');
  WriteFixPt(MAX(REAL), 20, 0);
  WriteLn;

  WriteString('Min(REAL) = ');
  WriteFixPt(MIN(REAL), 20, 0);
  WriteLn;

  RealToString(1.5, 20, 20, str, ok);

  WriteString('MAX(REAL) = ');
  WriteString(str);
  WriteLn;

  rv := 0.0;

  FOR cnt := 0 TO 17 DO
    WriteString('rv = ');
    WriteFixPt(rv, 20, 0);
    WriteLn;

    rv := rv + 0.012345;
  END;
END TestR.
