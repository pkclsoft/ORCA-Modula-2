MODULE Test3;

FROM InOut IMPORT WriteString, WriteLn;
FROM SYSTEM IMPORT BYTE;

VAR
  a, a2:  CARDINAL;
  b:      LONGCARD;
  c:      INTEGER;
  d:      LONGINT;
  pass:   BOOLEAN;
BEGIN
  pass := 64000 > 0;
  pass := pass AND (64000 >= 0);
  pass := pass AND (-5 < 0);
  pass := pass AND (-32000 <= 0);
  pass := pass AND (-5 < 32000);
  pass := pass AND (32000 > -32000);

  a := 64000;
  pass := pass AND (a > 0);
  pass := pass AND (0 <= a);

  b := 120123;
  pass := pass AND (b > 0);
  b := 64000;
  pass := pass AND (b > 0);

  d := 64000;
  pass := pass AND (d > 0);
  pass := pass AND (0 <= d);
  pass := pass AND (-5 < d);
  pass := pass AND (d > -32000);

  IF pass THEN
    WriteString('Test3 Passed');
  ELSE
    WriteString('Test3 Failed');
  END;

  WriteLn;
END Test3.
