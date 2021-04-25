(*$Keep TestMath*)
MODULE TestMath;

FROM InOut IMPORT WriteString, WriteLn;

IMPORT MathLib0, LongMath;

CONST
  delta = 0.001;
  ldelta = 0.001D;

VAR
  rv1, rv2, rv3:  REAL;
  int:            INTEGER;
  lr1, lr2, lr3:  LONGREAL;
  lint:           LONGINT;
  pass:           BOOLEAN;
BEGIN
  rv1 := MathLib0.sqrt(67.5);

  pass := (ABS(rv1 - 8.215838363) < delta);

  rv1 := 5.1;
  rv2 := 8.0;
  rv3 := MathLib0.ln(rv1);

  pass := pass AND (ABS(rv3 - 1.62924054) < delta);

  rv3 := rv2 * rv3;

  pass := pass AND (ABS(rv3 - 13.03392432) < delta);

  rv3 := MathLib0.exp(rv3);

  pass := pass AND (ABS(rv3 - 457679.4453) < delta);

  rv2 := MathLib0.sin(rv1);

  pass := pass AND (ABS(rv2 - (-0.925814682)) < delta);

  rv2 := MathLib0.cos(rv1);

  pass := pass AND (ABS(rv2 - 0.377977742) < delta);

  rv2 := MathLib0.arctan(rv1);

  pass := pass AND (ABS(rv2 - 1.377174334) < delta);

  rv1 := 6.7;
  int := MathLib0.entier(rv1);

  pass := pass AND (int = 7);

  lr1 := LongMath.sqrt(67.5);

  pass := pass AND (ABS(lr1 - 8.215838363D) < ldelta);

  lr1 := 5.1;
  lr2 := 8.0;
  lr3 := LongMath.ln(lr1);

  pass := pass AND (ABS(lr3 - 1.62924054D) < ldelta);

  lr3 := lr2 * lr3;

  pass := pass AND (ABS(lr3 - 13.03392432D) < ldelta);

  lr3 := LongMath.exp(lr3);

  pass := pass AND (ABS(lr3 - 457679.4453D) < ldelta);

  lr2 := LongMath.sin(lr1);

  pass := pass AND (ABS(lr2 - (-0.925814682D)) < ldelta);

  lr2 := LongMath.cos(lr1);

  pass := pass AND (ABS(lr2 - 0.377977742D) < ldelta);

  lr2 := LongMath.arctan(lr1);

  pass := pass AND (ABS(lr2 - 1.377174334D) < ldelta);

  lr1 := 6.7;
  lint := LongMath.entier(lr1);

  pass := pass AND (lint = 7D);

  IF pass THEN
    WriteString('TestMath Passed');
  ELSE
    WriteString('TestMath Failed');
  END;

  WriteLn;
END TestMath.
