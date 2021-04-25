(*$Keep 'TestMath'*)
program TestMath(output);

CONST
  delta = 0.001;
  ldelta = 0.001;

VAR
  rv1, rv2, rv3:  REAL;
  int:            INTEGER;
  lr1, lr2, lr3:  DOUBLE;
  lint:           LONGINT;
  pass:           BOOLEAN;
BEGIN
  rv1 := sqrt(67.5);

  pass := (ABS(rv1 - 8.215838363) < delta);

  rv1 := 5.1;
  rv2 := 8.0;
  rv3 :=  ln(rv1);

  pass := pass AND (ABS(rv3 - 1.62924054) < delta);

  rv3 := rv2 * rv3;

  pass := pass AND (ABS(rv3 - 13.03392432) < delta);

  rv3 :=  exp(rv3);

  pass := pass AND (ABS(rv3 - 457679.4453) < delta);

  rv2 :=  sin(rv1);

  pass := pass AND (ABS(rv2 - (-0.925814682)) < delta);

  rv2 :=  cos(rv1);

  pass := pass AND (ABS(rv2 - 0.377977742) < delta);

  rv2 :=  arctan(rv1);

  pass := pass AND (ABS(rv2 - 1.377174334) < delta);

  rv1 := 6.7;
  int :=  round(rv1);

  pass := pass AND (int = 7);

  lr1 :=  sqrt(67.5);

  pass := pass AND (ABS(lr1 - 8.215838363) < ldelta);

  lr1 := 5.1;
  lr2 := 8.0;
  lr3 :=  ln(lr1);

  pass := pass AND (ABS(lr3 - 1.62924054) < ldelta);

  lr3 := lr2 * lr3;

  pass := pass AND (ABS(lr3 - 13.03392432) < ldelta);

  lr3 :=  exp(lr3);

  pass := pass AND (ABS(lr3 - 457679.4453) < ldelta);

  lr2 :=  sin(lr1);

  pass := pass AND (ABS(lr2 - (-0.925814682)) < ldelta);

  lr2 :=  cos(lr1);

  pass := pass AND (ABS(lr2 - 0.377977742) < ldelta);

  lr2 :=  arctan(lr1);

  pass := pass AND (ABS(lr2 - 1.377174334) < ldelta);

  lr1 := 6.7;
  lint :=  round(lr1);

  pass := pass AND (lint = 7);

  IF pass THEN
    WriteLn('TestMath Passed')
  ELSE
    WriteLn('TestMath Failed');
END.
