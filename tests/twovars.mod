(*$Keep 'happy' *)
MODULE TwoVars;

PROCEDURE Test2Vars(M, N: CARDINAL);
VAR
  I:  CARDINAL;
  J:  CARDINAL;

  PROCEDURE Inner;
  BEGIN
    I := M;
  END Inner;

VAR
  K:  CARDINAL;
  L:  CARDINAL;

BEGIN
  Inner;

  K := I;
  I := N;
  L := M;
END Test2Vars;

BEGIN
  Test2Vars(32, 40);
END TwoVars.
