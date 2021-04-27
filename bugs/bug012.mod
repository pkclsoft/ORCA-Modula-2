MODULE Fortest;

VAR
  i: INTEGER;
  a: ARRAY [1..100] OF INTEGER;

  PROCEDURE incr(VAR i: INTEGER);
  BEGIN
    i := i + 1;
  END incr;

BEGIN
  FOR i := 1 TO 100 BY 0 DO
    a[i] := 0;
    incr(i);
  END;
END ForTest.
