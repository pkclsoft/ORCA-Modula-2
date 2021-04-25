MODULE Test1;

FROM InOut IMPORT WriteLn, WriteString;
FROM SYSTEM IMPORT ADR, ADDRESS;

IMPORT EZDump;

CONST
  delta = 0.001;  (* math accuracy *)

VAR
  a: CARDINAL;
  b: LONGCARD;

  c: INTEGER;
  d: LONGINT;

  e: REAL;
  f: LONGREAL;

  pass: BOOLEAN;

  adr:  ADDRESS;

(*$RangeCheck+*)
(*$OverflowCheck+*)
BEGIN
  b := 4;
  a := b;
  b := a;
  a := VAL(CARDINAL, b) + 5;
  b := a + 5;

  pass := a = 9;
  pass := pass AND (b = 14);

  d := -7;
  c := SHORT(d);
  d := c;
  c := VAL(INTEGER, d) + 5;
  d := c + 5;

  pass := pass AND (c = -2);
  pass := pass AND (d = 3);

  a := c + 4;
  c := VAL(INTEGER, a) + (-3);
  
  pass := pass AND (a = 2);
  pass := pass AND (c = -1);

  d := b;
  d := VAL(LONGINT, b) + d;
  DEC(d, b);
  INC(d, b);

  pass := pass AND (d = 28);

  e := 34.56778;
  f := e;
  e := f * 2.0L;
  e := e / 2.0;
  e := 2.0 * e;

  pass := pass AND (ABS(e - SHORT(2.0L * f)) < delta);

  adr := ADR(c);
  b := adr + 5;

  IF pass THEN
    WriteString('Test1 Passed');
  ELSE
    WriteString('Test1 Failed');
  END;

  WriteLn;
END Test1.
