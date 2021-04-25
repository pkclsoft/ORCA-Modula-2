MODULE TestLocals;

FROM InOut IMPORT WriteString, WriteLn;
FROM TestLocals2 IMPORT INCB, B;

  MODULE NESTED1;

  EXPORT A, INCA;

  VAR
    A:  CARDINAL;

  PROCEDURE INCA;
  BEGIN
    INC(A);
  END INCA;

  BEGIN
    A := 1;
  END NESTED1;

VAR
  pass: BOOLEAN;

BEGIN
  INCA;
  INCA;

  pass := A = 3;

  INCB;
  INCB;

  pass := pass AND (B = 17);

  IF pass THEN
    WriteString('TestLocals Passed');
  ELSE
    WriteString('TestLocals Failed');
  END;

  WriteLn;
END TestLocals.
