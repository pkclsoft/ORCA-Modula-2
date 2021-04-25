(*$OverflowCheck+*)
(*$RangeCheck+*)
MODULE TSTDIV2;

FROM InOut IMPORT WriteString, WriteLn;

VAR
  A, B, C: INTEGER;
	pass: BOOLEAN;
BEGIN
  A := 30;
  B := 6;
  C := A DIV B;

	pass := (C = 5);

  A := -50;
  B := 7;
  C := A DIV B;

	pass := pass AND (C = -7);

  A := 50;
  B := -8;
  C := A DIV B;

	pass := pass AND (C = -6);

  A := -500;
  B := -9;
  C := A DIV B;

	pass := pass AND (C = 55);

	IF pass THEN
		WriteString('TstDiv2 passed');
	ELSE
		WriteString('TstDiv2 failed');
	END;

	WriteLn;
END TSTDIV2.
