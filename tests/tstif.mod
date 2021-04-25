MODULE TstIf;

FROM InOut IMPORT WriteString, WriteLn;

VAR
  i,j:  INTEGER;

BEGIN
  i := 4;
  j := -7;

  IF (i = 0) OR
     ((j <> MIN(INTEGER)) &
      (i <> MIN(INTEGER)) &
      (ABS(j) <= 32767 DIV ABS(i))) THEN
    i := i * j;
  ELSE
    i := 0;
  END;

  IF i = -28 THEN
    WriteString('TstIf Passed');
  ELSE
    WriteString('TstIf Failed');
  END;

  WriteLn;
END TstIf.
