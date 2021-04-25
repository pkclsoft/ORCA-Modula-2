MODULE DataSize;

FROM InOut IMPORT WriteString, WriteLn;

VAR
  byte:   CHAR;
  array1: ARRAY [0..20000] OF CHAR;
  array2: ARRAY [0..15000] OF CARDINAL;
  array3: ARRAY [0..15000] OF BOOLEAN;
  index:  CARDINAL;
  indch:  CHAR;
  indbl:  BOOLEAN;
  pass:   BOOLEAN;
BEGIN
  pass := TRUE;

  indch := 0C;

  FOR index := 0 TO HIGH(array1) DO
    array1[index] := indch;

    IF indch < 377C THEN
      INC(indch);
    ELSE
      indch := 0C;
    END;
  END;

  FOR index := 0 TO HIGH(array2) DO
    array2[index] := index;
  END;

  indbl := FALSE;

  FOR index := 0 TO HIGH(array3) DO
    array3[index] := indbl;

    indbl := NOT indbl;
  END;

  indch := 0C;

  FOR index := 0 TO HIGH(array1) DO
    pass := pass AND (array1[index] = indch);

    IF indch < 377C THEN
      INC(indch);
    ELSE
      indch := 0C;
    END;
  END;

  FOR index := 0 TO HIGH(array2) DO
    pass := pass AND (array2[index] = index);
  END;

  indbl := FALSE;

  FOR index := 0 TO HIGH(array3) DO
    pass := pass AND (array3[index] = indbl);

    indbl := NOT indbl;
  END;

  IF pass THEN
    WriteString('DataSize Passed.');
  ELSE
    WriteString('DataSize Failed.');
  END;
END DataSize.
