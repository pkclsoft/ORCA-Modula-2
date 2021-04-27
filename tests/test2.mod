MODULE Test2;

FROM InOut IMPORT WriteString, WriteLn;
FROM SYSTEM IMPORT BYTE;

TYPE
  subcard = [0..65000];

VAR
  array1: ARRAY [-32000..32767] OF BYTE;
  array2: ARRAY [0..32767] OF CARDINAL;
  index1: INTEGER;
  index2: CARDINAL;
  pass:   BOOLEAN;
BEGIN
  FOR index1 := -32000 TO 32767 DO
    array1[index1] := VAL(BYTE, index1 MOD 256);
  END;

  FOR index2 := 0 TO HIGH(array2) DO
    array2[index2] := index2;
  END;

  pass := TRUE;

  FOR index1 := -32000 TO 32767 DO
    pass := pass AND (array1[index1] = VAL(BYTE, index1 MOD 256));
  END;

  FOR index2 := 0 TO HIGH(array2) DO
    pass := pass AND (array2[index2] = index2);
  END;

  IF pass THEN
    WriteString('Test2 Passed');
  ELSE
    WriteString('Test2 Failed');
  END;

  WriteLn;
END Test2.
