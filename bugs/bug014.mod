(*$RangeCheck+*)
MODULE Cypher;

FROM InOut IMPORT  WriteString, ReadInt, Write, WriteInt, WriteLn;

TYPE
  values = [0..27];
  alpha2 = ['A'..'['];

VAR
  base:     values;
  code:     ARRAY values OF CHAR;
  done:     BOOLEAN;
  num:      INTEGER;
  indexCh:  alpha2;
  index:    values;

BEGIN
  WriteString('Starting number: ');
  ReadInt(num); WriteLn;
  base := VAL(values, num);
  indexCh := 'A';
  index := base - 1;
  REPEAT
    code[index + 1] := indexCh;
    INC(indexCh);
    index := (index + 1) MOD 26;
  UNTIL indexCh > 'Z';
  done := FALSE;
  REPEAT
    ReadInt(num);
    IF num = 0 THEN
      done := TRUE;
    ELSIF (num >= 1) AND (num <= 26) THEN
      Write(code[num]); WriteLn;
    ELSIF num = 27 THEN
      Write(' ');
    ELSE
      WriteString(' is not a valid code number');
      done := TRUE;
    END;
  UNTIL done;
END Cypher.
