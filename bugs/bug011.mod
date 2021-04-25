MODULE Extract;

FROM InOut IMPORT WriteString, WriteLn, WriteInt, WriteLongInt, ReadString;
FROM NumberConversion IMPORT StringToLongInt;

VAR
  value:
    RECORD
      CASE :BOOLEAN OF
        TRUE:   l: LONGINT;
      | FALSE:  lsw, msw: INTEGER;
      END;
    END;
  
  PROCEDURE ReadLongInt(VAR l: LONGINT);
  VAR
    str:  ARRAY [0..19] OF CHAR;
    OK:   BOOLEAN;
  BEGIN
    ReadString(str);
    StringToLongInt(str, l, OK);
  END ReadLongInt;

BEGIN
  REPEAT
    WriteString('long integer: ');
    ReadLongInt(value.l); WriteLn;
    WriteLongInt(value.l, 10); WriteLn;
(*    value.l := 1234565;
    WriteLongInt(value.l, 10); WriteLn;  *)
    WriteString('least significant word: ');
    WriteInt(value.lsw, 10); WriteLn;
    WriteString('most significant word: ');
    WriteInt(value.msw, 10); WriteLn;
    WriteLn;
  UNTIL value.l = 0;
END Extract.
