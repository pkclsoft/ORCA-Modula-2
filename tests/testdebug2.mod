IMPLEMENTATION MODULE TestDebug2;

FROM InOut IMPORT WriteString, WriteCard, WriteLn;

VAR
  m:  CARDINAL;

PROCEDURE Proc1;
BEGIN
  m := 5;

  WHILE m < 10 DO
    WriteString('m = ');
    WriteCard(m, 2);
    WriteLn;

    INC(m);
  END;
END Proc1;

PROCEDURE Proc2;
BEGIN
  WHILE m > 0 DO
    WriteString('m = ');
    WriteCard(m, 2);
    WriteLn;

    DEC(m);
  END;
END Proc2;

END TestDebug2.
