(*$Keep 'M2CP'*)
MODULE M2CP;

FROM InOut IMPORT
  Write, WriteLn, WriteString, WriteCard, WriteLongInt, WriteReal,
  WriteLongReal;
FROM SYSTEM IMPORT
  BYTE;

IMPORT PasCalls;

VAR
  smlarr: PasCalls.smlarrtyp;
  lrgarr: PasCalls.lrgarrtyp;
  smlrec: PasCalls.smlrectyp;
  lrgrec: PasCalls.lrgrectyp;

  index:  CARDINAL;

  b:      BYTE;
  c:      CHAR;
  i:      INTEGER;
  bo:     BOOLEAN;
  l:      LONGINT;
  r:      REAL;
  db:     LONGREAL;

BEGIN
  PasCalls.nonvars(VAL(BYTE, 1), '#', 3, FALSE, 5, 6.7, 8.9L);

  b := VAL(BYTE, 1);
  c := '#';
  i := 3;
  bo := FALSE;
  l := 5;
  r := 6.7;
  db := 8.9L;
  PasCalls.vars(b, c, i, bo, l, r, db);

  FOR index := PasCalls.lowarr TO PasCalls.smlhigharr DO
    smlarr[index] := index * 7;
  END;

  PasCalls.smlnonvararr(smlarr);
  PasCalls.smlvararr(smlarr);

  FOR index := PasCalls.lowarr TO PasCalls.lrghigharr DO
    lrgarr[index] := index * 3;
  END;

  PasCalls.lrgnonvararr(lrgarr);
  PasCalls.lrgvararr(lrgarr);

  WITH smlrec DO
    a := 51;
    b := TRUE;
  END;

  PasCalls.smlnonvarrec(smlrec);
  PasCalls.smlvarrec(smlrec);
  
  WITH lrgrec DO
    a := 51;
    b := TRUE;
    c := 102;
  END;

  PasCalls.lrgnonvarrec(lrgrec);
  PasCalls.lrgvarrec(lrgrec);

  WriteString('byte(5)+1 = <');
  WriteCard(VAL(CARDINAL, PasCalls.incbyte(VAL(BYTE, 5))), 5);
  Write('>');
  WriteLn;
  
  WriteString("char('A')+1 = <");
  Write(PasCalls.incchar('A'));
  Write('>');
  WriteLn;
  
  WriteString('integer(9)+1 = <');
  WriteCard(PasCalls.incint(9), 5);
  Write('>');
  WriteLn;
  
  WriteString('longint(70001)+1 = <');
  WriteLongInt(PasCalls.inclint(70001), 7);
  Write('>');
  WriteLn;
  
  WriteString('not boolean(FALSE) = <');

  IF PasCalls.notbool(FALSE) THEN
    WriteString('TRUE');
  ELSE
    WriteString('FALSE');
  END;

  Write('>');
  WriteLn;
  
  WriteString('real(3)+10 = <');
  WriteReal(PasCalls.add10toreal(3.0), 10);
  Write('>');
  WriteLn;
  
  WriteString('longreal(5)+102.345 = <');
  WriteLongReal(PasCalls.add102tolrl(5.0L), 10);
  Write('>');
  WriteLn;
END M2CP.
