{$keep 'pcp'}
program pcp(input, output);

uses pascalls;

VAR
  smlarr: smlarrtyp;
  lrgarr: lrgarrtyp;
  smlrec: smlrectyp;
  lrgrec: lrgrectyp;

  index:  INTEGER;

  b:      BYTE;
  c:      CHAR;
  i:      INTEGER;
  bo:     BOOLEAN;
  l:      LONGINT;
  r:      REAL;
  db:     double;

BEGIN
  nonvars(1, '#', 3, FALSE, 5, 6.7, 8.9);

  b := 1;
  c := '#';
  i := 3;
  bo := FALSE;
  l := 5;
  r := 6.7;
  db := 8.9;
  vars(b, c, i, bo, l, r, db);

  FOR index := lowarr TO smlhigharr DO begin
    smlarr[index] := index * 7;
  END;

  smlnonvararr(smlarr);
  smlvararr(smlarr);

  FOR index := lowarr TO lrghigharr DO begin
    lrgarr[index] := index * 3;
  END;

  lrgnonvararr(lrgarr);
  lrgvararr(lrgarr);

  WITH smlrec DO begin
    a := 51;
    b := TRUE;
  END;

  smlnonvarrec(smlrec);
  smlvarrec(smlrec);
  
  WITH lrgrec DO begin
    a := 51;
    b := TRUE;
    c := 102;
  END;

  lrgnonvarrec(lrgrec);
  lrgvarrec(lrgrec);

  WriteLn('byte(5)+1 = <', incbyte(5):5, '>');
  WriteLn;
  
  WriteLn('char("A")+1 = <', incchar('A'), '>');
  WriteLn;
  
  WriteLn('integer(9)+1 = <', incint(9):5, '>');
  WriteLn;
  
  WriteLn('longint(70001)+1 = <', inclint(70001):7, '>');
  WriteLn;
  
  Write('not boolean(FALSE) = <');

  IF notbool(FALSE) THEN
    Write('TRUE')
  ELSE
    Write('FALSE');

  WriteLn('>');
  WriteLn;
  
  WriteLn('real(3)+10 = <', add10toreal(3.0), '>');
  WriteLn;
  
  WriteLn('longreal(5)+102.345 = <', add102tolrl(5.0), '>');
  WriteLn;
END.
