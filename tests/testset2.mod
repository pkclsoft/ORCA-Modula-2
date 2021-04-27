MODULE TestSet2;

FROM InOut IMPORT WriteString, WriteLn;

TYPE
  aDirective = (NDA, CDA, RTL, CDEV, INIT, NoImp, Pascal,
                RangeCheck, OverflowCheck, StackSize, Segment, Dynamic,
                StackCheck, Keep, ChainTo, NILCheck);
  aDirectiveSet = SET OF aDirective;

VAR
  set:  aDirectiveSet;
  int:  aDirectiveSet;
BEGIN
  set := aDirectiveSet{Keep};

  int := aDirectiveSet{NDA, CDA, RTL, INIT} * set;

  IF int = aDirectiveSet{} THEN
    WriteString('test1 - empty');
  ELSE
    WriteString('test1 - not empty');
  END;

  WriteLn;

  int := set * aDirectiveSet{NDA, CDA, RTL, INIT};

  IF int = aDirectiveSet{} THEN
    WriteString('test2 - empty');
  ELSE
    WriteString('test2 - not empty');
  END;

  WriteLn;
END TestSet2.
