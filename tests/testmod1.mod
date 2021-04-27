IMPLEMENTATION MODULE TestMod1;
FROM InOut IMPORT WriteString, WriteLn;

PROCEDURE PutTest();
BEGIN
  WriteString('TestMod1');
  WriteLn;
END PutTest;

PROCEDURE Sum(i,j: CARDINAL): CARDINAL;
BEGIN
  RETURN i+j;
END Sum;

END TestMod1.

