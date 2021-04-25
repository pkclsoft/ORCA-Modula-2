(*$Keep TestConst*)
MODULE TestConst;

FROM Terminal IMPORT WriteString, WriteLn;

PROCEDURE Test2;
CONST
  ConstStringInt = 'sad';
BEGIN
  WriteString(ConstStringInt);
  WriteLn;
END Test2;

CONST
  ConstString = 'happy';

VAR
  testString:   ARRAY [0..80] OF CHAR;
 
BEGIN
  WriteString(ConstString);
  WriteLn;

  Test2;

  testString := '';
  testString := 'fred';
END TestConst.
