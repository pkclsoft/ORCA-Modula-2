(*$CDA "A Test CDA" ExitCDA *)
MODULE TestCDA;

FROM InOut IMPORT WriteString, WriteLn, ReadString;
FROM Strings IMPORT Length;

PROCEDURE ExitCDA;
BEGIN
END ExitCDA;

VAR
  str: ARRAY [0..20] OF CHAR;

BEGIN
  WriteString('This is a test CDA! - An empty string will end.');
  WriteLn;

  REPEAT
    WriteString('Enter a string: "');
    ReadString(str);
    WriteString('"');
    WriteLn;

    WriteString('You entered: "');
    WriteString(str);
    WriteString('"');
    WriteLn;
  UNTIL Length(str) = 0;
END TestCDA.
