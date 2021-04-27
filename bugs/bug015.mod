MODULE Bug015;

FROM Common IMPORT SCB;
FROM EZTools IMPORT EZStartGraph, EZEndGraph;
FROM InOut IMPORT WriteString, WriteLn, ReadString;
FROM QuickDrawII IMPORT SetForeColor, SetBackColor, scbColorMode, 
  scbStayInGraph, MoveTo;
FROM Strings IMPORT Length;
FROM SYSTEM IMPORT ADR;
FROM ToolLocator IMPORT StartStopRecordPtr;

VAR
  myTools:  StartStopRecordPtr;
  str:      ARRAY [0..31] OF CHAR;

BEGIN
  myTools := EZStartGraph(SCB{scbColorMode, scbStayInGraph});

  MoveTo(10, 10);
  SetForeColor(15);
  SetBackColor(0);

  WriteString('This should appear on the graphics screen.');
  WriteLn;
  WriteString('Enter some strings, a null one will end the program');
  WriteLn;

  REPEAT
    WriteString('str>');
    ReadString(str);
  UNTIL Length(str) = 0;

  EZEndGraph(myTools);
END Bug015.
