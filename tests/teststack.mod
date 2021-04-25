MODULE TestStack;

FROM EZDump IMPORT DisplayReason;
FROM M2Lib IMPORT TermProc, aTerminateStatus;
FROM SYSTEM IMPORT ADDRESS;
FROM Terminal IMPORT WriteString, WriteLn;

PROCEDURE MyTermProc(a: ADDRESS; r: aTerminateStatus);
BEGIN
  WriteString('Program crashed, executing MyTermProc');
  WriteLn;

  DisplayReason(r);

  WriteString('MyTermProc completed!');
  WriteLn;
END MyTermProc;

(*$StackCheck+*)
PROCEDURE CallMe;
VAR
  a: INTEGER;
BEGIN
  CallMe;
END CallMe;
(*$StackCheck-*)

BEGIN
  TermProc := MyTermProc;

  WriteString('About to crash!');
  WriteLn;

  CallMe;
END TestStack.
