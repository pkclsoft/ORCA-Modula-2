MODULE TestHALT;

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

BEGIN
  TermProc := MyTermProc;

  WriteString('About to HALT!');
  WriteLn;

  HALT;
END TestHALT.
