(*$StackSize 2000H*)
MODULE TestInOut;

FROM InOut IMPORT
  OpenInput, OpenOutput, CloseInput, CloseOutput, WriteString, WriteLn, 
  ReadString, Write, termCh, in;

IMPORT InOut;

VAR
  str:  ARRAY [0..79] OF CHAR;

BEGIN
  WriteString('Opening input from bugprog.txt, and displaying on terminal');
  WriteLn;

  OpenInput('txt');

  IF InOut.Done THEN
    WHILE NOT in.eof DO
      ReadString(str);
      WriteString(str);
      Write(termCh);
    END;

    CloseInput;
  END;

  WriteLn;
  WriteLn;
  WriteLn;
  WriteString('Opening input from bugprog.txt, and displaying in output.file');
  WriteLn;

  OpenOutput('file');

  IF InOut.Done THEN
    OpenInput('txt');

    IF InOut.Done THEN
      WHILE NOT in.eof DO
        ReadString(str);
        WriteString(str);
        Write(termCh);
      END;

      CloseInput;
    END;

    CloseOutput;
  END;
END TestInOut.
