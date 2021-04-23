(*$Segment Terminal*)
IMPLEMENTATION MODULE Terminal;

FROM ASCII IMPORT
  nul, EOL;
FROM M2Lib IMPORT
  PutChar;
FROM SYSTEM IMPORT
  ADDRESS, INLINE, GETREG;
FROM W65C816 IMPORT SEP, LDA, ABSLONG, REP, And, IMMEDIATE, Acc, STA;

VAR
  doRead:       aReadProc;
  doWrite:      aWriteProc;

(*
  Note that the following two routines use INLINE code.  In Version 1.0, these
  routines were supplied by the ORCA shell, but this causes problems when a
  program was run in standalone mode, without the ORCA shell.

  What they do, is check the good ol' apple ii keyboard register to see if a
  key has been pressed (high bit in 00C000 is set), and then read that key
  stroke.
*)

PROCEDURE KeyAvailable(): BOOLEAN;
VAR
  key: CARDINAL;
BEGIN
  INLINE(SEP, 20H);
  INLINE(LDA+ABSLONG, 0, 0C0H, 0);
  INLINE(REP, 20H);
  INLINE(And+IMMEDIATE, 080H, 0);
  GETREG(Acc, key);
  RETURN key <> 0;
END KeyAvailable;

PROCEDURE ReadKey(VAR ch: CHAR);
VAR
  key: CARDINAL;
BEGIN
  INLINE(SEP, 20H);
  INLINE(LDA+ABSLONG, 0, 0C0H, 0);
  INLINE(STA+ABSLONG, 10H, 0C0H, 0);
  INLINE(REP, 20H);
  INLINE(And+IMMEDIATE, 07FH, 0);
  GETREG(Acc, key);

  INLINE(SEP, 20H);
  INLINE(LDA+IMMEDIATE, 0);
  INLINE(STA+ABSLONG, 10H, 0C0H, 0);
  INLINE(REP, 20H);

  ch := VAL(CHAR, key);
END ReadKey;

PROCEDURE ReadChar(VAR ch: CHAR; VAR done: BOOLEAN); FORWARD;

PROCEDURE AssignRead(readProc: aReadProc);
BEGIN
  doRead := readProc;
END AssignRead;

PROCEDURE AssignWrite(writeProc: aWriteProc);
BEGIN
  doWrite := writeProc;
END AssignWrite;

PROCEDURE BusyRead(VAR ch: CHAR);
VAR
  done: BOOLEAN;
BEGIN
  doRead(ch, done);

  IF NOT done THEN
    ch := nul;
  END;
END BusyRead;

PROCEDURE DeassignRead;
BEGIN
  doRead := ReadChar;
END DeassignRead;

PROCEDURE DeassignWrite;
BEGIN
  doWrite := PutChar;
END DeassignWrite;

PROCEDURE GetWriteProc(VAR curWriteProc: aWriteProc);
BEGIN
  curWriteProc := doWrite;
END GetWriteProc;

PROCEDURE Read(VAR ch: CHAR);
VAR
  done: BOOLEAN;
BEGIN
  REPEAT
    doRead(ch, done);
  UNTIL done;
END Read;

PROCEDURE ReadChar(VAR ch: CHAR; VAR done: BOOLEAN);
BEGIN
  IF KeyAvailable() THEN
    ReadKey(ch);
    done := TRUE;
  ELSE
    done := FALSE;
    ch := nul;
  END;
END ReadChar;

PROCEDURE Write(ch: CHAR);
BEGIN
  doWrite(ch);
END Write;

PROCEDURE WriteString(string: ARRAY OF CHAR);
VAR
  c, h: CARDINAL;
  ch:   CHAR;
BEGIN
  h := HIGH(string);
  c := 0;

  LOOP
    IF c > h THEN
      EXIT;
    END;

    ch := string[c];

    IF ch = 0C THEN
      EXIT;
    END;

    INC (c);

    doWrite(ch);
  END;
END WriteString;

PROCEDURE WriteLn;
BEGIN
  doWrite(EOL);
END WriteLn;

BEGIN
  doRead := ReadChar;
  doWrite := PutChar;
END Terminal.
