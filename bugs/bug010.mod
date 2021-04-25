MODULE FindChars;

FROM InOut IMPORT WriteString, WriteLn, ReadString, Write;
FROM FileSystem IMPORT File, Lookup, Response, ReadChar;

TYPE
  charSet = SET OF CHAR;

VAR
  found: charSet;
  in: File;

PROCEDURE OpenFile;
VAR
  fname: ARRAY [1..15] OF CHAR;
BEGIN
  REPEAT
    WriteString('File to scan: ');
    ReadString(fname); WriteLn;
    Lookup(in, fname, FALSE);
    IF in.res = notdone THEN
      WriteString('File not found...'); WriteLn;
    END;
  UNTIL in.res = done;
END OpenFile;

PROCEDURE FindChars;
VAR
  ch: CHAR;
BEGIN
  found := charSet{};
  ReadChar(in, ch);
  WHILE NOT in.eof DO
    (*INCL(found, ch);*)
    found := found + charSet{ch};
    ReadChar(in, ch);
  END;
END FindChars;

PROCEDURE PrintChars;
VAR
  ch: CHAR;
BEGIN
  FOR ch := ' ' TO '~' DO
    IF ch IN found THEN
      Write(ch);
    END;
  END;
END PrintChars;

BEGIN
  OpenFile;
  FindChars;
  PrintChars;
END FindChars.
