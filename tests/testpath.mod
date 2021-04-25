(*$Keep 'TestPath'*)
(*$StackSize 16384*)
MODULE TestPath;

FROM EZPaths IMPORT FindFileInPath;
FROM InOut IMPORT WriteString, WriteLn, ReadString;
FROM Strings IMPORT Length;

VAR
  filename: ARRAY [0..512] OF CHAR;
  pathname: ARRAY [0..512] OF CHAR;
  path:     ARRAY [0..512] OF CHAR;
  variable: ARRAY [0..512] OF CHAR;
  OK:       BOOLEAN;

BEGIN
  WriteString('Path:     ');
  ReadString(path);
  WriteLn;

  WriteString('variable: ');
  ReadString(variable);
  WriteLn;

  WriteString('Filename: ');
  ReadString(filename);
  WriteLn;

  WHILE Length(filename) <> 0 DO
    pathname := '';

    IF Length(variable) <> 0 THEN
      FindFileInPath(filename, variable, TRUE, pathname, OK);
    ELSE
      FindFileInPath(filename, path, FALSE, pathname, OK);
    END;

    IF OK THEN
      WriteString('Pathname: "');
      WriteString(pathname);
      WriteString('"');
      WriteLn;
    ELSE
      WriteString('Not found in path');
      WriteLn;
    END;

    WriteString('Filename: ');
    ReadString(filename);
    WriteLn;
  END;
END TestPath.
