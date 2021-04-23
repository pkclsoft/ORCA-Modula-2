IMPLEMENTATION MODULE CDAConsole;

FROM Terminal IMPORT AssignWrite, AssignRead;

(*$Pascal+*)
PROCEDURE CDStartup;
PROCEDURE CDWrite(ch: CHAR);
PROCEDURE CDRead(VAR ch:  CHAR);
PROCEDURE CDCharacterPresent(): BOOLEAN;
(*$Pascal-*)

PROCEDURE CDAInitialise;
BEGIN
  CDStartup;
END CDAInitialise;

PROCEDURE CDAWrite(ch: CHAR);
BEGIN
  CDWrite(ch);
END CDAWrite;

PROCEDURE CDARead(VAR ch: CHAR);
BEGIN
  IF CDCharacterPresent() THEN
    CDRead(ch);
  ELSE
    ch := ASCII.nul;
  END;
END CDARead;

BEGIN
  CDAInitialise;
  AssignWrite(CDAWrite);
  AssignRead(CDARead);
END CDAConsole.
