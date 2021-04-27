(*$INIT*)
MODULE TestInit;

FROM MiscToolSet IMPORT SysBeep2, sbYouHaveMail;

VAR
  i:  CARDINAL;
BEGIN
  FOR i := 0 TO 4 DO
    SysBeep2(sbYouHaveMail);
  END;
END TestInit.
