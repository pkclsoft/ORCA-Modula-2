IMPLEMENTATION MODULE Common;
(*
  This module contains a number of constants and types that may be used to bring
  together a number of facilities within the toolbox.  It imports certain items
  from the different toolsets to form a few useful types in order to make your
  life easier.
*)

FROM Strings IMPORT Assign, Length;
FROM SYSTEM IMPORT BYTE;

PROCEDURE ConvStrToPStr(str:  ARRAY OF CHAR; VAR PStr: String255);
(*
  OPERATION:
    Converts a C String (null terminated string) to a pascal string with a 
    length byte.
*)
BEGIN
  WITH PStr DO
    length := VAL(BYTE, Length(str));
    Assign(str, value);
  END;
END ConvStrToPStr;

END Common.
