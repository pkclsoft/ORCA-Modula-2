(*$NILCheck+ *)
MODULE TestNILChk;
(*
  This module tests the NIL pointer checking directive.
*)

FROM Storage IMPORT ALLOCATE, DEALLOCATE;

IMPORT EZDump;

VAR
  pc: POINTER TO CHAR;

BEGIN
  pc := NIL;

  NEW(pc);

  pc^ := 'A';

  DISPOSE(pc);

  pc^ := 'B';
END TestNILChk.
