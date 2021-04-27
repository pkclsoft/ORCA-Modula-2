(*
 
   Conformance 3.0.1
 
   Identifiers
 
   1. Identifiers are a letter followed by any number of letters or digits.
      (This test verifies that identifiers are unique to 72 characters.)
   2. By implication, there are 62 possible characters in an identifier; all
      are verified.
   3. "Capitol and lowercase letters are considered as distinct."
 
   Copyright 1993, Byte Works, Inc.
 
*)
 
MODULE Conformance;
 
FROM InOut IMPORT WriteString, WriteLn;
 
VAR
   i0123456789012345678901234567890123456789012345678901234567890123456789a:
      INTEGER;
   i0123456789012345678901234567890123456789012345678901234567890123456789b:
      INTEGER;
   abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789: INTEGER;
   a, A: INTEGER;
   pass: BOOLEAN;
 
BEGIN
pass := TRUE;
 
(* Verify that identifiers are unique to at least 72 characters. *)
i0123456789012345678901234567890123456789012345678901234567890123456789a := 1;
i0123456789012345678901234567890123456789012345678901234567890123456789b := 2;
IF i0123456789012345678901234567890123456789012345678901234567890123456789a
   <> 1 THEN
   pass := FALSE
END;
IF i0123456789012345678901234567890123456789012345678901234567890123456789b
   <> 2 THEN
   pass := FALSE
END;
 
(* Make sure all valid characters are allowed in identifiers. *)
abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 := 4;
 
(* Verify that identifiers are case sensitive. *)
a := 1;
A := 2;
 
IF (a <> 1) OR (A <> 2) THEN
   pass := FALSE
END;
 
IF pass THEN
   WriteString("Passed C3.0.1")
ELSE
   WriteString("Failed C3.0.1")
END;
WriteLn;
END Conformance.
