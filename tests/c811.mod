(*
 
   Conformance 8.1.1
 
   Functions as expression operands
 
   1. Functions can be used as operands in expressions.
 
   Copyright 1993, Byte Works, Inc.
 
*)
 
MODULE Conformance;

FROM InOut IMPORT WriteString, WriteLn;
 
CONST
   delta = 0.001;  (* math accuracy *)
 
VAR
   i, j: INTEGER;
   r, s: REAL;
   pass: BOOLEAN;
 
 
   PROCEDURE ip(): INTEGER;
 
   BEGIN
   RETURN 4;
   END ip;
 
   PROCEDURE pi(): REAL;
 
   BEGIN
   RETURN 3.1415926535;
   END pi;

   PROCEDURE p;
   BEGIN
     i := j * ip();
   END p;
 
BEGIN
pass := TRUE;
 
j := 6;
i := j*ip();
pass := pass AND (i = 24);
i := ip()*j;
pass := pass AND (i = 24);
i := 7*ip();
pass := pass AND (i = 28);
i := ip()*9;
pass := pass AND (i = 36);
p;
pass := pass AND (i = 24);

i := ip();
pass := pass AND (i = 4);
i := j*(4+ip());
pass := pass AND (i = 48);
 
r := 3.0;
s := r*pi();
pass := pass AND (ABS(s - 9.424777905) < delta);
s := pi()+r;
pass := pass AND (ABS(s - 6.1415926535) < delta);
 
IF pass THEN
   WriteString("Passed C8.1.1")
ELSE
   WriteString("Failed C8.1.1")
END;
WriteLn;
END Conformance.
