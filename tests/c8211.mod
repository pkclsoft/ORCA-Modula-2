(*
 
   Conformance 8.2.1.1
 
   INTEGER Multiplication
 
   1. Try the * operator on various INTEGER and constant operands.
 
   Copyright 1993, Byte Works, Inc.

*)
 
MODULE Conformance;
 
FROM InOut IMPORT WriteString, WriteLn;
 
CONST
   c1 = 1;
   c400 = 400;
 
VAR
   i, j, k: INTEGER;
   pass: BOOLEAN;
 
BEGIN
pass := TRUE;
 
i := 10;
j := 20;
i := i*j;
pass := pass AND (i = 200);
i := 4*j;
pass := pass AND (i = 80);
i := j*7;
pass := pass AND (i = 140);
i := (4+j)*j;
pass := pass AND (i = 480);
i := 4*(j-7);
pass := pass AND (i = 52);
i := 4*(j-27);
pass := pass AND (i = -28);
i := c1*j;
pass := pass AND (i = j);
i := c1*c400;
pass := pass AND (i = 400);
i := 4*(-7);
pass := pass AND (i = -28);
i := 32*1000;
pass := pass AND (i = 32000);
i := (-32)*1000;
pass := pass AND (i = -32000);
i := (-10)*(-10);
pass := pass AND (i = 100);
j := -10;
k := 10;
i := j*k;
pass := pass AND (i = -100);
i := k*j;
pass := pass AND (i = -100);
k := -10;
i := j*k;
pass := pass AND (i = 100);
 
IF pass THEN
   WriteString("Passed C8.2.1.1")
ELSE
   WriteString("Failed C8.2.1.1")
END;
WriteLn;
END Conformance.
