(*
   Conformance 6.4.1

   Arrays
 
   1. Try various legal array declarations and uses.
 
   Copyright 1993, Byte Works, Inc.
 
*)
 
MODULE Conformance;
 
FROM InOut IMPORT WriteString, WriteLn;
 
CONST
   N = 11;
 
TYPE
   Color = (red, orange, yellow, green, blue);
   WeekDay = (Monday, Tuesday, Wednesday, Thursday, Friday);
 
VAR
   A: ARRAY [0..N-1] OF CARDINAL;
   B: ARRAY[1..10],[1..20] OF INTEGER [0..99];  (* see p65 of 4th edition *)
   C: ARRAY[-10..+10] OF BOOLEAN;
   D: ARRAY WeekDay OF Color;
   E: ARRAY Color OF WeekDay;
 
   i,j: INTEGER;
   day: WeekDay;
   b: BOOLEAN;
   c: Color;
   pass: BOOLEAN;
 
BEGIN
pass := TRUE;
 
FOR i := 0 TO N-1 DO
   A[i] := i;
END;
FOR i := 0 TO N-1 DO
   IF A[i] <> CARDINAL(i) THEN
      pass := FALSE;
   END;
END;
 
FOR i := 1 TO 10 DO
   FOR j := 1 TO 20 DO
      B[i,j] := i + j*2;
   END;
END;
FOR i := 1 TO 10 DO
   FOR j := 1 TO 20 DO
      IF B[i][j] <> i + j*2 THEN
         pass := FALSE;
      END;
   END;
END;
 
b := TRUE;
FOR i := -10 TO 10 DO
   C[i] := b;
   b := NOT b;
END;
b := TRUE;
FOR i := -10 TO 10 DO
   IF C[i] <> b THEN
      pass := FALSE;
   END;
   b := NOT b;
END;
 
c := red;
FOR day := Monday TO Friday DO
   D[day] := c;
   IF c <> blue THEN
      INC(c);
   END;
END;
c := red;
FOR day := Monday TO Friday DO
   IF D[day] <> c THEN
      pass := FALSE;
   END;
   IF c <> blue THEN
      INC(c);
   END;
END;
 
day := Monday;
FOR c := red TO blue DO
   E[c] := day;
   IF day <> Friday THEN
      INC(day);
   END;
END;

c := D[Friday]; (* here simply to ensure that the compiler can handle an
                   constant enumerated type index *)

day := Monday;
FOR c := red TO blue DO
   IF E[c] <> day THEN
      pass := FALSE;
   END;
   IF day <> Friday THEN
      INC(day);
   END;
END;
 
IF pass THEN
   WriteString("Passed C6.4.1")
ELSE
   WriteString("Failed C6.4.1")
END;
WriteLn;
END Conformance.
