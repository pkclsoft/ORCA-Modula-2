(*$Segment 'M2Sets'*)
(*$RangeCheck+*)
(*$OverflowCheck+*)
IMPLEMENTATION MODULE M2Sets;

(*
  This module provides a number of routines to allow the Modula-2 compiler to
  implement bit sets with 17 or more elements.

  The compiler hooks into this module whenever it needs to carry out an 
  operation on a set that is longer than 4 bytes.  Sets of 4 bytes or less can
  be manipulated using the normal operations available through the registers.

  Set Constant operations are handled by procedures beginning with SC.  Those
  procedures simply manipulate a fake set that is actually an array of up to
  16 BITSETS with special consideration to sets that are actually an odd number
  of bytes in length.

  Set operations on variables are handled by procedures beginning with SV. 
  Those procedures generate inline code where possible that will manipulate
  bitsets in memory.

  In theory, when modifying the compiler for another machine, it should be 
  possible to simply re-write the SV procedures to re-implement the large
  sets.
*)

FROM InOut IMPORT Write, WriteCard, WriteHex, WriteString, WriteLn;
FROM M2Lib IMPORT HighWORD, LoWORD;
FROM SYSTEM IMPORT ADR, BYTE;

(* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *)
(*                        Data Structures for Fake Sets                       *)
(* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *)


(* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *)
(*                           Set Constant Operations                          *)
(* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *)

PROCEDURE SCDisplaySet(VAR x: aFakeSet; name: CHAR); FORWARD;

PROCEDURE SCClearSet(VAR x: aFakeSet);
(*
  OPERATION:
    Clears the set to an empty set;
*)
VAR
  setNo:  CARDINAL;
BEGIN
  WITH x DO
    FOR setNo := 0 TO HIGH(set) DO
      set[setNo] := {};
    END;
  END;
END SCClearSet;

PROCEDURE SCComplementSet(VAR x: aFakeSet);
(*
  OPERATION:
    Reverses the state of each bit of the set.
*)
VAR
  setNo:  CARDINAL;
BEGIN
  WITH x DO
    setNo := 0;

    REPEAT
      set[setNo] := VAL(BITSET, (0FFFFH - VAL(CARDINAL, set[setNo])));
      INC(setNo);
    UNTIL setNo > ((setSize - 1) DIV 2);
  END;
END SCComplementSet;

PROCEDURE SCDisplaySet(VAR x: aFakeSet; name: CHAR);
(*
  OPERATION:
    For debugging purposes, use this procedure to display the contents of a 
    set.
*)
VAR
  setNo:  CARDINAL;
  Done:   BOOLEAN;
BEGIN
  WITH x DO
    WriteString('ADR(');
    Write(name);
    WriteString(')=');
    WriteHex(HighWORD(VAL(LONGINT, ADR(x))), 2);
    WriteHex(LoWORD(VAL(LONGINT, ADR(x))), 4);
    WriteString(', Length = ');
    WriteCard(setSize, 3);
    WriteString(' bytes.');
    WriteLn;

    Done := FALSE;
    setNo := ((setSize - 1) DIV 2);

    WriteString('Set value = <');

    IF ODD(setSize) THEN
      WriteHex(VAL(CARDINAL, set[setNo]), 2);

      IF setNo = 0 THEN
        Done := TRUE;
      ELSE
        DEC(setNo);
      END;
    END;

    WHILE NOT Done DO
      WriteHex(VAL(CARDINAL, set[setNo]), 4);

      IF setNo = 0 THEN
        Done := TRUE;
      ELSE
        DEC(setNo);
      END;
    END;

    WriteString('>');
    WriteLn;
  END;
END SCDisplaySet;

PROCEDURE SCEqual(x, y: aFakeSet): BOOLEAN;
(*
  OPERATION:
    Returns TRUE if the set defined by "x" is the same as that of "y".
*)
VAR
  setNo:  CARDINAL;
  equal:  BOOLEAN;
BEGIN
  WITH x DO
    equal := TRUE;
    setNo := 0;

    WHILE equal AND (setNo <= ((setSize - 1) DIV 2)) DO
      equal := set[setNo] = y.set[setNo];
      INC(setNo);
    END;
  END;

  RETURN equal;
END SCEqual;

PROCEDURE SCIn(bit: CARDINAL; x: aFakeSet): BOOLEAN;
(*
  OPERATION:
    Returns TRUE if the bit specified by "bit" is set in the fakeset "x".
*)
BEGIN
  WITH x DO
    IF bit < (setSize * 8) THEN
      RETURN (bit MOD 16) IN set[bit DIV 16];
    END;
  END;
END SCIn;

PROCEDURE SCLessEqual(x, y: aFakeSet): BOOLEAN;
(*
  OPERATION:
    Returns TRUE if the set defined by "x" is less than or equal to that of "y".
*)
VAR
  setNo:        CARDINAL;
  lessAndEqual: BOOLEAN;
BEGIN
  WITH x DO
    lessAndEqual := TRUE;
    setNo := 0;

    WHILE lessAndEqual AND (setNo <= ((setSize - 1) DIV 2)) DO
      lessAndEqual := set[setNo] <= y.set[setNo];
      INC(setNo);
    END;
  END;

  RETURN lessAndEqual;
END SCLessEqual;

PROCEDURE SCMinus(VAR x:  aFakeSet; y: aFakeSet);
(*
  OPERATION:
    This procedure carries out the standard M2 set operation for the "-"
    symbol, which is x = AND(x, COM(y)) .
*)
VAR
  setNo:  CARDINAL;
BEGIN
  WITH x DO
    setNo := 0;

    REPEAT
      set[setNo] := set[setNo] - y.set[setNo];
      INC(setNo);
    UNTIL setNo > ((setSize - 1) DIV 2);
  END;
END SCMinus;

PROCEDURE SCPlus(VAR x:  aFakeSet; y: aFakeSet);
(*
  OPERATION:
    This procedure carries out the standard M2 set operation for the "+"
    symbol, which is OR.

    It ORs the two sets represented by 'x' and 'y', placing the result in 'x'.
*)
VAR
  setNo:  CARDINAL;
BEGIN
  WITH x DO
    setNo := 0;

    REPEAT
      set[setNo] := set[setNo] + y.set[setNo];
      INC(setNo);
    UNTIL setNo > ((setSize - 1) DIV 2);
  END;
END SCPlus;

PROCEDURE SCSetBit(VAR x: aFakeSet; bit:  CARDINAL);
(*
  OPERATION:
    Sets the specified bit in the set.
*)
BEGIN
  WITH x DO
    IF bit < (setSize * 8) THEN
      INCL(set[bit DIV 16], bit MOD 16);
    END;
  END;
END SCSetBit;

PROCEDURE SCSetBitRange(VAR x: aFakeSet; lo, hi:  CARDINAL);
(*
  OPERATION:
    Sets the specified range of bits in the set.
*)
VAR
  bit:  CARDINAL;
BEGIN
  WITH x DO
    FOR bit := lo TO hi DO
      SCSetBit(x, bit);
    END;
  END;
END SCSetBitRange;

PROCEDURE SCSlash(VAR x:  aFakeSet; y: aFakeSet);
(*
  OPERATION:
    This procedure carries out the standard M2 set operation for the "/"
    symbol, which is EOR.

    It EORs the two sets represented by 'x' and 'y', placing the result in 'x'.
*)
VAR
  setNo:  CARDINAL;
BEGIN
  WITH x DO
    setNo := 0;

    REPEAT
      set[setNo] := set[setNo] / y.set[setNo];
      INC(setNo);
    UNTIL setNo > ((setSize - 1) DIV 2);
  END;
END SCSlash;

PROCEDURE SCTimes(VAR x:  aFakeSet; y: aFakeSet);
(*
  OPERATION:
    This procedure carries out the standard M2 set operation for the "*"
    symbol, which is AND.

    It ANDs the two sets represented by 'x' and 'y', placing the result in 'x'.
*)
VAR
  setNo:  CARDINAL;
BEGIN
  WITH x DO
    setNo := 0;

    REPEAT
      set[setNo] := set[setNo] * y.set[setNo];
      INC(setNo);
    UNTIL setNo > ((setSize - 1) DIV 2);
  END;
END SCTimes;

END M2Sets.
