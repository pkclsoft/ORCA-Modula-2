(*$Segment M2Clock*)
(*$RangeCheck+*)
(*$OverflowCheck+*)
IMPLEMENTATION MODULE M2Clock;

FROM SYSTEM IMPORT WORD, INLINE, GETREG;
FROM W65C816 IMPORT PHA, PLX, PLA, CPU;

PROCEDURE ReadTimeHex(); TOOL 0D03H;

PROCEDURE GetKeyTime(VAR t: KeyTime);
TYPE
  aDateTime=
    RECORD
      CASE :CARDINAL OF
        0:  lv:             LONGINT;
        |
        1:  lo, hi:         WORD;
        |
        2:  minute, second: CHAR;
            year, day:      CHAR;
        |
        3:  month, hour:    CHAR;
            weekday, null:  CHAR;
      END;
    END;
VAR
  hi, lo: aDateTime;
BEGIN
  INLINE(PHA, PHA, PHA, PHA);
  ReadTimeHex();
  INLINE(PLA, PLX); (* pull the low longint *)
  GETREG(CPU, lo.lv);
  INLINE(PLA, PLX); (* pull the high longint *)
  GETREG(CPU, hi.lv);

  t.day         := VAL(CARDINAL, (ORD(lo.year)*20B + ORD(hi.month))*40B +
                                  ORD(lo.day));
  t.minute      := VAL(CARDINAL, ORD(hi.hour)*60 + ORD(lo.minute));
  t.millisecond := VAL(CARDINAL, ORD(lo.second)*1000);
END GetKeyTime;

END M2Clock.

