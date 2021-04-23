(*$Segment RealConversions*)
IMPLEMENTATION MODULE RealConversions;

FROM M2Lib IMPORT FormatLongReal, FormatReal;

CONST
  zero      =  0.0L;
  one       =  1.0L;
  ten       = 10.0L;

  maxRexp   =  38;
  minRexp   = -38;
  maxLRexp  =  308;
  minLRexp  = -306;

PROCEDURE StringToLongReal(    str:   ARRAY OF CHAR;
                           VAR r:     LONGREAL;
                           VAR okay:  BOOLEAN);
TYPE
  State = (start, numsign, before, dot, after, exp, expsign, expnum, stop);
CONST
  maxdig = 15;
VAR
  state: State;
  ch, lastChar: CHAR;
  index, c: CARDINAL;
  negNum, negExp, dotappeared: BOOLEAN;
  expVal, expCorr, nrofDigits, test, i: INTEGER;
BEGIN
  okay := FALSE;
  lastChar := 0C;
  r := zero;
  index := 0;
  negNum := FALSE;
  negExp := FALSE;
  expVal := 0;
  expCorr := 0;
  state := start;
  nrofDigits := 0;
  dotappeared := FALSE;

  LOOP
    IF index <= HIGH(str) THEN
      ch := str[index];
      INC(index);
    ELSE
      ch := 0C;
    END;

    IF (ch = '+') OR (ch = '-') THEN
      IF (state = start) OR (state = exp) THEN
        INC(state);

        IF state = numsign THEN
          negNum := ch = '-';
        ELSE
          negExp := ch = '-';
        END;
      ELSE
        EXIT;
      END;
    ELSIF ch = '.' THEN
      IF state <= before THEN
        state := dot;
        dotappeared := TRUE;
      ELSE
        EXIT;
      END;
    ELSIF (ch = 'e') OR (ch = 'E') THEN
      IF state <= after THEN
        state := exp;
      ELSE
        EXIT;
      END;
    ELSIF ('0' <= ch) AND (ch <= '9') THEN
      CASE state OF
        start:
          state := before;

          IF (ch <> 0C) AND ((ch <> '0') OR (nrofDigits <> 0)) THEN
            INC(nrofDigits);
          END;
      | numsign:
          INC(state);

          IF (ch <> 0C) AND ((ch <> '0') OR (nrofDigits <> 0)) THEN
            INC(nrofDigits);
          END;
      | before:
          IF (ch <> 0C) AND ((ch <> '0') OR (nrofDigits <> 0)) THEN
            INC(nrofDigits);
          END;
      | dot:
          INC(state);
          INC(expCorr);

          IF (ch <> 0C) AND ((ch <> '0') OR (nrofDigits <> 0)) THEN
            INC(nrofDigits);
          END;
      | after:
          INC(expCorr);

          IF (ch <> 0C) AND ((ch <> '0') OR (nrofDigits <> 0)) THEN
            INC(nrofDigits);
          END;
      | exp:
          state := expnum;
          expVal := expVal * 10 + INTEGER((ORD(ch) - ORD('0')));

          IF nrofDigits = 0 THEN
            nrofDigits := 1;
          END;
          (* invisible one in E10 == 1E10 *)
      | expsign:
          INC(state);
          expVal := expVal * 10 + INTEGER((ORD(ch) - ORD('0')));

          IF nrofDigits = 0 THEN
            nrofDigits := 1;
          END;
          (* invisible one in E+10 == 1E+10 *)
      | expnum:
          test := expVal * 10 + INTEGER((ORD(ch) - ORD('0')));

          IF negExp AND ((-test - expCorr) + nrofDigits < minLRexp) THEN
            EXIT;
          ELSIF NOT negExp AND ((test - expCorr) + nrofDigits > maxLRexp) THEN
            EXIT;
          ELSE
            expVal := test;
          END;
      ELSE
        EXIT
      END;
    ELSIF ch <= ' ' THEN
      IF (state = start) AND (ch = ' ') THEN
        (* skip leading blanks: go on *)
      ELSIF ((before <= state) AND (state <= after)) OR
            (state = expnum) THEN
        state := stop;

        IF negExp THEN
          expVal := - expVal;
        END;

        expVal := expVal - expCorr;
        negExp := expVal < 0;
        expVal := ABS(expVal);

        r := zero;
        c := 0;

        REPEAT
          ch := str[c];

          IF ('0' <= ch) AND (ch <= '9') THEN
            r := r * ten + FLOATD(ORD(ch) - ORD('0'));
          END;

          INC(c);
        UNTIL (c = index) OR (CAP(str[c-1]) = 'E');

        IF (CAP(str[c-1])='E') AND
           ((c = 1) OR ('0'>str[c-2]) OR (str[c-2]>'9')) THEN
          r := one;
        END;

        IF negExp THEN
          FOR i := 1 TO expVal DO
            r := r / ten;
          END;
        ELSE
          FOR i := 1 TO expVal DO
            r := r * ten;
          END;
        END;

        IF negNum THEN
          r := -r;
        END;

        okay := TRUE;

        EXIT;
      ELSE
        EXIT;
      END;
    ELSE
      EXIT;
    END;
  END; (* LOOP *)
END StringToLongReal;

PROCEDURE StringToReal(    str:   ARRAY OF CHAR;
                       VAR r:     REAL;
                       VAR okay:  BOOLEAN);
TYPE
  State = (start, numsign, before, dot, after, exp, expsign, expnum, stop);
CONST
  maxdig = 15;
VAR
  state: State;
  ch, lastChar: CHAR;
  index, c: CARDINAL;
  negNum, negExp, dotappeared: BOOLEAN;
  expVal, expCorr, nrofDigits, test, i: INTEGER;
BEGIN
  okay := FALSE;
  lastChar := 0C;
  r := zero;
  index := 0;
  negNum := FALSE;
  negExp := FALSE;
  expVal := 0;
  expCorr := 0;
  state := start;
  nrofDigits := 0;
  dotappeared := FALSE;

  LOOP
    IF index <= HIGH(str) THEN
      ch := str[index];
      INC(index);
    ELSE
      ch := 0C;
    END;

    IF (ch = '+') OR (ch = '-') THEN
      IF (state = start) OR (state = exp) THEN
        INC(state);

        IF state = numsign THEN
          negNum := ch = '-';
        ELSE
          negExp := ch = '-';
        END;
      ELSE
        EXIT;
      END;
    ELSIF ch = '.' THEN
      IF state <= before THEN
        state := dot;
        dotappeared := TRUE;
      ELSE
        EXIT;
      END;
    ELSIF (ch = 'e') OR (ch = 'E') THEN
      IF state <= after THEN
        state := exp;
      ELSE
        EXIT;
      END;
    ELSIF ('0' <= ch) AND (ch <= '9') THEN
      CASE state OF
        start:
          state := before;

          IF (ch <> 0C) AND ((ch <> '0') OR (nrofDigits <> 0)) THEN
            INC(nrofDigits);
          END;
      | numsign:
          INC(state);

          IF (ch <> 0C) AND ((ch <> '0') OR (nrofDigits <> 0)) THEN
            INC(nrofDigits);
          END;
      | before:
          IF (ch <> 0C) AND ((ch <> '0') OR (nrofDigits <> 0)) THEN
            INC(nrofDigits);
          END;
      | dot:
          INC(state);
          INC(expCorr);

          IF (ch <> 0C) AND ((ch <> '0') OR (nrofDigits <> 0)) THEN
            INC(nrofDigits);
          END;
      | after:
          INC(expCorr);

          IF (ch <> 0C) AND ((ch <> '0') OR (nrofDigits <> 0)) THEN
            INC(nrofDigits);
          END;
      | exp:
          state := expnum;
          expVal := expVal * 10 + INTEGER((ORD(ch) - ORD('0')));

          IF nrofDigits = 0 THEN
            nrofDigits := 1;
          END;
          (* invisible one in E10 == 1E10 *)
      | expsign:
          INC(state);
          expVal := expVal * 10 + INTEGER((ORD(ch) - ORD('0')));

          IF nrofDigits = 0 THEN
            nrofDigits := 1;
          END;
          (* invisible one in E+10 == 1E+10 *)
      | expnum:
          test := expVal * 10 + INTEGER((ORD(ch) - ORD('0')));

          IF negExp AND ((-test - expCorr) + nrofDigits < minRexp) THEN
            EXIT;
          ELSIF NOT negExp AND ((test - expCorr) + nrofDigits > maxRexp) THEN
            EXIT;
          ELSE
            expVal := test;
          END;
      ELSE
        EXIT
      END;
    ELSIF ch <= ' ' THEN
      IF (state = start) AND (ch = ' ') THEN
        (* skip leading blanks: go on *)
      ELSIF ((before <= state) AND (state <= after)) OR
            (state = expnum) THEN
        state := stop;

        IF negExp THEN
          expVal := - expVal;
        END;

        expVal := expVal - expCorr;
        negExp := expVal < 0;
        expVal := ABS(expVal);

        r := zero;
        c := 0;

        REPEAT
          ch := str[c];

          IF ('0' <= ch) AND (ch <= '9') THEN
            r := r * SHORT(ten) + FLOAT(ORD(ch) - ORD('0'));
          END;

          INC(c);
        UNTIL (c = index) OR (CAP(str[c-1]) = 'E');

        IF (CAP(str[c-1])='E') AND
           ((c = 1) OR ('0'>str[c-2]) OR (str[c-2]>'9')) THEN
          r := one;
        END;

        IF negExp THEN
          FOR i := 1 TO expVal DO
            r := r / SHORT(ten);
          END;
        ELSE
          FOR i := 1 TO expVal DO
            r := r * SHORT(ten);
          END;
        END;

        IF negNum THEN
          r := -r;
        END;

        okay := TRUE;

        EXIT;
      ELSE
        EXIT;
      END;
    ELSE
      EXIT;
    END;
  END; (* LOOP *)
END StringToReal;


(********************************************************************)

PROCEDURE LongRealToString(   r:              LONGREAL; 
                              digits, width:  INTEGER;
                          VAR str:            ARRAY OF CHAR; 
                          VAR okay:           BOOLEAN);
(*
  OPERATION:
    Convert the LONGREAL number r into a string with max characters "width", and
    "digits" digits after the decimal point.  If "width" is zero, then the 
    number is converted to exponential format.
*)
BEGIN
  FormatLongReal(r, width, digits, str);
  okay := TRUE;
END LongRealToString;

PROCEDURE RealToString(   r:              REAL; 
                          digits, width:  INTEGER;
                      VAR str:            ARRAY OF CHAR; 
                      VAR okay:           BOOLEAN);
(*
  OPERATION:
    Convert the REAL number r into a string with max characters "width", and
    "digits" digits after the decimal point.  If width is zero, then the 
    number is converted to exponential format.
*)
BEGIN
  FormatReal(r, width, digits, str);
  okay := TRUE;
END RealToString;

END RealConversions.
