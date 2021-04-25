(*$Segment M2EM*)
(*$RangeCheck+*)
(*$OverflowCheck+*)
IMPLEMENTATION MODULE M2EM; (* Hermann Seiler 19.2.86 / 10.6.86 / 28.4.89 *)

FROM M2Lib IMPORT aTerminateStatus;
FROM M2DM IMPORT
        ObjPtr, StrPtr, ParPtr, PDPtr, KeyPtr,
        ObjClass, StrForm, Standard, ConstValue, PDesc,
        Structure, aAdr, aSize, nilchk,
        notyp, undftyp, booltyp, chartyp, cardtyp, cardinttyp,
        inttyp, bitstyp, lcardtyp, dbltyp, realtyp, lrltyp,
        proctyp, stringtyp, addrtyp, wordtyp, bytetyp,
        WordSize, MaxChar, MaxCard, MinInt, MaxInt, MinDouble, MaxDouble;
FROM M2SM IMPORT
        Enter, Symbol, Mark, id;
FROM M2HM IMPORT
        byte, word, long, quad, StackTop, stackPointer,
        Condition, Release, PushAdr, DRegister, dontLoseIndex,
        WidType, ItemMode, ItSet, Item, curLev, GenTerminate,
        LongVal, WordVal, SimpleC, SignedT, SimpleT, RealT,
        Isz, SetregMd, SetconMd, SetglbMd, SetstkMd, SetDregMd,
        InvertCC, GenHalt, Branch, PushVal, 
        LoadAdr, LoadD, LoadX, Move, MoveAdr,
        Neg1, Abs1, Cap1, Tst1, Com1, Inc1, Dec1,
        Add2, Sub2, And2, Or2, Eor2, Div2, Mod2,
        Mul2, Cmp2, In2, ShiType, Shi2, Ash2, 
        ConvertTyp, ConIndex, VarIndex, GetHigh, Normalize,
        CheckHigh, CheckClimit, CheckRange, CheckDbltoSingle,
        direction, CheckCPUUsage, CallExt, LoadVal, GetReg, RegAdr, Unlock;
FROM M2FM IMPORT
        FMonad, FDyad;
FROM M2LM IMPORT AddBytesToObjectFile, FixLink, MergedLinks, PutOp,
        PutDPReference, PutLongReference,  PutLinkReference, newLink,
        PutStackReference;
FROM M2Sets IMPORT SCEqual, SCLessEqual, SCSlash, SCTimes, SCPlus, SCMinus,
        SCClearSet, SCSetBit, SCSetBitRange, SCIn, SCComplementSet;
FROM M2TM IMPORT Find;
FROM M2OMF IMPORT GenGlobalOp;
FROM SYSTEM IMPORT
        WORD, ADR, SHIFT;
FROM W65C816 IMPORT JSL, PEA, SEP, REP, LDA, IMMEDIATE, IMMEDIATE3, PHA,
        TAY, LSR, STA, DIRECT, TSC, CLC, ADC, TAX, TYA, And, CPY, DEY, BEQ,
        ASL, BNE, ABSLONGBYX, STX, TXA, CPX, DEX, DIRINDBYYLG, LDY, PHY, INCACC,
        DIRINDLONG, PLA, ORA, PLX, STKRELATIVE;

VAR
  mask   :  ARRAY [ 0 .. 32 ] OF LONGINT;
  wtab   :  ARRAY [ 0..wtabmax ] OF
              RECORD
                witem, mitem : Item;
              END;

PROCEDURE IncAdr(VAR adr: aAdr; s : INTEGER);
BEGIN
  IF (adr >= 0) & (s <= MaxInt - adr) OR
     (adr <  0) & (s >= MinInt - adr) THEN adr := adr + s
  ELSE Mark(211) (* address underflow/overflow *)
  END;
END IncAdr;

PROCEDURE AllocVar(obj : ObjPtr; VAR adr: aAdr);
VAR
  s: INTEGER;
BEGIN
  (* obj^.class = Var *)
  WITH obj^ DO
    s := VAL(INTEGER, typ^.size);

    vadr := adr;
    IncAdr(adr, s);
  END (*WITH*);
END AllocVar;

PROCEDURE AllocPar(par : ParPtr; VAR adr: aAdr);
CONST
  PointerSize = 4;
VAR
  s:            INTEGER;
BEGIN
  WITH par^ DO
    s := VAL(INTEGER, typ^.size);

    (* negative allocation for parameters *)
    IF (typ^.form = Array) & typ^.dyn THEN
      (* s = size of formal type for dynamic array parameter *)
      IncAdr(adr, -s);
    ELSIF varpar THEN
      IncAdr(adr, -PointerSize);
    ELSE
      IncAdr(adr, -s);
    END;
  END; (*WITH*)
END AllocPar;

PROCEDURE AllocFld(obj : ObjPtr; VAR adr: aAdr);
VAR
  s:  INTEGER;
BEGIN
  (* obj^.class = Field *)
  WITH obj^ DO
    s := VAL(INTEGER, typ^.size);
    offset := adr;
    IncAdr(adr, s)
  END (*WITH*);
END AllocFld;

PROCEDURE SRTest(VAR x : Item);
BEGIN
  WITH x DO
    IF typ^.form = Range THEN typ := typ^.RBaseTyp END;
  END (*WITH*);
END SRTest;

PROCEDURE setCC(VAR x : Item; fcc : Condition);
  (* transform all modes to 'cocMd' : *)
BEGIN
  Release(x);

  WITH x DO
    typ := booltyp; mode := cocMd; CC := fcc;
    Tjmp := 0; Fjmp := 0;
  END;
END setCC;

PROCEDURE SetBaseLimits(VAR x : Item; VAR lo, hi : INTEGER);
BEGIN
  (* x.typ^.form = Set *)

  WITH x DO
    typ := typ^.SBaseTyp;

    IF typ^.form = Enum THEN
      lo := 0; 
      hi := typ^.NofConst - 1;
    ELSIF typ^.form = Range THEN
      lo := typ^.min; 
      hi := typ^.max;
      typ := typ^.RBaseTyp;
    ELSIF typ^.form = Char THEN
      lo := 0;
      hi := 255;
    ELSE (* bitset *)
      lo := 0; 
      hi := 15;
    END;
  END (*WITH*);
END SetBaseLimits;

PROCEDURE ConnectToWith(VAR x : Item; flev : CARDINAL);
BEGIN
  IF (flev > 0) & (flev <= wtabmax) & (flev <= wlev) THEN
    (* caution : typ of item is changed. *)
    x := wtab[flev].witem;
  ELSE
    Release(x);
    SetglbMd(x, 0, undftyp);
    x.name := 1;
  END;
END ConnectToWith;

PROCEDURE GenItem(VAR x : Item; y, scope : ObjPtr);
VAR
  yy: ObjPtr;
BEGIN
  IF y <> NIL THEN
    yy := y;

    WITH x DO
      typ := y^.typ;
      CASE y^.class OF
        Const :    mode := conMd;
                   val := y^.conval;
      | Typ :      mode := typMd;
      | Var :      indir := y^.varpar;
                   mod := y^.vmod;
                   lev := y^.vlev;
                   adr := y^.vadr;
                   off := 0;
                   isParam := y^.vpar;
                   name := y^.name;
                   indexed := FALSE;
                   parent := y^.parent;
                   register := erNone;
                   notpop := FALSE;

                   IF mod = 0 THEN
                     IF lev = 0 THEN (* global *)
                       mode := glbMd;
                     ELSE (* local *)
                       mode := locMd;
                     END;
                   ELSE (* external *)
                     mode := glbMd;
                   END;

                   IF (y^.typ^.form = Array) AND
                      (y^.typ^.dyn)          THEN
                     indir := TRUE;
                   END;

      | Field :    ConnectToWith(x, scope^.name);
                   typ := y^.typ;

                   IF mode < conMd THEN
                     off := off + y^.offset;
                   END;
      | Proc :     mode := procMd;
                   proc := yy;
                   typ := notyp;
      | Tool :     mode := toolMd;
                   proc := yy;
                   typ := notyp;
      | Module :   SetglbMd(x, 0, typ);
                   name := 1;
                   Mark(107);
      | Temp :     SetglbMd(x, 0, undftyp);
                   name := 1;
                   Mark(50);
      END (*CASE*);
    END (*WITH*);
  ELSE
    SetglbMd(x, 0, undftyp);
    x.name := 1;
    Mark(50);
  END;
END GenItem;

PROCEDURE GenIndex(VAR x, y : Item);
(*
  OPERATION:
    Index the array 'x' by 'y'.

    The resulting index is placed on the stack, and 'x' is set to indicate that
    it's value is to be obtained using an index.
*)
VAR
  i:          LONGINT;
  up, low:    LONGINT;
  elsize:     CARDINAL;
  inxt, elet: StrPtr;
  z:          Item;
BEGIN
  SRTest(y);
  IF x.typ^.form = Array THEN
    elet := x.typ^.ElemTyp;
    elsize := elet^.size;

    IF x.typ^.dyn THEN
      (* dynamic array always descriptor indirect : *)
      IF (y.typ = inttyp) OR
         (y.typ = cardtyp) OR
         (y.typ = cardinttyp) THEN
        z := x;
        GetHigh(z); (* inhibit change of x *)

        (* the minimal width of an index must be word! *)
        IF (y.typ^.size = VAL(aSize, 2)) THEN
          y.typ := inttyp;
        ELSE
          Mark(109);
          Release(y);
        END;

        CheckHigh(y, z);
        Release(z);
        VarIndex(x,y,elsize);
      ELSE
        Mark(109);
      END;
    ELSE (* not dyn *)
      up := VAL(LONGINT, 0);
      low := VAL(LONGINT, 0);

      WITH x.typ^.IndexTyp^ DO
        IF form = Range THEN
          inxt := RBaseTyp;
          up := max;
          low := min;
        ELSIF form = Enum THEN
          inxt := x.typ^.IndexTyp;
          up := NofConst;
          low := VAL(LONGINT, 0);
        END;
      END (*WITH*);

      IF SimpleC(y) THEN
        (* constant index : *)
        i := LongVal(y);

        IF inxt <> y.typ THEN
          IF (i < VAL(LONGINT, 0)) OR
             ((inxt = cardtyp) & (y.typ <> inttyp)) OR
             ((inxt = inttyp) & (y.typ <> cardtyp)) THEN
            Mark(109);
          END;
        END;

        IF ((low <= i) &  (i <= up)) &
           ((low >= VAL(LONGINT, 0)) OR (i <= (VAL(LONGINT, MaxCard) + low))) THEN
          i := i - low; (* normalize index to 0 *)
        ELSE
          Mark(108);
          i := VAL(LONGINT, 0);
        END;

        (* now i >= 0 ! : *)
        IF (elsize = 0) OR
           (i <= (VAL(LONGINT, MaxCard DIV elsize))) THEN
          i := VAL(LONGINT, elsize) * i;
        ELSE
          Mark(108);
          i := VAL(LONGINT, 0);
        END;

        ConIndex(x, i);
      ELSE
        (* variable index : *)
        IF inxt <> y.typ THEN
          IF ((inxt <> cardtyp) OR (y.typ <> inttyp)) &
             ((inxt <> inttyp)  OR (y.typ <> cardtyp)) &
             (y.typ <> dbltyp) & (y.typ <> cardinttyp) THEN
            Mark(109);
          END;
        END;

        (* the width of an index must be word! *)
        IF (y.typ^.size = VAL(aSize, 2)) THEN
          IF low < 0 THEN
            y.typ := inttyp;
          ELSE
            y.typ := cardtyp;
          END;
        ELSIF inxt^.form = Enum THEN
          LoadX(y, word);
          y.typ := cardtyp;
        ELSE
          Mark(109);
        END;

        Normalize(y,low);

        (*
          After normalization, the index may be considered to be a cardinal,
          since we now have an array with bounds: 0 <= y <= (up - low).

          By turning it into a cardinal, we only end up doing unsigned 
          comparisons, which is correct, considering that it is in theory
          possible to have an upper bound that is > 32767 (which is -ve for
          an integer).
        *)
        y.typ := cardtyp;

        IF (low >= VAL(LONGINT, 0)) OR (up <= low + VAL(LONGINT, MaxCard)) THEN
          i := up - low;
        ELSE
          Mark(106);
          i := VAL(LONGINT, 0);
        END;

        (* Ensure that 'y' is within the correct range *)
        CheckClimit(y, i);

        (* now generate actual index, and place it on the stack *)
        VarIndex(x, y, elsize);
      END (*variable index*);
    END (*not dyn*);

    x.typ := elet;
  ELSE (*form <> Array*)
    Mark(109);
    Release(y);
  END;
END GenIndex;

PROCEDURE GenField(VAR x : Item; f : ObjPtr);
BEGIN
  WITH x DO
    (* typ^.form = Record *)
    IF (f <> NIL) & (f^.class = Field) THEN
      off := off + f^.offset;
      typ := f^.typ;
    ELSE
      Mark(110);
      Release(x);
      SetglbMd(x, 0, undftyp);
      name := 1;
    END;
  END (*WITH*);
END GenField;

PROCEDURE GenDeRef(VAR x : Item);
BEGIN
  WITH x DO
    IF (typ^.form = Pointer) OR (typ = addrtyp) THEN
      IF (mode < conMd) & NOT(indir) & NOT(indexed) THEN
        indir := TRUE;
        adr := adr + off;
        off := 0;
      ELSIF (mode <> conMd) THEN
        LoadD(x);
        indir := TRUE; (* LoadVal changes mode to regMd, and indir=FALSE *)
        off := 0;
      ELSE
        Mark(111); (* illegal dereferencing *)
      END;

      IF typ = addrtyp THEN
        typ := bytetyp;
      ELSE
        typ := typ^.PBaseTyp;
      END;
    ELSE
      Mark(111); (* illegal dereferencing *)
    END;
  END (*WITH*);
END GenDeRef;

PROCEDURE GenNeg(VAR x : Item);
  VAR f : StrForm; lv : LONGINT;
BEGIN SRTest(x);
  WITH x DO
    f := typ^.form;
    IF SimpleC(x) OR ((mode = conMd) & RealT(x)) THEN               (* V2.6 *)
      lv := LongVal(x);
      IF (f = Int) OR ((f = Card) & (lv <= VAL(LONGINT, 32767))) THEN
        IF WordVal(x) > MIN(INTEGER) THEN
          SetconMd(x, -lv, inttyp);
        ELSE
          Mark(201);
        END;
      ELSIF f = Double THEN
        IF lv > MIN(LONGINT) THEN
          SetconMd(x, -lv, dbltyp)
        ELSE
          Mark(201);
        END;
      ELSIF f = Real THEN
        val.R := - val.R;                                           (* V2.6 *)
      ELSIF f = LongReal THEN                                       (* V2.6 *)
        val.X := - val.X;                                           (* V2.6 *)
      ELSE Mark(112);
      END;
    ELSE (* x not a constant *)
      IF (f = Int) OR (f = Double) THEN Neg1(x)
      ELSIF (f = CardInt) THEN Neg1(x); typ := inttyp               (* V2.6 *)
      ELSIF (f = Real) OR (f = LongReal) THEN FMonad(NonStand,x)
      ELSE Mark(112);
      END;
    END;
  END (*WITH*);
END GenNeg;

PROCEDURE GenNot(VAR x : Item);
  VAR t : CARDINAL;
BEGIN SRTest(x);
  WITH x DO
    IF typ^.form = Bool THEN
      IF mode = conMd THEN
        val.B := NOT(val.B);
        SetconMd(x, VAL(LONGINT, ORD(val.B)), typ);
      ELSIF mode = cocMd THEN
        CC := InvertCC(CC);
        t := Tjmp; Tjmp := Fjmp; Fjmp := t;
      ELSE
        Tst1(x);
        setCC(x,NE);
      END;
    ELSE
      Mark(113);
    END;
  END (*WITH*);
END GenNot;

PROCEDURE GenAnd(VAR x : Item);
BEGIN SRTest(x);
  WITH x DO
    IF mode = cocMd THEN
      Branch(CC, Fjmp, Tjmp);
      FixLink(Tjmp);
    ELSIF (typ^.form = Bool) & (mode <> conMd) THEN
      Tst1(x);
      setCC(x,EQ);
      Branch(CC, Fjmp, Tjmp);
      FixLink(Tjmp);
    ELSIF typ^.form <> Bool THEN
      Mark(122);
      Release(x);
      SetconMd(x, 0, booltyp);
    END;
  END (*WITH*);
END GenAnd;

PROCEDURE GenOr(VAR x : Item);
BEGIN SRTest(x);
  WITH x DO
    IF mode = cocMd THEN
      Branch(InvertCC(CC), Tjmp, Fjmp);
      FixLink(Fjmp);
    ELSIF (typ^.form = Bool) & (mode <> conMd) THEN
      Tst1(x);
      setCC(x,NE);
      Branch(CC, Tjmp, Fjmp);
      FixLink(Fjmp);
    ELSIF typ^.form <> Bool THEN
      Mark(125);
      Release(x);
      SetconMd(x, 1, booltyp);
    END;
  END (*WITH*);
END GenOr;

PROCEDURE GenSingSet(VAR x, e : Item);
VAR
  s:          StrPtr;
  y:          Item;
  v:          INTEGER;
  lo:         INTEGER;
  hi:         INTEGER;
  i:          INTEGER;
  szx:        WidType;
  loop:       CARDINAL;
  endOfLoop:  CARDINAL;
  xadr:       DRegister;
  savexadr:   aAdr;

BEGIN (* x.typ^.form = Set *)
  SRTest(e);
  Isz(x, szx);
  s := x.typ; (* hold original type of set *)
  SetBaseLimits(x,lo,hi);

  IF (e.typ = x.typ) OR                                             (* V2.6 *)
     (((e.typ=inttyp) OR (e.typ=cardinttyp)) & (s = bitstyp)) THEN  (* V2.6 *)
    IF SimpleC(e) THEN (* e is constant *)
      v := WordVal(e);

      IF (v >= lo) & (v <= hi) THEN
        WITH x DO
          mode  := conMd;
          typ := s;

          val.FS.setSize := s^.size;
          SCClearSet(val.FS);
          SCSetBit(val.FS, v);
        END;
      ELSE
        WITH x DO
          mode  := conMd;
          typ := s;

          val.FS.setSize := s^.size;
          SCClearSet(val.FS);
        END;

        Mark(202);
      END
    ELSE (* e is expression *)
      Mark(418);
    END;
  ELSE
    Mark(116);
  END;

  Release(e);
END GenSingSet;

PROCEDURE GenSet(VAR x, e1, e2 : Item);
VAR
  s:              StrPtr;
  y:              Item;
  lo, hi, v1, v2: INTEGER;
  lv:             LONGINT;
  szx:            WidType;
BEGIN (* x.typ^.form = Set *)
  SRTest(e1);
  SRTest(e2);
  s := x.typ; (* hold original type of set *)
  SetBaseLimits(x,lo,hi);
  Isz(x, szx);

  IF (e1.typ = x.typ) & (e2.typ = x.typ) OR
     ( ((e1.typ = inttyp) OR (e1.typ = cardinttyp)) &               (* V2.6 *)
       ((e2.typ = inttyp) OR (e2.typ = cardinttyp)) &               (* V2.6 *)
       (s = bitstyp) ) THEN                                         (* V2.6 *)
    IF SimpleC(e1) & SimpleC(e2) THEN
      (* constant set-constructor : *)
      v1 := WordVal(e1);
      v2 := WordVal(e2);

      IF ((lo <= v2) & (v2 <= hi)) & (v1 <= v2) & (hi <= 255) THEN
        WITH x DO
          typ := s;
          mode := conMd;

          val.FS.setSize := s^.size;
          SCClearSet(val.FS);
          SCSetBitRange(val.FS, v1, v2);
        END;
      ELSE
        SetconMd(x, 0, s);
        Mark(202);
      END
    ELSE
      Mark(417);
    END;
  ELSE
    Mark(116);
  END;

  Release(e1);
  Release(e2);
END GenSet;

PROCEDURE GenIn(VAR x, y : Item);
VAR
  f:          StrForm;
  s:          StrPtr;
  v, lo, hi:  INTEGER;
  b:          BOOLEAN;
BEGIN
  SRTest(x);
  f := x.typ^.form;
  s := y.typ; (* hold original type of set *)

  IF ((Bool <= f) & (f <= Enum)) & (y.typ^.form = Set) THEN
    SetBaseLimits(y,lo,hi);

    IF (x.typ = y.typ) OR (x.typ = inttyp) & (s = bitstyp)          (* V2.6 *)
                       OR (x.typ = cardinttyp) & (s = bitstyp) THEN (* V2.6 *)
      y.typ := s; (* recover type of set *)

      IF SimpleC(x) THEN (* constant element x *)
        v := WordVal(x);

        IF (v >= lo) & (v <= hi) & (hi <= 255) THEN
          IF y.mode = conMd THEN (* constant set y *)
            b := SCIn(v, y.val.FS);
            SetconMd(x, VAL(LONGINT, ORD(b)), booltyp);
          ELSE (* variable set y *)
            In2(x,y);
            setCC(x,EQ);
          END;
        ELSE
          SetconMd(x, 0, booltyp);
          Mark(202);
        END;
      ELSE (* variable element x *)
        CheckRange(x,lo,hi,0);
        In2(x,y);
        setCC(x,EQ);
      END;
    ELSE
      Release(x);
      SetconMd(x, 0, booltyp);
      Mark(114);
    END;
  ELSE
    Release(x);
    SetconMd(x, 0, booltyp);
    Mark(115);
  END;

  x.typ := booltyp;
  Release(y);
END GenIn;

PROCEDURE GenWith(VAR x : Item; VAR fadr: aAdr);
VAR
  xt:   StrPtr;
BEGIN
  INC(wlev);

  IF wlev > wtabmax THEN
    Mark(236);
    Release(x);
    RETURN;
  END;

  xt := x.typ; (* holds original type of record *)

  WITH wtab[wlev] DO
    x.typ := xt;

    Release(x);
    PushAdr(x);
    x.notpop := TRUE;
    x.indir := TRUE;

    witem := x;
  END (*WITH wtab*);
END GenWith;

PROCEDURE GenWith2;
BEGIN
  IF wlev > 0 THEN
    IF wlev <= wtabmax THEN
      WITH wtab[wlev] DO
        IF witem.notpop THEN
          StackTop(4);  (* Pull the address back off the stack *)
        END;
      END (*WITH wtab*);
    END;

    DEC(wlev);
  END;
END GenWith2;

PROCEDURE InitM2EM;
  VAR k : CARDINAL; exp : LONGINT;
BEGIN
  wlev := 0;
  exp := 0; 
  mask[0] := 0; 
  mask[32] := -1;

  FOR k := 1 TO 31 DO 
    exp := exp + exp + VAL(LONGINT, 1); 
    mask[k] := exp;
  END;
END InitM2EM;

PROCEDURE GenOp(op : Symbol; VAR x, y : Item);
  VAR f, g : StrForm; c : Condition;
      xval, yval : ConstValue; xx : Item;
BEGIN
  SRTest(x); f := x.typ^.form;
  SRTest(y); g := y.typ^.form;

  IF x.typ <> y.typ THEN
    IF ((f = Int) OR (f = CardInt)) & (g = Card) & (y.mode = conMd)
     & (y.val.C <= MaxInt) THEN y.typ := x.typ
    ELSIF (f = Card) & ((g = Int) OR (g = CardInt)) & (x.mode = conMd)
     & (x.val.C <= MaxInt) THEN x.typ := y.typ; f := Int
    ELSIF (f = Double) & (g = LCard) & (x.mode = conMd) THEN        (* V2.6zz*)
      x.typ := y.typ; f := LCard                                    (* V2.6zz*)
    ELSIF (f = LCard) & (g = Double) & (y.mode = conMd) THEN        (* V2.6zz*)
      y.typ := x.typ                                                (* V2.6zz*)
    ELSIF (x.typ = addrtyp) & (g = Pointer) THEN f := Pointer
    ELSIF (g = CardInt) & ((f = Int) OR (f = Card)) THEN
      g := f; y.typ := x.typ
    ELSIF (f = CardInt) & ((g = Int) OR (g = Card)) THEN
      f := g; x.typ := y.typ
    ELSIF ((f = LCard) OR (f = Double)) &
          ((y.mode = conMd) & ((g = Int) OR (g = Card) OR (g = CardInt))) THEN
      g := f;
      SetconMd(y, LongVal(y), x.typ);
    ELSIF ((g = LCard) OR (g = Double)) &
          ((x.mode = conMd) & ((f = Int) OR (f = Card) OR (f = CardInt))) THEN
      f := g;
      SetconMd(x, LongVal(x), y.typ);
    ELSIF ((f <> Pointer) OR (y.typ <> addrtyp)) &
          ((f <> Double) OR (g <> Double)) THEN Mark(117);
    END;
  END;

  IF (x.mode = conMd) & (y.mode = conMd) THEN                       (* V2.6 *)
    xval := x.val; yval := y.val;
    CASE op OF
      times:  IF f = Card THEN
                IF (xval.C = 0) OR (yval.C <= MaxCard DIV xval.C) THEN
                  xval.C := xval.C * yval.C;
                ELSE
                  Mark(203);
                END;
              ELSIF f = Int THEN
                IF (xval.I = 0) OR
                   ((y.val.I <> MIN(INTEGER)) & (x.val.I <> MIN(INTEGER)) &
                   (ABS(yval.I) <= MaxInt DIV ABS(xval.I))) THEN
                  xval.I := xval.I * yval.I;
                ELSE
                  Mark(203);
                END;
  (* The next three components of the if statement have been changed to *)
  (* avoid a possible divide by zero problem.  Instead of assuming that *)
  (* the compiler compiling this code doesn't go past the first true    *)
  (* check in an if statement, we must separate the (val = 0) condition *)
  (* from the rest of the conditions.                                   *)
              ELSIF f = Double THEN
                IF (xval.D = VAL(LONGINT, 0)) THEN
                  xval.D := xval.D * yval.D;
                ELSIF ((y.val.D <> MIN(LONGINT)) &
                       (x.val.D <> MIN(LONGINT)) &
                       (ABS(yval.D) <= MaxDouble DIV ABS(xval.D))) THEN
                  xval.D := xval.D * yval.D;
                ELSE
                  Mark(203);
                END
              ELSIF f = Real THEN
                IF (xval.R = 0.0) THEN
                  xval.R := xval.R * yval.R;
                ELSIF (ABS(xval.R) <= FLOAT(1)) OR
                      (ABS(yval.R) <= ABS(MAX(REAL)) / ABS(xval.R)) THEN
                  xval.R := xval.R * yval.R;
                ELSE Mark(203)
                END
              ELSIF f = LongReal THEN                               (* V2.6 *)
                IF (xval.X = FLOATD(0)) THEN
                  xval.X := xval.X * yval.X;                        (* V2.6 *)
                ELSIF (ABS(xval.X) <= FLOATD(1)) OR (* V2.6 *)
                      (ABS(yval.X) <= ABS(MAX(LONGREAL))
                       / ABS(xval.X)) THEN (* V2.6 *)
                  xval.X := xval.X * yval.X;                        (* V2.6 *)
                ELSE Mark(203)                                       (* V2.6 *)
                END                                                 (* V2.6 *)
              ELSIF f = Set THEN
                SCTimes(xval.FS, yval.FS);
              ELSE Mark(118)
              END;

    | slash:  IF f = Real THEN                                      (* V2.6E*)
                IF (ABS(yval.R) >= FLOAT(1)) OR ((yval.R <> 0.0) &  (* V2.6E*)
                   (ABS(xval.R) <= ABS(yval.R) * ABS(MAX(REAL)))) THEN
                  xval.R := xval.R / yval.R;
                ELSE Mark(204)
                END
              ELSIF f = LongReal THEN                               (* V2.6 *)
                IF (ABS(yval.X) >= FLOATD(1)) OR ((yval.X <> FLOATD(0)) & (* V2.6 *)
                   (ABS(xval.X) <= ABS(yval.X) * ABS(MAX(LONGREAL)))) THEN (* V2.6 *)
                  xval.X := xval.X / yval.X;                        (* V2.6 *)
                ELSE Mark(204)                                       (* V2.6 *)
                END                                                 (* V2.6 *)
              ELSIF f = Set THEN
                SCSlash(xval.FS, yval.FS);
              ELSE Mark(119)
              END;

    | div:    IF f = Card THEN
                IF yval.C > 0 THEN
                  xval.C := xval.C DIV yval.C;
                ELSE Mark(205)
                END
              ELSIF f = Int THEN
                IF yval.I <> 0 THEN
                  xval.I := xval.I DIV yval.I;
                ELSE Mark(205)
                END
              ELSIF f = Double THEN
                IF yval.D <> VAL(LONGINT, 0) THEN
                  xval.D := xval.D DIV yval.D;
                ELSE Mark(205)
                END
              ELSE Mark(120)
              END;

    | rem:    IF (f = Int) OR (f = Card) OR (f = Double) THEN       (* V2.6 *)
                Mark(200)                                            (* V2.6 *)
              ELSIF (f = Real) OR (f = LongReal) THEN               (* V2.6 *)
                Mark(200)                                            (* V2.6 *)
              ELSE Mark(121)                                         (* V2.6 *)
              END;                                                  (* V2.6 *)

    | mod:    IF f = Card THEN
                IF yval.C > 0 THEN
                  xval.C := xval.C MOD yval.C;
                ELSE Mark(205)
                END
              ELSIF f = Int THEN
                IF (xval.I >= 0) & (yval.I > 0) THEN
                  xval.I := xval.I MOD yval.I;
                ELSE Mark(205)
                END
              ELSIF f = Double THEN
                IF (xval.D >= VAL(LONGINT, 0)) & (yval.D > VAL(LONGINT, 0)) THEN
                  xval.D := xval.D MOD yval.D;
                ELSE Mark(205)
                END
              ELSIF (f = Real) OR (f = LongReal) THEN               (* V2.6 *)
                Mark(200)                                            (* V2.6 *)
              ELSE Mark(121)
              END;

    | and:    IF f = Bool THEN xval.B := xval.B AND yval.B
              ELSE Mark(122)
              END;

    | plus:   IF f = Card THEN
                IF yval.C <= MaxCard - xval.C THEN
                  xval.C := xval.C + yval.C;
                ELSE Mark(206)
                END
              ELSIF f = Int THEN
                IF (xval.I >= 0) & (yval.I <= MaxInt - xval.I) OR
                   (xval.I <  0) & (yval.I >= MIN(INTEGER) - xval.I) THEN
                  xval.I := xval.I + yval.I;
                ELSE Mark(206)
                END
              ELSIF f = Real THEN
                IF (xval.R >= 0.0) & (yval.R <= ABS(MAX(REAL)) - xval.R) OR
                   (xval.R <  0.0) & (yval.R >= MIN(REAL) - xval.R) THEN
                  xval.R := xval.R + yval.R;
                ELSE Mark(206)
                END
              ELSIF f = LongReal THEN                               (* V2.6 *)
                IF (xval.X >= FLOATD(0)) & (yval.X <= ABS(MAX(LONGREAL)) - xval.X) OR
                   (xval.X <  FLOATD(0)) & (yval.X >= MIN(LONGREAL) - xval.X) THEN
                  xval.X := xval.X + yval.X;                        (* V2.6 *)
                ELSE Mark(206)                                       (* V2.6 *)
                END                                                 (* V2.6 *)
              ELSIF f = Set THEN
                SCPlus(xval.FS, yval.FS);
              ELSIF (f = Double) OR (f = LCard) THEN
                xval.D := xval.D + yval.D;
              ELSE Mark(123)
              END;

    | minus:  IF f = Card THEN
                IF yval.C <= xval.C THEN xval.C := xval.C - yval.C
                ELSIF yval.C - xval.C <= MaxInt THEN
                  xval.I := - VAL(INTEGER, yval.C - xval.C);
                  x.typ := inttyp; (* typ is changed! *)
                ELSE Mark(207)
                END
              ELSIF f = Int THEN
                IF (xval.I >= 0) &
                   ((yval.I >= 0) OR (xval.I <= MaxInt + yval.I)) OR
                   (xval.I < 0) &
                   ((yval.I <  0) OR (xval.I >= MIN(INTEGER) + yval.I)) THEN
                  xval.I := xval.I - yval.I;
                ELSE Mark(207)
                END
              ELSIF f = Real THEN
                IF (xval.R >= 0.0) &
                ((yval.R >= 0.0) OR (xval.R <= ABS(MAX(REAL)) + yval.R)) THEN
                  xval.R := xval.R - yval.R;
                ELSIF (xval.R < 0.0) &
                ((yval.R <  0.0) OR (xval.R >= MIN(REAL) + yval.R)) THEN
                  xval.R := xval.R - yval.R;
                ELSE Mark(207)
                END
              ELSIF f = LongReal THEN                               (* V2.6 *)
                IF (xval.X >= FLOATD(0)) &                       (* V2.6 *)
                ((yval.X >= FLOATD(0)) OR (xval.X <= ABS(MAX(LONGREAL)) + yval.X)) THEN
                  xval.X := xval.X - yval.X;                        (* V2.6 *)
                ELSIF (xval.X < FLOATD(0)) &                             (* V2.6 *)
                ((yval.X < FLOATD(0)) OR (xval.X >= MIN(LONGREAL) + yval.X)) THEN
                  xval.X := xval.X - yval.X;                        (* V2.6 *)
                ELSE Mark(207)                                       (* V2.6 *)
                END                                                 (* V2.6 *)
              ELSIF f = Set THEN
                SCMinus(xval.FS, yval.FS);
              ELSIF (f = Double) OR (f = LCard) THEN
                xval.D := xval.D - yval.D;
              ELSE Mark(124)
              END;

    | or:     IF f = Bool THEN xval.B := xval.B OR yval.B
              ELSE Mark(125)
              END;

    | eql:    IF    f = Card   THEN xval.B := xval.C  =  yval.C;
              ELSIF f = Int    THEN xval.B := xval.I  =  yval.I;
              ELSIF f = Real   THEN xval.B := xval.R  =  yval.R;
              ELSIF f = Bool   THEN xval.B := xval.B  =  yval.B;
              ELSIF f = Char   THEN xval.B := xval.Ch =  yval.Ch;
              ELSIF f = Double THEN xval.B := xval.D  =  yval.D;
              ELSIF f = Set    THEN xval.B := SCEqual(xval.FS, yval.FS);
              ELSIF f = Enum   THEN xval.B := xval.Ch =  yval.Ch;    (* V2.6 *)
              ELSIF f = LongReal THEN xval.B := xval.X = yval.X;     (* V2.6 *)
              ELSE Mark(126)
              END;
              x.typ := booltyp;

    | neq:    IF    f = Card   THEN xval.B := xval.C  <>  yval.C;
              ELSIF f = Int    THEN xval.B := xval.I  <>  yval.I;
              ELSIF f = Real   THEN xval.B := xval.R  <>  yval.R;
              ELSIF f = Bool   THEN xval.B := xval.B  <>  yval.B;
              ELSIF f = Char   THEN xval.B := xval.Ch <>  yval.Ch;
              ELSIF f = Double THEN xval.B := xval.D  <>  yval.D;
              ELSIF f = Set    THEN xval.B := NOT SCEqual(xval.FS, yval.FS);
              ELSIF f = Enum   THEN xval.B := xval.Ch <>  yval.Ch;   (* V2.6 *)
              ELSIF f = LongReal THEN xval.B := xval.X <> yval.X;    (* V2.6 *)
              ELSE Mark(126)
              END;
              x.typ := booltyp;
  
    | lss:    IF    f = Card   THEN xval.B := xval.C  <   yval.C;
              ELSIF f = Int    THEN xval.B := xval.I  <   yval.I;
              ELSIF f = Real   THEN xval.B := xval.R  <   yval.R;
              ELSIF f = Bool   THEN xval.B := xval.B  <   yval.B;
              ELSIF f = Char   THEN xval.B := xval.Ch <   yval.Ch;
              ELSIF f = Double THEN xval.B := xval.D  <   yval.D;
              ELSIF f = Enum   THEN xval.B := xval.Ch <   yval.Ch;   (* V2.6 *)
              ELSIF f = LongReal THEN xval.B := xval.X <  yval.X;    (* V2.6 *)
              ELSE Mark(126)
              END;
              x.typ := booltyp;

    | leq:    IF    f = Card   THEN xval.B := xval.C  <=  yval.C;
              ELSIF f = Int    THEN xval.B := xval.I  <=  yval.I;
              ELSIF f = Real   THEN xval.B := xval.R  <=  yval.R;
              ELSIF f = Bool   THEN xval.B := xval.B  <=  yval.B;
              ELSIF f = Char   THEN xval.B := xval.Ch <=  yval.Ch;
              ELSIF f = Double THEN xval.B := xval.D  <=  yval.D;
              ELSIF f = Set    THEN xval.B := SCLessEqual(xval.FS, yval.FS);
              ELSIF f = Enum   THEN xval.B := xval.Ch <=  yval.Ch;   (* V2.6 *)
              ELSIF f = LongReal THEN xval.B := xval.X <= yval.X;    (* V2.6 *)
              ELSE Mark(126)
              END;
              x.typ := booltyp;

    | gtr:    IF    f = Card   THEN xval.B := xval.C   >  yval.C;
              ELSIF f = Int    THEN xval.B := xval.I   >  yval.I;
              ELSIF f = Real   THEN xval.B := xval.R   >  yval.R;
              ELSIF f = Bool   THEN xval.B := xval.B   >  yval.B;
              ELSIF f = Char   THEN xval.B := xval.Ch  >  yval.Ch;
              ELSIF f = Double THEN xval.B := xval.D   >  yval.D;
              ELSIF f = Enum   THEN xval.B := xval.Ch  >  yval.Ch;   (* V2.6 *)
              ELSIF f = LongReal THEN xval.B := xval.X  > yval.X;    (* V2.6 *)
              ELSE Mark(126)
              END;
              x.typ := booltyp;

    | geq:    IF    f = Card   THEN xval.B := xval.C  >=  yval.C;
              ELSIF f = Int    THEN xval.B := xval.I  >=  yval.I;
              ELSIF f = Real   THEN xval.B := xval.R  >=  yval.R;
              ELSIF f = Bool   THEN xval.B := xval.B  >=  yval.B;
              ELSIF f = Char   THEN xval.B := xval.Ch >=  yval.Ch;
              ELSIF f = Double THEN xval.B := xval.D  >=  yval.D;
              ELSIF f = Set    THEN xval.B := NOT SCLessEqual(xval.FS, yval.FS);
              ELSIF f = Enum   THEN xval.B := xval.Ch >=  yval.Ch;   (* V2.6 *)
              ELSIF f = LongReal THEN xval.B := xval.X >= yval.X;    (* V2.6 *)
              ELSE Mark(126)
              END;
              x.typ := booltyp;
    END (*CASE op*);

    x.val := xval;
  ELSE (* NOT(x.mode = conMd) & (y.mode = conMd)) *)
    CASE op OF
      times:  IF (f = Int) OR (f = Card) OR
                 (f = CardInt) OR (f = Double) OR (f = LCard) THEN
                Mul2(x,y)
              ELSIF (f = Real) OR (f = LongReal) THEN
                FDyad(times,x,y)
              ELSIF f = Set THEN
                And2(x,y)
              ELSE Mark(118)
              END;

    | slash:  IF (f = Real) OR (f = LongReal) THEN                  (* V2.6 *)
                FDyad(slash,x,y)
              ELSIF f = Set THEN
                Eor2(x,y)
              ELSE Mark(119)
              END;

    | div:    IF (f = Int) OR (f = Card) OR
                 (f = CardInt) OR (f = Double) OR (f = LCard) THEN
                Div2(x,y)
              ELSE Mark(120)
              END;

    | rem:    IF (f = Int) OR (f = Card) OR
                 (f = CardInt) OR (f = Double) OR (f = LCard) THEN
                Mark(200)
              ELSIF (f = Real) OR (f = LongReal) THEN
                FDyad(rem,x,y)
              ELSE Mark(121)
              END;

    | mod:    IF (f = Int) OR (f = Card) OR
                 (f = CardInt) OR (f = Double) OR (f = LCard) THEN
                Mod2(x,y)
              ELSIF (f = Real) OR (f = LongReal) THEN
                FDyad(mod,x,y)
              ELSE Mark(121)
              END;

    | and:    IF x.mode = conMd THEN
                IF x.val.B THEN
                  IF y.mode <= conMd THEN
                    Tst1(y); setCC(y,EQ);
                  ELSIF y.mode <> cocMd THEN Mark(122)
                  END;
                ELSE setCC(y,T);
                END;
                setCC(x,EQ);
              ELSIF y.mode <> cocMd THEN
                IF y.typ^.form = Bool THEN
                  IF y.mode = conMd THEN
                    IF y.val.B THEN setCC(y,F) ELSE setCC(y,T) END;
                  ELSIF y.mode <= conMd THEN
                    Tst1(y); setCC(y,EQ);
                  ELSE Mark(122); setCC(y,EQ);
                  END;
                ELSE Mark(122); setCC(y,EQ);
                END;
              END;

              IF y.Fjmp <> 0 THEN
                x.Fjmp := MergedLinks(x.Fjmp, y.Fjmp);
              END;
              x.CC := y.CC; x.Tjmp := y.Tjmp;

    | plus:   IF (f = Int) OR (f = Card) OR
                 (f = CardInt) OR (f = Double) OR (f = LCard) THEN
                Add2(x,y)
              ELSIF (f = Real) OR (f = LongReal) THEN
                FDyad(plus,x,y)
              ELSIF f = Set THEN
                Or2(x,y)
              ELSE Mark(123)
              END;

    | minus:  IF (f = Int) OR (f = Card) OR
                 (f = CardInt) OR (f = Double) OR (f = LCard) THEN
                Sub2(x,y)
              ELSIF (f = Real) OR (f = LongReal) THEN
                FDyad(minus,x,y)
              ELSIF f = Set THEN
                IF y.mode = conMd THEN
                  SCComplementSet(y.val.FS);
                ELSE
                  Com1(y);
                END;

                And2(x,y);
              ELSE Mark(124)
              END;

    | or:     IF x.mode = conMd THEN
                IF NOT x.val.B THEN
                  IF y.mode <= conMd THEN
                    Tst1(y); setCC(y,EQ);
                  ELSIF y.mode <> cocMd THEN Mark(125)
                  END;
                ELSE setCC(y,F);
                END;
                setCC(x,EQ);
              ELSIF y.mode <> cocMd THEN
                IF y.typ^.form = Bool THEN
                  IF y.mode = conMd THEN
                    IF y.val.B THEN setCC(y,F) ELSE setCC(y,T) END;
                  ELSIF y.mode <= conMd THEN
                    Tst1(y); setCC(y,EQ);
                  ELSE Mark(125); setCC(y,EQ);
                  END;
                ELSE Mark(125); setCC(y,EQ);
                END;
              END;

              IF y.Tjmp <> 0 THEN
                x.Tjmp := MergedLinks(x.Tjmp, y.Tjmp);
              END;
              x.CC := y.CC; x.Fjmp := y.Fjmp;

    | eql:    IF (f <= Double) OR (f = Set) OR (f = ProcTyp) OR
                 (f = Pointer) OR (f = Opaque) THEN
                Cmp2(x,y); c := NE;
              ELSIF (f = Real) OR (f = LongReal) THEN
                FDyad(eql,x,y); c := FNE;
              ELSE
                Mark(126); c := NE;
              END;
              setCC(x,c);

    | neq:    IF (f <= Double) OR (f = Set) OR (f = ProcTyp) OR
                 (f = Pointer) OR (f = Opaque) THEN
                Cmp2(x,y); c := EQ;
              ELSIF (f = Real) OR (f = LongReal) THEN
                FDyad(neq,x,y); c := FEQ;
              ELSE
                Mark(126); c := EQ;
              END;
              setCC(x,c);

    | lss:    IF (f = Int) OR (f = Double) THEN
                Cmp2(x,y); c := GE;
              ELSIF f <= LCard THEN
                Cmp2(x,y); c := CS;
              ELSIF (f = Real) OR (f = LongReal) THEN
                FDyad(lss,x,y); c := FNLT;
              ELSE
                Mark(126); c := CS;
              END;
              setCC(x,c);

    | leq:    IF (f = Int) OR (f = Double) THEN
                Cmp2(x,y); c := GT;
              ELSIF f <= LCard THEN
                Cmp2(x,y); c := HI;
              ELSIF (f = Real) OR (f = LongReal) THEN
                FDyad(leq,x,y); c := FNLE;
              ELSIF f = Set THEN
                LoadD(x); Com1(y); And2(x,y); c := NE;
              ELSE
                Mark(126); c := HI;
              END;
              setCC(x,c);

    | gtr:    IF (f = Int) OR (f = Double) THEN
                Cmp2(x,y); c := LE;
              ELSIF f <= LCard THEN
                Cmp2(x,y); c := LS;
              ELSIF (f = Real) OR (f = LongReal) THEN
                FDyad(gtr,x,y); c := FNGT;
              ELSE
                Mark(126); c := LS;
              END;
              setCC(x,c);

    | geq:    IF (f = Int) OR (f = Double) THEN
                Cmp2(x,y); c := LT;
              ELSIF f <= LCard THEN
                Cmp2(x,y); c := CC;
              ELSIF (f = Real) OR (f = LongReal) THEN
                FDyad(geq,x,y); c := FNGE;
              ELSIF f = Set THEN
                LoadD(x); Com1(x); And2(x,y); c := NE;
              ELSE
                Mark(126); c := CC;
              END;
              setCC(x,c);
    END (*CASE op*);

    Release(y);
  END;
END GenOp;

PROCEDURE GenStParam(VAR p, x : Item; fctno : Standard;
                     parno : CARDINAL; morepar : BOOLEAN);
VAR
  restyp:     StrPtr;
  htyp:       StrPtr;
  f:          StrForm;
  y, z:       Item;
  sz, hsz:    WidType;
  shi:        ShiType;
  li:         LONGINT;
  i, lo, hi:  INTEGER;
  function:   Standard;
  r:          CARDINAL;
  ids, idn:   INTEGER;
  obj:        ObjPtr;
  par:        ParPtr;

  PROCEDURE FirstParam;
  VAR
    reg:    DRegister;
    notNIL: CARDINAL;
  BEGIN
    WITH x DO
      restyp := p.proc^.typ;
  
      IF (function <> Max) & (function <> Min) THEN
        SRTest(x);
      END;
  
      f := typ^.form;
  
      CASE function OF
        Abs:    restyp := typ; (* same type as argument type! *)
                IF (f = Int) OR (f = Double) THEN                  (* V2.6y *)
                  IF mode = conMd THEN                             (* V2.6y *)
                    IF (f = Int) & (val.I = MinInt) THEN           (* V2.6y *)
                      Mark(201); val.I := 1;                        (* V2.6y *)
                    ELSIF (f = Double) & (val.D = MinDouble) THEN  (* V2.6y *)
                      Mark(201); val.D := 1;                       (* V2.6y *)
                    END;                                           (* V2.6y *)
                    SetconMd(x, ABS(LongVal(x)), typ)              (* V2.6y *)
                  ELSE                                             (* V2.6y *)
                    Abs1(x)                                        (* V2.6y *)
                  END                                              (* V2.6y *)
                ELSIF (f = Real) OR (f = LongReal) THEN            (* V2.6 *)
                  IF mode = conMd THEN                             (* V2.6 *)
                    IF f = Real THEN                               (* V2.6 *)
                      val.R := ABS(val.R);                         (* V2.6 *)
                      typ := realtyp;                              (* V2.6 *)
                    ELSE (* LongReal *)                            (* V2.6 *)
                      val.X := ABS(val.X);                         (* V2.6 *)
                      typ := lrltyp;                               (* V2.6 *)
                    END                                            (* V2.6 *)
                  ELSE FMonad(Abs,x)                               (* V2.6 *)
                  END                                              (* V2.6 *)
                ELSE Mark(144)
                END;
  
      | Adr:    LoadAdr(x);
  
      | Cap:    IF typ = chartyp THEN                              (* V2.6y *)
                  IF mode = conMd THEN val.Ch := CAP(val.Ch)       (* V2.6y *)
                  ELSE Cap1(x)                                     (* V2.6y *)
                  END                                              (* V2.6y *)
                ELSE Mark(144)
                END;
  
      | Chr:    IF (f <> Undef) & (f <= Enum) THEN                  (* V2.6 *)
                  (* now range checks included ! *)                 (* V2.6 *)
                  IF mode = conMd THEN                              (* V2.6 *)
                    li := LongVal(x);                               (* V2.6 *)
                    IF (li < VAL(LONGINT, 0)) OR (li > VAL(LONGINT, MaxChar)) THEN       (* V2.6 *)
                      Mark(138); li := 0;                           (* V2.6 *)
                    END;                                            (* V2.6 *)
                    SetconMd(x, li, chartyp);                       (* V2.6 *)
                  ELSE                                              (* V2.6 *)
                    LoadX(x,word);                                  (* V2.6 *)
                    typ := inttyp; CheckClimit(x, ORD(MaxChar));    (* V2.6y*)
                  END;                                              (* V2.6 *)
                ELSE Mark(144)
                END;
  
      | Float:  IF (f = Int) OR (f = Card) OR (f = CardInt) OR      (* V2.6 *)
                   (f = Double) THEN                                (* V2.6 *)
                  IF mode = conMd THEN                              (* V2.6 *)
                    li := LongVal(x); val.R := FLOAT(li);           (* V2.6 *)
                    typ := realtyp;                                 (* V2.6 *)
                  ELSE (* variable *)                               (* V2.6 *)
                    IF (f = Card) OR (f = CardInt) THEN             (* V2.6 *)
                      LoadX(x,long); (* convert to a signed type *) (* V2.6 *)
                      typ := dbltyp;                                (* V2.6 *)
                    END;                                            (* V2.6 *)
                    (* M2TM defines the resulting type! *)          (* V2.6 *)
                    FMonad(function,x)                                 (* V2.6 *)
                  END                                               (* V2.6 *)
                ELSE Mark(144)
                END;
  
      | FloatD: IF (f = Int) OR (f = Card) OR (f = CardInt) OR     (* V2.6z *)
                   (f = Double) THEN                               (* V2.6z *)
                  IF (f = Card) OR (f = CardInt) THEN              (* V2.6z *)
                    LoadX(x,long); (* convert to a signed type *)  (* V2.6z *)
                    typ := dbltyp;                                 (* V2.6z *)
                  END;                                             (* V2.6z *)
                  (* M2TM defines the resulting type! *)           (* V2.6z *)
                  FMonad(function,x)                                  (* V2.6z *)
                ELSE Mark(144)
                END;
  
      | Halt:   GenHalt(0);
  
      | High:   IF (mode < conMd) & (typ^.form = Array) THEN
                  IF typ^.dyn THEN
                    GetHigh(x);
                    LoadD(x);
                    (* resulting type defined by M2TM! *)          (* V2.6y *)
                  ELSE
                    restyp := typ^.IndexTyp;
                    IF restyp^.form = Range THEN
                      i := restyp^.max; restyp := restyp^.RBaseTyp;
                    ELSE
                      Mark(144); i := 0
                    END;
                    Release(x);
                    (* Note : the result type must be the same *)  (* V2.6y *)
                    (* ----   as the index type of the array ! *)  (* V2.6y *)
                    (*        so it cannot be CardInt.         *)  (* V2.6y *)
                    SetconMd(x, VAL(LONGINT, i), restyp);                        (* V2.6y *)
                  END;
                ELSE Mark(144)
                END;
  
      | Max:    IF mode = typMd THEN (* transform to 'conMd' *)
                  mode := conMd;
                  CASE typ^.form OF
                    Bool :    val.C  := 1;  (* our compiler uses 1 for true *)
                  | Char :    val.Ch := MaxChar;
                  | Card :    val.C  := MaxCard;
                  | Int  :    val.I  := MaxInt;
                  | Enum :    val.Ch := VAL(CHAR, typ^.NofConst-1); (* V2.6 *)
                  | LCard :   val.D  := -1;
                  | Double :  val.D  := 07FFFFFFFH;
                  | Real :    val.R  := VAL(REAL, 07F7FFFFFH);
                  | LongReal: val.D3 := 07FEFH;
                              val.D2 := 0FFFFH;
                              val.D1 := 0FFFFH;
                              val.D0 := 0FFFFH;
                  | Range :   SetconMd(x, typ^.max, typ^.RBaseTyp);
                  ELSE
                    Mark(144); SetconMd(x, 0, undftyp);
                  END;
                  restyp := typ;
                ELSE Mark(145)
                END;
  
      | Min:    IF mode = typMd THEN (* transform to 'conMd' *)
                  mode := conMd;
                  CASE typ^.form OF
                    Bool :    val.C  := 0; (* our compiler uses 0 for FALSE *)
                  | Char :    val.Ch := 0C;
                  | Card :    val.C  := 0;
                  | Int  :    val.I  := MIN(INTEGER); (* -32768 *)
                  | Enum :    val.Ch := 0C;
                  | LCard :   val.D  := 0;
                  | Double :  val.D  := MIN(LONGINT);
                  | Real :    val.R  := VAL(REAL, 0FF7FFFFFH);
                  | LongReal: val.D3 := 0FFEFH;
                              val.D2 := 0FFFFH;
                              val.D1 := 0FFFFH;
                              val.D0 := 0FFFFH;
                  | Range :   SetconMd(x, typ^.min, typ^.RBaseTyp);
                  ELSE
                    Mark(144); SetconMd(x, 0, undftyp);
                  END;
                  restyp := typ;
                ELSE Mark(145)
                END;
  
      | Odd:    IF (f = Int) OR (f = Card) OR
                   (f = CardInt) OR (f = LCard) OR
                   (f = Double) OR (typ = addrtyp) THEN
                  IF mode = conMd THEN                             (* V2.6y *)
                    SetconMd(x, VAL(LONGINT, ODD(LongVal(x))), restyp)     (* V2.6y *)
                  ELSE                                             (* V2.6y *)
                    LoadD(x);                                      (* V2.6y *)
                    SetconMd(y, 1, typ);                          (* V2.6y *)
                    And2(x,y)                                      (* V2.6y *)
                  END;                                             (* V2.6y *)
                ELSE Mark(144)
                END;
  
      | Ord:    IF (f <> Undef) & (f <= Enum) THEN                  (* V2.6 *)
                  (* now range checks included ! *)                 (* V2.6 *)
                  IF mode = conMd THEN                              (* V2.6 *)
                    li := LongVal(x);                               (* V2.6 *)
                    IF (li < VAL(LONGINT, 0)) OR (li > VAL(LONGINT, MaxInt)) THEN        (* V2.6 *)
                      Mark(138); li := 0;                           (* V2.6 *)
                    END;                                            (* V2.6 *)
                    SetconMd(x, li, cardtyp);                       (* V2.6 *)
                    (* for constants change result to CARDINAL! *)  (* V2.6 *)
                    (* don't generate constant of type CardInt. *)  (* V2.6y*)
                    restyp := cardtyp;                              (* V2.6 *)
                  ELSE                                              (* V2.6 *)
                    LoadX(x,word);                                  (* V2.6 *)
                    Isz(x,sz);                                      (* V2.6z*)
                    IF SignedT(x) OR (sz > byte) THEN               (* V2.6z*)
                      typ := inttyp; CheckClimit(x, MaxInt);        (* V2.6z*)
                    END;                                            (* V2.6z*)
                  END;                                              (* V2.6 *)
                ELSE Mark(144)
                END;
  
      | Getreg: IF NOT morepar THEN
                  Mark(144); (* 2 parameters required *)
                ELSE
                  IF (mode = conMd) & ((f = Int) OR (f = Card)) THEN
                    restyp := typ; (* see you later *)
                  ELSE
                    Mark(144);
                  END;
                END;
  
      | Short:  IF (f = Double) OR (f = LCard) THEN                 (* V2.6 *)
                  (* now range checks included ! *)                 (* V2.6 *)
                  IF mode = conMd THEN                              (* V2.6 *)
                    li := LongVal(x);                               (* V2.6 *)
                    IF (li < VAL(LONGINT, MIN(INTEGER))) OR
                       (li > VAL(LONGINT, 32767)) THEN         (* V2.6 *)
                      Mark(138); li := 0;                           (* V2.6 *)
                    END;                                            (* V2.6 *)
                    SetconMd(x, li, inttyp);                        (* V2.6 *)
                  ELSE                                              (* V2.6 *)
                    LoadX(x,word);                                  (* V2.6 *)
                    SetconMd(y, 0, inttyp);                         (* V2.6 *)
                    CheckDbltoSingle(x,y);
                    Release(y);
                  END;                                              (* V2.6 *)
                  restyp := inttyp;                                 (* V2.6 *)
                ELSIF (f = LongReal) THEN                           (* V2.6 *)
                  IF mode = conMd THEN                              (* V2.6 *)
                    IF ABS(val.X) <= LONG(ABS(MAX(REAL))) THEN           (* V2.6 *)
                      val.R := SHORT(val.X);                               (* V2.6 *)
                    ELSE                                            (* V2.6 *)
                      val.R := FLOAT(0); Mark(138);                  (* V2.6 *)
                    END;                                            (* V2.6 *)
                    typ := realtyp;                                 (* V2.6 *)
                  ELSE FMonad(Short,x)                              (* V2.6 *)
                  END;                                              (* V2.6 *)
                  restyp := realtyp;
                ELSE Mark(144)
                END;
  
      | Size:   IF mode < conMd THEN
                  IF (typ^.form = Array) & typ^.dyn THEN
                    i := typ^.ElemTyp^.size;                       (* V2.6 *)
                    (* Caution: type is changed by GetHigh! *)     (* V2.6 *)
                    GetHigh(x);
                    LoadD(x);
                    Inc1(x);
                    IF i <> 1 THEN
                      SetconMd(y, VAL(LONGINT, i), typ);                           (* V2.6 *)
                      Mul2(x,y);
                    END;
                    (* resulting type defined by M2TM! *)          (* V2.6y *)
                  ELSE
                    (* for constants change result to CARDINAL! *) (* V2.6y *)
                    (* don't generate constant of type CardInt. *) (* V2.6y *)
                    Release(x);
                    restyp := cardtyp;                             (* V2.6y *)
                    SetconMd(x, typ^.size, restyp);                (* V2.6y *)
                  END;
                ELSIF mode = typMd THEN
                  restyp := cardtyp;                               (* V2.6y *)
                  SetconMd(x, typ^.size, restyp);
                ELSE
                  Mark(144);
                  Release(x);
                  restyp := cardtyp;                               (* V2.6y *)
                  SetconMd(x, 0, restyp);
                END;
  
      | Tsize:  IF mode = typMd THEN
                  i := typ^.size;
                ELSE
                  Mark(145);
                  Release(x);
                  i := 0;
                END;
                (* for constants change result to CARDINAL! *)     (* V2.6y *)
                (* don't generate constant of type CardInt. *)     (* V2.6y *)
                restyp := cardtyp;                                 (* V2.6y *)
                SetconMd(x, VAL(LONGINT, i), restyp);
  
  (*f*) | Long:   restyp := undftyp;
                (* 1 parameter required, 2. parameter optional : *)
                IF NOT morepar THEN
                  (* LONG with only one parameter : *)
                  IF (f <> Undef) & (f <= Enum) THEN               (* V2.6 *)
                    (* for all scalar types : *)                   (* V2.6 *)
                    IF mode = conMd THEN
                      SetconMd(x, LongVal(x), dbltyp)
                    ELSE
                      LoadX(x,long)
                    END;
                    restyp := dbltyp;
                  ELSIF (f = Real) THEN
                    IF mode = conMd THEN                           (* V2.6 *)
                      val.X := LONG(val.R);                   (* V2.6 *)
                      typ := lrltyp;                               (* V2.6 *)
                    ELSE FMonad(Long,x)                            (* V2.6 *)
                    END;                                           (* V2.6 *)
                    restyp := lrltyp;
                  ELSE
                    Mark(144);
                    Release(x);
                  END;
                ELSE
                  (* LONG with 2 parameters : *)
                  IF (f = Int) OR (f = Card) THEN
                    restyp := typ (* see you later *)
                  ELSE
                    Mark(144);
                    Release(x);
                  END;
                END;
  
  (*f*) | Shift:  restyp := undftyp;
                IF NOT morepar THEN
                  Mark(144) (* 2 parameters required *)
                ELSE
                  IF SimpleT(x) THEN
                    restyp := typ; (* see you later *)
                  ELSE
                    Mark(144);
                    Release(x);
                  END;
                END;
  
  (*f*) | Val:    restyp := undftyp;
                IF NOT morepar THEN
                  Mark(144) (* 2 parameters required *)
                ELSE
                  IF (mode = typMd) THEN
                    restyp := typ;            (* V2.6 *)
                  ELSE
                    Mark(144);
                    Release(x);
                  END;
                END;
  
  (*p*) | Dec,
        Inc:    (* 1 parameter required, 2. parameter optional : *)
                IF NOT morepar THEN
                  (*
                    Since the item we are altering may be being referenced via
                    an index, we don't want to lose it when we load the old
                    value.  By setting this flag, the M2HM module will know 
                    that the item should keep control of the index register,
                    until such time as the operation is truly complete.
                  *)
                  dontLoseIndex := TRUE;

                  (* DEC/INC with only one parameter : *)
                  IF (mode < conMd) &                              (* V2.6 *)
                     (f >= Char) & (f <= Double) THEN              (* V2.6 *)
                    (* for all scalar types : *)
                    y := x;
  
                    IF function = Dec THEN
                      Dec1(y);
                    ELSE
                      Inc1(y);
                    END;

                    (*
                      It's OK to allow index registers to be lost now.
                    *)
                    dontLoseIndex := FALSE;
  
                    Move(y, x);
                  ELSE
                    Mark(144);
                  END;
                ELSE
                  (* DEC/INC with 2 parameters : *)
                  IF (mode < conMd) &                              (* V2.6 *)
                     (f >= Char) & (f <= Double) THEN              (* V2.6 *)
                    restyp := typ; (* see you later *)
                  ELSE Mark(144)
                  END;
                END;
  
  (*p*) | Excl,
        Incl:   IF NOT morepar THEN Mark(144) (* 2 parameters required *)
                ELSE
                  IF (mode < conMd) & (f = Set) THEN
                    restyp := typ; (* see you later *)
                  ELSE Mark(144)
                  END;
                END;
  
  (*p*) | Inline: IF (mode = conMd) &
                     ((f = Int) OR (f = Card) OR (f = CardInt)) THEN
                    PutOp(WordVal(x), 1);
                  ELSE Mark(144)
                  END;
  
  (*p*) | Setreg: IF NOT morepar THEN
                  Mark(144); (* 2 parameters required *)
                ELSE
                  IF (mode = conMd) & ((f = Int) OR (f = Card)) THEN
                    restyp := typ; (* see you later *)
                  ELSE
                    Mark(144);
                  END;
                END;
  
      (* floating-point standard functions supplement : *)
  
      | ACos, ASin, ATan, ATanH, Cos, CosH, EtoX, EtoXM1,
        GetExp, GetMan, LogN, LogNP1, Log10, Log2, Sin, SinH,
        Sqrt, Tan, TanH, TentoX, TwotoX:
                IF (f = Real) OR (f = LongReal) THEN
                  restyp := typ; (* same type as argument type! *)
                  FMonad(function,x);
                ELSE Mark(144)
                END;
  
      | Trunc:  IF (f = Real) OR (f = LongReal) THEN               (* V2.6 *)
                  IF (mode = conMd) & (f = Real) THEN              (* V2.6 *)
                    IF (FLOAT(MinInt) <= val.R) &                  (* V2.6 *)
                       (val.R <= FLOAT(MaxInt)) THEN               (* V2.6 *)
                      li := VAL(LONGINT, TRUNC(val.R));                          (* V2.6 *)
                    ELSE                                           (* V2.6 *)
                      li := 0; Mark(138);                           (* V2.6 *)
                    END;                                           (* V2.6 *)
                    SetconMd(x, li, inttyp);                       (* V2.6 *)
                  ELSE (* variables and longreals *)               (* V2.6 *)
                    (* M2TM defines the resulting type! *)         (* V2.6 *)
                    FMonad(function,x)                                (* V2.6 *)
                  END                                              (* V2.6 *)
                ELSE Mark(144)                                      (* V2.6 *)
                END;                                               (* V2.6 *)
      | TruncD: IF (f = Real) OR (f = LongReal) THEN
                  (* M2TM defines the resulting type! *)
                  FMonad(function,x);
                ELSE Mark(144)
                END;
      | NewProcess:
        (*
          First parameter.  P: PROC is the procedure which constitutes the
          process
        *)
        IF mode = procMd THEN
          IF proc^.firstParam = NIL THEN
            IF proc^.pd^.lev = 0 THEN
              PushAdr(x);
            ELSE
              Mark(308);
            END;
          ELSE
            Mark(144);
          END;
        ELSE
          Mark(144);
        END;
      | Transfer, IOTransfer:
        (*
          First parameter.  VAR p1: ADDRESS is the process to be suspended
        *)
        IF typ = addrtyp THEN
          PushAdr(x);
        ELSE
          Mark(144);
        END;
      | New, Dispose:
        IF f = Pointer THEN
          ids := id;
  
          IF function = New THEN
            obj := Find(Enter("ALLOCATE"));
          ELSE
            obj := Find(Enter("DEALLOCATE"));
          END;
  
          id := ids;
  
          IF obj = NIL THEN
            Mark(306);
          ELSIF (obj^.class = Proc) OR
                ((obj^.class = Tool) AND
                 obj^.GSOSproc) THEN  (* ALLOCATE is a procedure *)
            IF obj^.class = Proc THEN
              par := obj^.firstParam;
            ELSE
              par := obj^.firstArg;
            END;
  
            IF par = NIL THEN
              Mark(307);
            ELSIF par^.varpar AND (par^.typ^.form = LCard) THEN
              par := par^.next;
  
              IF (par = NIL) OR
                 (par^.typ^.form <> Card) THEN
                Mark(307);
              ELSE
                PushAdr(x);

                IF nilchk AND (function = New) THEN
                  PutStackReference(LDA+STKRELATIVE, 3);
                  PutOp(PHA, 1);
                  PutStackReference(LDA+STKRELATIVE, 3);
                  PutOp(PHA, 1);
                END;

                SetconMd(z, typ^.PBaseTyp^.size, cardtyp);
                PushVal(z);
                CallExt(obj);
                DEC(stackPointer, 6);

                IF nilchk AND (function = New) THEN
                  newLink(notNIL);
                  GetReg(long, reg);
                  PutOp(PLA, 1);
                  PutOp(PLX, 1);
                  PutDPReference(STA+DIRECT, RegAdr(reg), 2);
                  PutDPReference(STX+DIRECT, RegAdr(reg)+2, 2);
                  PutDPReference(LDY+IMMEDIATE3, 2, 3);
                  PutDPReference(LDA+DIRINDLONG, RegAdr(reg), 2);
                  PutDPReference(ORA+DIRINDBYYLG, RegAdr(reg), 2);
                  PutLinkReference(BNE, notNIL, 2);
                  GenTerminate(tsOutOfMemory);
                  FixLink(notNIL);
                  Unlock(reg);
                END;
              END;
            ELSE
              Mark(307);
            END;
          ELSE
            Mark(307);
          END;
        ELSE
          Mark(144);
        END;
      | NonStand:
      END (*CASE*);
  
      p := x;
      p.typ := restyp;
  
      (*
        We may have loaded the registers with the value of 'x'.  We must make
        sure that if this is so, the 'locked' registers refer to 'p' which has
        become the actual value.  'p' is passed back up the chain of
        procedures that got us here. 'x' is not.  If we wish to maintain the
        'locking' of the CPU, we must 'lock' 'p' so that the code generator
        doesn't get confused.
      *)
  
      IF (p.mode = regMd) THEN
        Release(x);           (* ensure that the CPU has been unlocked *)
        SetregMd(p, p.typ);   (* now lock it with 'p' *)
      END;
  
      IF restyp = notyp THEN
        Release(x);
      END;
    END;
  END FirstParam;

  PROCEDURE SecondParam;
  VAR
    bytes:      CARDINAL;
    loop:       CARDINAL;
    endOfLoop:  CARDINAL;
    reg:        DRegister;
    szp:        WidType;
  BEGIN
    WITH x DO
      SRTest(x);
      f := typ^.form;
  
      CASE function OF
  
  (*f*) Long:   IF (p.typ <> undftyp) THEN
                  IF (f = Int) OR (f = Card) THEN
                    LoadD(x); x.typ := cardtyp;
                    (* Note : p is asserted of type INTEGER/CARDINAL! *)
                    IF (p.mode = conMd) &
                       (VAL(CARDINAL, WordVal(p)) <= MaxInt) THEN
                      li := SHIFT(LongVal(p), 16);
                      SetconMd(p, li, dbltyp);
                      LoadD(p);
                    ELSE
                      LoadD(p);
                      SetconMd(y, 16, cardtyp);
                      p.typ := dbltyp;
                      Shi2(p, y, left);   (* LSL.L *)
                    END;
                    p.typ := cardtyp;
                    Or2(p,x);          (* OR.W *)
                    p.typ := dbltyp;   (* resulting type *)
                    Release(x);
                  ELSE
                    Mark(144); Release(x);
                  END;
                END;
  
  (*f*) | Shift:  IF (f = Int) OR (f = Card) OR (f = CardInt) THEN
                  htyp := p.typ;   (* type of 1. parameter *)
                  LoadD(p);        (* load p *)
  
                  IF SignedT(p) THEN
                    shi := Asl;
                  ELSE
                    shi := Lsl;
                  END;
  
                  Ash2(p, x, shi); (* arithmetic/logical shift *)
                  p.typ := htyp;   (* resulting type *)
                ELSE
                  Mark(144); Release(x);
                END;
  
  (*f*) | Val:    (* replacement for the type transfer functions : *)
                ConvertTyp(p.typ, x);
                typ := p.typ; (* resulting type *)
                p := x;       (* resulting item *)
  
                IF x.mode = regMd THEN
                  Release(x);
                  SetregMd(p, p.typ);
                END;
  
  (*p*) | Dec,
        Inc:    IF p.typ <> notyp THEN
                  (*
                    Since the set we are altering may be being referenced via
                    an index, we don't want to lose it when we load the old
                    value.  By setting this flag, the M2HM module will know 
                    that the item should keep control of the index register,
                    until such time as the operation is truly complete.
                  *)
                  dontLoseIndex := TRUE;

                  restyp := p.typ;
                  IF (f = Int) OR (f = Card) OR (f = CardInt) OR
                     (f = Double) OR (f = LCard) THEN
                    IF mode = conMd THEN
                      SetconMd(x, LongVal(x), restyp);
                    ELSE
                      Isz(p,sz);
                      LoadX(x,sz);
                      typ := restyp;
                    END;
  
                    y := p;
  
                    IF function = Inc THEN
                      Add2(y,x);
                    ELSE
                      Sub2(y,x);
                    END;

                    (*
                      It's OK to allow index registers to be lost now.
                    *)
                    dontLoseIndex := FALSE;
  
                    Move(y, p);
                  ELSE
                    Mark(144);
                  END;
                END;
  
  (*p*) | Excl,
        Incl:   IF p.typ <> notyp THEN
                  restyp := p.typ; (* original type of set *)
                  SetBaseLimits(p,lo,hi);
  
                  IF (typ = p.typ) OR
                     ( (restyp = bitstyp) &
                       ((typ = inttyp) OR (typ = dbltyp) OR
                       (typ = lcardtyp) OR (typ = cardinttyp)) ) THEN
                    (* recover type and size of the set. *)
                    p.typ := restyp;
  
                    IF SimpleC(x) THEN
                      (* constant element to include/exclude. *)
                      li := LongVal(x);                            (* V2.6 *)
  
                      IF (li < VAL(LONGINT, lo)) OR (li > VAL(LONGINT, hi)) THEN   (* V2.6 *)
                        (* constant out of range *)
                        Mark(60);
                        i := 0;
                      ELSE
                        i := VAL(INTEGER, li);                     (* V2.6 *)
                      END;
  
                      WITH y DO
                        typ := p.typ;
                        mode  := conMd;

                        val.FS.setSize := p.typ^.size;
                        SCClearSet(val.FS);
                        SCSetBit(val.FS, i);
  
                        IF function = Excl THEN
                          SCComplementSet(val.FS);
                        END;
                      END;
  
                      (*
                        Since the set we are altering may be being referenced
                        via an index, we don't want to lose it when we load the
                        old value.  By setting this flag, the M2HM module will
                        know that the item should keep control of the index 
                        register, until such time as the operation is truly 
                        complete.
                      *)
                      dontLoseIndex := TRUE;

                      z := p;
  
                      IF function = Incl THEN
                        Or2(z,y);
                      ELSE
                        And2(z,y);
                      END;

                      (*
                        It's OK to allow index registers to be lost now.
                      *)
                      dontLoseIndex := FALSE;
  
                      Release(y);
  
                      Move(z, p);
                      Release(z);
                    ELSIF SimpleT(x) THEN
                      (* variable element to include/exclude. *)
                      CheckRange(x,lo,hi,0);
  
                      Isz(p, szp);
                      bytes := p.typ^.size;

                      IF szp < quad THEN
                        SetconMd(y, 1, p.typ);
                        Shi2(y, x, left);
                      ELSE
                        WHILE bytes > 1 DO
                          PutDPReference(PEA, 0, 3);
                          DEC(bytes, 2);
                        END;

                        IF bytes = 1 THEN
                          PutDPReference(SEP, 10H, 2);  (* Short accumulator and memory *)
                          PutDPReference(LDY+IMMEDIATE3, 0, 2);
                          PutOp(PHY, 1);
                          PutDPReference(REP, 10H, 2);  (* Long accumulator and memory *)
                        END;

                        INC(stackPointer, p.typ^.size);
                        SetstkMd(y, p.typ);

                        LoadVal(x, erCPU);
                        Unlock(erCPU);
                        PutOp(TAY, 1);
                        PutOp(LSR+IMMEDIATE, 1); (* div by  2 *)
                        PutOp(LSR+IMMEDIATE, 1); (* div by  4 *)
                        PutOp(LSR+IMMEDIATE, 1); (* div by  8 *)
                        GetReg(word, reg);
                        PutDPReference(STA+DIRECT, RegAdr(reg), 2);
                        PutOp(TSC, 1);
                        PutOp(INCACC, 1);
                        PutOp(CLC, 1);
                        PutDPReference(ADC+DIRECT, RegAdr(reg), 2);
                        PutOp(TAX, 1);
                        Unlock(reg);
                        PutOp(TYA, 1);
                        PutDPReference(And+IMMEDIATE, 07H, 3);
                        PutOp(TAY, 1);
                        PutDPReference(LDA+IMMEDIATE, 1, 3);

                        newLink(loop);
                        newLink(endOfLoop);

                        PutDPReference(CPY+IMMEDIATE3, 0, 3);
                        PutLinkReference(BEQ, endOfLoop, 2);
                        FixLink(loop);
                        PutOp(ASL+IMMEDIATE, 1);
                        PutOp(DEY, 1);
                        PutLinkReference(BNE, loop, 2);
                        FixLink(endOfLoop);
                        PutDPReference(SEP, 20H, 2);  (* Short accumulator and memory *)
                        PutLongReference(STA+ABSLONGBYX, 0, 4);
                        PutDPReference(REP, 20H, 2);  (* Long accumulator and memory *)
                        Release(x);
                      END;

                      (*
                        Since the set we are altering may be being referenced
                        via an index, we don't want to lose it when we load the
                        old value.  By setting this flag, the M2HM module will
                        know that the item should keep control of the index 
                        register, until such time as the operation is truly 
                        complete.
                      *)
                      dontLoseIndex := TRUE;

                      z := p;
  
                      IF function = Incl THEN
                        Or2(z,y);  (* places result on stack for big sets *)
                      ELSE
                        Com1(y); (* result saved in 'y' for big sets *)
                        And2(z,y);  (* places result on stack for big sets *)
                      END;

                      (*
                        It's OK to allow index registers to be lost now.
                      *)
                      dontLoseIndex := FALSE;
  
                      Move(z, p);
                      Release(z);
                      Release(y);
                    ELSE
                      Mark(144);
                    END;
                  ELSE (*typ <> p.typ*)
                    Mark(144);
                  END;
                END;
  
  (*p*) | Inline: IF SimpleC(x) THEN
                    PutOp(WordVal(x), 1);
                  ELSE
                    Mark(144);
                  END;
  
      | Getreg: IF p.typ <> notyp THEN
                  r := VAL(CARDINAL, WordVal(p)) MOD 4;
                  SetregMd(p, cardtyp);
    
                  CASE r OF
                    0: p.register := erCPU;
                  | 1: p.register := erAcc;
                  | 2: p.register := erXreg;
                  | 3: p.register := erYreg;
                  END;
  
                  IF (mode = procMd) OR (typ = stringtyp) THEN
                    Mark(144);
                  ELSIF (typ^.size = VAL(aSize, 4)) THEN
                    IF r # 0 THEN
                      Mark(144);
                    ELSE
                      Move(p, x);
                    END;
                  ELSIF (typ^.size = VAL(aSize, 2)) THEN
                    IF r = 0 THEN
                      Mark(144);
                    ELSE
                      Move(p, x);
                    END;
                  ELSE
                    Mark(144);
                  END;
                END;
  
  (*p*) | Setreg: IF p.typ <> notyp THEN
                  r := VAL(CARDINAL, WordVal(p)) MOD 4;
  
                  CheckCPUUsage(p);
  
                  SetregMd(p, cardtyp);
                  Release(p);  (* actually unlocks erCPU *)
  
                  CASE r OF
                    0: p.register := erCPU;
                  | 1: p.register := erAcc;
                  | 2: p.register := erXreg;
                  | 3: p.register := erYreg;
                  END;
  
                  IF (mode = procMd) OR (typ = stringtyp) THEN
                    IF r # 0 THEN
                      Mark(144);
                    ELSE
                      MoveAdr(x, p);
                    END;
                  ELSIF (typ^.size = VAL(aSize, 4)) THEN
                    IF r # 0 THEN
                      Mark(144);
                    ELSE
                      Move(x, p);
                    END;
                  ELSIF (typ^.size = VAL(aSize, 2)) THEN
                    IF r = 0 THEN
                      Mark(144);
                    ELSE
                      Move(x, p);
                    END;
                  ELSE
                    Mark(144);
                  END;
                END;
      | NewProcess:
        (*
          Second parameter.  A: ADDRESS is the base address of the process'
          workspace
        *)
        IF typ = addrtyp THEN
          PushVal(x);
        ELSE
          Mark(144);
        END;
      | Transfer, IOTransfer:
        (*
          Second parameter.  VAR p2: ADDRESS is the process to be resumed
        *)
        IF typ = addrtyp THEN
          PushAdr(x);
  
          IF function = Transfer THEN
            GenGlobalOp(JSL, 'M2Lib_TRANSFER', 0, 0, 3);
            AddBytesToObjectFile(4);
            DEC(stackPointer, 8);
          END;
        ELSE
          Mark(144);
        END;
      ELSE (*CASE function OF*)
        Mark(64);
      END;
  
      IF (function <> Val) & (function <> Long) & (function <> Shift) &
         (function <> Scale) THEN
        (* for all standard procedures : *)
        Release(p);
        Release(x);
        p.typ := notyp; (* no result-typ for procs. *)
      ELSIF (function = Long) & (p.typ = undftyp) THEN
        Release(x);
      END;
    END;
  END SecondParam;

  PROCEDURE OtherParam;
  BEGIN
    WITH x DO
      SRTest(x);
      f := typ^.form;

      IF (function = Inline) THEN
        IF SimpleC(x) THEN
          PutOp(WordVal(x), 1);
        ELSE
          Mark(144)
        END;
      ELSIF function = NewProcess THEN
        IF parno = 2 THEN
          (*
            Third parameter.  n: CARDINAL represents the size of the workspace
          *)
          IF ((mode = conMd) AND ((f = CardInt) OR (f = Card))) OR
             ((mode < conMd) AND (typ = cardtyp)) THEN
            PushVal(x);
          ELSE
            Mark(144);
          END;
        ELSIF parno = 3 THEN
          (*
            Fourth parameter. VAR p1: ADDRESS is the result parameter
          *)
          IF typ = addrtyp THEN
            PushAdr(x);
            GenGlobalOp(JSL, 'M2Lib_NEWPROCESS', 0, 0, 3);
            AddBytesToObjectFile(4);
            DEC(stackPointer, 10);
          ELSE
            Mark(144);
          END;
        ELSE
          Mark(64);
        END;
      ELSIF function = IOTransfer THEN
        IF parno = 2 THEN
          (*
            Third parameter.  I: CARDINAL represents the interrupt vector number
          *)
          IF ((mode = conMd) AND ((f = CardInt) OR (f = Card))) OR
             ((mode < conMd) AND (typ = cardtyp)) THEN
            PushVal(x);
            GenGlobalOp(JSL, 'M2Lib_IOTRANSFER', 0, 0, 3);
            AddBytesToObjectFile(4);
            DEC(stackPointer, 10);
          ELSE
            Mark(144);
          END;
        ELSE
          Mark(64);
        END;
      ELSE (* all other standards *)
        Mark(64);
      END;
      Release(x);
    END;
  END OtherParam;

BEGIN
  WITH x DO
    function := fctno;

    IF parno = 0 THEN (* 1. Parameter *)
      FirstParam;
    ELSIF parno = 1 THEN (* 2. Parameter *)
      SecondParam;
    ELSE (* 3. and following Parameters *)
      OtherParam;
    END;
  END (*WITH*);
END GenStParam;

PROCEDURE GenStFct(fctno : Standard; parno : CARDINAL);
  VAR must : CARDINAL;
BEGIN
  CASE fctno OF
    Abs, Adr, Cap, Chr, Float, FloatD, High, Max, Min,
    Odd, Ord, Short, Size, Tsize, Trunc, TruncD :  must := 1;
  | Excl, Incl, Getreg, Setreg, Shift, Transfer, Val : must := 2;
  | Dec, Inc, Long, Inline, New, Dispose: must := 1;
  | Halt : must := 0;
  | IOTransfer: must := 3;
  | NewProcess: must := 4;
  | ACos, ASin, ATan, ATanH, Cos, CosH, EtoX, EtoXM1,
    GetExp, GetMan, LogN, LogNP1, Log10, Log2, Sin, SinH,
    Sqrt, Tan, TanH, TentoX, TwotoX,
    CRom, CRomD, Entier, Round : must := 1;
  | Scale : must := 2;
  | NonStand : ;
  END (*CASE*);
  IF (fctno < NonStand) OR (must <> 0) THEN
    IF parno < must THEN Mark(65) END;
  END;
END GenStFct;

END M2EM.
