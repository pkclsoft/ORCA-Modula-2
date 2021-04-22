(*$Segment M2HMPart1*)
(*$RangeCheck+*)
(*$OverflowCheck+*)
IMPLEMENTATION MODULE M2HM;

(* M2HM-implementation for the W65C816 processor. *)

FROM M2DM IMPORT
   ObjPtr, StrPtr, StrForm, ConstValue, PDesc,
   Structure, aAdr, mainmod, aSize,
   notyp, undftyp, booltyp, chartyp, cardtyp,
   inttyp, bitstyp, dbltyp, realtyp, lrltyp,
   proctyp, stringtyp, addrtyp, wordtyp, bytetyp,
   WordSize, MinInt, MaxCard, stackchk, nilchk,
   rngchk, ovflchk, WorkingStorage, DirectPageSize;
FROM M2FM IMPORT
   LoadF, ExtendedSize;
FROM M2Lib IMPORT
   aProcessDescriptor, aTerminateStatus, LoWORD, HighWORD, LongWORD;
FROM M2LM IMPORT
   aSymbol, GetModuleKey,
   IdToSymbol, AddBytesToObjectFile, localStringList, PutLongReference,
   GetVariableName, GetModuleName, newLink, FixLink, AllocChar,
   PutLinkReference, GetProcedureName, GetStringName, PutOp,
   globalStringList, PutDPReference, CDAName, CDAShutdown, Stacksize,
   NDAOpen, NDAClose, NDAAction, NDAPeriod, NDAMask, NDAMenuline,
   PutStackReference, PutDisplayReference, CDEVName;
FROM M2OMF IMPORT GenGlobalOp, PutByte, ReferenceLabel, PutLabelText,
   EndSegment, aSegmentKind, aSegmentAttribute, aSegmentAttributeSet,
   NewSegmentHeader, GenLocalOp, objFile, PutByteConstant,
   IncrementSegmentLength;
FROM M2SM IMPORT
   Symbol, Mark, aDirective, aDirectiveSet, Directives;
FROM Strings IMPORT Concat, Length;
FROM SYSTEM IMPORT
   ADR, BYTE, WORD, SHIFT, TSIZE;
FROM W65C816 IMPORT TSC, CLC, SBC, ABSLONG, BPL, SEP, REP, PHA, PHX, BCC, BEQ,
  BNE, BCS, BVS, BVC, BMI, BRL, LDA, IMMEDIATE, IMMEDIATE3, BRA, PHK, PER, PEA,
  JMP, JABS, PHB, TAY, INCACC, TCS, TYA, ADC, LDY, DIRECT, LDX, DIRINDBYYLG,
  TAX, DEY, DIRINDLONG, PLA, STY, TXY, TYX, And, DIRECTBYX, TDC, ABSLONGBYX,
  PLX, STKRELATIVE, STX, TXA, PLY, ASL, ROL, LSR, SEC, ROR, ORA, EOR, CMP,
  CPX, DECACC, DEX, INX, JSL, RTS, PHD, PLD, DIRINDIRECT, DIRINDBYY, STKRELBYY,
  JML, JABSLIND, JABSL, BITIMMED, STA, DIRECTBYY, TCD, PLB, PHP, PLP, PHY;

IMPORT W65C816;

CONST
  (* WD65C816 instruction mnemonics. *)
  (* ______________________________  *)

     (* Left shift constants. *)
     LS3  =  10B;  LS4  =  20B;  LS5  =  40B;   LS6  =  100B;
     LS7  = 200B;  LS8  = 400B;  LS9  =  1000B; LS10 =  2000B;
     LS11 = 4000B; LS12 = 10000B;

TYPE
  aCpuReg = (YReg, XReg, AReg, AddToA);

VAR
  mask                : ARRAY [ 0 .. 32 ] OF LONGINT;
  hightyp             : StrPtr;
  registers           : DRegSet;
  lockItem            : POINTER TO Item;

(*$RangeCheck-*)
PROCEDURE Locked(reg: DRegister): BOOLEAN;
BEGIN
  RETURN (reg IN registers);
END Locked;

PROCEDURE Lock(reg: DRegister);
BEGIN
  IF Locked(reg) THEN
    Mark(178);
  ELSE
    INCL(registers, reg);

    IF reg = erQuad1 THEN
      Lock(er6);
      Lock(er7);
    ELSIF reg = erQuad2 THEN
      Lock(er1);
      Lock(er2);
    END;
  END;
END Lock;

PROCEDURE Unlock(reg: DRegister);
BEGIN
  IF Locked(reg) THEN
    EXCL(registers, reg);

    IF reg = erQuad1 THEN
      Unlock(er6);
      Unlock(er7);
    ELSIF reg = erQuad2 THEN
      Unlock(er1);
      Unlock(er2);
    ELSIF reg = erCPU THEN
      lockItem := NIL;
    END;
  END;
END Unlock;

PROCEDURE Release(VAR x: Item);
BEGIN
  WITH x DO
    IF mode IN ItSet{DregMd, regMd} THEN
      Unlock(register);
    END;

    IF (mode < conMd) AND indexed THEN
      Unlock(indexReg);
    END;
  END;
END Release;
(* $RangeCheck+*)

PROCEDURE RegAdr(reg: DRegister): aAdr;
BEGIN
  CASE reg OF
    erNone: RETURN 0;  (* long *)
    |
    er1:    RETURN 8;  (* long *)
    |
    er2:    RETURN 12; (* long *)
    |
    er3:    RETURN 16; (* word *)
    |
    er4:    RETURN 18; (* word *)
    |
    er5:    RETURN 20; (* word *)
    |
    er6:    RETURN 0;  (* long *)
    |
    er7:    RETURN 4;  (* long *)
    |
    erQuad1: RETURN 0; (* quadword *)
    |
    erQuad2: RETURN 8; (* quadword *)
  END;
END RegAdr;

PROCEDURE GetReg(size: WidType; VAR reg: DRegister);
BEGIN
  reg := erNone;

  IF size = long THEN
    IF Locked(er1) THEN
      IF Locked(er2) THEN
        IF Locked(er6) THEN
          IF Locked(er7) THEN
            Mark(185);
          ELSE
            reg := er7;
          END;
        ELSE
          reg := er6;
        END;
      ELSE
        reg := er2;
      END;
    ELSE
      reg := er1;
    END;
  ELSIF size = quad THEN
    IF Locked(er1) AND Locked(er2) THEN
      IF Locked(er6) AND Locked(er7) THEN
        Mark(186);
      ELSE
        reg := erQuad1;
      END;
    ELSE
      reg := erQuad2;
    END;
  ELSE
    IF Locked(er3) THEN
      IF Locked(er4) THEN
        IF Locked(er5) THEN
          Mark(184);
        ELSE
          reg := er5;
        END;
      ELSE
        reg := er4;
      END;
    ELSE
      reg := er3;
    END;
  END;

  IF reg <> erNone THEN
    Lock(reg);
  END;
END GetReg;

PROCEDURE ProcessorID(VAR id: Processor);
BEGIN
  id := "WD65C816";
END ProcessorID;

PROCEDURE IncAdr(incval: CARDINAL);
VAR
  skip: CARDINAL;
BEGIN
  IF incval <> 0 THEN
    newLink(skip);

    IF incval = 1 THEN
      PutOp(INCACC, 1);
      PutLinkReference(BNE, skip, 2);
    ELSE
      PutOp(CLC, 1);
      PutDPReference(ADC+IMMEDIATE, incval, 3);
      PutLinkReference(BCC, skip, 2);
    END;

    PutOp(INX, 1);
    FixLink(skip);
  END;
END IncAdr;

PROCEDURE GenModulaEntryCode;
(*
  OPERATION:
    Will generate the entry code for a modula style procedure call.

    As much as possible the library routines will be written in modula
    and linked in.

    To call these routines, the compiler must generate code that pushes the
    parameters onto the stack correctly.  It must then call this routine,
    and then finally, it should generate a JSL to the specified routine.
*)
BEGIN
END GenModulaEntryCode;

PROCEDURE GenStackCheck(SPLoaded: BOOLEAN);
VAR
  noOverflow: CARDINAL;
BEGIN
  IF stackchk THEN
    IF NOT SPLoaded THEN
      PutOp(TSC, 1);
    END;

    newLink(noOverflow);
    PutOp(SEC, 1);
    GenGlobalOp(SBC+ABSLONG, 'M2Lib_StackBottom', 0, 0, 3);
    AddBytesToObjectFile(4);
    PutLinkReference(BPL, noOverflow, 2);
    GenTerminate(tsStackOverflow);
    FixLink(noOverflow);
  END;
END GenStackCheck;

PROCEDURE GenNILCheck(reg:  DRegister; onStack: BOOLEAN);
VAR
  notNIL: CARDINAL;
  NILptr: CARDINAL;
  tmpReg: DRegister;
BEGIN
  IF nilchk THEN
    newLink(notNIL);
    newLink(NILptr);

    IF onStack THEN
      (*
        For the moment, assume that the erCPU registers are clear, and that the
        address is on the top of the stack.
      *)
      PutStackReference(LDA+STKRELATIVE, 1);
      PutStackReference(ORA+STKRELATIVE, 3);
      PutLinkReference(BEQ, NILptr, 2);
      PutLinkReference(BRA, notNIL, 2);
    ELSIF reg = erCPU THEN
      GetReg(word, tmpReg);
      PutDPReference(STA+DIRECT, RegAdr(tmpReg), 2);
      PutOp(TXA, 1);
      PutDPReference(ORA+DIRECT, RegAdr(tmpReg), 2);
      PutLinkReference(BEQ, NILptr, 2);
      PutDPReference(LDA+DIRECT, RegAdr(tmpReg), 2);
      PutLinkReference(BRA, notNIL, 2);
      Unlock(tmpReg);
    ELSE
      PutDPReference(LDA+DIRECT, RegAdr(reg), 2);
      PutDPReference(ORA+DIRECT, RegAdr(reg)+2, 2);
      PutLinkReference(BNE, notNIL, 2);
    END;

    FixLink(NILptr);
    GenTerminate(tsAddressOverflow);
    FixLink(notNIL);
  END;
END GenNILCheck;

PROCEDURE ShortM;
BEGIN
  PutDPReference(SEP, 20H, 2);  (* Short accumulator and memory *)
END ShortM;

PROCEDURE LongM;
BEGIN
  PutDPReference(REP, 20H, 2);  (* Long accumulator and memory *)
END LongM;

PROCEDURE CheckCPUUsage(VAR x: Item);
(*
  OPERATION:
    If the CPU registers are locked, then push the item that has them locked
    onto the stack, and release them.
*)
VAR
  szx:  WidType;
BEGIN
  IF Locked(erCPU) THEN  (* CPU registers are in use *)
    IF NOT errorInProc THEN
      IF (lockItem <> NIL) AND
         (ADR(x) <> lockItem) THEN
        Isz(lockItem^, szx);

        IF szx = byte THEN
          ShortM;
          PutOp(PHA, 1);
          LongM;
        ELSIF szx = word THEN
          PutOp(PHA, 1);
        ELSIF szx = long THEN
          PutOp(PHX, 1);
          PutOp(PHA, 1);
        ELSE
          Mark(401);
        END;

        INC(stackPointer, lockItem^.typ^.size);

        SetstkMd(lockItem^, lockItem^.typ);
        Unlock(erCPU);
      END;
    ELSE
      Unlock(erCPU);
    END;
  END;
END CheckCPUUsage;

PROCEDURE LoadParameters(VAR x, y: Item; xIntoReal: BOOLEAN);
(*
  OPERATION:
    This procedure should be used by any low level code generation routines
    that manipulate two Items.  It will load one parameter into a DRegister,
    and the other into the real registers.

    The calling procedure may specify which parameter gets to reside in the
    real registers using the third parameter.
*)
BEGIN
  IF xIntoReal THEN
    IF x.mode = regMd THEN
      LoadD(x);
    END;

    LoadD(y);
    LoadVal(x, erCPU);
  ELSE
    IF y.mode = regMd THEN
      LoadD(y);
    END;

    LoadD(x);
    LoadVal(y, erCPU);
  END;
END LoadParameters;

PROCEDURE Assert(ok : BOOLEAN);
BEGIN
  IF NOT ok THEN Mark(299) END;
END Assert;

PROCEDURE SignedT(VAR x : Item) : BOOLEAN;
(*      is x a signed type ?       *)
(* Note :  Real/LongReal excluded! *)
VAR
  s : StrPtr;
BEGIN
  s := x.typ; (* let x.typ unchanged *)

  IF s^.form = Range THEN
    s := s^.RBaseTyp;
  END;

  RETURN (s = inttyp) OR (s = dbltyp);
END SignedT;

PROCEDURE SimpleT(VAR x : Item) : BOOLEAN;
(*   is x a simple type of size   *)
(*         byte/word/long ?       *)
(* Note : Real/LongReal excluded! *)
VAR
  f:      StrForm;
  s:      StrPtr;
  sz:     CARDINAL;
  szx:    WidType;
BEGIN
  s := x.typ; (* let x.typ unchanged *)

  IF s^.form = Range THEN
    s := s^.RBaseTyp;
  END;

  f := s^.form;
  sz := VAL(CARDINAL, s^.size);
  Isz(x, szx);

  RETURN    ((f = Set)          AND (szx <> quad))
            OR
            ((sz IN {1,2,3,4})  AND ((f <= Double)    OR 
                                     (f = Pointer)    OR
                                     (f = ProcTyp)    OR 
                                     (f = Opaque)));
END SimpleT;

PROCEDURE RealT(VAR x : Item) : BOOLEAN;
(*  is x a floating-point-type ?  *)
(*       (REAL or LONGREAL)       *)
(* Note: floating-point-types are *)
(*       NOT considered as simple *)
VAR
  s: StrPtr;
BEGIN
  s := x.typ; (* let x.typ unchanged *)
  RETURN (s = realtyp) OR (s = lrltyp);
END RealT;

PROCEDURE SimpleC(VAR x : Item) : BOOLEAN;
(* is x a simple constant of size *)
(*         byte/word/long ?       *)
(* Note : Real/LongReal excluded! *)
BEGIN
  RETURN (x.mode = conMd) & SimpleT(x);
END SimpleC;

PROCEDURE LongVal(VAR x : Item) : LONGINT;
VAR
  r: LONGINT;
BEGIN
  r := 0;

  WITH x DO
    Assert(mode = conMd);

    IF typ^.form = Range THEN
      typ := typ^.RBaseTyp;
    END;

    CASE typ^.form OF
      Undef :         IF typ^.size = VAL(aSize, 1) THEN
                        r := VAL(LONGINT, val.Ch);        (* V2.6 *)
                      ELSIF typ^.size = VAL(aSize, 2) THEN
                        r := VAL(LONGINT, val.C);         (* V2.6 *)
                      ELSE
                        r := val.U;
                      END;                                (* V2.6 *)
    | Bool :          r := VAL(LONGINT, val.B);           (* V2.6 *)
    | Char :          r := VAL(LONGINT, val.Ch);          (* V2.6 *)
    | Card, CardInt:  r := VAL(LONGINT, val.C);           (* V2.6 *)
    | Int :           r := VAL(LONGINT, val.I);           (* V2.6 *)
    | Enum :          r := VAL(LONGINT, val.Ch);          (* V2.6 *)
    | Set :           r := LongWORD(VAL(CARDINAL, val.FS.set[0]),
                                    VAL(CARDINAL, val.FS.set[1]));
    | LCard,Double :  r := val.D;                         (* V2.6 *)
    | Real :          r := val.D;                         (* V2.6 - P.E. Should be val.R *)
    ELSE              r := val.D; (* String, etc. *)      (* V2.6 *)
    END;
  END; (*WITH*)

  RETURN r;
END LongVal;

PROCEDURE WordVal(VAR x : Item) : INTEGER;
VAR
  r: INTEGER;
BEGIN r := 0;
  WITH x DO
    Assert(mode = conMd);

    IF typ^.form = Range THEN
      typ := typ^.RBaseTyp;
    END;

    CASE typ^.form OF
      Undef :         IF typ^.size = VAL(aSize, 1) THEN
                        r := ORD(val.Ch);         (* V2.6 *)
                      ELSIF typ^.size = VAL(aSize, 2) THEN
                        r := VAL(INTEGER,val.C);  (* V2.6 *)
                      ELSE
                        r := VAL(INTEGER, val.U);
                      END;                        (* V2.6 *)
    | Bool :          r := ORD(val.B);            (* V2.6 *)
    | Char :          r := ORD(val.Ch);           (* V2.6 *)
    | Card, CardInt:  r := val.I;                 (* V2.6 - P.E. Should be VAL(INTEGER, val.C) *)
    | Int :           r := val.I;                 (* V2.6 *)
    | Enum :          r := ORD(val.Ch);           (* V2.6 *)
    | Set :           r := VAL(INTEGER, val.FS.set[0]);
    | LCard,Double :  r := VAL(INTEGER, val.D);   (* V2.6 *)
    | Real :          r := VAL(INTEGER, val.D);   (* V2.6 - P.E. Should be val.R *)
    ELSE              r := VAL(INTEGER, val.D);   (* String, etc. *)   (* V2.6 *)
    END;
  END; (*WITH*)

  RETURN r;
END WordVal;

PROCEDURE Isz(VAR x : Item; VAR fsz : WidType);
(* instruction size for item x : byte/word/long. *)
(* Note :  callable only for simple types !      *)
VAR
  s:  CARDINAL;
BEGIN
  (*
    It's possible that due to some error being detected that an item may not
    be set up correctly, so allow for it.
  *)
  IF x.typ^.size <= MAX(CARDINAL) THEN
    s := x.typ^.size;
  ELSE
    s := 0;  (* causes error to be displayed *)
  END;

  IF s = 1 THEN
    fsz := byte;
  ELSIF s = 2 THEN
    fsz := word;
  ELSIF s = 4 THEN
    fsz := long;
  ELSIF x.typ^.form = Set THEN
    fsz := setsized;
  ELSIF s = 8 THEN
    fsz := quad;
  ELSE
    fsz := long;
    Mark(238); (* invalid instruction size *)
  END;
END Isz;

PROCEDURE SetglbMd(VAR x : Item; fadr: aAdr; ftyp : StrPtr);
(* setup of an item designating a global variable *)
BEGIN
  WITH x DO
    IF ftyp <> NIL THEN
      typ := ftyp;
    ELSE
      typ := undftyp;
    END;

    mode := glbMd;
    mod := 0;
    lev := 0;
    adr := fadr;
    off := 0;
    isParam := FALSE;
    indir := FALSE;
    name := 0;
    notpop := FALSE;
    indexed := FALSE;
    indexReg := erNone;
    register := erNone;
    notpop := FALSE;
    parent := NIL;
  END (*WITH*);
END SetglbMd;

PROCEDURE SetlocMd(VAR x : Item; fadr: aAdr; ftyp : StrPtr);
(* setup of an item which is a local variable *)
BEGIN
  WITH x DO
    IF ftyp <> NIL THEN
      typ := ftyp;
    ELSE
      typ := undftyp;
    END;

    mode := locMd;
    mod := 0;
    lev := curLev;
    adr := fadr;
    off := 0;
    isParam := FALSE;
    indir := FALSE;
    name := 0;
    notpop := FALSE;
    indexed := FALSE;
    indexReg := erNone;
    register := erNone;
    notpop := FALSE;
    parent := NIL;
  END (*WITH*);
END SetlocMd;

PROCEDURE SetconMd(VAR x : Item; fval : LONGINT; ftyp : StrPtr);
BEGIN
  WITH x DO
    IF ftyp <> NIL THEN
      typ := ftyp;
    ELSE
      typ := undftyp;
    END;

    IF typ^.form = Range THEN
      typ := typ^.RBaseTyp;
    END;

    mode  := conMd;

    val.U := VAL(LONGINT, 0);

    CASE typ^.form OF
      (*
        If something is undefined, then an error must have occured.  Therefor,
        it shouldn't matter what the constant is.  For safety's sake, we will
        set it to zero.
      *)
      Undef :     IF typ^.size = VAL(aSize, 1) THEN
                    val.Ch := VAL(CHAR, fval);
                  ELSIF typ^.size = VAL(aSize, 2) THEN
                    val.C := VAL(CARDINAL, fval);
                  ELSE
                    val.U  := fval;
                  END;
    | Bool :      val.B  := VAL(BOOLEAN, fval);     (* V2.6 *)
    | Char :      val.Ch := VAL(CHAR, fval);        (* V2.6 *)
    | Card,                                       (* V2.6 *)
      CardInt :   val.C  := VAL(CARDINAL, fval);    (* V2.6 *)
    | Int :       val.I  := VAL(INTEGER, fval);     (* V2.6 *)
    | Enum :      val.Ch := VAL(CHAR, fval);        (* V2.6 *)
    | LCard :     val.D  := fval;                   (* V2.6 *)
    | Double :    val.D  := fval;                   (* V2.6 *)
    | Real :      val.R  := VAL(REAL, fval);        (* V2.6 *)
    | Set :       val.FS.set[0] := VAL(BITSET, LoWORD(fval));
                  val.FS.set[1] := VAL(BITSET, HighWORD(fval));
                  val.FS.setSize := typ^.size;
    ELSE          val.D  := fval; (* String, etc.      V2.6 *)
    END;
  END (*WITH*);
END SetconMd;

PROCEDURE SetregMd(VAR x : Item; ftyp : StrPtr);
(* setup of an item designating a register based variable
   note that variables located in registers are usually one of:

    * A result from a library routine
    * A dummy variable used at compile time

   Variables less than 3 bytes long are placed in the accumulator.

   Variables 4 bytes in length have the high word in the X register, and
   the low word in the accumulator.
*)
BEGIN
  WITH x DO
    IF ftyp <> NIL THEN
      typ := ftyp;
    ELSE
      typ := undftyp;
    END;

    mode := regMd;
    mod := 0;
    lev := 0;
    adr := 0;
    off := 0;
    register := erCPU;
    isParam := FALSE;
    indir := FALSE;
    name := 0;
    notpop := FALSE;
    indexed := FALSE;
    indexReg := erNone;
    notpop := FALSE;
    parent := NIL;
  END (*WITH*);

  IF lockItem <> ADR(x) THEN
    Lock(erCPU);
    lockItem := ADR(x);
  END;
END SetregMd;

PROCEDURE SetstkMd(VAR x : Item; ftyp : StrPtr);
(* setup of an item designating a variable located on the stack *)
BEGIN
  WITH x DO
    IF ftyp <> NIL THEN
      typ := ftyp;
    ELSE
      typ := undftyp;
    END;

    mode := stkMd;
    mod := 0;
    lev := curLev;
    adr := stackPointer;
    off := 0;
    isParam := FALSE;
    indir := FALSE;
    name := 0;
    notpop := FALSE;
    indexed := FALSE;
    indexReg := erNone;
    register := erNone;
    notpop := FALSE;
    parent := NIL;
  END (*WITH*);
END SetstkMd;

PROCEDURE SetDregMd(VAR x : Item; reg: DRegister; ftyp : StrPtr);
(*
  setup of an item designating a variable located in one of the fake data
  registers
*)
BEGIN
  WITH x DO
    IF ftyp <> NIL THEN
      typ := ftyp;
    ELSE
      typ := undftyp;
    END;

    mode := DregMd;
    mod := 0;
    lev := curLev;
    off := 0;
    register := reg;

    isParam := FALSE;
    indir := FALSE;
    name := 0;
    notpop := FALSE;
    indexed := FALSE;
    indexReg := erNone;
    notpop := FALSE;
    parent := NIL;

    adr := RegAdr(reg);
  END (*WITH*);
END SetDregMd;

PROCEDURE Branch(cond : Condition; VAR tJmp, fJmp : CARDINAL);
(* jump forward, build chain. *)
VAR
  l1:     CARDINAL;
BEGIN
  IF cond <> T THEN
    IF fJmp = 0 THEN
      newLink(fJmp);
    END;

    CASE cond OF
      T, F:
      |
      HI: PutLinkReference(BCC, fJmp, 2);
          PutLinkReference(BEQ, fJmp, 2);
      |
      LS: newLink(l1);
          PutLinkReference(BCC, l1, 2);
          PutLinkReference(BNE, fJmp, 2);
          FixLink(l1);
      |
      CC: PutLinkReference(BCS, fJmp, 2);
      |
      CS: PutLinkReference(BCC, fJmp, 2);
      |
      NE: PutLinkReference(BEQ, fJmp, 2);
      |
      EQ: PutLinkReference(BNE, fJmp, 2);
      |
      VC: PutLinkReference(BVS, fJmp, 2);
      |
      VS: PutLinkReference(BVC, fJmp, 2);
      |
      PL: PutLinkReference(BMI, fJmp, 2);
      |
      MI: newLink(l1);
          PutLinkReference(BEQ, l1, 2);
          PutLinkReference(BCS, fJmp, 2);
          FixLink(l1);
      |
      GE: PutLinkReference(BCC, fJmp, 2);
      |
      LT: PutLinkReference(BCS, fJmp, 2);
      |
      GT: PutLinkReference(BCC, fJmp, 2);
          PutLinkReference(BEQ, fJmp, 2);
      |
      LE: newLink(l1);
          PutLinkReference(BEQ, l1, 2);
          PutLinkReference(BCS, fJmp, 2);
          FixLink(l1);
      |
      FF:   (* for true *)
      |
      FEQ:  (* for neq *)
          PutLinkReference(BEQ, fJmp, 2);
      |
      FGT:  (* for not gtr *)
          PutLinkReference(BEQ, fJmp, 2);
      |
      FGE:  (* for not geq *)
          PutLinkReference(BEQ, fJmp, 2);
      |
      FLT:  (* for not lss *)
          PutLinkReference(BNE, fJmp, 2);
      |
      FLE:  (* for not leq *)
          PutLinkReference(BNE, fJmp, 2);
      |
      FGL:  (* for not ? *)
      |
      FGLE: (* for not ? *)
      |
      FNGLE:(* for ? *)
      |
      FNGL: (* for ? *)
      |
      FNLE: (* for leq *)
          PutLinkReference(BEQ, fJmp, 2);
      |
      FNLT: (* for lss *)
          PutLinkReference(BEQ, fJmp, 2);
      |
      FNGE: (* for geq *)
          PutLinkReference(BNE, fJmp, 2);
      |
      FNGT: (* for gtr *)
          PutLinkReference(BNE, fJmp, 2);
      |
      FNE:  (* for eql *)
          PutLinkReference(BNE, fJmp, 2);
      |
      FT:   (* for false *)
    END;
  END;

  IF tJmp = 0 THEN
    newLink(tJmp);
  END;

  IF (cond <> F) AND (cond <> FF) THEN
    PutLinkReference(BRL, tJmp, 3);
  END;
END Branch;

PROCEDURE BranchWith(cond : Condition; VAR tJmp, fJmp, wJmp: CARDINAL);
(* jump forward, build chain. *)
VAR
  l1:     CARDINAL;
BEGIN
  IF cond <> T THEN
    IF fJmp = 0 THEN
      newLink(fJmp);
    END;

    CASE cond OF
      T, F:
      |
      HI: PutLinkReference(BCC, fJmp, 2);
          PutLinkReference(BEQ, fJmp, 2);
      |
      LS: newLink(l1);
          PutLinkReference(BCC, l1, 2);
          PutLinkReference(BNE, fJmp, 2);
          FixLink(l1);
      |
      CC: PutLinkReference(BCS, fJmp, 2);
      |
      CS: PutLinkReference(BCC, fJmp, 2);
      |
      NE: PutLinkReference(BEQ, fJmp, 2);
      |
      EQ: PutLinkReference(BNE, fJmp, 2);
      |
      VC: PutLinkReference(BVS, fJmp, 2);
      |
      VS: PutLinkReference(BVC, fJmp, 2);
      |
      PL: PutLinkReference(BMI, fJmp, 2);
      |
      MI: newLink(l1);
          PutLinkReference(BEQ, l1, 2);
          PutLinkReference(BCS, fJmp, 2);
          FixLink(l1);
      |
      GE: PutLinkReference(BCC, fJmp, 2);
      |
      LT: PutLinkReference(BCS, fJmp, 2);
      |
      GT: PutLinkReference(BCC, fJmp, 2);
          PutLinkReference(BEQ, fJmp, 2);
      |
      LE: newLink(l1);
          PutLinkReference(BEQ, l1, 2);
          PutLinkReference(BCS, fJmp, 2);
          FixLink(l1);
      |
      FF:   (* for true *)
      |
      FEQ:  (* for neq *)
          PutLinkReference(BEQ, fJmp, 2);
      |
      FGT:  (* for not gtr *)
          PutLinkReference(BEQ, fJmp, 2);
      |
      FGE:  (* for not geq *)
          PutLinkReference(BEQ, fJmp, 2);
      |
      FLT:  (* for not lss *)
          PutLinkReference(BNE, fJmp, 2);
      |
      FLE:  (* for not leq *)
          PutLinkReference(BNE, fJmp, 2);
      |
      FGL:  (* for not ? *)
      |
      FGLE: (* for not ? *)
      |
      FNGLE:(* for ? *)
      |
      FNGL: (* for ? *)
      |
      FNLE: (* for leq *)
          PutLinkReference(BEQ, fJmp, 2);
      |
      FNLT: (* for lss *)
          PutLinkReference(BEQ, fJmp, 2);
      |
      FNGE: (* for geq *)
          PutLinkReference(BNE, fJmp, 2);
      |
      FNGT: (* for gtr *)
          PutLinkReference(BNE, fJmp, 2);
      |
      FNE:  (* for eql *)
          PutLinkReference(BNE, fJmp, 2);
      |
      FT:   (* for false *)
    END;
  END;

  FixLink(wJmp);

  IF tJmp = 0 THEN
    newLink(tJmp);
  END;

  IF (cond <> F) AND (cond <> FF) THEN
    PutLinkReference(BRL, tJmp, 3);
  END;
END BranchWith;

PROCEDURE LoadCC(VAR x : Item);
(* convert from 'cocMd' to 'regMd' while *)
(* generating conditional code.           *)
VAR
  done:   CARDINAL;
BEGIN
  WITH x DO
    IF NOT errorInProc THEN
      Assert( mode = cocMd );

      IF (Fjmp = 0) AND (Tjmp = 0) AND (CC < HI) THEN
        IF InvertCC(CC) = F THEN
          PutDPReference(LDA+IMMEDIATE, 0, 3);
        ELSE
          PutDPReference(LDA+IMMEDIATE, 1, 3);
        END;
      ELSE
        newLink(done);
        Branch(CC, Fjmp, Tjmp);

        IF CC <> T THEN
          FixLink(Tjmp);
        END;

        PutDPReference(LDA+IMMEDIATE, 1, 3);
        PutLinkReference(BRA, done, 2);
        FixLink(Fjmp);
        PutDPReference(LDA+IMMEDIATE, 0, 3);
        FixLink(done);
      END;
    END;

    (* transform 'cocMd' to 'regMd' *)
    SetregMd(x, booltyp);
  END (*WITH*);
END LoadCC;

PROCEDURE ExternalCall(mno, pno : CARDINAL; pname: CARDINAL);
  (* call of the external procedure #pno in module #mno. *)
VAR
  idName:   aSymbol;
  modName:  aSymbol;
  OK:       BOOLEAN;
BEGIN
  IF NOT errorInProc THEN
    IF pname = 0 THEN
      (*
        Illegal procedure number. It means that this is a call to a module
        initialisation procedure.
      *)
      GetModuleKey(mno, idName, OK);
    ELSE
      (*
        Otherwise get the procedure name from the ID buffer.
      *)
      IdToSymbol(pname, idName);
      Concat('_', idName, idName);
    END;
  
    GetModuleName(mno, modName, OK);
  
    (*
      GetModuleName returns false if the module is an interface module (ie, it
      has no implementation).  If we are trying to call the Initialisation
      procedure for such a module, then don't, as none exists.
    *)
  
    IF (OK) OR
       (pname <> 0) THEN
      Concat(modName, idName, idName);
      GenGlobalOp(JSL, idName, 0, 0, 3);
      AddBytesToObjectFile(4);  
    END;
  END;
END ExternalCall;

PROCEDURE GenTerminate(reason: aTerminateStatus);
(*
  OPERATION:
    Generates code to terminate the execution of the program abnormally.

    The code generated is:

        LONGM
        REP  #$20
        PHK
        PER  $FFFA ;-6
        PEA  reason
        JMP  M2Lib__Crash

    Note: If adding to the code generated here, the number of bytes generated
          must not exceed 120 bytes.  The reason for this is that we want to
          be able to do a relative branch around this code.
*)
BEGIN
  LongM;
  PutOp(PHK, 1);
  PutDPReference(PER, 0FFFAH, 3);
  PutDPReference(PEA, ORD(reason), 3);
  GenGlobalOp(JMP+JABSL, 'M2Lib__Crash', 0, 0, 3);
  AddBytesToObjectFile(4);
END GenTerminate;

PROCEDURE OvflTrap(signed : BOOLEAN);
  (* overflow-check thru TRAPV for signed arithmetic : *)
VAR
  noOverflow: CARDINAL;
BEGIN
  IF NOT ovflchk THEN
    RETURN;
  END;

  IF signed THEN
    newLink(noOverflow);
    PutLinkReference(BVC, noOverflow, 2);
    GenTerminate(tsOverflow);
    FixLink(noOverflow);
  END;
END OvflTrap;

PROCEDURE StackTop(i : INTEGER);
(*
  increment/decrement stack pointer SP:

    i > 0 :  increment SP, reset stack
    i < 0 :  decrement SP, reserve stack

  IMPORTANT:
    DO NOT AFFECT, ALTER, OR IN ANY WAY CHANGE THE X REGISTER IN THIS 
    PROCEDURE.  THE PROCEDURE "In2" ASSUMES THAT THE VALUE OF X IS SAFE WITHIN
    THIS PROCEDURE.
*)
VAR
  neg:        BOOLEAN;
  c:          INTEGER;
  abs:        CARDINAL;
  acclocked:  BOOLEAN;
BEGIN
  IF i <> 0 THEN
    neg := (i < 0);
    abs := ABS(i);
    acclocked := Locked(erCPU);

    IF neg THEN
      IF ((abs < 5) AND NOT acclocked) OR
         (abs < 7) THEN
        IF ODD(abs) THEN
          PutOp(PHB, 1);
          DEC(abs);
        END;

        WHILE abs > 0 DO
          PutOp(PHY, 1);
          DEC(abs, 2);
        END;

        GenStackCheck(FALSE);
      ELSE
        IF acclocked THEN
          PutOp(TAY, 1);                     (* save A if necessary      *)
        END;

        PutOp(TSC, 1);                       (* Transfer stack to A      *)
        PutOp(CLC, 1);                       (* Clear carry for addition *)

        PutDPReference(ADC+IMMEDIATE, i, 3); (* Add/Subtract offset to A *)

        PutOp(TCS, 1);                       (* Place new value in SP    *)

        GenStackCheck(TRUE);                 (* check the stack position *)

        IF acclocked THEN
          PutOp(TYA, 1);                     (* restore A if necessary   *)
        END;
      END;
    ELSE
      IF abs = 1 THEN
        PutOp(PHB, 1);
        PutOp(PLY, 1);
      ELSIF abs = 2 THEN
        PutOp(PLY, 1);
      ELSIF (abs = 4) AND acclocked THEN
        PutOp(PLY, 1);
        PutOp(PLY, 1);
      ELSE
        IF acclocked THEN
          PutOp(TAY, 1);                        (* save A if necessary      *)
        END;

        PutOp(TSC, 1);                          (* xfer stack to A          *)

        IF ((abs < 3) AND NOT acclocked) THEN
          WHILE abs > 0 DO
            PutOp(INCACC, 1);                   (* increment A by one byte  *)
            DEC(abs);
          END;
        ELSE
          PutOp(CLC, 1);                        (* Clear carry for addition *)
          PutDPReference(ADC+IMMEDIATE, i, 3);  (* Add/Subtract offset to A *)
        END;

        PutOp(TCS, 1);                          (* Place new value in SP    *)

        IF acclocked THEN
          PutOp(TYA, 1);                        (* restore A if necessary   *)
        END;
      END;
    END;

    stackPointer := stackPointer - i;
  END;
END StackTop;

PROCEDURE InitM2HM;
  VAR k : CARDINAL; exp : LONGINT;
BEGIN
  curLev := 0;
  exp := 0;

  registers := DRegSet{};
  stackPointer := 0;
  lockItem := NIL;

  mask[0] := 0;
  mask[32] := -1;
  FOR k := 1 TO 31 DO
    exp := exp + exp + VAL(LONGINT, 1);
    mask[k] := exp;
  END;

  IF DynArrDesSize = 6 THEN
    hightyp := inttyp;
  ELSE
    hightyp := dbltyp;
  END;

  errorInProc := FALSE;
  dontLoseIndex := FALSE;
END InitM2HM;

PROCEDURE ConvertTyp(functyp : StrPtr; VAR x : Item);
VAR
  fs, xs:   CARDINAL;
  szf, szx: WidType;
  y:        Item;
BEGIN
  SetstkMd(y, functyp);  (* dummy for SimpleT *)

  WITH x DO
    fs := functyp^.size;
    xs := typ^.size;

    IF fs <> xs THEN
      IF SimpleT(x) & SimpleT(y) THEN
        Isz(x, szx);
        Isz(y, szf);

        IF mode = conMd THEN
          SetconMd(x, LongVal(x), functyp);
        ELSIF (mode < conMd) OR (mode = cocMd) THEN
          LoadX(x, szf);
        ELSE
          Mark(81);
          Release(x);
        END;
      ELSE
        Mark(81);
        Release(x);
      END;
    END;

    typ := functyp; (* type of x IS changed ! *)

    IF SimpleT(y) THEN
      Isz(y,wid);
    END;
  END (*WITH*);
END ConvertTyp;

PROCEDURE GenHalt(haltindex : CARDINAL);
BEGIN
  haltindex := haltindex MOD 256;

  IF (haltindex <> 0) & NOT(rngchk) THEN
    RETURN
  END;

  GenTerminate(tsHalt);
END GenHalt;

PROCEDURE Power2(VAR x : Item; VAR exp2 : CARDINAL) : BOOLEAN;
  (* Note : negative numbers must NOT return as power of 2. *)
VAR
  pw2:  BOOLEAN;
  v:    LONGINT;

BEGIN
  exp2 := 0;
  pw2 := FALSE;

  IF SimpleC(x) THEN
    v := LongVal(x);
    pw2 := (v >= VAL(LONGINT, 1));              (* 1 = 2**0 *)

    WHILE (v > VAL(LONGINT, 1)) & pw2 DO
      pw2 := NOT ODD(v);
      v := SHIFT(v, -1);           (* v := v DIV 2D;  *)
      INC(exp2);                   (* side effect of Power2 *)
    END;
  END;

  RETURN pw2                       (* 0 <= exp2 <= 31 *)
END Power2;

PROCEDURE LoadIndexTo(reg: aCpuReg; indexReg: DRegister);
BEGIN
  CASE reg OF
    YReg:       PutDPReference(LDY+DIRECT, RegAdr(indexReg), 2);
    |
    XReg:       PutDPReference(LDX+DIRECT, RegAdr(indexReg), 2);
    |
    AReg:       PutDPReference(LDA+DIRECT, RegAdr(indexReg), 2);
    |
    AddToA:     PutOp(CLC, 1);
                PutDPReference(ADC+DIRECT, RegAdr(indexReg), 2);
  END;

  (*
    Some operations (INCL, EXCL, INC, DEC) carry out two operations (LOAD & 
    STORE) on the same item.  If that item has been indexed, then normally the
    index is lost after the LOAD.  M2EM sets the flag "dontLoseIndex" to prevent
    this from happening.
  *)
  IF NOT dontLoseIndex THEN
    Unlock(indexReg);
  END;
END LoadIndexTo;

PROCEDURE LoadVal(VAR x: Item; destReg: DRegister);
VAR
  moduleName:   aSymbol;
  line:         aSymbol;
  variableName: aSymbol;
  offset:       aAdr;
  reg:          DRegister;
  loc:          aAdr;
  dispAdr:      CARDINAL;
  op:           CARDINAL;
  szx:          WidType;

  PROCEDURE LoadIndirect(loc: aAdr; off: aAdr; indexed: BOOLEAN);
  BEGIN
    IF szx = long THEN
      INC(off, 2);
    END;

    IF off <> 0 THEN
      IF indexed THEN
        PutOp(TYA, 1);
        PutOp(CLC, 1);
        PutDPReference(ADC+IMMEDIATE, off, 3);
        PutOp(TAY, 1);
      ELSE
        PutDPReference(LDY+IMMEDIATE3, off, 3);
      END;
    END;

    IF szx = long THEN
      PutDPReference(LDA+DIRINDBYYLG, loc, 2);
      PutOp(TAX, 1);
    END;

    IF (off <> 0) OR indexed THEN
      IF szx = long THEN
        PutOp(DEY, 1);
        PutOp(DEY, 1);
      END;

      PutDPReference(LDA+DIRINDBYYLG, loc, 2);
    ELSE
      PutDPReference(LDA+DIRINDLONG, loc, 2);
    END;
  END LoadIndirect;

BEGIN
  Isz(x, szx);

  WITH x DO
    IF mode <> regMd THEN
      CheckCPUUsage(x);
    END;

    IF NOT errorInProc THEN
      IF mode = conMd THEN (* x is a constant *)
        IF typ^.form <> Set THEN
          IF szx = long THEN
            PutDPReference(LDX+IMMEDIATE3, HighWORD(val.D), 3);
            PutDPReference(LDA+IMMEDIATE, LoWORD(val.D), 3);
          ELSIF szx = word THEN
            IF destReg IN DRegSet{erCPU, erAcc} THEN
              PutDPReference(LDA+IMMEDIATE, val.C, 3);
            ELSIF destReg = erXreg THEN
              PutDPReference(LDX+IMMEDIATE3, val.C, 3);
            ELSIF destReg = erYreg THEN
              PutDPReference(LDY+IMMEDIATE3, val.C, 3);
            END;
          ELSE
            PutDPReference(LDA+IMMEDIATE, ORD(val.Ch), 3);
          END;
        ELSE (* x is a set constant *)
          IF typ^.size < VAL(aSize, 3) THEN (* 1 or 2 bytes *)
            PutDPReference(LDA+IMMEDIATE, val.FS.set[0], 3);
          ELSE (* 4 bytes *)
            PutDPReference(LDX+IMMEDIATE3, val.FS.set[1], 3);
            PutDPReference(LDA+IMMEDIATE, val.FS.set[0], 3);
          END;
        END;
      ELSIF mode = glbMd THEN (* global variable *)
        GetVariableName(x, variableName);

        IF indexed THEN
          LoadIndexTo(XReg, indexReg);
        END;

        IF indir THEN
          offset := adr;
          GetReg(long, reg);
          loc := RegAdr(reg);
        ELSE
          offset := adr + off;
        END;

        IF (szx = long) OR indir THEN
          IF indexed AND NOT indir THEN
            GenGlobalOp(LDA+ABSLONGBYX, variableName, offset+2, 0, 3);
          ELSE
            GenGlobalOp(LDA+ABSLONG, variableName, offset+2, 0, 3);
          END;

          AddBytesToObjectFile(4);

          PutOp(TAY, 1);
        END;

        IF indexed AND NOT indir THEN
          GenGlobalOp(LDA+ABSLONGBYX, variableName, offset, 0, 3);
        ELSE
          GenGlobalOp(LDA+ABSLONG, variableName, offset, 0, 3);
        END;

        AddBytesToObjectFile(4);

        IF indir THEN
          (* variable is being referenced indirectly *)
          PutDPReference(STA+DIRECT, loc, 2);
          PutDPReference(STY+DIRECT, loc+2, 2);

          GenNILCheck(reg, FALSE);

          IF indexed THEN
            PutOp(TXY, 1);
          END;

          LoadIndirect(loc, off, indexed);

          Unlock(reg);
        ELSIF szx = long THEN
          PutOp(TYX, 1);
        END;

        IF destReg = erXreg THEN
          PutOp(TAX, 1);
        ELSIF destReg = erYreg THEN
          PutOp(TAY, 1);
        END;

        IF szx = byte THEN
          PutDPReference(And+IMMEDIATE, 00FFH, 3);
        END;
      ELSIF mode = locMd THEN
        IF indir THEN
          offset := adr;
        ELSE
          offset := adr + off;
        END;

        IF NOT indir THEN
          IF ((VAL(aSize, offset) + typ^.size) < VAL(aSize, DirectPageSize)) AND
             (lev = curLev) THEN
            (*
              The item fits entirely within the DP.  We may thus use DP
              addressing.
            *)
            IF indexed THEN
              IF destReg = erXreg THEN
                LoadIndexTo(YReg, indexReg);
              ELSE
                LoadIndexTo(XReg, indexReg);
              END;
            END;

            IF indexed THEN
              IF destReg IN DRegSet{erCPU, erAcc} THEN
                op := LDA+DIRECTBYX;
              ELSIF destReg = erXreg THEN
                op := LDX+DIRECTBYY;
              ELSIF destReg = erYreg THEN
                op := LDY+DIRECTBYX;
              END;
            ELSE
              IF destReg IN DRegSet{erCPU, erAcc} THEN
                op := LDA+DIRECT;
              ELSIF destReg = erXreg THEN
                op := LDX+DIRECT;
              ELSIF destReg = erYreg THEN
                op := LDY+DIRECT;
              END;
            END;

            PutDPReference(op, offset, 2);

            IF szx = long THEN
              IF indexed THEN
                PutDPReference(LDY+DIRECTBYX, offset+2, 2);
              ELSE
                PutDPReference(LDY+DIRECT, offset+2, 2);
              END;

              PutOp(TYX, 1);
            END;
          ELSE
            (*
              The item doesn't fit within the DP, so we must use 16 bit absolute
              addressing.
            *)
            IF lev = curLev THEN
              PutOp(TDC, 1);
            ELSE
              dispAdr := (lev -1) * 2;
  
              GenGlobalOp(LDA+ABSLONG, 'M2Lib_Display', dispAdr, 0, 3);
              AddBytesToObjectFile(4);
            END;

            IF indexed THEN
              LoadIndexTo(AddToA, indexReg);
            END;

            PutOp(CLC, 1);

            IF lev = curLev THEN
              PutDPReference(ADC+IMMEDIATE, offset, 3);
            ELSE
              (*
                Other level locals have not had their addresses adjusted yet,
                so we must do so now.
              *)
              IF isParam THEN
                PutDisplayReference(ADC+IMMEDIATE, offset + WorkingStorage,
                                    parent, 3);
              ELSE
                PutDPReference(ADC+IMMEDIATE, offset + WorkingStorage, 3);
              END;
            END;

            PutOp(TAX, 1);

            IF szx = long THEN
              PutLongReference(LDA+ABSLONGBYX, 2, 4);
              PutOp(TAY, 1);
              PutLongReference(LDA+ABSLONGBYX, 0, 4);
              PutOp(TYX, 1);
            ELSE
              PutLongReference(LDA+ABSLONGBYX, 0, 4);
            END;
          END;
        ELSE  (* indir *)
          IF (lev <> curLev) OR
             ((lev = curLev) AND
              ((VAL(aSize, offset) + typ^.size) >= VAL(aSize, DirectPageSize))) THEN
            IF lev = curLev THEN
              PutOp(TDC, 1);
            ELSE
              dispAdr := (lev -1) * 2;
  
              GenGlobalOp(LDA+ABSLONG, 'M2Lib_Display', dispAdr, 0, 3);
              AddBytesToObjectFile(4);
            END;

            PutOp(CLC, 1);

            IF lev = curLev THEN
              PutDPReference(ADC+IMMEDIATE, offset, 3);
            ELSE
              (*
                Other level locals have not had their addresses adjusted yet,
                so we must do so now.
              *)
              IF isParam THEN
                PutDisplayReference(ADC+IMMEDIATE, offset + WorkingStorage,
                                    parent, 3);
              ELSE
                PutDPReference(ADC+IMMEDIATE, offset + WorkingStorage, 3);
              END;
            END;

            PutOp(TAX, 1);

            GetReg(long, reg);
            loc := RegAdr(reg);

            PutLongReference(LDA+ABSLONGBYX, 0, 4);
            PutDPReference(STA+DIRECT, loc, 2);
            PutLongReference(LDA+ABSLONGBYX, 2, 4);
            PutDPReference(STA+DIRECT, loc+2, 2);

            GenNILCheck(reg, FALSE);
            offset := loc;
            Unlock(reg);
          END;
  
          IF indexed THEN
            LoadIndexTo(YReg, indexReg);
          END;
  
          LoadIndirect(offset, off, indexed);
        END;
  
        IF szx = byte THEN
          PutDPReference(And+IMMEDIATE, 00FFH, 3);
        END;
      ELSIF mode = stkMd THEN
        IF indir THEN
          loc := stackPointer - adr;

          IF (loc = 0) AND
             (NOT notpop) THEN
            PutOp(PLA, 1);
            PutOp(PLX, 1);

            DEC(stackPointer, 4);
          ELSE
            PutStackReference(LDA+STKRELATIVE, loc+3);
            PutOp(TAX, 1);
            PutStackReference(LDA+STKRELATIVE, loc+1);
          END;

          GetReg(long, reg);
          loc := RegAdr(reg);

          PutDPReference(STA+DIRECT, loc, 2);
          PutDPReference(STX+DIRECT, loc+2, 2);

          GenNILCheck(reg, FALSE);

          IF indexed THEN
            LoadIndexTo(YReg, indexReg);
          END;

          LoadIndirect(loc, off, indexed);

          Unlock(reg);
        ELSE
          IF indexed THEN
            (*
              ### Unable to index a VALUE that is on the stack!
            *)
            Mark(402);
          END;

          IF szx = byte THEN
            ShortM;
          END;

          loc := stackPointer - adr;

          IF notpop THEN
            IF szx = long THEN
              PutStackReference(LDA+STKRELATIVE, loc+3);
              PutOp(TAX, 1);
            END;

            PutStackReference(LDA+STKRELATIVE, loc+1);
          ELSE
            PutOp(PLA, 1);

            IF szx = long THEN
              PutOp(PLX, 1);
            END;

            DEC(stackPointer, typ^.size);
          END;

          IF szx = byte THEN
            LongM;
          END;
        END;

        IF szx = byte THEN
          PutDPReference(And+IMMEDIATE, 00FFH, 3);
        END;
      ELSIF mode = DregMd THEN
        IF indir THEN
          GenNILCheck(reg, FALSE);

          IF indexed THEN
            LoadIndexTo(YReg, indexReg);
          END;

          LoadIndirect(adr, off, indexed);
        ELSE
          IF indexed THEN
            (*
              ### Unable to index a D Register
            *)
            Mark(403);
          END;

          IF destReg IN DRegSet{erCPU, erAcc} THEN
            op := LDA+DIRECT;
          ELSIF destReg = erXreg THEN
            op := LDX+DIRECT;
          ELSIF destReg = erYreg THEN
            op := LDY+DIRECT;
          END;

          PutDPReference(op, adr, 2);

          IF szx = long THEN
            PutDPReference(LDX+DIRECT, adr+2, 2);
          END;
        END;

        Release(x);

        IF szx = byte THEN
          PutDPReference(And+IMMEDIATE, 00FFH, 3);
        END;
      ELSIF mode = cocMd THEN
        LoadCC(x);
      ELSIF mode = regMd THEN
        (*
          The item may already be in reg mode.  This could happen if we loaded
          the address in a pointer, or when a previous call to LoadVal was made.

          If the former, AND the item is being referenced indirectly now, then we
          are currently being asked to load the value pointed to by the address
          currently in the registers.

          If the latter, then nothing need be done as the value is already in the
          registers, unless this is part of a GETREG or SETREG call.  If that is
          the case, then we must ensure that the correct register is loaded.
        *)
        IF indir THEN
          GetReg(long, reg);
          loc := RegAdr(reg);

          PutDPReference(STA+DIRECT, loc, 2);
          PutDPReference(STX+DIRECT, loc+2, 2);

          GenNILCheck(reg, FALSE);

          IF indexed THEN
            LoadIndexTo(YReg, indexReg);
          END;
  
          LoadIndirect(loc, off, indexed);
  
          Unlock(reg);

          IF szx = byte THEN
            PutDPReference(And+IMMEDIATE, 00FFH, 3);
          END;
        ELSE
          IF szx = word THEN
            IF destReg = erXreg THEN
              PutOp(TAX, 1);
            ELSIF destReg = erYreg THEN
              PutOp(TAY, 1);
            END;
          END;
        END;
      ELSIF mode = procMd THEN
      END;
    END;

    SetregMd(x, x.typ);
    x.register := destReg;
  END; (* WITH x *)
END LoadVal;

PROCEDURE StoreVal(VAR x: Item; fromReg: DRegister);
(*
  OPERATION:
    The opposite of LoadVal.

  NOTE:
    THIS PROCEDURE ASSUMES THAT THE VARIABLE IT IS STORING THE VALUE IN IS
    NOT BEING REFERENCED INDIRECTLY!!!

    THIS PROCEDURE IS SUPPLIED PURELY TO OPTIMISE THE CODE, AND IS ONLY CALLED
    FROM ONE PLACE, "Move".
*)
VAR
  moduleName:   aSymbol;
  line:         aSymbol;
  variableName: aSymbol;
  offset:       aAdr;
  reg:          DRegister;
  dispAdr:      CARDINAL;
  szx:          WidType;
BEGIN
  Isz(x, szx);

  WITH x DO
    IF NOT errorInProc THEN
      IF mode = glbMd THEN (* global variable *)
        GetVariableName(x, variableName);

        IF szx = byte THEN
          ShortM;
        END;

        IF szx = long THEN
          PutOp(TXY, 1);
        END;

        IF indexed THEN
          LoadIndexTo(XReg, indexReg);
        END;

        IF fromReg IN DRegSet{erXreg, erYreg} THEN
          PutOp(PHA, 1);
        END;

        IF fromReg = erXreg THEN
          PutOp(TXA, 1);
        ELSIF fromReg = erYreg THEN
          PutOp(TYA, 1);
        END;

        offset := adr + off;

        IF indexed THEN
          GenGlobalOp(STA+ABSLONGBYX, variableName, offset, 0, 3);
        ELSE
          GenGlobalOp(STA+ABSLONG, variableName, offset, 0, 3);
        END;

        AddBytesToObjectFile(4);

        IF fromReg IN DRegSet{erXreg, erYreg} THEN
          PutOp(PLA, 1);
        END;

        IF szx = long THEN
          PutOp(TYA, 1);
      
          INC(offset, 2);

          IF indexed THEN
            GenGlobalOp(STA+ABSLONGBYX, variableName, offset, 0, 3);
          ELSE
            GenGlobalOp(STA+ABSLONG, variableName, offset, 0, 3);
          END;

          AddBytesToObjectFile(4);
        END;

        IF szx = byte THEN
          LongM;
        END;
      ELSIF mode = locMd THEN
        offset := adr + off;

        IF ((VAL(aSize, offset) + typ^.size) < VAL(aSize, DirectPageSize)) AND
           (lev = curLev) THEN
          (*
            The item fits entirely within the DP.  We may thus use DP
            addressing.
          *)
          IF szx = byte THEN
            ShortM;
          END;

          IF fromReg = erXreg THEN
            PutOp(TXA, 1);
          ELSIF fromReg = erYreg THEN
            PutOp(TYA, 1);
          END;

          IF indexed THEN
            IF szx = long THEN
              LoadIndexTo(YReg, indexReg);
              PutOp(PHX, 1);
              PutOp(TYX, 1);
              PutOp(PLY, 1);
            ELSE
              LoadIndexTo(XReg, indexReg);
            END;
          END;

          IF indexed THEN
            PutDPReference(STA+DIRECTBYX, offset, 2);
          ELSE
            PutDPReference(STA+DIRECT, offset, 2);
          END;

          IF szx = long THEN
            IF indexed THEN
              PutOp(TYA, 1);
            ELSE
              PutOp(TXA, 1);
            END;

            IF indexed THEN
              PutDPReference(STA+DIRECTBYX, offset+2, 2);
            ELSE
              PutDPReference(STA+DIRECT, offset+2, 2);
            END;
          END;
        ELSE
          (*
            The item doesn't fit within the DP, so we must use absolute
            addressing.
          *)
          IF lev <> curLev THEN
            dispAdr := (lev -1) * 2;

            PutOp(PHA, 1);
            GenGlobalOp(LDA+ABSLONG, 'M2Lib_Display', dispAdr, 0, 3);
            AddBytesToObjectFile(4);

            PutOp(CLC, 1);

            IF isParam THEN
              PutDisplayReference(ADC+IMMEDIATE, offset + WorkingStorage,
                                  parent, 3);
            ELSE
              PutDPReference(ADC+IMMEDIATE, offset + WorkingStorage, 3);
            END;

            PutOp(TAY, 1);
            PutOp(PLA, 1);
          ELSE
            IF (VAL(aSize, offset) + typ^.size) >= VAL(aSize, DirectPageSize) THEN
              PutOp(PHA, 1);
              PutOp(TDC, 1);
              PutOp(CLC, 1);
              PutDPReference(ADC+IMMEDIATE, offset, 3);
              PutOp(TAY, 1);
              PutOp(PLA, 1);
            END;
          END;

          IF indexed THEN
            GetReg(word, reg);
            PutDPReference(STA+DIRECT, RegAdr(reg), 2);

            PutOp(TYA, 1);
            LoadIndexTo(AddToA, indexReg);
            PutOp(TAY, 1);

            PutDPReference(LDA+DIRECT, RegAdr(reg), 2);
            Unlock(reg);
          END;
  
          IF szx = byte THEN
            ShortM;
          END;

          IF szx = long THEN
            PutOp(PHX, 1);  (* swap X and Y so that X can be the index *)
            PutOp(TYX, 1);
            PutOp(PLY, 1);
            PutLongReference(STA+ABSLONGBYX, 0, 4);
            PutOp(TYA, 1);
            PutLongReference(STA+ABSLONGBYX, 2, 4);
          ELSE
            PutOp(TYX, 1);
            PutLongReference(STA+ABSLONGBYX, 0, 4);
          END;
        END;
  
        IF szx = byte THEN
          LongM;
        END;
      ELSIF mode = DregMd THEN
        IF indexed THEN
          (*
            ### Cannot index a D Register.
          *)
          Mark(404);
        END;

        PutDPReference(STA+DIRECT, adr, 2);
  
        IF szx = long THEN
          PutDPReference(STX+DIRECT, adr+2, 2);
        END;
      ELSIF mode = regMd THEN
      ELSE
        (*
          ### Mode is other than global or local!
        *)
        Mark(405);
      END;
    END;
  END; (* WITH x *)

  Unlock(erCPU);
END StoreVal;

PROCEDURE PushVal(VAR x: Item);
VAR
  szx:  WidType;
BEGIN
  IF (x.mode <> stkMd) OR
     ((x.mode = stkMd) AND
      ((stackPointer - x.adr <> 0) OR (x.indir AND x.notpop))) THEN
    Isz(x, szx);

    IF (x.mode = conMd) AND (szx > byte) THEN
      CheckCPUUsage(x);

      IF x.typ^.form <> Set THEN
        IF szx = long THEN
          PutDPReference(PEA, HighWORD(x.val.D), 3);
          PutDPReference(PEA, LoWORD(x.val.D), 3);
        ELSIF szx = word THEN
          PutDPReference(PEA, x.val.C, 3);
        END;
      ELSE (* x is a set constant *)
        IF x.typ^.size < VAL(aSize, 3) THEN (* 1 or 2 bytes *)
          PutDPReference(PEA, x.val.FS.set[0], 3);
        ELSE (* 4 bytes *)
          PutDPReference(PEA, x.val.FS.set[1], 3);
          PutDPReference(PEA, x.val.FS.set[0], 3);
        END;
      END;
    ELSE
      LoadVal(x, erCPU);

      WITH x.typ^ DO
        IF szx = byte THEN
          ShortM;
          PutOp(PHA, 1);
          LongM;
        ELSIF szx = word THEN
          PutOp(PHA, 1);
        ELSIF szx = long THEN
          PutOp(PHX, 1);
          PutOp(PHA, 1);
        ELSE
          Mark(411);
        END;
      END;

      Unlock(erCPU);
    END;

    INC(stackPointer, x.typ^.size);

    SetstkMd(x, x.typ);
  END;
END PushVal;

PROCEDURE MulPw2(VAR x: Item; exp : CARDINAL);      (* V2.6 *)
  (*       x * (power of 2)               *)
  (* relevant is the width, not the size! *)
VAR
  op :        aSymbol;
  szx:        WidType;
  loopStart:  CARDINAL;
  noover:     CARDINAL;
  noover1:    CARDINAL;
BEGIN
  Isz(x,szx);
  Assert(exp <= 31);

  IF exp <> 0 THEN
    LoadVal(x, erCPU);

    IF szx = byte THEN
      ShortM;
    END;

    IF (szx = byte) OR (szx = word) THEN
      WHILE exp > 0 DO
        PutOp(ASL+IMMEDIATE, 1);
        DEC(exp);
      END;
    ELSIF szx = long THEN
      newLink(loopStart);

      PutDPReference(LDY+IMMEDIATE3, exp, 3);

      FixLink(loopStart);
      PutOp(ASL+IMMEDIATE, 1);
      PutOp(PHA, 1);
      PutOp(TXA, 1);
      PutOp(ROL+IMMEDIATE, 1);
      PutOp(TAX, 1);
      PutOp(PLA, 1);
      PutOp(DEY, 1);
      PutLinkReference(BNE, loopStart, 2);
    END;

    IF szx = byte THEN
      LongM;
    END;
  END (*exp <> 0*);
END MulPw2;

PROCEDURE MUL2(VAR x, y: Item; ovfl: BOOLEAN);
  (*  x  *  y  --->>  x  *)
VAR
  pw2:        CARDINAL;
  szx, szy:   WidType;
  signar:     BOOLEAN;
  lv:         LONGINT;
  stackSave:  INTEGER;

BEGIN
  IF NOT errorInProc THEN
    Isz(x,szx);
    Isz(y,szy);

    signar := SignedT(x) OR SignedT(y);

    IF Power2(y, pw2) AND NOT ovfl THEN
      MulPw2(x, pw2);
      SetregMd(x, x.typ);
    ELSE
      IF y.mode = regMd THEN
        CheckCPUUsage(y);
      ELSE
        CheckCPUUsage(x);
      END;

      IF szx < long THEN
        IF x.mode = regMd THEN
          PushVal(x);             (* place x on stack *)
          LoadVal(y, erCPU);      (* load y into accumulator *)
          PutOp(PLX, 1);     (* pull x unto x-register *)
          DEC(stackPointer, 2);   (* adjust stack pointer since we pulled *)
        ELSE
          PushVal(y);             (* place y on stack *)
          LoadVal(x, erCPU);      (* load x into accumulator *)
          PutOp(PLX, 1);     (* pull y unto x-register *)
          DEC(stackPointer, 2);   (* adjust stack pointer since we pulled *)
        END;

        Release(x);
        Release(y);

        (*
          We change the modes, so that later on, if some code does a
          Release(y), it doesn't unlock the CPU registers by mistake.
        *)
        SetconMd(x, 0, x.typ);
        SetconMd(y, 0, x.typ);

        IF signar THEN
          GenGlobalOp(JSL, '~MUL2', 0, 0, 3);
          AddBytesToObjectFile(4);
        ELSE
          GenGlobalOp(JSL, '~M2UMul2', 0, 0, 3);
          AddBytesToObjectFile(4);
        END;

        SetregMd(x, x.typ);

        (*
          Unsigned multiplication can also have an overflow, so if we have
          been asked to generate overflow checks, then do so regardless of
          the signed/unsigned nature of the operation.
        *)
        OvflTrap(ovfl);
      ELSE
	      (*
	        Once the operation is complete, the stack will not have been affected,
	        so preserve the current stack position.
	      *)
	      stackSave := stackPointer;

	      IF y.mode = regMd THEN
	        PushVal(y);
	        PushVal(x);
	      ELSE
	        PushVal(x);
	        PushVal(y);
	      END;

        IF signar THEN
          GenGlobalOp(JSL, '~MUL4', 0, 0, 3);
          AddBytesToObjectFile(4);
        ELSE
          GenGlobalOp(JSL, '~M2UMul4', 0, 0, 3);
          AddBytesToObjectFile(4);
        END;

        (*
          These routines replace the first operand with the result, so adjust
          the stack pointer down 4 from the position it had before pushing
          anything.
        *)
        stackPointer := stackSave + 4;

	      (*
	        These two routines return their results on the stack.
	      *)
	      SetstkMd(x, x.typ);

        (*
          Unsigned multiplication can also have an overflow, so if we have
          been asked to generate overflow checks, then do so regardless of
          the signed/unsigned nature of the operation.
        *)
        OvflTrap(ovfl);
      END;
    END;
  ELSE
    SetstkMd(x, x.typ);
  END;
END MUL2;

PROCEDURE SHI2(type: direction; VAR x, y : Item);
  (*  shift left/right x by y.  *)
VAR
  szx:        WidType;
  loopStart:  CARDINAL;
  positive:   CARDINAL;
  skipLoop:   CARDINAL;
  bigShift:   BOOLEAN;
BEGIN
  IF NOT errorInProc THEN
    Isz(x,szx);
    newLink(loopStart);
    newLink(skipLoop);

    LoadParameters(x, y, FALSE);

    (*
      A simple type only has up to 255 bits, so a shift count that is more than
      a word in length is unnecessary.  Load it as a word for simplicity.
    *)
    LoadX(y, word);
    PutLinkReference(BEQ, skipLoop, 2);
    PutOp(TAY, 1);  (* place the count into the y register *)
    Unlock(erCPU);

    IF szx = byte THEN
      ShortM;
    END;

    FixLink(loopStart);

    IF szx < long THEN
      CASE type OF
        left:
          PutDPReference(ASL+DIRECT, x.adr, 2);
        |
        logLeft:
          PutDPReference(ASL+DIRECT, x.adr, 2);
        |
        right:
          PutDPReference(LSR+DIRECT, x.adr, 2);
        |
        rightfill:
          newLink(positive);
          PutOp(CLC, 1);
          PutDPReference(LDA+DIRECT, x.adr, 2);
          PutLinkReference(BPL, positive, 2);
          PutOp(SEC, 1);
          FixLink(positive);
          PutDPReference(ROR+DIRECT, x.adr, 2);
      END;
    ELSIF szx = long THEN
      CASE type OF
        left:
          PutDPReference(ASL+DIRECT, x.adr+2, 2);
          PutDPReference(ROL+DIRECT, x.adr, 2);
        |
        logLeft:
          PutDPReference(ASL+DIRECT, x.adr+2, 2);
          PutDPReference(ROL+DIRECT, x.adr, 2);
        |
        right:
          PutDPReference(LSR+DIRECT, x.adr+2, 2);
          PutDPReference(ROR+DIRECT, x.adr, 2);
        |
        rightfill:
          newLink(positive);
          PutOp(CLC, 1);
          PutDPReference(LDA+DIRECT, x.adr+2, 2);
          PutLinkReference(BPL, positive, 2);
          PutOp(SEC, 1);
          FixLink(positive);
          PutOp(ROR+IMMEDIATE, 1);
          PutDPReference(STA+DIRECT, x.adr+2, 2);
          PutDPReference(ROR+DIRECT, x.adr, 2);
      END;
    ELSE
      Mark(410);
    END;

    PutOp(DEY, 1);
    PutLinkReference(BNE, loopStart, 2);

    IF szx = byte THEN
      LongM;
    END;
  END;

  FixLink(skipLoop);

  Release(y);
  LoadVal(x, erCPU);  (* changes mode to regMd *)
END SHI2;

PROCEDURE Log2(inst: loginst; VAR x, y: Item);
  (* the logical operators AND, OR, EOR.  *)
  (*      x   AND   y  --->>   x          *)
  (*      x   OR    y  --->>   x          *)
  (*      x   EOR   y  --->>   x          *)
  (* Note : x can be a memory location *)
  (*        or on top of stack.        *)
VAR
  szx:      WidType;
  szy:      WidType;
  reg:      DRegister;
  reg2:     DRegister;
  sz:       CARDINAL;
  idx:      CARDINAL;
  loop:     CARDINAL;
  storeAtY: BOOLEAN;
BEGIN
  IF NOT errorInProc THEN
    Isz(x,szx);
    Isz(y,szy);

    IF szx < quad THEN
      LoadParameters(x, y, TRUE);

      IF szx = byte THEN
        ShortM;
      END;

      CASE inst OF
        lAnd: PutDPReference(And+DIRECT, y.adr, 2);
        |
        lOr:  PutDPReference(ORA+DIRECT, y.adr, 2);
        |
        lEor: PutDPReference(EOR+DIRECT, y.adr, 2);
      END;

      IF szx = long THEN
        PutOp(TAY, 1);
        PutOp(TXA, 1);

        CASE inst OF
          lAnd: PutDPReference(And+DIRECT, y.adr+2, 2);
          |
          lOr:  PutDPReference(ORA+DIRECT, y.adr+2, 2);
          |
          lEor: PutDPReference(EOR+DIRECT, y.adr+2, 2);
        END;

        PutOp(TAX, 1);
        PutOp(TYA, 1);
      END;

      IF szx = byte THEN
        LongM;
      END;

      Release(y);
      SetregMd(x, x.typ);
    ELSE (* size of x is 3, 5 or more bytes *)
      IF y.mode = conMd THEN
        (*
          We only get here if we are operating on two items that are 3, 5 or 
          more bytes in length.
        *)
        GetReg(long, reg);
        LoadAdr(x);
        PutDPReference(STA+DIRECT, RegAdr(reg), 2);
        PutDPReference(STX+DIRECT, RegAdr(reg)+2, 2);
        Unlock(erCPU);

        sz := x.typ^.size;
        idx := sz DIV 2;

        (*
          The following loop should operate on all of the item, unless the 
          item is an odd number of bytes, in which case it will do all but the
          last byte.

          Since the result of the operation is normally kept in the registers,
          we must simulate that by pushing it onto the stack where it can be
          manipulated as a part of an expression without altering the original
          value.
        *)

        (*
          Do the top byte if there is one.
        *)
        IF ODD(sz) THEN
          DEC(sz);

          ShortM;
          PutDPReference(LDY+IMMEDIATE3, sz, 3);
          PutDPReference(LDA+IMMEDIATE, VAL(CARDINAL, y.val.FS.set[idx]), 2);

          CASE inst OF
            lAnd: PutDPReference(And+DIRINDBYYLG, RegAdr(reg), 2);
            |
            lOr:  PutDPReference(ORA+DIRINDBYYLG, RegAdr(reg), 2);
            |
            lEor: PutDPReference(EOR+DIRINDBYYLG, RegAdr(reg), 2);
          END;

          PutOp(PHA, 1);

          LongM;
        END;

        WHILE sz > 0 DO
          DEC(idx);
          DEC(sz, 2);

          PutDPReference(LDA+IMMEDIATE, VAL(CARDINAL, y.val.FS.set[idx]), 3);

          IF sz <> 0 THEN
            PutDPReference(LDY+IMMEDIATE3, sz, 3);

            CASE inst OF
              lAnd: PutDPReference(And+DIRINDBYYLG, RegAdr(reg), 2);
              |
              lOr:  PutDPReference(ORA+DIRINDBYYLG, RegAdr(reg), 2);
              |
              lEor: PutDPReference(EOR+DIRINDBYYLG, RegAdr(reg), 2);
            END;
          ELSE
            CASE inst OF
              lAnd: PutDPReference(And+DIRINDLONG, RegAdr(reg), 2);
              |
              lOr:  PutDPReference(ORA+DIRINDLONG, RegAdr(reg), 2);
              |
              lEor: PutDPReference(EOR+DIRINDLONG, RegAdr(reg), 2);
            END;
          END;

          PutOp(PHA, 1);
        END;

        INC(stackPointer, x.typ^.size);
        SetstkMd(x, x.typ);
      ELSE
        (*
          We only get here if we are operating on two items that are 3, 5 or 
          more bytes in length.
        *)
        GetReg(long, reg);
        LoadAdr(x);
        PutDPReference(STA+DIRECT, RegAdr(reg), 2);
        PutDPReference(STX+DIRECT, RegAdr(reg)+2, 2);
        Unlock(erCPU);

        (*
          If 'y' is an intermediary value that has been placed on the stack, 
          then we don't want to leave it there.  Since we will be pushing the
          result of this operation onto the stack, we may as well overwrite
          'y' rather than discard it, and then push a value.  It's neater.
        *)
        storeAtY := (y.mode = stkMd);

        GetReg(long, reg2);
        LoadAdr(y);
        PutDPReference(STA+DIRECT, RegAdr(reg2), 2);
        PutDPReference(STX+DIRECT, RegAdr(reg2)+2, 2);
        Unlock(erCPU);

        sz := x.typ^.size;

        IF ODD(sz) THEN
          DEC(sz);

          ShortM;
          PutDPReference(LDY+IMMEDIATE3, sz, 3);
          PutDPReference(LDA+DIRINDBYYLG, RegAdr(reg), 2);

          CASE inst OF
            lAnd: PutDPReference(And+DIRINDBYYLG, RegAdr(reg2), 2);
            |
            lOr:  PutDPReference(ORA+DIRINDBYYLG, RegAdr(reg2), 2);
            |
            lEor: PutDPReference(EOR+DIRINDBYYLG, RegAdr(reg2), 2);
          END;

          IF storeAtY THEN
            PutDPReference(STA+DIRINDBYYLG, RegAdr(reg2), 2);
          ELSE
            PutOp(PHA, 1);
          END;

          LongM;
        END;

        WHILE sz > 0 DO
          DEC(sz, 2);

          IF sz <> 0 THEN
            PutDPReference(LDY+IMMEDIATE3, sz, 3);
            PutDPReference(LDA+DIRINDBYYLG, RegAdr(reg), 2);

            CASE inst OF
              lAnd: PutDPReference(And+DIRINDBYYLG, RegAdr(reg2), 2);
              |
              lOr:  PutDPReference(ORA+DIRINDBYYLG, RegAdr(reg2), 2);
              |
              lEor: PutDPReference(EOR+DIRINDBYYLG, RegAdr(reg2), 2);
            END;

            IF storeAtY THEN
              PutDPReference(STA+DIRINDBYYLG, RegAdr(reg2), 2);
            ELSE
              PutOp(PHA, 1);
            END;
          ELSE
            PutDPReference(LDA+DIRINDLONG, RegAdr(reg), 2);

            CASE inst OF
              lAnd: PutDPReference(And+DIRINDLONG, RegAdr(reg2), 2);
              |
              lOr:  PutDPReference(ORA+DIRINDLONG, RegAdr(reg2), 2);
              |
              lEor: PutDPReference(EOR+DIRINDLONG, RegAdr(reg2), 2);
            END;

            IF storeAtY THEN
              PutDPReference(STA+DIRINDLONG, RegAdr(reg2), 2);
            ELSE
              PutOp(PHA, 1);
            END;
          END;
        END;

        Unlock(reg2);

        IF NOT storeAtY THEN
          INC(stackPointer, x.typ^.size);
        END;
      END;

      Unlock(reg);
      SetstkMd(x, x.typ);
    END;
  END;
END Log2;

PROCEDURE DivPw2(VAR x : Item; exp : CARDINAL; modulus : BOOLEAN);
VAR 
  m: LONGINT; 
  y: Item;
BEGIN
  Assert(exp <= 31);

  IF exp = 0 THEN (* DIV/MOD 1 *)
    IF modulus THEN
      SetconMd(x, 0, x.typ);
    END;
    (* else no change if x DIV 1 *)
  ELSE
    IF NOT modulus THEN (* DIV *)
      SetconMd(y, VAL(LONGINT, exp), inttyp);

      IF SignedT(x) THEN
        SHI2(rightfill, x, y);
      ELSE
        SHI2(right, x, y);
      END;
    ELSE (* MOD *)
      m := mask[exp];  (* 2**exp - 1 *)
      SetconMd(y, m, x.typ);
      Log2(lAnd, x, y);
    END;
  END;
  (* x.wid is set by SHI2 and Log2 *)
END DivPw2;

PROCEDURE DIV2(VAR x, y : Item; modulus : BOOLEAN);
  (*  x  DIV/MOD  y  --->>  x  *)
VAR
  pw2:        CARDINAL;
  szx, szy:   WidType;
  signar:     BOOLEAN;
  lv:         LONGINT;
  stackSave:  INTEGER;
  notdivby0:  CARDINAL;
BEGIN
  IF NOT errorInProc THEN
    Isz(x,szx);
    Isz(y,szy);

    signar := SignedT(x) OR SignedT(y);

    IF NOT signar AND Power2(y, pw2) THEN
      DivPw2(x, pw2, modulus);
      SetregMd(x, x.typ);
    ELSE
      IF szx < long THEN
        IF x.mode = regMd THEN
          PushVal(x);             (* place x on stack *)
          LoadVal(y, erXreg);     (* load y into x-register *)
          PutOp(PLA, 1);          (* pull y unto accumulator *)
          DEC(stackPointer, 2);   (* adjust stack pointer since we pulled *)
        ELSIF x.mode = stkMd THEN
          LoadVal(y, erXreg);
          PutOp(PLA, 1);          (* pull y unto accumulator *)
          DEC(stackPointer, 2);   (* adjust stack pointer since we pulled *)
        ELSE
          PushVal(y);             (* place y on stack *)
          LoadVal(x, erCPU);      (* load x into accumulator *)
          PutOp(PLX, 1);          (* pull y unto x-register *)
          DEC(stackPointer, 2);   (* adjust stack pointer since we pulled *)
        END;

        Unlock(erCPU);

        IF modulus THEN
          IF signar THEN
            GenGlobalOp(JSL, '~MOD2', 0, 0, 3);
            AddBytesToObjectFile(4);
          ELSE
            GenGlobalOp(JSL, '~M2UMod2', 0, 0, 3);
            AddBytesToObjectFile(4);
          END;
        ELSE
          IF signar THEN
            GenGlobalOp(JSL, '~DIV2', 0, 0, 3);
            AddBytesToObjectFile(4);
          ELSE
            GenGlobalOp(JSL, '~M2UDiv2', 0, 0, 3);
            AddBytesToObjectFile(4);
          END;
        END;

        x.wid := word; (* resulting width *)
        SetregMd(x, x.typ);

        (*
          The library routines set the V bit if a divide by zero occured, so
          check for that now.
        *)
        newLink(notdivby0);
        PutLinkReference(BVC, notdivby0, 2);
        GenTerminate(tsDivideByZero);
        FixLink(notdivby0);
      ELSE
	      IF y.mode = regMd THEN
	        CheckCPUUsage(y);
	      ELSE
	        CheckCPUUsage(x);
	      END;

        (*
          Later on, we will be pushing x and then y onto the stack.  If y
          happens to be in regMd, then at the time we push x, the value of y
          (which is in the registers) will by pushed onto the stack for safe
          keeping.  This will make y unavailable when we go to push it onto the
          stack.

          To solve this dilema, LoadParameters to place y in a D-register, and
          x in the registers.  This allows us to push x and then y without fear.
        *)
        IF y.mode = regMd THEN
          LoadParameters(x, y, TRUE);
        END;
	
	      (*
	        Once the operation is complete, the stack will not have been affected,
	        so preserve the current stack position.
	      *)
	      stackSave := stackPointer;
	
	      PushVal(x);
	      PushVal(y);
	
        IF modulus THEN
          IF signar THEN
            GenGlobalOp(JSL, '~M2Mod4', 0, 0, 3);
            AddBytesToObjectFile(4);
          ELSE
            GenGlobalOp(JSL, '~M2UMod4', 0, 0, 3);
            AddBytesToObjectFile(4);
          END;
        ELSE
          IF signar THEN
            GenGlobalOp(JSL, '~M2Div4', 0, 0, 3);
            AddBytesToObjectFile(4);
          ELSE
            GenGlobalOp(JSL, '~M2UDiv4', 0, 0, 3);
            AddBytesToObjectFile(4);
          END;
        END;

        (*
          These routines replace the first operand with the result, so adjust
          the stack pointer down 4 from the position it had before pushing
          anything.
        *)
        stackPointer := stackSave + 4;

	      (*
	        These two routines return their results on the stack.
	      *)
	      SetstkMd(x, x.typ);
        x.wid := long; (* resulting width *)

        (*
          The library routines set the V bit if a divide by zero occured, so
          check for that now.
        *)
        newLink(notdivby0);
        PutLinkReference(BVC, notdivby0, 2);
        GenTerminate(tsDivideByZero);
        FixLink(notdivby0);
      END;
  
      Release(y);
    END;
  ELSE
    SetstkMd(x, x.typ);
  END;
END DIV2;

PROCEDURE ADD2(addition: BOOLEAN; VAR x, y : Item);
  (*       x  +  y    --->>   x        *)
  (*       x  -  y    --->>   x        *)
  (* Note : x can be a memory location *)
  (*        or on top of stack.        *)
VAR
  szx:  WidType;
  szy:  WidType;
BEGIN
  IF NOT errorInProc THEN
    Isz(x,szx);
    Isz(y,szy);

    IF szx <> szy THEN
      (*
        ### M2HM.ADD2:Size of x differs from size of y.
      *)
      Mark(406);
    END;

    LoadParameters(x, y, TRUE);

    IF szx = byte THEN
      ShortM;
    END;

    IF addition THEN
      PutOp(CLC, 1);
      PutDPReference(ADC+DIRECT, y.adr, 2);
    ELSE
      PutOp(SEC, 1);
      PutDPReference(SBC+DIRECT, y.adr, 2);
    END;

    IF szx = long THEN
      PutDPReference(STA+DIRECT, y.adr, 2);

      PutOp(TXA, 1);

      IF addition THEN
        PutDPReference(ADC+DIRECT, y.adr+2, 2);
      ELSE
        PutDPReference(SBC+DIRECT, y.adr+2, 2);
      END;

      PutOp(TAX, 1);
      PutDPReference(LDA+DIRECT, y.adr, 2);
    END;

    IF szx = byte THEN
      LongM;
    END;
  END;

  Release(y);
  SetregMd(x, x.typ);
END ADD2;

PROCEDURE Cmp2(VAR x, y : Item);
  (*         x   -   y                 *)
  (* Note : x can be a memory location *)
  (*        or on top of stack.        *)
VAR
  op, eax, eay: CARDINAL;
  szx, szy:     WidType;
  wv:           INTEGER;
  lv:           LONGINT;
  skipLow:      CARDINAL;
  skip2:        CARDINAL;
  loop:         CARDINAL;
  signar:       BOOLEAN;
  reg:          DRegister;
  reg2:         DRegister;
  sz:           CARDINAL;
  idx:          CARDINAL;
  z:            Item;
BEGIN
  IF NOT errorInProc THEN
    Isz(x,szx);
    Isz(y,szy);

    signar := SignedT(x) OR SignedT(y);

    IF signar THEN (* do a signed comparison *)
      newLink(skipLow);
      newLink(skip2);

      IF y.mode = conMd THEN
        LoadD(x);

        IF szx = byte THEN
          wv := WordVal(y);
          ShortM;
          PutDPReference(LDA+DIRECT, x.adr, 2);
          PutDPReference(EOR+IMMEDIATE, wv, 2);
          PutLinkReference(BMI, skipLow, 2);
          PutDPReference(LDA+DIRECT, x.adr, 2);
          PutDPReference(CMP+IMMEDIATE, wv, 2);
          PutLinkReference(BRA, skip2, 2);
          FixLink(skipLow);
          PutDPReference(LDA+IMMEDIATE, wv, 2);
          PutDPReference(CMP+DIRECT, x.adr, 2);
          FixLink(skip2);
          LongM;
        ELSIF szx = word THEN
          wv := WordVal(y);
          PutDPReference(LDA+DIRECT, x.adr, 2);
          PutDPReference(EOR+IMMEDIATE, wv, 3);
          PutLinkReference(BMI, skipLow, 2);
          PutDPReference(LDA+DIRECT, x.adr, 2);
          PutDPReference(CMP+IMMEDIATE, wv, 3);
          PutLinkReference(BRA, skip2, 2);
          FixLink(skipLow);
          PutDPReference(LDA+IMMEDIATE, wv, 3);
          PutDPReference(CMP+DIRECT, x.adr, 2);
          FixLink(skip2);
        ELSIF szx = long THEN
          lv := LongVal(y);
          PutDPReference(LDA+DIRECT, x.adr+2, 2);
          PutDPReference(EOR+IMMEDIATE, HighWORD(lv), 3);
          PutLinkReference(BPL, skipLow, 2);
          PutDPReference(LDA+IMMEDIATE, HighWORD(lv), 3);
          PutDPReference(CMP+DIRECT, x.adr+2, 2);
          PutLinkReference(BRA, skip2, 2);
          FixLink(skipLow);
          PutDPReference(LDA+DIRECT, x.adr+2, 2);
          PutDPReference(CMP+IMMEDIATE, HighWORD(lv), 3);
          PutLinkReference(BNE, skip2, 2);
          PutDPReference(LDA+DIRECT, x.adr, 2);
          PutDPReference(CMP+IMMEDIATE, LoWORD(lv), 3);
          FixLink(skip2);
        END;
      ELSE
        IF ((x.mode = stkMd) AND
            (y.mode = stkMd) AND
            (y.adr > x.adr)) OR
           (y.mode = regMd) THEN
          LoadD(y);
          LoadD(x);
        ELSE
          LoadD(x);
          LoadD(y);
        END;

        IF szx = byte THEN
          ShortM;
          PutDPReference(LDA+DIRECT, x.adr, 2);
          PutDPReference(EOR+DIRECT, y.adr, 2);
          PutLinkReference(BMI, skipLow, 2);
          PutDPReference(LDA+DIRECT, x.adr, 2);
          PutDPReference(CMP+DIRECT, y.adr, 2);
          PutLinkReference(BRA, skip2, 2);
          FixLink(skipLow);
          PutDPReference(LDA+DIRECT, y.adr, 2);
          PutDPReference(CMP+DIRECT, x.adr, 2);
          FixLink(skip2);
          Release(y);
          LongM;
        ELSIF szx = word THEN
          PutDPReference(LDA+DIRECT, x.adr, 2);
          PutDPReference(EOR+DIRECT, y.adr, 2);
          PutLinkReference(BMI, skipLow, 2);
          PutDPReference(LDA+DIRECT, x.adr, 2);
          PutDPReference(CMP+DIRECT, y.adr, 2);
          PutLinkReference(BRA, skip2, 2);
          FixLink(skipLow);
          PutDPReference(LDA+DIRECT, y.adr, 2);
          PutDPReference(CMP+DIRECT, x.adr, 2);
          FixLink(skip2);
          Release(y);
        ELSIF szx = long THEN
          PutDPReference(LDA+DIRECT, x.adr+2, 2);
          PutDPReference(EOR+DIRECT, y.adr+2, 2);
          PutLinkReference(BPL, skipLow, 2);
          PutDPReference(LDA+DIRECT, y.adr+2, 2);
          PutDPReference(CMP+DIRECT, x.adr+2, 2);
          PutLinkReference(BRA, skip2, 2);
          FixLink(skipLow);
          PutDPReference(LDA+DIRECT, x.adr+2, 2);
          PutDPReference(CMP+DIRECT, y.adr+2, 2);
          PutLinkReference(BNE, skip2, 2);
          PutDPReference(LDA+DIRECT, x.adr, 2);
          PutDPReference(CMP+DIRECT, y.adr, 2);
          FixLink(skip2);
          Release(y);
        END;
      END;
    ELSE (* do an unsigned comparison *)
      IF y.mode = conMd THEN
        IF szx = byte THEN
          LoadVal(x, erCPU);
          wv := WordVal(y);
          ShortM;
          PutDPReference(CMP+IMMEDIATE, wv, 2);
          LongM;
        ELSIF szx = word THEN
          LoadVal(x, erCPU);
          wv := WordVal(y);
          PutDPReference(CMP+IMMEDIATE, wv, 3);
        ELSIF szx = long THEN
          LoadVal(x, erCPU);
          lv := LongVal(y);
          newLink(skipLow);
          PutDPReference(CPX+IMMEDIATE3, HighWORD(lv), 3);
          PutLinkReference(BNE, skipLow, 2);
          PutDPReference(CMP+IMMEDIATE, LoWORD(lv), 3);
          FixLink(skipLow);
        ELSE (* a byte range of 3, 5 or more bytes, damn! *)
          (*
            We only get here if we are comparing two sets that are 3, 5 or more
            bytes in length.  Such a comparison is only done to test for 
            equality, so we will loop, testing each byte of the set, until
            we reach the end of the set, or we find a difference.  That way, 
            when we finish, the P segister will reflect the result of the
            comparison.
          *)
          newLink(skip2);

          GetReg(long, reg);
          z := x;
          LoadAdr(z);
          PutDPReference(STA+DIRECT, RegAdr(reg), 2);
          PutDPReference(STX+DIRECT, RegAdr(reg)+2, 2);
          Unlock(erCPU);

          sz := z.typ^.size;
          idx := sz DIV 2;

          (*
            The following loop should compare all of the set, unless the set is
            an odd number of bytes, in which case it will compare all but the
            last byte.
          *)

          (*
            Compare the top byte if there is one.
          *)
          IF ODD(sz) THEN
            DEC(sz);

            ShortM;
            PutDPReference(LDY+IMMEDIATE3, sz, 3);
            PutDPReference(LDA+IMMEDIATE, VAL(CARDINAL, y.val.FS.set[idx]), 2);
            PutDPReference(CMP+DIRINDBYYLG, RegAdr(reg), 2);
            LongM;

            PutLinkReference(BNE, skip2, 2);
          END;

          WHILE sz > 0 DO
            DEC(idx);
            DEC(sz, 2);

            IF sz <> 0 THEN
              PutDPReference(LDA+IMMEDIATE, VAL(CARDINAL, y.val.FS.set[idx]), 3);
              PutDPReference(LDY+IMMEDIATE3, sz, 3);
              PutDPReference(CMP+DIRINDBYYLG, RegAdr(reg), 2);
              PutLinkReference(BNE, skip2, 2);
            ELSE
              PutDPReference(LDA+IMMEDIATE, VAL(CARDINAL, y.val.FS.set[idx]), 3);
              PutDPReference(CMP+DIRINDLONG, RegAdr(reg), 2);
            END;
          END;

          FixLink(skip2);
          Unlock(reg);
          Unlock(erCPU);
        END;
      ELSE
        IF szx = byte THEN
          LoadParameters(x, y, TRUE);
          ShortM;
          PutDPReference(CMP+DIRECT, y.adr, 2);
          LongM;
        ELSIF szx = word THEN
          LoadParameters(x, y, TRUE);
          PutDPReference(CMP+DIRECT, y.adr, 2);
        ELSIF szx = long THEN
          LoadParameters(x, y, TRUE);
          newLink(skipLow);
          PutDPReference(CPX+DIRECT, y.adr+2, 2);
          PutLinkReference(BNE, skipLow, 2);
          PutDPReference(CMP+DIRECT, y.adr, 2);
          FixLink(skipLow);
        ELSE (* a byte range of 3, 5 or more bytes, damn! *)
          (*
            We only get here if we are comparing two sets that are 3, 5 or more
            bytes in length.  Such a comparison is only done to test for 
            equality, so we will loop, testing each byte of the set, until
            we reach the end of the set, or we find a difference.  That way, 
            when we finish, the P segister will reflect the result of the
            comparison.
          *)
          GetReg(long, reg);
          z := x;
          LoadAdr(z);
          PutDPReference(STA+DIRECT, RegAdr(reg), 2);
          PutDPReference(STX+DIRECT, RegAdr(reg)+2, 2);
          Unlock(erCPU);

          GetReg(long, reg2);
          LoadAdr(y);
          PutDPReference(STA+DIRECT, RegAdr(reg2), 2);
          PutDPReference(STX+DIRECT, RegAdr(reg2)+2, 2);
          Unlock(erCPU);

          PutDPReference(LDY+IMMEDIATE3,
                         VAL(CARDINAL, x.typ^.size - VAL(aSize, 1)), 3);

          ShortM;

          newLink(skip2);
          newLink(loop);

          FixLink(loop);
          PutDPReference(LDA+DIRINDBYYLG, RegAdr(reg), 2);
          PutDPReference(CMP+DIRINDBYYLG, RegAdr(reg2), 2);
          PutLinkReference(BNE, skip2, 2);
          PutOp(DEY, 1);
          PutLinkReference(BPL, loop, 2);
          PutDPReference(LDA+IMMEDIATE, 0, 2); (* force p register to say EQUAL! *)
          FixLink(skip2);

          LongM;
          Unlock(reg);
          Unlock(reg2);
        END;

        Release(y);
      END;
    END;
  END;
END Cmp2;

PROCEDURE In2(VAR x, y : Item);
(* perform bit-manipulations : BTST.  *)
(* y is the destination bit pattern,  *)
(* x is the bit number.               *)
VAR
  op:           CARDINAL;
  v:            INTEGER;
  sz:           WidType;
  bitMask:      CARDINAL;
  z, w:         Item;
  reg:          DRegister;
  bitCnt:       DRegister;
  yadr:         DRegister;
  ywasconst:    BOOLEAN;

BEGIN
  IF NOT errorInProc THEN
    (* width of set defines allowed bit-numbers *)
    Isz(y, sz);

    IF SimpleC(x) & NOT SimpleC(y) THEN
      (* static bit : *)
      v := WordVal(x);

      IF sz < long THEN
        LoadD(y); (* load bit pattern into direct page *)

        bitMask := SHIFT(1, v);
        PutDPReference(LDA+IMMEDIATE, bitMask, 3);  (* load the mask into acc *)

        IF sz = byte THEN
          ShortM;
        END;

        PutDPReference(And+DIRECT, y.adr, 2);       (* AND mask with value *)

        IF sz = byte THEN
          LongM;
        END;
      ELSE (* set is 3 or more bytes in length *)
        LoadAdr(y);
        GetReg(long, reg);
        PutDPReference(STA+DIRECT, RegAdr(reg), 2);
        PutDPReference(STX+DIRECT, RegAdr(reg)+2, 2);
        Unlock(erCPU);

        bitMask := SHIFT(1, v MOD 8);
        PutDPReference(LDY+IMMEDIATE3, v DIV 8, 3);
        PutDPReference(LDA+IMMEDIATE, bitMask, 3);  (* load the mask into acc *)
        ShortM;
        PutDPReference(And+DIRINDBYYLG, RegAdr(reg), 2);       (* AND mask with value *)
        LongM;
 
        Unlock(reg);
      END;
    ELSE
      (* dynamic bit : *)
      SetconMd(z, VAL(LONGINT, 1), cardtyp);

      IF sz > word THEN
        LoadVal(x, erCPU);
        GetReg(word, reg);
        GetReg(word, bitCnt);

        PutOp(TAX, 1);
        PutDPReference(And+IMMEDIATE, 00007H, 3);
        PutDPReference(STA+DIRECT, RegAdr(bitCnt), 2);

        PutOp(TXA, 1);
        PutOp(LSR+IMMEDIATE, 1);
        PutOp(LSR+IMMEDIATE, 1);
        PutOp(LSR+IMMEDIATE, 1);
        PutDPReference(STA+DIRECT, RegAdr(reg), 2);

        Unlock(erCPU);
        SetDregMd(x, bitCnt, x.typ);

        ywasconst := y.mode = conMd;

        IF ywasconst THEN
          SetstkMd(w, y.typ);
          Move(y, w);
          SetstkMd(y, y.typ);
        END;
      END;

      SHI2(left, z, x);  (* note that SHI2 will cause bitCnt to be unlocked *)

      IF sz > word THEN
        GetReg(long, yadr);
        LoadAdr(y);
        PutDPReference(STA+DIRECT, RegAdr(yadr), 2);
        PutDPReference(STX+DIRECT, RegAdr(yadr)+2, 2);
        Unlock(erCPU);

        PutDPReference(LDY+DIRECT, RegAdr(reg), 2);
        LoadVal(z, erCPU);
        ShortM;
        PutDPReference(And+DIRINDBYYLG, RegAdr(yadr), 2);
        LongM;
        Unlock(erCPU);
        Unlock(yadr);
        Unlock(reg);

        IF ywasconst THEN
          (*
            Note that this code assumes that "StackTop" does not affect the X
            register.
          *)
          PutOp(PHP, 1);
          PutOp(PHP, 1);
          PutOp(PLX, 1);
          StackTop(y.typ^.size);
          PutOp(PHX, 1);
          PutOp(PLP, 1);
          PutOp(PLP, 1);
        END;
      ELSE
        LoadParameters(z, y, TRUE);
        PutDPReference(And+DIRECT, y.adr, 2);  (* AND mask with value *)
      END;

      Release(z);
    END;

    Release(y);
    (* result is in the condition code register! *)
  END;

  SetregMd(x, booltyp);
END In2;

PROCEDURE Neg1(VAR x : Item);
VAR
  szx: WidType;
BEGIN
  Isz(x, szx);

  LoadD(x);

  IF NOT errorInProc THEN
    PutOp(SEC, 1);
    PutDPReference(LDA+IMMEDIATE, 0, 3);
    PutDPReference(SBC+DIRECT, x.adr, 2);

    IF szx = long THEN
      PutOp(TAY, 1);
      PutDPReference(LDA+IMMEDIATE, 0, 3);
      PutDPReference(SBC+DIRECT, x.adr+2, 2);
      PutOp(TAX, 1);
      PutOp(TYA, 1);
    END;

    OvflTrap(SignedT(x));
  END;

  Release(x);
  SetregMd(x, x.typ);
END Neg1;

PROCEDURE Abs1(VAR x : Item);
VAR
  noNegate: CARDINAL;
  done:     CARDINAL;
  y:        Item;
BEGIN
  newLink(noNegate);
  newLink(done);

  (*
    This is more fiddly than I would like.  After Cmp2, we don't know whether
    'x' is in the CPU registers, or in a DRegister.  When we call Neg1, we
    end up with it in the CPU.  But the Neg1 code isn't always executed, so
    when the result of the Cmp2 indicates that 'x' isn't negetive, we must
    do a LoadVal to ensure that by the end of this procedure the state of 'x'
    is the same, no matter what.

    The upshot is:

      THE ABSOLUTE VALUE OF 'x' __MUST__ BE IN THE CPU REGISTERS AT THE END OF
      THIS ACTION.
  *)

  SetconMd(y, VAL(LONGINT, 0), x.typ);

  Cmp2(x, y);
  PutLinkReference(BCS, noNegate, 2);
  (*
    For the negate, we use a copy of 'x', since the LoadVal needs to know
    where Cmp2 left it when it executes.
  *)
  y := x;
  Neg1(x);
  (*
    'x' is now in regMd.  That will upset LoadVal, so unlock 'x'.  LoadVal
    will put 'x' in regMd, so the end result will be correct.
  *)
  Release(x);
  x := y;
  PutLinkReference(BRA, done, 2);
  FixLink(noNegate);
  LoadVal(x, erCPU);
  FixLink(done);
END Abs1;

PROCEDURE Cap1(VAR x : Item);
VAR
  upper: CARDINAL;
BEGIN
  LoadVal(x, erCPU);

  newLink(upper);
  PutDPReference(CMP+IMMEDIATE, 61H, 3);
  PutLinkReference(BMI, upper, 2);
  PutDPReference(CMP+IMMEDIATE, 7BH, 3);
  PutLinkReference(BCS, upper, 2);
  PutDPReference(And+IMMEDIATE, 5FH, 3);
  FixLink(upper);
END Cap1;

PROCEDURE Tst1(VAR x : Item);
BEGIN
  LoadVal(x, erCPU);
END Tst1;

PROCEDURE Com1(VAR x : Item);
VAR
  szx:      WidType;
  sz:       CARDINAL;
  reg:      DRegister;
  loop:     CARDINAL;
  storeAtX: BOOLEAN;
BEGIN
  Isz(x, szx);

  IF szx < quad THEN
    LoadVal(x, erCPU);

    PutDPReference(EOR+IMMEDIATE, 0FFFFH, 3);

    IF szx = long THEN
      PutOp(TAY, 1);
      PutOp(TXA, 1);
      PutDPReference(EOR+IMMEDIATE, 0FFFFH, 3);
      PutOp(TAX, 1);
      PutOp(TYA, 1);
    END;
  ELSE (* get complement of structure of 3, 5 or more bytes *)
    sz := x.typ^.size;

    (*
      If x is already an intermediary value on the stack, then don't create a
      copy of it, overwrite it with the result.
    *)
    storeAtX := x.mode = stkMd;

    LoadAdr(x);
    GetReg(long, reg);
    PutDPReference(STA+DIRECT, RegAdr(reg), 2);
    PutDPReference(STX+DIRECT, RegAdr(reg)+2, 2);
    Unlock(erCPU);

    IF ODD(sz) THEN
      DEC(sz);

      ShortM;
      PutDPReference(LDY+IMMEDIATE3, sz, 3);
      PutDPReference(LDA+IMMEDIATE, 0FFH, 2);
      PutDPReference(EOR+DIRINDBYYLG, RegAdr(reg), 2);

      IF storeAtX THEN
        PutDPReference(STA+DIRINDBYYLG, RegAdr(reg), 2);
      ELSE
        PutOp(PHA, 1);
      END;

      LongM;
    END;

    WHILE sz > 0 DO
      DEC(sz, 2);

      IF sz <> 0 THEN
        PutDPReference(LDY+IMMEDIATE3, sz, 3);
        PutDPReference(LDA+IMMEDIATE, 0FFFFH, 3);
        PutDPReference(EOR+DIRINDBYYLG, RegAdr(reg), 2);

        IF storeAtX THEN
          PutDPReference(STA+DIRINDBYYLG, RegAdr(reg), 2);
        ELSE
          PutOp(PHA, 1);
        END;
      ELSE
        PutDPReference(LDA+IMMEDIATE, 0FFFFH, 3);
        PutDPReference(EOR+DIRINDLONG, RegAdr(reg), 2);

        IF storeAtX THEN
          PutDPReference(STA+DIRINDLONG, RegAdr(reg), 2);
        ELSE
          PutOp(PHA, 1);
        END;
      END;
    END;

    Unlock(reg);

    IF NOT storeAtX THEN
      INC(stackPointer, x.typ^.size);
    END;

    SetstkMd(x, x.typ);
  END;
END Com1;

PROCEDURE Inc1(VAR x : Item);
VAR
  nonzero:  CARDINAL;
  szx:      WidType;

  PROCEDURE OvflChk(xreg: BOOLEAN);
  VAR
    noover: CARDINAL;
  BEGIN
    IF ovflchk THEN
      newLink(noover);

      IF SignedT(x) THEN
        IF xreg THEN
          PutDPReference(CPX+IMMEDIATE3, 8000H, 3);
        ELSE
          PutDPReference(CMP+IMMEDIATE, 8000H, 3);
        END;
      END;

      PutLinkReference(BNE, noover, 2);
      GenTerminate(tsOverflow);
      FixLink(noover);
    END;
  END OvflChk;

BEGIN
  Isz(x, szx);

  LoadVal(x, erCPU);

  IF NOT errorInProc THEN
    PutOp(INCACC, 1);

    IF szx = long THEN
      newLink(nonzero);
      PutLinkReference(BNE, nonzero, 2);
      PutOp(INX, 1);
      OvflChk(TRUE);
      FixLink(nonzero);
    ELSE
      OvflChk(FALSE);
    END;
  END;
END Inc1;

PROCEDURE Dec1(VAR x : Item);
VAR
  notneg: CARDINAL;
  szx:    WidType;

  PROCEDURE OvflChk(xreg: BOOLEAN);
  VAR
    noover: CARDINAL;
  BEGIN
    IF ovflchk THEN
      newLink(noover);

      IF SignedT(x) THEN
        IF xreg THEN
          PutDPReference(CPX+IMMEDIATE3, 7FFFH, 3);
        ELSE
          PutDPReference(CMP+IMMEDIATE, 7FFFH, 3);
        END;
      ELSE
        IF xreg THEN
          PutDPReference(CPX+IMMEDIATE3, 0FFFFH, 3);
        ELSE
          PutDPReference(CMP+IMMEDIATE, 0FFFFH, 3);
        END;
      END;

      PutLinkReference(BNE, noover, 2);
      GenTerminate(tsOverflow);
      FixLink(noover);
    END;
  END OvflChk;

BEGIN
  Isz(x, szx);

  LoadVal(x, erCPU);

  IF NOT errorInProc THEN
    PutOp(DECACC, 1);

    IF szx = long THEN
      newLink(notneg);
      PutDPReference(CMP+IMMEDIATE, 0FFFFH, 3);
      PutLinkReference(BNE, notneg, 2);
      PutOp(DEX, 1);
      OvflChk(TRUE);
      FixLink(notneg);
    ELSE
      OvflChk(FALSE);
    END;
  END;
END Dec1;

PROCEDURE Add2(VAR x, y : Item);
CONST
  Add = TRUE;
VAR
  op: BOOLEAN;
  lv : LONGINT;
BEGIN
  IF NOT errorInProc THEN
    op := Add;

    IF y.mode = conMd THEN
      lv := LongVal(y);

      IF lv < VAL(LONGINT, 0) THEN
        SetconMd(y, -lv, y.typ);
        op := NOT Add;
      END;
    END;

    ADD2(op, x, y);

    OvflTrap(SignedT(x));
  END;
END Add2;

PROCEDURE Sub2(VAR x, y : Item);
CONST
  Add = TRUE;
VAR
  op: BOOLEAN;
  lv : LONGINT;
BEGIN
  op := NOT Add;

  IF NOT errorInProc THEN
    IF y.mode = conMd THEN
      lv := LongVal(y);

      IF lv < VAL(LONGINT, 0) THEN
        SetconMd(y, -lv, y.typ);
        op := Add;
      END;
    END;

    ADD2(op, x, y);

    OvflTrap(SignedT(x));
  END;
END Sub2;

PROCEDURE And2(VAR x, y : Item);
BEGIN
  Log2(lAnd, x, y);
END And2;

PROCEDURE Or2(VAR x, y : Item);
BEGIN
  Log2(lOr, x, y);
END Or2;

PROCEDURE Eor2(VAR x, y : Item);
BEGIN
  Log2(lEor, x, y);
END Eor2;

PROCEDURE Div2(VAR x, y : Item);
BEGIN
  IF (y.mode = conMd) & (LongVal(y) = VAL(LONGINT, 0)) THEN
    Mark(205);
  ELSE
    DIV2(x, y, FALSE);
  END;
END Div2;

PROCEDURE Mod2(VAR x, y : Item);
BEGIN
  IF (y.mode = conMd) & (LongVal(y) = VAL(LONGINT, 0)) THEN
    Mark(205);
  ELSE
    DIV2(x, y, TRUE);
  END;
END Mod2;

PROCEDURE Mul2(VAR x, y : Item);
BEGIN
  IF ((y.mode = conMd) & (LongVal(y) = VAL(LONGINT, 0))) THEN
    SetconMd(x, 0, x.typ);
  ELSIF NOT((y.mode = conMd) & (LongVal(y) = VAL(LONGINT, 1))) THEN
    MUL2(x, y, TRUE);
  END;
END Mul2;

PROCEDURE Shi2(VAR x, y : Item; shiftop : direction);
BEGIN
  SHI2(shiftop, x, y);
END Shi2;

PROCEDURE Ash2(VAR x, y : Item; shiftop : ShiType);
(*                                         *)
(*     Arithmetic Shift                    *)
(*     Logical Shift        x by y.        *)
(*     Rotate Shift                        *)
(*                                         *)
(*  y is the shift count of type INTEGER   *)
(*  or CARDINAL.                           *)
(*  if y >= 0 then shift LEFT.             *)
(*  if y <  0 then shift RIGHT.            *)
(*                                         *)
VAR
  op:       direction;
  positive: CARDINAL;
  done:     CARDINAL;
  sz:       WidType;
  it:       INTEGER;
  z, w:     Item;
BEGIN
  Assert( x.mode = DregMd );
  Assert((shiftop = Asl) OR (shiftop = Lsl));
  Isz(x,sz);

  IF NOT errorInProc THEN
    IF y.mode = conMd THEN
      it := WordVal(y);

      IF it >= 0 THEN
        CASE shiftop OF
          Asl: op := left;
          |
          Lsl: op := logLeft;
        END;
      ELSE
        CASE shiftop OF
          Asl: op := rightfill;
          |
          Lsl: op := right;
        END;

        SetconMd(y, VAL(LONGINT, ABS(it)), y.typ);
      END;

      SHI2(op, x, y);
    ELSE
      (* variable shift count of type INTEGER/CARDINAL : *)
      (* INTEGER/CARDINAL count treated the same way.    *)
      LoadX(y, word);      (* load shift count  *)
      z := y;
      w := x;

      newLink(positive);
      newLink(done);

      PutDPReference(CMP+IMMEDIATE, 0, 3);
      PutLinkReference(BPL, positive, 2);

      (* Right shift code *)
      IF shiftop = Asl THEN
        op := rightfill;
      ELSE
        op := right;
      END;

      Neg1(y);  (* We want a positive count of shifts *)
      SHI2(op, x, y);
      Unlock(erCPU);
      PutLinkReference(BRA, done, 2);
      FixLink(positive);

      (* left shift code *)
      IF shiftop = Asl THEN
        op := left;
      ELSE
        op := logLeft;
      END;

      (*
        Ensure that y is back in the registers, and that it is the lock item.
      *)
      SetregMd(y, y.typ);
      x := w;

      IF x.mode = DregMd THEN
        IF NOT Locked(x.register) THEN
          Lock(x.register);
        END;
      END;

      SHI2(op, x, y);
      FixLink(done);
    END;
  END;

  x.wid := sz; (* resulting width of D-Register *)
  Release(y);
END Ash2;

PROCEDURE ConIndex(VAR x : Item; inc : LONGINT);
(* called for constant index and field-offset. *)
(*   if NOT indir :  adr-field is incremented  *)
(*   if indir     :  off-field is incremented. *)
VAR
  i: INTEGER;
BEGIN
  WITH x DO
    IF mode < conMd THEN
      (* reference to indir, adr, off allowed. *)
      IF NOT indir THEN
        i := adr;
      ELSE
        i := off;
      END;

      IF (i >= 0) & (inc <= VAL(LONGINT, MaxCard) - VAL(LONGINT, i)) OR
         (i <  0) & (inc >= VAL(LONGINT, MIN(INTEGER) - i)) THEN
        i := i + SHORT(inc);

        IF NOT indir THEN
          adr := i;
        ELSE
          off := i;
        END;
      ELSE (* offset overflow *)
        (*
          we only get here if the item address has already been "offset" by
          using 'adr' or 'off' as indexes from the base address.  if the sum
          of the array index plus the address offset is greater than 65535
          (the max for an index), then we must place the absolute address on
          the stack, and then use that as a base address, with the index being
          the total index from that base.
        *)
        PushAdr(x);
        indexed := TRUE;
        indir := TRUE;
        adr := inc;
      END;
    ELSE (* all other modes *)
      Mark(235);
    END;
  END (*WITH*);
END ConIndex;

PROCEDURE Normalize(VAR x : Item; i : INTEGER);
(*
  normalize x with the low-bound i

  an array is truly indexed, starting at byte offset 0 in the memory block.
  this routine is used to convert an index as used by the source code to a
  "true" index for the memory block. eg.

    in the array:   x: ARRAY [-5..5] OF CHAR;

    the [-1] element should be at offset 4 into the memory block, and element
    2 should be at offset 7 in the memory block.
*)
CONST
  Add = TRUE;
VAR
  op: BOOLEAN;
  y:  Item;
BEGIN
  IF i <> 0 THEN
    (* Note : overflow-checks must be OFF for compiler! *)
    IF i > 0 THEN
      op := NOT Add;
    ELSE
      op := Add;
      i := ABS(i);
    END;

    SetconMd(y, VAL(LONGINT, i), x.typ);
    ADD2(op, x, y);
    Release(y);
  END;
END Normalize;

PROCEDURE CheckHigh(VAR x, high : Item);
(*
  OPERATION:
    Compare the value of 'x' with 'high'.  'x' must be less than or equal to
    'high' to be valid.  'x' is actually an index into an open array.  'high'
    is the size of the open array.
*)
VAR
  ea:       CARDINAL;
  sz, hsz:  WidType;
  xok:      CARDINAL;
BEGIN
  IF NOT rngchk THEN
    RETURN;
  END;

  Cmp2(x, high);  (* Use the general purpose routine to compare the two values *)

  newLink(xok); (* label to jump to if x <= high *)
  PutLinkReference(BCC, xok, 2);
  PutLinkReference(BEQ, xok, 2);
  GenTerminate(tsSubrangeExceeded);
  FixLink(xok);
END CheckHigh;

PROCEDURE CheckClimit(VAR x : Item; limit : LONGINT);              (* V2.6x *)
(* check item associated with x to be in the   *)
(* range indicated by [ 0 .. limit ].          *)
(* Note : Trap taken always if limit < 0.      *)                (* V2.6x *)
(*        CHK treats operand and upper-bound   *)
(*        as signed 2's complement integers!   *)
VAR
  sz:           WidType;
  y:            Item;
  outOfLimits:  CARDINAL;
  withinLimits: CARDINAL;
  maybeOK:      CARDINAL;
BEGIN
  IF NOT rngchk THEN
    RETURN;
  END;

  IF (limit < VAL(LONGINT, 0)) THEN
    Mark(286);
  END; (* invalid limit *)            (* V2.6x *)

  Isz(x,sz);

  IF sz = word THEN
    Assert(limit <= VAL(LONGINT, 65535));           (* V2.6x *)
  ELSIF sz = byte THEN
    Assert(limit <= VAL(LONGINT, 255));             (* V2.6x *)
  END;

  IF NOT errorInProc THEN
    newLink(withinLimits);
    newLink(outOfLimits);
    newLink(maybeOK);

    SetconMd(y, VAL(LONGINT, 0), x.typ);
    Cmp2(x, y);
    PutLinkReference(BEQ, maybeOK, 2);
    PutLinkReference(BCS, maybeOK, 2);
    PutLinkReference(BRA, outOfLimits, 2);
    FixLink(maybeOK);
    SetconMd(y, limit, x.typ);
    Cmp2(x, y);
    PutLinkReference(BCC, withinLimits, 2);
    PutLinkReference(BEQ, withinLimits, 2);
    FixLink(outOfLimits);
    GenTerminate(tsRangeError);
    FixLink(withinLimits);
  END;
END CheckClimit;

PROCEDURE CheckRange(VAR x: Item; min, max, BndAdr: LONGINT);
  (* check x in the constant range [ min .. max ] *)
VAR
  htyp: StrPtr;
  sz: WidType;
BEGIN
  IF NOT rngchk THEN
    RETURN;
  END;

  IF SimpleT(x) THEN
    Isz(x,sz);
    htyp := x.typ; (* hold original type of x *)
    LoadX(x,word);

    IF sz <= word THEN
      x.typ := inttyp;
    END;

    Normalize(x, min);

    IF (max >= min) & ((min >= 0) OR (max <= (VAL(LONGINT, MAX(CARDINAL)) + min))) THEN
      max := max - min;
    ELSE
      Mark(286);
      max := 0; (* range distance too big *)
    END;

    CheckClimit(x, max);

    (* Note : overflow-checks must be OFF for compiler! *)
    (* recover original value of x : *)
    Normalize(x, - min);

    x.typ := htyp; (* recover type of x *)
  END;
END CheckRange;

PROCEDURE CheckDbltoSingle(VAR x, y : Item);
  (* range check for assignment of double-word type x *)
  (* to single-word type y (INTEGER/CARDINAL).        *)
VAR
  inRange:    CARDINAL;
  outOfRange: CARDINAL;
  signedx:    BOOLEAN;
  signedy:    BOOLEAN;
BEGIN
  IF NOT rngchk THEN
    RETURN;
  END;

  LoadVal(x, erCPU);                             (* load long x *)

  IF NOT errorInProc THEN
    newLink(inRange);
    newLink(outOfRange);

    signedx := SignedT(x);
    signedy := SignedT(y);
  
    PutOp(TAY, 1);

    IF NOT signedx AND signedy THEN
      (*
        LONGCARD --> INTEGER
      *)
      PutDPReference(And+IMMEDIATE, 8000H, 3);
      PutLinkReference(BNE, outOfRange, 2);
      PutOp(TXA, 1);
      PutLinkReference(BEQ, inRange, 2);
    ELSIF NOT signedx THEN
      (*
        LONGCARD --> CARDINAL
      *)
      PutOp(TXA, 1);
      PutLinkReference(BEQ, inRange, 2);
    ELSIF NOT signedy THEN
      (*
        LONGINT --> CARDINAL
      *)
      PutOp(TXA, 1);
      PutLinkReference(BEQ, inRange, 2);
    ELSE
      (*
        LONGINT --> INTEGER
      *)
      PutOp(TXA, 1);
      PutDPReference(CPX+IMMEDIATE3, 0000H, 3);
      PutLinkReference(BEQ, inRange, 2);
      PutDPReference(CPX+IMMEDIATE3, 0FFFFH, 3);
      PutLinkReference(BEQ, inRange, 2);
    END;

    FixLink(outOfRange);
    GenTerminate(tsRangeError);
    FixLink(inRange);
    PutOp(TYA, 1);
  END;
END CheckDbltoSingle;

PROCEDURE VarIndex(VAR x, y : Item; elsize : CARDINAL);
(*
  OPERATION:
    Generate the offset into array 'x' given index 'y' and element size
    'elsize'.  This offset is then pushed onto the stack, and 'x' is
    altered to indicate this.
*)
VAR
  elsz:     Item;
  scale:    CARDINAL;
  saveFlag: BOOLEAN;
BEGIN
  IF NOT errorInProc THEN
    (* Note : the width of an index must be word! *)
    (* ----   VarIndex works also fine for long inidices. *)

    SetconMd(elsz, VAL(LONGINT, elsize), y.typ);

    IF elsize > 1 THEN
      MUL2(y, elsz, FALSE); (* inhibit overflow-checks *)

      Assert(y.typ^.size = VAL(aSize, 2));
    END;

    WITH x DO
      LoadVal(y, erCPU);

      (*
        if this item has already been indexed, then we must add the previous
        index to the one we just calculated.

        Note that in doing so, we don't want to lose the index register we 
        have allocated, so save and set the 'dontLoseIndex' flag.
      *)
      IF indexed THEN
        saveFlag := dontLoseIndex;
        dontLoseIndex := TRUE;
        LoadIndexTo(AddToA, indexReg);
        dontLoseIndex := saveFlag;
      ELSE
        GetReg(word, x.indexReg);
      END;

      PutDPReference(STA+DIRECT, RegAdr(x.indexReg), 2);
      Unlock(erCPU);

      indexed := TRUE;
    END;
  END;
END VarIndex;

PROCEDURE GetHigh(VAR x : Item);
  (* get high-index of dynamic array parameter : *)
  (*                                             *)
  (* Caution :  x.typ IS changed !               *)
  (* -------                                     *)
BEGIN
  WITH x DO
    IF mode < conMd THEN
      (* reference to indir, adr, off allowed. *)
      indir := FALSE;
      off := 0;
      adr := adr + 4;
      typ := hightyp;
    ELSE
      Mark(240);
    END;
  END (*WITH*);
END GetHigh;

PROCEDURE DynArray(VAR x, y : Item);
  (* generate descriptor for dynamic array parameters : *)
  (*                                                    *)
  (* Caution :    guarantee HIGH to be in the range     *)
  (* -------      0   <=   HIGH   <=   MaxCard          *)
  (*                                                    *)
CONST
  ByteSize = 1;
VAR
  high, onstack, e: Item;
  s:                StrPtr;
  elsize:           CARDINAL;
  i:                INTEGER;
  dynbyte:          BOOLEAN;
  reg:              DRegister;
BEGIN
  Assert((x.mode = stkMd) & (x.typ^.form = Array) & x.typ^.dyn);

  IF NOT errorInProc THEN
    dynbyte := (x.typ^.ElemTyp = bytetyp);

    IF (y.typ^.form = Array) THEN
      elsize := y.typ^.ElemTyp^.size;

      IF y.typ^.dyn THEN (* copy existing descriptor *)
        (*
          This code is used when a procedure with an open array passes that
          open array down to another procedure.  We can't generate another
          open array descriptor as normally done.  We must copy the descriptor
          that was passed to us.

          This next bit gets the "high" or length of the open array we already
          have.
        *)
        high := y;
        GetHigh(high);

        (*
          Now, if the open array we have is an array of some 2 or more byte
          element, and we are wanting to generate a descriptor for a byte
          element open array, then we must generate code to compute the
          new high of the array.

          This occurs when we are passing some open array of cardinal say
          to another procedure as an open array of byte.
        *)
        IF dynbyte & (elsize <> ByteSize) THEN
          Inc1(high);
          SetconMd(e, VAL(LONGINT, elsize), high.typ);
          MUL2(high,e,TRUE);
          Dec1(high);

          IF ovflchk THEN
            CheckClimit(high, MaxCard);
          END;
        END;
      ELSE (* generate new descriptor *)
        IF NOT dynbyte THEN
          s := y.typ^.IndexTyp;
          i := 0;

          WITH s^ DO
            IF form = Range THEN
              IF (max >= min) &
                 ((min >= VAL(aSize, 0)) OR
                  (max <= (VAL(aSize, MAX(CARDINAL)) + min))) THEN
                i := max - min;
              ELSE
                Mark(286); (* range distance too big *)
              END;
            END; (*Range*);
          END; (*WITH*);
        ELSE
          WITH y.typ^ DO  (*wh 17.12.86*)
            IF (form = Array) & (IndexTyp^.form = Range) & (elsize = 1) THEN
              i := IndexTyp^.max - IndexTyp^.min;
            ELSE
              i := size;

              IF i > 0 THEN
                DEC(i);
              END;
            END;
          END;
        END;

        SetconMd(high, VAL(LONGINT, i), hightyp);
      END;
    ELSIF (y.typ^.form = String) THEN
      IF y.val.strChar THEN
        i := 2;
      ELSE
        i := y.val.D1;
      END;

      IF i > 0 THEN
        DEC(i);
      END;

      SetconMd(high, VAL(LONGINT, i), hightyp);
    ELSE
      Assert(dynbyte);
      i := y.typ^.size;

      IF i > 0 THEN
        DEC(i);
      END;

      SetconMd(high, VAL(LONGINT, i), hightyp);

      IF y.mode >= conMd THEN
        Mark(231);
      END;
    END;

    SetstkMd(onstack, hightyp);

    IF (y.mode <> conMd) AND y.indexed THEN
      (*
        'y' is indexed.  therefor we must get it's address using the index
        that is on the stack before pushing anything else onto the stack.
        Indexing assumes that the index is on the top of stack.
      *)
      LoadAdr(y);
      GetReg(long, reg);
      PutDPReference(STA+DIRECT, RegAdr(reg), 2);
      PutDPReference(STX+DIRECT, RegAdr(reg) + 2, 2);
      Unlock(erCPU);

      Move(high,onstack);
      PutDPReference(LDA+DIRECT, RegAdr(reg), 2);
      PutDPReference(LDX+DIRECT, RegAdr(reg) + 2, 2);
      PutOp(PHX, 1);
      PutOp(PHA, 1);
      INC(stackPointer, 4);
      Unlock(reg);
    ELSE
      Move(high,onstack);
      MoveAdr(y,onstack);
    END;
  END;
END DynArray;

PROCEDURE CopyDynArray(descrAdr: INTEGER; eleSize: CARDINAL);
VAR
  x, y, e:    Item;
  size, new:  DRegister;
  saveStack:  INTEGER;
BEGIN
  Assert(eleSize > 0);

  IF NOT errorInProc THEN
    GetReg(word, size);

    SetlocMd(y, descrAdr+4, hightyp);

    LoadD(y);
    Inc1(y);

    IF eleSize > 1 THEN             (* if necessary, multiply the size by *)
      SetconMd(e, VAL(LONGINT, eleSize), y.typ);  (* the array element size to produce *)
      MUL2(y, e, TRUE);             (* the number of bytes to copy *)
    END;

    LoadVal(y, erCPU);
    PutDPReference(STA+DIRECT, RegAdr(size), 2);
    Unlock(erCPU); (* we must free up the cpu registers *)

    PutOp(TSC, 1);
    PutOp(SEC, 1);
    PutDPReference(SBC+DIRECT, RegAdr(size), 2);
    PutOp(TCS, 1);

    GenStackCheck(TRUE);

    GetReg(word, new);
    PutOp(INCACC, 1);
    PutDPReference(PEA, 0, 3);  (* push the address of the stack array space *)
    PutDPReference(STA+DIRECT, RegAdr(new), 2);
    PutOp(PHA, 1);

    saveStack := stackPointer;

    SetlocMd(x, descrAdr, addrtyp);     (* push the original address *)
    PushVal(x);

    PutDPReference(LDA+DIRECT, RegAdr(size), 2); (* push the number of bytes to copy *)
    PutOp(PHA, 1);

    GenGlobalOp(JSL, '~MOVE', 0, 0, 3);
    AddBytesToObjectFile(4);

    stackPointer := saveStack;

    PutDPReference(LDA+DIRECT, RegAdr(new), 2); (* now store the address of the stack array *)
                                                (* in the dynamic array descriptor *)
    PutDPReference(LDX+IMMEDIATE3, 0, 3);
    SetlocMd(x, descrAdr, addrtyp);
    StoreVal(x, erCPU);

    Unlock(new);
    Unlock(size);
    Release(x);
    Release(y);
  END;
END CopyDynArray;

PROCEDURE EnterCase(VAR x : Item; base: CARDINAL; lo, hi : INTEGER);
(*  enter case-statement processor *)
VAR
  n:        Item;
  inRange:  CARDINAL;
  badCase:  CARDINAL;
  doCase:   CARDINAL;
BEGIN
  (*
    When we pushed the value onto the stack, we had to adjust
    "stackPointer" to hide that fact.  The reason for this is that the code
    generated between there, and here (the actual case label code) mustn't
    think that the value is on the stack.

    Now that we are past the case label code, we must re-adjust "stackPointer"
    to indicate to any about-to-be-generated-code that the value is still
    there on the stack.
  *)
  INC(stackPointer, x.typ^.size);

  LoadX(x, word);

  newLink(badCase);
  newLink(inRange);

  IF lo <> 0 THEN
    SetconMd(n, VAL(LONGINT, lo), x.typ);
    Cmp2(x, n);
    PutLinkReference(BCC, badCase, 2);
  END;

  SetconMd(n, VAL(LONGINT, hi), x.typ);
  Cmp2(x, n);
  PutLinkReference(BEQ, inRange, 2);
  PutLinkReference(BCC, inRange, 2);
  Unlock(erCPU);

  FixLink(badCase);
  SetconMd(n, VAL(LONGINT, hi-lo+1), cardtyp);
  LoadX(n, word);
  Release(n);
  newLink(doCase);
  PutLinkReference(BRA, doCase, 2);

  FixLink(inRange);
  IF lo <> 0 THEN
    SetconMd(n, VAL(LONGINT, lo), x.typ);
    IF x.mode = regMd THEN
      SetregMd(x, x.typ);
    END;
    Sub2(x, n);
  END;

  FixLink(doCase);
  PutOp(ASL+IMMEDIATE, 1);
  PutOp(TAX, 1);

  PutLinkReference(LDA+ABSLONGBYX, base, 4);

  PutOp(PHA, 1);
  PutOp(RTS, 1);
  Release(x);
  Release(n);
END EnterCase;

PROCEDURE ExitCase;
  (*  leave case-statement *)
BEGIN
END ExitCase;

(*$Segment M2HMPart2*)

PROCEDURE Link(l: aAdr; lev : CARDINAL);
  (* generate entry-code for procedure at level lev *)
VAR
  offset: CARDINAL;
BEGIN
  IF NOT errorInProc THEN
    GenGlobalOp(LDA+ABSLONG, 'M2Lib_StackFramePointer', 0, 0, 3);
    AddBytesToObjectFile(4);
    PutOp(PHA, 1);

    offset := lev * 2;

    GenGlobalOp(LDA+ABSLONG, 'M2Lib_Display', offset, 0, 3);
    AddBytesToObjectFile(4);

    PutOp(PHA, 1);
    PutOp(PHD, 1);
    PutOp(TSC, 1);
    GenGlobalOp(STA+ABSLONG, 'M2Lib_StackFramePointer', 0, 0, 3);
    AddBytesToObjectFile(4);
    PutOp(SEC, 1);
    PutDPReference(SBC+IMMEDIATE, l, 3);
    PutOp(TCS, 1);
    PutOp(INCACC, 1);
    PutOp(TCD, 1);

    GenGlobalOp(STA+ABSLONG, 'M2Lib_Display', offset, 0, 3);
    AddBytesToObjectFile(4);
  END;

  GenStackCheck(FALSE);

  stackPointer := 0;
END Link;

PROCEDURE Unlink(parSize : CARDINAL; lev : CARDINAL);
  (* generate exit-code for procedure at level lev *)
VAR
  offset: CARDINAL;
BEGIN
  IF NOT errorInProc THEN
    offset := lev * 2;

    GenGlobalOp(LDA+ABSLONG, 'M2Lib_StackFramePointer', 0, 0, 3);
    AddBytesToObjectFile(4);
    PutOp(TCS, 1);
    PutOp(PLD, 1);
    PutOp(PLA, 1);

    GenGlobalOp(STA+ABSLONG, 'M2Lib_Display', offset, 0, 3);
    AddBytesToObjectFile(4);

    PutOp(PLA, 1);
    GenGlobalOp(STA+ABSLONG, 'M2Lib_StackFramePointer', 0, 0, 3);
    AddBytesToObjectFile(4);

    IF parSize > 0 THEN
      PutStackReference(LDA+STKRELATIVE, 2); (* Move the return address back to just *)
      PutOp(TAY, 1);                         (* after the result space (if any)      *)
      PutStackReference(LDA+STKRELATIVE, 1);
      PutOp(TAX, 1);

      PutOp(TSC, 1);             (* now adjust the stack pointer to point *)
      PutOp(CLC, 1);             (* just below the return address.        *)
      PutDPReference(ADC+IMMEDIATE, parSize, 3);
      PutOp(TCS, 1);

      PutOp(TYA, 1);
      PutStackReference(STA+STKRELATIVE, 2);
      PutOp(TXA, 1);
      PutStackReference(STA+STKRELATIVE, 1);
    END;
    (*
      Don't generate the return (RTL) here.  That is generated by M2CM.GenReturn
      since it must also supply code to handle function results.
    *)
  END;

  (*
    Ensure that now that generation of that procedure is complete, the
    CPU is unlocked.  It may have been left locked by a FUNCTION result.
  *)
  Unlock(erCPU);
END Unlink;

PROCEDURE StoreResult(VAR x: Item; size: CARDINAL);
(*
  OPERATION:
    Will store 'x' on the stack at the place designated as the result space
    for a function.  This result space is located by adding 'size' to the
    current StackFramePointer.
*)
VAR
  reg:        DRegister;
  stackSave:  aAdr;
  szx:        WidType;
BEGIN
  IF (x.mode = cocMd) (* a boolean result *) OR
     (x.mode = regMd)    (* value already loaded *) THEN
    (*
      This code assumes that the value is not a REAL or LONGREAL!

      Since we are about to re-use the CPU registers, we must preserve the
      result value on the stack.  Unfortunately, if the result is a boolean,
      the value may not yet be in the register, so we must ensure that it
      is loaded.

      Calling PushVal solves these problems.  It loads the value if required
      (as in the case of a boolean result), and then pushes it.  In most
      cases the result is already loaded, so only the push operation is
      generated.
    *)
    PushVal(x);
  END;

  IF NOT errorInProc THEN
    GetReg(word, reg);
    GenGlobalOp(LDA+ABSLONG, 'M2Lib_StackFramePointer', 0, 0, 3);
    AddBytesToObjectFile(4);
    PutOp(CLC, 1);
    PutDPReference(ADC+IMMEDIATE, size+1, 3);
    PutDPReference(STA+DIRECT, RegAdr(reg), 2);

    IF (x.typ = realtyp) OR
       (x.typ = lrltyp) THEN
      stackSave := stackPointer;

      LoadF(x);  (* load the value to the stack as an extended value *)
      PutDPReference(PEA, 0, 3);
      PutDPReference(LDA+DIRECT, RegAdr(reg), 2);
      PutOp(PHA, 1);

      IF x.typ = realtyp THEN
        GenGlobalOp(JSL, '~SAVEREAL', 0, 0, 3);
        AddBytesToObjectFile(4);
      ELSE
        GenGlobalOp(JSL, '~SAVEDOUBLE', 0, 0, 3);
        AddBytesToObjectFile(4);
      END;

      stackPointer := stackSave - ExtendedSize;
    ELSE
      Isz(x, szx);

      LoadVal(x, erCPU);

      IF szx = byte THEN
        ShortM;
      END;

      PutOp(PHB, 1);
      PutDPReference(PEA, 0, 3);
      PutOp(PLB, 1);
      PutOp(PLB, 1);
      PutDPReference(STA+DIRINDIRECT, RegAdr(reg), 2);

      IF szx = long THEN
        PutDPReference(LDY+IMMEDIATE3, 2, 3);
        PutOp(TXA, 1);
        PutDPReference(STA+DIRINDBYY, RegAdr(reg), 2);
      END;
  
      PutOp(PLB, 1);
  
      IF szx = byte THEN
        LongM;
      END;
    END;

    Unlock(reg);
  END;

  Unlock(erCPU);
END StoreResult;

PROCEDURE CallExt   (proc : ObjPtr);
  (* call of external procedure *)
VAR
  pname: aSymbol;
BEGIN
  WITH proc^ DO
    IF pd <> NIL THEN
      IF name <> 0 THEN
        GenModulaEntryCode;
      END;

      GetProcedureName(proc, pname);
      GenGlobalOp(JSL, pname, 0, 0, 3);
      AddBytesToObjectFile(4);
    END;
  END (*WITH*);
END CallExt;

PROCEDURE CallInd(VAR x : Item);
  (* call of procedure variable *)
  VAR ea : CARDINAL;
BEGIN
  LoadD(x);

  IF NOT errorInProc THEN
    PutOp(PHB, 1);
    PutOp(PHK, 1);
    PutOp(PLB, 1);
    PutDPReference(LDY+IMMEDIATE3, 0, 3);
    PutOp(TDC, 1);
    PutOp(CLC, 1);
    PutDPReference(ADC+IMMEDIATE, x.adr, 3);
    PutDPReference(PER, 9, 3);
    PutDPReference(STA+STKRELBYY, 1, 2);
    PutOp(PLA, 1);
    PutOp(PLB, 1);
    PutOp(PHK, 1);
    PutDPReference(PER, 2, 3);
    PutDPReference(JML+JABSLIND, 0, 3);
  END;

  Release(x);
END CallInd;

PROCEDURE ExitModule;
(*
  OPERATION:
    Generates the main module (or application) exit code.

    Will shutdown the application by:

      o Closing all open files.
      o Making a GSOS Quit Call.
*)
BEGIN
  IF (CDA IN Directives) OR
     (NDA IN Directives) THEN
    PutOp(PLB, 1);
    PutOp(W65C816.RTL, 1);
  ELSIF NOT (CDEV IN Directives) THEN
    GenGlobalOp(PEA, 'M2Lib__CloseParms', 0, -16, 2);
    AddBytesToObjectFile(3);
    GenGlobalOp(PEA, 'M2Lib__CloseParms', 0, 0, 2);
    AddBytesToObjectFile(3);
    PutDPReference(PEA, 2024H, 3);
    PutLongReference(JSL, 0E100B0H, 4);

    GenGlobalOp(LDA+ABSLONG, '~USER_ID', 0, 0, 3);
    AddBytesToObjectFile(4);
    PutOp(PHA, 1);
    PutDPReference(LDX+IMMEDIATE3, 1102H, 3);
    PutLongReference(JSL, 0E10000H, 4);

    PutDPReference(LDA+IMMEDIATE, 0, 3);

    IF (RTL IN Directives) OR
       (INIT IN Directives) THEN
      GenGlobalOp(JMP+JABSL, '~RTL', 0, 0, 3);
    ELSE
      GenGlobalOp(JMP+JABSL, '~QUIT', 0, 0, 3);
    END;

    AddBytesToObjectFile(4);
  END;

  EndSegment(skCode, aSegmentAttributeSet{});
END ExitModule;

PROCEDURE EnterModule;
(*
  OPERATION:
    Generates the main module (or application) entry code.

    The generated code will:

      o Place the CPU in Native mode, and ensure that the registers are all
        16 bits wide.
      o Setup the Data Bank Register to contain the bank containing the
        ~GLOBALS segment.
      o Store the user id supplied by GSOS in the global variable ~User_ID.
      o Store both the stack and direct page registers
*)
VAR
  modName:  aSymbol;
  keyName:  aSymbol;
  OK:       BOOLEAN;
  entry:    CARDINAL;
  label:    aSymbol;
  len:      CARDINAL;
  index:    CARDINAL;
  shutLink: CARDINAL;

  PROCEDURE PutAdr(id: CARDINAL);
  VAR
    idName: aSymbol;
  BEGIN
    IdToSymbol(id, label);
    Concat(modName, '_', idName);
    Concat(idName, label, idName);

    PutByte(VAL(BYTE, 0EBH));           (* begin expression   *)
    PutByte(VAL(BYTE, 004H));

    PutByte(VAL(BYTE, 083H));           (* address of label   *)
    PutLabelText(idName);

    PutByte(VAL(BYTE, 000H));            (* end of expression  *)

    AddBytesToObjectFile(4);
  END PutAdr;

  PROCEDURE FixBReg;
  BEGIN
    GenGlobalOp(PEA, '~__Globals', 0, -8, 2);
    AddBytesToObjectFile(3);
    PutOp(PLB, 1);
    PutOp(PLB, 1);
  END FixBReg;

  PROCEDURE PutJMPAdr(id: CARDINAL);
  VAR
    idName: aSymbol;
  BEGIN
    IdToSymbol(id, label);
    Concat(modName, '_', idName);
    Concat(idName, label, idName);

    GenGlobalOp(JMP+JABSL, idName, 0, 0, 3);
    AddBytesToObjectFile(4);
  END PutJMPAdr;

  PROCEDURE PutLAdr(label: CARDINAL);
  BEGIN
    PutByte(VAL(BYTE, 0EBH));           (* begin expression               *)
    PutByte(VAL(BYTE, 04H));
    PutByte(VAL(BYTE, 087H));           (* "A": displacement of label     *)
    ReferenceLabel(label);
    PutByte(VAL(BYTE, 00H));            (* end of expression              *)
    AddBytesToObjectFile(4);
  END PutLAdr;

  PROCEDURE PutFixUserID;
  BEGIN
    GenGlobalOp(LDA+ABSLONG, '~USER_ID', 0, 0, 3);
    AddBytesToObjectFile(4);
    PutDPReference(And+IMMEDIATE, 0F0FFH, 3);
    GenGlobalOp(STA+ABSLONG, 'M2Lib_MasterID', 0, 0, 3);
    AddBytesToObjectFile(4);
  END PutFixUserID;

BEGIN
  (*
    Only generate a stack/direct page segment for a load file.  CDA's, NDA's
    and CDEVs are supplied their stack by the system.
  *)
  IF (Directives * aDirectiveSet{CDA, NDA, CDEV}) = aDirectiveSet{} THEN
    NewSegmentHeader('STACKMIN');
    objFile.segmentHeader^.RESSPC := VAL(LONGCARD, Stacksize);
    objFile.segmentHeader^.LOADNAME := '~Direct   ';
    IncrementSegmentLength(Stacksize);
    EndSegment(skDirectPage, aSegmentAttributeSet{});
  END;

  NewSegmentHeader('~_ROOT');

  objFile.segmentHeader^.LOADNAME := '~_ROOT    ';

  GetModuleName(0, modName, OK);

  IF (CDA IN Directives) OR
     (NDA IN Directives) THEN
    newLink(entry);

    IF CDA IN Directives THEN
      IdToSymbol(CDAName, label);
      PutByte(VAL(BYTE, Length(label)+1));
      PutLabelText(label);
      AddBytesToObjectFile(Length(label)+1);
      PutLAdr(entry);
      PutAdr(CDAShutdown);
    ELSE
      PutAdr(NDAOpen);
      PutAdr(NDAClose);
      PutAdr(NDAAction);
      PutLAdr(entry);
      PutOp(NDAPeriod, 2);
      PutOp(NDAMask, 2);

      IdToSymbol(NDAMenuline, label);

      index := 0;
      len := Length(label);

      WHILE index < len DO
        PutByteConstant(label[index]);
        INC(index);
      END;

      PutByteConstant(VAL(BYTE, 0));

      AddBytesToObjectFile(len+1);
    END;

    FixLink(entry);

    IF NDA IN Directives THEN
      GenGlobalOp(STA+ABSLONG, 'M2Lib_NDACode', 0, 0, 3);
      AddBytesToObjectFile(4);
      GenGlobalOp(STA+ABSLONG, 'M2Lib__Bootup', 0, 0, 3);
      AddBytesToObjectFile(4);

      (*
        Use the ORCA routine ~DAID to set up or initialise the NDA User ID.
      *)
      PutOp(PHA, 1);
      GenGlobalOp(JSL, '~DAID', 0, 0, 3);
      AddBytesToObjectFile(4);
    ELSE
      (*
        Use the ORCA routine ~DAID to set up or initialise the CDA User ID.

        A non zero flag value causes the User ID to be initialised.
      *)
      PutDPReference(PEA, 0FFFFH, 3);
      GenGlobalOp(JSL, '~DAID', 0, 0, 3);
      AddBytesToObjectFile(4);
    END;

    PutFixUserID;

    PutOp(PHB, 1);
    FixBReg;
  ELSIF CDEV IN Directives THEN
    newLink(entry);

    PutStackReference(LDA+STKRELATIVE, 12); (* get the message code *)
    PutDPReference(CMP+IMMEDIATE, 7, 3);    (* is it the create cdev message? *)
    PutLinkReference(BNE, entry, 2);        (* no, goto the message centre *)

    (*
      Use the ORCA routine ~DAID to set up or initialise the CDA User ID.

      A non zero flag value causes the User ID to be initialised.
    *)
    PutDPReference(PEA, 0FFFFH, 3);
    GenGlobalOp(JSL, '~DAID', 0, 0, 3);
    AddBytesToObjectFile(4);

    PutFixUserID;
  ELSE
    IF (RTL IN Directives) OR
       (INIT IN Directives) THEN
      GenGlobalOp(JSL, '~_BWSTARTUP4', 0, 0, 3);
      AddBytesToObjectFile(4);
    ELSE
      GenGlobalOp(JSL, '~_BWSTARTUP3', 0, 0, 3);
      AddBytesToObjectFile(4);
    END;

    FixBReg;
    GenGlobalOp(LDA+ABSLONG, '~COMMANDLINE', 2, 0, 3);
    AddBytesToObjectFile(4);
    GenGlobalOp(STA+ABSLONG, 'M2Lib_CommandLine', 2, 0, 3);
    AddBytesToObjectFile(4);
    GenGlobalOp(LDA+ABSLONG, '~COMMANDLINE', 0, 0, 3);
    AddBytesToObjectFile(4);
    GenGlobalOp(STA+ABSLONG, 'M2Lib_CommandLine', 0, 0, 3);
    AddBytesToObjectFile(4);
    GenGlobalOp(LDA+ABSLONG, '~MINSTACK', 0, 0, 3);
    AddBytesToObjectFile(4);
    PutOp(TAX, 1);
    PutOp(CLC, 1);
    PutDPReference(ADC+IMMEDIATE, 200H, 3);
    GenGlobalOp(STA+ABSLONG, 'M2Lib_StackBottom', 0, 0, 3);
    AddBytesToObjectFile(4);
    PutOp(TXA, 1);
    PutOp(CLC, 1);
    PutDPReference(ADC+IMMEDIATE, Stacksize, 3);
    GenGlobalOp(STA+ABSLONG, 'M2Lib_StackTop', 0, 0, 3);
    AddBytesToObjectFile(4);

    PutFixUserID;
  END;

  IF NDA IN Directives THEN
    newLink(shutLink);
    GenGlobalOp(LDA+ABSLONG, 'M2Lib__Bootup', 0, 0, 3);
    AddBytesToObjectFile(4);
    PutLinkReference(BEQ, shutLink, 2);
  END;

  GenGlobalOp(LDA+IMMEDIATE, 'M2Lib__sysDescriptor', 0, -16, 2);
  AddBytesToObjectFile(3);
  GenGlobalOp(STA+ABSLONG, 'M2Lib_activeProcess', 2, 0, 3);
  AddBytesToObjectFile(4);
  GenGlobalOp(LDA+IMMEDIATE, 'M2Lib__sysDescriptor', 0, 0, 2);
  AddBytesToObjectFile(3);
  GenGlobalOp(STA+ABSLONG, 'M2Lib_activeProcess', 0, 0, 3);
  AddBytesToObjectFile(4);
  PutDPReference(LDA+IMMEDIATE, 1, 3);
  GenGlobalOp(STA+ABSLONG, 'M2Lib__sysDescriptor', 22H, 0, 3);
  AddBytesToObjectFile(4);

  (*
    Save the address of the main modules initialisation procedure in 
    M2Lib__InitProc in case we crash.
  *)
  GetModuleKey(0, keyName, OK);
  Concat(modName, keyName, keyName);
  GenGlobalOp(LDA+IMMEDIATE, keyName, 0, -16, 2);
  AddBytesToObjectFile(3);
  GenGlobalOp(STA+ABSLONG, 'M2Lib__InitProc', 2, 0, 3);
  AddBytesToObjectFile(4);
  GenGlobalOp(LDA+IMMEDIATE, keyName, 0, 0, 2);
  AddBytesToObjectFile(3);
  GenGlobalOp(STA+ABSLONG, 'M2Lib__InitProc', 0, 0, 3);
  AddBytesToObjectFile(4);

  IF NDA IN Directives THEN
    FixLink(shutLink);
  END;

  InitModule(0);  (* Initialise the main module.  This will in turn
                     call the init procedures of all other imported
                     modules. *)

  IF CDEV IN Directives THEN
    FixLink(entry);
    PutJMPAdr(CDEVName);
  END;
END EnterModule;

PROCEDURE InitModule(m : CARDINAL);
BEGIN
  ExternalCall(m, 0, 0);
END InitModule;

PROCEDURE Move(VAR x, y: Item);    (* value(x) --> y            *)
VAR
  reg:    DRegister;
  loc:    aAdr;
  skip:   CARDINAL;
  szx:    WidType;

  PROCEDURE StoreIndirect(loc: aAdr; offset: CARDINAL);
  BEGIN
    LoadVal(x, erCPU);

    IF szx = byte THEN
      ShortM;
    END;

    IF offset <> 0 THEN
      PutDPReference(LDY+IMMEDIATE3, offset, 3);
      PutDPReference(STA+DIRINDBYYLG, loc, 2);
    ELSE
      PutDPReference(STA+DIRINDLONG, loc, 2);
    END;

    IF szx = long THEN
      PutOp(TXA, 1);
      PutDPReference(LDY+IMMEDIATE3, offset+2, 3);
      PutDPReference(STA+DIRINDBYYLG, loc, 2);
    END;

    IF szx = byte THEN
      LongM;
    END;

    Unlock(erCPU);
  END StoreIndirect;

  PROCEDURE StoreYAdr;
  BEGIN
    IF y.indexed THEN
      LoadIndexTo(AddToA, y.indexReg);
      newLink(skip);
      PutLinkReference(BCC, skip, 2);
      PutOp(INX, 1);
      FixLink(skip);
      y.indexed := dontLoseIndex;
    END;

    GetReg(long, reg);
    loc := RegAdr(reg);

    PutDPReference(STA+DIRECT, loc, 2);
    PutDPReference(STX+DIRECT, loc+2, 2);

    Unlock(erCPU);
  END StoreYAdr;

BEGIN
  IF NOT errorInProc THEN
    Isz(x, szx);

    IF szx = setsized THEN
      MoveBlock(x, y, x.typ^.size, FALSE);
    ELSE
      IF y.mode = stkMd THEN
        IF y.indir THEN
          IF x.mode = regMd THEN
            LoadD(x);
          END;

          loc := stackPointer - y.adr;

          IF (loc = 0) AND
             (NOT y.notpop) THEN
            PutOp(PLA, 1);
            PutOp(PLX, 1);

            DEC(stackPointer, 4);
          ELSE
            PutStackReference(LDA+STKRELATIVE, loc + 3);
            PutOp(TAX, 1);
            PutStackReference(LDA+STKRELATIVE, loc + 1);
          END;

          IncAdr(y.off);

          (*
            StoreYAdr handles indexing and sets 'loc' to the direct page
            location of y's address
          *)
          StoreYAdr;
          StoreIndirect(loc, 0);
          Unlock(reg);
        ELSE
          LoadVal(x, erCPU);

          CASE szx OF (* size is in "bytes" *)
            byte:
              ShortM;
              PutOp(PHA, 1);
              LongM;
            |
            word:
              PutOp(PHA, 1);
            |
            long:
              PutOp(PHX, 1);
              PutOp(PHA, 1);
          END;

          INC(stackPointer, x.typ^.size);
        END;
      ELSE
        IF y.indir THEN
          IF y.mode = regMd THEN
            StoreYAdr; (* sets up loc and reg & handles indexing *)
            StoreIndirect(loc, y.off);
            Unlock(reg);
          ELSE
            IF y.mode <> DregMd THEN
              IF x.mode = regMd THEN
                PushVal(x);
              END;

              LoadAdr(y); (* LoadAdr allows for a post-offset *)
              StoreYAdr; (* sets up loc and reg & handles indexing *)
              StoreIndirect(loc, 0);
              Unlock(reg);
            ELSE
              IF y.indexed THEN
                LoadIndexTo(AddToA, y.indexReg);
                newLink(skip);
                PutLinkReference(BCC, skip, 2);
                PutOp(INX, 1);
                FixLink(skip);
                y.indexed := dontLoseIndex;
              END;

              StoreIndirect(y.adr, y.off);
            END;
          END;
        ELSE
          IF y.mode = regMd THEN
            LoadVal(x, y.register);
          ELSE
            IF x.mode <> regMd THEN
              LoadVal(x, erCPU);
            END;

            StoreVal(y, x.register);
          END;
        END;
      END;
    END;
  END;

  Unlock(erCPU);
END Move;

PROCEDURE MoveAdr   (VAR x, y: Item);    (* adr(x)   --> y            *)
VAR
  reg: DRegister;
  loc: aAdr;
BEGIN
  IF NOT errorInProc THEN
    IF (y.mode = stkMd) AND (NOT y.indir) THEN
      PushAdr(x);
    ELSIF y.mode = regMd THEN
      LoadAdr(x);
    ELSE
      LoadAdr(y);

      GetReg(long, reg);
      loc := RegAdr(reg);

      PutDPReference(STA+DIRECT, loc, 2);
      PutDPReference(STX+DIRECT, loc+2, 2);
      Release(y);  (* we have just stored 'y' in 'reg', so unlock it *)

      LoadAdr(x);

      PutDPReference(LDY+IMMEDIATE3, 2, 3);
      PutDPReference(STA+DIRINDLONG, loc, 2);
      PutOp(TXA, 1);
      PutDPReference(STA+DIRINDBYYLG, loc, 2);

      Release(x);
      Unlock(reg);
    END;
  END;
END MoveAdr;

PROCEDURE MoveBlock(VAR x, y:      Item;     (* mve 'sz' bytes of x to y  *)
                        sz:        CARDINAL;
                        isstring:  BOOLEAN);
VAR
  loopStart:        CARDINAL;
  idx:              CARDINAL;
  szx:              CARDINAL;
  reg:              DRegister;
  loc:              aAdr;
  saveStack:        INTEGER;
  intSize:          INTEGER;
  removeXFromStack: BOOLEAN;

  PROCEDURE PushSize;
  BEGIN
    PutDPReference(PEA, sz, 3);      (* push the max number of bytes *)
    INC(stackPointer, 2);
  END PushSize;

BEGIN
  IF NOT errorInProc THEN
    IF (y.mode = stkMd) AND
       (NOT y.indir) THEN
      IF x.mode = conMd THEN
        (* we must allow for the moving of a large set to the stack *)
        szx := sz;
        idx := (szx DIV 2);

        (*
          Push the top byte if there is an odd number of them
        *)
        IF ODD(szx) THEN
          DEC(szx);

          ShortM;
          PutDPReference(LDA+IMMEDIATE, VAL(CARDINAL, x.val.FS.set[idx]), 2);
          PutOp(PHA, 1);
          LongM;
        END;

        WHILE szx > 0 DO
          DEC(idx);
          DEC(szx, 2);
          PutDPReference(PEA, VAL(CARDINAL, x.val.FS.set[idx]), 3);
        END;

        INC(stackPointer, sz);
      ELSIF (x.mode <> stkMd) OR
            ((x.mode = stkMd) AND ((x.adr <> stackPointer) OR x.notpop)) THEN
        intSize := VAL(INTEGER, sz);
        StackTop(-intSize);        (* allocate space on the stack for the block *)

        GetReg(word, reg);
        loc := RegAdr(reg);

        PutOp(TSC, 1);
        PutOp(INCACC, 1);

        saveStack := stackPointer;

        IF NOT isstring THEN
          PutDPReference(PEA, 0, 3);       (* push address of stack copy of 'y' *)
          PutOp(PHA, 1);

          PushAdr(x);                     (* push address of 'x' *)

          PushSize;
          GenGlobalOp(JSL, '~MOVE', 0, 0, 3);
          AddBytesToObjectFile(4);
        ELSE
          PutDPReference(STA+DIRECT, loc, 2);

          PushSize;

          PushAdr(x);                     (* push address of 'x' *)

          PushSize;

          PutDPReference(PEA, 0, 3);       (* push address of stack copy of 'y' *)
          PutDPReference(LDA+DIRECT, loc, 2);
          PutOp(PHA, 1);

          GenModulaEntryCode;
          GenGlobalOp(JSL, 'M2Lib_CopyString', 0, 0, 3);
          AddBytesToObjectFile(4);
        END;

        stackPointer := saveStack;
        INC(stackPointer, sz);
        Unlock(reg);
      END;
    ELSIF (x.mode = conMd) AND (x.typ^.form = Set) THEN
      GetReg(long, reg);
      LoadAdr(y);
      PutDPReference(STA+DIRECT, RegAdr(reg), 2);
      PutDPReference(STX+DIRECT, RegAdr(reg)+2, 2);
      Unlock(erCPU);

      szx := x.typ^.size;
      idx := (szx DIV 2);

      (*
        Store the top byte if there is an odd number of them
      *)
      IF ODD(szx) THEN
        DEC(szx);

        ShortM;
        PutDPReference(LDY+IMMEDIATE3, szx, 3);
        PutDPReference(LDA+IMMEDIATE, VAL(CARDINAL, x.val.FS.set[idx]), 2);
        PutDPReference(STA+DIRINDBYYLG, RegAdr(reg), 2);
        LongM;
      END;

      WHILE szx > 0 DO
        DEC(idx);
        DEC(szx, 2);

        PutDPReference(LDA+IMMEDIATE, VAL(CARDINAL, x.val.FS.set[idx]), 3);

        IF szx <> 0 THEN
          PutDPReference(LDY+IMMEDIATE3, szx, 3);
          PutDPReference(STA+DIRINDBYYLG, RegAdr(reg), 2);
        ELSE
          PutDPReference(STA+DIRINDLONG, RegAdr(reg), 2);
        END;
      END;

      Unlock(reg);
    ELSE (* mode <> conMd *)
      saveStack := stackPointer;

      IF NOT isstring THEN
        (*
          If 'x' is currently on the stack, and we are moving it to a variable
          of some sort, AND we don't need it on the stack anymore, then remove it
          after the block copy has completed.
        *)
        removeXFromStack := (x.mode = stkMd) AND (NOT x.notpop);

        PushAdr(y);

        PushAdr(x);

        PushSize;
        GenGlobalOp(JSL, '~MOVE', 0, 0, 3);
        AddBytesToObjectFile(4);
      ELSE
        PushSize;

        (*
          If 'x' is currently on the stack, and we are moving it to a variable
          of some sort, AND we don't need it on the stack anymore, then remove it
          after the block copy has completed.
        *)
        removeXFromStack := (x.mode = stkMd) AND (NOT x.notpop);

        PushAdr(x);

        PushSize;

        PushAdr(y);

        GenModulaEntryCode;
        GenGlobalOp(JSL, 'M2Lib_CopyString', 0, 0, 3);
        AddBytesToObjectFile(4);
      END;

      stackPointer := saveStack;

      IF removeXFromStack THEN
        StackTop(sz);
      END;
    END;
  END;
END MoveBlock;

PROCEDURE LoadAdr   (VAR x: Item);       (* load address of x         *)
(*
  OPERATION:
    Load the address of the item into the A and X registers.  The A register
    will contain the LSW, and the X register will contain the MSW.
*)
VAR
  variableName: aSymbol;
  procName:     aSymbol;
  offset:       aAdr;
  skip:         CARDINAL;
  dispAdr:      CARDINAL;
  OK:           BOOLEAN;
  outsideDirect:BOOLEAN;
  strNum:       CARDINAL;
  strLevel:     CARDINAL;
  length:       CARDINAL;
BEGIN
  IF x.mode <> regMd THEN
    CheckCPUUsage(x);
  END;

  IF NOT errorInProc THEN
    WITH x DO
      IF mode = glbMd THEN
        GetVariableName(x, variableName);

        IF indir THEN
          GenGlobalOp(LDA+ABSLONG, variableName, 2+adr, 0, 3);
          AddBytesToObjectFile(4);
          PutOp(TAX, 1);
          GenGlobalOp(LDA+ABSLONG, variableName, adr, 0, 3);
          AddBytesToObjectFile(4);
          offset := off;
        ELSE
          GenGlobalOp(LDX+IMMEDIATE3, variableName, 0, -16, 2);
          AddBytesToObjectFile(3);
          GenGlobalOp(LDA+IMMEDIATE, variableName, 0, 0, 2);
          AddBytesToObjectFile(3);
          offset := adr + off;
        END;

        IncAdr(offset);
      ELSIF mode = locMd THEN
        IF indir THEN
          offset := adr;
        ELSE
          offset := adr + off;
        END;

        IF indir AND
           ((offset + 4) >= 256) THEN
          outsideDirect := TRUE;
        ELSE
          outsideDirect := FALSE;
        END;

        IF lev <> curLev THEN
          INC(offset, WorkingStorage);

          dispAdr := (lev - 1) * 2;

          GenGlobalOp(LDA+ABSLONG, 'M2Lib_Display', dispAdr, 0, 3);
          AddBytesToObjectFile(4);
        ELSIF NOT indir OR outsideDirect THEN
          PutOp(TDC, 1);
        END;

        IF (lev <> curLev) OR
           NOT indir OR
           outsideDirect THEN
          PutOp(CLC, 1);

          IF isParam AND (lev <> curLev) THEN
            PutDisplayReference(ADC+IMMEDIATE, offset, parent, 3);
          ELSE
            PutDPReference(ADC+IMMEDIATE, offset, 3);
          END;
        END;

        IF NOT indir THEN
          PutDPReference(LDX+IMMEDIATE3, 0, 3);
        ELSE
          IF (NOT outsideDirect) AND
             (lev = curLev) THEN
            PutDPReference(LDA+DIRECT, offset + 2, 2);
            PutOp(TAX, 1);
            PutDPReference(LDA+DIRECT, offset, 2);
          ELSE
            PutOp(TAX, 1);
            PutLongReference(LDA+ABSLONGBYX, 2, 4);
            PutOp(TAY, 1);
            PutLongReference(LDA+ABSLONGBYX, 0, 4);
            PutOp(TYX, 1);
          END;

          IncAdr(off);
        END;
      ELSIF mode = regMd THEN
        (* should already be in the registers *)
      ELSIF mode = stkMd THEN
        (* pull it back off the stack *)
  
        IF indir THEN
          offset := stackPointer - adr;

          IF (offset = 0) AND
             (NOT notpop) THEN
            PutOp(PLA, 1);
            PutOp(PLX, 1);

            DEC(stackPointer, 4);
          ELSIF (offset + 4) < 256 THEN
            PutStackReference(LDA+STKRELATIVE, offset + 3);
            PutOp(TAX, 1);
            PutStackReference(LDA+STKRELATIVE, offset + 1);
          ELSE
            PutOp(TSC, 1);
            PutOp(CLC, 1);
            PutDPReference(ADC+IMMEDIATE, offset+1, 3);
            PutOp(TAX, 1);
            PutLongReference(LDA+ABSLONGBYX, 2, 4);
            PutOp(TAY, 1);
            PutLongReference(LDA+ABSLONGBYX, 0, 4);
            PutOp(TYX, 1);
          END;

          IncAdr(off);
        ELSE
          offset := (stackPointer - adr) + 1;

          PutOp(TSC, 1);

          IF offset <> 1 THEN
            PutOp(CLC, 1);
            PutDPReference(ADC+IMMEDIATE, offset, 3);
          ELSE
            PutOp(INCACC, 1);
          END;

          PutDPReference(LDX+IMMEDIATE3, 0, 3);
        END;
      ELSIF mode = conMd THEN
        IF (NOT x.val.strChar) AND
           (x.typ = stringtyp) THEN
          strNum := x.val.D0;
          strLevel := x.val.D3;
        ELSE
          IF curLev = 0 THEN
            AllocChar(globalStringList, x.val.Ch, strNum, length);
          ELSE
            AllocChar(localStringList, x.val.Ch, strNum, length);
          END;

          strLevel := curLev;
        END;

        IF strLevel = 0 THEN
          GetStringName(strNum, mainmod^.modno, 0, variableName);
          GenGlobalOp(LDA+IMMEDIATE, variableName, 0, 0, 2);
          AddBytesToObjectFile(3);
          GenGlobalOp(LDX+IMMEDIATE3, variableName, 0, -16, 2);
          AddBytesToObjectFile(3);
        ELSE
          PutLinkReference(LDA+IMMEDIATE, strNum, 3);
          GenLocalOp(LDX+IMMEDIATE3, strNum, FALSE, 0, TRUE, 2);
          AddBytesToObjectFile(3);
        END;
      ELSIF mode = DregMd THEN
        IF indir THEN
          PutDPReference(LDA+DIRECT, adr, 2);
          PutDPReference(LDX+DIRECT, adr+2, 2);

          IncAdr(off);
        ELSE
          PutOp(TDC, 1);
          PutOp(CLC, 1);
          PutDPReference(ADC+IMMEDIATE, adr, 3);
          PutDPReference(LDX+IMMEDIATE3, 0, 3);
        END;

        Release(x);
      ELSIF mode = procMd THEN
        GetProcedureName(proc, procName);

        GenGlobalOp(LDA+IMMEDIATE, procName, 0, 0, 2);
        AddBytesToObjectFile(3);
        GenGlobalOp(LDX+IMMEDIATE3, procName, 0, -16, 2);
        AddBytesToObjectFile(3);
      ELSIF mode = toolMd THEN
        Mark(228);
      END; (* if mode ... *)

      IF indexed AND
         (mode > regMd) AND
         (mode < conMd) THEN
        newLink(skip);
        LoadIndexTo(AddToA, indexReg);
        PutLinkReference(BCC, skip, 2);
        PutOp(INX, 1);
        FixLink(skip);
        indexed := dontLoseIndex;
      END;

      IF indir THEN
        GenNILCheck(erCPU, FALSE);
      END;
    END; (* WITH x *)
  END;

  SetregMd(x, x.typ);
END LoadAdr;

PROCEDURE PushAdr   (VAR x: Item);       (* push address of x         *)
VAR
  pushit:       BOOLEAN;
  variableName: aSymbol;
  procName:     aSymbol;
  offset:       aAdr;
  skip:         CARDINAL;
  dispAdr:      CARDINAL;
  OK:           BOOLEAN;
  outsideDirect:BOOLEAN;
  strNum:       CARDINAL;
  strLevel:     CARDINAL;
  length:       CARDINAL;
BEGIN
  IF x.mode <> regMd THEN
    CheckCPUUsage(x);
  END;

  IF NOT errorInProc THEN
    pushit := TRUE;

    WITH x DO
      IF mode = glbMd THEN
        GetVariableName(x, variableName);

        IF indir THEN
          GenGlobalOp(LDA+ABSLONG, variableName, 2+adr, 0, 3);
          AddBytesToObjectFile(4);
          PutOp(TAX, 1);
          GenGlobalOp(LDA+ABSLONG, variableName, adr, 0, 3);
          AddBytesToObjectFile(4);
          offset := off;
        ELSE
          offset := adr + off;

          IF (offset = 0) AND NOT indexed THEN
            GenGlobalOp(PEA, variableName, 0, -16, 2);
            AddBytesToObjectFile(3);
            GenGlobalOp(PEA, variableName, 0, 0, 2);
            AddBytesToObjectFile(3);

            INC(stackPointer, 4);

            pushit := FALSE;
          ELSE
            GenGlobalOp(LDX+IMMEDIATE3, variableName, 0, -16, 2);
            AddBytesToObjectFile(3);
            GenGlobalOp(LDA+IMMEDIATE, variableName, 0, 0, 2);
            AddBytesToObjectFile(3);
          END;
        END;

        IncAdr(offset);
      ELSIF mode = locMd THEN
        IF indir THEN
          offset := adr;
        ELSE
          offset := adr + off;
        END;

        IF indir AND
           ((offset + 4) >= 256) THEN
          outsideDirect := TRUE;
        ELSE
          outsideDirect := FALSE;
        END;

        IF lev <> curLev THEN
          INC(offset, WorkingStorage);

          dispAdr := (lev - 1) * 2;

          GenGlobalOp(LDA+ABSLONG, 'M2Lib_Display', dispAdr, 0, 3);
          AddBytesToObjectFile(4);
        ELSIF NOT indir OR outsideDirect THEN
          PutOp(TDC, 1);
        END;

        IF (lev <> curLev) OR
           NOT indir OR
           outsideDirect THEN
          PutOp(CLC, 1);

          IF isParam AND (lev <> curLev) THEN
            PutDisplayReference(ADC+IMMEDIATE, offset, parent, 3);
          ELSE
            PutDPReference(ADC+IMMEDIATE, offset, 3);
          END;
        END;

        IF NOT indir THEN
          IF indexed THEN
            PutDPReference(LDX+IMMEDIATE3, 0, 3);
          ELSE
            PutDPReference(PEA, 0, 3);
            PutOp(PHA, 1);

            INC(stackPointer, 4);

            pushit := FALSE;
          END;
        ELSE
          IF (NOT outsideDirect) AND
             (lev = curLev) THEN
            PutDPReference(LDA+DIRECT, offset + 2, 2);
            PutOp(TAX, 1);
            PutDPReference(LDA+DIRECT, offset, 2);
          ELSE
            PutOp(TAX, 1);
            PutLongReference(LDA+ABSLONGBYX, 2, 4);
            PutOp(TAY, 1);
            PutLongReference(LDA+ABSLONGBYX, 0, 4);
            PutOp(TYX, 1);
          END;

          IncAdr(off);
        END;
      ELSIF mode = regMd THEN
        (* should already be in the registers *)
      ELSIF mode = stkMd THEN
        (* pull it back off the stack *)
  
        IF indir THEN
          offset := stackPointer - adr;

          IF (offset = 0) AND
             (NOT notpop) THEN
            IF NOT indexed THEN
              pushit := FALSE;
            ELSE
              PutOp(PLA, 1);
              PutOp(PLX, 1);

              DEC(stackPointer, 4);
            END;
          ELSIF (offset + 4) < 256 THEN
            PutStackReference(LDA+STKRELATIVE, offset + 3);
            PutOp(TAX, 1);
            PutStackReference(LDA+STKRELATIVE, offset + 1);
          ELSE
            PutOp(TSC, 1);
            PutOp(CLC, 1);
            PutDPReference(ADC+IMMEDIATE, offset+1, 3);
            PutOp(TAX, 1);
            PutLongReference(LDA+ABSLONGBYX, 2, 4);
            PutOp(TAY, 1);
            PutLongReference(LDA+ABSLONGBYX, 0, 4);
            PutOp(TYX, 1);
          END;

          IncAdr(off);
        ELSE
          offset := (stackPointer - adr) + 1;

          PutOp(TSC, 1);

          IF offset <> 1 THEN
            PutOp(CLC, 1);
            PutDPReference(ADC+IMMEDIATE, offset, 3);
          ELSE
            PutOp(INCACC, 1);
          END;

          IF NOT indexed THEN
            PutDPReference(PEA, 0, 3);
            PutOp(PHA, 1);

            INC(stackPointer, 4);

            pushit := FALSE;
          ELSE
            PutDPReference(LDX+IMMEDIATE3, 0, 3);
          END;
        END;
      ELSIF mode = conMd THEN
        IF (NOT x.val.strChar) AND
           (x.typ = stringtyp) THEN
          strNum := x.val.D0;
          strLevel := x.val.D3;
        ELSE
          IF curLev = 0 THEN
            AllocChar(globalStringList, x.val.Ch, strNum, length);
          ELSE
            AllocChar(localStringList, x.val.Ch, strNum, length);
          END;

          strLevel := curLev;
        END;

        IF strLevel = 0 THEN
          GetStringName(strNum, mainmod^.modno, 0, variableName);
          GenGlobalOp(PEA, variableName, 0, -16, 2);
          AddBytesToObjectFile(3);
          GenGlobalOp(PEA, variableName, 0, 0, 2);
          AddBytesToObjectFile(3);
        ELSE
          GenLocalOp(PEA, strNum, FALSE, 0, TRUE, 2);
          AddBytesToObjectFile(3);
          PutLinkReference(PEA, strNum, 3);
        END;

        INC(stackPointer, 4);

        pushit := FALSE;
      ELSIF mode = DregMd THEN
        IF indir THEN
          PutDPReference(LDA+DIRECT, adr, 2);
          PutDPReference(LDX+DIRECT, adr+2, 2);

          IncAdr(off);
        ELSE
          PutOp(TDC, 1);
          PutOp(CLC, 1);
          PutDPReference(ADC+IMMEDIATE, adr, 3);

          PutDPReference(PEA, 0, 3);
          PutOp(PHA, 1);

          INC(stackPointer, 4);

          pushit := FALSE;
        END;

        Release(x);
      ELSIF mode = procMd THEN
        GetProcedureName(proc, procName);

        GenGlobalOp(PEA, procName, 0, -16, 2);
        AddBytesToObjectFile(3);
        GenGlobalOp(PEA, procName, 0, 0, 2);
        AddBytesToObjectFile(3);

        INC(stackPointer, 4);

        pushit := FALSE;
      ELSIF mode = toolMd THEN
        Mark(228);
      END; (* if mode ... *)

      IF indexed AND
         (mode > regMd) AND
         (mode < conMd) THEN
        newLink(skip);
        LoadIndexTo(AddToA, indexReg);
        PutLinkReference(BCC, skip, 2);
        PutOp(INX, 1);
        FixLink(skip);
        indexed := dontLoseIndex;
      END;

      IF indir THEN
        GenNILCheck(erCPU, NOT pushit);
      END;
    END; (* WITH x *)
  END;

  IF pushit THEN
    PutOp(PHX, 1);
    PutOp(PHA, 1);
    INC(stackPointer, 4);
  END;

  SetstkMd(x, x.typ);
(*CHANGE
BEGIN
  LoadAdr(x);
  PutOp(PHX, 1);
  PutOp(PHA, 1);
  INC(stackPointer, 4);
  SetstkMd(x, x.typ);
  Unlock(erCPU);
*)
END PushAdr;

PROCEDURE LoadD(VAR x: Item);       (* load data designated by x *)
VAR
  szx:  WidType;
  reg:  DRegister;
  y:    Item;
BEGIN
  Isz(x, szx);

  WITH x DO
    IF (mode <> DregMd) OR indir THEN
      (*
        Move would normally do the LoadVal for 'x' except that if it is already
        in DregMd we want to release the current DRegister before allocating
        another one.  To do this we load the value before we allocate the new
        register and do the move.
      *)
      LoadVal(x, erCPU); (* releases any allocated DRegister *)
      GetReg(szx, reg);
      SetDregMd(y, reg, typ);
      Move(x, y);
      x := y;
    END;
  END;
END LoadD;

PROCEDURE LoadX(VAR x : Item; req : WidType);
  (* load simple type x to a D-Register and    *)
  (* sign extend it to the width given by req. *)

VAR
  sz:     WidType;
  cload:  BOOLEAN;
  signar: BOOLEAN;
  lv:     LONGINT;

  PROCEDURE ExtendSign(oldWid, newWid: WidType);
  VAR
    positive: CARDINAL;
    done:     CARDINAL;
  BEGIN
    newLink(positive);

    IF oldWid = byte THEN
      IF newWid = word THEN
        PutDPReference(BITIMMED, 0080H, 3);
        PutLinkReference(BEQ, positive, 2);
        PutDPReference(ORA+IMMEDIATE, 0FF00H, 3);
        FixLink(positive);
      ELSIF newWid = long THEN
        newLink(done);
        PutDPReference(BITIMMED, 0080H, 3);
        PutLinkReference(BEQ, positive, 2);
        PutDPReference(ORA+IMMEDIATE, 0FF00H, 3);
        PutDPReference(LDX+IMMEDIATE3, 0FFFFH, 3);
        PutLinkReference(BRA, done, 2);
        FixLink(positive);
        PutDPReference(LDX+IMMEDIATE3, 0, 3);
        FixLink(done);
      ELSE
        Mark(412);
      END;
    ELSIF oldWid = word THEN
      (* word --> byte NOTE:
          if the value is -128 < x < 127, then we can sign extend, otherwise
          we can't. *)
      IF newWid = byte THEN
        PutDPReference(And+IMMEDIATE, 00FFH, 3);
      ELSIF newWid = long THEN
        newLink(done);
        PutDPReference(BITIMMED, 8000H, 3);
        PutLinkReference(BEQ, positive, 2);
        PutDPReference(LDX+IMMEDIATE3, 0FFFFH, 3);
        PutLinkReference(BRA, done, 2);
        FixLink(positive);
        PutDPReference(LDX+IMMEDIATE3, 0, 3);
        FixLink(done);
      ELSE
        Mark(413);
      END;
    ELSIF oldWid = long THEN
      IF newWid = word THEN
        (* do nothing.  just discard the x register *)
      ELSIF newWid = byte THEN
        PutDPReference(And+IMMEDIATE, 00FFH, 3);
      ELSE
        Mark(414);
      END;
    END;
  END ExtendSign;

BEGIN (* LoadX *)
  Isz(x,sz);

  cload := SimpleC(x); (* Real constants not included *)
  signar := SignedT(x);

  LoadVal(x, erCPU);

  IF NOT errorInProc THEN
    WITH x DO
      IF cload THEN
        IF signar THEN
          ExtendSign(sz, req);
        ELSE
          CASE req OF
            byte:
              IF sz <> byte THEN
                PutDPReference(And+IMMEDIATE, 00FFH, 3);
              END;
            |
            word:
            |
            long:
              PutDPReference(LDX+IMMEDIATE3, 0, 3);
            ELSE
              Mark(415);
          END;
        END;
  
        x.wid := req;
      ELSE
        IF sz < req THEN
          IF req = word THEN
            IF sz = byte THEN
              IF signar THEN
                ExtendSign(sz, req);
              END;
            END;
          ELSIF req = long THEN
            IF signar THEN
              IF sz < long THEN
                ExtendSign(sz, req);
              END;
            ELSE (* unsigned types *)
              IF sz < long THEN
                PutDPReference(LDX+IMMEDIATE3, 0, 3);
              END;
            END;
          ELSE
            Mark(416);
          END;
        END (*wid < req*);
  
        wid := req;
      END;
    END (*WITH*);
  ELSE
    x.wid := req;
  END;
END LoadX;

PROCEDURE InvertCC  (cond: Condition): Condition;  (* inverse condition *)
VAR
  c: CARDINAL;
BEGIN
  c := ORD(cond);

  IF c < 16 THEN
    IF ODD(c) THEN
      DEC(cond);
    ELSE
      INC(cond);
    END;
  ELSE
    c := c - 16;
    c := 15 - c;
    c := c + 16;
    cond := VAL(Condition, c);
  END;

  RETURN cond;
END InvertCC;

PROCEDURE SaveRegs(VAR savedRegs: DRegSet);
BEGIN
  savedRegs := registers;
END SaveRegs;

PROCEDURE SetRegs(toRegs: DRegSet);
BEGIN
  registers := toRegs;
END SetRegs;

END M2HM.
