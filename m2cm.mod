(*$Segment M2CM*)
(*$RangeCheck+*)
(*$OverflowCheck+*)
IMPLEMENTATION MODULE M2CM; (* HS 19.1.86 / 10.6.86 / 28.4.89 *)
                            (* WH 18.4.85 / 15.1.86 / 8.7.86; VC 5.6.88 *)

FROM M2Debug IMPORT
  GenStartProcedure, GenEndProcedure, GenDebugSymbols;
FROM M2DM IMPORT
  WordSize, MaxInt, MaxDouble, ObjPtr, StrPtr, ParPtr, PDPtr,       (* V2.6x *)
  Standard, ObjClass, StrForm, PDesc, ovflchk, aAdr, databank,
  notyp, undftyp, booltyp, chartyp, cardtyp, cardinttyp, inttyp,
  bitstyp,  lcardtyp, dbltyp, realtyp, lrltyp, proctyp, stringtyp,
  addrtyp,  wordtyp, bytetyp, mainmod, WorkingStorage, language, aSize;
FROM M2EM IMPORT
  ParamStartAdr;
FROM M2FM IMPORT
  FMove, LoadF, FMonad, ExtendedSize, SetfltMd;
FROM M2HM IMPORT
  byte, word, long, DRegSet, Release, SaveRegs, StoreResult,
  Condition, WidType, ItemMode, Item, StackTop, InvertCC, RegAdr,
  LongVal, WordVal, SimpleT, RealT, Inc1, Dec1, GetReg, Unlock,
  SetregMd, SetconMd, SetstkMd, MoveAdr, PushVal, PushAdr,
  Move, LoadD, LoadX, curLev, stackPointer, CheckCPUUsage,
  Tst1, Add2, Cmp2, MoveBlock, errorInProc, BranchWith,
  CheckClimit, CheckRange, CheckDbltoSingle, DynArray, DRegister,
  Branch, EnterCase, ExitCase, Link, Unlink, CallExt, CallInd,
  EnterModule, ExitModule, InitModule, GenTerminate;
FROM M2Lib IMPORT
  aTerminateStatus;
FROM M2LM IMPORT
  pc, maxM, FixLink, aSymbol, newLink, PutLinkAddress, AllocChar, 
  GetProcedureName, PutStringConstants, AddBytesToObjectFile, GetModuleName,
  localStringList, globalStringList, clearStringList, PutLinkReference, 
  segmentName, NDAOpen, NDAClose, NDAAction, CDAShutdown, CDEVName,
  PutOp, PutDPReference, PutLongReference, PutStackReference, IdToSymbol;
FROM M2OMF IMPORT NewSegmentHeader, EndSegment, GenGlobalOp, aSegmentAttribute,
  aSegmentAttributeSet, objFile, aSegmentKind, PutByte, PutWordConstant,
  PutWord, PutLabelText, PutLong;
FROM M2RM IMPORT ModNo, OutName;
FROM M2SM IMPORT
  Mark, Directives, aDirective, Diff, IdBuf, pModListEntry, modList;
FROM M2Shell IMPORT ShowProcs, DebugCode;
FROM M2TM IMPORT Find;
FROM Strings IMPORT Assign, Concat, Length;
FROM SYSTEM IMPORT BYTE, WORD;
FROM Terminal IMPORT Write, WriteString, WriteLn;
FROM W65C816 IMPORT PEA, JSL, STA, ABSLONG, LDX, IMMEDIATE3, PHA, PHY, PHX,
  PHB, PLB, IMMEDIATE, LDA, CMP, BNE, BRL, BEQ, BRA, TCS, STKRELATIVE, PER,
  PHK, DIRECT, DIRINDBYYLG, STX, LDY;

IMPORT W65C816;

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

PROCEDURE GenAssign(VAR x, y : Item);
  (*       x    :=    y     *)
  (*       y  ---->>  x     *)
  (* or    g  ---->>  f     *)
VAR
  f, g, h:  StrForm;
  xp, yp:   ParPtr;
  x0, y0:   Item;
  s:        CARDINAL;
  len:      CARDINAL;
  sadr:     aAdr;
  L:        CARDINAL;
  sz:       WidType;
  xt:       StrPtr;
BEGIN
  IF (x.mode = conMd) OR (x.mode > stkMd) THEN
    Mark(134);
  END;

  SRTest(y);
  f := x.typ^.form;
  h := f; (* holds original form of x during GenAssign *)
  g := y.typ^.form;

  IF f = Range THEN
    (* perform range check. *)
    WITH x DO
      IF y.mode = conMd THEN
        IF (LongVal(y) < typ^.min) OR
           (LongVal(y) > typ^.max) THEN
          Mark(138)
        END
      ELSE
        CheckRange(y, typ^.min, typ^.max, typ^.BndAdr);
      END;

      typ := typ^.RBaseTyp;
      f := typ^.form;
    END (*WITH*);
  END (*f = Range*);

  xt := x.typ; (* hold original type of x *)

  CASE f (* destination form *) OF

    Undef :    IF ((x.typ = wordtyp) & (y.typ^.size = VAL(aSize, 2))) OR
                  ((x.typ = bytetyp) & (y.typ^.size = VAL(aSize, 1))) THEN
                 Move(y,x)
               ELSE Mark(133)
               END;

  | Bool :     IF g = Bool THEN Move(y,x)
               ELSE Mark(133)
               END;

  | Char :     IF g = Char THEN Move(y,x)
               ELSE Mark(133)
               END;

  | Card :     IF (g = Card) OR (g = CardInt) THEN Move(y,x)
               ELSIF g = Int THEN
                 IF y.mode = conMd THEN
                   IF y.val.I < 0 THEN Mark(132) END;
                 ELSE (* emit check *)
                   IF h <> Range THEN CheckClimit(y, MaxInt) END;
                 END;
                 Move(y,x)
               ELSIF (g = Double) OR (g = LCard) THEN
                 LoadD(y); (* must be loaded long! *)
                 CheckDbltoSingle(y,x); (* emit check *)
                 y.typ := x.typ;
                 Move(y,x)
               ELSE Mark(133)
               END;

  | Int :      IF (g = Int) OR (g = CardInt) THEN Move(y,x)
               ELSIF g = Card THEN
                 IF y.mode = conMd THEN
                   IF y.val.C > MaxInt THEN Mark(208) END;
                 ELSE (* emit check *)
                   IF h <> Range THEN CheckClimit(y, MaxInt) END;
                 END;
                 Move(y,x)
               ELSIF (g = Double) OR (g = LCard) THEN
                 LoadD(y); (* must be loaded long! *)
                 CheckDbltoSingle(y,x); (* emit check *)
                 y.typ := x.typ;
                 Move(y,x)
               ELSE Mark(133)
               END;
  
  | Double :   IF g = Double THEN Move(y,x)                         (* V2.6x *)
               ELSIF (g = LCard) & (y.typ # addrtyp) THEN           (* V2.6x *)
                 IF y.mode = conMd THEN                             (* V2.6x *)
                   (* genuine LONGCARD-constant : *)                (* V2.6x *)
                   IF y.val.D < VAL(LONGINT, 0) THEN Mark(217) END;  (* V2.6x *)
                 ELSE (* emit check *)                              (* V2.6x *)
                   CheckClimit(y, MaxDouble);                       (* V2.6x *)
                 END;                                               (* V2.6x *)
                 Move(y,x)                                          (* V2.6x *)
               ELSIF (g = Int) OR (g = Card) OR (g = CardInt) THEN  (* V2.6x *)
                 IF y.mode = conMd THEN                             (* V2.6x *)
                   SetconMd(y, LongVal(y), dbltyp);                 (* V2.6x *)
                 ELSE                                               (* V2.6x *)
                   LoadX(y,long); y.typ := dbltyp;                  (* V2.6x *)
                 END;                                               (* V2.6x *)
                 Move(y,x)                                          (* V2.6x *)
               ELSE Mark(133)                                        (* V2.6x *)
               END;                                                 (* V2.6x *)

  | LCard :    IF g = LCard THEN Move(y,x)                          (* V2.6x *)
               ELSIF g = Double THEN                                (* V2.6x *)
                 IF y.mode = conMd THEN                             (* V2.6x *)
                   (* no check! allow all double constants, *)      (* V2.6x *)
                   (* because scanner delivers all these    *)      (* V2.6x *)
                   (* long constants of type LONGINT, even  *)      (* V2.6x *)
                   (* values such as 0FFFFFFFFH etc.        *)      (* V2.6x *)
                 ELSE (* emit check for genuine LCard only! *)      (* V2.6x *)
                   IF (xt = lcardtyp) THEN                          (* V2.6x *)
                     CheckClimit(y, MaxDouble);                     (* V2.6x *)
                   END;                                             (* V2.6x *)
                 END;                                               (* V2.6x *)
                 Move(y,x)                                          (* V2.6x *)
               ELSIF (g = Int) OR (g = Card) OR (g = CardInt) THEN  (* V2.6x *)
                 (* emit checks for genuine LCard only! *)          (* V2.6x *)
                 IF y.mode = conMd THEN                             (* V2.6x *)
                   IF (xt = lcardtyp) & (g = Int) THEN              (* V2.6x *)
                     IF y.val.I < 0 THEN Mark(218) END;              (* V2.6x *)
                   END;                                             (* V2.6x *)
                   SetconMd(y, LongVal(y), x.typ);                  (* V2.6x *)
                 ELSE                                               (* V2.6x *)
                   LoadX(y,long); y.typ := x.typ;                   (* V2.6x *)
                   IF (xt = lcardtyp) & (g = Int) THEN              (* V2.6x *)
                     CheckClimit(y, MaxInt);                        (* V2.6x *)
                   END;                                             (* V2.6x *)
                 END;                                               (* V2.6x *)
                 Move(y,x)                                          (* V2.6x *)
               ELSIF (xt = addrtyp) & (g = Pointer) THEN Move(y,x)  (* V2.6x *)
               ELSE Mark(133)                                        (* V2.6x *)
               END;                                                 (* V2.6x *)

  | Real :     IF g = Real THEN
                 FMove(y,x)
               ELSIF g = LongReal THEN
                 FMonad(Short,y);
                 y.typ := x.typ;
                 FMove(y,x)
               ELSE Mark(133)
               END;
  
  | LongReal : IF g = LongReal THEN FMove(y,x)
               ELSIF g = Real THEN
                 FMonad(Long,y);
                 y.typ := x.typ;
                 FMove(y,x)
               ELSE Mark(133)
               END;
  | Enum :     IF x.typ = y.typ THEN Move(y,x)
               ELSE Mark(133)
               END;
  
  | Set :      IF x.typ = y.typ THEN 
                 IF (x.typ^.size <= VAL(aSize, 4)) AND
                    (x.typ^.size <> VAL(aSize, 3)) THEN
                   Move(y,x);
                 ELSE
                   MoveBlock(y, x, x.typ^.size, FALSE);
                 END;
               ELSE Mark(133)
               END;

  | Pointer :  IF (x.typ = y.typ) OR (y.typ = addrtyp) THEN         (* V2.6 *)
                 Move(y,x)                                          (* V2.6 *)
               ELSE Mark(133)                                        (* V2.6 *)
               END;                                                 (* V2.6 *)

  | Opaque :   IF (x.typ = y.typ) THEN Move(y,x)                    (* V2.6 *)
               ELSE Mark(133)                                        (* V2.6 *)
               END;                                                 (* V2.6 *)
  
  | Record :   IF x.typ = y.typ THEN
                 s := x.typ^.size;
                 MoveBlock(y,x,s,FALSE)
               ELSE Mark(133)
               END;

  | ProcTyp :  IF y.mode = procMd THEN
                 (* procedure-constant to procedure-variable : *)
                 IF y.proc^.pd^.lev <> 0 THEN Mark(127)
                 ELSIF x.typ^.resTyp <> y.proc^.typ THEN Mark(128)
                 ELSE xp := x.typ^.firstPar; yp := y.proc^.firstParam;
                   WHILE xp <> NIL DO
                     IF yp <> NIL THEN
                       IF (xp^.varpar <> yp^.varpar) OR
                          ((xp^.typ <> yp^.typ) AND
                          ((xp^.typ^.form <> Array) OR
                           NOT xp^.typ^.dyn OR
                           (yp^.typ^.form <> Array) OR
                           NOT yp^.typ^.dyn OR
                           (xp^.typ^.ElemTyp <> yp^.typ^.ElemTyp))) THEN
                         Mark(129)
                       END;
                       yp := yp^.next
                     ELSE Mark(130)
                     END;
                     xp := xp^.next
                   END (*WHILE*);
                   IF yp <> NIL THEN Mark(131) END;
                   MoveAdr(y,x);
                 END;
               ELSIF x.typ = y.typ THEN Move(y,x)
               ELSE Mark(133)
               END;

  | Array :    s := x.typ^.size;
               IF (x.typ = y.typ) & NOT(x.typ^.dyn) THEN
                 MoveBlock(y,x,s,FALSE)
               ELSIF (x.mode = stkMd) & x.typ^.dyn THEN
                 (* formal parameter is dynamic array : *)
                 IF (g = Array) & (x.typ^.ElemTyp = y.typ^.ElemTyp) THEN
                   DynArray(x,y)
                 ELSE
                   IF (x.typ^.ElemTyp = chartyp) OR
                      (x.typ^.ElemTyp = bytetyp) THEN
                     IF g = String THEN
                       DynArray(x,y)
                     ELSIF (g = Char) & (y.mode = conMd) THEN
                       (* character-constant to dynamic array : *)

                       WITH y DO
                         typ := stringtyp;
                         val.strChar := TRUE;
                       END (*WITH*);

                       DynArray(x,y)
                     ELSIF (x.typ^.ElemTyp = bytetyp) THEN
                       DynArray(x,y)
                     ELSE Mark(133)
                     END
                   ELSE Mark(133)
                   END
                 END
               ELSIF x.typ^.ElemTyp = chartyp THEN
                 IF g = String THEN
                   (* string to fixed-size array : *)
                   (* check length of string.      *)
                   (* string constants have a nul terminator, making their *)
                   (* actual length larger.  it may be that the programmer *)
                   (* is using the knowledge that a string may be terminated *)
                   (* by it's length, so when checking the length, don't *)
                   (* include the nul terminator character. *)
                   IF (y.val.D1 - 1) > s THEN Mark(146) END;
                   MoveBlock(y,x,s,TRUE);
                 ELSIF (g = Char) & (y.mode = conMd) THEN
                   (* character-constant to fixed-size array : *)
                   IF curLev = 0 THEN
                     AllocChar(globalStringList, y.val.Ch, y.val.D0, len);
                   ELSE
                     AllocChar(localStringList, y.val.Ch, y.val.D0, len);
                   END;

                   WITH y DO
                     typ := stringtyp; val.D1 := len;
                   END (*WITH*);

                   MoveBlock(y,x,s,TRUE);
                  ELSE Mark(133)
                 END
               ELSE Mark(133)
               END;

  ELSE (* must not occur on the left side *)                        (* V2.6y *)
    Mark(133)                                                        (* V2.6y *)
  END (*CASE f*);

  x.typ := xt; (* restore original type of x *)
  Release(x);
  Release(y);
END GenAssign;

PROCEDURE GenFJ(VAR loc: CARDINAL);
VAR
  tmp: CARDINAL;
BEGIN
  Branch(T, loc, tmp);
END GenFJ;

PROCEDURE GenCFJ(VAR x: Item; VAR loc: CARDINAL);
BEGIN
  IF x.typ = booltyp THEN
    IF x.mode <> cocMd THEN
      Tst1(x);
      setCC(x, EQ);
    END;
  ELSE
    setCC(x, EQ);
    Mark(135);  (* type of expression must be boolean *)
  END;

  WITH x DO
    loc := Fjmp;
    Branch(CC, loc, Tjmp);
    FixLink(Tjmp);
  END;
END GenCFJ;

PROCEDURE GenBJ(loc: CARDINAL);
VAR
  tmp: CARDINAL;
BEGIN
  Branch(T, loc, tmp);
END GenBJ;

PROCEDURE GenCBJ(VAR x: Item; loc: CARDINAL);
BEGIN
  IF x.typ = booltyp THEN
    IF x.mode <> cocMd THEN
      Tst1(x);
      setCC(x, EQ);
    END;
  ELSE
    setCC(x, EQ);
    Mark(135);  (* type of expression must be boolean *)
  END;

  WITH x DO
    BranchWith(CC, loc, Tjmp, Fjmp);
    FixLink(Tjmp);
  END;
END GenCBJ;

PROCEDURE SpaceForFunction(func : StrPtr);
  (* reserve space on top of stack for function result. *)
VAR
  nul: Item;
  sz:  CARDINAL;
BEGIN
  sz := VAL(CARDINAL, func^.size);
  SetstkMd(nul, func);

  IF SimpleT(nul) OR
     RealT(nul) OR
     (sz IN {1, 2, 4, 8}) THEN
    StackTop(-func^.size);
  ELSE
    Mark(200); (* this function result size not implemented! *)
  END;
END SpaceForFunction;

PROCEDURE PrepCall(VAR x: Item; VAR fp: ParPtr; VAR regs: DRegSet);
VAR
  func:       StrPtr;
  stackBased: BOOLEAN;
BEGIN
  WITH x DO
    IF (mode = procMd) OR (mode = toolMd) THEN
      stackBased := (mode = toolMd) OR (proc^.procType = modula2);
      func := proc^.typ;
      fp := proc^.firstParam;
    ELSIF typ^.form = ProcTyp THEN
      stackBased := TRUE;
      func := typ^.resTyp;
      fp := typ^.firstPar;
    ELSE
      func := notyp;
      fp := NIL;
      Mark(136);  (* call of an object which is not a procedure *)
    END;

    SaveRegs(regs);

    IF (func <> notyp) AND stackBased THEN
      (*
        Doesn't change x, it just pushes the acc onto the stack if it is
        locked.
      *)
      CheckCPUUsage(x);
      SpaceForFunction(func);
    END;

    stkPtr := stackPointer;
  END (*WITH*);
END PrepCall;

PROCEDURE GenParam(VAR x: Item; VAR ap: Item; f: ParPtr);
VAR
  fp:       Item;
  ft, g:    StrForm;
  s:        CARDINAL;
BEGIN
  SetstkMd(fp, f^.typ);

  IF f^.varpar THEN
    IF (fp.typ^.form = Array) & fp.typ^.dyn & (fp.typ^.ElemTyp = bytetyp) THEN
      IF (x.mode = procMd) AND (x.proc^.procType <> modula2) THEN
        Mark(313);
      ELSE
        DynArray(fp, ap);
      END;
    ELSIF (fp.typ^.form = Array) & fp.typ^.dyn &
          (ap.typ^.form = Array) & (ap.typ^.ElemTyp = fp.typ^.ElemTyp) THEN
      IF (x.mode = procMd) AND (x.proc^.procType <> modula2) THEN
        Mark(313);
      ELSE
        DynArray(fp, ap);
      END;
    ELSIF (ap.typ = fp.typ) OR
          (fp.typ = wordtyp) & (ap.typ^.size = VAL(aSize, 2)) OR
          (fp.typ = bytetyp) & (ap.typ^.size = VAL(aSize, 1)) OR
          (fp.typ = addrtyp) & (ap.typ^.form = Pointer) THEN
      IF (ap.mode = procMd) & (f^.typ^.form # ProcTyp) THEN
        Mark(137)
      ELSIF ap.mode = conMd THEN
        Mark(143);
      ELSE
        PushAdr(ap);
      END;
    ELSE
      Mark(137);  (* type of VAR par is not identical to that of actual par *)
    END;
  ELSIF (x.mode = procMd) AND (x.proc^.procType = pascal) THEN
    IF ((fp.typ = realtyp) OR (fp.typ = lrltyp)) THEN
      IF fp.typ = ap.typ THEN
        LoadF(ap);
        (* GenAssign(fp, ap); *)
      ELSE
        Mark(133);
      END;
    ELSIF (ap.typ^.size <= VAL(aSize, 4)) AND (fp.typ^.form <> Array) THEN
      g := ap.typ^.form;

      IF (ap.typ^.size = VAL(aSize, 1)) AND
         ((g = Undef) OR (g = Char) OR (g = Enum)) THEN
        (*
          Pascal treats bytes, characters, and enumerated types as being 2 
          byte values!  We must therefor push an extra byte.
        *)
        LoadX(ap, word);

        SRTest(ap);
        ft := fp.typ^.form;
        g := ap.typ^.form;

        IF ft = Range THEN
          (* perform range check. *)
          WITH fp DO
            CheckRange(ap, typ^.min, typ^.max, typ^.BndAdr);

            typ := typ^.RBaseTyp;
            ft := typ^.form;
          END (*WITH*);
        END (*f = Range*);

        SetregMd(ap, wordtyp);

        CASE ft OF
          Undef:  IF fp.typ = bytetyp THEN
                    Move(ap, fp);
                  ELSE
                    Mark(133);
                  END;
        | Char:   IF g = Char THEN
                    Move(ap, fp);
                  ELSE
                    Mark(133);
                  END;
        | Enum:   IF fp.typ = ap.typ THEN
                    Move(ap, fp);
                  ELSE
                    Mark(133);
                  END;
        END;
      ELSE
        GenAssign(fp, ap);
      END;
    ELSIF ap.typ = fp.typ THEN
      (*
        Pascal structures that are > 4 bytes are pushed as addresses.  The
        code generated by the pascal compiler then creates a copy of the 
        structure.
      *)
      PushAdr(ap);
    ELSE
      Mark(117);
    END;
  ELSE
    GenAssign(fp, ap);  (* type check in GenAssign *)
  END;

  Release(ap);
END GenParam;

TYPE
  aResultLocation = (resultOnStack, resultAnywhere);

PROCEDURE RestoreResult(VAR x : Item; loc: aResultLocation);
VAR
  y:        Item;
  z:        Item; 
  sz:       CARDINAL;
  realadr:  aAdr;
  realreg:  DRegister;
BEGIN
  WITH x DO
    IF ((mode = procMd) AND (proc^.procType = modula2)) OR
       (loc = resultOnStack) OR
       (mode = toolMd) THEN
      SetstkMd(x, x.typ);
    ELSE
      IF (typ <> realtyp) AND (typ <> lrltyp) THEN
        SetregMd(x, x.typ);
      ELSE
        GetReg(long, realreg);
        realadr := RegAdr(realreg);

        PutDPReference(STA+DIRECT, realadr, 2);
        PutDPReference(STX+DIRECT, realadr+2, 2);

        PutDPReference(LDY+IMMEDIATE3, 8, 3);
        PutDPReference(LDA+DIRINDBYYLG, realadr, 2);
        PutOp(PHA, 1);
        PutDPReference(LDY+IMMEDIATE3, 6, 3);
        PutDPReference(LDA+DIRINDBYYLG, realadr, 2);
        PutOp(PHA, 1);
        PutDPReference(LDY+IMMEDIATE3, 4, 3);
        PutDPReference(LDA+DIRINDBYYLG, realadr, 2);
        PutOp(PHA, 1);
        PutDPReference(LDY+IMMEDIATE3, 2, 3);
        PutDPReference(LDA+DIRINDBYYLG, realadr, 2);
        PutOp(PHA, 1);
        PutDPReference(LDY+IMMEDIATE3, 0, 3);
        PutDPReference(LDA+DIRINDBYYLG, realadr, 2);
        PutOp(PHA, 1);
        INC(stackPointer, ExtendedSize);
        SetfltMd(x, stackPointer, typ);

        Unlock(realreg);
      END;
    END;
  END;
END RestoreResult;

PROCEDURE GenCall(VAR x: Item; regs: DRegSet);
VAR
  pd:           PDPtr;
  y:            Item;
  z:            Item;
  stackBased:   BOOLEAN;
  resultLoc:    aResultLocation;
  saveStack:    aAdr;
BEGIN
  WITH x DO
    resultLoc := resultAnywhere;

    (*
      This Sucks!

      Various calls may lose this value, in their various manipulations.
      They do so by copying some other item variable into "x".  The correct
      solution would be to fix up all of the problem areas, but due to the
      rather late stage of the game (and the fact that I want some dinner),
      I am preserving the value here, doing the call that may or may not
      alter the field, and then restoring it.  Yucky, but a quickie!
    *)
    saveStack := stkPtr;

    IF mode = procMd THEN
      pd := proc^.pd;
      CallExt(proc);
      typ := proc^.typ;
    ELSIF (mode <= conMd) & (typ <> undftyp) THEN
      CallInd(x);
      typ := typ^.resTyp;
      resultLoc := resultOnStack;
    ELSIF mode = toolMd THEN
      IF proc^.GSOSproc THEN
        PutDPReference(PEA, proc^.cnum, 3);
        PutLongReference(JSL, 0E100B0H, 4);
        GenGlobalOp(STA+ABSLONG, '~TOOLERROR', 0, 0, 3);
        AddBytesToObjectFile(4);
      ELSE
        PutDPReference(LDX+IMMEDIATE3, proc^.cnum, 3);
        PutLongReference(JSL, 0E10000H, 4);
        GenGlobalOp(STA+ABSLONG, '~TOOLERROR', 0, 0, 3);
        AddBytesToObjectFile(4);
      END;

      typ := proc^.typ;
    END;

    stackPointer := saveStack;

    IF typ <> notyp THEN  (* function call *)
      RestoreResult(x, resultLoc);
    END;
  END (*WITH*);
END GenCall;

PROCEDURE ThisIs(name: CARDINAL; proc: ObjPtr): BOOLEAN;
(*
  Returns TRUE if the procedure is top level, AND if the procedure's name
  is the same as that specified by the "name" parameter.

  Used primarily for checking to see if a procedure is one of the special
  NDA or CDA procedures.

  The top level check is to ensure that if the programmer has a local
  or nested procedure with the same name as one of the special procedures,
  we don't generate any special code.
*)
BEGIN
  RETURN (Diff(name, proc^.name) = 0) AND (proc^.pd^.lev = 0);
END ThisIs;

PROCEDURE GenEnter(VAR l: aAdr; proc: ObjPtr; isImp: BOOLEAN);
VAR
  procName:   aSymbol;
  procName2:  aSymbol;
  modName:    aSymbol;
  globalName: aSymbol;
  Interface:  BOOLEAN;
  OK:         BOOLEAN;
  I:          CARDINAL;
  length:     CARDINAL;
  entry:      CARDINAL;
  expObj:     ObjPtr;
  multiName:  BOOLEAN;

  PROCEDURE SetupDataBank;
  BEGIN
    PutOp(PHB, 1);
    INC(stackPointer);
    GenGlobalOp(PEA, '~__Globals', 0, -8, 2);
    AddBytesToObjectFile(3);
    PutOp(PLB, 1);
    PutOp(PLB, 1);
  END SetupDataBank;

BEGIN
  GetProcedureName(proc, procName);

  NewSegmentHeader(procName);

  multiName := FALSE;

  IF curMod <> 0 THEN
    expObj := Find(proc^.name);

    IF (expObj <> NIL) AND
       (expObj^.class = Proc) AND
       (expObj^.pmod = 0) AND
       (expObj^.exported) THEN
      multiName := TRUE;

      IdToSymbol(proc^.name, procName2);
      Concat('_', procName2, procName2);
      GetModuleName(proc^.pmod, modName, OK);
      Concat(modName, procName2, procName2);

      PutByte(VAL(BYTE, 0E6H)); (* GLOBAL record *)
      PutLabelText(procName2);
      PutWord(0);
      PutByte(VAL(BYTE, "G"));  (* GEQU type entry *)
      PutByte(VAL(BYTE, 0));    (* private flag = 0 --> public *)
    END;
  END;

  length := Length(segmentName);

  IF length <> 0 THEN
    I := 0;

    WITH objFile.segmentHeader^ DO
      WHILE I <= HIGH(LOADNAME) DO
        IF I < length THEN
          LOADNAME[I] := segmentName[I];
        ELSE
          LOADNAME[I] := ' ';
        END;

        INC(I);
      END;
    END;
  END;

  IF NDA IN Directives THEN
    IF ThisIs(NDAAction, proc) THEN
      newLink(entry);
      PutOp(PHA, 1);  (* push the action code *)
      PutOp(PHY, 1);  (* push the event record pointer *)
      PutOp(PHX, 1);
      PutOp(PHK, 1);
      PutDPReference(PER, 1, 3);
      PutDPReference(BRA, entry, 2);
      PutOp(W65C816.RTL, 1);
      FixLink(entry);
    END;
  END;

  Link(l, proc^.pd^.lev);

  IF databank THEN
    SetupDataBank;
  ELSIF (CDA IN Directives) OR
     (NDA IN Directives) OR
     (CDEV IN Directives) THEN
    IF ThisIs(NDAOpen, proc) OR
       ThisIs(CDEVName, proc) OR
       ThisIs(NDAClose, proc) OR
       ThisIs(NDAAction, proc) OR
       ThisIs(CDAShutdown, proc) THEN
      SetupDataBank;
    END;
  END;

  IF ShowProcs THEN
    FOR I :=  1 TO curLev+3 DO
      Write(' ');
    END;
  
    WriteString(procName);

    IF multiName THEN
      WriteString(', ');
      WriteString(procName2);
    END;

    WriteLn;
  END;

  OutName(procName);
END GenEnter;

PROCEDURE GenResult(VAR x: Item; proc: ObjPtr; VAR l: CARDINAL);
VAR
  res:    Item;
  label:  aSymbol;
BEGIN
  IF x.typ <> notyp THEN (* function *)
    StoreResult(x, proc^.pd^.size + ParamStartAdr);
  END;

  GenFJ(l);
END GenResult;

PROCEDURE GenReturn(proc: ObjPtr; l: CARDINAL; isimp: BOOLEAN);
VAR
  modName:  aSymbol;
  procName: aSymbol;
  OK:       BOOLEAN;
  segAttr:  aSegmentAttributeSet;

  PROCEDURE RestoreDataBank;
  BEGIN
    PutOp(PLB, 1);
    DEC(stackPointer);
  END RestoreDataBank;

BEGIN
  segAttr := aSegmentAttributeSet{};

  IF Dynamic IN Directives THEN
    INCL(segAttr, atDynamic);
  END;

  IF proc^.class = Module THEN
    IF NOT isimp AND 
       NOT (NDA IN Directives) AND
       (proc^.modno = 0) THEN
      PutDPReference(LDA+IMMEDIATE, 0, 3);
      GenGlobalOp(STA+ABSLONG, 'M2Lib__Bootup', 0, 0, 3);
      AddBytesToObjectFile(4);
      InitModule(0);
      PutDPReference(LDA+IMMEDIATE, 1, 3);
      GenGlobalOp(STA+ABSLONG, 'M2Lib__Bootup', 0, 0, 3);
      AddBytesToObjectFile(4);
    END;

    IF DebugCode AND NOT isimp THEN
      GenEndProcedure;
    END;

    FixLink(l);

    Unlink(0, 0);

    PutOp(W65C816.RTL, 1);

    PutStringConstants(localStringList);
    clearStringList(localStringList);
  ELSE  (* Proc *)
    IF proc^.typ <> notyp THEN
      GenTerminate(tsNoReturnFromFunction);
    END;  (* function *)

    FixLink(l);

    IF DebugCode THEN
      GenEndProcedure;
    END;

    IF databank THEN
      RestoreDataBank;
    ELSIF (CDA IN Directives) OR
       (NDA IN Directives) OR
       (CDEV IN Directives) THEN
      IF ThisIs(NDAOpen, proc) OR
         ThisIs(CDEVName, proc) OR
         ThisIs(NDAClose, proc) OR
         ThisIs(NDAAction, proc) OR
         ThisIs(CDAShutdown, proc) THEN
        RestoreDataBank;
      END;

      IF ThisIs(NDAAction, proc) THEN
        GenGlobalOp(LDA+ABSLONG, 'M2Lib_StackFramePointer', 0, 0, 3);
        AddBytesToObjectFile(4);
        PutOp(TCS, 1);
        PutStackReference(LDA+STKRELATIVE, 14);
        GenGlobalOp(STA+ABSLONG, 'M2Lib_NDACode', 0, 0, 3);
        AddBytesToObjectFile(4);
      END;
    END;

    Unlink(proc^.pd^.size, proc^.pd^.lev);

    IF (NDA IN Directives) AND
       ThisIs(NDAAction, proc) THEN
      GenGlobalOp(LDA+ABSLONG, 'M2Lib_NDACode', 0, 0, 3);
      AddBytesToObjectFile(4);
    ELSIF (CDA IN Directives) AND
          ThisIs(CDAShutdown, proc) THEN
      (*
        Use the ORCA routine ~DAID to set up or initialise the CDA User ID.

        A zero flag value causes the User ID to be released.
      *)
      PutDPReference(PEA, 0, 3);
      GenGlobalOp(JSL, '~DAID', 0, 0, 3);
      AddBytesToObjectFile(4);
    END;

    PutOp(W65C816.RTL, 1);

    GetProcedureName(proc, procName);
    Concat(procName, "__OTP", procName);

    PutStringConstants(localStringList);
    clearStringList(localStringList);

    PutByte(VAL(BYTE, 0E7H)); (* GEQU record *)
    PutLabelText(procName);
    PutWord(0);
    PutByte(VAL(BYTE, "G"));

    IF NOT proc^.pd^.exp THEN
      INCL(segAttr, atPrivate);
      PutByte(VAL(BYTE, 1));                (* private flag = 1 --> private *)
    ELSE
      PutByte(VAL(BYTE, 0));                (* private flag = 0 --> public  *)
    END;

    PutByte(VAL(BYTE, 81H));                        (* constant operand     *)
    PutLong(VAL(LONGINT, proc^.pd^.accessLink));    (* value of offset      *)
    PutByte(VAL(BYTE, 00H));                        (* end of expression    *)
  END;

  EndSegment(skCode, segAttr);

  errorInProc := FALSE;
END GenReturn;

PROCEDURE GenCase1(VAR x: Item; VAR l0: CARDINAL);
BEGIN
  SRTest(x);

  IF (x.typ^.form = Undef) OR (x.typ^.form > Enum) THEN
    Mark(140);  (* illegal type of case expression *)
  END;

  PushVal(x);

  (*
    The pushed value is taken back off the stack before the actual case code
    gets to execute.
  *)
  DEC(stackPointer, x.typ^.size);

  newLink(l0);
  GenFJ(l0);
END GenCase1;

PROCEDURE GenCase2(done: CARDINAL);
VAR
  tmp: CARDINAL;
BEGIN
  Branch(T, done, tmp);

  ExitCase; (* does nothing *)
END GenCase2;

PROCEDURE GenCase3(VAR x: Item; l0, l1, n: CARDINAL;
                   VAR tab: ARRAY OF LabelRange);
VAR
  i:      CARDINAL;
  base:   CARDINAL;
  j:      INTEGER;
BEGIN
  newLink(base);
  FixLink(base);

  IF n > 0 THEN  (* not empty CASE statement *)
    i := 0;
    j := tab[0].low;

    WHILE i < n DO
      WHILE j < tab[i].low DO
        PutLinkAddress(l1, 2);
        INC(j);    (* ELSE entry *)
      END;

      WHILE j <= tab[i].high DO
        PutLinkAddress(tab[i].label, 2);
        INC(j);
      END;

      INC(i);
    END;

    PutLinkAddress(l1, 2);           (* ELSE entry *)
    FixLink(l0);
    EnterCase(x, base, tab[0].low, tab[n-1].high);
  ELSE
    PutLinkAddress(l1, 2);           (* ELSE entry *)
    FixLink(l0);
    EnterCase(x, base, 1, 0);  (* empty CASE statement *)
  END;
END GenCase3;

PROCEDURE GenFor1(v: Item; VAR e1: Item);
BEGIN
  SRTest(v);
  IF (v.typ^.form = Undef) OR (v.typ^.form > Enum) OR v.isParam THEN
    Mark(142);  (* illegal type of control variable *)
  END;
END GenFor1;

PROCEDURE GenFor2(v: Item; VAR e1, e2: Item);
  VAR w: Item;
BEGIN
  SRTest(e2);
  IF e2.typ = cardinttyp THEN
    e2.typ := v.typ;
  END;

  IF (v.typ = e2.typ) OR (v.typ = inttyp) & (e2.typ = cardtyp) &
     (e2.mode = conMd) & (e2.val.C <= MaxInt) THEN
    IF e2.mode <> conMd THEN
      LoadD(e2);
    END;
  ELSE
    Mark(117);  (* incompatible operand types *)
  END;
END GenFor2;

PROCEDURE GenFor3(v: Item; VAR e2, e3: Item; VAR l0, l1: CARDINAL);
VAR
  f:      StrForm;
  c:      Condition;
  tmp:    CARDINAL;
  wv:     INTEGER;
  z:      Item;
BEGIN
  SRTest(v);
  tmp := 0;
  wv := WordVal(e3);
  f := v.typ^.form;
  newLink(l0);  (* l0 is the start of loop label *)

  IF e2.mode <> conMd THEN
    PushVal(e2);
    e2.notpop := TRUE;
  END;

  (*
    For steps that are greater than 1, we need to do the actual check BEFORE the
    iteration, not after.
  *)
  IF ABS(wv) <> 1 THEN
    FixLink(l0);
  END;

  z := e2;
  Cmp2(v, z);
  Release(v);

  IF wv > 0 THEN
    IF f = Int THEN
      c := GT;
    ELSE
      c := HI;
    END;
  ELSIF wv < 0 THEN
    IF f = Int THEN
      c := LT;
    ELSE
      c := CC;
    END;
  ELSE
    Mark(141);  (* step in FOR clause cannot be 0 *)
    c := GT;
  END;

  newLink(l1);   (* l1 is the label placed just after the for loop *)

  (*
    When the stepping value is 1, we check the control value after the block.
  *)
  IF ABS(wv) = 1 THEN
    Branch(c, l1, l0);
    FixLink(l0);
  ELSE
    Branch(c, l1, tmp);
    FixLink(tmp);
  END;
END GenFor3;

PROCEDURE GenFor4(v: Item; VAR e2, e3: Item; l0, l1: CARDINAL);
VAR
  c:    Condition;
  f:    StrForm;
  chk:  BOOLEAN;
  tmp:  CARDINAL;
  z:    Item;
  y:    Item;
  wv:   INTEGER;
BEGIN
  SRTest(v);
  f := v.typ^.form;
  wv := WordVal(e3);

  IF ABS(wv) = 1 THEN
    tmp := 0;
    z := e2;
    y := v;
    Cmp2(y, z);

    Branch(EQ, l1, tmp);
    FixLink(tmp);
    Release(z);
    Release(y);
  END;

  tmp := 0;
  chk := ovflchk;
  ovflchk := FALSE;
  z := v;
  y := e3;

  IF wv = 1 THEN
    Inc1(z);
  ELSIF wv = -1 THEN
    Dec1(z);
  ELSE
    IF z.typ^.size = VAL(aSize, 1) THEN
      LoadX(z, word);
      z.typ := cardtyp;
    END;

    Add2(z, y);
  END;

  ovflchk := chk;
  Release(v);
  Move(z, v);

  IF v.typ^.form = Int THEN
    IF ABS(wv) = 1 THEN
      IF wv > 0 THEN
        PutDPReference(CMP+IMMEDIATE, 8000H, 3);
        c := NE;
      ELSE
        PutDPReference(CMP+IMMEDIATE, 7FFFH, 3);
        c := NE;
      END;
    ELSE
      c := VC;
    END;
  ELSIF wv > 0 THEN
    IF wv = 1 THEN
      c := NE;
    ELSE
      c := CC;
    END;
  ELSE
    IF wv = -1 THEN
      PutDPReference(CMP+IMMEDIATE, 0FFFFH, 3);
      c := NE;
    ELSE
      c := CS;
    END;
  END;

  Branch(c, l0, tmp);

  IF c <> T THEN
    FixLink(tmp);
  END;

  IF ovflchk THEN
    GenTerminate(tsOverflow);
  END;

  FixLink(l1);

  IF e2.mode <> conMd THEN
    IF e2.notpop THEN
      StackTop(e2.typ^.size);
    END;

    Release(e2);
  END;

  Release(y);
END GenFor4;

PROCEDURE GenLoop1(VAR m, n: CARDINAL);
BEGIN
  m := n;
  newLink(n);
END GenLoop1;

PROCEDURE GenLoop2(m: CARDINAL; VAR n: CARDINAL);
BEGIN
  FixLink(n);
  n := m;
END GenLoop2;

PROCEDURE GenExit(VAR n: CARDINAL);
BEGIN
  GenFJ(n);
END GenExit;

PROCEDURE GenEnterMod(mno, pno: CARDINAL; mainModule: BOOLEAN);
BEGIN
  maxM := mno;

  IF mainModule THEN
    EnterModule;
  END;
END GenEnterMod;

PROCEDURE GenExitMod;
BEGIN
  ExitModule;
END GenExitMod;

PROCEDURE GenInitMod(VAR endOfInit: CARDINAL;
                         isimp:     BOOLEAN;
                         segName:   aSymbol;
                         mod:       ObjPtr);
VAR
  i:          CARDINAL;
  flag:       CARDINAL;
  ok:         CARDINAL;
  chkFlag:    CARDINAL;
  initOthers: CARDINAL;
  initThis:   CARDINAL;
  clrFlag:    CARDINAL;
  curMod:     pModListEntry;

  PROCEDURE InitSubMods;
  BEGIN
    WHILE curMod <> NIL DO
      WITH curMod^ DO
        IF (module^.class = Module) THEN
          IF (module^.parentMod = mod^.modno) THEN
            InitModule(module^.modno);
          END;
        END;
      END;

      curMod := curMod^.next;
    END;
  END InitSubMods;

BEGIN
  (*
    It is necessary to prevent an initialisation procedure from executing
    more than once.  It is called once by every module that imports it, so
    we must place a mechanism to prevent multiple execution, and possible
    circular execution.

    The mechanism will be:

              LDA >flag       ; Where 'flag' is set to Zero at compile time
              BEQ ok
              BRA endOfInit   ; This is a label that must be fixed later.
      flag    DC  H'00 00'    ; The flag itself.
      ok      LDA #$0001      ; Set the flag to non-zero when the procedure is
              STA >flag       ; first called.

    For all programs it is also necessary to provide a mechanism for zeroing
    the flag when the program terminates.  This is achieved by inserting the
    following code before the previous mechanism:

              LDA >M2Lib__Bootup
              BNE chkFlag
              LDA >flag
              BNE clrFlag
              BRL endOfInit
      clrFlag LDA #$0000
              STA >flag
              BRA initOthers
      chkFlag ANOP

    By zeroing M2Lib__Bootup, and calling the main module's Initialisation
    procedure, we succeed in zeroing out all of the flags, in preparation
    for the next execution.

    For CDA's this is vital, since a CDA's initialisation code is executed
    each time the CDA is invoked.

    For normal programs, it is also necessary, as GSOS allows a program to
    remain resident in memory.  If we want to support this with M2, then
    we must leave a program in a state that will allow it to run, without
    having to re-load from disk.

    For an NDA, the Initialisation code is used as the Init Procedure, and as
    such, the body of the initialisation code must be executed at shutdown as
    well.
  *)

  newLink(ok);
  newLink(flag);
  newLink(endOfInit);
  newLink(clrFlag);
  newLink(chkFlag);
  newLink(initOthers);

  GenGlobalOp(LDA+ABSLONG, 'M2Lib__Bootup', 0, 0, 3);
  AddBytesToObjectFile(4);

  PutLinkReference(BNE, chkFlag, 2);
  PutLinkReference(LDA+ABSLONG, flag, 4);
  PutLinkReference(BNE, clrFlag, 2);
  PutLinkReference(BRL, endOfInit, 3);
  FixLink(clrFlag);
  PutDPReference(LDA+IMMEDIATE, 0, 3);
  PutLinkReference(STA+ABSLONG, flag, 4);
  PutLinkReference(BRL, initOthers, 3);
  FixLink(chkFlag);

  PutLinkReference(LDA+ABSLONG, flag, 4);
  PutLinkReference(BEQ, ok, 2);
  PutLinkReference(BRL, endOfInit, 3);
  FixLink(flag);
  PutWordConstant(0);
  AddBytesToObjectFile(2);
  FixLink(ok);
  PutDPReference(LDA+IMMEDIATE, 1, 3);
  PutLinkReference(STA+ABSLONG, flag, 4);

  IF DebugCode THEN
    GenDebugSymbols(mod);
    GenStartProcedure(segName);
  END;

  FixLink(initOthers);

  curMod := modList;

  InitSubMods;

  IF NOT (NDA IN Directives) THEN
    newLink(initThis);
    GenGlobalOp(LDA+ABSLONG, 'M2Lib__Bootup', 0, 0, 3);
    AddBytesToObjectFile(4);
    PutLinkReference(BNE, initThis, 2);
    PutLinkReference(BRL, endOfInit, 3);
    FixLink(initThis);
  END;
END GenInitMod;

END M2CM.
