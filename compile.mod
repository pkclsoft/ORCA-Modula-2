(*$Segment Compile*)
(*$RangeCheck+*)
(*$OverflowCheck+*)
(*$StackSize 4000H*)
MODULE Compile;    (* NW 6.3.83 / 24.12.85; WH 30.9.87; HS 28.4.89 *)

FROM ASCII IMPORT nul, bs;
FROM ConsoleIO IMPORT scClearEOL;
FROM EZFileSystem IMPORT MakeFileName;
FROM GSOSInterface IMPORT GSOSNameString;
FROM InOut IMPORT WriteCard, WriteLongInt, Write, WriteHex, WriteString,
     WriteLn, OpenErrorOutput, CloseErrorOutput;
FROM M2Clock IMPORT KeyTime, GetKeyTime;
FROM M2CM IMPORT
     LabelRange, ExitTable, curPrio, GenAssign, GenFJ, GenCFJ, GenBJ, GenCBJ,
     PrepCall, GenParam, GenCall, GenEnter, GenResult, GenReturn, curMod,
     GenCase1, GenCase2, GenCase3, GenFor1, GenFor2, GenFor3, GenFor4,
     GenLoop1, GenLoop2, GenExit, GenEnterMod, GenExitMod, GenInitMod;
FROM M2Debug IMPORT
     GenStartProcedure, GenLoadSource, GenDebugSymbols;
FROM M2DM IMPORT
     WordSize, MaxInt, Standard, rngchk, ovflchk, stackchk, nilchk, aAdr,
     inttyp, cardinttyp, cardtyp, realtyp, chartyp, bitstyp, dbltyp, notyp,
     stringtyp, lrltyp, addrtyp, undftyp, mainmod, sysmod, WorkingStorage,
     ObjPtr, StrPtr, ParPtr, ConstValue, StrForm, ObjClass, InitM2DM,
     Available, CloseHeaps, CloseMainHeap, language, databank;
FROM M2EM IMPORT
     ParamStartAdr, wlev, AllocVar, AllocPar, AllocFld,
     GenItem, GenIndex, GenField, GenDeRef, GenNeg, GenNot, GenAnd,
     GenOr, GenSingSet, GenSet, GenIn, GenOp, GenWith, GenWith2,
     GenStParam, GenStFct, InitM2EM;
FROM M2HM IMPORT
     DynArrDesSize, ItemMode, Item, curLev, maxLev, SetRegs,
     WordVal, SetstkMd, SetconMd, DRegSet, SaveRegs, GenTerminate, Link,
     ConvertTyp, GenHalt, Processor, ProcessorID, InitM2HM, CopyDynArray,
     LoadVal, DRegister, LongVal;
FROM M2Lib IMPORT
     aTerminateStatus;
FROM M2LM IMPORT
     pc, InitOMF, CloseObjectFile, AllocString, FixLink, AllocGlobalVar,
     CodeOverflow, aSymbol, newLink, PutGlobals, GetModuleNameByPtr, 
     localStringList, globalStringList, objectDataSize, IdToSymbol,
     objectCodeSize, InitM2LM, GetProcedureName, GetModuleKey, AllocBounds;
FROM M2OMF IMPORT NewSegmentHeader, PutByte, PutLabelText;
FROM M2RM IMPORT
     ModNo, ModList, InitM2RM, SymAuxType, RefAuxType,
     InitRef, InRef, OpenRef, RefPoint, OutUnit, CloseRef;
FROM M2Shell IMPORT InitM2Shell, GetFileName, ShutdownM2Shell, ShowProgress,
     AllErrFatal, outFileMask, keepStyle, OpenNextObjectFile, DebugCode,
     objectFileInError, userHasAborted, currentObjectName, DisplayKey,
     terminalError;
FROM M2SM IMPORT
     Symbol, sym, id, numtyp, intval, dblval, realval, lrlval, SourceOpened,
     IdBuf, scanerr, InitScanner, GetSym, Diff, KeepId, Mark, CloseScanner,
     AddModule, InitM2SM, moduleDeclared, isint, Directives, source, aDirective,
     errorCount;
FROM M2TM IMPORT
     topScope, Scope, NewObj, NewStr, NewPar, NewImp, InitM2TM,
     NewScope, CloseScope, Find, FindImport, FindInScope, CheckUDP,
     MarkHeap, ReleaseHeap, InitTableHandler;
FROM OrcaShell IMPORT ConsoleOutDCB, ConsoleOut;
FROM Strings IMPORT Concat;
FROM SYSTEM IMPORT BYTE;

IMPORT EZDump;

(* sym, id, numtyp, intval, dblval, realval, lrlval
   are implicit results of GetSym ! *)

CONST NofCases     = 128;
      NofExits      = 16;
      LoopLevels     = 4;
      EnumTypSize    = 1;
      PointerTypSize = 4;
      ProcTypSize    = 4;
      MaxSetElements = 256;
      BitsPerByte    = 8;
      ESC = 3C;
      MaxArraySize   = 65536;

VAR
  ch:             CHAR;
  pno:            CARDINAL;
  isdef:          BOOLEAN;
  isimp:          BOOLEAN;
  FileName:       GSOSNameString;
  errorsOccured:  BOOLEAN;
  spinner:        CHAR;

PROCEDURE Type(VAR typ: StrPtr); FORWARD;
PROCEDURE Expression(VAR x: Item); FORWARD;
PROCEDURE Block(ancestor: ObjPtr; qual: BOOLEAN;
                VAR adr: aAdr; VAR L0: CARDINAL); FORWARD;

PROCEDURE UpdateSpinner;
VAR
  CO: ConsoleOutDCB;
BEGIN
  CO.pCount := 1;
  CO.ch := spinner;
  ConsoleOut(CO);

  IF spinner >= ' ' THEN
    CO.ch := bs;
    ConsoleOut(CO);
  END;

  IF spinner = '|' THEN
    spinner := '/';
  ELSIF spinner = '/' THEN
    spinner := '-';
  ELSIF spinner = '-' THEN
    spinner := '\';
  ELSIF spinner = '\' THEN
    spinner := '|';
  END;
END UpdateSpinner;

PROCEDURE CheckSym(s: Symbol; error: CARDINAL);
(*
  If the current symbol matches that specified, then get the next symbol,
  otherwise report the specified error.
*)
BEGIN
  IF sym = s THEN
    GetSym;
  ELSE
    Mark(error);
  END;
END CheckSym;

PROCEDURE qualident(VAR obj: ObjPtr);
(*
  determine the full specification of the object, by chasing down module
  chains.

  an object may be an identifier declared in an external module, so this
  routine is used to find it.
*)
BEGIN (*sym = ident*)
  obj := Find(id);
  GetSym;

  WHILE (sym = period) AND
        (obj # NIL) AND
        (obj^.class = Module) DO
    IF userHasAborted OR
       terminalError THEN
      RETURN;
    END;

    GetSym;

    IF sym = ident THEN
      obj := FindInScope(id, obj^.root);
      GetSym;

      IF (obj # NIL) & NOT obj^.exported THEN
        obj := NIL;
      END;
    ELSE
      Mark(10);
    END;
  END;
END qualident;

PROCEDURE GenVal(VAR x: Item);                                       (* V2.6 *)
BEGIN
  IF x.mode = cocMd THEN
    LoadVal(x, erCPU);
  END;                               (* V2.6 *)
END GenVal;                                                          (* V2.6 *)

PROCEDURE ConstExpression(VAR x: Item);
BEGIN
  Expression(x);

  IF x.mode # conMd THEN
    SetconMd(x, 1, cardtyp); Mark(44);  (* constant expression expected *)
  END
END ConstExpression;

PROCEDURE CheckComp(t0, t1: StrPtr);
BEGIN
  IF (t0 # t1) &
     ((t0 # inttyp) OR (t1 # cardtyp)) &
     ((t0 # cardinttyp) OR (t1 # cardtyp)) THEN Mark(61) END
END CheckComp;

PROCEDURE CaseLabelList(    Ltyp:         StrPtr;
                        VAR n:            CARDINAL;
                        VAR tab:          ARRAY OF LabelRange;
                            isCode:       BOOLEAN);
(*
  NOTE that this procedure is used for processing variant types AND CASE
  statements.  To differentiate, "isCode" is TRUE if this procedure is being
  called while processing a CASE statement.  It is FALSE if processing a
  variant record.
*)
VAR
  x,y:      Item;
  i:        CARDINAL;
  f:        StrForm;
BEGIN
  f := Ltyp^.form;

  IF f = Range THEN
    Ltyp := Ltyp^.RBaseTyp;
  ELSIF f > Enum THEN
    Mark(83);
  END;

  LOOP
    IF userHasAborted OR
       terminalError THEN
      RETURN;
    END;

    ConstExpression(x);
    CheckComp(Ltyp, x.typ);

    IF sym = ellipsis THEN
      GetSym;
      ConstExpression(y);
      CheckComp(Ltyp, y.typ);

      IF WordVal(x) > WordVal(y) THEN
        Mark(63);
        y := x;
      END;
    ELSE
      y := x;
    END ;

    (*enter label range into ordered table*)
    i := n;

    IF i < NofCases THEN
      LOOP
        IF i = 0 THEN
          EXIT
        END;

        IF tab[i-1].low <= VAL(INTEGER, WordVal(y)) THEN
          IF tab[i-1].high >= VAL(INTEGER, WordVal(x)) THEN
            Mark(62);
          END;

          EXIT
        END;

        tab[i] := tab[i-1];
        DEC(i);
      END;

      WITH tab[i] DO
        low := WordVal(x);
        high := WordVal(y);

        IF NOT isdef AND isCode THEN
          newLink(label);
          FixLink(label);
        END;
      END;

      INC(n);
    ELSE
      Mark(92);
    END;

    IF sym = comma THEN
      GetSym;
    ELSIF (sym = number) OR (sym = ident) THEN
      Mark(11);
    ELSE
      EXIT;
    END;
  END;
END CaseLabelList;

PROCEDURE Subrange(VAR typ: StrPtr);
VAR
  x, y: Item;
  f, g: StrForm;
BEGIN
  typ := NewStr(Range);
  ConstExpression(x);

  f := x.typ^.form;

  IF f <= Enum THEN
    typ^.min := LongVal(x);
  ELSE
    Mark(82);
  END;

  CheckSym(ellipsis, 21);
  ConstExpression(y);

  g := y.typ^.form;
  CheckComp(x.typ, y.typ);

  WITH typ^ DO
    max := LongVal(y);

    IF min > max THEN
      Mark(63);
      min := max;
    END;

    RBaseTyp := x.typ;
    size := x.typ^.size;

    IF rngchk THEN
      AllocBounds(min, max, size, BndAdr);
    END;
  END;
END Subrange;

PROCEDURE SimpleType(VAR typ: StrPtr);
VAR
  obj, last:  ObjPtr;
  typ0:       StrPtr;
  n:          CARDINAL;
BEGIN
  typ := undftyp;

  IF sym = ident THEN
    qualident(obj);

    IF (obj # NIL) & (obj^.class = Typ) THEN
      typ := obj^.typ;
    ELSE
      Mark(52);
    END;

    IF sym = lbrak THEN
      IF typ^.form = Range THEN
        typ := typ^.RBaseTyp;
      END;            (* V2.6 *)

      GetSym;
      typ0 := typ;
      Subrange(typ);

      IF typ^.RBaseTyp # typ0 THEN
        IF (typ0 = inttyp) & (typ^.RBaseTyp = cardtyp) THEN
          typ^.RBaseTyp := inttyp;
        ELSE
          Mark(61);
        END;
      END;

      IF sym = rbrak THEN
        GetSym;
      ELSE
        Mark(16);

        IF sym = rparen THEN
          GetSym;
        END;
      END;
    END;
  ELSIF sym = lparen THEN
    GetSym;
    typ := NewStr(Enum);
    last := NIL;
    n := 0;

    LOOP
      IF userHasAborted OR
         terminalError THEN
        RETURN;
      END;

      IF sym = ident THEN
        obj := NewObj(id, Const);
        KeepId;
        obj^.conval.Ch := VAL(CHAR, n);                             (* V2.6 *)

        IF n > 255 THEN
          Mark(300);
        END;                               (* V2.6 *)

        obj^.conval.prev := last;
        obj^.typ := typ;
        last := obj;
        INC(n);
        GetSym;
      ELSE
        Mark(10);
      END;

      IF sym = comma THEN
        GetSym;
      ELSIF sym = ident THEN
        Mark(11);
      ELSE
        EXIT;
      END;
    END;

    WITH typ^ DO
      ConstLink := last;
      NofConst := n;
      size := EnumTypSize;
    END;

    CheckSym(rparen, 15);
  ELSIF sym = lbrak THEN
    GetSym;
    Subrange(typ);

    IF sym = rbrak THEN
      GetSym;
    ELSE
      Mark(16);

      IF sym = rparen THEN
        GetSym;
      END;
    END;
  ELSE
    Mark(32);
  END;
END SimpleType;

PROCEDURE FieldListSequence(VAR maxadr: aAdr; adr: aAdr);
VAR
  fld1, last, tagfldtyp:  ObjPtr;
  typ:                    StrPtr;

  PROCEDURE VariantPart;
    (*variables of Fieldlist used: maxadr, adr*)
  VAR
    lastadr:  aAdr;
    N:        CARDINAL;
    tab:      ARRAY [0..NofCases-1] OF LabelRange;
  BEGIN
    maxadr := adr;
    N := 0;

    LOOP
      IF userHasAborted OR
         terminalError THEN
        RETURN;
      END;

      IF sym < bar THEN
        CaseLabelList(typ, N, tab, FALSE);
        CheckSym(colon, 13);
        FieldListSequence(lastadr, adr);

        IF lastadr > maxadr THEN
          maxadr := lastadr;
        END;
      END;

      IF sym = bar THEN
        GetSym;
      ELSE
        EXIT;
      END;
    END;

    IF sym = else THEN
      GetSym;
      FieldListSequence(lastadr, adr);

      IF lastadr > maxadr THEN
        maxadr := lastadr;
      END;
    END;
  END VariantPart;

BEGIN
  typ := undftyp;

  IF (sym = ident) OR (sym = case) THEN
    LOOP
      IF userHasAborted OR
         terminalError THEN
        RETURN;
      END;

      IF sym = ident THEN
        last := topScope^.last;

        LOOP
          IF userHasAborted OR
             terminalError THEN
            RETURN;
          END;

          IF sym = ident THEN
            fld1 := NewObj(id, Field);
            KeepId;
            GetSym;
          ELSE
            Mark(10);
          END;

          IF sym = comma THEN
            GetSym;
          ELSIF sym = ident THEN
            Mark(11);
          ELSE
            EXIT;
          END;
        END;

        CheckSym(colon, 13);
        Type(typ);
        fld1 := last^.next;

        WHILE fld1 # NIL DO
          fld1^.typ := typ;
          AllocFld(fld1, adr);
          fld1 := fld1^.next
        END;
      ELSIF sym = case THEN
        GetSym;
        fld1 := NIL;
        tagfldtyp := NIL;

        IF sym = ident THEN
          fld1 := NewObj(id, Field);
          KeepId;
          GetSym;
        END;

        CheckSym(colon, 13);

        IF sym = ident THEN
          qualident(tagfldtyp);
        ELSE
          Mark(10);
        END;

        IF (tagfldtyp # NIL) & (tagfldtyp^.class = Typ) THEN
          typ := tagfldtyp^.typ;
        ELSE
          Mark(52);
        END;

        IF fld1 # NIL THEN
          fld1^.typ := typ;
          AllocFld(fld1, adr);
        END;

        CheckSym(of, 23);
        VariantPart;
        adr := maxadr;
        CheckSym(end, 20)
      END;

      IF sym = semicolon THEN
        GetSym;
      ELSIF sym = ident THEN
        Mark(12);
      ELSE
        EXIT;
      END;
    END;
  END;

  maxadr := adr;
END FieldListSequence;

PROCEDURE FormalType(VAR typ: StrPtr);
VAR
  objtyp: ObjPtr;
BEGIN
  typ := undftyp;

  IF sym = array THEN
    GetSym;
    typ := NewStr(Array);

    WITH typ^ DO
      strobj := NIL;
      size := DynArrDesSize;
      dyn := TRUE;
    END;

    CheckSym(of, 23);

    IF sym = ident THEN
      qualident(objtyp);

      IF (objtyp # NIL) & (objtyp^.class = Typ) THEN
        typ^.ElemTyp := objtyp^.typ;
      ELSE
        Mark(52);
      END;
    ELSE
      Mark(10);
    END;
  ELSIF sym = ident THEN
    qualident(objtyp);

    IF (objtyp # NIL) & (objtyp^.class = Typ) THEN
      typ := objtyp^.typ;
    ELSE
      typ := undftyp;
      Mark(52);
    END;
  ELSE
    Mark(10);
  END;
END FormalType;

PROCEDURE FormalTypeList(proctyp: StrPtr);
VAR
  obj:              ObjPtr;
  par, par0, par1:  ParPtr;
  isvar:            BOOLEAN;
BEGIN par := NIL;
  IF (sym = ident) OR (sym = var) OR (sym = array) THEN
    LOOP
      IF userHasAborted OR
         terminalError THEN
        RETURN;
      END;

      IF sym = var THEN GetSym; isvar := TRUE ELSE isvar := FALSE END ;
      par := NewPar(0, isvar, par); FormalType(par^.typ);
      IF sym = comma THEN GetSym
      ELSIF sym = ident THEN Mark(11)
      ELSE EXIT
      END
    END
  END ;
  CheckSym(rparen, 15); (*reverse liest*)
  par1 := NIL; (*reverse list*)
  WHILE par # NIL DO
    par0 := par; par := par0^.next; par0^.next := par1; par1 := par0
  END ;
  proctyp^.firstPar := par1;
  IF sym = colon THEN
    GetSym; proctyp^.resTyp := undftyp;
    IF sym = ident THEN qualident(obj);
      IF (obj # NIL) & (obj^.class = Typ) THEN proctyp^.resTyp := obj^.typ
        ELSE Mark(52)
      END
    ELSE Mark(10)
    END
  ELSE proctyp^.resTyp := notyp
  END
END FormalTypeList;

PROCEDURE ArrayType(VAR typ: StrPtr);
VAR
  a,b:  LONGINT;
BEGIN
  typ := NewStr(Array);
  typ^.dyn := FALSE;

  SimpleType(typ^.IndexTyp);

  WITH typ^.IndexTyp^ DO
    IF form = Range THEN
      a := min;
      b := max;
    ELSIF form = Enum THEN
      a := 0;
      b := NofConst-1;
    ELSE
      Mark(94);
      form := Range;
      RBaseTyp := inttyp;
      min := 0;
      max := 0
    END;
  END;

  IF sym = of THEN
    GetSym;
    Type(typ^.ElemTyp);
  ELSIF sym = comma THEN
    GetSym;
    ArrayType(typ^.ElemTyp);
  ELSE
    Mark(23);
  END;

  IF b >= VAL(LONGINT, 0) THEN
    IF b - VAL(LONGINT, MAX(CARDINAL)) >= a THEN
      Mark(212);
      a := b;
    END;
  ELSIF a < VAL(LONGINT, 0) THEN
    IF b >= a + VAL(LONGINT, MAX(CARDINAL)) THEN
      Mark(212);
      a := b;
    END;
  END;

  a := b - a + VAL(LONGINT, 1);
  b := typ^.ElemTyp^.size;

  IF (b = VAL(LONGINT, 0)) OR (MaxArraySize DIV b >= a) THEN
    a := a * b;
  ELSE
    Mark(210);
    a := 4;
  END;

(*
  typ^.size := VAL(INTEGER, -a MOD VAL(LONGINT, 2)) + VAL(INTEGER, a); (*%*)
*)
  IF ABS(a) > MaxArraySize THEN
    Mark(210);
    typ^.size := 4;
  ELSE
    typ^.size := ABS(a);
  END;
END ArrayType;

PROCEDURE Type(VAR typ: StrPtr);
VAR
  obj:      ObjPtr;
  btyp:     StrPtr;
  numEle:   CARDINAL;
  typSize:  INTEGER;
BEGIN
  IF sym < lparen THEN Mark(33);
    REPEAT
      IF userHasAborted OR
         terminalError THEN
        RETURN;
      END;

      GetSym;
    UNTIL sym >= lparen;
  END;

  IF sym = array THEN
    GetSym; ArrayType(typ)
  ELSIF sym = record THEN
    GetSym; typ := NewStr(Record); NewScope(Typ);
    FieldListSequence(typSize, 0);
    typ^.size := typSize;    
    typ^.firstFld := topScope^.next;
    CheckSym(end, 20); CloseScope
  ELSIF sym = set THEN
    GetSym;
    CheckSym(of, 23);
    typ := NewStr(Set);
    SimpleType(typ^.SBaseTyp);
    btyp := typ^.SBaseTyp;

    IF btyp^.form = Enum THEN
      numEle := btyp^.NofConst;

      IF numEle > MaxSetElements THEN 
        Mark(209);
      END
    ELSIF btyp^.form = Range THEN
      numEle := btyp^.max - btyp^.min + 1;

      IF (btyp^.min < 0) OR (numEle > MaxSetElements) THEN 
        Mark(209);
      END;
    ELSIF btyp^.form = Char THEN
      numEle := 256;
    ELSE 
      numEle := BitsPerByte;
      Mark(60);
    END;

    typ^.size := numEle DIV BitsPerByte;

    IF numEle MOD BitsPerByte <> 0 THEN
      INC(typ^.size);
    END; 
  ELSIF sym = pointer THEN
    GetSym; typ := NewStr(Pointer);
    typ^.BaseId := 0; typ^.size := PointerTypSize; CheckSym(to, 24);
    IF sym = ident THEN qualident(obj);
      IF obj = NIL THEN typ^.BaseId := id; KeepId (*forward ref*)
      ELSIF obj^.class = Typ THEN typ^.PBaseTyp := obj^.typ
      ELSIF obj^.class = Temp THEN
        typ^.BaseId := id; KeepId; (* forward ref to exported type *)
      ELSE Mark(52)
      END
    ELSE Type(typ^.PBaseTyp)
    END
  ELSIF sym = procedure THEN
    GetSym; typ := NewStr(ProcTyp); typ^.size := ProcTypSize;
    IF sym = lparen THEN
      GetSym; FormalTypeList(typ)
    ELSE typ^.resTyp := notyp;
    END
  ELSE
    SimpleType(typ)
  END ;
  IF (sym < semicolon) OR (else < sym) THEN Mark(34);
    WHILE (sym < ident) OR (else < sym) AND (sym < begin) DO
      IF userHasAborted OR
         terminalError THEN
        RETURN;
      END;

      GetSym
    END
  END
END Type;

PROCEDURE selector(VAR x: Item; obj: ObjPtr);
(*
  Selects the specified part of "obj", eg:

    o If "obj" is indexed (ie using [expression]), select the specified
      element of "obj".
    o If "obj" is a record (indicated by a period), then select the specified
      field of the record "obj".
    o If "obj" is a pointer that is being dereferenced (using the ^), then
      select the data it is pointing to.
*)
VAR
  y: Item;
BEGIN
  GenItem(x, obj, Scope);

  LOOP
    IF userHasAborted OR
       terminalError THEN
      RETURN;
    END;

    IF sym = lbrak THEN
      GetSym;

      LOOP
        IF userHasAborted OR
           terminalError THEN
          RETURN;
        END;

        Expression(y);
        GenIndex(x, y);

        IF sym = comma THEN
          GetSym;
        ELSE
          EXIT;
        END;
      END;

      CheckSym(rbrak, 16);
    ELSIF sym = period THEN
      GetSym;

      IF sym = ident THEN
        IF (x.typ # NIL) & (x.typ^.form = Record) THEN
          obj := FindInScope(id, x.typ^.firstFld);
          GenField(x, obj);
        ELSE
          Mark(57);
        END;

        GetSym;
      ELSE
        Mark(10);
      END;
    ELSIF sym = arrow THEN
      GetSym;
      GenDeRef(x);
    ELSE
      EXIT;
    END;
  END;
END selector;

PROCEDURE ActualParameters(VAR x: Item; fpar: ParPtr);
VAR
  apar: Item;
BEGIN
  IF sym # rparen THEN
    LOOP
      IF userHasAborted OR
         terminalError THEN
        RETURN;
      END;

      Expression(apar);

      IF fpar # NIL THEN
        GenParam(x, apar, fpar);
        fpar := fpar^.next;
      ELSE
        Mark(64);
      END;

      IF sym = comma THEN
        GetSym;
      ELSIF (lparen <= sym) & (sym <= ident) THEN
        Mark(11);
        GetSym;
      ELSE
        EXIT;
      END
    END
  END;

  IF fpar # NIL THEN
    Mark(65);
  END;
END ActualParameters;

PROCEDURE StandProcCall(VAR p: Item);
VAR
  x: Item;
  m: Standard;
  n: CARDINAL;
BEGIN m := p.proc^.std; n := 0;
  IF m = Halt THEN
    GenHalt(0); (*HALT*)
  ELSE
    CheckSym(lparen, 22);

    LOOP
      IF userHasAborted OR
         terminalError THEN
        RETURN;
      END;

      Expression(x);
      GenVal(x);                                   (* V2.6 *)
      GenStParam(p, x, m, n, sym = comma);
      INC(n);

      IF sym = comma THEN
        GetSym;
      ELSIF sym # ident THEN
        EXIT;
      END;
    END;

    CheckSym(rparen, 15);
    GenStFct(m, n);
  END;
END StandProcCall;

PROCEDURE Element(VAR x: Item);
VAR
  e1, e2: Item;
BEGIN 
  ConstExpression(e1); 
  GenVal(e1);                                    (* V2.6 *)

  IF sym = ellipsis THEN
    GetSym; 
    ConstExpression(e2); 
    GenVal(e2);                              (* V2.6 *)
    GenSet(x, e1, e2);
  ELSE 
    GenSingSet(x, e1);
  END;
END Element;

PROCEDURE Sets(VAR x: Item; styp: StrPtr);
VAR
  y: Item;
BEGIN 
  x.typ := styp; 
  y.typ := styp;

  IF sym # rbrace THEN
    Element(x);

    LOOP
      IF userHasAborted OR
         terminalError THEN
        RETURN;
      END;

      IF sym = comma THEN 
        GetSym;
      ELSIF (lparen <= sym) & (sym <= ident) THEN 
        Mark(11);
      ELSE 
        EXIT;
      END;

      Element(y); 
      GenOp(plus, x, y);
    END;
  ELSE 
    SetconMd(x, 0, styp);
  END;

  CheckSym(rbrace, 17);
END Sets;

PROCEDURE Factor(VAR x: Item);
VAR
  obj:  ObjPtr;
  xt:   StrPtr;
  fpar: ParPtr;
  savedRegs: DRegSet;
BEGIN
  IF sym < lparen THEN Mark(31);
    REPEAT
      IF userHasAborted OR
         terminalError THEN
        RETURN;
      END;

      GetSym;
    UNTIL sym >= lparen;
  END;

  IF sym = ident THEN
    qualident(obj);

    IF sym = lbrace THEN
      GetSym;

      IF (obj # NIL) & (obj^.class = Typ) &
         (obj^.typ^.form = Set) THEN
        Sets(x, obj^.typ);
      ELSE
        Mark(52);
        Sets(x, bitstyp);
      END;
    ELSE
      selector(x, obj);

      IF (x.mode = toolMd) &
         (x.proc^.std # NonStand) THEN
        StandProcCall(x);
      ELSIF sym = lparen THEN
        GetSym;

        IF x.mode = typMd THEN
          xt := x.typ;
          Expression(x);
          ConvertTyp(xt, x);
          x.typ := xt;
        ELSE
          PrepCall(x, fpar, savedRegs);
          ActualParameters(x, fpar);
          GenCall(x, savedRegs);
        END;

        CheckSym(rparen, 15);
      ELSIF ((x.mode = procMd) OR (x.typ^.form = ProcTyp)) AND
            ((times <= sym) AND (sym <= or)) THEN
        Mark(22);
      END;
    END;
  ELSIF sym = number THEN
    GetSym;
    x.mode := conMd;

    CASE numtyp OF
      1: x.typ := cardtyp; x.val.C  := intval;
    | 2: x.typ := dbltyp;  x.val.D  := dblval;
    | 3: x.typ := chartyp; x.val.Ch := VAL(CHAR, intval);            (* V2.6 *)
         x.val.strChar := FALSE;
    | 4: x.typ := realtyp; x.val.R  := realval;
    | 5: x.typ := lrltyp;  x.val.X  := lrlval;
    | 6: x.typ := chartyp; x.val.Ch := VAL(CHAR, intval);
         x.val.strChar := TRUE;
    END;

    x.val.modNo := mainmod^.modno;
  ELSIF sym = string THEN
    x.typ := stringtyp;
    x.mode := conMd;

    IF curLev = 0 THEN
      AllocString(globalStringList, id, x.val.D0, x.val.D1);
    ELSE
      AllocString(localStringList, id, x.val.D0, x.val.D1);
    END;

    x.val.D2 := 0;
    x.val.D3 := curLev;
    x.val.modNo := mainmod^.modno;
    x.val.strChar := FALSE;
    GetSym;
  ELSIF sym = lparen THEN
    GetSym;
    Expression(x);
    CheckSym(rparen, 15);
  ELSIF sym = lbrace THEN
    GetSym;
    Sets(x, bitstyp);
  ELSIF sym = not THEN
    GetSym;
    Factor(x);
    GenNot(x);
  ELSE
    Mark(31);
    SetconMd(x, 0, undftyp);
  END;
END Factor;

PROCEDURE Term(VAR x: Item);
VAR
  y:      Item;
  mulop:  Symbol;
BEGIN
  Factor(x);

  WHILE (times <= sym) AND (sym <= and) DO
    IF userHasAborted OR
       terminalError THEN
      RETURN;
    END;

    mulop := sym;
    GetSym;

    IF mulop = and THEN
      GenAnd(x);
    END;

    Factor(y);
    GenOp(mulop, x, y);
  END
END Term;

PROCEDURE SimpleExpression(VAR x: Item);
VAR
  y:      Item;
  addop:  Symbol;
BEGIN
  IF sym = minus THEN
    GetSym;
    Term(x);
    GenNeg(x);
  ELSE
    IF sym = plus THEN
      GetSym;
    END;

    Term(x);
  END;

  WHILE (plus <= sym) AND (sym <= or) DO
    IF userHasAborted OR
       terminalError THEN
      RETURN;
    END;

    addop := sym;
    GetSym;

    IF addop = or THEN
      GenOr(x);
    END;

    Term(y);
    GenOp(addop, x, y);
  END;
END SimpleExpression;

PROCEDURE Expression(VAR x: Item);
VAR
  y:        Item;
  relation: Symbol;
BEGIN
  SimpleExpression(x);

  IF (eql <= sym) & (sym <= in) THEN
    relation := sym;
    GetSym;
    GenVal(x);                                                      (* V2.6 *)
    SimpleExpression(y);
    GenVal(y);                                                      (* V2.6 *)

    IF relation = in THEN
      GenIn(x,y);
    ELSE
      GenOp(relation,x,y);
    END;
  END;
END Expression;

PROCEDURE Priority;
VAR
  x: Item;
BEGIN
  IF sym = lbrak THEN
    GetSym;
    ConstExpression(x);

    IF (x.typ = cardtyp) &
       (x.val.C < 16) THEN
      curPrio := x.val.C;
    ELSE
      Mark(147);
    END;

    CheckSym(rbrak, 16);
  ELSE
    curPrio := 0;
  END;
END Priority;

PROCEDURE ImportList(impmod: ObjPtr);
  VAR obj: ObjPtr;
BEGIN
  IF terminalError THEN
    RETURN;
  END;

  IF (impmod # NIL) & (impmod^.class # Module) THEN
    Mark(55);
    impmod := NIL;
  END;

  LOOP
    IF sym = ident THEN
      IF impmod = NIL THEN
        obj := FindImport(id);
      ELSE
        obj := FindInScope(id, impmod^.root);
        
        IF (obj # NIL) & NOT obj^.exported THEN
          obj := NIL;
        END;
      END;

      IF obj # NIL THEN
        NewImp(topScope, obj);
      ELSE
        Mark(50);
      END;

      GetSym;
    ELSE
      Mark(10);
    END;

    IF sym = comma THEN
      GetSym;
    ELSIF sym = ident THEN
      Mark(11);
    ELSE
      EXIT;
    END;
  END;

  CheckSym(semicolon, 12);
END ImportList;

PROCEDURE ExportList;
  VAR obj: ObjPtr;
BEGIN
  LOOP
    IF userHasAborted OR
       terminalError THEN
      RETURN;
    END;

    IF sym = ident THEN
      obj := NewObj(id, Temp); KeepId; GetSym
    ELSE Mark(10)
    END ;

    IF sym = comma THEN GetSym
    ELSIF sym = ident THEN Mark(11)
    ELSE EXIT
    END
  END ;
  CheckSym(semicolon, 12)
END ExportList;

PROCEDURE Block(ancestor: ObjPtr; qual: BOOLEAN;
                VAR adr: aAdr; VAR L0: CARDINAL);
VAR
  obj, last:    ObjPtr;
  newtypdef:    BOOLEAN;
  id0:          CARDINAL;
  x:            Item;
  typ:          StrPtr;
  offset:       aAdr;
  L1, exits, loopLev, blockEnd: CARDINAL;
  exitTab:      ExitTable;
  modName:      aSymbol;
  segName:      aSymbol;
  globalName:   aSymbol;
  OK:           BOOLEAN;
  saveStackChk: BOOLEAN;

  PROCEDURE FormalParameters(proc: ObjPtr);
  VAR
    isvar:            BOOLEAN;
    size:             INTEGER;
    par, par0, par1:  ParPtr;
    typ0:             StrPtr;
  BEGIN
    par := NIL;
    size := 0;

    IF (sym = ident) OR (sym = var) THEN
      LOOP
        IF userHasAborted OR
           terminalError THEN
          RETURN;
        END;

        par1 := par;
        isvar := FALSE;

        IF sym = var THEN
          GetSym;
          isvar := TRUE;
        END;

        LOOP
          IF userHasAborted OR
             terminalError THEN
            RETURN;
          END;

          IF sym = ident THEN
            par := NewPar(id, isvar, par);
            KeepId;
            GetSym;
          ELSE
            Mark(10);
          END;

          IF sym = comma THEN
            GetSym;
          ELSIF sym = ident THEN
            Mark(11);
          ELSIF sym = var THEN
            Mark(11);
            GetSym;
          ELSE
            EXIT;
          END;
        END;

        CheckSym(colon, 13);
        FormalType(typ0);
        par0 := par;

        WHILE par0 # par1 DO
          par0^.typ := typ0;
          AllocPar(par0, size);
          par0 := par0^.next;
        END;

        IF sym = semicolon THEN
          GetSym;
        ELSIF sym = ident THEN
          Mark(12);
        ELSE
          EXIT;
        END;
      END;
    END;

    par1 := NIL; (*reverse list*)

    WHILE par # NIL DO
      par0 := par;
      par := par0^.next;
      par0^.next := par1;
      par1 := par0;
    END;

    proc^.firstParam := par1;
    proc^.pd^.size := ABS(size); (*of param area*)
    CheckSym(rparen, 15);
  END FormalParameters;

  PROCEDURE CheckParameters(proc: ObjPtr);
  VAR
    isvar:            BOOLEAN;
    par, par0, par1:  ParPtr;
    typ0:             StrPtr;
  BEGIN par0 := proc^.firstParam;
    IF (sym = ident) OR (sym = var) THEN
      LOOP par1 := par0; isvar := FALSE;
        IF userHasAborted OR
           terminalError THEN
          RETURN;
        END;

        IF sym = var THEN 
          GetSym;
          isvar := TRUE;
        END;

        LOOP
          IF userHasAborted OR
             terminalError THEN
            RETURN;
          END;

          IF sym = ident THEN
            IF par0 # NIL THEN par0^.name := id; par0 := par0^.next
              ELSE Mark(66)
            END ;
            KeepId; GetSym
          ELSE Mark(10)
          END ;

          IF sym = comma THEN GetSym
          ELSIF sym = ident THEN Mark(11)
          ELSIF sym = var THEN Mark(11); GetSym
          ELSE EXIT
          END
        END ;
        CheckSym(colon, 13); FormalType(typ0); par := par1;
        WHILE par # par0 DO
          IF (par^.typ # typ0) &
            ((par^.typ^.form # Array) OR (typ0^.form # Array) OR
             (par^.typ^.ElemTyp # typ0^.ElemTyp)) THEN Mark(69)
          END ;
          IF par^.varpar # isvar THEN Mark(68) END ;
          par := par^.next
        END ;
        IF sym = semicolon THEN GetSym
        ELSIF sym = ident THEN Mark(12)
        ELSE EXIT
        END
      END
    END ;
    IF par0 # NIL THEN Mark(70) END ;
    CheckSym(rparen, 15)
  END CheckParameters;

  PROCEDURE MakeParameterObjects(proc: ObjPtr);
  VAR
    par: ParPtr;
    obj: ObjPtr;
    adr: aAdr;
  BEGIN
    par := proc^.firstParam;

    adr := ParamStartAdr + proc^.pd^.size;

    WHILE par # NIL DO
      obj := NewObj(par^.name, Var); (*name field no longer used*)

      WITH obj^ DO
        typ := par^.typ;
        vmod := 0;
        vlev := curLev;
        varpar := par^.varpar;
        AllocPar(par, adr);
        vadr := adr;
        vpar := TRUE;
        parent := proc;
      END;

      par := par^.next;
    END
  END MakeParameterObjects;

  PROCEDURE ProcedureDeclaration(VAR proc: ObjPtr);
  VAR
    i, L0, L1:  CARDINAL;
    adr:        aAdr;
    par, res:   ObjPtr;
    saveSym:    Symbol;
  BEGIN
    UpdateSpinner;

    proc := Find(id);

    IF (proc # NIL) & (proc^.class = Proc) & (proc^.pmod = 0) &
       ((proc^.pd^.adr = 0) & (curLev = 0) & isimp OR     (*heading in def mod *)
        proc^.pd^.forward & (proc^.pd^.lev = curLev)) THEN (*forward*)
      IF proc^.pd^.adr = 0 THEN
        proc^.pd^.exp := TRUE;
      END;

      CheckSym(ident, 10);

      IF sym = lparen THEN
        GetSym;
        CheckParameters(proc);

        IF sym = colon THEN
          GetSym;

          IF sym = ident THEN
            qualident(res);

            IF (res = NIL) OR (res^.class # Typ) OR (res^.typ # proc^.typ) THEN
              Mark(71);
            END
          ELSE
            Mark(10);
          END;
        ELSIF proc^.typ # notyp THEN
          Mark(72);
        END;
      ELSIF proc^.firstParam # NIL THEN
        Mark(73);
      END;
    ELSE (*new procedure*)
      proc := NewObj(id, Proc);
      KeepId;

      WITH proc^ DO
        pmod := 0;
        typ := notyp;
        firstParam := NIL;
        parent := ancestor;

        IF Pascal IN Directives THEN
          procType := pascal;
        ELSE
          procType := modula2;
        END;
      END;

      WITH proc^.pd^ DO
        forward := FALSE;
        exp := FALSE;
        lev := curLev;
        adr := 0;
        size := 0;
        INC(pno);
        num := pno;
      END;

      CheckSym(ident, 10);

      IF sym = lparen THEN
        GetSym;
        FormalParameters(proc);

        IF sym = colon THEN
          GetSym;
          proc^.typ := undftyp;

          IF sym = ident THEN
            qualident(res);

            IF (res # NIL) & (res^.class = Typ) THEN
              proc^.typ := res^.typ;
            ELSE
              Mark(52);
            END;
          ELSE
            Mark(10);
          END;
        END;
      END;
    END;

    CheckSym(semicolon, 12);

    IF (sym = tool) OR (sym = gsos) THEN
      IF Pascal IN Directives THEN
        Mark(311);
      END;

      saveSym := sym;
      GetSym;
      DEC(pno);

      WITH proc^ DO
        IF pd^.exp OR pd^.forward THEN
          Mark(74);
        END;

        class := Tool;
        std := NonStand;
        ConstExpression(x);

        IF x.typ = cardtyp THEN
          cnum := x.val.C;
          GSOSproc := (saveSym = gsos);
        ELSE
          cnum := 0;
          Mark(133);
        END;
      END;

      CheckSym(semicolon, 12);
    ELSIF proc^.procType = pascal THEN
      IF curLev > 0 THEN
        Mark(310);
      END;

      DEC(pno);

      WITH proc^ DO
        IF pd^.exp OR pd^.forward THEN
          Mark(74);
        END;
      END;
    ELSIF NOT isdef THEN
      MarkHeap;
      NewScope(Proc);
      INC(curLev);

      IF curLev > maxLev THEN
        Mark(309);
      END;

      IF sym = forward THEN
        GetSym;

        WITH proc^.pd^ DO
          IF exp OR forward THEN
            Mark(74);
          END;

          forward := TRUE;
          exp := FALSE;
          lev := curLev-1;
        END;
      ELSE
        pc := 0;                (* We want to start the counter again since *)
        CodeOverflow := FALSE;  (* it is used as an offset in RefPoint      *)

        (* After this call to make the parameter objects, each of the parm's *)
        (* is given an offset from the bottom of the access link in the stack *)
        (* frame.  This offset is placed in the respective objects 'vadr'    *)
        (* field. *)
        MakeParameterObjects(proc);

        proc^.pd^.adr := 1;
        proc^.pd^.forward := FALSE;
        L0 := pc;
        adr := 0;
        Block(proc, FALSE, adr, L0);
      END;

      DEC(curLev);
      CloseScope;
      ReleaseHeap;
      CheckSym(semicolon, 12);
    END
  END ProcedureDeclaration;

  PROCEDURE ModuleDeclaration(VAR mod: ObjPtr; VAR adr: aAdr; VAR L0: CARDINAL);
  VAR
    prio:     CARDINAL;
    qual:     BOOLEAN;
    impmod:   ObjPtr;
    prevMod:  CARDINAL;
  BEGIN
    qual := FALSE;
    CheckSym(ident, 10);

    mod := NewObj(id, Module);
    KeepId;

    mod^.modno := ModNo;
    mod^.key := sysmod^.key;

    prevMod := curMod;
    curMod := ModNo;

    IF ancestor^.class = Module THEN
      mod^.parentMod := ancestor^.modno;
    ELSE
      mod^.parentMod := ancestor^.pmod;
    END;

    prio := curPrio;
    Priority;
    mod^.parent := ancestor;
    AddModule(ModNo, mod);
    INC(ModNo);

    CheckSym(semicolon, 12); 
    NewScope(Module);

    WHILE (sym = from) OR (sym = import) DO
      IF userHasAborted OR
         terminalError THEN
        RETURN;
      END;

      impmod := NIL;

      IF sym = from THEN
        GetSym;

        IF sym = ident THEN
          impmod := FindImport(id);
          GetSym;
        ELSE
          Mark(10);
        END;

        CheckSym(import, 30);
      ELSE
        GetSym;
      END;

      ImportList(impmod);
    END;

    IF sym = export THEN
      GetSym;

      IF sym = qualified THEN 
        GetSym; 
        qual := TRUE;
      END;

      ExportList;
    END;

    Block(mod, qual, adr, L0);

    CloseScope;
    curPrio := prio;
    curMod := prevMod;
  END ModuleDeclaration;

  PROCEDURE StatSeq;
  VAR
    obj:          ObjPtr;
    fpar:         ParPtr;
    x, y:         Item;
    L0, L1, e:    CARDINAL;
    savedRegs:    DRegSet;

    PROCEDURE CasePart;
    VAR
      x:          Item;
      n:          CARDINAL;
      L0, L1, L2: CARDINAL;
      tab:        ARRAY [0..NofCases-1] OF LabelRange;
      savedReg:   DRegSet;
      newReg:     DRegSet;
    BEGIN
      n := 0;
      Expression(x);
      GenCase1(x, L0);
      CheckSym(of, 23);
      newLink(L2);

      SaveRegs(savedReg);

      LOOP
        IF userHasAborted OR
           terminalError THEN
          RETURN;
        END;

        IF sym < bar THEN
          CaseLabelList(x.typ, n, tab, TRUE);
          CheckSym(colon, 13);
          StatSeq;
          GenCase2(L2);
        END;

        IF sym = bar THEN
          GetSym;
        ELSE
          EXIT;
        END;
      END;

      newLink(L1);  (* l1 is the location of the ELSE code *)
      FixLink(L1);

      IF sym = else THEN
        GetSym;
        StatSeq;
        GenCase2(L2);
      ELSE
        GenTerminate(tsUndefinedCase);
      END;

      SaveRegs(newReg);

      IF savedReg <> newReg THEN
        Mark(400);
      END;

      RefPoint;

      GenCase3(x, L0, L1, n, tab);

      FixLink(L2);
    END CasePart;

    PROCEDURE ForPart;
    VAR
      obj:            ObjPtr;
      v, e1, e2, e3:  Item;
      L0, L1:         CARDINAL;
    BEGIN
      obj := NIL;

      IF sym = ident THEN
        obj := Find(id);

        IF obj # NIL THEN
          IF (obj^.class # Var) OR
             obj^.varpar OR
             (obj^.vmod > 0) THEN
            Mark(75);
          END;
        ELSE
          Mark(50);
        END;

        GetSym;
      ELSE
        Mark(10);
      END;

      GenItem(v, obj, Scope);

      IF sym = becomes THEN
        GetSym;
      ELSE
        Mark(19);

        IF sym = eql THEN
          GetSym;
        END;
      END;

      Expression(e1);
      GenVal(e1);
      GenFor1(v, e1);                      (* V2.6 *)
      CheckSym(to, 24);
      GenAssign(v, e1);
      Expression(e2);
      GenVal(e2);
      GenFor2(v, e1, e2);(* V2.6 *)

      IF sym = by THEN
        GetSym;
        ConstExpression(e3);
      ELSE
        SetconMd(e3, 1, v.typ);
      END;

      GenFor3(v, e2, e3, L0, L1);
      CheckSym(do, 25);
      StatSeq;
      GenFor4(v, e2, e3, L0, L1)
    END ForPart;

  BEGIN
    UpdateSpinner;

    LOOP
      IF userHasAborted OR
         objectFileInError OR
         terminalError THEN
        RETURN;
      END;

      (*
        We do this so that if an error has been detected while parsing a 
        statement, leaving some registers allocated, we have them all
        available again for the next statement.  This prevents a whole
        swathe of parasitic errors.
      *)
      SetRegs(DRegSet{});

      IF sym < ident THEN Mark(35);
        REPEAT
          IF userHasAborted OR
             terminalError THEN
            RETURN;
          END;

          GetSym;
        UNTIL sym >= ident;
      END ;
      IF sym = ident THEN
        (* if we have an identifier, then it can be one of several things
           occuring... *)
        RefPoint;
        qualident(obj); 
        selector(x, obj);
        IF sym = becomes THEN
          GetSym; Expression(y); GenVal(y); GenAssign(x, y)
        ELSIF sym = eql THEN
          Mark(19); GetSym; Expression(y); GenAssign(x, y)
        ELSIF (x.mode = toolMd) & (x.proc^.std # NonStand) THEN
          StandProcCall(x);
          IF x.typ # notyp THEN Mark(76) END
        ELSE
          PrepCall(x, fpar, savedRegs);

          IF sym = lparen THEN
            GetSym; 
            ActualParameters(x, fpar); 
            CheckSym(rparen, 15);
          ELSIF fpar # NIL THEN
            Mark(65);
          END;

          GenCall(x, savedRegs);

          IF x.typ # notyp THEN
            Mark(76);
          END;
        END;
      ELSIF sym = if THEN
        GetSym;
        RefPoint;
        Expression(x);
        GenCFJ(x, L0);
        CheckSym(then, 27);
        StatSeq;
        L1 := 0;

        WHILE sym = elsif DO
          IF userHasAborted OR
             terminalError THEN
            RETURN;
          END;

          GetSym;
          GenFJ(L1);
          FixLink(L0);
          RefPoint;
          Expression(x);
          GenCFJ(x, L0);
          CheckSym(then, 27);
          StatSeq;
        END;

        IF sym = else THEN
          GetSym;
          GenFJ(L1);
          FixLink(L0);
          StatSeq;
        ELSE
          FixLink(L0)
        END;

        FixLink(L1);
        CheckSym(end, 20)
      ELSIF sym = case THEN
        GetSym; RefPoint; CasePart; CheckSym(end, 20)
      ELSIF sym = while THEN
        GetSym;
        newLink(L1);      (* L1 is at beginning of loop code *)
        FixLink(L1);      (* generate the start of loop label *)
        RefPoint;
        Expression(x);    (* generate the expression *)
        GenCFJ(x, L0);    (* if false goto L0 *)
        CheckSym(do, 25);
        StatSeq;          (* generate the statement sequence *)
        GenBJ(L1);        (* goto the start of the loop code *)
        FixLink(L0);      (* generate the end of loop label *)
        CheckSym(end, 20)
      ELSIF sym = repeat THEN
        GetSym;
        newLink(L0);      (* L0 is at beginning of loop code *)
        FixLink(L0);      (* generate the start of loop label *)
        StatSeq;          (* generate the statement sequence *)
        IF sym = until THEN
          GetSym;
          RefPoint;
          Expression(x);  (* generate the expression *)
          GenCBJ(x, L0);  (* if false goto L0 *)
        ELSE
          Mark(26);
        END
      ELSIF sym = loop THEN
        GetSym;
        INC(loopLev);       (* loopLev is used when an exit statement is
                               detected.  if > 0, then the exit is valid,
                               otherwise it is invalid to exit a non-existant
                               loop *)
        GenLoop1(e, exits); (* "e" now contains the parent loops (if any) exit
                               label.  "exits" is given a new value, that of
                               the exit label for this new loop.  any "exit"
                               statements will use this new value of "exits"
                               as the label to jump to. *)
        newLink(L0);        (* L0 is the beginning of loop label *)
        FixLink(L0);        (* generate the start of loop label *)
        StatSeq;            (* generate the loop contents code *)
        GenBJ(L0);          (* goto the start of loop label *)
        CheckSym(end, 20);
        GenLoop2(e, exits); (* restore "exits" to the parents exit label value *)
        DEC(loopLev);       (* decrement loopLev, to indicate the end of loop
                               processing *)
      ELSIF sym = for THEN
        GetSym; RefPoint; ForPart; CheckSym(end, 20)
      ELSIF sym = with THEN
        GetSym;
        x.typ := NIL;

        IF sym = ident THEN
          qualident(obj);
          selector(x, obj);

          IF x.typ^.form = Record THEN
            NewScope(Typ);
            GenWith(x, adr);
            topScope^.name := wlev;
            topScope^.right := x.typ^.firstFld;
          ELSE
            Mark(57);
            x.typ := NIL;
          END
        ELSE
          Mark(10);
        END;

        CheckSym(do, 25);
        StatSeq;
        CheckSym(end, 20);

        IF x.typ # NIL THEN
          CloseScope;
        END;

        GenWith2;
      ELSIF sym = exit THEN
        GetSym;

        IF loopLev > 0 THEN
          GenExit(exits);
        ELSE
          Mark(39);
        END;
      ELSIF sym = return THEN
        GetSym;

        IF sym < semicolon THEN
          Expression(x);
        ELSE
          x.typ := notyp;

          IF ancestor^.typ # notyp THEN
            Mark(139);
          END;
        END;

        GenResult(x, ancestor, blockEnd);
      END;

      IF sym = semicolon THEN
        GetSym;
      ELSIF (sym <= ident) OR (if <= sym) & (sym <= for) THEN
        Mark(12);
      ELSE
        EXIT;
      END;
    END;
  END StatSeq;

  PROCEDURE CheckExports(obj: ObjPtr);
  BEGIN
    IF obj # NIL THEN
      IF obj^.class = Temp THEN
        Mark(80);
      ELSIF NOT qual AND obj^.exported THEN (*import in outer scope*)
        NewImp(topScope^.left, obj);
      END;

      CheckExports(obj^.left);
      CheckExports(obj^.right);
    END;
  END CheckExports;

  PROCEDURE CheckUDProc(obj: ObjPtr);
  BEGIN (*check for undefined procedure bodies*)
    WHILE obj # NIL DO
      IF (obj^.class = Proc) & (obj^.pmod = 0) & (obj^.procType = modula2) &
         ((obj^.pd^.adr = 0) OR obj^.pd^.forward) THEN
        Mark(89);
      END;

      obj := obj^.next;
    END;
  END CheckUDProc;

BEGIN (*Block*)
  LOOP
    UpdateSpinner;

    IF userHasAborted OR
       objectFileInError OR
       terminalError THEN
      RETURN;
    END;

    IF sym = const THEN
      GetSym;

      WHILE sym = ident DO
        IF userHasAborted OR
           terminalError THEN
          RETURN;
        END;

        id0 := id;
        KeepId;
        GetSym;

        IF sym = eql THEN
          GetSym;
          ConstExpression(x);
        ELSIF sym = becomes THEN
          Mark(18);
          GetSym;
          ConstExpression(x);
        ELSE
          Mark(18);
        END;

        obj := NewObj(id0, Const);
        obj^.typ := x.typ;
        obj^.conval := x.val;

        IF (x.typ = stringtyp) & (obj^.conval.D2 = 0) THEN
          obj^.conval.D2 := id;
          KeepId;
        END;

        CheckSym(semicolon, 12);
      END;
    ELSIF sym = type THEN
      GetSym;

      WHILE sym = ident DO
        IF userHasAborted OR
           terminalError THEN
          RETURN;
        END;

        typ := undftyp;
        obj := NIL;
        newtypdef := TRUE;

        IF isimp & (curLev = 0) THEN
          obj := Find(id);
          IF (obj # NIL) & (obj^.class = Typ) & (obj^.typ^.form = Opaque) THEN
            newtypdef := FALSE;
          END;
        END;

        IF newtypdef THEN
          id0 := id;
          KeepId;
        END;

        GetSym;

        IF sym = eql THEN
          GetSym;
          Type(typ);
        ELSIF (sym = becomes) OR (sym = colon) THEN
          Mark(18);
          GetSym;
          Type(typ);
        ELSIF NOT isdef THEN
          Mark(18);
        ELSE
          typ := NewStr(Opaque);
          typ^.size := PointerTypSize;
        END;

        IF newtypdef THEN
          obj := NewObj(id0, Typ);
          obj^.typ := typ;
          obj^.mod := mainmod;

          IF typ^.strobj = NIL THEN
            typ^.strobj := obj;
          END;
        ELSIF typ^.size = VAL(LONGINT, PointerTypSize) THEN
          obj^.typ^ := typ^;  (* param! *)
        ELSE
          Mark(502);
        END;

        CheckUDP(obj, topScope^.right);  (*check for undefined pointer types*)
        CheckSym(semicolon, 12);
      END;
    ELSIF sym = var THEN
      GetSym;

      WHILE sym = ident DO
        IF userHasAborted OR
           terminalError THEN
          RETURN;
        END;

        last := topScope^.last;
        obj := last;

        LOOP
          IF userHasAborted OR
             terminalError THEN
            RETURN;
          END;

          IF sym = ident THEN
            obj := NewObj(id, Var);
            KeepId;
            GetSym;
          ELSE
            Mark(10);
          END;

          IF sym = comma THEN
            GetSym;
          ELSIF sym = ident THEN
            Mark(11);
          ELSE
            EXIT;
          END;
        END;

        CheckSym(colon, 13);
        Type(typ);

        WHILE (last # obj) & (last # NIL) DO
          last := last^.next;
          last^.typ := typ;

          WITH last^ DO
            varpar := FALSE;
            vmod := 0;
            vlev := curLev;
            parent := ancestor;
          END;

          IF curLev = 0 THEN
            AllocGlobalVar(last);
            adr := 0;
          ELSE
            AllocVar(last, adr);
          END;
        END;

        CheckSym(semicolon, 12);
      END;
    ELSIF sym = procedure THEN
      GetSym;
      ProcedureDeclaration(obj);
    ELSIF sym = module THEN
      GetSym;
      ModuleDeclaration(obj, adr, L0);
      CheckSym(semicolon, 12);
    ELSE
      IF (sym # begin) & (sym # end) THEN
        Mark(36);

        REPEAT
          IF userHasAborted OR
             terminalError THEN
            RETURN;
          END;

          GetSym
        UNTIL (sym >= begin) OR (sym = end);
      END;

      IF (sym <= begin) OR (sym = eof) THEN
        EXIT;
      END;
    END;
  END;

  exits := 0;
  loopLev := 0;
  blockEnd := 0;  (* label used in RETURN *)

  IF ancestor^.class = Module THEN
    CheckExports(topScope^.right);
    ancestor^.firstObj := topScope^.next;
    ancestor^.root := topScope^.right;

    IF NOT isdef THEN
      GetModuleNameByPtr(ancestor, modName);
      GetModuleKey(ancestor^.modno, segName, OK);
      Concat(modName, segName, segName);
      NewSegmentHeader(segName);

      saveStackChk := stackchk;
      stackchk := FALSE;
      Link(WorkingStorage, 0);
      stackchk := saveStackChk;
      GenInitMod(blockEnd, isimp, segName, ancestor);
    END;
  ELSE (*procedure*)
    IF NOT isdef THEN
      ancestor^.pd^.entryPoint := pc;
      ancestor^.firstLocal := topScope^.next;

      (*
        the stack frame size for this procedure can be determined by the
        equation:

        stackFrameSize := ancestor^.pd^.size +
                          ParamStartAdr +
                          VAL(CARDINAL, ABS(adr)) +
                          WorkingStorage;

        'offset' is the size of the local data plus the size of the working
        storage area.
      *)

      offset := ABS(adr) + WorkingStorage;

      (*
        it also provides an offset into the direct page to the access link,
        and therefor the return address and the stack frame pointer, where:

        if accessLink        = x     then

           returnAddress     = x + 2 and
           stackFramePointer = x + 5
      *)

      ancestor^.pd^.accessLink := ABS(adr);

      (*
        the following loop will adjust the offset addresses of the parameters
        and local variables so that the last local variable will start at the
        first byte above the working storage area on the stack.  The other
        local variables will follow, then the access link, return address,
        stack frame pointer, and the procedure parameters.
      *)

      obj := topScope^.next;
      WHILE (obj <> NIL) DO
        IF (obj^.class = Var) THEN
          obj^.vadr := obj^.vadr + WorkingStorage;

          IF obj^.vpar THEN
            obj^.vadr := obj^.vadr + ABS(adr);
          END;
        END;

        obj := obj^.next;
      END;

      GenEnter(offset, ancestor, isimp);

      obj := topScope^.next;
      WHILE (obj <> NIL) DO
        IF (obj^.typ^.form = Array) AND
           (obj^.typ^.dyn)          AND
           (NOT obj^.varpar)        THEN
          CopyDynArray(obj^.vadr, obj^.typ^.ElemTyp^.size);
        END;
  
        obj := obj^.next;
      END;

      GetProcedureName(ancestor, segName);

      IF DebugCode THEN
        GenStartProcedure(segName);
        GenDebugSymbols(ancestor);
      END;
    END;
  END;

  IF NOT isdef THEN
    CheckUDProc(topScope^.next);

    IF DebugCode THEN
      GenLoadSource(source.nameString.inString.text);
    END;
  END;

  IF sym = begin THEN
    (*
      now that we have completed the procedure declaration, including all
      local data and procedures, we may process the statement sequence.
    *)
    IF isdef THEN
      Mark(37);
    END ;

    GetSym;
    StatSeq;
    RefPoint;
  END;

  IF NOT isdef THEN
    (* generate the return code *)
    GenReturn(ancestor, blockEnd, isimp);
  END;

  CheckSym(end, 20);

  IF NOT scanerr AND isdef THEN
    OutUnit(ancestor);
  END;

  IF sym = ident THEN
    IF Diff(id, ancestor^.name) # 0 THEN
      Mark(77);
    END;

    GetSym;
  ELSE
    Mark(10);
  END;
END Block;

VAR
  FName:  GSOSNameString;
  TName:  GSOSNameString;

PROCEDURE CompilationUnit;
VAR
  id0, adr: aAdr; L0: CARDINAL;
  hdr, importMod:     ObjPtr;
  impok:              BOOLEAN;
  i:                  CARDINAL;

  PROCEDURE GetFileName(    j:      CARDINAL;
                        VAR FName:  ARRAY OF CHAR;
                            ext:    ARRAY OF CHAR);
  VAR
    origname: aSymbol;
  BEGIN
    IdToSymbol(j, origname);
    MakeFileName(origname, FName, ext);
  END GetFileName;

  PROCEDURE ImportModule;
  VAR
    adr:    aAdr;
    pno:    CARDINAL;
    FName:  GSOSNameString;
  BEGIN
    IF sym = ident THEN
      IF Diff(id, sysmod^.name) = 0 THEN
        importMod := sysmod;
      ELSE
        GetFileName(id, FName, ".SYM");

        IF ShowProgress THEN
          WriteString(" Importing ");
        END;

        InRef(FName, hdr, adr, pno);

        IF hdr # NIL THEN
          importMod := hdr^.right;
          AddModule(importMod^.modno, importMod);
        ELSE 
          impok := FALSE;
          importMod := NIL;

          IF ShowProgress THEN
            WriteString(" Import of ");
            WriteString(FName);
            WriteString(" failed.");
            WriteLn;
          END;
        END;
      END;

      GetSym;
    ELSE
      Mark(10);
    END ;
  END ImportModule;

VAR
  modName:  aSymbol;
  keyName:  aSymbol;
  OK:       BOOLEAN;

BEGIN
  isdef := FALSE;
  isimp := FALSE;
  isint := FALSE;
  impok := TRUE;
  curLev := 0;
  curPrio := 0;
  spinner := '|';
  GetSym;

  IF sym = definition THEN
    GetSym;
    isdef := TRUE;
  ELSIF sym = implementation THEN
    GetSym;
    isimp := TRUE;
  END;

  moduleDeclared := TRUE;

  IF sym = module THEN
    GetSym;

    IF sym = ident THEN
      id0 := id;
      mainmod^.name := id0;
      mainmod^.isInt := isint;
      AddModule(0, mainmod);
      curMod := 0;
      KeepId;
      GetSym;

      IF NOT isdef THEN
        Priority;
      END;

      CheckSym(semicolon, 12);
      MarkHeap;
      NewScope(Module);

      GetFileName(id0, FName, ".SYM");

      IF isimp THEN
        IF ShowProgress THEN
          WriteString(" Importing ");
        END;

        InRef(FName, hdr, adr, pno);

        IF hdr # NIL THEN
          importMod := hdr^.right;
          topScope^.right := importMod^.root;  (*mainmod*)
          topScope^.next := hdr^.next;
          topScope^.last := hdr^.last;
        ELSE
          importMod := NIL;
          impok := FALSE;

          IF ShowProgress THEN
            WriteString("Import of ");
            WriteString(FName);
            WriteString(" failed.");
            WriteLn;
          END;
        END;
      ELSE
        adr := 0;
        pno := 0;
        mainmod^.key := sysmod^.key;
      END;

      WHILE ((sym = from) OR (sym = import)) AND
            (NOT userHasAborted) AND
            (NOT terminalError) DO
        IF sym = from THEN
          GetSym;
          ImportModule;
          CheckSym(import, 30);
          ImportList(importMod);
        ELSE (*sym = import*)
          GetSym;

          LOOP
            IF userHasAborted OR
               terminalError THEN
              RETURN;
            END;

            ImportModule;

            IF importMod # NIL THEN
              NewImp(topScope, importMod);
            END;

            IF sym = comma THEN
              GetSym;
            ELSIF sym # ident THEN
              EXIT;
            END;
          END;

          CheckSym(semicolon, 12);
        END;
      END;
        
      IF NOT userHasAborted AND NOT terminalError THEN
        IF sym = export THEN
          IF isdef THEN
            GetSym;

            IF sym = qualified THEN
              GetSym;  (* always qualified *)
            END;

            ExportList;
          ELSE
            GetSym;
            Mark(38);

            WHILE (sym # semicolon) AND
                  (NOT userHasAborted) AND
                  (NOT terminalError) DO
              GetSym;
            END;

            GetSym;
          END;
        END;

        IF impok THEN
          TName := FName;

          IF isdef THEN
            GetFileName(id0, TName, ".SYM");
          ELSE
            OpenNextObjectFile(NOT isimp);

            GenEnterMod(ModNo, pno, NOT isimp);

            IF NOT isimp THEN
              GenExitMod;

              CloseObjectFile;
              OpenNextObjectFile(NOT isimp);
            END;

            GetFileName(id0, TName, ".REF");
            L0 := 0;
          END;

          OpenRef(TName);

          (*
            If we failed to open the reference/symbol file, then it is a
            terminal error.
          *)
          IF ShowProgress THEN
            IF NOT terminalError THEN
              WriteString(" Creating  ");
              WriteString(TName);
              WriteLn;
            ELSE
              WriteString(" Create of ");
              WriteString(TName);
              WriteString(' failed. (Error during open)');
              WriteLn;
              scanerr := TRUE;
            END;
          END;

          IF NOT terminalError THEN
            Block(mainmod, TRUE, adr, L0);

            IF sym # period THEN
              Mark(14);
            END;
          END;

          IF NOT scanerr AND NOT userHasAborted AND NOT terminalError THEN
            IF NOT isdef THEN
              PutGlobals(isimp);

              IF ShowProgress THEN
                IF currentObjectName[0] = nul THEN
                  WriteString(" No object created.");
                ELSE
                  WriteString(" Creating  ");
                  WriteString(currentObjectName);
                END;

                WriteString(' (Data: ');
                WriteLongInt(objectDataSize, 6);
                WriteString(' bytes  Code: ');
                WriteLongInt(objectCodeSize, 6);
                WriteString(' bytes)');
                WriteLn;
              END;

              CloseObjectFile;
              CloseRef(adr, pno, VAL(LONGINT, RefAuxType), TRUE);
            ELSE
              CloseRef(adr, pno, VAL(LONGINT, SymAuxType), TRUE);

              IF DisplayKey THEN
                GetModuleNameByPtr(mainmod, modName);
                GetModuleKey(mainmod^.modno, keyName, OK);

                WriteLn;
                WriteLn;
                WriteString('Key for DEFINITION module is: "');
                WriteString(modName);
                WriteString(keyName);
                WriteString('".');
                WriteLn;
                WriteString('Be sure to update the ASM source accordingly.');
                WriteLn;
              END;
            END;
          ELSE
            CloseObjectFile;
            CloseRef(adr, pno, 0, FALSE);
          END;
        END;
      END;

      CloseScope;
      ReleaseHeap;
    ELSE
      Mark(10);
    END;
  ELSE
    Mark(28);
  END;

  (*
    Clear any spinners from the screen...
  *)
  spinner := scClearEOL;
  UpdateSpinner;

  IF ShowProgress THEN
    IF scanerr THEN
      WriteLn;
      WriteString("Errors detected during compilation.");
      WriteLn;

      OpenErrorOutput;
      WriteCard(errorCount, 4);

      IF errorCount = 1 THEN
        WriteString(' error detected.');
      ELSE
        WriteString(' errors detected.');
      END;

      WriteLn;
      CloseErrorOutput;
    ELSIF userHasAborted THEN
      WriteLn;
      WriteString("Compilation aborted by Open-Apple-Period.");
      WriteLn;
    ELSE
      OpenErrorOutput;
      WriteString('no errors detected.');
      WriteLn;
      CloseErrorOutput;
    END;
  END;
END CompilationUnit;

PROCEDURE Initialise;
BEGIN
  InitM2DM;
  InitM2SM;
  InitM2TM;
  InitM2RM;
  InitM2LM;
  InitM2Shell;
END Initialise;

VAR
  processor:      Processor;
  TM:             KeyTime;
BEGIN
  Initialise;
  errorsOccured := FALSE;

  ProcessorID(processor);

  IF ShowProgress THEN
(*  WriteString(processor); *)
    WriteString("ORCA/Modula-2 1.1.0d3");
    WriteLn;
    WriteString("Copyright 1993, Peter Easdown");
    WriteLn;
    WriteString("Published by Byte Works, Inc.");
    WriteLn;
  END;

  rngchk := FALSE;
  ovflchk := FALSE;
  stackchk := FALSE;
  nilchk := FALSE;
  databank := FALSE;

  GetFileName(FileName);

  IF FileName[0] <> 0C THEN
    IF ShowProgress THEN
      WriteLn;
      WriteString(' Compiling ');
      WriteString(FileName);
    END;

    IF SourceOpened(FileName) THEN
      IF ShowProgress THEN
        WriteLn;
      END;

      GetKeyTime(TM);

      WITH sysmod^.key^ DO
        k0 := TM.day; k1 := TM.minute;
        k2 := TM.millisecond;
      END;

      InitScanner(FileName);
      InitTableHandler;
      InitRef;
      InitOMF;
      InitM2HM;
      InitM2EM;
      CompilationUnit;
      CloseScanner;
      CloseHeaps;
    ELSIF ShowProgress THEN
      WriteString(" -- file not found");
      WriteLn;
    END;

    IF scanerr OR
       terminalError THEN
      errorsOccured := TRUE;
    END;
  END;

  ShutdownM2Shell(NOT errorsOccured, FileName);
  CloseMainHeap;
END Compile.
