(*$Segment M2TM*)
(*$RangeCheck+*)
(*$OverflowCheck+*)
IMPLEMENTATION MODULE M2TM; (* NW 7.4.83 / 19.12.85; WH 10.1.86 *)
                            (* HS 1.7.86 / 28.4.89 *)

FROM M2DM IMPORT
  WordSize, NilVal, ObjPtr, Object, ObjClass, StrPtr, Structure, StrForm,
  Standard, ParPtr, Parameter, PDesc, PDPtr, KeyPtr, Key, mainmod, sysmod, 
  language, undftyp, cardtyp, cardinttyp, inttyp, booltyp, chartyp, bitstyp,
  realtyp, lrltyp, lcardtyp, dbltyp, proctyp, notyp, stringtyp, addrtyp, 
  bytetyp, wordtyp, ALLOCATE, ResetHeap;
FROM M2SM IMPORT
  id, Diff, Enter, Mark;

VAR
  obj:      ObjPtr;
  universe: ObjPtr;
  BBtyp:    StrPtr;
  expo:     BOOLEAN;

  PROCEDURE FindInScope(id: CARDINAL; root: ObjPtr): ObjPtr;
    VAR obj: ObjPtr; d: INTEGER;
  BEGIN obj := root;
    LOOP IF obj = NIL THEN EXIT END;
      d := Diff(id, obj^.name);
      IF d < 0 THEN obj := obj^.left
      ELSIF d > 0 THEN obj := obj^.right
      ELSE EXIT
      END
    END;
    RETURN obj
  END FindInScope;

  PROCEDURE Find(id: CARDINAL): ObjPtr;
    VAR obj: ObjPtr;
  BEGIN Scope := topScope;
    LOOP obj := FindInScope(id, Scope^.right);
      IF obj # NIL THEN EXIT END;
      IF Scope^.kind = Module THEN
        obj := FindInScope(id, universe^.right); EXIT
      END;
      Scope := Scope^.left
    END;
    RETURN obj
  END Find;

  PROCEDURE FindImport(id: CARDINAL): ObjPtr;
    VAR obj: ObjPtr;
  BEGIN Scope := topScope^.left;
    LOOP obj := FindInScope(id, Scope^.right);
      IF obj # NIL THEN EXIT END;
      IF Scope^.kind = Module THEN
        obj := FindInScope(id, universe^.right); EXIT
      END;
      Scope := Scope^.left
    END;
    RETURN obj
  END FindImport;

  PROCEDURE NewObj(id: CARDINAL; cl: ObjClass): ObjPtr;
    VAR ob0, ob1: ObjPtr; d: INTEGER;
  BEGIN
    ob0 := topScope;
    ob1 := ob0^.right;
    d := 1;

    LOOP
      IF ob1 # NIL THEN
        d := Diff(id, ob1^.name);

        IF d < 0 THEN ob0 := ob1; ob1 := ob0^.left
        ELSIF d > 0 THEN ob0 := ob1; ob1 := ob0^.right
        ELSIF ob1^.class = Temp THEN (*export*)
          (*change variant*) ob1^.exported := TRUE;
          topScope^.last^.next := ob1; topScope^.last := ob1; EXIT
        ELSE
          (*double def*)
          Mark(100);
          ob1 := NIL;  (* we should create a new object anyway, in order to  *)
                       (* keep the compiler going.  otherwise the address we *)
                       (* pass back will be "behind" the topScopes last      *)
        END
      ELSE (*insert new object*)
        ALLOCATE(ob1, SIZE(Object));

        IF d < 0 THEN ob0^.left := ob1 ELSE ob0^.right := ob1 END;
        ob1^.left := NIL; ob1^.right := NIL; ob1^.next := NIL;
        IF cl # Temp THEN
          topScope^.last^.next := ob1; topScope^.last := ob1
        END;
        ob1^.exported := FALSE; EXIT
      END
    END;
    WITH ob1^ DO
      name := id; typ := undftyp; class := cl;
      parent := NIL;
      CASE cl OF
      | Header: kind := Proc; last := NIL; heap := NIL; withadr := 0;
      | Const:  WITH conval DO
                  D0 := 0; D1 := 0; D2 := 0; D3 := 0; modNo := 0;
                END;
      | Typ:    mod := mainmod;
      | Var:    varpar := FALSE;
                vmod := 0; vlev := 0; vadr := 0; vpar := FALSE;
      | Field:  offset := 0;
      | Proc:   ALLOCATE(pd, SIZE(PDesc)); firstParam := NIL; firstLocal := NIL;
                pmod := 0; procType := modula2;
      | Tool:   cd := NIL; firstArg := NIL; std := Halt; cnum := 0;
                GSOSproc := FALSE;
      | Module: key := NIL; firstObj := NIL; root := NIL; modno := 0;
                typ := notyp; parentMod := 0; isInt := FALSE;
      | Temp:   baseref := 0;
      END;
    END;
    RETURN ob1
  END NewObj;

  PROCEDURE NewStr(frm: StrForm): StrPtr;
    VAR str: StrPtr;
  BEGIN ALLOCATE(str, SIZE(Structure));
    WITH str^ DO
      strobj := NIL; size := 0; ref := 0; form := frm;
      CASE frm OF
        Undef, Bool, Char, Card, CardInt, Int, Enum, LCard, Double,
        Real, LongReal, Opaque, String: |
        Range: RBaseTyp := undftyp; min := 0; max := 0; BndAdr := 0 |
        Pointer: PBaseTyp := undftyp |
        Set:     SBaseTyp := undftyp |
        Array:   ElemTyp := undftyp; IndexTyp := undftyp |
        Record:  firstFld := NIL |
        ProcTyp: firstPar := NIL; resTyp := NIL
      END
    END;
    RETURN str
  END NewStr;

  PROCEDURE NewImp(scope, obj: ObjPtr);
    VAR ob0, ob1, ob1L, ob1R: ObjPtr; d: INTEGER;
  BEGIN ob0 := scope; ob1 := ob0^.right; d := 1;
    LOOP
      IF ob1 # NIL THEN
        d := Diff(obj^.name, ob1^.name);
        IF d < 0 THEN ob0 := ob1; ob1 := ob1^.left
        ELSIF d > 0 THEN ob0 := ob1; ob1 := ob1^.right
        ELSIF (ob1^.class = Temp) (*exported by higher local module*) OR
              ((ob1^.class = obj^.class) AND 
               ob1^.exported) (*exported by definition module*) THEN
          ob1L := ob1^.left; ob1R := ob1^.right;
          ob1^ := obj^; ob1^.exported := TRUE;
          ob1^.left := ob1L; ob1^.right := ob1R; EXIT
        ELSE Mark(100); EXIT
        END
      ELSE (*insert copy of imported object*)
        ALLOCATE(ob1, SIZE(Object)); ob1^ := obj^;
        IF d < 0 THEN ob0^.left := ob1 ELSE ob0^.right := ob1 END;
        ob1^.left := NIL; ob1^.right := NIL; ob1^.exported := FALSE;

        IF (obj^.class = Typ) & (obj^.typ^.form = Enum) THEN
          (*import enumeration constants too*)
          ob0 := obj^.typ^.ConstLink;
          WHILE ob0 # NIL DO
            NewImp(scope, ob0); ob0 := ob0^.conval.prev
          END
        END;
        EXIT
      END
    END
  END NewImp;

  PROCEDURE NewPar(ident: CARDINAL; isvar: BOOLEAN; last: ParPtr): ParPtr;
    VAR par: ParPtr;
  BEGIN
    ALLOCATE(par, SIZE(Parameter));
    par^.name := ident;
    par^.varpar := isvar;
    par^.next := last;
    RETURN par;
  END NewPar;

  PROCEDURE NewScope(cl: ObjClass);
    VAR hd: ObjPtr;
  BEGIN ALLOCATE(hd, SIZE(Object));
    WITH hd^ DO
      name := 0; typ := NIL; class := Header; parent := NIL;
      left := topScope; right := NIL; last := hd; next := NIL; kind := cl
    END;
    topScope := hd
  END NewScope;

  PROCEDURE CloseScope;
  BEGIN topScope := topScope^.left
  END CloseScope;

  PROCEDURE CheckUDP(obj, node: ObjPtr);
    (*obj is newly defined type; check for undefined forward references
      pointing to this new type by traversing the tree*)
  BEGIN
    IF node # NIL THEN
      IF (node^.class = Typ) & (node^.typ^.form = Pointer) &
         (node^.typ^.PBaseTyp = undftyp) &
         (Diff(node^.typ^.BaseId, obj^.name) = 0) THEN
        node^.typ^.PBaseTyp := obj^.typ
      END;
      CheckUDP(obj, node^.left); CheckUDP(obj, node^.right)
    END
  END CheckUDP;

  PROCEDURE MarkHeap;
  BEGIN ALLOCATE(topScope^.heap, 0); topScope^.name := id
  END MarkHeap;

  PROCEDURE ReleaseHeap;
  BEGIN ResetHeap(topScope^.heap); id := topScope^.name
  END ReleaseHeap;

  PROCEDURE InitTableHandler;
  BEGIN topScope := universe; mainmod^.firstObj := NIL; ReleaseHeap;
  END InitTableHandler;

  PROCEDURE EnterTyp(VAR str: StrPtr; name: ARRAY OF CHAR;
                     frm: StrForm; sz: CARDINAL);
  BEGIN obj := NewObj(Enter(name), Typ); str := NewStr(frm);
    obj^.typ := str; str^.strobj := obj; str^.size := sz;
    obj^.exported := expo
  END EnterTyp;

  PROCEDURE EnterProc(name: ARRAY OF CHAR; num: Standard; res: StrPtr);
  BEGIN obj := NewObj(Enter(name), Tool);
    obj^.typ := res; obj^.std := num; obj^.exported := expo
  END EnterProc;

PROCEDURE InitM2TM;
BEGIN topScope := NIL; Scope := NIL;
  NewScope(Module); universe := topScope;
  undftyp := NewStr(Undef); undftyp^.size := 1;
  notyp := NewStr(Undef); notyp^.size := 0;
  stringtyp := NewStr(String); stringtyp^.size := 0;
  BBtyp := NewStr(Range); (*Bitset Basetyp*)
  ALLOCATE(mainmod, SIZE(Object));
  WITH mainmod^ DO
    parent := NIL; parentMod := MAX(CARDINAL);
    class := Module; modno := 0; typ := notyp; next := NIL; exported := FALSE;
    ALLOCATE(key, SIZE(Key))
  END;

  (*initialization of Universe*)
  expo := FALSE;
  EnterTyp(booltyp,  "BOOLEAN",  Bool,     2);
  EnterTyp(chartyp,  "CHAR",     Char,     1);
  EnterTyp(cardtyp,  "CARDINAL", Card,     2);
  EnterTyp(cardinttyp,".CARDINT",CardInt,  2);
  EnterTyp(inttyp,   "INTEGER",  Int,      2);
  EnterTyp(bitstyp,  "BITSET",   Set,      2);
  EnterTyp(lcardtyp, "LONGCARD", LCard,    4);
  EnterTyp(dbltyp,   "LONGINT",  Double,   4);
  EnterTyp(realtyp,  "REAL",     Real,     4);
  EnterTyp(lrltyp,   "LONGREAL", LongReal, 8);
  EnterTyp(proctyp,  "PROC",     ProcTyp,  4);

  (*initialization of module SYSTEM*)
  NewScope(Module);
  expo := TRUE;
  EnterTyp(bytetyp, "BYTE", Undef, 1);
  EnterTyp(wordtyp, "WORD", Undef, 2);
  EnterTyp(addrtyp, "ADDRESS", LCard, 4);
  EnterProc('ADR',    Adr,    addrtyp);
  EnterProc('TSIZE',  Tsize,  inttyp);
  EnterProc('INLINE', Inline, notyp);
  EnterProc('GETREG', Getreg, notyp);
  EnterProc('SETREG', Setreg, notyp);
  EnterProc('LONG',   Long,   dbltyp);
  EnterProc('SHIFT',  Shift,  inttyp);
  EnterProc('SHORT',  Short,  inttyp);
  EnterProc('NEWPROCESS', NewProcess, notyp);
  EnterProc('TRANSFER', Transfer, notyp);
  EnterProc('IOTRANSFER', IOTransfer, notyp);
  ALLOCATE(sysmod, SIZE(Object));
  WITH sysmod^ DO
    name := Enter("SYSTEM"); class := Module; modno := 0FFFFH; exported := FALSE;
    left := NIL; right := NIL; next := NIL; parent := NIL;
    firstObj := topScope^.right; root := topScope^.right;
    ALLOCATE(key, SIZE(Key))
  END;
  CloseScope;

  (* initialization of Universe continued *)
  expo := FALSE;

  obj := NewObj(Enter("FALSE"), Const);
  obj^.typ := booltyp; obj^.conval.B := FALSE;
  obj := NewObj(Enter("TRUE"), Const);
  obj^.typ := booltyp; obj^.conval.B := TRUE;
  obj := NewObj(Enter("NIL"), Const);
  obj^.typ := addrtyp; obj^.conval.D := NilVal;
  bitstyp^.SBaseTyp := BBtyp;
  WITH BBtyp^ DO
    RBaseTyp := cardtyp; min := 0; max := WordSize-1; size := 2;
  END;
  proctyp^.firstPar := NIL; proctyp^.resTyp := notyp;

  EnterProc('ABS',    Abs,    inttyp);
  EnterProc('CAP',    Cap,    chartyp);
  EnterProc('CHR',    Chr,    chartyp);
  EnterProc('DEC',    Dec,    notyp);
  EnterProc('DISPOSE',Dispose,notyp);
  EnterProc('EXCL',   Excl,   notyp);
  EnterProc('FLOAT',  Float,  realtyp);
  EnterProc('FLOATD', FloatD, lrltyp);
  EnterProc('HALT',   Halt,   notyp);
  EnterProc('HIGH',   High,   cardinttyp);
  EnterProc('INC',    Inc,    notyp);
  EnterProc('INCL',   Incl,   notyp);
  EnterProc('LONG',   Long,   dbltyp);
  EnterProc('MAX',    Max,    inttyp);
  EnterProc('MIN',    Min,    inttyp);
  EnterProc('NEW',    New,    notyp);
  EnterProc('ODD',    Odd,    booltyp);
  EnterProc('ORD',    Ord,    cardinttyp);
  EnterProc('SHORT',  Short,  inttyp);
  EnterProc('SIZE',   Size,   cardinttyp);                            (* V2.6 *)
  EnterProc('TRUNC',  Trunc,  inttyp);
  EnterProc('TRUNCD', TruncD, dbltyp);
  EnterProc('VAL',    Val,    inttyp);
  MarkHeap
END InitM2TM;

END M2TM.
 
