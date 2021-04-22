(*$Segment M2RM*)
(*$RangeCheck+*)
(*$OverflowCheck+*)
IMPLEMENTATION MODULE M2RM; (*JG 2.11.85 / NW 17.12.85 / WH 14.7.86*)

FROM EZFileSystem IMPORT
  GetFileType, SetFileType;
FROM EZPaths IMPORT FindFileInPath;
(*XXX*)
FROM FastFileSystem IMPORT
  LookupNew, Load, Purge, ReadChar, File, ReadWord, Response, Close, WriteWord,
  WriteChar;
FROM InOut IMPORT WriteString, WriteLn;
FROM M2Debug IMPORT GenLine;
FROM M2DM IMPORT ObjClass, Object, ObjPtr, StrForm, Structure, StrPtr, aAdr,
  Parameter, ParPtr, PDesc, Key, KeyPtr, undftyp, booltyp, chartyp, inttyp,
  cardtyp, dbltyp, lcardtyp, realtyp, lrltyp, stringtyp, bytetyp, wordtyp,
  addrtyp, bitstyp, proctyp, notyp, mainmod, ALLOCATE, ResetHeap, Standard,
  NewHeap, RevertToMainHeap, ResizeHeap, language, aSize;
FROM M2Lib IMPORT LoWORD, HighWORD, LongWORD;
FROM M2LM IMPORT pc, AllocString, AllocBounds, AllocGlobalVar,
  globalStringList;
FROM M2Shell IMPORT DebugCode, ShowProgress, terminalError, SaveToDisk;
FROM M2SM IMPORT GetSourcePos, IdBuf, id, Diff, Mark, ReportBadKey, 
   sourceLine, versionHandle, Enter, IdBufLeng;
FROM Strings IMPORT Assign, Length;
FROM SYSTEM IMPORT WORD, ADR;

CONST
  REFFILE = 334B;
  CTL = -5000B; anchor = 0; ModTag = 1; ProcTag = 2; RefTag = 3; linkage = 4;
        lineno = 5;
  STR = -6000B; enum = 0; range = 1; pointer = 2; set = 3; procTyp = 4;
        funcTyp = 5; array = 6; dynarr = 7; record = 8; opaque = 9;
  CMP = -7000B; parref = 0; par = 1; field = 2;
  OBJ = -10000B; varref = 0; var = 1; const = 2; string = 3; type = 4;
        proc = 5; func = 6; module = 7; svc = 8; svcfunc = 9;
  maxM = 64; minS = 32 (*first non-standard structure*); maxS = 1024;

VAR
  CurStr:           CARDINAL;
  err:              BOOLEAN;
  Temps, Fields:    ObjPtr;
  Params, lastPar:  ParPtr;
  oldPos:           LONGINT;
  ff:               File;
  fixedPath:        ARRAY [0..20] OF CHAR;
  environmentPath:  ARRAY [0..20] OF CHAR;
  pathname:         ARRAY [0..511] OF CHAR;
  RefFile:          File;

  PROCEDURE ReadId;
  VAR
    i, L: CARDINAL;
    ch:   CHAR;
  BEGIN i := id;
    IF i = IdBufLeng THEN
      Mark(503);
    ELSE
      ReadChar(ff, ch);
      IdBuf[i] := ch;
      INC(i);
      L := ORD(ch)-1;

      WHILE (L > 0) AND (i < IdBufLeng) DO
        ReadChar(ff, ch);
        IdBuf[i] := ch;
        INC(i);
        DEC(L);
      END;

      IF i < IdBufLeng THEN
        id := i;
      ELSE
        Mark(503);
      END;
    END;
  END ReadId;

  PROCEDURE InitRef;
  BEGIN
    WITH mainmod^ DO
      left := NIL;
      right := NIL;
      next := NIL;
    END;

    (*
      The structures that are initialised here, are actually allocated in
      InitM2RM.  This avoid the problem of allocating a copy each time the
      compiler runs.
    *)
    ALLOCATE(ModList, SIZE(Object));
    ALLOCATE(Temps, SIZE(Object));
    ALLOCATE(Fields, SIZE(Object));
    ALLOCATE(Params, SIZE(Parameter));

    WITH ModList^ DO
      class := Header;
      next := mainmod;
      last := mainmod;
      left := NIL;
      right := NIL;
      parent := NIL;
    END;

    ModNo := 1;
    WITH Temps^ DO
      class := Header;
      next := NIL;
      last := Temps;
      left := NIL;
      right := NIL;
      parent := NIL;
    END;

    WITH Fields^ DO
      class := Header;
      next := NIL;
      last := Fields;
      left := NIL;
      right := NIL;
      parent := NIL;
    END;

    Params^.next := NIL;
    lastPar := Params
  END InitRef;

  PROCEDURE Insert(root, obj: ObjPtr): ObjPtr;
  VAR
    ob0, ob1: ObjPtr;
    d:        INTEGER;
  BEGIN ob0 := root; ob1 := ob0^.right; d := 1;
    LOOP
      IF ob1 # NIL THEN
        d := Diff(obj^.name, ob1^.name);
        IF d < 0 THEN ob0 := ob1; ob1 := ob1^.left
        ELSIF d > 0 THEN ob0 := ob1; ob1 := ob1^.right
        ELSE EXIT
        END
      ELSE ob1 := obj;
        IF d < 0 THEN ob0^.left := ob1 ELSE ob0^.right := ob1 END ;
        ob1^.left := NIL; ob1^.right := NIL; EXIT
      END
    END;
    RETURN ob1
  END Insert;

  PROCEDURE InRef(VAR filename: ARRAY OF CHAR; VAR hdr: ObjPtr;
                  VAR adr: aAdr; VAR pno: CARDINAL);
  VAR
    GlbMod:         ARRAY [0..maxM] OF ObjPtr;
    Struct:         ARRAY [0..maxS] OF StrPtr;
    CurMod:         CARDINAL;
    FileType:       CARDINAL;
    id0, s, m, p:   CARDINAL;
    block:          INTEGER;
    lev0, newobj, obj: ObjPtr;
    newpar:         ParPtr;
    newstr:         StrPtr;
    ch:             CHAR;
    OK:             BOOLEAN;
    tempWord:       CARDINAL;
    tempWord2:      CARDINAL;
    idx:            CARDINAL;
    ftype:          CARDINAL;
    aux:            LONGINT;
    fVersion:       LONGINT;
  BEGIN
    FindFileInPath(filename, fixedPath, FALSE, pathname, OK);

    IF NOT OK THEN
      FindFileInPath(filename, environmentPath, TRUE, pathname, OK);
    END;

    IF OK THEN
      GetFileType(pathname, ftype, aux, OK);

      IF (ftype = SymRefFileType) AND
         ((aux = VAL(LONGINT, SymAuxType)) OR
          (aux = VAL(LONGINT, RefAuxType))) THEN
        (*XXX*)
        Load(ff, pathname);
(*
        Lookup(ff, pathname, FALSE);
*)
      ELSE
        ff.res := notdone;
        Mark(508);
      END;
    ELSE
      ff.res := notdone;
    END;

    IF ff.res = done THEN
      IF ShowProgress THEN
        WriteString(pathname);
        WriteLn;
      END;

      NewHeap;  (* create a new heap for this import list *)

      ReadWord(ff, FileType);

      IF FileType = REFFILE THEN
        ReadWord(ff, tempWord);  (* read the symbol file version number *)
        ReadWord(ff, tempWord2);
        fVersion := LongWORD(tempWord, tempWord2);

        (*
          if the symbol file was not generated by this version of the compiler
          then, we can't trust it.
        *)
        IF fVersion <> versionHandle^^.version THEN
          Mark(507);
        END;

        Struct[1] := undftyp; Struct[2] := booltyp; Struct[3] := chartyp;
        Struct[4] := inttyp; Struct[5] := cardtyp; Struct[6] := dbltyp;
        Struct[7] := realtyp; Struct[8] := lrltyp; Struct[9] := bitstyp;
        Struct[10] := proctyp; Struct[11] := stringtyp;
        Struct[12] := addrtyp; Struct[13] := bytetyp; Struct[14] := wordtyp;
        Struct[15] := lcardtyp;
        CurMod := 0; CurStr := minS; err := FALSE;
        id0 := id; ALLOCATE(lev0, 0);

        LOOP
          ReadWord(ff, block);

          IF block < CMP THEN block := block - OBJ;
            IF block > svcfunc THEN err := TRUE; Mark(502); EXIT END;
            ALLOCATE(newobj, SIZE(Object)); m := 0;
            WITH newobj^ DO next := NIL;
              parent := NIL;

              CASE block OF
                var    : class := Var; ReadWord(ff, s); typ := Struct[s];
                         varpar := FALSE; vmod := GlbMod[0]^.right^.modno;
                         ReadWord(ff, vlev); ReadWord(ff, vadr)
              | const  : class := Const; ReadWord(ff, s); typ := Struct[s];
                         ReadWord(ff, m);
                         ReadChar(ff, ch);
                         ReadWord(ff, conval.strChar);

                         IF typ^.form <> Set THEN
                           CASE ORD(ch) OF
                             2: ReadChar(ff, ch); conval.Ch := ch;
                           | 3: ReadWord(ff, conval.I);
                           | 5: ReadWord(ff, conval.D0); 
                                ReadWord(ff, conval.D1);
                           | 9: ReadWord(ff, conval.D0); 
                                ReadWord(ff, conval.D1);
                                ReadWord(ff, conval.D2); 
                                ReadWord(ff, conval.D3);
                           END;
                         ELSE (* a set constant *)
                           conval.FS.setSize := typ^.size;

                           FOR idx := 0 TO (VAL(CARDINAL, typ^.size) - 1) DIV 2 DO
                             ReadWord(ff, conval.FS.set[idx]);
                           END;
                         END;

                         conval.modNo := GlbMod[0]^.right^.modno;
              | string : class := Const; ReadWord(ff, s); typ := Struct[s];
                         conval.D2 := id; ReadId;
                         AllocString(globalStringList, conval.D2, conval.D0,
                                     conval.D1);
                         conval.D3 := 0; conval.strChar := FALSE;
                         conval.modNo := GlbMod[0]^.right^.modno;
              | type   : class := Typ; ReadWord(ff, s); typ := Struct[s];
                         IF typ^.strobj = NIL THEN typ^.strobj := newobj END;
                         ReadWord(ff, m); mod := GlbMod[m]^.right
              | proc, func : class := Proc;
                         IF block = func THEN
                           ReadWord(ff, s);
                           typ := Struct[s]
                         ELSE
                           typ := notyp
                         END;

                         ALLOCATE(pd, SIZE(PDesc));

                         ReadWord(ff, pd^.num);
                         ReadWord(ff, pd^.lev);
                         ReadWord(ff, pd^.adr);
                         ReadWord(ff, pd^.size);

                         pd^.forward := FALSE;
                         pd^.exp := FALSE;
                       (*pd^.extern := TRUE; pd^.link := 0;  (*NS*)*)
                         firstLocal := NIL;
                         firstParam := Params^.next;
                         Params^.next := NIL;
                         lastPar := Params;
                         pmod := GlbMod[0]^.right^.modno;
                         ReadChar(ff, VAL(CHAR, procType));
              | svc, svcfunc: class := Tool;
                         IF block = svcfunc THEN
                           ReadWord(ff, s);
                           typ := Struct[s]
                         ELSE
                           typ := notyp
                         END;

                         ReadWord(ff, cnum);
                         std := NonStand;
                         firstArg := Params^.next;
                         Params^.next := NIL; 
                         lastPar := Params;
                         ReadWord(ff, GSOSproc);
              END;

              name := id;
              ReadId;

              exported := TRUE;
              obj := Insert(GlbMod[m]^.right, newobj);

              IF block = var THEN
                AllocGlobalVar(newobj);
              END;

              IF obj = newobj THEN (*new object*)
                GlbMod[m]^.last^.next := newobj; GlbMod[m]^.last := newobj;
                IF (class = Const) & (typ^.form = Enum) THEN
                  conval.prev := typ^.ConstLink; typ^.ConstLink := newobj
                END;
                id0 := id; ALLOCATE(lev0, 0)
              ELSE
                IF obj^.class = Typ THEN Struct[s] := obj^.typ END;
                id := id0; ResetHeap(lev0)
              END
            END
          ELSIF block < STR THEN block := block - CMP;
            IF block > field THEN err := TRUE; Mark(502); EXIT END;
            IF block = field THEN
              ALLOCATE(newobj, SIZE(Object));
              WITH newobj^ DO
                class := Field; next := NIL; parent := NIL;
                ReadWord(ff, s); typ := Struct[s];
                ReadWord(ff, offset); name := id; ReadId;
                newobj := Insert(Fields, newobj)
              END;
              Fields^.last^.next := newobj; Fields^.last := newobj
            ELSE (*parameter*)
              ALLOCATE(newpar, SIZE(Parameter));
              WITH newpar^ DO
                next := NIL; ReadWord(ff, s); typ := Struct[s];
                varpar := block = parref;
                lastPar^.next := newpar; lastPar := newpar
              END
            END
          ELSIF block < CTL THEN block := block - STR;
            IF block > opaque THEN err := TRUE; Mark(502); EXIT END;
            ALLOCATE(newstr, SIZE(Structure));
            WITH newstr^ DO
              strobj := NIL;
              ReadWord(ff, tempWord);
              size := VAL(aSize, tempWord);
              ref := 0;

              CASE block OF
                enum    : form := Enum; ReadWord(ff, NofConst);
                          ConstLink := NIL
              | range   : form := Range;
                          ReadWord(ff, s); RBaseTyp := Struct[s];
                          ReadWord(ff, tempWord); ReadWord(ff, tempWord2);
                          min := tempWord;  max := tempWord2;
                          AllocBounds(VAL(INTEGER, tempWord),
                                      VAL(INTEGER, tempWord2), size, BndAdr);
              | pointer : form := Pointer; PBaseTyp := NIL;
                          BaseId := 0
              | set     : form := Set; ReadWord(ff, s);
                          SBaseTyp := Struct[s]
              | procTyp, funcTyp : form := ProcTyp;
                          IF block = funcTyp THEN
                            ReadWord(ff, s); resTyp := Struct[s]
                          ELSE resTyp := notyp
                          END;
                          firstPar := Params^.next;
                          Params^.next := NIL; lastPar := Params
              | array   : form := Array; ReadWord(ff, s);
                          ElemTyp := Struct[s]; dyn := FALSE;
                          ReadWord(ff, s); IndexTyp := Struct[s]
              | dynarr  : form := Array; ReadWord(ff, s);
                          ElemTyp := Struct[s]; dyn := TRUE;
                          IndexTyp := NIL
              | record  : form := Record;
                          firstFld := Fields^.right; Fields^.right := NIL;
                          Fields^.next := NIL; Fields^.last := Fields
              | opaque  : form := Opaque
              END
            END;
            IF CurStr > maxS THEN err := TRUE; Mark(98); EXIT END;
            Struct[CurStr] := newstr;
            CurStr := CurStr + 1;
            ALLOCATE(lev0, 0);
          ELSIF block < 0 THEN block := block - CTL;
            IF block = linkage THEN ReadWord(ff, s); ReadWord(ff, p);
              IF Struct[p]^.PBaseTyp # NIL THEN
                id := id0; ResetHeap(lev0)
              ELSE Struct[p]^.PBaseTyp := Struct[s];
                id0 := id; ALLOCATE(lev0, 0)
              END
            ELSIF block = ModTag THEN (*main module*) ReadWord(ff, m);
            ELSIF block = anchor THEN
              ALLOCATE(newobj, SIZE(Object));
              WITH newobj^ DO
                class := Module; typ := NIL; left := NIL; right := NIL;
                ALLOCATE(key, SIZE(Key));  parent := NIL;
                ReadWord(ff, key^.k0); ReadWord(ff, key^.k1); ReadWord(ff, key^.k2);
                firstObj := NIL; root := NIL; name := id; ReadId;
                ReadWord(ff, isInt);
                parentMod := 0;
              END;
              IF CurMod > maxM THEN Mark(96); EXIT END;
              ALLOCATE(GlbMod[CurMod], SIZE(Object));
              id0 := id; ALLOCATE(lev0, 0);
              WITH GlbMod[CurMod]^ DO
                class := Header; kind := Module; typ := NIL;
                next := NIL; left := NIL; last := GlbMod[CurMod];
                obj := ModList^.next; (*find mod*) parent := NIL;
                WHILE (obj # NIL) & (Diff(obj^.name, newobj^.name) # 0) DO
                  obj := obj^.next
                END;
                IF obj # NIL THEN GlbMod[CurMod]^.right := obj;
                  IF (CurMod = 0) & (obj = mainmod) THEN
                    (*newobj is own definition module*)
                    obj^.key^ := newobj^.key^
                  ELSIF (obj^.key^.k0 # newobj^.key^.k0)
                     OR (obj^.key^.k1 # newobj^.key^.k1)
                     OR (obj^.key^.k2 # newobj^.key^.k2) THEN
                    Mark(85);
                    ReportBadKey(obj, newobj, filename);
                  ELSIF (CurMod = 0) & (obj^.firstObj # NIL) THEN
                    CurMod := 1; EXIT (*module already loaded*)
                  END;
                  id := id0; ResetHeap(lev0)
                ELSE GlbMod[CurMod]^.right := newobj;
                  newobj^.next := NIL; newobj^.modno := ModNo; INC(ModNo);
                  ModList^.last^.next := newobj; ModList^.last := newobj;
                  id0 := id; ALLOCATE(lev0, 0);
                END
              END;
              CurMod := CurMod + 1
            ELSIF block = RefTag THEN
              ReadWord(ff, adr); ReadWord(ff, pno); EXIT
            ELSE err := TRUE; Mark(502); EXIT
            END
          ELSE (*line block*) err := TRUE; Mark(502); EXIT
          END;
        END;

        IF NOT err & (CurMod # 0) THEN hdr := GlbMod[0];
          hdr^.right^.root := hdr^.right^.right;
          (*leave hdr^.right.right for later searches*)
          hdr^.right^.firstObj := hdr^.next;
        ELSE hdr := NIL
        END
      ELSE Mark(502); hdr := NIL
      END;

      (*XXX*)
      Purge(ff);

      ResizeHeap;  (* free up any unwanted memory *)
      RevertToMainHeap;
    ELSE
      IF ShowProgress THEN
        WriteString(filename);
        WriteLn;
      END;

      Mark(501);
      hdr := NIL;
    END;
  END InRef;

  PROCEDURE WriteId(i: CARDINAL);
    VAR L: CARDINAL;
  BEGIN L := ORD(IdBuf[i]);
    REPEAT WriteChar(RefFile, IdBuf[i]); INC(i); DEC(L)
    UNTIL L = 0
  END WriteId;

  PROCEDURE OpenRef(refName: ARRAY OF CHAR);
  VAR
    obj:  ObjPtr;
  BEGIN
    (*XXX*)
    LookupNew(RefFile, refName);
(*
    Lookup(RefFile, refName, TRUE);
*)
    IF RefFile.res = notdone THEN
      terminalError := TRUE;
    ELSE
      WriteWord(RefFile, REFFILE);
      WriteWord(RefFile, LoWORD(versionHandle^^.version));
      WriteWord(RefFile, HighWORD(versionHandle^^.version));

      obj := ModList^.next;

      WHILE obj # NIL DO
        WriteWord(RefFile, LoWORD(CTL+anchor));
        WITH obj^ DO
          WriteWord(RefFile, key^.k0);
          WriteWord(RefFile, key^.k1);
          WriteWord(RefFile, key^.k2);
          WriteId(name);
          WriteWord(RefFile, VAL(INTEGER, isInt));
        END;

        obj := obj^.next
      END;

      CurStr := minS;
      oldPos := 0
    END;
  END OpenRef;
  
  PROCEDURE OutPar(prm: ParPtr);
  BEGIN
    WHILE prm # NIL DO (*out param*)
      WITH prm^ DO
        IF varpar THEN WriteWord(RefFile, LoWORD(CMP+parref))
          ELSE WriteWord(RefFile, LoWORD(CMP+par))
        END;
        WriteWord(RefFile, typ^.ref)
      END;        
      prm := prm^.next
    END
  END OutPar;

  PROCEDURE OutStr(str: StrPtr);
    VAR obj: ObjPtr; par: ParPtr;

    PROCEDURE OutFldStrs(fld: ObjPtr);
    BEGIN
      WHILE fld # NIL DO
        IF fld^.typ^.ref = 0 THEN OutStr(fld^.typ) END;
        fld := fld^.next
      END
    END OutFldStrs;

    PROCEDURE OutFlds(fld: ObjPtr);
    BEGIN
      WHILE fld # NIL DO
        WITH fld^ DO
          WriteWord(RefFile, LoWORD(CMP+field)); WriteWord(RefFile, typ^.ref);
          WriteWord(RefFile, offset); WriteId(name)
        END;
        fld := fld^.next
      END
    END OutFlds;

  BEGIN
    WITH str^ DO
      CASE form OF
        Enum    : WriteWord(RefFile, LoWORD(STR+enum));
                  WriteWord(RefFile, SHORT(size));
                  WriteWord(RefFile, NofConst);
      | Range   : IF RBaseTyp^.ref = 0 THEN OutStr(RBaseTyp) END;
                  WriteWord(RefFile, LoWORD(STR+range));
                  WriteWord(RefFile, SHORT(size));
                  WriteWord(RefFile, RBaseTyp^.ref);
                  WriteWord(RefFile, SHORT(min));
                  WriteWord(RefFile, SHORT(max));
      | Pointer : ALLOCATE(obj, SIZE(Object));
                  WITH obj^ DO left := NIL; next := NIL; parent := NIL;
                    class := Temp; typ := PBaseTyp; baseref := CurStr;
                    Temps^.last^.next := obj; Temps^.last := obj
                  END;
                  WriteWord(RefFile, LoWORD(STR+pointer));
                  WriteWord(RefFile, SHORT(size));
      | Set     : IF SBaseTyp^.ref = 0 THEN OutStr(SBaseTyp) END;
                  WriteWord(RefFile, LoWORD(STR+set));
                  WriteWord(RefFile, SHORT(size));
                  WriteWord(RefFile, SBaseTyp^.ref)
      | ProcTyp : par := firstPar;
                  WHILE par # NIL DO (*out param structure*)
                    IF par^.typ^.ref = 0 THEN OutStr(par^.typ) END;
                    par := par^.next
                  END;
                  OutPar(firstPar);
                  IF resTyp # notyp THEN
                    IF resTyp^.ref = 0 THEN OutStr(resTyp) END;
                    WriteWord(RefFile, LoWORD(STR+funcTyp));
                    WriteWord(RefFile, SHORT(size));
                    WriteWord(RefFile, resTyp^.ref)
                  ELSE 
                    WriteWord(RefFile, LoWORD(STR+procTyp));
                    WriteWord(RefFile, SHORT(size));
                  END
      | Array   : IF ElemTyp^.ref = 0 THEN OutStr(ElemTyp) END;
                  IF dyn THEN WriteWord(RefFile, LoWORD(STR+dynarr));
                    WriteWord(RefFile, VAL(CARDINAL, size));
                    WriteWord(RefFile, ElemTyp^.ref)
                  ELSE
                    IF IndexTyp^.ref = 0 THEN OutStr(IndexTyp) END;
                    WriteWord(RefFile, LoWORD(STR+array));
                    WriteWord(RefFile, VAL(CARDINAL, size));
                    WriteWord(RefFile, ElemTyp^.ref);
                    WriteWord(RefFile, IndexTyp^.ref)
                  END
      | Record  : OutFldStrs(firstFld); OutFlds(firstFld);
                  WriteWord(RefFile, LoWORD(STR+record));
                  WriteWord(RefFile, VAL(CARDINAL, size));
      | Opaque  : WriteWord(RefFile, LoWORD(STR+opaque));
                  WriteWord(RefFile, VAL(CARDINAL, size));
      END;
      ref := CurStr; CurStr := CurStr + 1
    END
  END OutStr;

  PROCEDURE OutExt(str: StrPtr);
    VAR obj: ObjPtr; par: ParPtr;

    PROCEDURE OutFlds(fld: ObjPtr);
    BEGIN
      WHILE fld # NIL DO
        IF fld^.typ^.ref = 0 THEN OutExt(fld^.typ) END;
        fld := fld^.next
      END
    END OutFlds;

  BEGIN
    WITH str^ DO
      CASE form OF
        Range   : IF RBaseTyp^.ref = 0 THEN OutExt(RBaseTyp) END
      | Set     : IF SBaseTyp^.ref = 0 THEN OutExt(SBaseTyp) END
      | ProcTyp : par := firstPar;
                  WHILE par # NIL DO
                    IF par^.typ^.ref = 0 THEN OutExt(par^.typ) END;
                    par := par^.next
                  END;
                  IF (resTyp # notyp) & (resTyp^.ref = 0) THEN OutExt(resTyp) END
      | Array   : IF ElemTyp^.ref = 0 THEN OutExt(ElemTyp) END;
                  IF NOT dyn THEN OutExt(IndexTyp) END
      | Record  : OutFlds(firstFld)
      | Enum, Pointer, Opaque :
      END;
      IF (strobj # NIL) & (strobj^.mod^.modno # 0) THEN
        IF ref = 0 THEN OutStr(str) END;
        IF form = Enum THEN obj := ConstLink;
          WHILE obj # NIL DO
            WriteWord(RefFile, LoWORD(OBJ+const));
            WriteWord(RefFile, ref);
            WriteWord(RefFile, strobj^.mod^.modno);
            WriteChar(RefFile, 2C); 
            WriteWord(RefFile, FALSE);
            WriteChar(RefFile, obj^.conval.Ch);
            WriteId(obj^.name);
            obj := obj^.conval.prev
          END
        END;
        WriteWord(RefFile, LoWORD(OBJ+type));
        WriteWord(RefFile, ref);
        WriteWord(RefFile, strobj^.mod^.modno);
        WriteId(strobj^.name)
      END
    END
  END OutExt;

  PROCEDURE OutObj(obj: ObjPtr);
  VAR 
    par:    ParPtr;
    idx:    CARDINAL;
  BEGIN
    WITH obj^ DO
      CASE class OF
        Module : WriteWord(RefFile, LoWORD(OBJ+module)); WriteWord(RefFile, modno)
      | Proc   : par := firstParam;
                 WHILE par # NIL DO
                   IF par^.typ^.ref = 0 THEN
                     OutExt(par^.typ);
                   END;

                   par := par^.next
                 END;

                 IF (typ # notyp) & (typ^.ref = 0) THEN
                   OutExt(typ);
                 END;

                 par := firstParam;

                 WHILE par # NIL DO (*out param structure*)
                   IF par^.typ^.ref = 0 THEN OutStr(par^.typ) END;
                   par := par^.next
                 END;

                 IF (typ # notyp) & (typ^.ref = 0) THEN
                   OutStr(typ);
                 END;

                 OutPar(firstParam);

                 IF typ # notyp THEN
                   WriteWord(RefFile, LoWORD(OBJ+func));
                   WriteWord(RefFile, typ^.ref);
                 ELSE
                   WriteWord(RefFile, LoWORD(OBJ+proc));
                 END;

                 WriteWord(RefFile, pd^.num);
                 WriteWord(RefFile, pd^.lev);
                 WriteWord(RefFile, pd^.adr);
                 WriteWord(RefFile, pd^.size);
                 WriteChar(RefFile, VAL(CHAR, procType));
      | Tool   : par := firstArg;
                 WHILE par # NIL DO
                   IF par^.typ^.ref = 0 THEN OutExt(par^.typ) END;
                   par := par^.next
                 END;
                 IF (typ # notyp) & (typ^.ref = 0) THEN OutExt(typ) END;
                 par := firstArg;
                 WHILE par # NIL DO (*out param structure*)
                   IF par^.typ^.ref = 0 THEN OutStr(par^.typ) END;
                   par := par^.next
                 END;
                 IF (typ # notyp) & (typ^.ref = 0) THEN OutStr(typ) END;
                 OutPar(firstArg);
                 IF typ # notyp THEN
                   WriteWord(RefFile, LoWORD(OBJ+svcfunc)); WriteWord(RefFile, typ^.ref)
                 ELSE WriteWord(RefFile, LoWORD(OBJ+svc))
                 END;
                 WriteWord(RefFile,VAL(INTEGER,cnum));
                 WriteWord(RefFile,VAL(INTEGER,GSOSproc));
      | Const  : IF typ^.ref = 0 THEN OutExt(typ) END;
                 IF typ^.ref = 0 THEN OutStr(typ) END;
                 IF typ^.form = String THEN 
                   WriteWord(RefFile, LoWORD(OBJ+string));
                   WriteWord(RefFile, typ^.ref); WriteId(conval.D2)
                 ELSE WriteWord(RefFile, LoWORD(OBJ+const));
                   WriteWord(RefFile, typ^.ref);
                   WriteWord(RefFile, 0); (*main*)
                   WriteChar(RefFile, VAL(CHAR, typ^.size + VAL(aSize, 1)));
                   WriteWord(RefFile, conval.strChar);

                   IF typ^.form <> Set THEN
                     CASE VAL(CARDINAL, typ^.size) OF
                       1: WriteChar(RefFile, conval.Ch);
                     | 2: WriteWord(RefFile, conval.I);
                     | 4: WriteWord(RefFile, conval.D0);
                          WriteWord(RefFile, conval.D1);
                     | 8: WriteWord(RefFile, conval.D0);
                          WriteWord(RefFile, conval.D1);
                          WriteWord(RefFile, conval.D2);
                          WriteWord(RefFile, conval.D3);
                     END;
                   ELSE (* a set constant *)
                     FOR idx := 0 TO (VAL(CARDINAL, typ^.size) - 1) DIV 2 DO
                       WriteWord(RefFile, conval.FS.set[idx]);
                     END;
                   END;
                 END
      | Typ    : IF typ^.ref = 0 THEN OutExt(typ) END;
                 IF typ^.ref = 0 THEN OutStr(typ) END;
                 WriteWord(RefFile, LoWORD(OBJ+type));
                 WriteWord(RefFile, typ^.ref); WriteWord(RefFile, 0) (*main*)
      | Var    : IF typ^.ref = 0 THEN OutExt(typ) END;
                 IF typ^.ref = 0 THEN OutStr(typ) END;
                 IF varpar THEN WriteWord(RefFile, LoWORD(OBJ+varref))
                   ELSE WriteWord(RefFile, LoWORD(OBJ+var))
                 END;
                 WriteWord(RefFile, typ^.ref);
                 WriteWord(RefFile, vlev); WriteWord(RefFile, vadr)
      | Temp   :
      END;

      WriteId(name);
    END
  END OutObj;

  PROCEDURE OutLink;
    VAR obj: ObjPtr;
  BEGIN obj := Temps^.next;
    WHILE obj # NIL DO
      WITH obj^ DO
        IF typ^.ref = 0 THEN OutExt(typ) END;
        IF typ^.ref = 0 THEN OutStr(typ) END;
        WriteWord(RefFile, LoWORD(CTL+linkage));
        WriteWord(RefFile, typ^.ref);
        WriteWord(RefFile, baseref)
      END;
      obj := obj^.next
    END;
    Temps^.next := NIL; Temps^.last := Temps
  END OutLink;
 
  PROCEDURE OutUnit(unit: ObjPtr);
    VAR lev0, obj: ObjPtr;
  BEGIN ALLOCATE(lev0, 0);
    IF unit^.class = Proc THEN obj := unit^.firstLocal;
      WHILE obj # NIL DO OutObj(obj); obj := obj^.next END;
      OutLink;
      WriteWord(RefFile, LoWORD(CTL+ProcTag));
      WriteWord(RefFile, unit^.pd^.num);
    ELSIF unit^.class = Module THEN obj := unit^.firstObj;
      WHILE obj # NIL DO OutObj(obj); obj := obj^.next END;
      OutLink;
      WriteWord(RefFile, LoWORD(CTL+ModTag));
      WriteWord(RefFile, unit^.modno)
    END;
    ResetHeap(lev0)
  END OutUnit;

  PROCEDURE OutName(name: ARRAY OF CHAR);
  VAR
    idx:  CARDINAL;
    len:  CARDINAL;
  BEGIN
    WriteWord(RefFile, OBJ+proc);

    len := Length(name)-1;

    FOR idx := 0 TO len DO
      WriteChar(RefFile, name[idx]);
    END;
  END OutName;

  PROCEDURE RefPoint;
    VAR p0, p1: CARDINAL;
  BEGIN 
    WriteWord(RefFile, CTL+lineno);

    GetSourcePos(p0, p1);
    WriteWord(RefFile, pc);           (* code offset of reference point *)
    WriteWord(RefFile, p0);           (* source position in file *)
    WriteWord(RefFile, p1);
    WriteWord(RefFile, sourceLine);   (* source line number *)

    IF DebugCode THEN
      GenLine(sourceLine);
    END;
  END RefPoint;

  PROCEDURE CloseRef(adr:     aAdr;
                     pno:     CARDINAL;
                     auxType: LONGINT;
                     fileOK:  BOOLEAN);
  VAR
    OK: BOOLEAN;
  BEGIN
    IF fileOK THEN
      WriteWord(RefFile, LoWORD(CTL+RefTag));
      WriteWord(RefFile, adr);
      WriteWord(RefFile, pno);

      (*XXX*)
      Close(RefFile, FALSE, SaveToDisk);
(*
      Close(RefFile);
*)
      IF RefFile.res # done THEN
        Mark(506);
      ELSIF SaveToDisk THEN
        SetFileType(RefFile.nameString.text, SymRefFileType, auxType, OK);
  
        IF NOT OK THEN
          Mark(506);
        END;
      END;
    ELSE
      (*XXX*)
      Close(RefFile, TRUE, FALSE);
(*
      Close(RefFile);
*)
      IF RefFile.res # done THEN
        Mark(506);
      END;
    END;
  END CloseRef;

PROCEDURE InitM2RM;
BEGIN
  undftyp^.ref := 1; booltyp^.ref := 2; chartyp^.ref := 3; inttyp^.ref := 4;
  cardtyp^.ref := 5; dbltyp^.ref := 6; realtyp^.ref := 7; lrltyp^.ref := 8;
  bitstyp^.ref := 9; proctyp^.ref := 10; stringtyp^.ref := 11;
  addrtyp^.ref := 12; bytetyp^.ref := 13; wordtyp^.ref := 14;  (*NS*)
  lcardtyp^.ref := 15;

  Assign("8::13/m2defs", fixedPath);
  Assign("m2sym", environmentPath);
(*
  ALLOCATE(ModList, SIZE(Object));
  ALLOCATE(Temps, SIZE(Object));
  ALLOCATE(Fields, SIZE(Object));
  ALLOCATE(Params, SIZE(Parameter));
*)
END InitM2RM;

END M2RM.
