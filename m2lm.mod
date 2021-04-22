(*$Segment M2LM*)
(*$RangeCheck+*)
(*$OverflowCheck+*)
IMPLEMENTATION MODULE M2LM; (* HS 20.5.85 / WH 2.8.85 *)

FROM Common IMPORT String255, ConvStrToPStr;
FROM EZFileSystem IMPORT MakeFileName;
FROM M2DM IMPORT
  ObjPtr, StrPtr, KeyPtr, ObjClass, mainmod, aAdr, StrForm, stringtyp,
  sObjClass, language;
FROM M2HM IMPORT
  Item, ItemMode, maxLev;
FROM M2OMF IMPORT NewLabel, FixLabel, GenData, PutByte, NewSegmentHeader,
  objFile, GenDS, EndSegment, aSegmentKind, aSegmentAttribute, CloseOMFFile,
  aSegmentAttributeSet, ReferenceLabel, PutLong, GenLocalOp, PutWord,
  OpenNewOMFFile, PutByteConstant, PutWordConstant, PutLongConstant,
  IncrementSegmentLength, LabelDisplacement, GenGlobalOp;
FROM M2Shell IMPORT
  KeepObject, SaveToDisk, userHasAborted, objectFileInError;
FROM M2SM IMPORT
  Mark, IdBuf, Diff, scanerr, FindModule, aDirective, Directives;
FROM M2TM IMPORT
  FindInScope;
FROM NumberConversion IMPORT
  CardToString, NumToString;
FROM Storage IMPORT
  ALLOCATE, DEALLOCATE;
FROM Strings IMPORT
  Assign, Concat, Length, Insert;
FROM SYSTEM IMPORT
  BYTE, WORD, TSIZE, ADR;
FROM W65C816 IMPORT
  BRA, BRL;

IMPORT SYSTEM;

CONST
  MaxCodeSize       = 65535;
  MaxStringLength   = 255;

TYPE
  pStringListEntry  = POINTER TO aStringListEntry;
  aStringListEntry  =
    RECORD
      nextString:     pStringListEntry;
      theString:      ARRAY [0..MaxStringLength-1] OF CHAR;
      labelNumber:    CARDINAL;
    END;

  pGlobalVarListEntry = POINTER TO aGlobalVarListEntry;
  aGlobalVarListEntry =
    RECORD
      nextGlobal:     pGlobalVarListEntry;
      theGlobal:      ObjPtr;
    END;

  pLinkListEntry      = POINTER TO aLinkListEntry;
  aLinkListEntry      =
    RECORD
      nextLink:         pLinkListEntry;
      link:             CARDINAL;
    END;

  pListOfLinks        = POINTER TO aListOfLinks;
  aListOfLinks        =
    RECORD
      nextList:       pListOfLinks;
      links:          pLinkListEntry;
    END;

VAR
  globalList:       pGlobalVarListEntry;
  globalSize:       LONGINT;
  ConvStr:          ARRAY [0..15] OF CHAR;
  listOfLinks:      pListOfLinks;

PROCEDURE getLabelText(    labelNumber:   CARDINAL;
                           labelType:     ARRAY OF CHAR;
                       VAR labelText:     aSymbol);
(*
  OPERATION:
    Will produce a label consisting of the string in labelType, followed by
    the string representation of labelNumber.

    For example, if labelNumber = 2, and labelType = "_Loc", then the resulting
    labelText will be "_Loc2".
*)
VAR
  number:   ARRAY [0..3] OF CHAR;
BEGIN
  CardToString(labelNumber, number, 4);
  Concat(labelType, number, labelText);
END getLabelText;

PROCEDURE IdToSymbol(id: WORD; VAR theSymbol: aSymbol);
(*
  PROCEDURE:
    Will convert an id in the symbol table to a symbol string for further
    manipulation.
*)
VAR
  i,L, ID: CARDINAL;
BEGIN
  i := 0;
  ID := VAL(CARDINAL, id);
  L := ORD(IdBuf[ID]) - 1;

  IF L > aSymbolLength THEN
    L := aSymbolLength;
    Mark(300);
  END;

  INC(ID);

  REPEAT
    theSymbol[i] := IdBuf[ID];
    INC(i);
    INC(ID);
    DEC(L);
  UNTIL L = 0;

  IF i < aSymbolLength THEN
    theSymbol[i] := 0C;
  END;
END IdToSymbol;

PROCEDURE GetModuleNameByPtr(x: ObjPtr; VAR modName: aSymbol);
(*
  OPERATION:
    Retrieves a full module name.  For use when allowing for local modules.
*)
VAR
  nextPart: aSymbol;
  mod:      ObjPtr;
  done:     BOOLEAN;
BEGIN
  mod := x;
  modName := '';
  done := FALSE;

  WHILE NOT done DO
    IdToSymbol(mod^.name, nextPart);
    Concat(nextPart, modName, modName);

    mod := mod^.parent;

    IF mod <> NIL THEN
      Concat('_', modName, modName);
    ELSE
      done := TRUE;
    END;
  END;
END GetModuleNameByPtr;

PROCEDURE GetModuleKey(mno: CARDINAL; VAR theKey: aSymbol; VAR OK: BOOLEAN);
(*
  OPERATION:
    Returns the textual key used by the compiler for link time version 
    checking.
*)

  PROCEDURE KeyToString(key:  KeyPtr; VAR string: ARRAY OF CHAR);
  VAR
    number: ARRAY [0..5] OF CHAR;
  BEGIN
    NumToString(key^.k0, 16, string, 4);
    NumToString(key^.k1, 16, number, 4);
    Concat(string, number, string);
    NumToString(key^.k2, 16, number, 4);
    Concat(string, number, string);
    Concat('__', string, string);
  END KeyToString;

VAR
  name:   CARDINAL;
  obj:    ObjPtr;
  intobj: ObjPtr;
  Found:  BOOLEAN;
BEGIN
  (*
    FindModule searches the list of top-level modules for the desired
    module.
  *)
  FindModule(mno, obj);
  OK := TRUE;

  IF obj = NIL THEN
    (*
      If the module is not in the top-level list, it may still exist as
      a part of the scope of the main module.  If so, then the module is
      probably one that was imported as a result of importing a top-level
      module.

      If we find it in this search, return it's key, but also set OK to
      false so that the caller knows that it shouldn't have access to this
      module.

      If we don't find it, then it is an error!
    *)

    obj := mainmod;
    Found := FALSE;

    WHILE NOT Found AND (obj <> NIL) AND OK DO
      IF obj^.modno = mno THEN
        Found := TRUE;
        OK := FALSE;
      END;

      IF NOT Found THEN
        obj := obj^.next;
      END;
    END;

    IF NOT Found THEN
      HALT;
    ELSE
      KeyToString(obj^.key, theKey);
    END;
  ELSE
    (*
      We must check to see if the module is an interface module that has
      no implementation.

      Such a module would be an imported module.
    *)
    intobj := mainmod;
    Found := FALSE;

    WHILE NOT Found AND (intobj <> NIL) DO
      IF Diff(intobj^.name, obj^.name) = 0 THEN
        Found := TRUE;
      ELSE
        intobj := intobj^.next;
      END;
    END;

    IF intobj <> NIL THEN
      IF intobj^.isInt THEN
        OK := FALSE;
      END;
    END;

    KeyToString(obj^.key, theKey);
  END;
END GetModuleKey;

PROCEDURE GetModuleName(mno: CARDINAL; VAR theName: aSymbol; VAR OK: BOOLEAN);
(*
  OPERATION:
    Will get the specified module name and return it to the caller.
*)
VAR
  name:   CARDINAL;
  obj:    ObjPtr;
  intobj: ObjPtr;
  Found:  BOOLEAN;
BEGIN
  IF mno = 0FFFFH THEN
    Assign('SYSTEM', theName);
  ELSE
    (*
      FindModule searches the list of top-level modules for the desired
      module.
    *)
    FindModule(mno, obj);
    OK := TRUE;

    IF obj = NIL THEN
      (*
        If the module is not in the top-level list, it may still exist as
        a part of the scope of the main module.  If so, then the module is
        probably one that was imported as a result of importing a top-level
        module.

        If we find it in this search, return it's name, but also set OK to
        false so that the caller knows that it shouldn't have access to this
        module.

        If we don't find it, then it is an error!
      *)

      obj := mainmod;
      Found := FALSE;

      WHILE NOT Found AND (obj <> NIL) AND OK DO
        IF obj^.modno = mno THEN
          Found := TRUE;
          OK := FALSE;
        END;

        IF NOT Found THEN
          obj := obj^.next;
        END;
      END;

      IF NOT Found THEN
        HALT;
      ELSE
        GetModuleNameByPtr(obj, theName);
      END;
    ELSE
      (*
        We must check to see if the module is an interface module that has
        no implementation.

        Such a module would be an imported module.
      *)
      intobj := mainmod;
      Found := FALSE;

      WHILE NOT Found AND (intobj <> NIL) DO
        IF Diff(intobj^.name, obj^.name) = 0 THEN
          Found := TRUE;
        ELSE
          intobj := intobj^.next;
        END;
      END;

      IF intobj <> NIL THEN
        IF intobj^.isInt THEN
          OK := FALSE;
        END;
      END;

      GetModuleNameByPtr(obj, theName);
    END;
  END;
END GetModuleName;

PROCEDURE GetStringName(    number:   CARDINAL;
                            module:   CARDINAL;
                            level:    CARDINAL;
                        VAR theLabel: aSymbol);
(*
  OPERATION:
    Will return the name of the specified string.
*)
VAR
  moduleName:   aSymbol;
  OK:           BOOLEAN;
BEGIN
  getLabelText(number, '__String', theLabel);

  IF level = 0 THEN
    GetModuleName(module, moduleName, OK);
    Concat(moduleName, theLabel, theLabel);
  END;
END GetStringName;

PROCEDURE GetProcedureName(VAR x: ObjPtr; VAR procName: aSymbol);
(*
  OPERATION:
    Will get both the module and procedure name of the specified object.
    These are concatenated to form one name, and that is returned.
*)
VAR
  nextPart: aSymbol;
  OK:       BOOLEAN;
  proc:     ObjPtr;
  done:     BOOLEAN;
  complete: BOOLEAN;
  index:    CARDINAL;
BEGIN
  proc := x;
  procName := '';
  complete := FALSE;
  done := FALSE;

  WHILE NOT done DO
    IdToSymbol(proc^.name, nextPart);
    Concat(nextPart, procName, procName);

    IF proc^.procType = pascal THEN
      done := TRUE;
      complete := TRUE;

      index := Length(procName) + 1;

      REPEAT
        DEC(index);
        procName[index] := CAP(procName[index]);
      UNTIL index = 0;
    ELSIF (proc^.class = Proc) AND
       (proc^.parent = NIL) THEN          (* exported from THIS module *)
      done := TRUE;
    ELSE
      proc := proc^.parent;

      IF proc <> NIL THEN
        Concat('_', procName, procName);
      ELSE
        done := TRUE;
        complete := TRUE;
      END;
    END;
  END;

  IF NOT complete THEN
    GetModuleName(x^.pmod, nextPart, OK);
    Concat('_', procName, procName);
    Concat(nextPart, procName, procName);
  END;
END GetProcedureName;

PROCEDURE GetVariableName(VAR x: Item; VAR varName: aSymbol);
(*
  OPERATION:
    Returns the full name of the variable (module_variable).
*)
VAR
  proc:         ObjPtr;
  done:         BOOLEAN;
  variablePart: aSymbol;
  OK:           BOOLEAN;
  complete:     BOOLEAN;
BEGIN
  proc := x.parent;
  varName := '';
  done := proc = NIL;
  complete := FALSE;

  WHILE NOT done DO
    IdToSymbol(proc^.name, variablePart);
    Concat(variablePart, varName, varName);

    proc := proc^.parent;

    IF proc <> NIL THEN
      Concat('_', varName, varName);
    ELSE
      done := TRUE;
      complete := TRUE;
    END;
  END;

  IF NOT complete THEN
    GetModuleName(x.mod, varName, OK);
  END;

  IF x.mode = conMd THEN
    (*
      ### M2LM.GetVariableName:variable is a constant
    *)
    Mark(408);
  ELSE
    IdToSymbol(x.name, variablePart);
    Concat(varName, "_", varName);
    Concat(varName, variablePart, varName);
  END;
END GetVariableName;

PROCEDURE AddBytesToObjectFile(codeSize: CARDINAL);
(*
  OPERATION:
    Will increment the size of the current level zero procedure, as well as
    the entire object file being produced.

    It then checks to see if the current procedure length has exceeded 64K,
    which is the maximum size for any GSOS segment.
*)
BEGIN
  pc := pc + codeSize;
  objectCodeSize := objectCodeSize + VAL(LONGINT, codeSize);
  IncrementSegmentLength(codeSize);
END AddBytesToObjectFile;

PROCEDURE newStringListEntry(VAR list:      pStringListEntry;
                             VAR number:    CARDINAL;
                                 isGlobal:  BOOLEAN): pStringListEntry;
(*
  OPERATION:
    Will allocate a new StringListEntry, and place it at the end of the list.
    The address of the entry is returned to the caller.
*)
VAR
  curEntry: pStringListEntry;
BEGIN
  IF isGlobal THEN
    number := 0;
  ELSE
    NewLabel(number);
  END;

  IF list = NIL THEN
    ALLOCATE(list, TSIZE(aStringListEntry));
    WITH list^ DO
      nextString := NIL;
      theString := '';
      labelNumber := number;
    END;

    RETURN list;
  ELSE
    IF isGlobal THEN
      INC(number);
    END;

    curEntry := list;

    WHILE curEntry^.nextString <> NIL DO
      curEntry := curEntry^.nextString;

      IF isGlobal THEN
        INC(number);
      END;
    END;

    ALLOCATE(curEntry^.nextString, TSIZE(aStringListEntry));
    WITH curEntry^.nextString^ DO
      nextString := NIL;
      theString := '';
      labelNumber := number;
    END;

    RETURN curEntry^.nextString;
  END;
END newStringListEntry;

PROCEDURE clearStringList(VAR list: pStringListEntry);
(*
  OPERATION:
    Clears out the list of string constants.  This will normally be called at
    the end of the production of a procedures code.
*)
VAR
  curEntry:   pStringListEntry;
  nextEntry:  pStringListEntry;
BEGIN
  IF list <> NIL THEN
    curEntry := list;
    nextEntry := curEntry^.nextString;

    WHILE nextEntry <> NIL DO
      DEALLOCATE(curEntry, TSIZE(aStringListEntry));
      curEntry := nextEntry;
      nextEntry := curEntry^.nextString;
    END;

    DEALLOCATE(curEntry, TSIZE(aStringListEntry));
    list := NIL;
  END;
END clearStringList;

PROCEDURE AllocString(VAR list:   pStringListEntry;
                          pos:    CARDINAL;
                      VAR number: CARDINAL;
                      VAR length: CARDINAL);
(*
  OPERATION:
    Will create a new string constant for the current code segment.
*)
VAR
  stringEntry:  pStringListEntry;
  i, L:         CARDINAL;
  global:       BOOLEAN;
BEGIN
  global := ADR(list) = ADR(globalStringList);

  stringEntry := newStringListEntry(list, number, global);

  WITH stringEntry^ DO
    L := ORD(IdBuf[pos]) - 1;

    IF L > MaxStringLength THEN
      L := MaxStringLength;
      Mark(301);
    END;

    length := L + 1;
    INC(pos);
    i := 0;

    WHILE L > 0 DO
      theString[i] := IdBuf[pos];
      INC(i);
      INC(pos);
      DEC(L);
    END;

    IF i < MaxStringLength THEN
      theString[i] := 0C;
    END;
  END;
END AllocString;

PROCEDURE AllocPString(VAR list:   pStringListEntry;
                           str:    ARRAY OF CHAR;
                       VAR number: CARDINAL;
                       VAR length: CARDINAL);
(*
  OPERATION:
    Will create a new "pascal" string constant for the current code segment.
*)
VAR
  stringEntry:  pStringListEntry;
  global:       BOOLEAN;
  lengthChar:   ARRAY [0..0] OF CHAR;
BEGIN
  global := ADR(list) = ADR(globalStringList);

  stringEntry := newStringListEntry(list, number, global);

  WITH stringEntry^ DO
    length := Length(str);

    IF length > MaxStringLength THEN
      length := MaxStringLength;
      Mark(301);
    END;

    lengthChar[0] := VAL(CHAR, length);

    Assign(str, theString);
    Insert(lengthChar, theString, 0);
  END;
END AllocPString;

PROCEDURE AllocCString(VAR list:   pStringListEntry;
                           str:    ARRAY OF CHAR;
                       VAR number: CARDINAL;
                       VAR length: CARDINAL);
(*
  OPERATION:
    Will create a new "C" string constant for the current code segment.
*)
VAR
  stringEntry:  pStringListEntry;
  global:       BOOLEAN;
BEGIN
  global := ADR(list) = ADR(globalStringList);

  stringEntry := newStringListEntry(list, number, global);

  WITH stringEntry^ DO
    length := Length(str);

    IF length > MaxStringLength THEN
      length := MaxStringLength;
      Mark(301);
    END;

    Assign(str, theString);
  END;
END AllocCString;

PROCEDURE AllocChar(VAR list:   pStringListEntry;
                        ch:     CHAR;
                    VAR number: CARDINAL;
                    VAR length: CARDINAL);
(*
  OPERATION:
    Will create a new string constant for the current code segment.
*)
VAR
  stringEntry: pStringListEntry;
  global:       BOOLEAN;
BEGIN
  global := ADR(list) = ADR(globalStringList);

  stringEntry := newStringListEntry(list, number, global);

  WITH stringEntry^ DO
    length := 2;
    theString[0] := ch;
    theString[1] := 0C;
  END;
END AllocChar;

PROCEDURE AllocBounds(min, max: INTEGER; size: CARDINAL; VAR adr: aAdr);
(* allocate the bounds of a subrange or index. *)
BEGIN
  adr := 0 (* signal NO bound-pair allocated! *)
END AllocBounds;

PROCEDURE PutStringConstants(VAR list: pStringListEntry);
(*
  OPERATION:
    Will write to the object file, the list of string constants that has been
    declared for the current code segment.
*)
VAR
  theLine:      ARRAY [0..255] OF CHAR;
  theLabel:     aSymbol;
  moduleName:   aSymbol;
  curEntry:     pStringListEntry;
  isGlobal:     BOOLEAN;
  level:        CARDINAL;
  ci:           CARDINAL;
  len:          CARDINAL;
BEGIN
  curEntry := list;
  isGlobal := (ADR(list) = ADR(globalStringList));

  WHILE curEntry <> NIL DO
    IF isGlobal THEN
      WITH curEntry^ DO
        GetStringName(labelNumber, mainmod^.modno, 0, theLabel);

        Assign(theString, theLine);
        GenData(theLabel, ADR(theLine), Length(theLine)+1, TRUE);
        objectDataSize := objectDataSize + VAL(LONGINT, Length(theLine)+1);
        IncrementSegmentLength(Length(theLine)+1);
      END;
    ELSE
      WITH curEntry^ DO
        FixLabel(labelNumber); (* labelNumber is field in curEntry^ *)

        len := Length(theString);

        IF len <> 0 THEN
          FOR ci := 0 TO len-1 DO
            PutByteConstant(VAL(BYTE, theString[ci]));
          END;
        END;

        PutByteConstant(VAL(BYTE, 0)); (* Terminate the string *)
        AddBytesToObjectFile(len+1);
      END;
    END;

    curEntry := curEntry^.nextString;
  END;
END PutStringConstants;

PROCEDURE newGlobalListEntry(): pGlobalVarListEntry;
(*
  OPERATION:
    Will allocate a new GlobalListEntry, and place it at the end of the list.
    The address of the entry is returned to the caller.
*)
VAR
  curEntry: pGlobalVarListEntry;
BEGIN
  IF globalList = NIL THEN
    ALLOCATE(globalList, TSIZE(aGlobalVarListEntry));
    WITH globalList^ DO
      nextGlobal := NIL;
      theGlobal := NIL;
    END;

    RETURN globalList;
  ELSE
    curEntry := globalList;

    WHILE curEntry^.nextGlobal <> NIL DO
      curEntry := curEntry^.nextGlobal;
    END;

    ALLOCATE(curEntry^.nextGlobal, TSIZE(aGlobalVarListEntry));

    WITH curEntry^.nextGlobal^ DO
      nextGlobal := NIL;
      theGlobal := NIL;
    END;

    RETURN curEntry^.nextGlobal;
  END;
END newGlobalListEntry;

PROCEDURE clearGlobalList;
(*
  OPERATION:
    Clears out the list of Global variables.  This will normally be called at
    the end of the production of a modules.
*)
VAR
  curEntry:   pGlobalVarListEntry;
  nextEntry:  pGlobalVarListEntry;
BEGIN
  IF globalList <> NIL THEN
    curEntry := globalList;
    nextEntry := curEntry^.nextGlobal;

    WHILE nextEntry <> NIL DO
      DEALLOCATE(curEntry, TSIZE(aGlobalVarListEntry));
      curEntry := nextEntry;
      nextEntry := curEntry^.nextGlobal;
    END;

    DEALLOCATE(curEntry, TSIZE(aGlobalVarListEntry));
    globalList := NIL;
  END;
END clearGlobalList;

PROCEDURE AllocGlobalVar(theVar: ObjPtr);
(*
  OPERATION:
    Will create a new Global variable for the current module, adding it to
    the list.

    An example of a global variable name as placed in this list would be:

      M2LM_AllocGlobalVar
*)
VAR
  globalEntry: pGlobalVarListEntry;
  pos:   CARDINAL;
BEGIN
  IF theVar^.vmod = mainmod^.modno THEN
    globalEntry := newGlobalListEntry();

    WITH globalEntry^ DO
      theGlobal := theVar;
    END;

    globalSize := globalSize + VAL(LONGINT, theVar^.typ^.size);
  END;
END AllocGlobalVar;

PROCEDURE PutGlobals(isImp: BOOLEAN);
(*
  OPERATION:
    Will write to the object file, the list of Globals and string constants that
    has been declared for the current module.  These globals will be placed in a
    segment called ~GLOBALS.
*)
VAR
  moduleName:   aSymbol;
  segmentName:  aSymbol;
  curEntry:     pGlobalVarListEntry;
  varName:      aSymbol;
  i:            CARDINAL;
  OK:           BOOLEAN;
  debstr:       String255;
BEGIN
  (*
    We only bother generating a global data segment if:

      1.  There are global variables to be generated.
      2.  It is the main module being compiled.
  *)
  IF (globalList <> NIL) OR
     (NOT isImp) THEN 
    (*
      globalList is not empty, so place any globals in the ~globals segment.
    *)
    curEntry := globalList;
    IdToSymbol(mainmod^.name, moduleName);

    IF isImp THEN
      Concat(moduleName, '__Globals', segmentName);
    ELSE
      Concat("~", '__Globals', segmentName);
    END;

    NewSegmentHeader(segmentName);

    objFile.segmentHeader^.LOADNAME := '~globals  ';
    objFile.segmentHeader^.BANKSIZE := 0;

    WHILE curEntry <> NIL DO
      WITH curEntry^ DO
        IF theGlobal^.parent = NIL THEN
          IdToSymbol(mainmod^.name, moduleName);
        ELSE
          GetModuleNameByPtr(theGlobal^.parent, moduleName);
        END;

        IdToSymbol(theGlobal^.name, varName);

        Concat('_', varName, varName);
        Concat(moduleName, varName, varName);
        GenDS(varName, theGlobal^.typ^.size, NOT theGlobal^.exported);

        objectDataSize := objectDataSize + theGlobal^.typ^.size;
        IncrementSegmentLength(theGlobal^.typ^.size);
      END;

      curEntry := curEntry^.nextGlobal;
    END;

    EndSegment(skData, aSegmentAttributeSet{});
  END; (* globalList is not empty *)

  IF globalStringList <> NIL THEN
    (*
      Strings defined at the global level are placed in the ~arrays segment,
      but only if some exist.  There's no need to create an empty segment.
    *)
    IdToSymbol(mainmod^.name, moduleName);

    IF isImp THEN
      Concat(moduleName, '__Arrays', segmentName);
    ELSE
      Concat("~", '__Arrays', segmentName);
    END;

    NewSegmentHeader(segmentName);

    objFile.segmentHeader^.LOADNAME := '~arrays   ';
    objFile.segmentHeader^.BANKSIZE := 0;

    PutStringConstants(globalStringList);

    EndSegment(skData, aSegmentAttributeSet{});
  END;
END PutGlobals;

PROCEDURE ClearLink(link: pLinkListEntry);
VAR
  prevLink: pLinkListEntry;
BEGIN
  WHILE link <> NIL DO
    prevLink := link;
    link := link^.nextLink;

    DEALLOCATE(prevLink, TSIZE(aLinkListEntry));
  END;
END ClearLink;

PROCEDURE clearListOfLinks;
VAR
  curList:  pListOfLinks;
  prevList: pListOfLinks;
BEGIN
  curList := listOfLinks;

  WHILE curList <> NIL DO
    ClearLink(curList^.links);
    prevList := curList;
    curList := curList^.nextList;

    DEALLOCATE(prevList, TSIZE(aListOfLinks));
  END;

  listOfLinks := NIL;
END clearListOfLinks;

PROCEDURE linkPresent(list: pLinkListEntry; L: CARDINAL): BOOLEAN;
BEGIN
  IF list = NIL THEN
    RETURN FALSE;
  ELSE
    WHILE list <> NIL DO
      IF list^.link = L THEN
        RETURN TRUE;
      ELSE
        list := list^.nextLink;
      END;
    END;

    RETURN FALSE;
  END;
END linkPresent;

PROCEDURE MergedLinks(L0, L1: CARDINAL): CARDINAL;
VAR
  curList:  pListOfLinks;
  Found:    BOOLEAN;

  PROCEDURE AddToLinks(VAR list: pLinkListEntry; L: CARDINAL);
  VAR
    alreadyPresent: BOOLEAN;
    endOfList:      BOOLEAN;
  BEGIN
    IF list = NIL THEN
      ALLOCATE(list, TSIZE(aLinkListEntry));

      list^.nextLink := NIL;
      list^.link := L;
    ELSE
      alreadyPresent := FALSE;
      endOfList := FALSE;

      WHILE (NOT endOfList) AND (NOT alreadyPresent) DO
        IF list^.link = L THEN
          alreadyPresent := TRUE;
        ELSE
          IF list^.nextLink  = NIL THEN
            endOfList := TRUE;
          ELSE
            list := list^.nextLink;
          END;
        END;
      END;

      IF NOT alreadyPresent THEN
        ALLOCATE(list^.nextLink, TSIZE(aLinkListEntry));

        WITH list^.nextLink^ DO
          nextLink := NIL;
          link := L;
        END;
      END;
    END;
  END AddToLinks;

BEGIN
  IF L0 <> 0 THEN
    IF listOfLinks <> NIL THEN
      curList := listOfLinks;
      Found := FALSE;

      WHILE (curList <> NIL) AND
            (NOT Found) DO
        IF linkPresent(curList^.links, L0) THEN
          Found := TRUE;
          AddToLinks(curList^.links, L1);
        ELSIF linkPresent(curList^.links, L1) THEN
          Found := TRUE;
          AddToLinks(curList^.links, L0);
        ELSE
          curList := curList^.nextList;
        END;
      END;

      IF NOT Found THEN
        ALLOCATE(curList, TSIZE(aListOfLinks));

        WITH curList^ DO
          nextList := listOfLinks;
          links := NIL;

          AddToLinks(links, L0);
          AddToLinks(links, L1);
        END;

        listOfLinks := curList;
      END;
    ELSE
      ALLOCATE(listOfLinks, TSIZE(aListOfLinks));

      WITH listOfLinks^ DO
        nextList := NIL;
        links := NIL;

        AddToLinks(links, L0);
        AddToLinks(links, L1);
      END;
    END;

    RETURN L0;
  ELSE
    RETURN L1;
  END;
END MergedLinks;

PROCEDURE newLink(VAR number: CARDINAL);
(*
  OPERATION:
    This procedure will allocate a new local label number.
*)
BEGIN
  NewLabel(number);
END newLink;

PROCEDURE FixLink(labelNumber: CARDINAL);
(*
  OPERATION:
    Will generate a line on the object file that places the specified local
    label at the current point.
*)
VAR
  labelText:  aSymbol;
  curList:    pListOfLinks;
  prevList:   pListOfLinks;
  curLink:    pLinkListEntry;
  Found:      BOOLEAN;
BEGIN
  IF labelNumber <> 0 THEN
    Found := FALSE;

    IF listOfLinks <> NIL THEN
      prevList := NIL;
      curList := listOfLinks;

      WHILE (curList <> NIL) AND
            (NOT Found) DO
        IF linkPresent(curList^.links, labelNumber) THEN
          Found := TRUE;
        ELSE
          prevList := curList;
          curList := curList^.nextList;
        END;
      END;
    END;

    IF NOT Found THEN
      FixLabel(labelNumber);
    ELSE
      curLink := curList^.links;

      WHILE curLink <> NIL DO
        FixLabel(curLink^.link);
        curLink := curLink^.nextLink;
      END;

      IF prevList <> NIL THEN
        prevList^.nextList := curList^.nextList;
      ELSE
        listOfLinks := curList^.nextList;
      END;

      ClearLink(curList^.links);

      DEALLOCATE(curList, TSIZE(aListOfLinks));
    END;
  END;
END FixLink;

PROCEDURE PutLinkAddress(labelNumber: CARDINAL; size: CARDINAL);
(*
  OPERATION:
    Will generate a line in the object file that places the address of the
    specified local label at the current point.
*)
VAR
  labelText:  aSymbol;
  sizeText:   ARRAY [0..1] OF CHAR;
BEGIN
  PutByte(VAL(BYTE, 0EBH));           (* begin expression               *)
  PutByte(VAL(BYTE, size));

  PutByte(VAL(BYTE, 087H));           (* "A": displacement of label     *)
  ReferenceLabel(labelNumber);
  PutByte(VAL(BYTE, 081H));           (* "B": 1                         *)
  PutLong(VAL(LONGINT, 1));
  PutByte(VAL(BYTE, 002H));           (* subtract "B" from "A" --> "C"  *)

  PutByte(VAL(BYTE, 00H));            (* end of expression              *)
  AddBytesToObjectFile(size);
END PutLinkAddress;

PROCEDURE PutLinkReference(operand:     CARDINAL;
                           labelNumber: CARDINAL;
                           codeSize:    CARDINAL);
(*
  OPERATION:
    Will generate a line in the object file that places the a reference to the
    specified local label at the current point.  The reference is preceded by
    an operand as specified, thus allowing references like:

        LDA   _Loc2
    and BRA   _Loc24
*)
BEGIN
  IF (operand = BRL) AND (LabelDisplacement(labelNumber) < 126) THEN
    GenLocalOp(BRA, labelNumber, TRUE, 0, FALSE, 1);
    AddBytesToObjectFile(2);
  ELSE
    GenLocalOp(operand, labelNumber, (codeSize = 2) OR (operand = BRL), 0, 
               FALSE, codeSize - 1);
    AddBytesToObjectFile(codeSize);
  END;
END PutLinkReference;

PROCEDURE PutDisplayReference(instruction:  CARDINAL;
                              value:        WORD;
                              proc:         ObjPtr;
                              codeSize:     CARDINAL);
(*
  OPERATION:
    Generates an instruction that has as it's operand a constant value that is
    calculated by adding "value" to the value of the global symbol specified
    by the name of the parent procedure. eg:

        Where the parent procedure is "M2LM_PutDPReference", the global symbol
        will be "M2LM_PutDPReference__OTP" where OTP stands for Offset To
        Parameters.
*)
VAR
  procName: aSymbol;
BEGIN
  GetProcedureName(proc, procName);
  Concat(procName, "__OTP", procName);

  GenGlobalOp(instruction, procName, VAL(INTEGER, value), 0, 2);
  AddBytesToObjectFile(3);
END PutDisplayReference;

PROCEDURE PutDPReference(operand:   CARDINAL;
                         adr:       WORD;
                         codeSize:  CARDINAL);
(*
  OPERATION:
    Will generate a line in the form:

      operand $adr

    eg.  LDA $02
*)
BEGIN
  PutByteConstant(VAL(BYTE, operand));

  IF codeSize = 2 THEN
    PutByteConstant(VAL(BYTE, adr));
  ELSE
    PutWordConstant(adr);
  END;

  AddBytesToObjectFile(codeSize);
END PutDPReference;

PROCEDURE PutLongReference(operand:   CARDINAL;
                           adr:       LONGCARD;
                           codeSize:  CARDINAL);
(*
  OPERATION:
    Will generate a line in the form:

      operand $adr

    eg.  LDA $020000
*)
TYPE
  aLongword =
    RECORD
      CASE :CARDINAL OF
        0:  lc:             LONGCARD;
      | 1:  d0, d1, d2, d3: BYTE;
      END;
    END;
VAR
  value:  aLongword;
BEGIN
  PutByteConstant(VAL(BYTE, operand));

  value.lc := adr;
  PutByteConstant(value.d0);
  PutByteConstant(value.d1);
  PutByteConstant(value.d2);

  AddBytesToObjectFile(codeSize);
END PutLongReference;

PROCEDURE PutStackReference(operand:   CARDINAL;
                            adr:       WORD);
(*
  OPERATION:
    Will generate a line in the form:

      operand $adr,S

    eg.  LDA $02,S
*)
BEGIN
  PutByteConstant(VAL(BYTE, operand));
  PutByteConstant(VAL(BYTE, adr));
  AddBytesToObjectFile(2);
END PutStackReference;

PROCEDURE PutOp(operand:  CARDINAL;
                codeSize: CARDINAL);
(*
  OPERATION:
    Places the operand into the object file as one or two bytes.
*)
BEGIN
  IF codeSize = 1 THEN
    PutByteConstant(VAL(BYTE, operand));
  ELSE
    PutWordConstant(operand);
  END;

  AddBytesToObjectFile(codeSize);
END PutOp;

PROCEDURE CloseObjectFile;
(*
  OPERATION:
    This procedure closes the object file.  If an error occured during the
    compilation, then the object file is deleted.
*)
VAR
  discardObjectFile: BOOLEAN;
BEGIN
  (*
    We may not want the object file we have created.  Three reasons exist:

      1.  A compilation error occured.

      2.  No KEEP directive was used.  If this is true, then "KeepObject" in
          the M2Shell module will be FALSE.

      3.  The user may have pressed Command-Period to abort the compile.

      4.  An error has occured whilst trying to write to the object file.

    If either of these conditions are present, then cause the 'file' to be
    discarded when we close it.
  *)
  discardObjectFile := scanerr OR 
                       NOT KeepObject OR 
                       userHasAborted OR
                       objectFileInError;

  (*
    The "SaveToDisk" flag is TRUE by default (because -M is default for a
    compile of any sort).  If the user wants a memory compile, and specifies
    +M, then SaveToDisk will be FALSE.  Assuming that we don't want to discard
    the file, this means that we give the file to FastFile, but don't save it
    to disk.
  *)
  CloseOMFFile(discardObjectFile, SaveToDisk);
END CloseObjectFile;

PROCEDURE OpenObjectFile(filename: ARRAY OF CHAR; extension: ARRAY OF CHAR);
(*
  OPERATION:
    Opens the object file.
*)
CONST
  NL = 511;
VAR
  FName: ARRAY [0..NL] OF CHAR;
BEGIN
  MakeFileName(filename, FName, extension);

  OpenNewOMFFile(FName);
END OpenObjectFile;

PROCEDURE InitOMF;
(*
  OPERATION:
    Initialises the object file formatter module, in preparation for the
    compilation of a new module.
*)
BEGIN
  pc := 0;
  objectCodeSize := 0;
  objectDataSize := 0;
  maxM := 0;
  segmentName := '';

  globalSize := 0;
  clearGlobalList;
  clearStringList(localStringList);
  clearStringList(globalStringList);
  clearListOfLinks;
END InitOMF;

PROCEDURE InitM2LM;
BEGIN
  localStringList := NIL;
  globalStringList := NIL;
  globalList := NIL;
  listOfLinks := NIL;
  Stacksize := 4096;
END InitM2LM;

BEGIN
  ConvStr := '0123456789ABCDEF';
END M2LM.
 
