(*$Segment 'M2Debug'*)
(*$StackCheck+*)
(*$RangeCheck+*)
(*$OverflowCheck+*)
IMPLEMENTATION MODULE M2Debug;

(*
  This module generates the various prizm debugger records.
*)

FROM M2DM IMPORT ObjPtr, StrPtr, StrForm, ObjClass, chartyp, MaxInt;
FROM M2EM IMPORT GenItem;
FROM M2HM IMPORT Item;
FROM M2LM IMPORT AllocPString, localStringList,
  AddBytesToObjectFile, IdToSymbol, GetVariableName, aSymbol;
FROM M2OMF IMPORT PutByte, PutByteConstant, PutWordConstant, ReferenceLabel,
  PutLong, PutLongConstant, PutLabelText;
FROM M2Shell IMPORT COP8asCOP5, ShowProgress;
FROM M2TM IMPORT Scope;
FROM SYSTEM IMPORT BYTE;
FROM InOut IMPORT WriteString, WriteCard, WriteLn;
FROM W65C816 IMPORT COP;

PROCEDURE GenLine(sourceLine: CARDINAL);
BEGIN
  (* COP 'lineType' - Start of next statement *)
  PutByteConstant(VAL(BYTE, COP));
  PutByteConstant(VAL(BYTE, lineType));
  AddBytesToObjectFile(2);

  PutWordConstant(sourceLine);
  AddBytesToObjectFile(2);
END GenLine;

PROCEDURE GenStartProcedure(name: ARRAY OF CHAR);
VAR
  len:            CARDINAL;
  procedureLabel: CARDINAL;
BEGIN
  (* COP 3 - Subroutine entered *)
  PutByteConstant(VAL(BYTE, COP));
  PutByteConstant(VAL(BYTE, 3));
  AddBytesToObjectFile(2);

  AllocPString(localStringList, name, procedureLabel, len);

  PutByte(VAL(BYTE, 0EBH));           (* begin expression               *)
  PutByte(VAL(BYTE, 2));

  PutByte(VAL(BYTE, 087H));           (* "A": displacement of label     *)
  ReferenceLabel(procedureLabel);

  PutByte(VAL(BYTE, 00H));            (* end of expression              *)

  PutByte(VAL(BYTE, 0EBH));           (* begin expression               *)
  PutByte(VAL(BYTE, 2));

  PutByte(VAL(BYTE, 087H));           (* "A": displacement of label     *)
  ReferenceLabel(procedureLabel);

  PutByte(VAL(BYTE, 81H));            (* constant operand     *)
  PutLong(VAL(LONGINT, -16));         (* value of offset      *)
  PutByte(VAL(BYTE, 07H));            (* shift by 16 bits     *)

  PutByte(VAL(BYTE, 00H));            (* end of expression              *)

  AddBytesToObjectFile(4);
END GenStartProcedure;

PROCEDURE GenEndProcedure;
BEGIN
  (* COP 4 - Subroutine exited *)
  PutByteConstant(VAL(BYTE, COP));
  PutByteConstant(VAL(BYTE, 4));
  AddBytesToObjectFile(2);
END GenEndProcedure;

TYPE
  sStrForm        = SET OF StrForm;
  aPass           = (zeroingOffsets, calculatingSize, buildingTable);

VAR
  tablesize:        CARDINAL;
  tableposition:    CARDINAL;
  pass:             aPass;
  firstDone:        BOOLEAN;

PROCEDURE AnArray(typ:  StrPtr): BOOLEAN;
BEGIN
  IF (typ^.form <> Array) AND
     (typ^.form <> Pointer) THEN
    RETURN FALSE;
  ELSIF typ^.form = Pointer THEN
    RETURN AnArray(typ^.PBaseTyp);
  ELSIF typ^.ElemTyp = chartyp THEN
    RETURN FALSE;
  ELSE
    RETURN TRUE;
  END;
END AnArray;

PROCEDURE BaseOK(typ: StrPtr): BOOLEAN;
BEGIN
(* Records & Pointers blocked to prevent table generation bug
  IF typ^.form IN sStrForm{Bool, Char, Card, Int, Enum, LCard, Double, Range,
                           Real, LongReal, String, Record, Pointer} THEN
    RETURN TRUE;
  ELSIF typ^.form = Array THEN
    RETURN BaseOK(typ^.ElemTyp);
  ELSE
    RETURN FALSE;
  END;
*)
  IF typ^.form IN sStrForm{Bool, Char, Card, Int, Enum, LCard, Double, Range,
                           Real, LongReal, String, Record} THEN
    RETURN TRUE;
  ELSIF typ^.form = Array THEN
    RETURN BaseOK(typ^.ElemTyp);
  ELSIF typ^.form = Pointer THEN
    IF typ^.PBaseTyp^.form = Undef THEN
      RETURN TRUE;
    ELSE
      RETURN BaseOK(typ^.PBaseTyp);
    END;
  ELSE
    RETURN FALSE;
  END;
END BaseOK;

PROCEDURE GenValueFormat(typ: StrPtr; pointedTo: BOOLEAN);
VAR
  type: CARDINAL;
BEGIN
  IF (typ^.form = Pointer) AND
     (typ^.PBaseTyp^.form <> Pointer) AND
     (NOT AnArray(typ^.PBaseTyp)) THEN
    GenValueFormat(typ^.PBaseTyp, TRUE);
  ELSE
    CASE typ^.form OF
      Bool:
        type := 09H;
    | Char, Undef:
        type := 08H;
    | Card:
        type := 41H;
    | Int:
        type := 01H;
    | Enum:
        type := 40H;
    | LCard:
        type := 42H;
    | Double:
        type := 02H;
    | Range:
        type := 01H;
    | Real:
        type := 03H;
    | LongReal:
        type := 04H;
    | String:
        type := 06H;
    | Array:
        type := 06H;
    | Pointer:
        type := 0BH;
    | Record:
        WITH typ^ DO
          CASE pass OF
            zeroingOffsets:
              debugDefining := FALSE;
              debugOffset := 0;
          | calculatingSize,
            buildingTable:
              IF NOT debugDefining AND
                 (debugOffset = 0) THEN
                debugOffset := tableposition - 9;
                debugDefining := TRUE;
              END;
          END;

          IF debugDefining THEN
            IF pointedTo THEN
              type := 0DH; (* pointer to record being defined may use derived type *)
            ELSE
              type := 0CH; (* not defined yet, so define it *)
            END;
          ELSE
            type := 0DH; (* derived type, so point to already generated record *)
          END;
        END;
    ELSE
      IF ShowProgress THEN
        WriteString('Compiler Error:Bad structure type (');
        WriteCard(ORD(typ^.form), 3);
        WriteString(').');
        WriteLn;
      END;
    END;

    IF pointedTo THEN
      type := type + 80H;
    END;

    CASE pass OF
      zeroingOffsets:
    | calculatingSize:
        INC(tablesize);
        INC(tableposition);
    | buildingTable:
        PutByteConstant(VAL(BYTE, type));
        AddBytesToObjectFile(1);
        INC(tableposition);
    END;
  END;
END GenValueFormat;

PROCEDURE GenSubscript(indextyp: StrPtr);
VAR
  lower:  CARDINAL;
  upper:  CARDINAL;
BEGIN
  CASE pass OF
    zeroingOffsets:
  | calculatingSize:
      INC(tablesize, 8);
      INC(tableposition, 8);
  | buildingTable:
      upper := MaxInt;
      lower := 0;

      WITH indextyp^ DO
        IF form = Range THEN
          upper := max;
          lower := min;
        END;
      END (*WITH*);

      PutLongConstant(VAL(LONGINT, lower));
      PutLongConstant(VAL(LONGINT, upper));
      AddBytesToObjectFile(8);

      INC(tableposition, 8);
  END;
END GenSubscript;

PROCEDURE GenObject(obj: ObjPtr; global: BOOLEAN); FORWARD;

PROCEDURE GenStructure(typ: StrPtr; chainedFromPointer: BOOLEAN);

  PROCEDURE GenZeroName;
  BEGIN
    CASE pass OF
      zeroingOffsets:
    | calculatingSize:
        INC(tablesize, 9);
        INC(tableposition, 9);
    | buildingTable:
        PutLongConstant(0);                 (* zero name *)
        PutLongConstant(0);                 (* zero address *)
        PutByteConstant(VAL(BYTE, 0));      (* zero address flag *)
        AddBytesToObjectFile(9);

        INC(tableposition, 9);
    END;
  END GenZeroName;

VAR
  OK:         BOOLEAN;
  done:       BOOLEAN;
  pointedTo:  BOOLEAN;
  anarray:    BOOLEAN;
  subsCount:  CARDINAL;
  arrobj:     StrPtr;
  curtyp:     StrPtr;
  fieldobj:   ObjPtr;
  override:   BOOLEAN;
BEGIN
  IF chainedFromPointer THEN
    GenZeroName;
  END;

  done := FALSE;
  pointedTo := FALSE;

  WITH typ^ DO
    anarray := AnArray(PBaseTyp);

    IF (form = Pointer) AND
       ((PBaseTyp^.form = Pointer) OR anarray) THEN
      GenValueFormat(typ, FALSE);

      CASE pass OF
        zeroingOffsets:
      | calculatingSize:
          INC(tablesize, 2);
          INC(tableposition, 2);
      | buildingTable:
          IF (PBaseTyp^.form = Record) AND
             (NOT PBaseTyp^.debugDefining) THEN
            (*
              If this is an instance of a record structure that has already
              been defined, then the subscript value is the offset to the
              original definition.
            *)
            PutWordConstant(PBaseTyp^.debugOffset-1);
          ELSE
            PutWordConstant(0);  (* zero subscripts *)
          END;

          AddBytesToObjectFile(2);
          INC(tableposition, 2);
      END;

      curtyp := PBaseTyp;

      IF curtyp^.form = Pointer THEN
        IF curtyp^.PBaseTyp^.form = Pointer THEN
          (*
            If "typ" is a pointer to a pointer to a pointer then go down 
            another level.
          *)
          GenStructure(curtyp, TRUE);
          done := TRUE;
        ELSE
          (*
            If "typ" is a pointer to a pointer to something other than a
            pointer and that something is not an array then end the chain
            here.
          *)
          IF NOT anarray THEN
            GenZeroName;
            pointedTo := TRUE;
          ELSE
            (*
              Otherwise, go down another level to take care of this pointer,
              and then the array.
            *)
            GenStructure(curtyp, TRUE);
            done := TRUE;
          END;
        END;
      ELSE
        (*
          pointer to an array, generate two entries, a pointer and an
          array.
        *)
        GenStructure(curtyp, TRUE);
        done := TRUE;
      END;
    ELSE
      curtyp := typ;
      pointedTo := FALSE;
    END;
  END; (* with typ^ *)

  override := FALSE;

  IF NOT done THEN
    IF NOT AnArray(curtyp) THEN
      GenValueFormat(curtyp, pointedTo);

      IF curtyp^.form = Pointer THEN
        curtyp := curtyp^.PBaseTyp;
        override := TRUE;
      END;

      CASE pass OF
        zeroingOffsets:
      | calculatingSize:
          INC(tablesize, 2);
          INC(tableposition, 2);
      | buildingTable:
          IF (curtyp^.form = Record) AND
             ((NOT curtyp^.debugDefining) OR override) THEN
            (*
              If this is an instance of a record structure that has already
              been defined, then the subscript value is the offset to the
              original definition.
            *)
            PutWordConstant(curtyp^.debugOffset-1);
          ELSE
            PutWordConstant(0);  (* zero subscripts *)
          END;

          AddBytesToObjectFile(2);
          INC(tableposition, 2);
      END;
    ELSE (* an array *)
      (*
        start off by determining the base type
      *)
      subsCount := 0;
      arrobj := curtyp;

      WHILE arrobj^.form = Array DO
        INC(subsCount);
        arrobj := arrobj^.ElemTyp;
      END;

      IF pass <> zeroingOffsets THEN
        WITH arrobj^ DO
          IF form = Record THEN
            IF debugOffset = 0 THEN
              debugOffset := tableposition + 3 + (subsCount * 12);
            END;

            debugDefining := TRUE;
          END;
        END;
      END;

      GenValueFormat(arrobj, pointedTo);

      CASE pass OF
        zeroingOffsets:
      | calculatingSize:
          INC(tablesize, 2);
          INC(tableposition, 2);
      | buildingTable:
          PutWordConstant(subsCount);
          AddBytesToObjectFile(2);
          INC(tableposition, 2);
      END;

      (*
        now generate the index information
      *)
      arrobj := curtyp;

      WHILE arrobj^.form = Array DO
        GenSubscript(arrobj^.IndexTyp);

        CASE pass OF
          zeroingOffsets:
        | calculatingSize:
            INC(tablesize, 4);
            INC(tableposition, 4);
        | buildingTable:
            PutLongConstant(arrobj^.ElemTyp^.size);
            AddBytesToObjectFile(4);
            INC(tableposition, 4);
        END;

        arrobj := arrobj^.ElemTyp;
      END;
(*
      curtyp := curtyp^.ElemTyp;
*)
      curtyp := arrobj;
    END;
  END;

  IF curtyp^.form = Record THEN
    WITH curtyp^ DO
      IF debugDefining AND NOT override THEN
        fieldobj := firstFld;

        WHILE fieldobj <> NIL DO
          GenObject(fieldobj, FALSE);

          fieldobj := fieldobj^.next;
        END;

        IF pass <> zeroingOffsets THEN
          debugDefining := FALSE;
        END;
      END;
    END;
  END;
END GenStructure;

PROCEDURE GenObject(obj: ObjPtr; global: BOOLEAN);
VAR
  varname:    aSymbol;
  x:          Item;
  namelabel:  CARDINAL;
  len:        CARDINAL;
BEGIN
  WITH obj^ DO
    IF ((class = Var) OR (class = Field)) AND
       BaseOK(typ) THEN
      CASE pass OF
        zeroingOffsets:
      | calculatingSize:
          INC(tablesize, 9);
          INC(tableposition, 9);
      | buildingTable:
          IF NOT firstDone AND (tableposition > 0) THEN
            firstDone := TRUE;
            tableposition := 1;
          END;

          IdToSymbol(name, varname);
          AllocPString(localStringList, varname, namelabel, len);
          PutByte(VAL(BYTE, 0EBH));         (* begin expression               *)
          PutByte(VAL(BYTE, 2));
          PutByte(VAL(BYTE, 087H));         (* "A": displacement of label     *)
          ReferenceLabel(namelabel);
          PutByte(VAL(BYTE, 00H));          (* end of expression              *)
          PutByte(VAL(BYTE, 0EBH));         (* begin expression               *)
          PutByte(VAL(BYTE, 2));
          PutByte(VAL(BYTE, 087H));         (* "A": displacement of label     *)
          ReferenceLabel(namelabel);
          PutByte(VAL(BYTE, 81H));          (* constant operand     *)
          PutLong(VAL(LONGINT, -16));       (* value of offset      *)
          PutByte(VAL(BYTE, 07H));          (* shift by 16 bits     *)
          PutByte(VAL(BYTE, 00H));          (* end of expression              *)
          AddBytesToObjectFile(4);

          IF class = Var THEN
            IF global THEN
              GenItem(x, obj, Scope);
              GetVariableName(x, varname);
              PutByte(VAL(BYTE, 0EBH));       (* begin normal expr    *)
              PutByte(VAL(BYTE, 4));
  
              PutByte(VAL(BYTE, 83H));        (* 1st operand is label *)
              PutLabelText(varname);
  
              PutByte(VAL(BYTE, 00H));        (* end of expression    *)
  
              PutByteConstant(VAL(BYTE, 1));  (* absolute long addressing *)
            ELSE (* local variable, place direct page offset *)
              PutLongConstant(VAL(LONGINT, vadr));
              PutByteConstant(VAL(BYTE, 0));  (* direct page offset *)
            END;
          ELSE
            PutLongConstant(VAL(LONGINT, offset));

            IF next = NIL THEN
              PutByteConstant(VAL(BYTE, 0)); (* no more fields *)
            ELSE
              PutByteConstant(VAL(BYTE, 1)); (* more field(s) *)
            END;
          END;

          AddBytesToObjectFile(5);
          INC(tableposition, 9);
      END;

      GenStructure(typ, FALSE);
    END;
  END;
END GenObject;

PROCEDURE GenDebugSymbols(header: ObjPtr);

  PROCEDURE DoPass(thisPass:  aPass; header: ObjPtr);
  VAR
    global: BOOLEAN;
    obj:    ObjPtr;
  BEGIN
    WITH header^ DO
      pass := thisPass;

      IF class = Module THEN
        obj := firstObj;
        global := TRUE;
      ELSE (* class = Proc *)
        obj := firstLocal;
        global := FALSE;
      END;

      WHILE obj <> NIL DO
        GenObject(obj, global);

        obj := obj^.next;
      END;
    END;
  END DoPass;

  PROCEDURE GenSymbolTableSize(header:  ObjPtr; VAR OK: BOOLEAN);
  VAR
    obj:        ObjPtr;
    global:     BOOLEAN;
  BEGIN
    tablesize := 0;
    tableposition := 1;

    (*
      We must go through the object list twice here.  The first pass is used
      to zero out the offsets in each of the record types so that we can set
      them up in the second pass for use in generating derived type records
      in the third pass.
    *)

    (*
      Clear the offsets and flags out so that the calculatingSize pass can
      work properly.
    *)
    DoPass(zeroingOffsets, header);
    DoPass(calculatingSize, header);

    (*
      Clear the offsets and flags out again, so that the buildingTable pass
      can work properly.
    *)
    DoPass(zeroingOffsets, header);

    IF tablesize <> 0 THEN
      (* COP 8/5 - Symbol table follows *)
      PutByteConstant(VAL(BYTE, COP));

      IF (header^.class = Module) AND NOT COP8asCOP5 THEN
        (* Use COP 8 for global variables *)
        PutByteConstant(VAL(BYTE, 8));
      ELSE
        (* Use COP 5 for local variables *)
        PutByteConstant(VAL(BYTE, 5));
      END;

      AddBytesToObjectFile(2);

      PutWordConstant(tablesize);
      AddBytesToObjectFile(2);
      OK := TRUE;
    ELSE
      OK := FALSE;
    END;
  END GenSymbolTableSize;

VAR
  OK:         BOOLEAN;
  
BEGIN
  (* place the table size into the object file *)
  GenSymbolTableSize(header, OK);

  (*
    If there were no symbols to define, then OK will be FALSE.
  *)
  IF OK THEN
    (* now generate the actual symbol table *)
    tableposition := 1;
    firstDone := TRUE;
    DoPass(buildingTable, header);
  END;
END GenDebugSymbols;

PROCEDURE GenLoadSource(filename: ARRAY OF CHAR);
VAR
  nameLabel:  CARDINAL;
  len:        CARDINAL;
BEGIN
  (* COP 6 - Load file named by 'filename' *)
  PutByteConstant(VAL(BYTE, COP));
  PutByteConstant(VAL(BYTE, 6));
  AddBytesToObjectFile(2);

  AllocPString(localStringList, filename, nameLabel, len);

  PutByte(VAL(BYTE, 0EBH));           (* begin expression               *)
  PutByte(VAL(BYTE, 2));

  PutByte(VAL(BYTE, 087H));           (* "A": displacement of label     *)
  ReferenceLabel(nameLabel);

  PutByte(VAL(BYTE, 00H));            (* end of expression              *)

  PutByte(VAL(BYTE, 0EBH));           (* begin expression               *)
  PutByte(VAL(BYTE, 2));

  PutByte(VAL(BYTE, 087H));           (* "A": displacement of label     *)
  ReferenceLabel(nameLabel);

  PutByte(VAL(BYTE, 81H));            (* constant operand     *)
  PutLong(VAL(LONGINT, -16));         (* value of offset      *)
  PutByte(VAL(BYTE, 07H));            (* shift by 16 bits     *)

  PutByte(VAL(BYTE, 00H));            (* end of expression              *)

  AddBytesToObjectFile(4);
END GenLoadSource;

END M2Debug.
