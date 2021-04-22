(*$Segment M2OMF*)
(*$RangeCheck+*)
(*$OverflowCheck+*)
IMPLEMENTATION MODULE M2OMF;

(*
  M2OMF.

  This module provides a way of creating an OMF file as the end result of a
  compile.  Note that at the time of writing, it was written to support v2.1
  of OMF.

  The OMF file is created wholly in memory. The module does not write the file
  out to disk unless explicitly told to do so by a call to FlushToDisk.

  Note that this module does a lot of memory allocation and deallocation.  It
  does NOT use the standard libraries to do this, as they are not 
  comprehensive enough.  We need to be able to extend a block as we need more
  space.  Only the GS Memory Manager provides this sort of functionality, so
  direct calls to the MM are used.
*)

FROM ASCII IMPORT nul;
FROM GSOSInterface IMPORT GSOSInString, StringToGSOSInString, AccessSet,
  accessAttributes;
FROM MemoryManager IMPORT HUnLock, SetHandleSize, HLock, NewHandle,
  DisposeHandle, AttributeSet, Attribute;
FROM M2Lib IMPORT UserID, ToolError, aTerminateStatus, Terminate, LoWORD,
  HighWORD;
FROM M2LM IMPORT objectCodeSize, objectDataSize;
FROM M2Shell IMPORT objectFileInError;
FROM M2SM IMPORT Mark;
FROM OrcaShell IMPORT FastFileDCB, FastFile, actSave, actAdd, actPurge,
  bDiskAndMemory, bMayBePurged;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;
FROM Strings IMPORT Assign, Length;
FROM SYSTEM IMPORT ADR, ADDRESS, BYTE, WORD, TSIZE;

CONST
  chunkSize         = 16384; (* number of bytes to extend the file by *)

  maxLabelCount     = 1000; (* Maximum # of labels for a single code segment *)

  maxConstants      = 0DFH; (* Maximum number of constant bytes in a local *)
                            (* CONST record. *)
TYPE
  pDisplacement = POINTER TO aDisplacement;

  pLabelReference = POINTER TO aLabelReference;
  aLabelReference =
    RECORD
      next: pLabelReference;
      ref:  aDisplacement;
    END;

  aLabel =
    RECORD
      defined:  BOOLEAN;
      location: aDisplacement;
      refList:  pLabelReference;
    END;
  pLabel = POINTER TO aLabel;

  aLongword =
    RECORD
      CASE :CARDINAL OF
        0:  lc:             LONGCARD;
      | 1:  li:             LONGINT;
      | 2:  d0, d1, d2, d3: BYTE;
      END;
    END;

VAR
  label:            ARRAY [1..maxLabelCount] OF aLabel;
  nextLabel:        CARDINAL;

  constant:         ARRAY [1..maxConstants] OF BYTE;
  nextConstant:     CARDINAL;

  nextSegment:      CARDINAL;

  segmentBytes:     LONGINT;

(*FORWARDS*)
PROCEDURE ExtendFile; FORWARD;
PROCEDURE InitialiseLabels; FORWARD;

PROCEDURE BackPatch(labelRef: pLabelReference; location: aDisplacement);
(*
  OPERATION:
    This procedure will patch the address described by 'labelRef' with the 
    location (or displacement) specified by 'location'.
*)
VAR
  refAdr: pDisplacement;
BEGIN
  WITH labelRef^ DO
    refAdr := VAL(ADDRESS, ref) + objFile.segmentData;
    refAdr^ := location;
  END;
END BackPatch;

PROCEDURE CloseOMFFile(discardFile: BOOLEAN;
                       saveToDisk:  BOOLEAN);
(*
  OPERATION:
    This procedure closes the 'file' if it is open.

    If discardFile is TRUE, then it simply disposes of any memory taken up
    by the file.

    If discardFile is FALSE, then saveToDisk is checked.  If TRUE, the memory
    file is written to disk.  Finally, the 'file' is given to FastFile, which
    leaves the file in memory, but removes it from our care.
*)
VAR
  fileSize: LONGINT;
  parms:    FastFileDCB;
  name:     GSOSInString;
BEGIN
  WITH objFile DO
    IF open THEN
      IF discardFile THEN
        DisposeOMFFile;
      ELSE
        (*
          Unlock the block so that FastFile may do with it as it wishes.
        *)
        HUnLock(handle);

        fileSize := VAL(LONGINT, (nextByte.a - handle^));

        StringToGSOSInString(fileName, name);

        WITH parms DO
          pCount := 14;

          IF saveToDisk THEN
            action := actSave;
            flags := {bDiskAndMemory, bMayBePurged};
          ELSE
            action := actAdd;
            flags := { };
          END;

          index := 0;
          filehandle := handle;
          pathName := ADR(name);
          access := AccessSet{aaReadEnabled, aaWriteEnabled, aaRenameEnabled,
                               aaDestroyEnabled};
          fileType := 0B1H; (* object file type *)
          auxType := 0;
          storageType := 0;
          createDate := 0;
          createTime := 0;
          modDate := 0;
          modTime := 0;
          optionList := NIL;
          eof := fileSize;
          blocksUsed := 0;
        END;

        FastFile(parms);

        IF (ToolError() = 0) AND saveToDisk THEN
          parms.action := actPurge;

          FastFile(parms);
        END;

        IF ToolError() <> 0 THEN
          Mark(504);
          objectFileInError := TRUE;
        END;
      END;

      open := FALSE;
    END;
  END;
END CloseOMFFile;

PROCEDURE DisposeOMFFile;
(*
  OPERATION:
    Dispose of any memory taken up by the 'file'.  This would normaly only
    be done if a compile error had occured.
*)
BEGIN
  WITH objFile DO
    (*
      Only try to dispose of the memory if we believe the file to be 'open'.
    *)
    IF open THEN
      DisposeHandle(handle);

      open := FALSE;
    END;
  END;
END DisposeOMFFile;

PROCEDURE DisposeReferences(VAR refList:  pLabelReference);
(*
  OPERATION:
    Will dispose of all label references in the specified list.
*)
VAR
  thisRef:  pLabelReference;
BEGIN
  WHILE refList <> NIL DO
    thisRef := refList;
    refList := refList^.next;

    DISPOSE(thisRef);
  END;
END DisposeReferences;

PROCEDURE EndSegment(kind:  aSegmentKind; attr: aSegmentAttributeSet);
(*
  OPERATION:
    This procedure completes the current segment within the file by updating 
    the header accordingly.
*)
CONST
  okSegment = aSegmentAttributeSet{atBankRelative, atSkipSegment, 
                                   atReloadSegment, atAbsoluteBank, 
                                   atNoSpecialMemory, atPosIndependant,
                                   atPrivate, atDynamic};

VAR
  offset: LONGCARD;
BEGIN
  WITH objFile DO
    IF open THEN
      PutByte(VAL(BYTE, 0)); (* end of segment *)

      segmentHeader^.KIND := ORD(kind) + VAL(CARDINAL, attr * okSegment);

      (*
        The BYTECNT field has to contain the total number of bytes taken up by
        the segment, including the header.
      *)
      offset := VAL(LONGCARD, nextByte.a - VAL(ADDRESS, segmentHeader));

      (*
        When the segment is initialised, BYTECNT is set to the size of the 
        header, including the variable length segment name.
      *)
      segmentHeader^.BYTECNT := offset;
      segmentHeader^.LENGTH := segmentBytes;
    END;
  END;

  (*
    When we end a segment, we want to clear out any labels that have been
    defined by a that segment.
  *)
  InitialiseLabels;
END EndSegment;

PROCEDURE EnsureAvailable(len:  CARDINAL);
(*
  OPERATION:
    Ensures that we have at least 'len' bytes available in the file to be
    written to.
*)
VAR
  remaining:  LONGCARD;
BEGIN
  WITH objFile DO
    IF open THEN
      (*
        We calculate the available space by subtracting the current byte 
        address from the address of the last available byte.
      *)
      remaining := VAL(LONGCARD, (lastByte - nextByte.a));

      (*
        Extend the file even if we have just enough.  I figure that we can save 
        some time by grabbing the extra space now, instead of finding out later
        that we have used up every last byte.
      *)
      IF remaining <= VAL(LONGCARD, len) THEN
        ExtendFile;
      END;
    END;
  END;
END EnsureAvailable;

PROCEDURE ExtendFile;
(*
  OPERATION:
    This procedure extends the 'file' by 'chunkSize' bytes, and updates the 
    file descriptor accordingly.
*)
VAR
  oldBlock: ADDRESS;
  offset:   ADDRESS;
BEGIN
  WITH objFile DO
    IF open THEN
      (*
        Save the starting address of the old block, in case the MM moves the
        file around.
      *)
      oldBlock := handle^;

      (*
        Unlock the block so that the MM can move it.
      *)
      HUnLock(handle);

      (*
        Increase the block size
      *)
      INC(size, chunkSize);
      SetHandleSize(size, handle);

      IF ToolError() <> 0 THEN
        Mark(505);
        objectFileInError := TRUE;
      END;

      (*
        Re-lock the block to prevent it moving when we don't want it to.
      *)
      HLock(handle);

      lastByte := handle^ + VAL(ADDRESS, size);

      (*
        Now adjust the various pointers and addresses in case the block had to
        be moved.
      *)
      IF handle^ <> oldBlock THEN
        offset := VAL(ADDRESS, segmentHeader) - oldBlock;
        segmentHeader := handle^ + offset;

        offset := segmentData - oldBlock;
        segmentData := handle^ + offset;

        offset := nextByte.a - oldBlock;
        nextByte.a := handle^ + offset;
      END;
    END;
  END;
END ExtendFile;

PROCEDURE FixLabel(labelNumber: CARDINAL);
(*
  OPERATION:
    This procedure is called when it is time to define the actual location of
    a label.  If there are any prior references to the label, then they are
    back-patched accordingly.
*)
VAR
  thisRef:  pLabelReference;
BEGIN
  WITH label[labelNumber] DO
    IF defined THEN
      Mark(229);
    ELSE
      defined := TRUE;
      location := segmentBytes;

      WHILE refList <> NIL DO
        thisRef := refList;
        refList := refList^.next;

        BackPatch(thisRef, location);

        DISPOSE(thisRef);
      END;
    END;
  END;
END FixLabel;

PROCEDURE FlushConstantsToFile;
(*
  OPERATION:
    This procedure will copy any byte constants currently in the constant 
    table to the file.
*)
VAR
  index:  CARDINAL;
  top:    CARDINAL;
BEGIN
  IF nextConstant > 1 THEN
    top := nextConstant - 1;  (* save the number of constants for the loop *)
    nextConstant := 1; (* reset to 1, so that PutByte doesn't get into a loop *)

    PutByte(VAL(BYTE, top));  (* Put the byte count as the CONST record header *)

    FOR index := 1 TO top DO    (* Place the bytes themselves into the file *)
      PutByte(constant[index]);
    END;
  END;
END FlushConstantsToFile;

PROCEDURE GenData(label:    ARRAY OF CHAR;
                  value:    ADDRESS;
                  length:   LONGCARD;
                  private:  BOOLEAN);
(*
  OPERATION:
    This procedure will generate a label, and associated data.  

    'label' is the textual label associated with the data.
    'value' contains the address of the first byte of the data.
    'length' is the number of bytes of data.
    'private' specifies whether to generate the label as being a private label.
*)
VAR
  byte:   pByte;
BEGIN
  IF objFile.open THEN
    PutByte(VAL(BYTE, 0E6H)); (* GLOBAL record *)
    PutLabelText(label);
    PutWord(SHORT(length));
    PutByte(VAL(BYTE, "H"));  (* Hex data following *)

    IF private THEN
      PutByte(VAL(BYTE, 1));    (* private flag = 1 --> private *)
    ELSE
      PutByte(VAL(BYTE, 0));    (* private flag = 0 --> public *)
    END;

    byte.a := value;

    WHILE length > 0 DO
      PutByteConstant(byte.b^);
      INC(byte.a);
      DEC(length);
    END;
  END;
END GenData;

PROCEDURE GenDS(label:    ARRAY OF CHAR;
                length:   LONGCARD;
                private:  BOOLEAN);
(*
  OPERATION:
    This procedure generates a label, and reserves 'length' bytes of space in
    the form:

      label DS length

    The 'private' parameter specifies whether to generate the label as being
    a private label.
*)
BEGIN
  IF objFile.open THEN
    PutByte(VAL(BYTE, 0E6H)); (* GLOBAL record *)
    PutLabelText(label);
    PutWord(0);
    PutByte(VAL(BYTE, "N"));  (* assembler directive, uses no space *)

    IF private THEN
      PutByte(VAL(BYTE, 1));    (* private flag = 1 --> private *)
    ELSE
      PutByte(VAL(BYTE, 0));    (* private flag = 0 --> public *)
    END;

    PutByte(VAL(BYTE, 0F1H)); (* DS record *)
    PutLong(length);
  END;
END GenDS;

PROCEDURE GenGlobalOp(instruction:  CARDINAL;
                      label:        ARRAY OF CHAR;
                      offset:       INTEGER;
                      shift:        INTEGER;
                      width:        CARDINAL);
(*
  OPERATION:
    This procedure will generate the OMF equivalent of an instruction.  The 
    operand for the instruction is a label as defined by 'label'.  An
    optional offset to that label is supplied in 'offset'.  'width' defines 
    the number of bytes to be generated as a result of the expression. 'shift'
    indicates that the result of the addition of the label and offset should
    be shifted.
*)
BEGIN
  IF objFile.open THEN
    (*
      Place the instruction in the file.  This is the easy part.
    *)
    PutByteConstant(VAL(BYTE, instruction));

    PutByte(VAL(BYTE, 0EBH));         (* begin normal expr    *)
    PutByte(VAL(BYTE, width));

    (*
      The first operand is the label itself.  In the case of a global label,
      insert the label name.
    *)
    PutByte(VAL(BYTE, 83H));            (* 1st operand is label *)
    PutLabelText(label);

    (*
      Now, if an offset has been specified, then generate a second operand
      that will be added to the label.
    *)
    IF offset <> 0 THEN
      PutByte(VAL(BYTE, 81H));          (* constant operand     *)
      PutLong(VAL(LONGINT, offset));   (* value of offset      *)
      PutByte(VAL(BYTE, 01H));          (* add offset to label  *)
    END;

    (*
      Finally, if asked to, we generate a second operation in the expression
      to shift the result right by 16 bits.
    *)
    IF shift <> 0 THEN
      PutByte(VAL(BYTE, 81H));          (* constant operand     *)
      PutLong(VAL(LONGINT, shift));       (* value of offset      *)
      PutByte(VAL(BYTE, 07H));          (* add offset to label  *)
    END;

    PutByte(VAL(BYTE, 00H));            (* end of expression    *)
  END;
END GenGlobalOp;

PROCEDURE GenLocalOp(instruction: CARDINAL;
                     labelNumber: CARDINAL;
                     relative:    BOOLEAN;
                     offset:      INTEGER;
                     shift:       BOOLEAN;
                     width:       CARDINAL);
(*
  OPERATION:
    This procedure will generate the OMF equivalent of an instruction.  The 
    operand for the instruction is a label as defined by 'labelNumber'.  An
    optional offset to that label is supplied in 'offset'.  If 'relative' is
    TRUE, then the operand is generated as a relative expression. 'width'
    defines the number of bytes to be generated as a result of the expression.
    'shift', if TRUE indicates that the operand should be shifted right 16
    bits.
*)
BEGIN
  IF objFile.open THEN
    (*
      Place the instruction in the file.  This is the easy part.
    *)
    PutByteConstant(VAL(BYTE, instruction));

    IF relative THEN
      (*
        The instruction is using relative addressing, so use a relative
        expression.
      *)
      PutByte(VAL(BYTE, 0EEH));         (* begin relative expr  *)
      PutByte(VAL(BYTE, width));
      PutLong(VAL(LONGCARD, width));
    ELSE
      PutByte(VAL(BYTE, 0EBH));         (* begin normal expr    *)
      PutByte(VAL(BYTE, width));
    END;

    (*
      The first operand is the label itself.  In the case of a local label,
      use 'ReferenceLabel' in insert the displacement of the label.
    *)
    PutByte(VAL(BYTE, 87H));            (* 1st operand is label *)
    ReferenceLabel(labelNumber);

    (*
      Now, if an offset has been specified, then generate a second operand
      that will be added to the label.
    *)
    IF offset <> 0 THEN
      PutByte(VAL(BYTE, 81H));          (* constant operand     *)
      PutLong(VAL(LONGINT, offset));   (* value of offset      *)
      PutByte(VAL(BYTE, 01H));          (* add offset to label  *)
    END;

    (*
      Finally, if asked to, we generate a second operation in the expression
      to shift the result right by 16 bits.
    *)
    IF shift THEN
      PutByte(VAL(BYTE, 81H));          (* constant operand     *)
      PutLong(VAL(LONGINT, -16));       (* value of offset      *)
      PutByte(VAL(BYTE, 07H));          (* add offset to label  *)
    END;

    PutByte(VAL(BYTE, 00H));            (* end of expression    *)
  END;
END GenLocalOp;

PROCEDURE GrabSomeBytes(VAR ptr:  ADDRESS;
                            len:  CARDINAL);
(*
  OPERATION:
    Allocates 'len' bytes from the end of the file.  If necessary, it extends
    the file in order to do so.  The result 'ptr' contains the address of the
    range of bytes.
*)
BEGIN
  WITH objFile DO
    IF open THEN
      (*
        Before we take up any space, we should flush any constants to the 
        file.
      *)
      IF nextConstant > 1 THEN
        FlushConstantsToFile;
      END;

      (*
        Make sure that we have enough space. Then set 'ptr' to point to the 
        current byte.  Finally, make nextByte point to the first byte after 
        the allocated area.
      *)
      EnsureAvailable(len);
      ptr := nextByte.a;
      INC(nextByte.a, VAL(ADDRESS, len));
    END;
  END;
END GrabSomeBytes;

PROCEDURE IncrementSegmentLength(bytes: LONGCARD);
(*
  OPERATION:
    Increments the length of the current segment by 'bytes'.
*)
BEGIN
  INC(segmentBytes, bytes);
END IncrementSegmentLength;

PROCEDURE InitialiseLabels;
(*
  OPERATION:
    Initialises the label table.  This involves disposing of any label 
    references that have built up, and marking the labels as NOT defined.
*)
BEGIN
  FOR nextLabel := 1 TO maxLabelCount DO
    WITH label[nextLabel] DO
      defined := FALSE;
      location := 0;

      DisposeReferences(refList);
    END;
  END;

  nextLabel := 1;
END InitialiseLabels;

PROCEDURE LabelDisplacement(labelNumber: CARDINAL): aDisplacement;
(*
  OPERATION:
    Returns the number of bytes between the current location within the current
    segment, and the label specified by 'labelNumber'.
*)
BEGIN
  WITH label[labelNumber] DO
    IF NOT defined THEN
      RETURN MAX(LONGCARD);
    ELSE
      RETURN ABS(segmentBytes - VAL(LONGINT, location));
    END;
  END;
END LabelDisplacement;

PROCEDURE NewLabel(VAR labelNumber: CARDINAL);
(*
  OPERATION:
    This procedure returns the number of a new label.  It assumes that the 
    label table has been initialised properly using 'InitialiseLabels' by 
    not doing any initialisation itself.
*)
BEGIN
  IF nextLabel < maxLabelCount THEN
    labelNumber := nextLabel;
    INC(nextLabel);
  ELSE
    Mark(227);
  END;
END NewLabel;

PROCEDURE NewSegmentHeader(name:    ARRAY OF CHAR);
(*
  OPERATION:
    This procedure will allocate a new segment header, and initialise it to be
    a static code segment.  The value of the "name" parameter is placed after
    the header, and it's length is used to determine the actual size of the
    header.

    Any fields that have a value defined by Apple are filled in.
*)
VAR
  segLen:   CARDINAL;
BEGIN
  WITH objFile DO
    IF open THEN
      (*
        Grab enough space for the header, LESS the segment name.  The segment
        name is inserted later.
      *)
      GrabSomeBytes(segmentHeader, SIZE(aSegmentHeader));

      (*
        The actual length of the segment header includes the segment name.
      *)
      segLen := Length(name) + SIZE(aSegmentHeader) + 1;

      IF segmentHeader <> NIL THEN
        WITH segmentHeader^ DO
          BYTECNT := VAL(LONGCARD, segLen);  (* This should be incremented as we add bytes *)
          RESSPC := 0;
          LENGTH := 0;
          undefined := VAL(BYTE, 0);
          LABLEN := VAL(BYTE, 0);
          NUMLEN := VAL(BYTE, 4);
          VERSION := VAL(BYTE, 2);
          REVISION := VAL(BYTE, 0);
          BANKSIZE := 10000H;
          attributes := aSegmentAttributeSet{atNoSpecialMemory};
          undefined2 := 0;
          ORG := NIL;
          ALIGN := 0;
          NUMSEX := VAL(BYTE, 0);
          SEGNUM := nextSegment;
          ENTRY := 0;
          DISPNAME := VAL(CARDINAL, (ADR(LOADNAME) - ADR(BYTECNT)));
          DISPDATA := segLen;
          tempORG := 0;
          LOADNAME := '          ';
        END;

        (*
          Insert the segment name after the header
        *)
        PutLabelText(name);

        Assign(name, segmentName);

        segmentData := nextByte.a;
      END;

      INC(nextSegment);

      segmentBytes := 0;
    END;
  END;
END NewSegmentHeader;

PROCEDURE OpenNewOMFFile(name: ARRAY OF CHAR);
(*
  OPERATION:
    Allocate the initial chunk of memory for the file, and initialise the 
    OMFFileID accordingly.
*)
BEGIN
  WITH objFile DO
    IF name[0] <> nul THEN
      (*
        The block may be virtually anywhere, but we want it locked.  We don't
        mind if it moves, but only at certain times.  At those times, we will
        explicitly unlock it.
      *)
      handle := NewHandle(chunkSize, UserID() + 0100H,
                          AttributeSet{attrNoSpec, attrLocked}, NIL);

      IF ToolError() = 0 THEN
        open := TRUE;
        Assign(name, fileName);
        size := chunkSize;
        segmentHeader := NIL; (* no header there yet *)
        segmentData := NIL;
        nextByte.a := handle^;
        lastByte := nextByte.a + VAL(ADDRESS, chunkSize);
        nextSegment := 1;
      ELSE
        open := FALSE;
      END;
    ELSE
      (*
        IF the name is a nul string, then don't bother trying to open the file
        up.
      *)
      open := FALSE;
    END;
  END;  
END OpenNewOMFFile;

PROCEDURE PutByte(byte: BYTE);
(*
  OPERATION:
    Insert one byte into the file.
*)
BEGIN
  WITH objFile DO
    IF open THEN
      (*
        Before we take up any space, we should flush any constants to the 
        file.
      *)
      IF nextConstant > 1 THEN
        FlushConstantsToFile;
      END;

      EnsureAvailable(1);

      nextByte.b^ := byte;
      INC(nextByte.a);
    END;
  END;
END PutByte;

PROCEDURE PutByteConstant(byte: BYTE);
(*
  OPERATION:
    Place a byte constant in the constant buffer.
*)
BEGIN
  WITH objFile DO
    IF open THEN
      IF nextConstant > maxConstants THEN
        FlushConstantsToFile;
      END;

      constant[nextConstant] := byte;
      INC(nextConstant);
    END;
  END;
END PutByteConstant;

PROCEDURE PutLabelText(label: ARRAY OF CHAR);
(*
  OPERATION:
    Inserts the supplied label into the file, preceded by a length byte.

    Note that this procedure does NOT use PutByte to insert each byte, for
    performance reasons.
*)
VAR
  count:  CARDINAL;
  index:  CARDINAL;
BEGIN
  WITH objFile DO
    IF open THEN
      (*
        Before we take up any space, we should flush any constants to the 
        file.
      *)
      IF nextConstant > 1 THEN
        FlushConstantsToFile;
      END;

      count := Length(label);

      (*
        Make sure we have enough space for the string, and the leading length
        byte.
      *)
      EnsureAvailable(count + 1);

      (*
        Place the length byte.
      *)
      nextByte.b^ := VAL(BYTE, count);
      INC(nextByte.a);

      (*
        Place the string into the file.
      *)
      index := 0;

      WHILE index < count DO
        nextByte.c^ := label[index];
        INC(nextByte.a);
        INC(index);
      END;
    END;
  END;
END PutLabelText;

PROCEDURE PutLong(long: LONGINT);
(*
  OPERATION:
    Insert four bytes into the file.
*)
VAR
  value: aLongword;
BEGIN
  WITH objFile DO
    IF open THEN
      value.li := long;
      PutByte(value.d0);
      PutByte(value.d1);
      PutByte(value.d2);
      PutByte(value.d3);
    END;
  END;
END PutLong;

PROCEDURE PutLongConstant(long: LONGINT);
(*
  OPERATION:
    Insert four bytes into the constant buffer.
*)
VAR
  value: aLongword;
BEGIN
  WITH objFile DO
    IF open THEN
      value.li := long;
      PutByteConstant(value.d0);
      PutByteConstant(value.d1);
      PutByteConstant(value.d2);
      PutByteConstant(value.d3);
    END;
  END;
END PutLongConstant;

PROCEDURE PutWord(word: WORD);
(*
  OPERATION:
    Insert two bytes into the file.
*)
VAR
  value:  CARDINAL;
BEGIN
  WITH objFile DO
    IF open THEN
      value := VAL(CARDINAL, word);
      PutByte(VAL(BYTE, value MOD 256));
      PutByte(VAL(BYTE, value DIV 256));
    END;
  END;
END PutWord;

PROCEDURE PutWordConstant(word: WORD);
(*
  OPERATION:
    Insert two bytes in the constant buffer.
*)
VAR
  value:  CARDINAL;
BEGIN
  WITH objFile DO
    IF open THEN
      value := VAL(CARDINAL, word);
      PutByteConstant(VAL(BYTE, value MOD 256));
      PutByteConstant(VAL(BYTE, value DIV 256));
    END;
  END;
END PutWordConstant;

PROCEDURE ReferenceLabel(labelNumber: CARDINAL);
(*
  OPERATION:
    This procedure generates a reference to the specified label.  The reference
    is 'inserted' at the current position in the 'file'.  This means that:

    o   If the label hasn't been defined, then a label reference record is 
        created, that refers to the next byte in the file.

    o   If the label has been defined, then it's location is inserted starting
        at the next byte in the file.

    Both cases result in four bytes being allocated in the file.
*)
VAR
  thisRef:      pLabelReference;
BEGIN
  IF objFile.open THEN
    IF labelNumber < nextLabel THEN
      WITH label[labelNumber] DO
        IF defined THEN
          (*
            It has been defined, so simply insert the displacement into the
            'file'.
          *)
          PutLong(location);
        ELSE
          (*
            Not defined yet... Create a label reference so that we can backpatch
            this location later on when it does get defined.
          *)
          NEW(thisRef);

          WITH thisRef^ DO
            next := refList;
            ref := VAL(aDisplacement, objFile.nextByte.a - objFile.segmentData);
          END;

          refList := thisRef;

          (*
            We need to reserve four bytes of space, as if there were a true
            displacement there.
          *)
          PutLong(0);
        END;
      END;
    ELSE
      Mark(227);
    END;
  END;
END ReferenceLabel;

BEGIN
  FOR nextLabel := 1 TO maxLabelCount DO
    WITH label[nextLabel] DO
      defined := FALSE;
      location := 0;
      refList := NIL;
    END;
  END;

  nextLabel := 1;

  nextConstant := 1;

  nextSegment := 1;

  objFile.open := FALSE;
END M2OMF.
