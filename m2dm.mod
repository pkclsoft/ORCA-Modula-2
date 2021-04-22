(*$Segment M2DM*)
(*$RangeCheck+*)
(*$OverflowCheck+*)
IMPLEMENTATION MODULE M2DM;  (* NW 20.5.84; WH 19.8.86 *)

FROM InOut IMPORT WriteString, WriteCard, WriteHex, WriteLn;
FROM M2Lib IMPORT LoWORD, HighWORD;
FROM M2Shell IMPORT ShowProgress;
FROM SYSTEM IMPORT ADDRESS, ADR, TSIZE;

IMPORT Storage;
IMPORT EZStorage;

CONST
  heapSize      = 64000;
  mainHeapSize  = 64000;

TYPE
  pHeap     =   POINTER TO aHeap;

  aHeap     =   RECORD
                  buffer:   ADDRESS;
                  size:     CARDINAL;
                  current:  ADDRESS;
                  last:     ADDRESS;
                  next:     pHeap;
                END;

VAR
  mainHeap:     pHeap;
  heaps:        pHeap;
  currentHeap:  pHeap;

PROCEDURE ALLOCATE(VAR a: ADDRESS; n: CARDINAL);
BEGIN
  IF currentHeap = NIL THEN
    IF ShowProgress THEN
      WriteLn;
      WriteString("No heap created from which to allocate space");
    END;

    HALT;
  END;

  IF n >= Available() THEN
    IF ShowProgress THEN
      WriteLn;
      WriteString("Unable to allocate heap space (wanted:");
      WriteCard(n, 6);
      WriteString(", got:");
      WriteCard(Available(), 6);
      WriteString(").");
      WriteLn;
    END;

    HALT;
  END;

  WITH currentHeap^ DO
    a := current;
    current :=  current + VAL(ADDRESS, n);
  END;
END ALLOCATE;

PROCEDURE Available(): CARDINAL;
BEGIN
  IF currentHeap <> NIL THEN
    WITH currentHeap^ DO
      RETURN last - current;
    END;
  ELSE
    RETURN 0;
  END;
END Available;

PROCEDURE CloseHeaps;
VAR
  current:  pHeap;
  previous: pHeap;
BEGIN
  current := heaps;

  WHILE current <> NIL DO
    WITH current^ DO
      EZStorage.DEALLOCATE(buffer, size);
    END;

    previous := current;
    current := current^.next;

    Storage.DEALLOCATE(previous, TSIZE(aHeap));
  END;

  heaps := NIL;
END CloseHeaps;

PROCEDURE CloseMainHeap;
BEGIN
  WITH mainHeap^ DO
    EZStorage.DEALLOCATE(buffer, size);
  END;

  Storage.DEALLOCATE(mainHeap, TSIZE(aHeap));

  mainHeap := NIL;
END CloseMainHeap;

PROCEDURE NewHeap;
VAR
  new:      pHeap;
  current:  pHeap;
BEGIN
  Storage.ALLOCATE(new, TSIZE(aHeap));

  WITH new^ DO
    EZStorage.ALLOCATE(buffer, heapSize);

    IF buffer = NIL THEN
      IF ShowProgress THEN
        WriteLn;
        WriteString("Unable to allocate a new heap.");
      END;

      HALT;
    END;
(*
    WriteString('Heap Address: ');
    WriteHex(HighWORD(VAL(LONGINT, buffer)), 4);
    WriteHex(LoWORD(VAL(LONGINT, buffer)), 4);
    WriteString('H, Heap Length: ');
    WriteHex(heapSize, 4);
    WriteString('H');
    WriteLn;
*)  
    size := heapSize;
    current := buffer;
    last := current + VAL(ADDRESS, heapSize);
    next := NIL;
  END;

  IF heaps = NIL THEN
    heaps := new;
  ELSE
    current := heaps;

    WHILE current^.next <> NIL DO
      current := current^.next;
    END;

    current^.next := new;
  END;

  currentHeap := new;
END NewHeap;

PROCEDURE ResetHeap(a: ADDRESS);
BEGIN
  IF currentHeap = NIL THEN
    IF ShowProgress THEN
      WriteLn;
      WriteString("Resetting a non-existant heap."); 
    END;

    HALT;
  END;

  WITH currentHeap^ DO
    current := a;
  END;
END ResetHeap;

PROCEDURE ResizeHeap;
VAR
  newSize:  CARDINAL;
  OK:       BOOLEAN;
  oldAdr:   ADDRESS;
  oldLen:   CARDINAL;
BEGIN
  IF currentHeap = mainHeap THEN
    IF ShowProgress THEN
      WriteLn;
      WriteString("Resizing the main heap.");
    END;

    HALT;
  END;

  WITH currentHeap^ DO
    oldAdr := buffer;
    oldLen := size;
    newSize := ((VAL(CARDINAL, current - buffer) DIV 256) + 1) * 256;

    EZStorage.ChangeSize(buffer, newSize, OK);

    IF OK THEN
      last := buffer + VAL(ADDRESS, newSize);
      size := newSize;
(*
      WriteString('Changed heap size...');
      WriteLn;
      WriteString('Old Heap Address: ');
      WriteHex(HighWORD(VAL(LONGINT, oldAdr)), 4);
      WriteHex(LoWORD(VAL(LONGINT, oldAdr)), 4);
      WriteString('H, Old Heap Length: ');
      WriteHex(oldLen, 4);
      WriteString('H');
      WriteLn;

      WriteString('New Heap Address: ');
      WriteHex(HighWORD(VAL(LONGINT, buffer)), 4);
      WriteHex(LoWORD(VAL(LONGINT, buffer)), 4);
      WriteString('H, New Heap Length: ');
      WriteHex(size, 4);
      WriteString('H');
      WriteLn;
*)
    ELSIF ShowProgress THEN
      WriteLn;
      WriteString("Failed to resize a heap from ");
      WriteCard(size, 5);
      WriteString(" bytes to ");
      WriteCard(newSize, 5);
      WriteString(" bytes.");

      HALT;
    END;
  END;
END ResizeHeap;

PROCEDURE RevertToMainHeap;
BEGIN
  currentHeap := mainHeap;
END RevertToMainHeap;

PROCEDURE InitM2DM;
BEGIN
  Storage.ALLOCATE(mainHeap, TSIZE(aHeap));

  WITH mainHeap^ DO
    EZStorage.ALLOCATE(buffer, mainHeapSize);

    IF buffer = NIL THEN
      IF ShowProgress THEN
        WriteLn;
        WriteString("Unable to allocate the main heap.");
      END;

      HALT;
    END;
(*
    WriteString('Main Heap Address: ');
    WriteHex(HighWORD(VAL(LONGINT, buffer)), 4);
    WriteHex(LoWORD(VAL(LONGINT, buffer)), 4);
    WriteString('H, Main Heap Length: ');
    WriteHex(mainHeapSize, 4);
    WriteString('H');
    WriteLn;
*)
    size := mainHeapSize;
    current := buffer;
    last := current + VAL(ADDRESS, mainHeapSize);
    next := NIL;
  END;

  currentHeap := mainHeap;
END InitM2DM;

BEGIN
  mainHeap := NIL;
  heaps := NIL;
  currentHeap := NIL;
END M2DM.
