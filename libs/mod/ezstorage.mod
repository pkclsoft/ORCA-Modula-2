(*$Segment EZStorage*)
IMPLEMENTATION MODULE EZStorage;
(*
  Standard dynamic storage management

  Storage management for dynamic variables. Calls to the Modula-2 standard
  procedures NEW and DISPOSE are translated into calls to ALLOCATE and
  DEALLOCATE. The standard way to provide these two procedures is to import
  them from this module 'Storage'.

  Memory is allocated with these attributes:

      attrLocked    - It must not move as it is being referenced by a pointer
      attrFixed     - Ditto
      attrNoSpec    - Do not use special memory
*)

FROM M2Lib IMPORT
  UserID, ToolError;
FROM MemoryManager IMPORT
  Handle, DisposeHandle, NewHandle, AttributeSet, Attribute, MaxBlock,
  CheckHandle, SetHandleSize;
FROM SYSTEM IMPORT
  ADDRESS, TSIZE;

TYPE
  (*
    Every block allocated by this module will contain "aStorageRecord" at it's
    beginning.  This will allow us to deallocate the block when we want, and
    to tie the block to a particular process if need be.

    Currently, we reserve 16 bytes for this record.
  *)
  aStorageRecord =
    RECORD
      blockHandle:  Handle;
      padding:      ARRAY [1..12] OF CHAR;
    END;
  pStorageRecord = POINTER TO aStorageRecord;

PROCEDURE ALLOCATE(VAR a: ADDRESS; size: CARDINAL);
(*
  OPERATION:
    Allocate some dynamic storage (contiguous memory area).

  NOTE:
    The actual number of bytes allocated may be slightly greater than 'size',
    due to administrative overhead.

  Errors:
    If not enough space is available, or when attempting to allocate more than
    65520 (0FFF0H) bytes at once, then 'a' is returned as NIL.
*)
VAR
  allocRec:     aStorageRecord;
  allocRecPtr:  pStorageRecord;
BEGIN
  IF size > 65520 THEN
    a := NIL;
  ELSE
    WITH allocRec DO
      blockHandle := NewHandle(VAL(LONGINT, size + TSIZE(aStorageRecord)),
                               UserID(), 
                               AttributeSet{attrLocked, attrFixed, attrNoSpec}, 
                               NIL);
    END;

    IF ToolError() <> 0 THEN
      a := NIL;
    ELSE
      allocRecPtr := allocRec.blockHandle^;
      allocRecPtr^ := allocRec;
      a := VAL(ADDRESS, allocRecPtr) + VAL(ADDRESS, TSIZE(aStorageRecord));
    END;
  END;
END ALLOCATE;

PROCEDURE ALLOCATESTACK(VAR a: ADDRESS; size: CARDINAL);
(*
  OPERATION:
    Allocate some dynamic storage (contiguous memory area).  The difference
    between this and ALLOCATE, is that this procedure will ask for the memory
    to be allocated from bank zero, meaning it can be used as either stack
    space or directpage space.

    This sort of memory is needed when starting up tools manualy, or before
    starting a new process.

  NOTE:
    The actual number of bytes allocated may be slightly greater than 'size',
    due to administrative overhead.

  Errors:
    If not enough space is available, or when attempting to allocate more than
    65520 (0FFF0H) bytes at once, then 'a' is returned as NIL.
*)
VAR
  allocRec:     aStorageRecord;
  allocRecPtr:  pStorageRecord;
  attr:         AttributeSet;
BEGIN
  IF size > 65520 THEN
    a := NIL;
  ELSE
    attr := AttributeSet{attrLocked, attrFixed, attrNoCross, attrBank};

    WITH allocRec DO
      blockHandle := NewHandle(VAL(LONGINT, size + TSIZE(aStorageRecord)),
                               UserID(), attr, NIL);
    END;

    IF ToolError() <> 0 THEN
      a := NIL;
    ELSE
      allocRecPtr := allocRec.blockHandle^;
      allocRecPtr^ := allocRec;
      a := VAL(ADDRESS, allocRecPtr) + VAL(ADDRESS, TSIZE(aStorageRecord));
    END;
  END;
END ALLOCATESTACK;

PROCEDURE ChangeSize(a: ADDRESS; newSize: CARDINAL; VAR done: BOOLEAN);
(*
  OPERATION:
    Changes the size of the block to the specified size.  Note that due to the
    Fixed and Locked attributes use at allocation time, that the block will
    not be moved, so the change in size will not always work.  This is best
    used when trying to reduce the size of the block.

    The return value "done" is set to TRUE if and only if the change in size
    was successfull.
*)
VAR
  allocRec:     aStorageRecord;
  allocRecPtr:  pStorageRecord;
BEGIN
  IF a <> NIL THEN
    allocRecPtr := a - VAL(ADDRESS, TSIZE(aStorageRecord));

    CheckHandle(allocRecPtr^.blockHandle);

    IF ToolError() = 0 THEN
      SetHandleSize(VAL(LONGINT, newSize + TSIZE(aStorageRecord)), 
                    allocRecPtr^.blockHandle);

      IF ToolError() = 0 THEN
        done := TRUE;
      ELSE
        done := FALSE;
      END;
    ELSE
      done := FALSE;
    END;
  END;
END ChangeSize;

PROCEDURE DEALLOCATE (VAR a: ADDRESS; size: CARDINAL);
(*
  OPERATION:
    Release some dynamic storage (contiguous memory area).

  NOTE:
    The storage area released is made available for subsequent calls to
    ALLOCATE.

    'a' is set to NIL as a result of the deallocation.
*)
VAR
  allocRec:     aStorageRecord;
  allocRecPtr:  pStorageRecord;
BEGIN
  IF a <> NIL THEN
    allocRecPtr := a - VAL(ADDRESS, TSIZE(aStorageRecord));

    CheckHandle(allocRecPtr^.blockHandle);

    IF ToolError() = 0 THEN
      DisposeHandle(allocRecPtr^.blockHandle);

      IF ToolError() = 0 THEN
        a := NIL;
      END;
    END;
  END;
END DEALLOCATE;

PROCEDURE Available(size: CARDINAL) : BOOLEAN;
(*
  OPERATION:
    Returns TRUE if there exists a block of memory at lease 'size' bytes in
    size.
*)
VAR
  maxBlock: LONGINT;
BEGIN
  maxBlock := MaxBlock();

  IF maxBlock >= VAL(LONGINT, size) THEN
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END Available;

END EZStorage.
