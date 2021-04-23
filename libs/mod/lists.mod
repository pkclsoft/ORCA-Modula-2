IMPLEMENTATION MODULE Lists;
(*
  Author  : John Cameron
*)
FROM FileSystem IMPORT File, ReadWord, WriteWord;
FROM EZFileSystem IMPORT ReadNBytes, WriteNBytes;
FROM M2Lib IMPORT aTerminateStatus, Terminate;
FROM Storage IMPORT ALLOCATE, Available, DEALLOCATE;
FROM Strings IMPORT Assign, CompareStr;
FROM SYSTEM IMPORT ADDRESS, ADR, TSIZE;

CONST
  ValidList = 'OK';
TYPE
  aList = POINTER TO 
            RECORD            (* Opaque data type *)
              first, last:  aListLocation;
              count:        CARDINAL;       (* Number of entries in list *)
              current:      aListLocation;  (* Pointer to current entry *)
              position:     CARDINAL;       (* Position in list of current 
                                               entry, 0 if don't know *)
              validate:     ARRAY [0..1] OF CHAR;  (* Used to make sure this is
                                                      a valid list header *)
            END;
VAR
  FatalListErrors: aListErrorSet;

(*
  FORWARD Declarations
*)
PROCEDURE CheckList( List: aList ); FORWARD;
PROCEDURE CheckPtr( List: aList; ListPtr: aListLocation ); FORWARD;
PROCEDURE DumpEndOfList( VAR DumpFile: File; VAR OK: BOOLEAN ); FORWARD;
PROCEDURE DumpListEntryHeader( VAR DumpFile: File; VAR OK: BOOLEAN ); FORWARD;
PROCEDURE DumpListStatus( VAR DumpFile: File; ListStatus: aListStatus;
                          VAR OK: BOOLEAN ); FORWARD;
PROCEDURE InsertBetween( List: aList; DataPtr: ADDRESS; 
                         APtr, BPtr: aListLocation; VAR OK: BOOLEAN ); FORWARD;
PROCEDURE LinkBetween( List: aList; ListPtr, APtr, BPtr: aListLocation ); FORWARD;
PROCEDURE ReadListEntryHeader( VAR DumpFile: File; VAR More: BOOLEAN;
                               VAR OK: BOOLEAN ); FORWARD;
PROCEDURE ReadListStatus( VAR DumpFile: File; VAR ListStatus: aListStatus;
                          VAR OK: BOOLEAN ); FORWARD;
PROCEDURE Unlink( List: aList; ListPtr: aListLocation ); FORWARD;

PROCEDURE MemoryError;
BEGIN
  Terminate(tsOutOfMemory);
END MemoryError;

PROCEDURE GetListStatus( List: aList; VAR ListStatus: aListStatus );
(*
  OPERATION:
    Gets pointer independent state of list. 
    ie Contains no ListLocations - computes positions instead. 
    Calling this procedure triggers some error checking. The results of the
    checks are returned in the ListStatus.error field.
  NOTE:
    GetListStatus will accept a Nul List pointer as well as pointers to
    junk (unless the pointer is outside of memory or causes some other
    kind of memory access violation). In such cases, it will set 
    ListStatus.error accordingly and return.

    Preserves list positioning.
*)
VAR
  DataPtr: ADDRESS;
  LastNotLast: BOOLEAN;
  SaveLocation: aListLocation;
BEGIN
  IF List = NIL THEN
    ListStatus.error := leNul;
    RETURN;
  END;

  WITH List^ DO
    IF CompareStr(validate, ValidList) <> 0 THEN
      ListStatus.error := leInvalidList;
      RETURN; 
    END;

    SaveLocation := current;
    ListStatus.position := position;
    Assign( validate, ListStatus.validate );
    ListStatus.count := count;

    (*
      Use a funny way to get deduce first, last and current positions so that
      we preserve any bugs - ie I want to get status complete with bugs if
      possible.
    *)
    LGotoLocation( List, first );
    LGetPosition( List, ListStatus.first );
    LGotoLocation( List, last );
    LGetPosition( List, ListStatus.last );

    (*
      While we are on the supposed last, check that it is, in fact, last
    *)
    IF LEndOfList( List ) THEN
      LastNotLast := FALSE;
    ELSE
      LGetNext( List, DataPtr );
      LastNotLast := NOT LEndOfList( List );
    END;

    LGotoLocation( List, SaveLocation );
    LGetPosition( List, ListStatus.current );
  END;

  WITH ListStatus DO
    IF first <> 1 THEN
      error := leFirstNotFirst;
    ELSIF LastNotLast THEN
      error := leLastNotLast;
    ELSIF last <> count THEN
      error := leLastNECount;
    ELSIF current <> position THEN
      error := leCurrentNEPosition;
    ELSE
      error := leOK;
    END;
  END;
END GetListStatus;

PROCEDURE DumpList( VAR DumpFile: File;
                        List:     aList;
                        DumpProc: aListDumpProc;
                    VAR OK:       BOOLEAN );
(*
  OPERATION:

    Dumps the contents of the given list to the DumpFile in a standard
    format which can be read by ReadList - see below.
    The contents of individual list entries is dumped by the user supplied
    procedure, DumpProc
  NOTE:
    Warning to myself: Because of the nature of my data structures, ie Lists
    of entries which contain other lists, this procedure may be called
    recursively - Moral: Careful of globals
*)
VAR
  DataPtr: ADDRESS;
  ListStatus: aListStatus;
BEGIN
  GetListStatus( List, ListStatus );
  DumpListStatus( DumpFile, ListStatus, OK );

  IF NOT OK THEN
    RETURN;
  END;

  IF NOT (ListStatus.error IN FatalListErrors) THEN
    LGetFirst( List, DataPtr );

    WHILE NOT LEndOfList( List ) DO
      (*DumpStock entry is preceeded by 1 byte list entry header*)
      DumpListEntryHeader( DumpFile, OK ); 
      DumpProc( DumpFile, DataPtr, OK );

      IF NOT OK THEN
        RETURN;
      END;

      LGetNext( List, DataPtr );
    END; (*FOR each list entry*)

    DumpEndOfList( DumpFile, OK ); (*This dumps 1 byte list entry header with 
                                    special end of list value*)
  END;
END DumpList;

PROCEDURE LDelete( List: aList; DataBytes: CARDINAL );
(*
  OPERATION
    Deletes the current list entry.
    DataBytes gives the size of the list entry data records.
    The current list location is advanced to the following entry. If the
    last entry is deleted, LEndOFList will return TRUE and the current 
    list location is set to undefined (NIL).
 NOTE
    It is important that the size is correct (ie corresponds to actual size
    of the data).
    The code cannot check this, so if you screw up, you pay the consequences.
  WARNING
    Be careful about deleting list entries that contain pointers to
    other dynamic data. In these cases, you should return the dynamic data
    to the free pool (using DISPOSE or DEALLOCATE) before deleting the list
    entry.
*)
VAR
  OldCurrent: aListLocation;
BEGIN
  CheckList( List );
  OldCurrent := List^.current;
  CheckPtr( List, OldCurrent );

  WITH List^ DO
    current := OldCurrent^.forward;

    IF current = NIL THEN
      position := 0;
    END;
  END;

  Unlink( List, OldCurrent );

  DEALLOCATE( OldCurrent^.data, DataBytes );
  DISPOSE( OldCurrent );
END LDelete;

PROCEDURE LDeleteAll( List: aList; DataBytes: CARDINAL );
(*
  OPERATION
    Deletes the all entries in the given list.
    DataBytes gives the size of the list entry data records.
    LEndOFList will return TRUE and the current list location is 
    set to undefined (NIL).
 NOTE
    It is important that the size is correct (ie corresponds to actual size
    of the data).
    The code cannot check this, so if you screw up, you pay the consequences.
  WARNING
    Be careful about deleting list entries that contain pointers to
    other dynamic data. In these cases, you should return the dynamic data
    to the free pool (using DISPOSE or DEALLOCATE) before deleting the list
    entry.
*)
VAR
  Next, Ptr: aListLocation;
BEGIN
  CheckList( List );
  Ptr := List^.first;

  WHILE Ptr <> NIL DO
    Next := Ptr^.forward;
    DEALLOCATE( Ptr^.data, DataBytes );
    DISPOSE( Ptr );
    Ptr := Next;
  END;
  
  WITH List^ DO
    first := NIL;
    last := NIL;
    current := NIL;
    position := 0;
    count := 0;
  END;
END LDeleteAll;

PROCEDURE LEndOfList( List: aList ): BOOLEAN;
(*
  OPERATION
    Returns TRUE if the current list location is at the end of the list.
  NOTE
    When we are positioned at the end of a list, the current list location 
    will be undefined (NIL).
    The end of a list can be reached either by going past the last entry in
    the list, or by backing up before the first entry.
    For example, LEndOfList may be set TRUE by repeated LGetNext's or by
    repeated LGetPrevious's.
*)
BEGIN
  CheckList( List );
  RETURN List^.current = NIL;
END LEndOfList;

PROCEDURE LFindLocation(List: aList; DataPtr: ADDRESS; VAR Found: BOOLEAN);
(*
  OPERATION:
    Given the data address, find the appropriate list entry.

    NOTE:
      If the specified data is not found, then Found is set to FALSE, and the
      current position is left unchanged.

      If it is found, then the current position is set to be that of the 
      located data, and Found is set to TRUE.
*)
VAR
  oldLocation:  aListLocation;
  listData:     ADDRESS;
BEGIN
  CheckList(List);
  LGetLocation(List, oldLocation);

  LGetFirst(List, listData);

  WHILE (listData <> DataPtr) AND
        (NOT LEndOfList(List)) DO
    LGetNext(List, listData);
  END;

  IF listData = DataPtr THEN
    Found := TRUE;
  ELSE
    Found := FALSE;
    LGotoLocation(List, oldLocation);
  END;
END LFindLocation;

PROCEDURE LGet( List: aList; ListPtr: aListLocation; VAR DataPtr: ADDRESS );
(*
  OPERATION
    Retrieves data pointer to data at the given list location

    The list location is updated to point to this new entry.

  NOTE
    ListPtr better be a valid list location, because no checks are made.
*)
BEGIN
  CheckList( List );
  CheckPtr( List, ListPtr );
  DataPtr := ListPtr^.data;
  List^.current := ListPtr;
  List^.position := 0;
END LGet;

PROCEDURE LGetCurrent( List: aList; VAR DataPtr: ADDRESS );
(*
  OPERATION
    Retrieves data pointer of current entry in List.

    If the list is at end of list, the DataPtr returns NIL;
*)
BEGIN
  CheckList( List );

  WITH List^ DO
    IF current = NIL THEN
      DataPtr := NIL
    ELSE
      DataPtr := current^.data;
    END;
  END;
END LGetCurrent;

PROCEDURE LGetFirst( List: aList; VAR DataPtr: ADDRESS );
(*
  OPERATION
    Retrieves data pointer of first entry in List.

    The list location is updated to point to this new entry.

    If no such list entry exists, the DataPtr returns NIL;
*)
BEGIN
  CheckList( List );

  WITH List^ DO
    IF first = NIL THEN
      DataPtr := NIL;
      position := 0;
    ELSE
      DataPtr := first^.data;
      position := 1;
    END;

    current := first;
  END;
END LGetFirst;

PROCEDURE LGetLast( List: aList; VAR DataPtr: ADDRESS );
(*
  OPERATION
    Retrieves data pointer of last entry in List.

    The list location is updated to point to this new entry.

    If no such list entry exists, the DataPtr returns NIL;
*)
BEGIN
  CheckList( List );

  WITH List^ DO
    IF last = NIL THEN
      DataPtr := NIL
    ELSE
      DataPtr := last^.data;
    END;

    current := last;
    position := count;
  END;
END LGetLast;

PROCEDURE LGetLocation( List: aList; VAR Current: aListLocation );
(*
  OPERATION
    Retrieves current location in list (sort of like a record number)

    If the list is currently positioned at End of List (LEndOfList returns
    TRUE), Current returns a special undefined (NIL) value.
*)
BEGIN
  CheckList( List );
  Current := List^.current;
END LGetLocation;

PROCEDURE LGetNext( List: aList; VAR DataPtr: ADDRESS );
(*
  OPERATION
    Advances current list location and retrieves the corresponding
    data pointer.

    If the current list location is already at the end of the list,
    the list location becomes undefined (NIL), and DataPtr returns NIL.
*)
BEGIN
  CheckList( List );

  WITH List^ DO
    current := current^.forward;

    IF current = NIL THEN
      DataPtr := NIL;
      position := 0;
    ELSE
      DataPtr := current^.data;

      IF position > 0 THEN
        INC( position );
      END;
    END;
  END;
END LGetNext;

PROCEDURE LGetNth( List: aList; N: CARDINAL; VAR DataPtr: ADDRESS );
(*
  OPERATION
    Retrieves data pointer to data at the N'th list entry (origin 1).

    The list location is updated to point to this new entry.

    If no such entry exists, the list is positioned at End of List and
    DataPtr returns NIL.

  NOTE
    Lists are by nature sequential data structures, so that random accesses
    to particular entries may not be very efficient. However, this procedure
    will find the N'th entry quite efficiently in the following cases:
            1) if the N'th entry is near the start of the list
            2) if the N'th entry is near the end of the list
            3) if the N'th entry is near the current list location
    In particular, case 3 makes it quite practical to scan a list sequentially
    using successive calls to LGetNth (instead of LGetNext or LGetPrevious).

    If you need to access the list randomly by entry number, with more 
    efficiency, you can always construct your own index to the list.
    Use your index to find a list location from an entry number and then
    use LGet to get that specific entry.
    
*)
VAR
  d: INTEGER;
  Distance: CARDINAL;
  ListPtr: aListLocation;
  SearchForwards: BOOLEAN;
BEGIN
  CheckList( List );

  WITH List^ DO
    (*
      First check whether such an entry exists
    *)
    IF (N = 0) OR (N > count) THEN
      DataPtr := ADDRESS( NIL );
      current := NIL;
      position := 0;
    ELSE
      (*
        Work out the best starting point for the search: first, last or current.
      *)
      ListPtr := first;
      Distance := N-1;
      SearchForwards := TRUE;

      IF (count - N) < Distance THEN
        Distance := count - N;
        ListPtr := last;
        SearchForwards := FALSE;
      END;

      (*
        Consider current list position if we know it.
      *)
      IF position > 0 THEN
        d := VAL(INTEGER, N) - VAL(INTEGER, position);

        IF ORD(ABS(d)) < Distance THEN
          Distance := ORD(ABS(d));
          ListPtr := current;
          SearchForwards := d > 0;
        END;
      END;

      WHILE Distance > 0 DO
        IF SearchForwards THEN
          ListPtr := ListPtr^.forward
        ELSE
          ListPtr := ListPtr^.back
        END;

        DEC(Distance);
      END;
    
      DataPtr := ListPtr^.data;
      current := ListPtr;
      position := N;
    END; (*ELSE*)
  END; (*WITH List^*)
END LGetNth;

PROCEDURE LGetPosition( List: aList; VAR N: CARDINAL );
(*
  OPERATION
    Retrieves the position of the current entry within the list.

    If the list is currently positioned at End of List (LEndOfList returns
    TRUE), N returns 0.

  NOTE
    This will usually be an efficient operation because I try to keep track
    of current position at all times. However, some operations lose the
    current position. In these cases, I have to scan the list in order to
    find out my current position, which, of course, is not particularly
    efficient for long lists.
    I lose current position any time we are at end of list.
    Also the following procedures always lose current position:
           LGet
           LSort
    After losing the current position I will get back on the tracks if you 
    call any of the following procedures:
           LGetFirst, LGetLast, LInsertFirst, LInsertLast, LGetNth, 
           LGetPosition, LMerge, LMoveFirst, LMoveLast.

*)
VAR
  ListPtr: aListLocation;
BEGIN
  CheckList( List );

  WITH List^ DO
    IF position = 0 THEN
      (*
        Try and find position unless we are at end of list
      *)
      IF current <> NIL THEN
        position := 1;
        ListPtr := first;

        WHILE (ListPtr <> NIL) AND (ListPtr <> current) DO
          INC(position);
          ListPtr := ListPtr^.forward;
        END;

        IF ListPtr = NIL THEN
          HALT;
        END;
      END; (*IF current <> NIL*)
    END; (*IF position = 0*)

    N := position;
  END; (*WITH List^*)
END LGetPosition;

PROCEDURE LGetPrevious( List: aList; VAR DataPtr: ADDRESS );
(*
  OPERATION
    Backs up current list location and retrieves the corresponding
    data pointer.

    If the current list location is already at the beginning of the list,
    the list location becomes undefined (NIL), and DataPtr returns NIL.
*)
BEGIN
  CheckList( List );

  WITH List^ DO
    current := current^.back;

    IF current = NIL THEN
      DataPtr := NIL;
      position := 0;
    ELSE
      DataPtr := current^.data;

      IF position > 0 THEN
        DEC( position );
      END;
    END;
  END;
END LGetPrevious;

PROCEDURE LGotoLocation( List: aList; Location: aListLocation );
(*
  OPERATION:
    Positions the list at the specified list location.
  NOTE:
    Location may be NIL, in which case list is placed at end-of-list.
*)
BEGIN
  CheckList( List );

  IF Location <> NIL THEN
    CheckPtr( List, Location );
  END;

  WITH List^ DO
    current := Location;
    position := 0;
  END;
END LGotoLocation;

PROCEDURE LInsertAfter( List: aList; DataPtr: ADDRESS; VAR OK: BOOLEAN );
(* 
  OPERATION
    Inserts the data pointed to by DataPtr into the List AFTER the current
    list location.

    If current list location is undefined (NIL), the entry is moved to the 
    end of the list.

    If the list is empty, this entry becomes the first in the list.

    The list location is updated to point to this new entry.

    OK is FALSE if not enough memory to add new entry. You should release
    some memory and try again.
*)
BEGIN
  CheckList( List );

  IF List^.current = NIL THEN
    InsertBetween( List, DataPtr, List^.last, NIL, OK );
    List^.position := List^.count;
  ELSE
    InsertBetween( List, DataPtr, List^.current, List^.current^.forward, OK );

    IF List^.position > 0 THEN
      INC( List^.position );
    END;
  END;
END LInsertAfter;

PROCEDURE LInsertBefore( List: aList; DataPtr: ADDRESS; VAR OK: BOOLEAN );
(* 
  OPERATION
    Inserts the data pointed to by DataPtr into the List BEFORE the current
    list location.

    If current list location is undefined (NIL), the entry is moved to the 
    front of the list.

    If the list is empty, this entry becomes the first in the list.

    The list location is updated to point to this new entry.

    OK is FALSE if not enough memory to add new entry. You should release
    some memory and try again.
*)
BEGIN
  CheckList( List );

  IF List^.current = NIL THEN
    InsertBetween( List, DataPtr, NIL, List^.first, OK );
    List^.position := 1;
  ELSE
    InsertBetween( List, DataPtr, List^.current^.back, List^.current, OK );
    (*
      List position is unchanged
    *)
  END;
END LInsertBefore;

PROCEDURE LInsertFirst( List: aList; DataPtr: ADDRESS; VAR OK: BOOLEAN );
(* 
  OPERATION
    Inserts the data pointed to by DataPtr at the front of the List.

    The list location is updated to point to this new entry.

    OK is FALSE if not enough memory to add new entry. You should release
    some memory and try again.
*)
BEGIN
  CheckList( List );
  InsertBetween( List, DataPtr, NIL, List^.first, OK );
  List^.position := 1;
END LInsertFirst;

PROCEDURE LInsertLast( List: aList; DataPtr: ADDRESS; VAR OK: BOOLEAN );
(* 
  OPERATION
    Appends the data pointed to by DataPtr to the end of the List.

    The list location is updated to point to this new entry.

    OK is FALSE if not enough memory to add new entry. You should release
    some memory and try again.
*)
BEGIN
  CheckList( List );
  InsertBetween( List, DataPtr, List^.last, NIL, OK );
  List^.position := List^.count;
END LInsertLast;

PROCEDURE LLength( List: aList ): CARDINAL;
(*
  OPERATION
    Returns the number of entries in the list.
    Returns 0 if list is empty.
*)
BEGIN
  CheckList( List );
  RETURN List^.count;
END LLength;

PROCEDURE LMerge( List: aList; DataPtr: ADDRESS; Less: aLessProc; 
                  VAR OK: BOOLEAN );
(* 
  OPERATION
    Merges the data pointed to by DataPtr into the List at its correct sorted
    list location, as specified by the ordering defined by Less.

    Less is a BOOLEAN function as described in LSort.

    The list location is updated to point to this new entry.

    OK is FALSE if not enough memory to add new entry. You should release
    some memory and try again.
*)
VAR
  pos: CARDINAL;
  next, prev: aListLocation;
BEGIN
  CheckList( List );

  (*
    Scan down looking for first entry not less than data pointed to by DataPtr
  *)
  pos := 1;
  next := List^.first;

  WHILE (next <> NIL) AND Less( next^.data, DataPtr ) DO
    INC(pos);
    next := next^.forward;
  END;

  List^.position := pos;

  (*
    Find predecessor to next, and insert new entry after that
  *)
  IF next = NIL THEN
    (* Entry belongs at end of List *)
    prev := List^.last
  ELSE
    prev := next^.back
  END;

  InsertBetween( List, DataPtr, prev, next, OK );
END LMerge;

PROCEDURE LMoveAfter( List: aList; ListPtr: aListLocation );
(*
  OPERATION
    Moves the given list entry to the new location AFTER current list location.

    If current list location is undefined (NIL), the entry is moved to the 
    end of the list.

    The list location is updated to point to ListPtr.
*)
BEGIN
  CheckList( List );
  CheckPtr( List, ListPtr );

  (*
    Nothing to do if attempting to move relative to self

  *)
  IF ListPtr <> List^.current THEN
    Unlink( List, ListPtr );

    IF List^.current = NIL THEN
      LinkBetween( List, ListPtr, List^.last, NIL );
      List^.position := List^.count;
    ELSE
      LinkBetween( List, ListPtr, List^.current, List^.current^.forward );

      IF List^.position > 0 THEN
        INC( List^.position );
      END;
    END;

    List^.current := ListPtr;  
  END;
END LMoveAfter;

PROCEDURE LMoveBefore( List: aList; ListPtr: aListLocation );
(*
  OPERATION
    Moves the given list entry to the new location BEFORE current list location.

    If current list location is undefined (NIL), the entry is moved to the 
    front of the list.

    The list location is updated to point to ListPtr.
*)
BEGIN
  CheckList( List );
  CheckPtr( List, ListPtr );

  (*
    Nothing to do if attempting to move relative to self

  *)
  IF ListPtr <> List^.current THEN
    Unlink( List, ListPtr );

    IF List^.current = NIL THEN
      LinkBetween( List, ListPtr, NIL, List^.first );
      List^.position := 1;
    ELSE
      LinkBetween( List, ListPtr, List^.current^.back, List^.current );

      IF List^.position > 0 THEN
        DEC( List^.position );
      END;
    END;

    List^.current := ListPtr;  
  END;
END LMoveBefore;

PROCEDURE LMoveFirst( List: aList; ListPtr: aListLocation );
(* 
  OPERATION
    Moves the given list entry to first in the list.

    The list location is updated to point to ListPtr.
*)
BEGIN
  CheckList( List );
  CheckPtr( List, ListPtr );
  Unlink( List, ListPtr );
  LinkBetween( List, ListPtr, NIL, List^.first );
  List^.position := 1;
END LMoveFirst;

PROCEDURE LMoveLast( List: aList; ListPtr: aListLocation );
(* 
  OPERATION
    Moves the given list entry to last in the list.

    The list location is updated to point to ListPtr.
*)
BEGIN
  CheckList( List );
  CheckPtr( List, ListPtr );
  Unlink( List, ListPtr );
  LinkBetween( List, ListPtr, List^.last, NIL );
  List^.position := List^.count;
END LMoveLast;

PROCEDURE LOpen( VAR List: aList );
(*
  OPERATION
    Creates a new (empty) list
*)
BEGIN
  NEW( List );

  WITH List^ DO
    first := NIL;          
    last := NIL;          
    current := NIL;
    count := 0;
    position := 0;
    validate := ValidList;
  END;
END LOpen;

PROCEDURE LPurge( VAR List: aList );
(*
  OPERATION
    Deletes a list.
  NOTE
    Only empty lists may be purged. To completely get rid of a list that has
    entries in it, do a LDeleteAll followed by a LPurge.
    If you attempt to purge a non empty list, this procedure will crash the
    system.
*)
BEGIN
  CheckList( List );

  IF List^.first = NIL THEN
    List^.validate := 'XX';
    DISPOSE( List );
  ELSE
    (*
      List not empty
    *)
    HALT;
  END;
END LPurge;

PROCEDURE LRemove( List: aList; VAR DataPtr: ADDRESS );
(*
  OPERATION
    Removes the current list entry from the list, and returns a pointer to
    the data.
    The current list location is advanced to the following entry. If the
    last entry is deleted, LEndOFList will return TRUE and the current 
    list location is set to undefined (NIL).

 NOTE
    This is different from LDelete which also removes an entry from the list,
    but deletes the data as well.
    LRemove does not delete the data. 
*)
VAR
  OldCurrent: aListLocation;
BEGIN
  CheckList( List );
  OldCurrent := List^.current;
  CheckPtr( List, OldCurrent );

  WITH List^ DO
    current := OldCurrent^.forward;
    IF current = NIL THEN
      position := 0;
    END;
  END;

  Unlink( List, OldCurrent );

  DataPtr := OldCurrent^.data;
  DISPOSE( OldCurrent );
END LRemove;

PROCEDURE LSort( List: aList; Less: aLessProc );
(*
  OPERATION
    Sorts the list into ascending order as specified by Less.

    Less should be a BOOLEAN function that, given pointers to two list
    data areas, returns TRUE if the first data should be sorted as less
    than the second data.
*)
VAR
  lowerPtr, next, NextTarget, target: aListLocation;
  i, j, lower, n, upper: CARDINAL;
  TargetLessThanNext: BOOLEAN;
BEGIN
  CheckList( List );

  WITH List^ DO
    (*
      Don't bother sorting if list is empty 
    *)
    IF first <> NIL THEN
      (*
        Use a variant of a simple insertion sort as described in, for example, 
        "The Art of Computer Programming", Vol 3, Knuth, p81. 
        The variation is that I use a binary search to find position to insert
        into, rather than a simple linear search (which is more usual).
        The primary advantage of the binary search is that it reduces the number
        of comparisons needed for the sort. Comparisons will dominate the sort
        time in this case, so they are worth minimizing.
                                             JC 21 November 88
      *)
      target := first^.forward;
      n := 0;

      WHILE target <> NIL DO
        INC(n);
        NextTarget := target^.forward;

        (*
          Do quick check to see if target is already in sorted position
        *)
        IF Less( target^.data, target^.back^.data ) THEN
          (*
            Do binary search through list entries 1 to n looking for where target
            belongs.
          *)
          lower := 1;
          upper := n;
          lowerPtr := first;

          WHILE upper >= lower DO
            i := (lower+upper) DIV 2;
            next := lowerPtr;

            FOR j := lower+1 TO i DO
              next := next^.forward;
            END;

            TargetLessThanNext := Less( target^.data, next^.data );

            IF TargetLessThanNext THEN
              upper := i - 1;
            ELSE
              lower := i + 1;
              lowerPtr := next^.forward;
            END;
          END;
  
          (*
            If target needs to be moved, move it into its correct sorted position
          *)
          IF TargetLessThanNext THEN
            (*
              Insert target before next
            *)
            Unlink( List, target );
            LinkBetween( List, target, next^.back, next );
          ELSE
            (*
              Check whether target needs to be moved at all
            *)
            IF next <> target^.back THEN
              (*
                Insert target after next
              *)
              Unlink( List, target );
              LinkBetween( List, target, next, next^.forward );
            END;
          END;
        END;

        target := NextTarget;
      END;
    END;

    position := 0;
  END; (*WITH List^*)
END LSort;

PROCEDURE ReadList( VAR DumpFile: File;
                    VAR List:     aList;
                        ReadProc: aListReadProc;
                    VAR OK:       BOOLEAN );
(*
  OPERATION:
    Reads the dump of a list made by DumpList, see above. Individual list
    entries are read in by the user supplied procedure ReadProc.
    It is the user's responsibility to ensure that his ReadProc matches his
    DumpProc, used when the list was dumped.
  NOTE:
    Warning to myself: Because of the nature of my data structures, ie Lists
    of entries which contain other lists, this procedure may be called
    recursively - Moral: Careful of globals
*)
VAR
  DataPtr: ADDRESS;
  ListStatus: aListStatus;
  More: BOOLEAN;
BEGIN
  ReadListStatus( DumpFile, ListStatus, OK );
  IF NOT OK THEN RETURN END;

  IF ListStatus.error IN FatalListErrors THEN
    CASE ListStatus.error OF
      leNul:
        List := aList(NIL);
      |
      leInvalidList:
        LOpen( List );
        UpdateListStatus( List, ListStatus );
    END;
  ELSE
    LOpen( List );
    ReadListEntryHeader( DumpFile, More, OK );
    IF NOT OK THEN RETURN END;

    WHILE More DO
      ReadProc( DumpFile, DataPtr, OK );
      IF NOT OK THEN RETURN END;

      LInsertLast( List, DataPtr, OK );
      IF NOT OK THEN RETURN END;

      ReadListEntryHeader( DumpFile, More, OK );
      IF NOT OK THEN RETURN END;
    END;

    UpdateListStatus( List, ListStatus );
  END;
END ReadList;

PROCEDURE UpdateListStatus( List: aList; ListStatus: aListStatus );
(*
  OPERATION:
    Restores list header according a list status as provided by 
    GetListStatus.
  NOTE:
    DO USE THIS PROCEDURE UNLESS YOU REALLY KNOW WHAT YOU ARE DOING.
    It is potentially very dangerous. It will set the list status
    without any regard to the actual contents of the list. For example, the
    count of items in the list will be set irrespective of the actual number
    of entries.
*)
VAR
  DataPtr: ADDRESS;
  CurrentLocation: aListLocation;
  FirstLocation: aListLocation;
  LastLocation: aListLocation;
BEGIN
  WITH ListStatus DO
    LGetNth( List, first, DataPtr );
    LGetLocation( List, FirstLocation );
    LGetNth( List, last, DataPtr );
    LGetLocation( List, LastLocation );
    LGetNth( List, current, DataPtr );
    LGetLocation( List, CurrentLocation );
  END;

  WITH List^ DO
    first := FirstLocation;
    last := LastLocation;
    count := ListStatus.count;
    current := CurrentLocation;
    position := ListStatus.position;
    Assign( ListStatus.validate, validate );
  END;
END UpdateListStatus;

(*
  -----------------------------------------------------------------------
            UTILITY ROUTINES
  -----------------------------------------------------------------------
*)
PROCEDURE CheckList( List: aList );
(*
  OPERATION:
    Check that ListPtr is a valid list location in List.

    If not, terminates program.
*)
BEGIN
  IF ListChecking THEN
    IF (List = NIL) OR (CompareStr(List^.validate, ValidList) <> 0) THEN
      HALT;
    END;
  END;
END CheckList;

PROCEDURE CheckPtr( List: aList; ListPtr: aListLocation );
(*
  OPERATION:
    Check that ListPtr is a valid list location in List.

    If not, terminates program.
  NOTE:
    NIL is NOT a valid list location as far as this procedure is concerned.
*)
VAR
  Ptr: aListLocation;
BEGIN
  IF ListChecking THEN
    Ptr := List^.first;

    WHILE (Ptr <> NIL) AND (Ptr <> ListPtr) DO
      Ptr := Ptr^.forward;
    END;

    IF Ptr = NIL THEN
      HALT;
    END;
  END;
END CheckPtr;

PROCEDURE DumpEndOfList( VAR DumpFile: File; VAR OK: BOOLEAN );
(*
  OPERATION:
    Writes termination byte, indicating that no more list entries follow
*)
CONST
  More = TRUE;
BEGIN
  WriteWord( DumpFile, NOT More );
  OK := TRUE;
END DumpEndOfList;

PROCEDURE DumpListEntryHeader( VAR DumpFile: File; VAR OK: BOOLEAN );
(*
  OPERATION:
    Writes header byte, indicating that a list entry follows
*)
CONST
  More = TRUE;
BEGIN
  WriteWord( DumpFile, More );
  OK := TRUE;
END DumpListEntryHeader;

PROCEDURE DumpListStatus( VAR DumpFile: File; ListStatus: aListStatus;
                          VAR OK: BOOLEAN );
VAR
  Written: LONGINT;
BEGIN
  WriteNBytes( DumpFile, TSIZE(aListStatus), ADR(ListStatus), Written );
  OK := Written >= TSIZE(aListStatus);
END DumpListStatus;

PROCEDURE InsertBetween( List: aList; DataPtr: ADDRESS; 
                         APtr, BPtr: aListLocation; VAR OK: BOOLEAN );
(*
  OPERATION
    Creates a new list entry and adds it to List between the entries pointed to
    by APtr and BPtr. DataPtr points to the data for the new entry.

    The list location is updated to point to this new entry.


    If APtr is NIL, the entry is inserted at the front of the list. 
    BPtr is ignored.

    If BPtr is NIL, the entry is inserted at the end of the list.
    APtr is ignored.

    OK is FALSE if not enough memory to add new entry. You should release
    some memory and try again.
*)
VAR
  NewOne: aListLocation;
BEGIN
  OK := Available( TSIZE( aListLocation ) );

  IF OK THEN
    NEW( NewOne );
    NewOne^.data := DataPtr;
    LinkBetween( List, NewOne, APtr, BPtr );
    List^.current := NewOne;
  END;
END InsertBetween;

PROCEDURE LinkBetween( List: aList; ListPtr, APtr, BPtr: aListLocation );
(*
  OPERATION
    Adds an existing list entry (ListPtr) (which is not already linked
    into the list) to the new location between entries APtr and BPtr. 

    Increments list count.

    If APtr is NIL, the entry is moved to the front of the list. 
    BPtr is ignored.

    If BPtr is NIL, the entry is moved to the end of the list.
    APtr is ignored.

    If neither APtr nor BPtr is NIL then they must be successive list entries.
    That is, APtr^.forward = Bptr etc. If this is not so, the procedure will
    crash.
*)
VAR
  temp: aListLocation;
BEGIN
  IF APtr = NIL THEN
    (*
      Make first entry in the list
    *)
    temp := List^.first;
    IF temp <> NIL THEN
      temp^.back := ListPtr;
    END;

    ListPtr^.forward := temp;
    ListPtr^.back := NIL;
    List^.first := ListPtr;

    IF List^.last = NIL THEN
      List^.last := ListPtr;
    END;
  ELSIF BPtr = NIL THEN
    (*
      Make last entry in the list
    *)
    temp := List^.last;

    IF temp <> NIL THEN
      temp^.forward := ListPtr;
    END;

    ListPtr^.back := temp;
    ListPtr^.forward := NIL;
    List^.last := ListPtr;

    IF List^.first = NIL THEN
      List^.first := ListPtr;
    END;
  ELSE
    (*
      Check that they are successive entries
    *)
    IF APtr^.forward <> BPtr THEN
      HALT;
    END;

    (*
      Inserting somewhere in middle of list
    *)
    APtr^.forward := ListPtr;
    BPtr^.back := ListPtr;
    ListPtr^.forward := BPtr;
    ListPtr^.back := APtr;
  END;

  INC(List^.count);
END LinkBetween;

PROCEDURE ReadListEntryHeader( VAR DumpFile: File; VAR More: BOOLEAN;
                               VAR OK: BOOLEAN );
(*
  OPERATION:
    Read list entry header (or end of list, if no more entries) from file
*)
BEGIN
  ReadWord( DumpFile, More );
  OK := TRUE;
END ReadListEntryHeader;

PROCEDURE ReadListStatus( VAR DumpFile: File; VAR ListStatus: aListStatus;
                          VAR OK: BOOLEAN );
(*
  OPERATION:
    Read list status details from a file
*)
VAR
  nRead: LONGINT;
BEGIN
  ReadNBytes( DumpFile, TSIZE(aListStatus), ADR(ListStatus), nRead );
  OK := nRead >= TSIZE(aListStatus);
END ReadListStatus;

PROCEDURE Unlink( List: aList; ListPtr: aListLocation );
(*
  OPERATION
    Removes the given list entry from the list.
    Decrements list count.
  NOTE
    Does not delete the entry, just pulls it out of list.
*)
VAR
  Prev, Next: aListLocation;
BEGIN
  Prev := ListPtr^.back;
  Next := ListPtr^.forward;

  IF Prev = NIL THEN
    List^.first := Next;
  ELSE
    Prev^.forward := Next;
  END;

  IF Next = NIL THEN
    List^.last := Prev
  ELSE
    Next^.back := Prev;
  END;

  ListPtr^.forward := NIL;
  ListPtr^.back := NIL;

  DEC(List^.count);
END Unlink;

BEGIN
  ListChecking := TRUE;
  FatalListErrors := aListErrorSet{ leNul, leInvalidList };
END Lists.
 
