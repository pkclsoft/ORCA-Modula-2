IMPLEMENTATION MODULE ListUtility;
(*
  Author  : John Cameron
*)

FROM Lists IMPORT 
  aList, aListLocation, LEndOfList, LGetCurrent, LGetFirst, LGetLocation, 
  LGetPosition, LGetNext, LGetNth, LGotoLocation, LInsertLast, LOpen, 
  MemoryError;
FROM SYSTEM IMPORT ADDRESS;

PROCEDURE DuplicateList( List: aList; VAR ListCopy: aList );
(*
  OPERATION:
    Makes a copy of a list.
  NOTE:
    The data pointed to by list entries is not duplicated - just the list
    structure.
    This allows you to have move than one position or ordering to your data
    entries.
    Of course, if you want to maintain this list as a duplicate you will have
    to duplicate all future additions and deletions from list.
    The original list positioning remains unchanged. The copied list is
    positioned similarly.
*)
VAR
  n: CARDINAL;
  OK: BOOLEAN;
  Ptr: ADDRESS;
  SaveLocation: aListLocation;
BEGIN
  LOpen( ListCopy );
  LGetLocation( List, SaveLocation);
  LGetPosition( List, n );
  LGetFirst( List, Ptr );

  WHILE NOT LEndOfList( List ) DO
    LInsertLast( ListCopy, Ptr, OK );

    IF NOT OK THEN
      MemoryError;
    END;

    LGetNext( List, Ptr );
  END;

  LGotoLocation(List, SaveLocation);
  LGetNth(ListCopy, n, Ptr);
END DuplicateList;

PROCEDURE FindEntryInList( List: aList; DataPtr: ADDRESS; VAR Found: BOOLEAN );
(*
  OPERATION
    Scans list entries looking for DataPtr.
    Returns Found TRUE if it finds it.
    If Found, list is left positioned at the found entry
    If NOT Found, list is left at end of list
*)
VAR
  Ptr: ADDRESS;
BEGIN
  (*
    Try for quick find. Check if it is current entry
  *)
  LGetCurrent( List, Ptr );
  Found := Ptr = DataPtr;

  IF NOT Found THEN
    (*
      Not current entry. Scan through whole list looking for it.
    *)
    LGetFirst( List, Ptr );

    WHILE NOT LEndOfList( List ) AND (Ptr <> DataPtr) DO
      LGetNext( List, Ptr );
    END;

    Found := Ptr = DataPtr;
  END;
END FindEntryInList;

BEGIN
END ListUtility.
 
