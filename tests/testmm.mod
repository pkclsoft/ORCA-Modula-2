MODULE TestMM;

FROM Storage IMPORT
  ALLOCATE, DEALLOCATE, Available;

TYPE
  pCARDINAL = POINTER TO CARDINAL;

  aRecord =
    RECORD
      a:  INTEGER;
      b:  CHAR;
      c:  LONGINT;
    END;
  pRecord = POINTER TO aRecord;

VAR
  pc: pCARDINAL;
  pr: pRecord;
  b:  BOOLEAN;

BEGIN
  NEW(pc);
  NEW(pr);

  pc^ := 1;
  pr^.b := 'A';

  b := Available(20);

  DISPOSE(pc);
  DISPOSE(pr);
END TestMM.
