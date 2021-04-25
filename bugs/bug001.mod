(*$NILCheck+*)
MODULE Bug001;

FROM InOut IMPORT WriteInt, WriteLn;
FROM Storage IMPORT ALLOCATE;

IMPORT EZDump;

TYPE
  a = ARRAY [1..10000] OF INTEGER;

VAR
  p:      POINTER TO a;
  count:  INTEGER;
BEGIN
  count := 0;

  LOOP
    WriteInt(count, 1);  WriteLn;
    count := count + 1;
    NEW(p);
  END;
END Bug001.
