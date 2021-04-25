MODULE TestDebug;

FROM ASCII IMPORT nul;
FROM InOut IMPORT WriteString, WriteCard, WriteLn;
FROM Storage IMPORT ALLOCATE;
FROM SYSTEM IMPORT ADR;
(*
FROM TestDebug2 IMPORT Proc1, Proc2;
*)

TYPE
  arec =  RECORD
            cardfield:  CARDINAL;
            realfield:  REAL;
          END;

VAR
  onebi:  (one, two, three);
  twobi:  INTEGER;
  fourbi: LONGINT;
  twobc:  CARDINAL;
  fourbc: LONGCARD;
  real:   REAL;
  lreal:  LONGREAL;
  char:   CHAR;
  bool1:  BOOLEAN;
  bool2:  BOOLEAN;
  str:    ARRAY [0..20] OF CHAR;
  arr1:   ARRAY [0..5] OF CARDINAL;
  arr2:   ARRAY [1..2],[0..3] OF CHAR;

  rec1:   arec;
  rec2:   RECORD
            a:  arec;
            b:  CARDINAL;
            c:  arec;
          END;
  rec3:   ARRAY [1..5] OF arec;
  rec4:   POINTER TO arec;

(*
TYPE
  enum = (four, five, six);

  aponebi =  POINTER TO enum;
  aptwobi =  POINTER TO INTEGER;
  apfourbi = POINTER TO LONGINT;
  aptwobc =  POINTER TO CARDINAL;
  apfourbc = POINTER TO LONGCARD;
  apreal =   POINTER TO REAL;
  aplreal =  POINTER TO LONGREAL;
  apchar =   POINTER TO CHAR;
  apbool1 =  POINTER TO BOOLEAN;
  apbool2 =  POINTER TO BOOLEAN;
  apstr =    POINTER TO ARRAY [0..20] OF CHAR;
  aparr1 =   POINTER TO ARRAY [0..5] OF CARDINAL;
  aparr2 =   POINTER TO ARRAY [1..2],[0..3] OF CHAR;

VAR
  ponebi:  aponebi;
  ptwobi:  aptwobi;
  pfourbi: apfourbi;
  ptwobc:  aptwobc;
  pfourbc: apfourbc;
  preal:   apreal;
  plreal:  aplreal;
  pchar:   apchar;
  pbool1:  apbool1;
  pbool2:  apbool2;
  pstr:    apstr;
  parr1:   aparr1;
  parr2:   aparr2;

  pponebi:  POINTER TO aponebi;
  pptwobi:  POINTER TO aptwobi;
  ppfourbi: POINTER TO apfourbi;
  pptwobc:  POINTER TO aptwobc;
  ppfourbc: POINTER TO apfourbc;
  ppreal:   POINTER TO apreal;
  pplreal:  POINTER TO aplreal;
  ppchar:   POINTER TO apchar;
  ppbool1:  POINTER TO apbool1;
  ppbool2:  POINTER TO apbool2;
  ppstr:    POINTER TO apstr;
  pparr1:   POINTER TO aparr1;
  pparr2:   POINTER TO aparr2;

PROCEDURE Proc0;
VAR
  n:  CARDINAL;
BEGIN
  FOR n := 0 TO 10 DO
    WriteString('n = ');
    WriteCard(n, 2);
    WriteLn;

    str[n+1] := nul;
    str[n] := CHR(n+ORD("A"));
  END;

  Proc2;
END Proc0;
*)
BEGIN
  str := '';
  onebi := one;
  onebi := three;
  twobi := MIN(INTEGER);
  fourbi := MIN(LONGINT);
  twobi := MAX(INTEGER);
  fourbi := MAX(LONGINT);
  twobc := MIN(CARDINAL);
  fourbc := MIN(LONGCARD);
  twobc := MAX(CARDINAL);
  fourbc := MAX(LONGCARD);
  real := MIN(REAL);
  real := MAX(REAL);
  lreal := MIN(LONGREAL);
  lreal := MAX(LONGREAL);
  char := MIN(CHAR);
  char := MAX(CHAR);
  bool1 := FALSE;
  bool2 := TRUE;

(*
  Proc1;

  Proc0;
*)

  FOR twobi := 1 TO 2 DO
    FOR twobc := 0 TO 3 DO
      arr2[twobi,twobc] := CHR(twobc+ORD("a"));
    END;
  END;

  rec1.cardfield := 5;
  rec1.realfield := 20.4;

  WITH rec2 DO
    a.cardfield := 6;
    a.realfield := 30.1;
    b := 7;
    c := rec1;
  END;

  rec3[4] := rec2.a;
  rec3[1] := rec1;
  rec3[5] := rec1;

  NEW(rec4);
  rec4^ := rec3[4];

(*
  NEW(ponebi);
  NEW(ptwobi);
  NEW(pfourbi);
  NEW(ptwobc);
  NEW(pfourbc);
  NEW(preal);
  NEW(plreal);
  NEW(pchar);
  NEW(pbool1);
  NEW(pbool2);
  NEW(pstr);
  NEW(parr1);
  NEW(parr2);

  pstr^ := '';
  ponebi^ := four;
  ponebi^ := six;
  ptwobi^ := MIN(INTEGER);
  pfourbi^ := MIN(LONGINT);
  ptwobi^ := MAX(INTEGER);
  pfourbi^ := MAX(LONGINT);
  ptwobc^ := MIN(CARDINAL);
  pfourbc^ := MIN(LONGCARD);
  ptwobc^ := MAX(CARDINAL);
  pfourbc^ := MAX(LONGCARD);
  preal^ := MIN(REAL);
  preal^ := MAX(REAL);
  plreal^ := MIN(LONGREAL);
  plreal^ := MAX(LONGREAL);
  pchar^ := MIN(CHAR);
  pchar^ := MAX(CHAR);
  pbool1^ := FALSE;
  pbool2^ := TRUE;

  FOR twobi := 1 TO 2 DO
    FOR twobc := 0 TO 3 DO
      ptwobi^ := twobi;
      ptwobc^ := twobc;
      parr2^[ptwobi^, ptwobc^] := CHR(ptwobc^+ORD("A"));
    END;
  END;

  pponebi := ADR(ponebi);
  pptwobi := ADR(ptwobi);
  ppfourbi := ADR(pfourbi);
  pptwobc := ADR(ptwobc);
  ppfourbc := ADR(pfourbc);
  ppreal := ADR(preal);
  pplreal := ADR(plreal);
  ppchar := ADR(pchar);
  ppbool1 := ADR(pbool1);
  ppbool2 := ADR(pbool2);
  ppstr := ADR(pstr);
  pparr1 := ADR(parr1);
  pparr2 := ADR(parr2);

  FOR twobi := 1 TO 2 DO
    FOR twobc := 0 TO 3 DO
      pptwobi^^ := twobi;
      pptwobc^^ := twobc;
      pparr2^^[pptwobi^^, pptwobc^^] := CHR(pptwobc^^+ORD("!"));
    END;
  END;
*)
END TestDebug.
