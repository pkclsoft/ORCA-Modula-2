MODULE Bench1;

FROM FileSystem IMPORT Close, Lookup, File, ReadWord, SetPos;
FROM InOut IMPORT WriteCard;
FROM OrcaShell IMPORT Stop, StopDCB;
(*FROM RealMath IMPORT Sin, Exp, Ln, Sqrt;*)
FROM Storage IMPORT ALLOCATE;
FROM SYSTEM IMPORT BYTE;
FROM Terminal IMPORT WriteString, Read, Write, WriteLn;

IMPORT EZDump;

TYPE
  NodePtr   = POINTER TO Node;
  Node      = RECORD
                next: NodePtr;
                x,y:  CARDINAL;
              END;
  GreatSet =  SET OF [0..31];

VAR
  A, B, C:  ARRAY [0..255] OF CARDINAL;
  D, E:     ARRAY [0..255] OF BYTE;
  M:        ARRAY [0..99],[0..99] OF CARDINAL;
  head:     NodePtr;
  f:        File;

PROCEDURE KeyPressed(): BOOLEAN;
VAR
  s:  StopDCB;
BEGIN
  s.pCount := 1;
  Stop(s);
  RETURN s.stopFlag;
END KeyPressed;

PROCEDURE P; END P;

PROCEDURE Q(x, y, z:  CARDINAL); END Q;

PROCEDURE Test(ch: CHAR);
VAR
  i, j, k:  CARDINAL;
  lj, lk:   LONGCARD;
  l, m, n:  INTEGER;
  ll, lm:   LONGINT;
  r0, r1, 
  r2, c0, 
  c1, c2, 
  c3:       REAL;
  rl0, rl1, 
  rl2:      LONGREAL;
  p:        NodePtr;
  b, b1:    BITSET;
  g, g1:    GreatSet;

  PROCEDURE Nested;
  BEGIN
  END Nested;

BEGIN
  CASE ch OF 
    "a":  k := 32000;

          REPEAT 
            DEC(k);
          UNTIL k = 0;
  | "b":  i := 32000;

          WHILE i > 0 DO
            DEC(i);
          END;
  | "c":  FOR i := 1 TO 32000 DO
          END;
  | "d":  m := 0;
          n := 10000;

          REPEAT
            DEC(n);
            INC(m);
            l := (n * 3) DIV (m * 5);
          UNTIL n = 0;
  | "e":  j := 0;
          k := 10000;

          REPEAT
            DEC(k);
            INC(j);
            i := (k * 3) DIV (j * 5);
          UNTIL k = 0;
  | "f":  k := 1000;
          r1 := 7.28;
          r2 := 34.8;

          REPEAT
            DEC(k);
            r0 := (r1 * r2) / (r1 + r2);
          UNTIL k = 0;
  | "g":  k := 1000;
          rl1 := 7.28;
          rl2 := 34.8;

          REPEAT
            DEC(k);
            rl0 := (rl1 * rl2) / (rl1 + rl2);
          UNTIL k = 0;
  | "h":
  | "i":  k := 100;
          c0 := 0.7;
          c1 := 2.0;
          c2 := 10.0;
          c3 := 18.0;

          REPEAT
(*            r0 := Sin(c0);
            r1 := Exp(c1);
            r0 := Ln(c2);
            r1 := Sqrt(c3);
*)
            DEC(k);
          UNTIL k = 0;
  | "j":  k := 20000;
          i := 0;
          B[0] := 73;

          REPEAT
            A[i] := B[i];
            B[i] := A[i];
            DEC(k);
          UNTIL k = 0;
  | "k":  i := 0;

          REPEAT
            j := 0;

            REPEAT
              M[i,j] := M[j,i];
              INC(j);
            UNTIL j = 100;

            INC(i);
          UNTIL i = 100;
  | "l":  k := 20000;
          i := 0;
          E[0] := VAL(BYTE, 73);

          REPEAT
            D[i] := E[i];
            E[i] := D[i];
            DEC(k);
          UNTIL k = 0;
  | "m":  k := 500;

          REPEAT
            DEC(k);
            A := B;
            B := C;
            C := A;
          UNTIL k = 0;
  | "n":  k := 500;

          REPEAT
            p := head;

            REPEAT
              p := p^.next;
            UNTIL p = NIL;

            DEC(k);
          UNTIL k = 0;
  | "o":  k := 5000;

          REPEAT
            DEC(k);
            ReadWord(f, i);
          UNTIL k = 0;

          SetPos(f, 0, 0);
  | "p":  l := 0;
          m := 1;
          i := 20000;

          REPEAT
            IF l < m THEN 
            END;

            IF l > m THEN 
            END;

            IF l = m THEN 
            END;

            IF (l < m) OR (l = m) OR (l > m) THEN 
            END;

            IF l = m THEN 
            ELSIF l > m THEN 
            ELSIF l < m THEN
            END;

            DEC(i);
          UNTIL i = 0;
  | "q":  ll := 0;
          lm := 1;
          i := 20000;

          REPEAT
            IF ll < lm THEN 
            END;

            IF ll > lm THEN 
            END;

            IF ll = lm THEN 
            END;

            IF (ll < lm) OR (ll = lm) OR (ll > lm) THEN 
            END;

            IF ll = lm THEN 
            ELSIF ll > lm THEN 
            ELSIF ll < lm THEN
            END;

            DEC(i);
          UNTIL i = 0;
  | "r":  j := 0;
          k := 1;
          i := 20000;

          REPEAT
            IF j < k THEN 
            END;

            IF j > k THEN 
            END;

            IF j = k THEN 
            END;

            IF (j < k) OR (j = k) OR (j > k) THEN 
            END;

            IF j = k THEN 
            ELSIF j > k THEN 
            ELSIF j < k THEN
            END;

            DEC(i);
          UNTIL i = 0;
  | "s":  lj := 0;
          lk := 1;
          i := 20000;

          REPEAT
            IF lj < lk THEN 
            END;

            IF lj > lk THEN 
            END;

            IF lj = lk THEN 
            END;

            IF (lj < lk) OR (lj = lk) OR (lj > lk) THEN 
            END;

            IF lj = lk THEN 
            ELSIF lj > lk THEN 
            ELSIF lj < lk THEN
            END;

            DEC(i);
          UNTIL i = 0;
  | "t":  r0 := 0.;
          r1 := 1.;
          i := 500;

          REPEAT
            IF r0 < r1 THEN 
            END;

            IF r0 > r1 THEN 
            END;

            IF r0 = r1 THEN 
            END;

            IF (r0 < r1) OR (r0 = r1) OR (r0 > r1) THEN 
            END;

            IF r0 = r1 THEN 
            ELSIF r0 > r1 THEN 
            ELSIF r0 < r1 THEN
            END;

            DEC(i);
          UNTIL i = 0;
  | "u":  k := 20000;

          REPEAT
            P;
            DEC(k);
          UNTIL k = 0;
  | "v":  k := 20000;

          REPEAT
            Q(i, j, k);
            DEC(k);
          UNTIL k = 0;
  | "w":  j := 1000;

          REPEAT
            b := {};
            i := 0;

            REPEAT
              INCL(b, i);
              INC(i);
            UNTIL i = 16;

            i := 0;

            REPEAT
              EXCL(b, i);
              INC(i);
            UNTIL i = 16;

            b := b1 + b;
            b := b1 - b;
            b := b1 * b;
            DEC(j);
          UNTIL j = 0;
  | "x":  j := 1000;

          REPEAT
            g := GreatSet{};
            i := 0;

            REPEAT
              INCL(g, i);
              INC(i);
            UNTIL i = 32;

            i := 0;

            REPEAT
              EXCL(g, i);
              INC(i);
            UNTIL i = 32;

            g := g1 + g;
            g := g1 - g;
            g := g1 * g;
            DEC(j);
          UNTIL j = 0;
  END;
END Test;

VAR
  ch: CHAR;
  n:  CARDINAL;
  q:  NodePtr;

BEGIN
  Lookup(f, "BenchFile", TRUE);
  head := NIL;
  n := 100;

  REPEAT
    q := head;
    NEW(head);
    head^.next := q;
    DEC(n);
  UNTIL n = 0;

  WriteString("Welcome to Kate BenchMark !");
  WriteLn;
  WriteLn;
  WriteString("Select a Benchmark & press open-apple-period after a selected period of time.");
  WriteLn;
  WriteLn;
  WriteString('["a".."x"]>'); 
  Read(ch);

  WHILE ("a" <= ch) AND (ch <= "x") DO
    n := 0;

    REPEAT
      INC(n);
      Test(ch);
    UNTIL KeyPressed();

    WriteString('"');
    Write(ch);
    WriteString('"--> ');
    WriteCard(n, 4);
    WriteString(' loops.');
    WriteLn;

    WriteString('["a".."x"]>'); 
    Read(ch);
  END;

  WriteLn;
  Close(f);
END Bench1.
