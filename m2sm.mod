(*$Segment M2SM*)
(*$RangeCheck+*)
(*$OverflowCheck+*)
IMPLEMENTATION MODULE M2SM; (* NW 17.8.83 / 23.3.85; HS 31.5.86 / 28.4.89 *)

  FROM ASCII IMPORT nul, cr, bs;
  FROM ConsoleIO IMPORT scEnableMouse, scDisableMouse, scSetNormal, 
    scSetInverse;
  FROM EZFileSystem IMPORT MakeFileName;
  FROM FastFileSystem IMPORT
    Load, ReadChar, GetPos, SetPos, Purge, Response;
  FROM GSOSInterface IMPORT GSOSOutString, GSOSNameString, getNameDCB, 
    GetName, AccessSet, aaReadEnabled, StringToGSOSOutString;
  FROM InOut IMPORT WriteString, Write, WriteLn, WriteCard, WriteHex,
    WriteLongInt;
  FROM MemoryManager IMPORT Handle;
  FROM M2Debug IMPORT lineType, step, breakPoint, autoGo, breakPointCode, 
    autoGoCode;
  FROM M2DM IMPORT MaxCard, rngchk, ovflchk, stackchk, ObjPtr, nilchk, databank;
  FROM M2HM IMPORT errorInProc, curLev;
  FROM M2Lib IMPORT ToolError;
  FROM M2LM IMPORT segmentName, IdToSymbol, CDAName, CDAShutdown, NDAOpen,
       NDAClose, NDAAction, NDAPeriod, NDAMask, NDAMenuline, Stacksize,
       aSymbol, KeepName, ChainToName, CDEVName;
  FROM M2Shell IMPORT ShowProgress, userHasAborted, objectFileInError,
    outFileMask, keepStyle, KeepObject, AllErrFatal, terminalError,
    OutputFile, ListSource, PauseOnError;
  FROM NumberConversion IMPORT CardToString;
  FROM OrcaShell IMPORT StopDCB, Stop, aKeepStyle, ConsoleOutDCB, ConsoleOut;
  FROM ResourceManager IMPORT LoadResource, GetCurResourceFile, sysFileID,
    CloseResourceFile, OpenResourceFile;
  FROM Storage IMPORT ALLOCATE, DEALLOCATE;
  FROM Strings IMPORT Assign, Concat, Length, Insert;
  FROM Terminal IMPORT BusyRead, Read;
  FROM SYSTEM IMPORT ADR, ADDRESS;

CONST
  KW            = 44;   (* number of keywords *)
  maxDig        = 15;   (* to avoid floating overflow in scanner *)
  maxExp        = 308;  (* note the correspondence with the pow-array! *)
  maxBuf        = 30;   (* maximum length for a number *)
  IdBufLim      = IdBufLeng - 100;
  maxDirective  = 17;
  eol = cr;

TYPE
  l2w =
    RECORD
      CASE :BOOLEAN OF
        TRUE:   long:   LONGINT;
      | FALSE:  p1,p0:  CARDINAL;
      END;
    END;

  aSymbolPos =
    RECORD
      pos:      l2w;
      line:     l2w;
      lineNum:  CARDINAL;
      colNum:   CARDINAL;
    END;

VAR
  ch:           CHAR;      (* current character *)
  id0:          CARDINAL;
  id1:          CARDINAL;  (* indices of identifier buffer *)
  pow:          ARRAY [0..8] OF LONGREAL; (* must cover exponent range *)
  keyTab:       ARRAY [0..KW-1] OF
                  RECORD
                    sym: Symbol; 
                    ind: CARDINAL; 
                  END;
  D:            CARDINAL;
  K:            CARDINAL;
  lastLine:     l2w;
  prevSym:      aSymbolPos;
  lastSym:      aSymbolPos;
  sourceColumn: CARDINAL;
  eolDet:       BOOLEAN;
  dirTab:       ARRAY [0..maxDirective-1] OF
                  RECORD
                    directive:  aDirective;
                    name:       CARDINAL;
                  END;

  errorStringHandle:  Handle;
  errorString:        POINTER TO ARRAY [0..79] OF CHAR;

  unknownError:
    RECORD
      CASE :BOOLEAN OF
        TRUE:   message:  ARRAY [0..55] OF CHAR;
      | FALSE:  text:     ARRAY [0..16] OF CHAR;  (* "Unknown error. (#" *)
                number:   ARRAY [0..2] OF CHAR;
                text2:    ARRAY [0..34] OF CHAR;  (* ")  Please contact customer support." *)
      END;
    END;

  myResourceFile: CARDINAL;

  PROCEDURE ClearModList;
  VAR
    ce, ne: pModListEntry;
  BEGIN
    ce := modList;

    WHILE ce <> NIL DO
      ne := ce;
      ce := ce^.next;

      DEALLOCATE(ne, SIZE(aModListEntry));
    END;

    modList := NIL;
  END ClearModList;

  PROCEDURE DisplayLastLine;
  VAR
    buf:  CHAR;
    curr: l2w;
  BEGIN
    IF ShowProgress THEN
      GetPos(source, curr.long);

      SetPos(source, lastLine.long);
      ReadChar(source, buf);

      WHILE (buf <> eol) AND (NOT source.eof) DO
        Write(buf);
        ReadChar(source, buf);
      END;

      WriteLn;

      SetPos(source, curr.long);
    END;
  END DisplayLastLine;

  PROCEDURE GetCh;
  BEGIN
    ReadChar(source, ch);

    IF source.res = done THEN
      IF ch = eol THEN
        IF eolDet THEN
          INC(sourceLine);
          sourceColumn := 1;
          GetPos(source, lastLine.long);

          IF ListSource AND ShowProgress THEN
            WriteLn;
          END;
        ELSIF ListSource THEN
          DisplayLastLine;
        END;

        eolDet := TRUE;
      ELSIF ch >= ' ' THEN
        IF eolDet THEN
          INC(sourceLine);
          sourceColumn := 1;
          GetPos(source, lastLine.long);
          DEC(lastLine.long);
          eolDet := FALSE;
          lineType := step;
        ELSE
          INC(sourceColumn);
        END;
      END;

      IF ch = breakPointCode THEN
        lineType := breakPoint;
        GetCh;
      ELSIF ch = autoGoCode THEN
        lineType := autoGo;
        GetCh;
      END;
    END; (* if ReadChar done *)
  END GetCh;

  PROCEDURE MarkSym;
  BEGIN
    prevSym := lastSym;
    WITH lastSym DO
      GetPos(source, pos.long);
      DEC(pos.long);
      line := lastLine;
      lineNum := sourceLine;
      colNum := sourceColumn;
    END;
  END MarkSym;

  PROCEDURE Mark(n: CARDINAL);

    PROCEDURE LoadError(error: CARDINAL);
    CONST
      errorOffset = 2000H;
      rCString    = 801DH;
    BEGIN
      IF (error < 400) OR (error > 499) THEN
        errorStringHandle := LoadResource(rCString, error+errorOffset);

        IF (errorStringHandle = NIL) OR
           (ToolError() <> 0) THEN
          errorString := ADR(unknownError.message);
          CardToString(error, unknownError.number, 3);
        ELSE
          errorString := errorStringHandle^;
        END;
      ELSE
        errorString := ADR(unknownError.message);
        CardToString(error, unknownError.number, 3);
      END;
    END LoadError;

  VAR 
    buf:        CHAR;
    col:        CARDINAL;
    curr:       l2w;
    CO:         ConsoleOutDCB;
    stopParms:  StopDCB;
  BEGIN
    IF userHasAborted OR terminalError THEN
      RETURN;
    END;

    terminalError := AllErrFatal OR (n >= 500);
    scanerr := TRUE;
    errorInProc := TRUE;
    INC(errorCount);

    IF NOT objectFileInError AND ShowProgress THEN
      LoadError(n);

      GetPos(source, curr.long);

      IF lastLine.long > lastSym.pos.long THEN
        col := lastSym.colNum + 8;
        lastLine := lastSym.line;
        sourceLine := lastSym.lineNum;
        sourceColumn := lastSym.colNum;
      ELSE
        col := sourceColumn + 8 - VAL(CARDINAL, curr.long - lastSym.pos.long);
      END;

      WriteCard(sourceLine, 5);
      WriteString(' : ');

      IF firstError.position = VAL(LONGINT, 0) THEN
        firstError.position := lastSym.pos.long;
        Assign(errorString^, firstError.string);
      END;

      DisplayLastLine;

      REPEAT
        Write(' ');
        DEC(col);
      UNTIL col < 1;

      WriteString('^  ');
      WriteString(errorString^);
      WriteLn;

      BusyRead(buf);

      IF (buf <> nul) OR PauseOnError THEN
        CO.pCount := 1;
        CO.ch := scEnableMouse;
        ConsoleOut(CO);
        CO.ch := scSetInverse;
        ConsoleOut(CO);
        CO.ch := 'C'; (* hour glass symbol *)
        ConsoleOut(CO);
        CO.ch := scSetNormal;
        ConsoleOut(CO);
        CO.ch := scDisableMouse;;
        ConsoleOut(CO);
        CO.ch := bs;
        ConsoleOut(CO);

        REPEAT
          (*
            This will, cause the compiler to quit if the user hits apple-period
            when the compiler is waiting for the user to "un-pause" an error
            display.
          *)
          stopParms.pCount := 1;
          Stop(stopParms);
          userHasAborted := stopParms.stopFlag;

          IF NOT userHasAborted THEN
            BusyRead(buf);
          END;
        UNTIL userHasAborted OR (buf <> nul);
      END;
    END;
  END Mark;

(*$RangeCheck- to speed up and to avoid range errors for invalid i or j *)
(*$OverflowCheck- *)

  PROCEDURE Diff(i, j: CARDINAL): INTEGER;
    VAR k: CARDINAL; d: INTEGER;
  BEGIN
    IF IdBuf[i] # IdBuf[j] THEN
      RETURN VAL(INTEGER,ORD(IdBuf[i])) - VAL(INTEGER,ORD(IdBuf[j]))
    END;
    k := ORD(IdBuf[i])-1; INC(i); INC(j);
    LOOP
      IF k = 0 THEN RETURN 0
      ELSIF IdBuf[i] # IdBuf[j] THEN
        RETURN VAL(INTEGER,ORD(IdBuf[i])) - VAL(INTEGER,ORD(IdBuf[j]))
      ELSE INC(i); INC(j); DEC(k)
      END
    END
  END Diff;

  PROCEDURE KeepId;
  BEGIN
    id := id1
  END KeepId;

  PROCEDURE String(termCh: CHAR);
  BEGIN id1 := id + 1;
    IF id1 > IdBufLim THEN Mark(503); id1 := 1 END;
    LOOP GetCh;
      IF ch = termCh THEN EXIT END;
      IF ch < " " THEN Mark(45); EXIT END;
      IdBuf[id1] := ch; INC(id1)
    END;
    GetCh;
    IF id1-id <= ORD(MAX(CHAR)) THEN
      IdBuf[id] := CHR(id1-id); (*length*)
    ELSE
      IdBuf[id] := MAX(CHAR);  (*default maximum length*)
      Mark(146);
    END;
    IF IdBuf[id] = 2C THEN
      sym := number; numtyp := 6; intval := ORD(IdBuf[id+1])
    ELSE sym := string;
      IF IdBuf[id] = 1C THEN  (*empty string*)
        IdBuf[id1] := 0C; INC(id1); IdBuf[id] := 2C
      END
    END
  END String;

  PROCEDURE Identifier;
    VAR k, l, m: CARDINAL; cap: BOOLEAN;
  BEGIN
    id1 := id + 1; cap := TRUE;
    IF id1 > IdBufLim THEN Mark(503); id1 := 1 END;
    REPEAT
      IdBuf[id1] := ch;
      cap := cap AND (ch <= "Z");
      INC(id1); GetCh
    UNTIL ((CAP(ch) < "A") OR ("Z" < CAP(ch))) & ((ch < "0") OR ("9" < ch));
    IdBuf[id] := CHR(id1-id); (*Length*)
    IF cap THEN
      k := 0; l := KW;
      REPEAT m := (k + l) DIV 2;
        IF Diff(id, keyTab[m].ind) <= 0 THEN l := m ELSE k := m + 1 END
      UNTIL k >= l;
      IF (k < KW) & (Diff(id, keyTab[k].ind) = 0) THEN sym := keyTab[k].sym
      ELSE sym := ident
      END
    ELSE sym := ident
    END
  END Identifier;

  PROCEDURE Number;
    VAR i, j, l, d, e, n: CARDINAL;
    x, f:   LONGREAL;
    d0, d1: LONGCARD;
    neg:    BOOLEAN;
    lastCh: CHAR;
    dig:    ARRAY [0..maxBuf] OF CHAR;
    longval: RECORD
               CASE :BOOLEAN OF
                 TRUE: lc: LONGCARD;
                 |
                 FALSE: li: LONGINT;
               END;
             END;

    PROCEDURE Ten(e: CARDINAL): LONGREAL;
        VAR k: CARDINAL; u: LONGREAL;
    BEGIN k := 0; u := FLOATD(1);
      WHILE e > 0 DO
        IF ODD(e) THEN u := pow[k] * u END;
        e := e DIV 2; INC(k)
      END;
      RETURN u
    END Ten;

  BEGIN sym := number; i := 0; l := 0;                                (* V2.6 *)
    REPEAT dig[i] := ch; INC(l);                                      (* V2.6 *)
      IF i < maxBuf THEN INC(i) END;                                  (* V2.6 *)
      GetCh                                            (* V2.6 *)
    UNTIL (ch < "0") OR ("9" < ch) & (CAP(ch) < "A") OR ("Z" < CAP(ch));
    IF l > maxBuf THEN Mark(46) END; (* too many digits *)            (* V2.6 *)
    lastCh := ch; j := 0;
    WHILE (j < i) & (dig[j] = "0") DO INC(j) END;
    IF ch = "." THEN GetCh;
      IF ch = "." THEN
        lastCh := 0C; ch := 177C (*ellipsis*)
      END
    END;
    IF lastCh = "." THEN (*decimal point*)
      x := FLOATD(0); l := 0;
      WHILE j < i DO (*read integer part*)
        IF l < maxDig THEN
          IF dig[j] > "9" THEN Mark(40) END;
          x := x * FLOATD(10) + FLOATD(ORD(dig[j]) - 60B);
          INC(l)
        ELSE Mark(41)
        END;
        INC(j)
      END;
      l := 0; f := FLOATD(0);
      WHILE ("0" <= ch) & (ch <= "9") DO (*read fraction*)
        IF l < maxDig THEN
          f := f * FLOATD(10) + FLOATD(ORD(ch) - 60B);
          INC(l)
        END;
        GetCh
      END;
      x := f / Ten(l) + x; e := 0; neg := FALSE; numtyp := 4;
      IF (ch = "E") OR (ch = "L") THEN
        IF ch = "L" THEN numtyp := 5 END;
        GetCh;
        IF ch = "-" THEN
          neg := TRUE; GetCh
        ELSIF ch = "+" THEN GetCh
        END;
        WHILE ("0" <= ch) & (ch <= "9") DO (*read exponent*)
          d := ORD(ch) - 60B;                                         (* V2.6 *)
          IF (MaxCard - d) DIV 10 >= e THEN (* avoid cardinal ov *)   (* V2.6 *)
            e := e*10 + d;                                            (* V2.6 *)
          END;                                                        (* V2.6 *)
          GetCh
        END
      END;
      IF neg THEN
        IF e <= maxExp THEN
          x := x / Ten(e);
        ELSE
          x := FLOATD(0);
        END;
      ELSE
        IF e <= maxExp THEN
          f := Ten(e);
          IF ABS(MAX(LONGREAL)) / f >= x THEN
            x := f*x;
          ELSE
            Mark(41);
          END;
        ELSE
          Mark(41);
        END;
      END;
      IF numtyp = 5 THEN
        lrlval := x;
      ELSE
        IF x <= LONG(ABS(MAX(REAL))) THEN
          realval := SHORT(x);
        ELSE
          Mark(41);
          realval := FLOAT(1);
        END;
      END;
    ELSE (*integer*)
      lastCh := dig[i-1];
      IF lastCh = "B" THEN DEC(i);
        numtyp := 1; intval := 0;
        WHILE j < i DO
          d := ORD(dig[j]) - 60B;
          IF (d < 10B) & ((MaxCard - d) DIV 10B >= intval) THEN       (* V2.6 *)
            intval := 10B * intval + d
          ELSE Mark(29); intval := 0
          END;
          INC(j)
        END
      ELSIF lastCh = "H" THEN DEC(i);
        IF i <= j+4 THEN
          numtyp := 1; intval := 0;
          WHILE j < i DO
            d := ORD(dig[j]) - 60B;
            IF d > 26B THEN Mark(29); d := 0
               ELSIF d > 9 THEN d := d-7
            END;
            intval := 10H * intval + d; INC(j)
          END
        ELSIF i <= j+8 THEN
          numtyp := 2; longval.lc := 0;
          REPEAT d := ORD(dig[j]) - 60B;
            IF d > 26B THEN Mark(29); d := 0
               ELSIF d > 9 THEN d := d-7
            END;
            longval.lc := longval.lc * VAL(LONGCARD, 16) + VAL(LONGCARD, d);
            INC(j);
          UNTIL j = i;
          dblval := longval.li;
        ELSE Mark(29); numtyp := 2; dblval := 0
        END
      ELSIF lastCh = "L" THEN DEC(i);
        numtyp := 2;
        d1 := 0;

        WHILE j < i DO
          d0 := VAL(LONGCARD, ORD(dig[j]) - 60B);

          IF (d0 < 10) AND ((MAX(LONGCARD) - d0) DIV 10 >= d1) THEN
            d1 := 10 * d1 + d0;
          ELSE
            Mark(29);
            d1 := 0;
          END;

          INC(j);
        END;

        dblval := d1;
      ELSIF lastCh = "C" THEN DEC(i);
        numtyp := 3; intval := 0;
        WHILE j < i DO
          d := ORD(dig[j]) - 60B; intval := 10B * intval + d;
          IF (d >= 10B) OR (intval >= 400B) THEN
            Mark(29); intval := 0
          END;
          INC(j)
        END
      ELSE (*decimal?*)
        numtyp := 1;
        intval := 0;
        d1 := 0;

        WHILE j < i DO
          d0 := VAL(LONGCARD, ORD(dig[j]) - 60B);

          IF (d0 < 10) AND ((MAX(LONGCARD) - d0) DIV 10 >= d1) THEN
            d1 := 10 * d1 + d0;
          ELSE
            Mark(29);
            d1 := 0;
          END;

          INC(j);
        END;

        IF d1 > VAL(LONGCARD, MAX(CARDINAL)) THEN
          numtyp := 2;
          dblval := d1;
        ELSE
          intval := d1;
        END;
      END
    END
  END Number;

(* $RangeCheck+*)
(* $OverflowCheck+ *)

  PROCEDURE GetSym;

    PROCEDURE TestOption;
    VAR
      k, l, m:      CARDINAL;
      theDirective: aDirective;
      intersection: aDirectiveSet;
      name:         aSymbol;
    BEGIN
      id1 := id + 1;

      IF id1 > IdBufLim THEN
        Mark(503);
        id1 := 1;
      END;

      REPEAT
        IdBuf[id1] := ch;
        INC(id1);
        GetCh;
      UNTIL (CAP(ch) < "A") OR ("Z" < CAP(ch));

      IdBuf[id] := CHR(id1-id); (*Length*)

      k := 0;
      l := maxDirective;

      REPEAT
        m := (k + l) DIV 2;
        IF Diff(id, dirTab[m].name) <= 0 THEN
          l := m;
        ELSE
          k := m + 1;
        END;
      UNTIL k >= l;

      IF (k < maxDirective) AND (Diff(id, dirTab[k].name) = 0) THEN
        theDirective := dirTab[k].directive;

        CASE theDirective OF
          RangeCheck, OverflowCheck, NILCheck, StackCheck, DataBank:
            IF (theDirective IN aDirectiveSet{StackCheck, DataBank}) AND
               (curLev > 0) THEN
              (*
                Some directives are not valid within the scope of a procedure.
              *)
              Mark(511);
            END;

            (*
              Syntax is:
                Directive"+"|"-"
            *)

            IF ch = "+" THEN
              INCL(Directives, dirTab[k].directive);

              CASE dirTab[k].directive OF
                RangeCheck:
                  rngchk := TRUE;
                |
                OverflowCheck:
                  ovflchk := TRUE;
                |
                StackCheck:
                  stackchk := TRUE;
                |
                NILCheck:
                  nilchk := TRUE;
                |
                DataBank:
                  databank := TRUE;
                ELSE
              END;
            ELSIF ch = "-" THEN
              EXCL(Directives, dirTab[k].directive);

              CASE dirTab[k].directive OF
                RangeCheck:
                  rngchk := FALSE;
                |
                OverflowCheck:
                  ovflchk := FALSE;
                |
                StackCheck:
                  stackchk := FALSE;
                |
                NILCheck:
                  nilchk := FALSE;
                |
                DataBank:
                  databank := FALSE;
                ELSE
              END;
            ELSE
              Mark(304);
            END;
          |
          Segment, Dynamic:
            IF curLev > 0 THEN
              Mark(511);
            END;

            (*
              Syntax is:
                Directive "name"

                where "name" is the segment name to use
            *)
            GetSym;

            IF (sym = string) OR (sym = ident) THEN
              IdToSymbol(id, segmentName);

              INCL(Directives, dirTab[k].directive);

              (*
                Segment and Dynamic are mutually exclusive
              *)
              CASE dirTab[k].directive OF
                Segment:
                  EXCL(Directives, Dynamic);
                |
                Dynamic:
                  EXCL(Directives, Segment);
                ELSE
              END;
            ELSE
              Mark(304);
            END;
          |
          CDA:
            (*
              Syntax is:
                CDA "name" "shutdown"

                where "name" is the name of the CDA placed in the header
                  and "shutdown" is the (case sensitive) name of the shutdown
                      procedure.
            *)

            IF NOT moduleDeclared THEN
              intersection := Directives * aDirectiveSet{NDA, RTL, CDEV, INIT};

              IF intersection = aDirectiveSet{} THEN
                GetSym;

                IF (sym = string) OR
                   (sym = ident)  THEN
                  CDAName := id;
                  KeepId;

                  GetSym;

                  IF (sym = string) OR
                     (sym = ident) THEN
                    CDAShutdown := id;
                    KeepId;
                    INCL(Directives, dirTab[k].directive);
                  ELSE
                    Mark(304);
                  END;
                ELSE
                  Mark(304);
                END;
              ELSE
                Mark(509);
              END;
            ELSE
              Mark(510);
            END;
          |
          NDA:
            (*
              Syntax is:
                NDA "open" "close" "action" period eventmask "menuline"

                where "open" is the name of the open procedure
                      "close" is the name of the close procedure
                      "action" is the name of the action procedure
                      period is a number (0H..FFFFH) representing the action
                        frequency.
                      eventmask is a longcard number (0H..FFFFFFFFH) taking
                        the same format as an event manager eventmask.
                      "menuline" is a string containing the menu item for the
                        apple menu.
            *)

            IF NOT moduleDeclared THEN
              intersection := Directives * aDirectiveSet{CDA, RTL, CDEV, INIT};

              IF intersection = aDirectiveSet{} THEN
                GetSym;

                IF (sym = string) OR
                   (sym = ident)  THEN
                  NDAOpen := id;
                  KeepId;

                  GetSym;

                  IF (sym = string) OR
                     (sym = ident) THEN
                    NDAClose := id;
                    KeepId;

                    GetSym;

                    IF (sym = string) OR
                       (sym = ident) THEN
                      NDAAction := id;
                      KeepId;

                      GetSym;

                      IF (sym = number) OR
                         (numtyp = 1) THEN
                        NDAPeriod := intval;

                        GetSym;

                        IF (sym = number) AND
                           (numtyp = 1) THEN
                          NDAMask := intval;

                          GetSym;

                          IF (sym = string) OR
                             (sym = ident) THEN
                            NDAMenuline := id;
                            KeepId;

                            INCL(Directives, dirTab[k].directive);
                          ELSE
                            Mark(304);
                          END;
                        ELSE
                          Mark(304);
                        END;
                      ELSE
                        Mark(304);
                      END;
                    ELSE
                      Mark(304);
                    END;
                  ELSE
                    Mark(304);
                  END;
                ELSE
                  Mark(304);
                END;
              ELSE
                Mark(509);
              END;
            ELSE
              Mark(510);
            END;
          |
          RTL:
            IF NOT moduleDeclared THEN
              intersection := Directives * aDirectiveSet{CDA, NDA, CDEV, INIT};

              IF intersection = aDirectiveSet{} THEN
                INCL(Directives, dirTab[k].directive);
              ELSE
                Mark(509);
              END;
            ELSE
              Mark(510);
            END;
          |
          CDEV:
            (*
              Syntax is:
                CDEV "entry"

                where "entry" is the name of the procedure called to handle
                each cdev message.
            *)

            IF NOT moduleDeclared THEN
              intersection := Directives * aDirectiveSet{CDA, NDA, RTL, INIT};

              IF intersection = aDirectiveSet{} THEN
                GetSym;

                IF (sym = string) OR
                   (sym = ident)  THEN
                  CDEVName := id;
                  KeepId;

                  INCL(Directives, dirTab[k].directive);
                ELSE
                  Mark(304);
                END;
              ELSE
                Mark(509);
              END;
            ELSE
              Mark(510);
            END;
          |
          INIT:
            IF NOT moduleDeclared THEN
              intersection := Directives * aDirectiveSet{CDA, NDA, RTL, CDEV};

              IF intersection = aDirectiveSet{} THEN
                INCL(Directives, dirTab[k].directive);
              ELSE
                Mark(509);
              END;
            ELSE
              Mark(510);
            END;
          |
          Pascal:
            IF curLev > 0 THEN
              Mark(511);
            END;

            (*
              Syntax is:
                Pascal"+"|"-"
            *)

            IF ch = "+" THEN
              INCL(Directives, dirTab[k].directive);
            ELSIF ch = "-" THEN
              EXCL(Directives, dirTab[k].directive);
            ELSE
              Mark(304);
            END;
          |
          NoImp:
            IF NOT moduleDeclared THEN
              isint := TRUE;
            ELSE
              Mark(510);
            END;
          |
          StackSize:
            IF NOT moduleDeclared THEN
              GetSym;

              IF (sym = number) OR
                  (numtyp = 1) THEN
                INCL(Directives, dirTab[k].directive);
                Stacksize := intval;
              ELSE
                Mark(304);
              END;
            ELSE
              Mark(510);
            END;
          |
          Keep:
            (*
              Syntax is:

                Keep "keepname"

                where "keepname" is to be the name of the object file, less it's
                extension.

                If the shell does not provide a keep name, and this directive is
                not present, then the compiler will not generate an object file.
            *)
            IF NOT moduleDeclared THEN
              GetSym;

              IF (sym = string) OR
                 (sym = ident) THEN
                IF NOT KeepObject THEN
                  INCL(Directives, dirTab[k].directive);
                  KeepObject := TRUE;
                  IdToSymbol(id, name);
                  Assign(name, outFileMask);
                  StringToGSOSOutString(name, OutputFile);
                  keepStyle := kSaveObject;
                END;
              ELSE
                Mark(304);
              END;
            ELSE
              Mark(510);
            END;
          |
          ChainTo:
            (*
              Syntax is:

                Chain "filename"

                where "filename" is the pathname of a file to be compiled after
                the current file.

                If this directive is used, the compiler places the name in the
                sFile parameter of a SetLInfo call before terminating, allowing
                the shell to automatically invoke the appropriate compiler.
            *)
            IF NOT moduleDeclared THEN
              GetSym;

              IF (sym = string) OR
                 (sym = ident) THEN
                INCL(Directives, dirTab[k].directive);
                ChainToName := id;
                KeepId;
              ELSE
                Mark(304);
              END;
            ELSE
              Mark(510);
            END;
        END;
      ELSE
        Mark(305)  (* invalid option *)
      END
    END TestOption;

    PROCEDURE Comment;
    VAR
      commentPos:       l2w;
      commentLastLine:  l2w;
      commentSym:       aSymbolPos;
      commentLine:      CARDINAL;
      commentColumn:    CARDINAL;
    BEGIN
      commentLastLine := lastLine;
      commentSym := lastSym;
      commentLine := sourceLine;
      commentColumn := sourceColumn;
      GetPos(source, commentPos.long);

      GetCh;

      REPEAT
        IF ch = "$" THEN
          GetCh;
          TestOption;
        END;

        WHILE (ch # "*") & (ch > 0C) DO
          IF ch = "(" THEN
            GetCh;

            IF ch = "*" THEN
              Comment;
            END;
          ELSE
            GetCh;
          END;
        END;

        GetCh
      UNTIL (ch = ")") OR (ch = 0C);

      IF ch > 0C THEN
        GetCh;
      ELSE
        lastLine := commentLastLine;
        lastSym := commentSym;
        sourceLine := commentLine;
        sourceColumn := commentColumn;
        SetPos(source, commentPos.long);
        Mark(512);
      END;
    END Comment;

  VAR 
    stopParms:  StopDCB;

  BEGIN
    IF userHasAborted OR terminalError THEN
      (* MW - added so parse loops will terminate gracefully *)
      sym := eof; ch := 0C;
    ELSE
      stopParms.pCount := 1;
      Stop(stopParms);
      userHasAborted := stopParms.stopFlag;

      (*
        A user abortion is considered to be a terminal error.
      *)
      terminalError := userHasAborted;

      LOOP (*ignore control characters*)
        IF ch <= " " THEN
          IF ch = 0C THEN ch := " "; EXIT ELSE GetCh END;
        ELSIF ch > 177C THEN GetCh
        ELSE EXIT
        END
      END;

      MarkSym;

      CASE ch OF   (* " " <= ch <= 177C *)
          " "  : sym := eof; ch := 0C |
          "!"  : sym := null; GetCh |
          '"'  : String('"') |
          "#"  : sym := neq; GetCh  |
          "$"  : sym := null; GetCh |
          "%"  : sym := null; GetCh |
          "&"  : sym := and; GetCh  |
          "'"  : String("'") |
          "("  : GetCh;
                 IF ch = "*" THEN Comment; GetSym
                   ELSE sym := lparen
                 END |
          ")"  : sym := rparen; GetCh|
          "*"  : sym := times; GetCh |
          "+"  : sym := plus; GetCh  |
          ","  : sym := comma; GetCh |
          "-"  : sym := minus; GetCh |
          "."  : GetCh;
                 IF ch = "." THEN GetCh; sym := ellipsis
                   ELSE sym := period
                 END |
          "/"  : sym := slash; GetCh |
          "0".."9": Number |
          ":"  : GetCh;
                 IF ch = "=" THEN GetCh; sym := becomes
                   ELSE sym := colon
                 END |
          ";"  : sym := semicolon; GetCh |
          "<"  : GetCh;
                 IF ch = "=" THEN GetCh; sym := leq
                   ELSIF ch = ">" THEN GetCh; sym := neq
                   ELSE sym := lss
                 END |
          "="  : sym := eql; GetCh   |
          ">"  : GetCh;
                 IF ch = "=" THEN GetCh; sym := geq
                   ELSE sym := gtr
                 END |
          "?"  : sym := null; GetCh  |
          "@"  : sym := null; GetCh  |
          "A".."Z": Identifier       |
          "["  : sym := lbrak; GetCh |
          "\"  : sym := null; GetCh  |
          "]"  : sym := rbrak; GetCh |
          "^"  : sym := arrow; GetCh |
          "_"  : sym := becomes; GetCh  |
          "`"  : sym := null; GetCh  |
          "a".."z": Identifier       |
          "{"  : sym := lbrace; GetCh|
          "|"  : sym := bar; GetCh   |
          "}"  : sym := rbrace; GetCh|
          "~"  : sym := not; GetCh   |
          177C : sym := ellipsis; GetCh
      END;
    END;
  END GetSym;

  PROCEDURE Enter(name: ARRAY OF CHAR): CARDINAL;
    VAR j, l: CARDINAL;
  BEGIN l := HIGH(name) + 1; id1 := id;
    IF id1+l < IdBufLeng THEN
      IdBuf[id] := CHR(l); INC(id);
      FOR j := 0 TO l-1 DO IdBuf[id] := name[j]; INC(id) END
    END;
    RETURN id1
  END Enter;

  PROCEDURE InitScanner(filename: ARRAY OF CHAR);
  BEGIN
    ch := " ";
    scanerr := FALSE;
    lastLine.long := 0;
    WITH lastSym DO
      pos.long := 0;
      line := lastLine;
      lineNum := 0;
      colNum := 0;
    END;
    prevSym := lastSym;
    sourceLine := 1;
    sourceColumn  := 1;
    eolDet := FALSE;
    id0 := id;
    ClearModList;
    isint := FALSE;
    moduleDeclared := FALSE;
    Directives := aDirectiveSet{};
  END InitScanner;

  PROCEDURE CloseScanner;
  BEGIN
    Purge(source);

    CloseResourceFile(myResourceFile);

    ClearModList;
  END CloseScanner;

  PROCEDURE EnterKW(sym: Symbol; name: ARRAY OF CHAR);
  VAR l, L: CARDINAL;
  BEGIN
    keyTab[K].sym := sym;
    keyTab[K].ind := id;
    l := 0; L := HIGH(name);
    IdBuf[id] := CHR(L+1); INC(id);
    WHILE l <= L DO
      IdBuf[id] := name[l];
      INC(id); INC(l)
    END;
    INC(K)
  END EnterKW;

  PROCEDURE EnterDirective(dir: aDirective; name: ARRAY OF CHAR);
  VAR l, L: CARDINAL;
  BEGIN
    dirTab[D].name := id;
    dirTab[D].directive := dir;
    l := 0; L := HIGH(name);
    IdBuf[id] := CHR(L+1); INC(id);
    WHILE l <= L DO
      IdBuf[id] := name[l];
      INC(id); INC(l)
    END;
    INC(D);
  END EnterDirective;

  PROCEDURE AddModule(number: CARDINAL; mod: ObjPtr);
  VAR
    ce, me: pModListEntry;
  BEGIN
    ALLOCATE(me, SIZE(aModListEntry));

    WITH me^ DO
      modnum := number;
      module := mod;
      next := NIL;
    END;

    IF modList = NIL THEN
      modList := me;
    ELSE
      ce := modList;

      WHILE ce^.next <> NIL DO
        ce := ce^.next;
      END;

      ce^.next := me;
    END;
  END AddModule;

  PROCEDURE FindModule(number: CARDINAL; VAR mod: ObjPtr);
  VAR
    ce:     pModListEntry;
    Found:  BOOLEAN;
  BEGIN
    ce := modList;
    Found := FALSE;

    WHILE NOT Found AND
          (ce <> NIL) DO
      IF ce^.modnum = number THEN
        Found := TRUE;
      ELSE
        ce := ce^.next;
      END;
    END;

    IF Found THEN
      mod := ce^.module;
    ELSE
      mod := NIL;
    END;
  END FindModule;

PROCEDURE InitM2SM;
BEGIN
  K := 0;
  D := 0;
  IdBuf[0] := 1C;
  id := 1;
  id0 := 0;
  (* assert maxExp < 512 for actual pow! *)                           (* V2.6 *)
  pow[0] := FLOATD(10)(* 1.0E1 *);
  pow[1] := pow[0] * pow[0]  (* 1.0E2 *);
  pow[2] := pow[1] * pow[1]  (* 1.0E4 *);
  pow[3] := pow[2] * pow[2]  (* 1.0E8 *);
  pow[4] := pow[3] * pow[3]  (* 1.0E16 *);
  pow[5] := pow[4] * pow[4]  (* 1.0E32 *);
  pow[6] := pow[5] * pow[5]  (* 1.0E64 *);
  pow[7] := pow[6] * pow[6]  (* 1.0E128 *);
  pow[8] := pow[7] * pow[7]  (* 1.0E256 *);
  EnterKW(by,"BY");
  EnterKW(do,"DO");
  EnterKW(if,"IF");
  EnterKW(in,"IN");
  EnterKW(of,"OF");
  EnterKW(or,"OR");
  EnterKW(to,"TO");
  EnterKW(and,"AND");
  EnterKW(div,"DIV");
  EnterKW(end,"END");
  EnterKW(for,"FOR");
  EnterKW(mod,"MOD");
  EnterKW(not,"NOT");
  EnterKW(rem,"REM");
  EnterKW(set,"SET");
  EnterKW(var,"VAR");
  EnterKW(case,"CASE");
  EnterKW(else,"ELSE");
  EnterKW(exit,"EXIT");
  EnterKW(from,"FROM");
  EnterKW(gsos,"GSOS");
  EnterKW(loop,"LOOP");
  EnterKW(then,"THEN");
  EnterKW(tool,"TOOL");
  EnterKW(type,"TYPE");
  EnterKW(with,"WITH");
  EnterKW(array,"ARRAY");
  EnterKW(begin,"BEGIN");
  EnterKW(const,"CONST");
  EnterKW(elsif,"ELSIF");
  EnterKW(until,"UNTIL");
  EnterKW(while,"WHILE");
  EnterKW(export,"EXPORT");
  EnterKW(import,"IMPORT");
  EnterKW(module,"MODULE");
  EnterKW(record,"RECORD");
  EnterKW(repeat,"REPEAT");
  EnterKW(return,"RETURN");
  EnterKW(forward,"FORWARD");
  EnterKW(pointer,"POINTER");
  EnterKW(procedure,"PROCEDURE");
  EnterKW(qualified,"QUALIFIED");
  EnterKW(definition,"DEFINITION");
  EnterKW(implementation,"IMPLEMENTATION");

  EnterDirective(CDA, "CDA");
  EnterDirective(NDA, "NDA");
  EnterDirective(RTL, "RTL");
  EnterDirective(CDEV, "CDEV");
  EnterDirective(INIT, "INIT");
  EnterDirective(Keep, "Keep");
  EnterDirective(NoImp, "NoImp");
  EnterDirective(Pascal, "Pascal");
  EnterDirective(ChainTo, "ChainTo");
  EnterDirective(Dynamic, "Dynamic");
  EnterDirective(Segment, "Segment");
  EnterDirective(DataBank, "DataBank");
  EnterDirective(NILCheck, "NILCheck");
  EnterDirective(StackSize, "StackSize");
  EnterDirective(RangeCheck, "RangeCheck");
  EnterDirective(StackCheck, "StackCheck");
  EnterDirective(OverflowCheck, "OverflowCheck");

  modList := NIL;
  isint := FALSE;
  moduleDeclared := FALSE;
  Directives := aDirectiveSet{};
  firstError.position := 0;
  lineType := step;
  errorCount := 0;
END InitM2SM;

PROCEDURE SourceOpened(file: ARRAY OF CHAR): BOOLEAN;
(*
  OPERATION:
    Attempts to open the source file, and returns TRUE if successfull.
*)
BEGIN
  Load(source, file);

  RETURN source.res = done;
END SourceOpened;

PROCEDURE GetSourcePos(VAR p0, p1: CARDINAL);
(*
  OPERATION:
    Returns the current position within the source file.
*)
VAR
  pos: l2w;
BEGIN
  GetPos(source, pos.long);

  p0 := pos.p0;
  p1 := pos.p1;
END GetSourcePos;

PROCEDURE ReportBadKey(expected, got: ObjPtr; filename: ARRAY OF CHAR);
(*       
  OPERATION:
    Reports an error where the key of a Symbol file is not correct.
*)
VAR
  ErrorMessage: ARRAY [0..127] OF CHAR;
  symbol:       aSymbol;
BEGIN
  IF ShowProgress THEN
    IdToSymbol(got^.name, symbol);

    ErrorMessage := 'Version conflict for module "';
    Concat(ErrorMessage, symbol, ErrorMessage);
    Concat(ErrorMessage, '" whilst importing "', ErrorMessage);
    Concat(ErrorMessage, filename, ErrorMessage);
    Concat(ErrorMessage, '"', ErrorMessage);

    WriteString(ErrorMessage);
    WriteLn;
  END;
END ReportBadKey;

CONST
  rVersion      = 8029H;
VAR
  appName:        GSOSOutString;
  getNameParms:   getNameDCB;

BEGIN
(*
  IF GetCurResourceFile() = sysFileID THEN
    appName.inLength := SIZE(GSOSOutString);
    getNameParms.pCount := 1;
    getNameParms.dataBuffer := ADR(appName);
    GetName(getNameParms);

    IF appName.outLength < SIZE(GSOSNameString) THEN
      appName.text[appName.outLength] := nul;
    END;

    Insert("9:", appName.text, 0);
    appName.inString.length := appName.outLength + 2;

    WriteString('resource file name is <');
    WriteString(appName.text);
    WriteString('>');
    WriteLn;
  END;
*)
  appName.inString.text := '16:modula2';
  appName.inString.length := Length(appName.inString.text);

  myResourceFile := OpenResourceFile(AccessSet{aaReadEnabled}, NIL, appName.inString);

  IF ToolError() <> 0 THEN
    WriteString('Unable to open resource fork, error #');
    WriteHex(ToolError(), 4);
    WriteString('H');
    WriteLn;
  END;

  (*
    Load the resource that contains the version number of the compiler that
    last affected the format of the symbol/reference files.
  *)
  versionHandle := hGSVersion(LoadResource(rVersion, 2));

  IF ToolError() <> 0 THEN
    WriteString('Unable to load version resource, error #');
    WriteHex(ToolError(), 4);
    WriteString('H');
    WriteLn;
  END;

  unknownError.message[HIGH(unknownError.message)] := nul;
  unknownError.text := "Unknown error. (#";
  unknownError.text2 := ")  Please contact customer support.";
END M2SM.
