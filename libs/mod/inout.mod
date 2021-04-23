 (*$Segment InOut*)
IMPLEMENTATION MODULE InOut;

FROM ASCII IMPORT
  bs, del, esc, nul, cr, vt, si, so;
FROM CharSet IMPORT aCHARSet;
FROM Common IMPORT
  Ref, String255, ConvStrToPStr;
FROM EZTools IMPORT EZGraphicsActive;
FROM FileSystem IMPORT
  File, Lookup, Close, Response, ReadChar, ReadWord, WriteChar, WriteWord;
FROM GSOSInterface IMPORT
  GSOSNameLength, GSOSNameString, GSOSInString, GSOSOutString, 
  GSOSOutStringToString, GSOSOutStringPtr, GSOSOutStringHand;
FROM M2Lib IMPORT
  FormatReal, FormatLongReal, PutCharToError;
FROM MemoryManager IMPORT
  Handle, DisposeHandle;
FROM NumberConversion IMPORT StringToLongInt;
FROM QuickDrawII IMPORT GetForeColor, GetBackColor, SetForeColor, SetBackColor,
  Point, GetPen, MoveTo;
FROM RealConversions IMPORT StringToReal, StringToLongReal;
FROM StandardFile IMPORT
  SFStatus, SFGetFile2, SFPutFile2, TypeListv5, ReplyRecordv5, FilterProcv5;
FROM Strings IMPORT
  Assign, Concat, Length;
FROM SYSTEM IMPORT
  ADR, TSIZE, WORD;

IMPORT Terminal;

CONST
  input = TRUE;

VAR
  saveWriteProc: Terminal.aWriteProc;

PROCEDURE GetName(VAR name:       ARRAY OF CHAR;
                      defext:     ARRAY OF CHAR;
                      inputfile:  BOOLEAN);
VAR
  promptStr:  String255;
  prompt:     Ref;
  origRef:    Ref;
  origName:   GSOSInString;
  replyName:  GSOSOutStringPtr;
  replyPath:  GSOSOutStringPtr;
  result:     GSOSNameString;
  reply:      ReplyRecordv5;
  tname:      GSOSOutStringHand;
BEGIN
  (*
    If standard file is active then use it to determine the file name.
  *)
  IF SFStatus() THEN
    IF inputfile THEN
      ConvStrToPStr('Please select input file', promptStr);
      prompt := Ref(ADR(promptStr));

      reply.nameRefDesc := 3;
      reply.nameRef := Ref(NIL);
      reply.pathRefDesc := 3;
      reply.pathRef := Ref(NIL);

      SFGetFile2(20, 20, 0, prompt, FilterProcv5(NIL), NIL, reply);

      WITH reply DO
        Done := good;

        IF good THEN
          tname := GSOSOutStringHand(pathRef);
          replyPath := tname^;
          GSOSOutStringToString(replyPath^, result);

          IF Length(result) <= HIGH(name) THEN
            Assign(result, name);
          ELSE
            tname := GSOSOutStringHand(nameRef);
            replyName := tname^;
            GSOSOutStringToString(replyName^, result);

            IF Length(result) <= HIGH(name) THEN
              Assign(result, name);
            ELSE
              Done := FALSE;
            END;
          END;
        END;

        IF Handle(pathRef) <> NIL THEN
          DisposeHandle(Handle(pathRef));
        END;

        IF Handle(nameRef) <> NIL THEN
          DisposeHandle(Handle(nameRef));
        END;
      END;
    ELSE
      ConvStrToPStr('Please select output file', promptStr);
      prompt := Ref(ADR(promptStr));

      reply.nameRefDesc := 3;
      reply.nameRef := Ref(NIL);
      reply.pathRefDesc := 3;
      reply.pathRef := Ref(NIL);

      origRef := Ref(ADR(origName));
      origName.length := 0;
      origName.text := '';

      SFPutFile2(20, 20, 0, prompt, 0, origRef, reply);

      WITH reply DO
        Done := good;

        IF good THEN
          tname := GSOSOutStringHand(pathRef);
          replyPath := tname^;
          GSOSOutStringToString(replyPath^, result);

          IF Length(result) <= HIGH(name) THEN
            Assign(result, name);
          ELSE
            tname := GSOSOutStringHand(nameRef);
            replyName := tname^;
            GSOSOutStringToString(replyName^, result);

            IF Length(result) <= HIGH(name) THEN
              Assign(result, name);
            ELSE
              Done := FALSE;
            END;
          END;
        END;

        IF Handle(pathRef) <> NIL THEN
          DisposeHandle(Handle(pathRef));
        END;

        IF Handle(nameRef) <> NIL THEN
          DisposeHandle(Handle(nameRef));
        END;
      END;
    END;
  ELSE
    (*
      Otherwise, use rudimentary text io to get the file name.
    *)
    IF inputfile THEN
      Terminal.WriteString('in> ');
    ELSE
      Terminal.WriteString('out> ');
    END;

    ReadString(name);

    IF name[Length(name)-1] = '.' THEN
      Concat(name, defext, name);
    END;
  END;
END GetName;

PROCEDURE OpenErrorOutput;
(*
  OPERATION:
    Tells the InOut module to send all future output to Error Output, until
    such time as the application closes Error output with a call to 
    CloseErrorOutput.
*)
BEGIN
  Terminal.GetWriteProc(saveWriteProc);
  Terminal.AssignWrite(PutCharToError);
END OpenErrorOutput;

PROCEDURE OpenInput(defext: ARRAY OF CHAR);
(*
  OPERATION:
    Request a file name and open input file "in".

  NOTES:
    o Done := "file was successfully opened".
    o If open, subsequent input is read from this file.
    o If name ends with ".", append extension defext
*)
VAR
  filename: ARRAY [0..GSOSNameLength-1] OF CHAR;
BEGIN
  REPEAT
    GetName(filename, defext, input);

    IF Length(filename) # 0 THEN
      Lookup(in, filename, FALSE);

      IF in.res # done THEN
        Terminal.WriteString(" not found");
      END;
    END;

    Terminal.WriteLn;
  UNTIL (in.res = done) OR (Length(filename) = 0);

  Done := (in.res = done);
END OpenInput;

PROCEDURE OpenOutput(defext: ARRAY OF CHAR);
(*
  OPERATION:
    Request a file name and open output file "out".

  NOTES:
    o Done := "file was successfully opened.
    o If open, subsequent output is written on this file.
    o If name ends with ".", append extension defext.
*)
VAR
  filename: ARRAY [0..GSOSNameLength-1] OF CHAR;
BEGIN
  REPEAT
    GetName(filename, defext, NOT input);

    IF Length(filename) # 0 THEN
      Lookup(out, filename, TRUE);
    END;

    Terminal.WriteLn;
  UNTIL (out.res = done) OR (Length(filename) # 0);

  Done := (out.res = done);
END OpenOutput;

PROCEDURE CloseErrorOutput;
(*
  OPERATION:
    Closes error output direction.  returns output to standard output.
*)
BEGIN
  IF saveWriteProc <> Terminal.aWriteProc(NIL) THEN
    Terminal.AssignWrite(saveWriteProc);
  ELSE
    Terminal.DeassignWrite;
  END;
END CloseErrorOutput;

PROCEDURE CloseInput;
(*
  OPERATION:
    Closes input file; returns input to terminal.
*)
BEGIN
  IF in.open THEN
    Close(in);
  END;
END CloseInput;

PROCEDURE CloseOutput;
(*
  OPERATION:
    Closes output file; returns output to terminal.
*)
BEGIN
  IF out.open THEN
    Close(out);
  END;
END CloseOutput;

PROCEDURE WriteCursor;
VAR
  fore, back: CARDINAL;
  pen:        Point;
BEGIN
  IF NOT EZGraphicsActive THEN
    Terminal.Write(si);
    Terminal.Write(" ");
    Terminal.Write(so);
    Terminal.Write(bs);
  END;
END WriteCursor;

PROCEDURE Read(VAR ch: CHAR);
(*
  OPERATION:
    Reads a character from either the Terminal, or the Open Input File (if any).

  NOTE:
    Done := NOT in.eof
*)
BEGIN
  IF in.open THEN
    ReadChar(in, ch);
    Done := NOT in.eof;
  ELSE
    WriteCursor;
    Terminal.Read(ch);
    Done := ch <> nul;
  END
END Read;

PROCEDURE ReadString(VAR s: ARRAY OF CHAR);
(*
  OPERATION:
    Read string, i.e. sequence of characters not containing blanks nor control
    characters; leading blanks are ignored. Input is terminated by any
    character <= " "; this character is assigned to termCH.
*)
VAR
  ch:   CHAR;
  i:    CARDINAL;

BEGIN
  i := 0;

  IF in.open THEN
    ReadChar(in, ch);

    WHILE (NOT in.eof) AND (ch <= " ") DO
      ReadChar(in, ch);
    END;

    WHILE (NOT in.eof) AND (ch > " ") AND (i <= HIGH(s)) DO
      s[i] := ch;

      IF i < HIGH(s) THEN
        ReadChar(in, ch);
      END;

      INC(i);
    END;
  ELSE
    WriteCursor;

    REPEAT
      Terminal.Read(ch);

      IF ch = bs THEN
        ch := del;
      END;
    UNTIL (ch > " ") OR (ch = esc) OR (ch = cr);

    WHILE ch > " " DO
      IF ch = del THEN
        IF i > 0 THEN
          DEC(i);

          IF NOT EZGraphicsActive THEN
            Terminal.Write(bs);
            Terminal.Write(vt);
          END;

          WriteCursor;
        END;
      ELSE
        IF i <= HIGH(s) THEN
          s[i] := ch;
          INC(i);
          Terminal.Write(ch);
          WriteCursor;
        END;
      END;

      Terminal.Read(ch);

      IF ch = bs THEN
        ch := del;
      END;
    END;
  END;

  IF i < HIGH(s) THEN
    s[i] := nul;
  END;

  termCh := ch;
END ReadString;

PROCEDURE ReadInt(VAR x: INTEGER);
(*
  OPERATION:
    Read string and convert to integer.

    Syntax:
      integer = ["+"|"-"] digit {digit}.

  NOTES:
    o Leading blanks are ignored.
    o Done := "integer was read"
*)
VAR
  i: CARDINAL;
  n: CARDINAL;
  ch: CHAR; neg: BOOLEAN;
  buf: ARRAY [0..9] OF CHAR;

  PROCEDURE next;
  BEGIN
    ch := buf[n];
    INC(n);
  END next;

BEGIN
  ReadString(buf);

  n := 0;

  next;

  IF ch = "-" THEN
    neg := TRUE;
    next;
  ELSE
    neg := FALSE;

    IF ch = "+" THEN
      next;
    END;
  END;

  IF ("0" <= ch) & (ch <= "9") THEN
    i := 0;
    Done := TRUE;

    REPEAT
      IF neg THEN
        IF (i < minint DIV 10) OR
           ((i = minint DIV 10) AND
            ((ORD(ch) - 60B) <= minint MOD 10)) THEN
          i := (10 * i) + (ORD(ch) - 60B);
        ELSE
          Done := FALSE;
        END;
      ELSE
        IF (i < maxint DIV 10) OR
           ((i = maxint DIV 10) AND
            ((ORD(ch) - 60B) <= maxint MOD 10)) THEN
          i := (10 * i) + (ORD(ch) - 60B);
        ELSE
          Done := FALSE;
        END;
      END;

      next;
    UNTIL (ch < "0") OR ("9" < ch) OR NOT Done;

    IF neg THEN
      IF i <> minint THEN
        x := INTEGER(maxcard - i + 1);
      ELSE
        x := INTEGER(minint);
      END;
    ELSE
      x := i;
    END
  ELSE
    Done := FALSE;
  END;
END ReadInt;

PROCEDURE ReadCard(VAR x: CARDINAL);
(*
  OPERATION:
    Read string and convert to cardinal.

    Syntax:
      cardinal = digit {digit}.

  NOTES:
    o Leading blanks are ignored.
    o Done := "cardinal was read"
*)
VAR
  i,n: CARDINAL;
  ch: CHAR;
  buf: ARRAY [0..9] OF CHAR;

  PROCEDURE next;
  BEGIN
    ch := buf[n];
    INC(n);
  END next;

BEGIN
  ReadString(buf);

  n := 0;
  next;

  IF ("0" <= ch) & (ch <= "9") THEN
    i := 0;
    Done := TRUE;

    REPEAT
      IF (i < maxcard DIV 10) OR
         ((i = maxcard DIV 10) AND ((ORD(ch) - 60B) <= maxcard MOD 10)) THEN
        i := 10*i + (ORD(ch) - 60B);
        next;
      ELSE
        Done := FALSE;
      END;
    UNTIL (ch < "0") OR ("9" < ch) OR NOT Done;

    x := i;
  ELSE
    Done := FALSE;
  END;
END ReadCard;

PROCEDURE ReadWrd(VAR w: WORD);
(*
  OPERATION:
    Read a WORD from "in".

  NOTE:
    Done := NOT in.eof
*)
BEGIN
  IF in.open THEN
    ReadWord(in, w);
    Done := NOT in.eof;
  ELSE
    Done := FALSE;
  END
END ReadWrd;

PROCEDURE Write(ch: CHAR);
(*
  OPERATION:
    Write a character to the Terminal or the current output file (if any).
*)
BEGIN
  IF out.open THEN
    WriteChar(out, ch);
  ELSE
    Terminal.Write(ch);
  END;
END Write;

PROCEDURE WriteLn;
(*
  OPERATION:
    Terminate line.
*)
BEGIN
  IF out.open THEN
    WriteChar(out, EOL);
  ELSE
    Terminal.WriteLn;
  END;
END WriteLn;

PROCEDURE WriteString(s: ARRAY OF CHAR);
(*
  OPERATION:
    Write a string to the Terminal, or the current output file (if any).
*)
VAR
  i: CARDINAL;
BEGIN
  IF out.open THEN
    i := 0;

    WHILE (i <= HIGH(s)) AND (s[i] <> 0C) DO
      WriteChar(out, s[i]);
      INC(i);
    END;
  ELSE
    Terminal.WriteString(s)
  END
END WriteString;

PROCEDURE WriteInt(x: INTEGER; n: CARDINAL);
(*
  OPERATION:
    Write integer x with (at least) n characters. If n is greater than the
    number of digits needed, blanks are added preceding the number.
*)
VAR
  i, x0: CARDINAL;
  a: ARRAY [0..6] OF CHAR;
BEGIN
  i := 0;

  IF x = INTEGER(minint) THEN
    x0 := minint;
  ELSE
    x0 := ABS(x);
  END;

  REPEAT
    a[i] := CHR(x0 MOD 10 + 60B);
    x0 := x0 DIV 10;
    INC(i);
  UNTIL x0 = 0;

  IF x < 0 THEN
    a[i] := "-";
    INC(i);
  END;

  WHILE n > i DO
    DEC(n);
    Write(" ");
  END;

  REPEAT
    DEC(i);
    Write(a[i]);
  UNTIL i = 0;
END WriteInt;

PROCEDURE WriteCard(x,n: CARDINAL);
(*
  OPERATION:
    Write cardinal x with (at least) n characters. If n is greater than the
    number of digits needed, blanks are added preceding the number.
*)
VAR
  i: CARDINAL;
  a: ARRAY [0..6] OF CARDINAL;
BEGIN
  i := 0;

  REPEAT
    a[i] := x MOD 10;
    x := x DIV 10;
    INC(i);
  UNTIL x = 0;

  WHILE n > i DO
    DEC(n);
    Write(" ");
  END;

  REPEAT
    DEC(i);
    Write(CHR(a[i] + 60B));
  UNTIL i = 0;
END WriteCard;

PROCEDURE WriteOct(x,n: CARDINAL);
(*
  OPERATION:
    Write cardinal x with (at least) n characters as an octal number. If n is
    greater than the number of digits needed, blanks are added preceding the
    number.
*)
VAR
  i: CARDINAL;
  a: ARRAY [0..6] OF CARDINAL;
BEGIN
  i := 0;

  REPEAT
    a[i] := x MOD 8;
    x := x DIV 8;
    INC(i);
  UNTIL i = 5;

  IF x = 0 THEN
    a[i] := 0;
  ELSE
    a[i] := 1;
  END;

  INC(i);

  WHILE n > i DO
    DEC(n);
    Write(" ");
  END;

  REPEAT
    DEC(i);
    Write(CHR(a[i] + 60B));
  UNTIL i = 0;
END WriteOct;

PROCEDURE WriteHex(x,n: CARDINAL);
(*
  OPERATION:
    Write cardinal in hexadecimal representation with four hex digits. If n>4,
    blanks are appended preceding the number
*)
  PROCEDURE HexDig(d: CARDINAL);
  BEGIN
    d := d MOD 16;

    IF d < 10 THEN
      d := d + 60B;
    ELSE
      d := d + 67B;
    END;

    Write(CHR(d));
  END HexDig;

BEGIN
  WHILE n > 4 DO
    DEC(n);
    Write(" ");
  END;

  HexDig(x DIV 1000H);
  HexDig(x DIV 100H);
  HexDig(x DIV 10H);
  HexDig(x);
END WriteHex;

PROCEDURE WriteWrd(w: WORD);
(*
  OPERATION:
    Writes a WORD to the current output file (if any).

  NOTE:
    Done := "word was written to file 'in' and file 'in' has been opened"
*)
BEGIN
  IF out.open THEN
    WriteWord(out, w);
    Done := (out.res = done);
  ELSE
    Done := FALSE;
  END;
END WriteWrd;

PROCEDURE ReadLongInt(VAR x : LONGINT);
(*
  OPERATION:
    Read string and convert to long integer.

    Syntax:
      integer = ["+"|"-"] digit {digit}

    NOTES:
      o Leading blanks are ignored.
      o Done := "long integer was read"
*)
VAR
  buf:  ARRAY [0..19] OF CHAR;

BEGIN
  ReadString(buf);
  StringToLongInt(buf, x, Done);
END ReadLongInt;

PROCEDURE WriteLongInt(x : LONGINT; n : CARDINAL);
(*
  OPERATION:
    Write long integer x with (at least) n characters on file "out". If n is
    greater than the number of digits needed, blanks are added preceding the
    number.
*)
VAR
  i:            CARDINAL;
  x0:           LONGINT;
  a:            ARRAY [0..10] OF CHAR;
  neg:          BOOLEAN;
  subtractFlag: BOOLEAN;

BEGIN
  i := 0;
  subtractFlag := FALSE;
  neg := x < VAL(LONGINT, 0);

  IF x = MIN(LONGINT) THEN
    INC(x);
    subtractFlag := TRUE;
  END;

  x0 := ABS(x);

  REPEAT
    a[i] := CHR(VAL(CARDINAL, x0 MOD VAL(LONGINT, 10)) + 60B);
    x0 := x0 DIV VAL(LONGINT, 10);
    INC(i);
  UNTIL x0 = VAL(LONGINT, 0);

  IF subtractFlag THEN
    a[0] := CHR(ORD(a[0])+1);
  END;

  IF neg THEN
    a[i] := "-";
    INC(i);
  END;

  WHILE n > i DO
    DEC(n);
    Write(" ");
  END ;

  REPEAT
    DEC(i);
    Write(a[i]);
  UNTIL i = 0;
END WriteLongInt;

PROCEDURE ReadReal(VAR x: REAL);
(*
  OPERATION:
    Read a real number from keyboard according to EBNF-syntax:
      ["-"|"+"]<digit>{<digit>}["."<digit>{<digit>}]
      ["E"["-"|"+"]<digit>{<digit>}]
*)
VAR
  buf:  ARRAY [0..19] OF CHAR;
BEGIN
  ReadString(buf);
  StringToReal(buf, x, Done);
END ReadReal;

PROCEDURE WriteReal(x: REAL; n: CARDINAL);
(*
  OPERATION:
    Write a real number with n characters.
*)
VAR
  str:  ARRAY [0..79] OF CHAR;
BEGIN
  FormatReal(x, n, 2, str);
  WriteString(str);
END WriteReal;

PROCEDURE WriteFixPt(x: REAL; n, k: CARDINAL);
(*
  OPERATION:
    Write x using n characters with k digits after the decimal point. If fewer
    than n characters are needed, leading blanks are inserted.

    If k is zero, then the numer is written in exponential format.
*)
VAR
  str:    ARRAY [0..79] OF CHAR;
  index:  CARDINAL;
  done:   BOOLEAN;
BEGIN
  FormatReal(x, n, k, str);

  index := Length(str);
  done := FALSE;

  WHILE NOT done DO
    IF str[index] IN aCHARSet{'a'..'z'} THEN
      str[index] := CAP(str[index]);
      done := TRUE;
    END;

    IF index > 0 THEN
      DEC(index);
    ELSE
      done := TRUE;
    END;
  END;

  WriteString(str);
END WriteFixPt;

PROCEDURE WriteLongFixPt(x: LONGREAL; n, k: CARDINAL);
(*
  OPERATION:
    Write x using n characters with k digits after the decimal point. If fewer
    than n characters are needed, leading blanks are inserted.

    If k is zero, then the numer is written in exponential format.
*)
VAR
  str:    ARRAY [0..79] OF CHAR;
  index:  CARDINAL;
  done:   BOOLEAN;
BEGIN
  FormatLongReal(x, n, k, str);

  index := Length(str);
  done := FALSE;

  WHILE NOT done DO
    IF str[index] IN aCHARSet{'a'..'z'} THEN
      str[index] := CAP(str[index]);
      done := TRUE;
    END;

    IF index > 0 THEN
      DEC(index);
    ELSE
      done := TRUE;
    END;
  END;

  WriteString(str);
END WriteLongFixPt;

PROCEDURE ReadLongReal(VAR x: LONGREAL);
(*
  OPERATION:
    Read a real number from keyboard according to EBNF-syntax:
      ["-"|"+"]<digit>{<digit>}["."<digit>{<digit>}]
      ["E"["-"|"+"]<digit>{<digit>}]
*)
VAR
  buf:  ARRAY [0..19] OF CHAR;
BEGIN
  ReadString(buf);
  StringToLongReal(buf, x, Done);
END ReadLongReal;

PROCEDURE WriteLongReal(x: LONGREAL; n: CARDINAL);
(*
  OPERATION:
    Write a real number with n characters.
*)
VAR
  str:  ARRAY [0..79] OF CHAR;
BEGIN
  FormatLongReal(x, n, 4, str);
  WriteString(str);
END WriteLongReal;

BEGIN
  in.open := FALSE;
  out.open := FALSE;
  Done := FALSE;
  saveWriteProc := Terminal.aWriteProc(NIL);
END InOut.
 
