IMPLEMENTATION MODULE Crc;
(*
  Author  : John Cameron
*)
FROM SYSTEM IMPORT ADR, SETREG, GETREG, INLINE, WORD;
FROM W65C816 IMPORT XBA, Acc, LSR, ASL, IMMEDIATE;

IMPORT FileSystem, FastFileSystem, EZFileSystem;

PROCEDURE CRC(VAR dx: WORD; data: WORD); 
(*
  OPERATION:
    Takes byte in 'data' and updates CRC in 'dx'.
                        16   12   5
    CRC is CRC-CCITT = X  + X  + X + 1
  SOURCE:
    Dr. Dobb's Journal, Feb 1986, p80.
  AUTHOR:
    T.F. Ritter
*) 
VAR
  tx:   BITSET;
  tx2:  BITSET;
  tx3:  BITSET;
BEGIN 
  (*
    Swap the two bytes of dx around

    Done this way for efficiency.
  *)
  SETREG(Acc, dx);
  INLINE(XBA);
  GETREG(Acc, dx);

  (*
    Set operator "/" produces exclusive or
  *)
  tx := VAL(BITSET, dx) / VAL(BITSET, data);

  (*
    Get the low byte of tx
  *)
  tx2 := tx * {0..7};

  (*
    Shift it right four bits
  *)
  SETREG(Acc, tx2);
  INLINE(LSR+IMMEDIATE, 
         LSR+IMMEDIATE, 
         LSR+IMMEDIATE, 
         LSR+IMMEDIATE);
  GETREG(Acc, tx2);

  (*
    Now exclusive or them
  *)
  tx := (tx / tx2);

  (*
    tx2 is now the low byte of tx
  *)
  tx2 := tx * {0..7};

  (*
    Swap the bytes and shift is left four bits, placing the result in tx3.
  *)
  SETREG(Acc, tx2);
  INLINE(XBA, 
         ASL+IMMEDIATE, 
         ASL+IMMEDIATE, 
         ASL+IMMEDIATE, 
         ASL+IMMEDIATE);
  GETREG(Acc, tx3);

  (*
    Shift tx2 left five bytes
  *)
  SETREG(Acc, tx2);
  INLINE(ASL+IMMEDIATE, 
         ASL+IMMEDIATE, 
         ASL+IMMEDIATE, 
         ASL+IMMEDIATE, 
         ASL+IMMEDIATE);
  GETREG(Acc, tx2);

  dx := VAL(WORD, (tx / (tx3 / tx2)));
END CRC; 

PROCEDURE FileCRC(f: FileSystem.File): CARDINAL;
(*
  OPERATION
    Returns CRC of the given file
*)
CONST
  BufLength = 128;
VAR
  Buf:      ARRAY [1..BufLength] OF CHAR;
  j:        CARDINAL;
  nRead:    LONGINT;
  WorkCRC:  CARDINAL;
BEGIN
  FileSystem.SetPos(f, 0, 0);
  WorkCRC := 0;

  REPEAT
    EZFileSystem.ReadNBytes(f, BufLength, ADR(Buf), nRead); 

    FOR j := 1 TO VAL(CARDINAL, nRead) DO 
      CRC(WorkCRC, ORD(Buf[j]));
    END; 
  UNTIL (nRead <> BufLength);

  RETURN WorkCRC;
END FileCRC;

PROCEDURE FastFileCRC(f: FastFileSystem.File): CARDINAL;
(*
  OPERATION
    Returns CRC of the given file using Fastfile under the ORCA shell.  Note
    that you shouldn't use this procedure if your application is not going to
    run under ORCA.
*)
VAR
  WorkCRC:  CARDINAL;
  ch:       CHAR;
BEGIN
  FastFileSystem.SetPos(f, 0);
  WorkCRC := 0;

  REPEAT
    FastFileSystem.ReadChar(f, ch);
    CRC(WorkCRC, ORD(ch));
  UNTIL f.eof;

  RETURN WorkCRC;
END FastFileCRC;

BEGIN
END Crc.
 
