(*$Segment Strings*)
IMPLEMENTATION MODULE Strings;

FROM ASCII IMPORT
  nul;
FROM SYSTEM IMPORT
  ADR;

IMPORT MemoryManager;
IMPORT M2Lib;

PROCEDURE Assign(    source:      ARRAY OF CHAR;
                 VAR destination: ARRAY OF CHAR);
(*
  OPERATION:
    Assign the contents of string variable source into string variable
    destination. source is passed as a VAR for efficiency.
*)
BEGIN
  M2Lib.CopyString(HIGH(source)+1, ADR(source),
                   HIGH(destination)+1, ADR(destination));
END Assign;

PROCEDURE Insert(    substr: ARRAY OF CHAR;
                 VAR str:    ARRAY OF CHAR;
                     inx:    CARDINAL);
(*
  OPERATION:
    Insert the string substr into str,starting at str[inx].

  NOTE:
    If inx is equal or greater than Length(str) then substr is appended to end
    of dest.
*)
VAR
  x,y,z : CARDINAL;
BEGIN
  y := Length(substr);

  IF y <> 0 THEN
    x := Length(str);     (* len of destination *)

    IF inx > x THEN
      inx := x;           (* adjust 'inx' to len of 'str' *)
    END;

    IF x+y <= HIGH(str) THEN
      str[x + y] := nul;     (* set EOS at end of sum of strings *)
    END;

    IF y > HIGH(str) + 1 - inx THEN
      y := HIGH(str) + 1 - inx; (* adjust y to remaining len *)
    END;

    IF x > HIGH(str) + 1 - y  THEN
      z := HIGH(str) + 1 - inx - y;
    ELSE
      z := x - inx;
    END;

    x := inx + y;

 (*     x = first char in str to be moved                       *)
 (*     y = number of chars to insert                           *)
 (*     z = number of chars after insertion point to move       *)

    IF (z <> 0) AND (x <> inx) THEN
      (* move the component of 'str' *)
      MemoryManager.BlockMove(ADR(str[inx]), ADR(str[x]), z);
    END;

    (* copy substr into the space created by the previous copy *)
    MemoryManager.BlockMove(ADR(substr), ADR(str[inx]), y);
  END;
END Insert;

PROCEDURE Delete(VAR str: ARRAY OF CHAR;
                     inx: CARDINAL;
                     len: CARDINAL);
(*
  OPERATION:
    Delete len characters from str, starting at str[inx].

  NOTE:
    If inx >= Length(str) then nothing happens. If there are not len
    characters to delete, characters to the end of string are deleted.
*)
VAR
  strLen:   CARDINAL;
  moveLen:  CARDINAL;

BEGIN
  strLen := Length(str);

  IF strLen <> 0 THEN
    IF inx <= strLen THEN
      IF inx + len = strLen THEN
        str[inx] := nul;
      ELSE
        IF ((inx + len) > strLen) THEN
          moveLen := strLen - inx;
        ELSE
          moveLen := strLen - (inx + len);
        END;

        MemoryManager.BlockMove(ADR(str[inx+len]), ADR(str[inx]), moveLen);

        str[inx+moveLen] := nul;
      END;
    END;
  END;
END Delete;

PROCEDURE Pos(substr, str: ARRAY OF CHAR): CARDINAL;
(*
  OPERATION:
    Return the index into str of the first occurrence of the substr.

  NOTE:
    Pos returns a value greater then HIGH(str) if no occurrence of the
    substring is found.
*)
VAR
   i, lsub, ls:CARDINAL;
BEGIN
  lsub := Length(substr);
  ls := Length(str);

  IF  (lsub <> 0)  THEN
    i := 0;

    WHILE i + lsub <= ls DO
      IF M2Lib.CompareBytes(ADR(substr), ADR(str[i]), lsub) = 0 THEN
        RETURN i;
      END;

      INC(i);
    END;
  END;

  RETURN HIGH(str)+1            
END Pos;

PROCEDURE Copy(    str:    ARRAY OF CHAR;
                   inx:    CARDINAL;
                   len:    CARDINAL;
               VAR result: ARRAY OF CHAR);
(*
  OPERATION:
    Copy at most len characters from str into result.

    str     source string,
    inx     starting position in 'str',
    len     maximum number of characters to copy,

    result  copied string
*)
VAR
  x : CARDINAL;
BEGIN
  x:= Length(str);

  IF (inx < x) THEN
    IF  ((inx + len) > x)  THEN
      len:= x-inx;
    END;

    IF  (len > (HIGH(result)+1))  THEN
      len:= HIGH(result)+1;
    END; 

    M2Lib.CopyString(len, ADR(str[inx]), HIGH(result)+1, ADR(result));

    IF len < HIGH(result)+1 THEN
      result[len] := nul;
    END;
  ELSE  (* 'inx' points after end of string *)
    result[0] := nul;
  END;
END Copy; 

PROCEDURE Concat(    s1, s2: ARRAY OF CHAR;
                 VAR result: ARRAY OF CHAR);
(*
  OPERATION:
    Concatenate two strings.

    s1      left string,
    s2      right string,

    result  receives left string followed by right string.
*)
BEGIN
  Assign(s1, result);
  Insert(s2, result, Length(s1))
END Concat;

PROCEDURE Length(str: ARRAY OF CHAR): CARDINAL;
(*
  OPERATION:
    Return the number of characters in a string.
*)
VAR
  len: CARDINAL;
BEGIN
  len := 0;

  WHILE (len <= HIGH(str)) AND
        (str[len] <> nul) DO
    INC(len);
  END;

  RETURN len;
END Length;

BEGIN
  CompareStr := M2Lib.CompareStr;
END Strings.
 
