MODULE TestSet;

FROM InOut IMPORT WriteString, WriteCard, WriteLn, Read, Write;
FROM SYSTEM IMPORT TSIZE;

FROM SetConst IMPORT small, medium, big, asmallset, amediumset, abigset, 
  acharset, testconstsmall, testconstmedium, testconstbig, testconstcharset;

PROCEDURE Wait;
VAR
  ch: CHAR;
BEGIN
  WriteString('Press a key...');
  Read(ch);
  WriteLn;
END Wait;

VAR
  smallset:   asmallset;
  mediumset:  amediumset;
  bigset:     abigset;

  smallset2:  asmallset;
  mediumset2: amediumset;
  bigset2:    abigset;

  index1:     small;
  index2:     medium;
  index3:     big;

  charset:    acharset;
  charset2:   acharset;
  char:       CHAR;
  line:       CARDINAL;

PROCEDURE DisplaySmallset;
BEGIN
  FOR index1 := MIN(small) TO MAX(small) DO
    WriteString('Smallset bit #');
    WriteCard(ORD(index1), 4);
    WriteString(', is ');

    IF index1 IN smallset THEN
      WriteString('On');
    ELSE
      WriteString('Off');
    END;

    WriteLn;
  END;

  Wait;
END DisplaySmallset;

PROCEDURE DisplayMediumset;
BEGIN
  FOR index2 := MIN(medium) TO MAX(medium) DO
    WriteString('Mediumset bit #');
    WriteCard(ORD(index2), 4);
    WriteString(', is ');

    IF index2 IN mediumset THEN
      WriteString('On');
    ELSE
      WriteString('Off');
    END;

    WriteLn;
  END;

  Wait;
END DisplayMediumset;

PROCEDURE DisplayBigset;
BEGIN
  FOR index3 := MIN(big) TO MAX(big) DO
    WriteString('Bigset bit #');
    WriteCard(ORD(index3), 4);
    WriteString(', is ');

    IF index3 IN bigset THEN
      WriteString('On');
    ELSE
      WriteString('Off');
    END;

    WriteLn;
  END;

  Wait;
END DisplayBigset;

PROCEDURE Mess(str: ARRAY OF CHAR);
BEGIN
  WriteString(str);
  WriteLn;
END Mess;

BEGIN
  Mess('CONSTANT OPERATIONS!');
  Mess('');

  smallset := asmallset{};

  Mess('All bits off.');

  DisplaySmallset;

  Mess('Bits 2 and 4 on.');

  smallset := asmallset{three, five};

  DisplaySmallset;

  Mess('Bits 1 thru 5 on.');

  smallset := asmallset{two..six};

  DisplaySmallset;

  Mess('Add bits 0, 2, and 6');

  smallset := smallset + asmallset{one, three, seven};

  DisplaySmallset;

  Mess('Subtract bits 0, 3 and 6, leaving 1, 2, 4, 5 on.');

  smallset := smallset - asmallset{one, four, seven};

  DisplaySmallset;

  Mess('Set all bits on');

  smallset := smallset + asmallset{one, two, three, four, five, six, seven};

  DisplaySmallset;

  Mess('Multiply wih 1, 3 and 5, giving only those bits');

  smallset := smallset * asmallset{two, four, six};

  DisplaySmallset;

  Mess('Divide by all bits, giving all 0, 2, 4, 6 on');

  smallset := smallset / asmallset{one, two, three, four, five, six, seven};

  DisplaySmallset;

  Mess('Set to value of "testconstsmall", {1, 5}');

  smallset := testconstsmall;

  DisplaySmallset;

  Mess('All bit off.');

  mediumset := amediumset{};

  DisplayMediumset;

  Mess('Bits 2, 5, 8 and 9 on');

  mediumset := amediumset{green, black, purple, magenta};

  DisplayMediumset;

  Mess('Bits 1 thru 7 on');

  mediumset := amediumset{blue..orange};

  DisplayMediumset;

  Mess('Add bits 0 and 9');

  mediumset := mediumset + amediumset{red, magenta};

  DisplayMediumset;

  Mess('Subtract bits 1, 3 and 5 leaving 0, 2, 4, 6, 7 and 9 on');

  mediumset := mediumset - amediumset{blue, yellow, black};

  DisplayMediumset;

  Mess('Multiply with bits 0 and 9, leaving only bits 0 and 9 on');

  mediumset := mediumset * amediumset{red, magenta};

  DisplayMediumset;

  Mess('Divide with all bits, leaving all but 0 and 9 on');

  mediumset := mediumset / amediumset{red, blue, green, yellow, white, black, brown, orange, purple, magenta};

  DisplayMediumset;

  Mess('Set to value of "testconstmedium", {1, 3, 4, 5, 9}');

  mediumset := testconstmedium;

  DisplayMediumset;

  Mess('All bits off.');

  bigset := abigset{};

  DisplayBigset;

  Mess('Bits 0, 2, 7, 10, 17 on');

  bigset := abigset{cat, insect, duck, monkey, cow};

  DisplayBigset;

  Mess('Bits 3 thru 6 on');

  bigset := abigset{pig..mouse};

  DisplayBigset;

  Mess('Add bits 8 thru 16');

  bigset := bigset + abigset{horse..lamb};

  DisplayBigset;

  Mess('Subtract bits 4, 9, 17, leaving 3, 5, 6, 8, 10, 11, 12, 13, 14, 15 and 16 on');

  bigset := bigset - abigset{snake, mule, insect};

  DisplayBigset;

  Mess('Multiply with odd bits, leaving 3, 5, 11, 13 and 15 on');

  bigset := bigset * abigset{dog, pig, rat, duck, mule, man, alpaca, sheep, insect};

  DisplayBigset;

  Mess('Divide by bits 0 thru 9, leaving 0, 1, 2, 4, 6, 7, 8, 9, 11, 13 and 15 on');

  bigset := bigset / abigset{cat..mule};

  DisplayBigset;

  Mess('Set to value of "testconstbig", {0..6, 9, 15, 16}');

  bigset := testconstbig;

  DisplayBigset;

(* VARIABLE based operations *)

  Mess('VARIABLE OPERATIONS!');
  Mess('');

  smallset := asmallset{};

  Mess('Even bits on.');

  FOR index1 := MIN(small) TO MAX(small) BY 2 DO
    INCL(smallset, index1);
  END;

  DisplaySmallset;

  Mess('Bits 2 and 4 on.');

  smallset2 := asmallset{three, five};
  smallset := smallset2;

  DisplaySmallset;

  Mess('Bits 1 thru 5 on.');

  smallset2 := asmallset{two..six};
  smallset := smallset2;

  DisplaySmallset;

  Mess('Add bits 0, 2, and 6');

  smallset2 := asmallset{one, three, seven};
  smallset := smallset + smallset2;

  DisplaySmallset;

  Mess('Subtract bits 0, 3 and 6, leaving 1, 2, 4, 5 on.');

  smallset2 := asmallset{one, four, seven};
  smallset := smallset - smallset2;

  DisplaySmallset;

  Mess('Set all bits on');

  smallset2 := asmallset{one, two, three, four, five, six, seven};
  smallset := smallset + smallset2;

  DisplaySmallset;

  Mess('Multiply wih 1, 3 and 5, giving only those bits');

  smallset2 := asmallset{two, four, six};
  smallset := smallset * smallset2;

  DisplaySmallset;

  Mess('Divide by all bits, giving all 0, 2, 4, 6 on');

  smallset2 := asmallset{one, two, three, four, five, six, seven};
  smallset := smallset / smallset2;

  DisplaySmallset;

  Mess('Even bits on.');

  mediumset := amediumset{};

  FOR index2 := MIN(medium) TO MAX(medium) BY 2 DO
    INCL(mediumset, index2);
  END;

  DisplayMediumset;

  Mess('Bits 2, 5, 8 and 9 on');

  mediumset2 := amediumset{green, black, purple, magenta};
  mediumset := mediumset2;

  DisplayMediumset;

  Mess('Bits 1 thru 7 on');

  mediumset2 := amediumset{blue..orange};
  mediumset := mediumset2;

  DisplayMediumset;

  Mess('Add bits 0 and 9');

  mediumset2 := amediumset{red, magenta};
  mediumset := mediumset + mediumset2;

  DisplayMediumset;

  Mess('Subtract bits 1, 3 and 5 leaving 0, 2, 4, 6, 7 and 9 on');

  mediumset2 := amediumset{blue, yellow, black};
  mediumset := mediumset - mediumset2;

  DisplayMediumset;

  Mess('Multiply with bits 0 and 9, leaving only bits 0 and 9 on');

  mediumset2 := amediumset{red, magenta};
  mediumset := mediumset * mediumset2;

  DisplayMediumset;

  Mess('Divide with all bits, leaving all but 0 and 9 on');

  mediumset2 := amediumset{red, blue, green, yellow, white, black, brown, orange, purple, magenta};
  mediumset := mediumset / mediumset2;

  DisplayMediumset;

  Mess('Even bits on.');

  bigset := abigset{};

  FOR index3 := MIN(big) TO MAX(big) BY 2 DO
    INCL(bigset, index3);
  END;

  DisplayBigset;

  Mess('Bits 0, 2, 7, 10, 17 on');

  bigset2 := abigset{cat, insect, duck, monkey, cow};
  bigset := bigset2;

  DisplayBigset;

  Mess('Bits 3 thru 6 on');

  bigset2 := abigset{pig..mouse};
  bigset := bigset2;

  DisplayBigset;

  Mess('Add bits 8 thru 16');

  bigset2 := abigset{horse..lamb};
  bigset := bigset + bigset2;

  DisplayBigset;

  Mess('Subtract bits 4, 9, 17, leaving 3, 5, 6, 8, 10, 11, 12, 13, 14, 15 and 16 on');

  bigset2 := abigset{snake, mule, insect};
  bigset := bigset - bigset2;

  DisplayBigset;

  Mess('Multiply with odd bits, leaving 3, 5, 11, 13 and 15 on');

  bigset2 := abigset{dog, pig, rat, duck, mule, man, alpaca, sheep, insect};
  bigset := bigset * bigset2;

  DisplayBigset;

  Mess('Divide by bits 0 thru 9, leaving 0, 1, 2, 4, 6, 7, 8, 9, 11, 13 and 15 on');

  bigset2 := abigset{cat..mule};
  bigset := bigset / bigset2;

  DisplayBigset;

  charset := testconstcharset;
  charset := charset + acharset{'A'..'Z'};
  charset2 := acharset{'a'..'z'};
  charset := charset + charset2;
  charset := charset + acharset{310C..361C};
  line := 1;

  FOR char := MIN(CHAR) TO MAX(CHAR) DO
    WriteString('ord(');
    WriteCard(ORD(char), 3);

    IF char IN charset THEN
      WriteString('), "');
      Write(char);
      WriteString('" is IN.');
    ELSE
      WriteString(') is OUT.');
    END;

    WriteLn;

    INC(line);

    IF line = 20 THEN
      Wait;
      line := 1;
    END;
  END;
END TestSet.
