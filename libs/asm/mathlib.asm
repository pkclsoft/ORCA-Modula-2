****************************************************************
*
*  Native Code Libraries for arithmetic
*
*  These libraries are common routines used by the 65816 ORCA
*  native code compilers.
*
*  Copyright 1987, 1990
*  Byte Works, Inc.
*  All rights reserved.
*
*  By Mike Westerfield and Phil Montoya
*  May 1987
*
*  Routines beginning with 'M2' are based on routines from the
*  Byteworks source library, and have been modified by Peter
*  Easdown for use with EZ Modula-2
*
****************************************************************
		
    CASE    ON
    LONGA   ON
    LONGI   ON
    65816   ON
    OBJCASE ON
    DATACHK OFF

    MCOPY asm/mathlib.macros
    COPY 13/ainclude/e16.sane
*
* Dummy is a tacky way of forcing all of the routines into the .A file. The ORCA
* assembler will place Dummy in the .ROOT file and then place all other segments
* into the .A file.
*
Dummy     start
          end

*******************************
*                             *
* Floating Point support code *
*                             *
*******************************

****************************************************************
*
*  ~LoadRealConstant
*
*  Inputs:
*        REAL constant by value 
*
*  Outputs:
*        Extended value on stack.
*
****************************************************************
*
~LoadRealConstant START
         tsc              ;We already have 7 bytes of space (4 = value +
         sec              ;3 = return address), so we only need to allocate
         sbc   #3         ;another 3 bytes for the result.
         tcs
         tay              ;Save the address of the result in Y.
         iny
         lda   5,S        ;Now load the return address. bytes 2-3 in X, and
         tax              ;bytes 1-2 in Acc.
         lda   4,S
         phx              ;Push bytes 2-3 from X.
         phb              ;Push one byte.
         sta   1,S        ;Overwrite bytes 1-2 with Acc value.
         lda   12,S       ;Now copy the value of the REAL onto the stack
         pha              ;below the return address.  Both LDA instructions load
         lda   12,S       ;from 12,S because of the PHA between them.
         pha
         tsc              ;Get the address of the value that we just pushed
         inc   a
         pea   0          ;Push the address of the value's copy
         pha
         pea   0          ;Now push the address of the result, remember Y holds
         phy              ;the bank offset.
         fs2x             ;Do the conversion
         pla              ;Pull the copy of the REAL value off the stack,
         pla              ;leaving the return address and result.
         rtl              ;And return
         end

****************************************************************
*
*  ~LoadDoubleConstant
*
*  Inputs:
*        LONGREAL constant by value
*
*  Outputs:
*        Extended value on stack.
*
****************************************************************
*
~LoadDoubleConstant START

         tsc              ;We already have enough space (8 = value + 3 = return
         tay              ;address), so we don't need to allocate any more.
         iny              ;Save the address of the result in Y.
         iny
*
* Shuffle the return address down by two bytes.
*
         lda   2,S        ;Load the return address. bytes 2-3 in X, and
         tax              ;bytes 1-2 in Acc.
         lda   1,S
         pha              ;Push Acc. This effectively moves bytes 1-2 down by
         txa              ;two bytes.  Next overwrite bytes 2-3 from X.
         sta   2,S
*
* Place a copy of the value on the top of the stack for disposal later. We may
* actually be able to simply supply SANE with the address of the original, but
* we don't know for sure whether SANE will overwrite it's output buffer before
* looking at the original LONGREAL value, so we copy it, and supply SANE with
* the address of the copy.
*
         lda   12,S       ;Now copy the value of the LONGREAL onto the stack
         pha              ;below the return address. All LDA instuctions load
         lda   12,S       ;from 12,S because of the PHA between them
         pha
         lda   12,S
         pha
         lda   12,S
         pha
         tsc              ;Get the address of the value that we just pushed
         inc   a
         pea   0          ;Push the address of the value's copy
         pha
         pea   0          ;Now push the address of the result, remember Y holds
         phy              ;the bank offset.
         fd2x             ;Do the conversion
         pla              ;Pull the copy of the LONGREAL value off the stack,
         pla              ;leaving the return address and result.
         pla
         pla
         rtl              ;And return
         end

****************************************************************
*
*  ~M2CnvLE - convert a longint into an extended SANE real
*
*  Inputs:
*        longint by reference
*
*  Outputs:
*        extended real on stack
*
****************************************************************
*
~M2CnvLE start
         longa on
         longi on
*
* This stuff is tricky, so read the comments carefully before changing it.  The
* goal is to return with the extended result on the stack without using any 
* global storage like the pascal equivalent ~CnvLE.
*
         tsc              ;We already have 7 bytes of space (4 = address +
         sec              ;3 = return address), so we only need to allocate
         sbc   #3         ;another 3 bytes for the result.
         tcs
         tay              ;Save the address of the result in Y.
         iny
         lda   5,S        ;Now load the return address. bytes 2-3 in X, and
         tax              ;bytes 1-2 in Acc.
         lda   4,S
         phx              ;Push bytes 2-3 from X.
         phb              ;Push one byte.
         sta   1,S        ;Overwrite bytes 1-2 with Acc value.
         lda   12,S       ;Now copy the address of the longint onto the stack
         pha              ;below the return address.  Both LDA instructions load
         lda   12,S       ;from 12,S because of the PHA between them.
         pha
         pea   0          ;Now push the address of the result, remember Y holds
         phy              ;the bank offset.
         fl2x             ;Do the conversion
         rtl              ;And return
         end

*******************************
*                             *
* Two byte Integer Math Code  *
*                             *
*******************************

****************************************************************
*
*  ~M2UDiv2 - Two Byte Unsigned Integer Divide
*
*  Inputs:
*        X - denominator
*        A - numerator
*
*  Outputs:
*        A - result
*        X - unsigned remainder
*        V - set for division by zero
*
*  Notes:
*        1) Assumes long A and X on entry.
*
****************************************************************
*
~M2UDiv2 START
NUM1     EQU   3                        numerator
NUM2     EQU   1                        denominator

         CPX   #$0000
         BEQ   ERR
         PHA
         PHX
         TSC                            set up DP
         PHD
         TCD

         LDA   #0                       initialize the remainder
         LDY   #16                      16 bits to go
DV3      ASL   NUM1                     roll up the next number
         ROL   A
         SEC                            subtract the digit
         SBC   NUM2
         BCS   DV4
         ADC   NUM2                     digit is 0
         DEY
         BNE   DV3
         BRA   DV5
DV4      INC   NUM1                     digit is 1
         DEY
         BNE   DV3

DV5      TAX                            save the remainder
         LDA   NUM1                     get the result
         CLV                            clear the error flag
         PLD                            reset DP
         PLY
         PLY
         RTL

ERR      SEP   #%01000000               SEV
         RTL

         END

****************************************************************
*
*  ~M2UMod2 - Two Byte Unsigned Integer Modulo Operation
*
*  Inputs:
*        X - denominator
*        A - numerator
*
*  Outputs:
*        A - result
*        V - set for division by zero
*        Z - set to match result
*
*  Notes:
*        1) Assumes long A and X on entry.
*
****************************************************************
*
~M2UMod2 START

         CPX   #$0000                   error if 2nd arg = 0
         BEQ   ERR
         JSL   ~M2UDiv2                   do the integer divide
         BVS   ERR
         TXA
ERR      RTL

         END

****************************************************************
*
*  ~M2UMul2 - Two Byte Unsigned Integer Multiply
*
*  Inputs:
*        A - multiplicand
*        X - multipier
*
*  Outputs:
*        A - result
*        V - set if an overflow occurred
*
*  Notes:
*        1) Assumes long A and X on entry.
*
****************************************************************
*
~M2UMul2 START
         LONGA ON
         LONGI ON
NUM1     EQU   5
NUM2     EQU   1

         TAY                    ;Save value 1 in Y
         PHD                    ;Set up local space
         TSC
         SEC
         SBC   #6
         TCD
         TCS
         STY   NUM1             ;Save value 1
         STX   NUM2             ;Save value 2
         STZ   NUM2+2

         LDA   #0               ; Initialise result to zero

MUL1     LDX   NUM1             ;Get value 1
         BEQ   MUL3             ;If zero, then we are done
         LSR   NUM1             ;Get right bit of value 1
         BCC   MUL2             ;If clear, then no addition to previous products
         CLC                    ; else add value 2 to partial result
         ADC   NUM2
         LDY   NUM2+2
         BNE   OVFL
MUL2     ASL   NUM2             ;Shift value 2 left for possible add the next
         ROL   NUM2+2           ; time around.
         BRA   MUL1

MUL3     TAY                    ;No overflow occured, free up the local space,
         TDC                    ; clear the "V" bit, and then return.
         CLC
         ADC   #6
         TCS
         PLD
         TYA
         CLV
         RTL

*
* Overflow has occured, return with V set.
*
OVFL     TDC                    ;An overflow occured. Free up the local space,
         CLC                    ; set the "V" bit, and return.
         ADC   #6
         TCS
         PLD
         SEP   #%01000000               SEV
         RTL

         END

*******************************
*                             *
* Four byte Integer Math Code *
*                             *
*******************************

****************************************************************
*
*  ~M2Div4 - FOUR BYTE SIGNED INTEGER DIVIDE
*
*     PROCEDURE M2Div4(numerator, denominator: LONGINT): LONGINT;
*
*  INPUTS:
*        4 byte signed numerator by value
*        4 byte denominator by value
*
*  OUTPUTS:
*        4 byte signed result by value
*        V - SET FOR DIVISION BY ZERO
*
*  NOTES
*        1) USES ~SIGN.
*        2) This procedure differs from ~DIV4 in that it only
*           returns a result, and no remainder.  Also, it does
*           not call SystemError when a divide by zero occurs.
*           For consistency, it returns with the "V" bit set
*           for a divide by zero.
*
****************************************************************
*
~M2Div4  START
SIGN     EQU   1                        SIGN OF ANSWER
NUM1     EQU   20                       ARGUMENTS
NUM2     EQU   16
ANS      EQU   5                        ANSWER
REM      EQU   9                        REMAINDER
RETURN   EQU   13
;
;  INITIALIZE
;
         TSC                            SET UP DP
         SEC
         SBC   #12
         TCS
         PHD
         TCD
         LDA   NUM2                     CHECK FOR DIVISION BY ZERO
         ORA   NUM2+2
         BNE   DV1
         BRL   DV11

DV1      JSL   ~SIGN                    CONVERT TO POSITIVE NUMBERS
         LDA   NUM1+2                   DO 16 BIT DIVIDES SEPARATELY
         ORA   NUM2+2
         BEQ   DV5
;
;  32 BIT DIVIDE
;
         LDY   #32                      32 BITS TO GO
DV3      ASL   ANS                      ROLL UP THE NEXT NUMBER
         ROL   ANS+2
         ROL   ANS+4
         ROL   ANS+6
         SEC                            SUBTRACT FOR THIS DIGIT
         LDA   ANS+4
         SBC   NUM2
         TAX
         LDA   ANS+6
         SBC   NUM2+2
         BCC   DV4                      BRANCH IF MINUS
         STX   ANS+4                    TURN THE BIT ON
         STA   ANS+6
         INC   ANS
DV4      DEY                            NEXT BIT
         BNE   DV3
         BEQ   DV9                      GO DO THE SIGN
;
;  16 BIT DIVIDE
;
DV5      LDA   #0                       INITIALIZE THE REMAINDER
         LDY   #16                      16 BITS TO GO
DV6      ASL   ANS                      ROLL UP THE NEXT NUMBER
         ROL   A
         SEC                            SUBTRACT THE DIGIT
         SBC   NUM2
         BCS   DV7
         ADC   NUM2                     DIGIT IS 0
         DEY
         BNE   DV6
         BEQ   DV8
DV7      INC   ANS                      DIGIT IS 1
         DEY
         BNE   DV6

DV8      STA   ANS+4                    SAVE THE REMAINDER
;
;  SET SIGN
;
DV9      LDA   SIGN                     BRANCH IF POSITIVE
         BEQ   DV10
         SEC                            NEGATE THE RESULT
         LDA   #0
         SBC   ANS
         STA   ANS
         LDA   #0
         SBC   ANS+2
         STA   ANS+2
DV10     LDA   ANS                      MOVE ANSWER, REMAINDER TO STACK
         STA   NUM1
         LDA   ANS+2
         STA   NUM1+2
         CLV
         LDA   RETURN-1
         STA   NUM2
         LDA   RETURN+1
         STA   NUM2+2
         PLD                            FIX STACK, DP
         TSC
         CLC
         ADC   #16
         TCS
         RTL

DV11     LDA   RETURN-1
         STA   NUM2
         LDA   RETURN+1
         STA   NUM2+2
         PLD                            FIX STACK, DP
         TSC
         CLC
         ADC   #16
         TCS
         SEP   #%01000000               DIVISION BY ZERO, EXIT
         RTL

         END

****************************************************************
*
*  ~M2Mod4 - Four Byte Signed Integer Modulus
*
*     PROCEDURE M2Mod4(numerator, denominator: LONGINT): LONGINT;
*
*  Inputs:
*        4 byte signed numerator by value
*        4 byte signed denominator by value
*
*  Outputs:
*        4 byte signed modulus by value
*        V - set for division by zero
*
*  Notes
*        1) Uses ~DIV4, ~SIGN.
*        2) This differs from ~MOD4, in that only a modulus is 
*           returned.
*
****************************************************************
*
~M2Mod4  START
NUM1     EQU   9
NUM2     EQU   5
ANS      EQU   5

         PHB                            set up data bank
         PHK
         PLB
         LDA   NUM2+2,S                 error if 2nd arg <= 0
         BMI   ERR
         STA   N2+2                     save 2nd arg
         LDA   NUM2,S
         STA   N2
         LDA   NUM1+2,S                 save sign of 1st arg
         STA   SIGN
         PLA                            do the integer divide
         STA   RETADDR
         PLA
         STA   RETADDR+2
         JSL   ~DIV4
         LDA   RETADDR+2
         STA   ANS-2,S
         LDA   RETADDR
         STA   ANS-4,S
         BVS   ERR
         LDA   SIGN                     if 1st arg < 0
         BPL   LB1
         LDA   ANS,S                     and result <> 0 then
         ORA   ANS+2,S
         BEQ   LB1
         SEC                              result := result - NUM2
         LDA   N2
         SBC   ANS,S
         STA   ANS,S
         LDA   N2+2
         SBC   ANS+2,S
         STA   ANS+2,S
LB1      PLB
         RTL

ERR      SEP   #%01000000               overflow
         PLB
         RTL

N2       DS    4
SIGN     DS    2
RETADDR  DS    4
         END

****************************************************************
*
*  ~M2UDiv4 - FOUR BYTE UNSIGNED INTEGER DIVIDE
*
*     PROCEDURE M2UDiv4(numerator, denominator: LONGCARD): LONGCARD;
*
*  INPUTS:
*        4 byte unsigned numerator by value
*        4 byte unsigned denominator by value
*
*  OUTPUTS:
*        4 byte unsigned result by value
*        V - Set for division by zero
*
****************************************************************
*
~M2UDiv4 START
NUM1     EQU   18                       ARGUMENTS
NUM2     EQU   14
ANS      EQU   3                        ANSWER
REM      EQU   7                        REMAINDER
RETURN   EQU   11
;
;  INITIALIZE
;
         TSC                            SET UP DP
         SEC
         SBC   #10
         TCS
         PHD
         TCD
         LDA   NUM2                     CHECK FOR DIVISION BY ZERO
         ORA   NUM2+2
         BNE   DV1
         BRL   DV10

DV1      STZ   REM
         STZ   REM+2
         LDA   NUM1
         STA   ANS
         LDA   NUM1+2
         STA   ANS+2

         LDA   NUM1+2                   DO 16 BIT DIVIDES SEPARATELY
         ORA   NUM2+2
         BEQ   DV5
;
;  32 BIT DIVIDE
;
         LDY   #32                      32 BITS TO GO
DV3      ASL   ANS                      ROLL UP THE NEXT NUMBER
         ROL   ANS+2
         ROL   ANS+4
         ROL   ANS+6
         SEC                            SUBTRACT FOR THIS DIGIT
         LDA   ANS+4
         SBC   NUM2
         TAX
         LDA   ANS+6
         SBC   NUM2+2
         BCC   DV4                      BRANCH IF MINUS
         STX   ANS+4                    TURN THE BIT ON
         STA   ANS+6
         INC   ANS
DV4      DEY                            NEXT BIT
         BNE   DV3
         BEQ   DV9
;
;  16 BIT DIVIDE
;
DV5      LDA   #0                       INITIALIZE THE REMAINDER
         LDY   #16                      16 BITS TO GO
DV6      ASL   ANS                      ROLL UP THE NEXT NUMBER
         ROL   A
         SEC                            SUBTRACT THE DIGIT
         SBC   NUM2
         BCS   DV7
         ADC   NUM2                     DIGIT IS 0
         DEY
         BNE   DV6
         BEQ   DV8
DV7      INC   ANS                      DIGIT IS 1
         DEY
         BNE   DV6

DV8      STA   ANS+4                    SAVE THE REMAINDER

DV9      LDA   ANS                      MOVE ANSWER, REMAINDER TO STACK
         STA   NUM1
         LDA   ANS+2
         STA   NUM1+2
         CLV
         LDA   RETURN-1
         STA   NUM2
         LDA   RETURN+1
         STA   NUM2+2
         PLD                            FIX STACK, DP
         TSC
         CLC
         ADC   #14
         TCS
         RTL

DV10     LDA   RETURN-1
         STA   NUM2
         LDA   RETURN+1
         STA   NUM2+2
         PLD                            FIX STACK, DP
         TSC
         CLC
         ADC   #14
         TCS
         SEP   #%01000000               DIVISION BY ZERO, EXIT
         RTL

         END

****************************************************************
*
*  ~M2UMod4 - FOUR BYTE UNSIGNED INTEGER MODULUS
*
*     PROCEDURE M2UMod4(numerator, denominator: LONGCARD): LONGCARD;
*
*  INPUTS:
*        4 byte unsigned numerator by value
*        4 byte unsigned denominator by value
*
*  OUTPUTS:
*        4 byte unsigned modulus by value
*        V - Set for division by zero
*
****************************************************************
*
~M2UMod4  START
NUM1     EQU   18                       ARGUMENTS
NUM2     EQU   14
ANS      EQU   3                        ANSWER
REM      EQU   7                        REMAINDER
RETURN   EQU   11
;
;  INITIALIZE
;
         TSC                            SET UP DP
         SEC
         SBC   #10
         TCS
         PHD
         TCD
         LDA   NUM2                     CHECK FOR DIVISION BY ZERO
         ORA   NUM2+2
         BNE   DV1
         BRL   DV10

DV1      STZ   REM
         STZ   REM+2
         LDA   NUM1
         STA   ANS
         LDA   NUM1+2
         STA   ANS+2

         LDA   NUM1+2                   DO 16 BIT DIVIDES SEPARATELY
         ORA   NUM2+2
         BEQ   DV5
;
;  32 BIT DIVIDE
;
         LDY   #32                      32 BITS TO GO
DV3      ASL   ANS                      ROLL UP THE NEXT NUMBER
         ROL   ANS+2
         ROL   ANS+4
         ROL   ANS+6
         SEC                            SUBTRACT FOR THIS DIGIT
         LDA   ANS+4
         SBC   NUM2
         TAX
         LDA   ANS+6
         SBC   NUM2+2
         BCC   DV4                      BRANCH IF MINUS
         STX   ANS+4                    TURN THE BIT ON
         STA   ANS+6
         INC   ANS
DV4      DEY                            NEXT BIT
         BNE   DV3
         BEQ   DV9
;
;  16 BIT DIVIDE
;
DV5      LDA   #0                       INITIALIZE THE REMAINDER
         LDY   #16                      16 BITS TO GO
DV6      ASL   ANS                      ROLL UP THE NEXT NUMBER
         ROL   A
         SEC                            SUBTRACT THE DIGIT
         SBC   NUM2
         BCS   DV7
         ADC   NUM2                     DIGIT IS 0
         DEY
         BNE   DV6
         BEQ   DV8
DV7      INC   ANS                      DIGIT IS 1
         DEY
         BNE   DV6

DV8      STA   ANS+4                    SAVE THE REMAINDER

DV9      LDA   ANS+4                    MOVE REMAINDER TO STACK
         STA   NUM1
         LDA   ANS+6
         STA   NUM1+2
         CLV
         LDA   RETURN-1
         STA   NUM2
         LDA   RETURN+1
         STA   NUM2+2
         PLD
         TSC
         CLC
         ADC   #14
         TCS
         RTL

DV10     LDA   RETURN-1
         STA   NUM2
         LDA   RETURN+1
         STA   NUM2+2
         PLD
         TSC
         CLC
         ADC   #14
         TCS
         SEP   #%01000000               DIVISION BY ZERO, EXIT
         RTL

         END

****************************************************************
*
*  ~M2UMul4 - Four Byte Unsigned Integer Multiply
*
*     PROCEDURE M2UMul4(x, y: LONGCARD): LONGCARD;
*
*  Inputs:
*        4 byte unsigned multiplier by value
*        4 byte unsigned multiplicand by value
*
*  Outputs:
*        4 byte unsigned result by value
*        V - Set for overflow
*
****************************************************************
*
~M2UMul4 START
NUM1     EQU   18                       arguments
NUM2     EQU   14
ANS      EQU   3                        answer
RETURN   EQU   11
;
;  Initialize the sign and split on precision.
;
         TSC                            set up DP
         SEC
         SBC   #10
         TCS
         PHD
         TCD

         LDA   NUM1
         STA   ANS
         LDA   NUM1+2
         STA   ANS+2
         STZ   ANS+4
         STZ   ANS+6
         
         TAX
         BEQ   ML3
;
;  Do a 32 bit by 32 bit multiply.
;
         LDY   #32                      32 bit multiply
         JSR   ML1
         BRL   ML7

ML1      LDA   ANS                      SYSS1*SYSS1+2+SYSS1+2 -> SYSS1,SYSS1+2
         LSR   A
         BCC   ML2
         CLC                            add multiplicand to the partial product
         LDA   ANS+4
         ADC   NUM2
         STA   ANS+4
         LDA   ANS+6
         ADC   NUM2+2
         STA   ANS+6
ML2      ROR   ANS+6                    shift the interem result
         ROR   ANS+4
         ROR   ANS+2
         ROR   ANS
         DEY                            loop til done
         BNE   ML1
         RTS
;
;  Do and 16 bit by 32 bit multiply.
;
ML3      ORA   NUM2+2                   branch if 16x16 is possible
         BEQ   ML4

         LDY   #16                      set up for 16 bits
         JSR   ML1                      do the multiply
         LDA   ANS+2                    move the answer
         STA   ANS
         LDA   ANS+4
         STA   ANS+2
         LDA   ANS+6
         STA   ANS+4
         STZ   ANS+6
         BRL   ML7
;
;  Do a 16 bit by 16 bit multiply.
;
ML4      LDY   #16                      set the 16 bit counter
         LDX   ANS                      move the low word
         STX   ANS+2
ML5      LSR   ANS+2                    test the bit
         BCC   ML6                      branch if the bit is off
         CLC
         ADC   NUM2
ML6      ROR   A                        shift the answer
         ROR   ANS
         DEY                            loop
         BNE   ML5
         STA   ANS+2                    save the high word
         BRA   ML8                      it's impossible for a 16*16 to overflow
;
;  Check for overflows for all multiply precisions.
;
ML7      LDA   ANS+4                    check for overflow
         ORA   ANS+6
         BEQ   ML8
         MOVE4 RETURN-1,NUM2
         PLD                            fix stack, DP
         TSC
         CLC
         ADC   #14
         TCS
         SEP   #%01000000               overflow
         RTL

ML8      CLV                            normal return
         MOVE4 ANS,NUM1
ML10     MOVE4 RETURN-1,NUM2
         PLD                            fix stack, DP
         TSC
         CLC
         ADC   #14
         TCS
         RTL

         END

****************************************************************
*
*  ~M2FormatReal - convert an extended SANE number to a string
*
*  Inputs:
*        ext - extended format real number
*        fw - field width
*        decDig - fixed precision digit count
*        len - length of result buffer
*        adr - address of result buffer
*
*  Outputs:
*        passtring - string with leading length byte
*
****************************************************************
*
~M2FormatReal start
         longa on
         longi on

         sub   (10:ext,2:fw,2:decDig,2:len,4:adr),0
         phb
         phk
         plb

         lda   len
         inc   a
         cmp   fw
         bpl   oklen
         sta   fw

oklen    anop
         lda   decDig                   if exponential format then
         bne   lb3
         stz   style                      set style to exponential
         sec                              set # sig digits
         lda   fw
         bmi   lb1
         sbc   #7
         bmi   lb1
         cmp   #2
         bge   lb2
lb1      lda   #2
lb2      sta   digits
         bra   lb4                      else
lb3      sta   digits                     set # decimal digits
         lda   #1                         set style to fixed
         sta   style
lb4      anop                           endif

         ph4   #decForm                 convert to decimal record
         ph2   #0
         clc
         tdc
         adc   #ext
         pha
         ph4   #decRec
         fx2dec
         ph4   #decForm                 convert to string
         ph4   #decRec
         ph4   #pasString
         fdec2str
         lda   decDig                   if exponential format then
         bne   lb7
         short I,M
         ldx   pasString                  if format is e+0 then
         lda   pasString-2,X
         cmp   #'e'
         bne   lb5
         lda   pasString,X                  make it e+00
         sta   pasString+1,X
         lda   #'0'
         sta   pasString,X
         inc   pasString
lb5      anop                             endif
         ldx   pasString                  if format is e+00 then
         lda   pasString-3,X
         cmp   #'e'
         bne   lb6
         lda   pasString,X                  make it e+000
         sta   pasString+1,X
         lda   pasString-1,X
         sta   pasString,X
         lda   #'0'
         sta   pasString-1,X
         inc   pasString
lb6      anop                             endif
         long  I,M
lb7      anop                           endif

         lda   #80                      if fw > 80 then fw := 80;
         cmp   fw
         bge   lb8
         sta   fw
lb8      anop
lb9      lda   pasString                while len(string) < fw do
         and   #$00FF
         cmp   fw
         bge   lb11
         short I,M                        insert(' ',string);
         ldx   pasString
lb10     lda   pasString,X
         sta   pasString+1,X
         dex
         bne   lb10
         lda   #' '
         sta   pasString+1
         inc   pasString
         long  I,M
         bra   lb9
lb11     anop

         short  I,M
         ldy    pasString
lb12     lda    pasString,Y
         dey
         sta    [adr],Y
         cpy    #0
         bne    lb12

         ldy    pasString
         cpy    len
         bcc    termit
         bne    lb13

termit   anop
         lda    #0
         sta    [adr],y

lb13     anop
         long  I,M
         plb

         ret

decForm  anop                           decForm record
style    ds    2                        0 -> exponential; 1 -> fixed
digits   ds    2                        significant digits; decimal digits

decRec   anop                           decimal record
sgn      ds    2                        sign
exp      ds    2                        exponent
sig      ds    29                       significant digits

pasString ds   81                       printable string

         end ~M2FormatReal
