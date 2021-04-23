*
* LongMath
*
* These assembler routines supply a glue to the ORCA SysFloat library routines
* for the ORCA/Modula2 compiler.
*

    CASE    ON
    LONGA   ON
    LONGI   ON
    65816   ON
    OBJCASE ON
    DATACHK OFF

*
* Dummy is a tacky way of forcing all of the routines into the .A file. The ORCA
* assembler will place Dummy in the .ROOT file and then place all other segments
* into the .A file.
*
Dummy     START
          END Dummy


*
* PROCEDURE sqrt(x: LONGREAL): LONGREAL;
*

LongMath_sqrt   START

                TSC
                PEA     0               ;push address of LONGREAL value
                CLC
                ADC     #4
                PHA
                JSL     >~LOADDOUBLE    ;convert to SANE extended value
                JSL     >~SQTE          ;get square root of SANE extended value
                TSC
                PEA     0               ;push address of result
                CLC
                ADC     #22
                PHA
                JSL     >~SAVEDOUBLE   ;convert back to LONGREAL value in result
                LDA     2,S            ;shuffle up return address
                STA     10,S
                LDA     1,S
                STA     9,S
                TSC                     ;get rid of unwanted crap
                CLC
                ADC     #8
                TCS
                RTL

                END     LongMath_sqrt

*
* PROCEDURE exp(x: LONGREAL): LONGREAL;
*

LongMath_exp    START

                TSC
                PEA     0               ;push address of LONGREAL value
                CLC
                ADC     #4
                PHA
                JSL     >~LOADDOUBLE    ;convert to SANE extended value
                JSL     >~EXPE          ;get exponentiation of SANE extd value
                TSC
                PEA     0               ;push address of result
                CLC
                ADC     #22
                PHA
                JSL     >~SAVEDOUBLE   ;convert back to LONGREAL value in result
                LDA     2,S            ;shuffle up return address
                STA     10,S
                LDA     1,S
                STA     9,S
                TSC                     ;get rid of unwanted crap
                CLC
                ADC     #8
                TCS
                RTL

                END     LongMath_exp

*
* PROCEDURE ln(x: LONGREAL): LONGREAL;
*

LongMath_ln     START

                TSC
                PEA     0               ;push address of LONGREAL value
                CLC
                ADC     #4
                PHA
                JSL     >~LOADDOUBLE    ;convert to SANE extended value
                JSL     >~LOGE          ;get natural log of SANE extended value
                TSC
                PEA     0               ;push address of result
                CLC
                ADC     #22
                PHA
                JSL     >~SAVEDOUBLE   ;convert back to LONGREAL value in result
                LDA     2,S            ;shuffle up return address
                STA     10,S
                LDA     1,S
                STA     9,S
                TSC                     ;get rid of unwanted crap
                CLC
                ADC     #8
                TCS
                RTL

                END     LongMath_ln

*
* PROCEDURE sin(x: LONGREAL): LONGREAL;
*

LongMath_sin    START

                TSC
                PEA     0               ;push address of LONGREAL value
                CLC
                ADC     #4
                PHA
                JSL     >~LOADDOUBLE    ;convert to SANE extended value
                JSL     >~SINE          ;get sine of SANE extended value
                TSC
                PEA     0               ;push address of result
                CLC
                ADC     #22
                PHA
                JSL     >~SAVEDOUBLE   ;convert back to LONGREAL value in result
                LDA     2,S            ;shuffle up return address
                STA     10,S
                LDA     1,S
                STA     9,S
                TSC                     ;get rid of unwanted crap
                CLC
                ADC     #8
                TCS
                RTL

                END     LongMath_sin

*
* PROCEDURE cos(x: LONGREAL): LONGREAL;
*

LongMath_cos    START

                TSC
                PEA     0               ;push address of LONGREAL value
                CLC
                ADC     #4
                PHA
                JSL     >~LOADDOUBLE    ;convert to SANE extended value
                JSL     >~COSE          ;get cosine of SANE extended value
                TSC
                PEA     0               ;push address of result
                CLC
                ADC     #22
                PHA
                JSL     >~SAVEDOUBLE   ;convert back to LONGREAL value in result
                LDA     2,S            ;shuffle up return address
                STA     10,S
                LDA     1,S
                STA     9,S
                TSC                     ;get rid of unwanted crap
                CLC
                ADC     #8
                TCS
                RTL

                END     LongMath_cos

*
* PROCEDURE arctan(x: LONGREAL): LONGREAL;
*

LongMath_arctan START

                TSC
                PEA     0               ;push address of LONGREAL value
                CLC
                ADC     #4
                PHA
                JSL     >~LOADDOUBLE    ;convert to SANE extended value
                JSL     >~ATNE          ;get arctan of SANE extended value
                TSC
                PEA     0               ;push address of result
                CLC
                ADC     #22
                PHA
                JSL     >~SAVEDOUBLE   ;convert back to LONGREAL value in result
                LDA     2,S            ;shuffle up return address
                STA     10,S
                LDA     1,S
                STA     9,S
                TSC                     ;get rid of unwanted crap
                CLC
                ADC     #8
                TCS
                RTL

                END     LongMath_arctan

*
* PROCEDURE entier(x: LONGREAL): LONGINT;
*

LongMath_entier START

                TSC
                PEA     0               ;push address of LONGREAL value
                CLC
                ADC     #4
                PHA
                JSL     >~LOADDOUBLE    ;convert to SANE extended value
                JSL     >~ROUND4        ;round value 2wards minus infinity
                STA     12,S
                TXA
                STA     14,S
                LDA     2,S            ;shuffle up return address
                STA     10,S
                LDA     1,S
                STA     9,S
                TSC                     ;get rid of unwanted crap
                CLC
                ADC     #8
                TCS
                RTL

                END LongMath_entier
