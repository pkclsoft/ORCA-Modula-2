*
* MathLib0
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
* PROCEDURE sqrt(x: REAL): REAL;
*

MathLib0_sqrt   START

                TSC
                PEA     0               ;push address of REAL value
                CLC
                ADC     #4
                PHA
                JSL     >~LOADREAL      ;convert to SANE extended value
                JSL     >~SQTE          ;get square root of SANE extended value
                TSC
                PEA     0               ;push address of result
                CLC
                ADC     #18
                PHA
                JSL     >~SAVEREAL      ;convert back to REAL value in result
                LDA     2,S            ;shuffle up return address
                STA     6,S
                LDA     1,S
                STA     5,S
                TSC                     ;get rid of unwanted crap
                CLC
                ADC     #4
                TCS
                RTL

                END     MathLib0_sqrt

*
* PROCEDURE exp(x: REAL): REAL;
*

MathLib0_exp    START

                TSC
                PEA     0               ;push address of REAL value
                CLC
                ADC     #4
                PHA
                JSL     >~LOADREAL      ;convert to SANE extended value
                JSL     >~EXPE          ;get exponentiation of SANE extd value
                TSC
                PEA     0               ;push address of result
                CLC
                ADC     #18
                PHA
                JSL     >~SAVEREAL      ;convert back to REAL value in result
                LDA     2,S            ;shuffle up return address
                STA     6,S
                LDA     1,S
                STA     5,S
                TSC                     ;get rid of unwanted crap
                CLC
                ADC     #4
                TCS
                RTL

                END     MathLib0_exp

*
* PROCEDURE ln(x: REAL): REAL;
*

MathLib0_ln     START

                TSC
                PEA     0               ;push address of REAL value
                CLC
                ADC     #4
                PHA
                JSL     >~LOADREAL      ;convert to SANE extended value
                JSL     >~LOGE          ;get natural log of SANE extended value
                TSC
                PEA     0               ;push address of result
                CLC
                ADC     #18
                PHA
                JSL     >~SAVEREAL      ;convert back to REAL value in result
                LDA     2,S            ;shuffle up return address
                STA     6,S
                LDA     1,S
                STA     5,S
                TSC                     ;get rid of unwanted crap
                CLC
                ADC     #4
                TCS
                RTL

                END     MathLib0_ln

*
* PROCEDURE sin(x: REAL): REAL;
*

MathLib0_sin    START

                TSC
                PEA     0               ;push address of REAL value
                CLC
                ADC     #4
                PHA
                JSL     >~LOADREAL      ;convert to SANE extended value
                JSL     >~SINE          ;get sine of SANE extended value
                TSC
                PEA     0               ;push address of result
                CLC
                ADC     #18
                PHA
                JSL     >~SAVEREAL      ;convert back to REAL value in result
                LDA     2,S            ;shuffle up return address
                STA     6,S
                LDA     1,S
                STA     5,S
                TSC                     ;get rid of unwanted crap
                CLC
                ADC     #4
                TCS
                RTL

                END     MathLib0_sin

*
* PROCEDURE cos(x: REAL): REAL;
*

MathLib0_cos    START

                TSC
                PEA     0               ;push address of REAL value
                CLC
                ADC     #4
                PHA
                JSL     >~LOADREAL      ;convert to SANE extended value
                JSL     >~COSE          ;get cosine of SANE extended value
                TSC
                PEA     0               ;push address of result
                CLC
                ADC     #18
                PHA
                JSL     >~SAVEREAL      ;convert back to REAL value in result
                LDA     2,S            ;shuffle up return address
                STA     6,S
                LDA     1,S
                STA     5,S
                TSC                     ;get rid of unwanted crap
                CLC
                ADC     #4
                TCS
                RTL

                END     MathLib0_cos

*
* PROCEDURE arctan(x: REAL): REAL;
*

MathLib0_arctan START

                TSC
                PEA     0               ;push address of REAL value
                CLC
                ADC     #4
                PHA
                JSL     >~LOADREAL      ;convert to SANE extended value
                JSL     >~ATNE          ;get arctan of SANE extended value
                TSC
                PEA     0               ;push address of result
                CLC
                ADC     #18
                PHA
                JSL     >~SAVEREAL      ;convert back to REAL value in result
                LDA     2,S            ;shuffle up return address
                STA     6,S
                LDA     1,S
                STA     5,S
                TSC                     ;get rid of unwanted crap
                CLC
                ADC     #4
                TCS
                RTL

                END     MathLib0_arctan

*
* PROCEDURE entier(x: REAL): INTEGER;
*

MathLib0_entier START

                TSC
                PEA     0               ;push address of REAL value
                CLC
                ADC     #4
                PHA
                JSL     >~LOADREAL      ;convert to SANE extended value
                JSL     >~ROUND         ;round value 2wards minus infinity
                STA     8,S
                LDA     2,S            ;shuffle up return address
                STA     6,S
                LDA     1,S
                STA     5,S
                TSC                     ;get rid of unwanted crap
                CLC
                ADC     #4
                TCS
                RTL

                END MathLib0_entier
