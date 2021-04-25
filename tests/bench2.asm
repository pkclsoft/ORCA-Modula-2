  CASE ON
  65816 ON
  ABSADDR ON

~_ROOT START

        USING   M2Lib__Globals

        PEA     $1000
        JSL     >~_BWSTARTUP
        PEA     ~__Globals|-8
        PLB     
        PLB     
        LDA     >~COMMANDLINE+2
        STA     >M2Lib_CommandLine+2
        LDA     >~COMMANDLINE
        STA     >M2Lib_CommandLine
        LDA     >~MINSTACK
        STA     >M2Lib_StackBottom
        CLC     
        ADC     #$1000
        STA     >M2Lib_StackTop
        LDA     >~USER_ID
        AND     #$f0ff
        STA     >M2Lib_MasterID
        PEA     M2Lib_activeProcess|-16
        PEA     M2Lib_activeProcess
        PEA     |$24
        JSL     >Storage_ALLOCATE
        LDA     >M2Lib_activeProcess
        STA     $0
        LDA     >M2Lib_activeProcess+2
        STA     $2
        LDY     #$22 ; char='"'
        LDA     #$01
        STA     [$0],Y
        JSL     >Bench2__Initialisation
        PEA     M2Lib__CloseParms|-16
        PEA     M2Lib__CloseParms
        PEA     $2024
        JSL     $e100b0
        LDA     >~USER_ID
        PHA     
        LDX     #$1102
        JSL     $e10000
        LDA     #$00
        JMP     >~QUIT

        END

Bench2_KeyPressed START

        USING   M2Lib__Globals

        LDA     M2Lib_StackFramePointer
        PHA     
        LDA     M2Lib_Display
        PHA     
        PHD     
        TSC     
        STA     M2Lib_StackFramePointer
        SEC     
        SBC     #$1a
        TCS     
        INC     A
        TCD     
        STA     M2Lib_Display
        LDA     #$01
        STA     $16
        TDC     
        CLC     
        ADC     #$16
        LDX     #$00
        PHX     
        PHA     
        PEA     $153
        JSL     $e100b0
        STA     ~TOOLERROR
        LDA     M2Lib_StackFramePointer
        CLC     
        ADC     #$0a
        STA     $10
        LDA     $18
        PHB     
        PEA     |$0
        PLB     
        PLB     
        STA     ($10)
        PLB     
        BRL     a1
e1      REP     #$20
        LONGA   ON
        PHK     
        PER     e1
        PEA     |$8
        JMP     M2Lib__Crash
a1      LDA     M2Lib_StackFramePointer
        TCS     
        PLD     
        PLA     
        STA     M2Lib_Display
        PLA     
        STA     M2Lib_StackFramePointer
        RTL     

  END

Bench2_P START

        USING   M2Lib__Globals

        LDA     M2Lib_StackFramePointer
        PHA     
        LDA     M2Lib_Display
        PHA     
        PHD     
        TSC     
        STA     M2Lib_StackFramePointer
        SEC     
        SBC     #$16
        TCS     
        INC     A
        TCD     
        STA     M2Lib_Display
        LDA     M2Lib_StackFramePointer
        TCS     
        PLD     
        PLA     
        STA     M2Lib_Display
        PLA     
        STA     M2Lib_StackFramePointer
        RTL     

  END

Bench2_Q START

        USING   M2Lib__Globals

        LDA     M2Lib_StackFramePointer
        PHA     
        LDA     M2Lib_Display
        PHA     
        PHD     
        TSC     
        STA     M2Lib_StackFramePointer
        SEC     
        SBC     #$16
        TCS     
        INC     A
        TCD     
        STA     M2Lib_Display
        LDA     M2Lib_StackFramePointer
        TCS     
        PLD     
        PLA     
        STA     M2Lib_Display
        PLA     
        STA     M2Lib_StackFramePointer
        LDA     $2,S
        TAY     
        LDA     $1,S
        TAX     
        TSC     
        CLC     
        ADC     #$06
        TCS     
        TYA     
        STA     $2,S
        TXA     
        STA     $1,S
        RTL     

  END

Bench2_Test START

        USING   ~__Globals
        USING   M2Lib__Globals

        LDA     M2Lib_StackFramePointer
        PHA     
        LDA     M2Lib_Display
        PHA     
        PHD     
        TSC     
        STA     M2Lib_StackFramePointer
        SEC     
        SBC     #$76 ; char='v'
        TCS     
        INC     A
        TCD     
        STA     M2Lib_Display

        LDA     $7f
        AND     #$ff
        SEP     #$20
        LONGA   OFF
        PHA     
        REP     #$20
        LONGA   ON
        BRL     docase

casea anop
        LDA     #$7d00
        STA     $70
t1      LDA     $70
        DEC     A
        STA     $70
        LDA     $70
;        CMP     #$00
;        BEQ     t2
;        BRA     t1
        BNE     t1
t2      BRL     donecase

*******

caseb anop
        LDA     #$7d00
        STA     $74
t3      LDA     $74
        CMP     #$00
        BCC     t4
        BNE     t5
t4      BRL     t6
t5      LDA     $74
        DEC     A
        STA     $74
        BRA     t3
t6      BRL     donecase

*******

casec anop
        LDA     #$01
        STA     $74
        LDA     $74
        CMP     #$7d00
        BCC     t7
        BEQ     t7
        BRA     t9
t7      LDA     $74
        CMP     #$7d00
        BNE     t8
        BRA     t9
t8      LDA     $74
        INC     A
        STA     $74
        BEQ     t9
        BRA     t7
t9      BRL     donecase

*******

cased anop
        LDA     #$00
        STA     $64
        LDA     #$2710
        STA     $62
t9a     LDA     $62
        DEC     A
        STA     $62
        LDA     $64
        INC     A
        STA     $64
        LDA     #$03
        PHA     
        LDA     $62
        PLX     
        JSL     >~MUL2
        PHA     
        LDA     #$05
        PHA     
        LDA     $64
        PLX     
        JSL     >~MUL2
        PHA     
        PLX     
        PLA     
        JSL     >~DIV2
        BVC     t10
e2      REP     #$20
        LONGA   ON
        PHK     
        PER     e2
        PEA     |$5
        JMP     >M2Lib__Crash
t10     STA     $66
        LDA     $62
        STA     $10
        LDA     $10
        EOR     #$00
        BMI     t11
        LDA     $10
        CMP     #$00
        BRA     t12
t11     LDA     #$00
        CMP     $10
t12     BEQ     t12a
        BRA     t9a
t12a    BRL     donecase

*******

casee anop
        LDA     #$00
        STA     $72
        LDA     #$2710
        STA     $70
t13     LDA     $70
        DEC     A
        STA     $70
        LDA     $72
        INC     A
        STA     $72
        LDA     #$03
        PHA     
        LDA     $70
        PLX     
        JSL     >~M2UMul2
        PHA     
        LDA     #$05
        PHA     
        LDA     $72
        PLX     
        JSL     >~M2UMul2
        PHA     
        PLX     
        PLA     
        JSL     >~M2UDiv2
        BVC     t14
e3      REP     #$20
        LONGA   ON
        PHK     
        PER     e3
        PEA     |$5
        JMP     >M2Lib__Crash
t14     STA     $74
        LDA     $70
        CMP     #$00
        BEQ     t15
        BRA     t13
t15     BRL     donecase

*******

casef anop
        LDA     #$3e8
        STA     $70
        LDX     #$40e8
        LDA     #$f5c3
        STA     $52
        TXA     
        STA     $54
        LDX     #$420b
        LDA     #$3333
        STA     $4e
        TXA     
        STA     $50
t16     LDA     $70
        DEC     A
        STA     $70
        TDC     
        CLC     
        ADC     #$52 ; char='R'
        LDX     #$00
        PHX     
        PHA     
        JSL     >~LOADREAL
        TDC     
        CLC     
        ADC     #$4e ; char='N'
        LDX     #$00
        PHX     
        PHA     
        JSL     >~LOADREAL
        JSL     >~MULE
        TDC     
        CLC     
        ADC     #$52 ; char='R'
        LDX     #$00
        PHX     
        PHA     
        JSL     >~LOADREAL
        TDC     
        CLC     
        ADC     #$4e ; char='N'
        LDX     #$00
        PHX     
        PHA     
        JSL     >~LOADREAL
        JSL     >~ADDE
        TDC     
        CLC     
        ADC     #$08
        LDX     #$00
        PHX     
        PHA     
        JSL     >~SAVEREAL
        TDC     
        CLC     
        ADC     #$08
        LDX     #$00
        PHX     
        PHA     
        JSL     >~LOADREAL
        JSL     >~DIVE
        TDC     
        CLC     
        ADC     #$56 ; char='V'
        LDX     #$00
        PHX     
        PHA     
        JSL     >~SAVEREAL
        LDA     $70
        CMP     #$00
        BEQ     t17
        BRA     t16
t17     BRL     donecase

*******

caseg anop
        LDA     #$3e8
        STA     $70
        PEA     $40e8
        PEA     $f5c3
        JSL     >~LoadRealConstant
        TDC     
        CLC     
        ADC     #$2e ; char='.'
        LDX     #$00
        PHX     
        PHA     
        JSL     >~SAVEDOUBLE
        PEA     $420b
        PEA     $3333
        JSL     >~LoadRealConstant
        TDC     
        CLC     
        ADC     #$26 ; char='&'
        LDX     #$00
        PHX     
        PHA     
        JSL     >~SAVEDOUBLE
t18     LDA     $70
        DEC     A
        STA     $70
        TDC     
        CLC     
        ADC     #$2e ; char='.'
        LDX     #$00
        PHX     
        PHA     
        JSL     >~LOADDOUBLE
        TDC     
        CLC     
        ADC     #$26 ; char='&'
        LDX     #$00
        PHX     
        PHA     
        JSL     >~LOADDOUBLE
        JSL     >~MULE
        TDC     
        CLC     
        ADC     #$2e ; char='.'
        LDX     #$00
        PHX     
        PHA     
        JSL     >~LOADDOUBLE
        TDC     
        CLC     
        ADC     #$26 ; char='&'
        LDX     #$00
        PHX     
        PHA     
        JSL     >~LOADDOUBLE
        JSL     >~ADDE
        TDC     
        CLC     
        ADC     #$08
        LDX     #$00
        PHX     
        PHA     
        JSL     >~SAVEDOUBLE
        TDC     
        CLC     
        ADC     #$08
        LDX     #$00
        PHX     
        PHA     
        JSL     >~LOADDOUBLE
        JSL     >~DIVE
        TDC     
        CLC     
        ADC     #$36 ; char='6'
        LDX     #$00
        PHX     
        PHA     
        JSL     >~SAVEDOUBLE
        LDA     $70
        CMP     #$00
        BEQ     t19
        BRA     t18
t19     BRL     donecase

*******

caseh anop
        BRL     donecase

*******

casei anop
        LDA     #$64 ; char='d'
        STA     $70
        LDX     #$3f33
        LDA     #$3333
        STA     $4a
        TXA     
        STA     $4c
        LDX     #$4000
        LDA     #$00
        STA     $46
        TXA     
        STA     $48
        LDX     #$4120
        LDA     #$00
        STA     $42
        TXA     
        STA     $44
        LDX     #$4190
        LDA     #$00
        STA     $3e
        TXA     
        STA     $40
t20     LDA     $70
        DEC     A
        STA     $70
        LDA     $70
        CMP     #$00
        BEQ     t21
        BRA     t20
t21     BRL     donecase

*******

casej anop
        LDA     #$4e20
        STA     $70
        LDA     #$00
        STA     $74
        LDA     #$49 ; char='I'
        STA     Bench2_B
t22     LDA     $74
        ASL     A
        STA     $10
        LDA     $74
        ASL     A
        STA     $12
        LDX     $12
        LDA     Bench2_B,X
        LDX     $10
        STA     Bench2_A,X
        LDA     $74
        ASL     A
        STA     $10
        LDA     $74
        ASL     A
        STA     $12
        LDX     $12
        LDA     Bench2_A,X
        LDX     $10
        STA     Bench2_B,X
        LDA     $70
        DEC     A
        STA     $70
        LDA     $70
        CMP     #$00
        BEQ     t23
        BRA     t22
t23     BRL     donecase

*******

casek anop
        LDA     #$00
        STA     $74
t24     LDA     #$00
        STA     $72
t25     LDA     #$c8
        PHA     
        LDA     $74
        PLX     
        JSL     >~MUL2
        STA     $10
        LDA     $72
        ASL     A
        CLC     
        ADC     $10
        STA     $10
        LDA     #$c8
        PHA     
        LDA     $72
        PLX     
        JSL     >~MUL2
        STA     $10
        LDA     $74
        ASL     A
        CLC     
        ADC     $10
        STA     $10
        LDX     $10
        LDA     Bench2_M,X
        LDX     $10
        STA     Bench2_M,X
        LDA     $72
        INC     A
        STA     $72
        LDA     $72
        CMP     #$64 ; char='d'
        BEQ     t26
        BRA     t25
t26     LDA     $74
        INC     A
        STA     $74
        LDA     $74
        CMP     #$64 ; char='d'
        BEQ     t27
        BRA     t24
t27     BRL     donecase

*******

casel anop
        LDA     #$4e20
        STA     $70
        LDA     #$00
        STA     $74
        LDA     #$49 ; char='I'
        SEP     #$20
        LONGA   OFF
        STA     Bench2_E
        REP     #$20
        LONGA   ON
t28     LDA     $74
        STA     $10
        LDA     $74
        STA     $12
        LDX     $12
        LDA     Bench2_E,X
        AND     #$ff
        SEP     #$20
        LONGA   OFF
        LDX     $10
        STA     Bench2_D,X
        REP     #$20
        LONGA   ON
        LDA     $74
        STA     $10
        LDA     $74
        STA     $12
        LDX     $12
        LDA     Bench2_D,X
        AND     #$ff
        SEP     #$20
        LONGA   OFF
        LDX     $10
        STA     Bench2_E,X
        REP     #$20
        LONGA   ON
        LDA     $70
        DEC     A
        STA     $70
        LDA     $70
        CMP     #$00
        BEQ     t29
        BRA     t28
t29     BRL     donecase

*******

casem anop
        LDA     #$1f4
        STA     $70
t30     LDA     $70
        DEC     A
        STA     $70
        LDX     #Bench2_B|-16
        LDA     #Bench2_B
        PHX     
        PHA     
        LDX     #Bench2_A|-16
        LDA     #Bench2_A
        PHX     
        PHA     
        PEA     |$0
        PEA     $200
        LDX     #$2b02
        JSL     $e10000
        STA     ~TOOLERROR
        LDX     #Bench2_C|-16
        LDA     #Bench2_C
        PHX     
        PHA     
        LDX     #Bench2_B|-16
        LDA     #Bench2_B
        PHX     
        PHA     
        PEA     |$0
        PEA     $200
        LDX     #$2b02
        JSL     $e10000
        STA     ~TOOLERROR
        LDX     #Bench2_A|-16
        LDA     #Bench2_A
        PHX     
        PHA     
        LDX     #Bench2_C|-16
        LDA     #Bench2_C
        PHX     
        PHA     
        PEA     |$0
        PEA     $200
        LDX     #$2b02
        JSL     $e10000
        STA     ~TOOLERROR
        LDA     $70
        CMP     #$00
        BEQ     t31
        BRA     t30
t31     BRL     donecase

*******

casen anop
        LDA     #$1f4
        STA     $70
t32     LDA     Bench2_head+2
        TAY     
        LDA     Bench2_head
        TYX     
        STA     $22
        TXA     
        STA     $24
t32a    LDY     #$02
        LDA     [$22],Y
        TAX     
        DEY     
        DEY     
        LDA     [$22],Y
        STA     $22
        TXA     
        STA     $24
        LDA     $22
        LDY     $24
        TYX     
        CPX     #$00
        BNE     t33
        CMP     #$00
        BEQ     t34
t33     BRA     t32a
t34     LDA     $70
        DEC     A
        STA     $70
        LDA     $70
        CMP     #$00
        BEQ     t35
        BRA       t32
t35     BRL     donecase

*******

caseo anop
        LDA     #$1388
        STA     $70
t36     LDA     $70
        DEC     A
        STA     $70
        LDX     #Bench2_f|-16
        LDA     #Bench2_f
        PHX     
        PHA     
        TDC     
        CLC     
        ADC     #$74 ; char='t'
        LDX     #$00
        PHX     
        PHA     
        JSL     >FileSystem_ReadWord
        LDA     $70
        CMP     #$00
        BEQ     t37
        BRA     t36
t37     LDX     #Bench2_f|-16
        LDA     #Bench2_f
        PHX     
        PHA     
        LDA     #$00
        PHA     
        LDA     #$00
        PHA     
        JSL     >FileSystem_SetPos
        BRL     donecase

*******

casep anop
        LDA     #$00
        STA     $66
        LDA     #$01
        STA     $64
        LDA     #$4e20
        STA     $74
t38     LDA     $66
        STA     $10
        LDA     $64
        STA     $12
        LDA     $10
        EOR     $12
        BMI     t39
        LDA     $10
        CMP     $12
        BRA     t40
t39     LDA     $12
        CMP     $10
t40     BCC     t41
        BRA     t41
t41     LDA     $66
        STA     $10
        LDA     $64
        STA     $12
        LDA     $10
        EOR     $12
        BMI     t42
        LDA     $10
        CMP     $12
        BRA     t43
t42     LDA     $12
        CMP     $10
t43     BEQ     t44
        BCS     t45
t44     BRA     t45
t45     LDA     $66
        STA     $10
        LDA     $64
        STA     $12
        LDA     $10
        EOR     $12
        BMI     t46
        LDA     $10
        CMP     $12
        BRA     t47
t46     LDA     $12
        CMP     $10
t47     BEQ     t48
        BRA     t48
t48     LDA     $66
        STA     $10
        LDA     $64
        STA     $12
        LDA     $10
        EOR     $12
        BMI     t49
        LDA     $10
        CMP     $12
        BRA     t50
t49     LDA     $12
        CMP     $10
t50     BCS     t51
        BRA     t58
t51     LDA     $66
        STA     $10
        LDA     $64
        STA     $12
        LDA     $10
        EOR     $12
        BMI     t52
        LDA     $10
        CMP     $12
        BRA     t53
t52     LDA     $12
        CMP     $10
t53     BNE     t54
        BRA     t58
t54     LDA     $66
        STA     $10
        LDA     $64
        STA     $12
        LDA     $10
        EOR     $12
        BMI     t55
        LDA     $10
        CMP     $12
        BRA     t56
t55     LDA     $12
        CMP     $10
t56     BEQ     t57
        BCS     t58
t57     BRA     t58
t58     LDA     $66
        STA     $10
        LDA     $64
        STA     $12
        LDA     $10
        EOR     $12
        BMI     t59
        LDA     $10
        CMP     $12
        BRA     t60
t59     LDA     $12
        CMP     $10
t60     BEQ     t61
        BRL     t62
t61     BRA     t70
t62     LDA     $66
        STA     $10
        LDA     $64
        STA     $12
        LDA     $10
        EOR     $12
        BMI     t63
        LDA     $10
        CMP     $12
        BRA     t64
t63     LDA     $12
        CMP     $10
t64     BEQ     t65
        BCS     t66
t65     BRA     t67
t66     BRA     t70
t67     LDA     $66
        STA     $10
        LDA     $64
        STA     $12
        LDA     $10
        EOR     $12
        BMI     t68
        LDA     $10
        CMP     $12
        BRA     t69
t68     LDA     $12
        CMP     $10
t69     BCC     t70
        BRA     t70
t70     LDA     $74
        DEC     A
        STA     $74
        LDA     $74
        CMP     #$00
        BEQ     t71
        BRL     t38
t71     BRL     donecase

*******

caseq anop
        LDX     #$00
        LDA     #$00
        STA     $5e
        TXA     
        STA     $60
        LDX     #$00
        LDA     #$01
        STA     $5a
        TXA     
        STA     $5c
        LDA     #$4e20
        STA     $74
t72     LDA     $5e
        LDY     $60
        TYX     
        STA     $8
        STX     $a
        LDA     $5a
        LDY     $5c
        TYX     
        STA     $c
        STX     $e
        LDA     $a
        EOR     $e
        BPL     t73
        LDA     $e
        CMP     $a
        BRA     t74
t73     LDA     $a
        CMP     $e
        BNE     t74
        LDA     $8
        CMP     $c
t74     BCC     t75
        BRA     t75
t75     LDA     $5e
        LDY     $60
        TYX     
        STA     $8
        STX     $a
        LDA     $5a
        LDY     $5c
        TYX     
        STA     $c
        STX     $e
        LDA     $a
        EOR     $e
        BPL     t76
        LDA     $e
        CMP     $a
        BRA     t77
t76     LDA     $a
        CMP     $e
        BNE     t77
        LDA     $8
        CMP     $c
t77     BEQ     t78
        BCS     t79
t78     BRA     t79
t79     LDA     $5e
        LDY     $60
        TYX     
        STA     $8
        STX     $a
        LDA     $5a
        LDY     $5c
        TYX     
        STA     $c
        STX     $e
        LDA     $a
        EOR     $e
        BPL     t80
        LDA     $e
        CMP     $a
        BRA     t81
t80     LDA     $a
        CMP     $e
        BNE     t81
        LDA     $8
        CMP     $c
t81     BEQ     t82
        BRL     t82
t82     LDA     $5e
        LDY     $60
        TYX     
        STA     $8
        STX     $a
        LDA     $5a
        LDY     $5c
        TYX     
        STA     $c
        STX     $e
        LDA     $a
        EOR     $e
        BPL     t83
        LDA     $e
        CMP     $a
        BRA     t84
t83     LDA     $a
        CMP     $e
        BNE     t84
        LDA     $8
        CMP     $c
t84     BCS     t85
        BRA     t92
t85     LDA     $5e
        LDY     $60
        TYX     
        STA     $8
        STX     $a
        LDA     $5a
        LDY     $5c
        TYX     
        STA     $c
        STX     $e
        LDA     $a
        EOR     $e
        BPL     t86
        LDA     $e
        CMP     $a
        BRA     t87
t86     LDA     $a
        CMP     $e
        BNE     t87
        LDA     $8
        CMP     $c
t87     BNE     t88
        BRA     t92
t88     LDA     $5e
        LDY     $60
        TYX     
        STA     $8
        STX     $a
        LDA     $5a
        LDY     $5c
        TYX     
        STA     $c
        STX     $e
        LDA     $a
        EOR     $e
        BPL     t89
        LDA     $e
        CMP     $a
        BRA     t90
t89     LDA     $a
        CMP     $e
        BNE     t90
        LDA     $8
        CMP     $c
t90     BEQ     t91
        BCS     t92
t91     BRL     t92
t92     LDA     $5e
        LDY     $60
        TYX     
        STA     $8
        STX     $a
        LDA     $5a
        LDY     $5c
        TYX     
        STA     $c
        STX     $e
        LDA     $a
        EOR     $e
        BPL     t93
        LDA     $e
        CMP     $a
        BRA     t94
t93     LDA     $a
        CMP     $e
        BNE     t94
        LDA     $8
        CMP     $c
t94     BEQ     t95
        BRA     t96
t95     BRL     t104
t96     LDA     $5e
        LDY     $60
        TYX     
        STA     $8
        STX     $a
        LDA     $5a
        LDY     $5c
        TYX     
        STA     $c
        STX     $e
        LDA     $a
        EOR     $e
        BPL     t97
        LDA     $e
        CMP     $a
        BRA     t98
t97     LDA     $a
        CMP     $e
        BNE     t98
        LDA     $8
        CMP     $c
t98     BEQ     t99
        BCS     t100
t99     BRL     t101
t100    BRL     t104
t101    LDA     $5e
        LDY     $60
        TYX     
        STA     $8
        STX     $a
        LDA     $5a
        LDY     $5c
        TYX     
        STA     $c
        STX     $e
        LDA     $a
        EOR     $e
        BPL     t102
        LDA     $e
        CMP     $a
        BRA     t103
t102    LDA     $a
        CMP     $e
        BNE     t103
        LDA     $8
        CMP     $c
t103    BCC     t104
        BRA   t104
t104    LDA     $74
        DEC     A
        STA     $74
        LDA     $74
        CMP     #$00
        BEQ     t105
        BRL     t72
t105    BRL     donecase

*******

caser anop
        LDA     #$00
        STA     $72
        LDA     #$01
        STA     $70
        LDA     #$4e20
        STA     $74
        LDA     $70
        STA     $10
        LDA     $72
        CMP     $10
        BCC     t106
        BRA     t106
t106    LDA     $70
        STA     $10
        LDA     $72
        CMP     $10
        BCC     t107
        BNE     t108
t107    BRA     t108
t108    LDA     $70
        STA     $10
        LDA     $72
        CMP     $10
        BEQ     t109
        BRA     t109
t109    LDA     $70
        STA     $10
        LDA     $72
        CMP     $10
        BCS     t110
        BRA     t113
t110    LDA     $70
        STA     $10
        LDA     $72
        CMP     $10
        BNE     t111
        BRA     t113
t111    LDA     $70
        STA     $10
        LDA     $72
        CMP     $10
        BCC     t112
        BNE     t113
t112    BRA     t113
t113    LDA     $70
        STA     $10
        LDA     $72
        CMP     $10
        BEQ     t114
        BRL     t115
t114    BRA     t119
t115    LDA     $70
        STA     $10
        LDA     $72
        CMP     $10
        BCC     t116
        BNE     t117
t116    BRA     t118
t117    BRA     t119
t118    LDA     $70
        STA     $10
        LDA     $72
        CMP     $10
        BCC     t119
        BRA     t119
t119    LDA     $74
        DEC     A
        STA     $74
        LDA     $74
        CMP     #$00
        BEQ     t120
        BRL     t106
t120    BRL     donecase

*******

cases anop
        LDX     #$00
        LDA     #$00
        STA     $6c
        TXA     
        STA     $6e
        LDX     #$00
        LDA     #$01
        STA     $68
        TXA     
        STA     $6a
        LDA     #$4e20
        STA     $74
        LDA     $68
t121    LDY     $6a
        TYX     
        STA     $8
        STX     $a
        LDA     $6c
        LDY     $6e
        TYX     
        CPX     $a
        BNE     t122
        CMP     $8
t122    BCC     t123
        BRA     t123
t123    LDA     $68
        LDY     $6a
        TYX     
        STA     $8
        STX     $a
        LDA     $6c
        LDY     $6e
        TYX     
        CPX     $a
        BNE     t124
        CMP     $8
t124    BCC     t125
        BNE     t126
t125    BRA     t126
t126    LDA     $68
        LDY     $6a
        TYX     
        STA     $8
        STX     $a
        LDA     $6c
        LDY     $6e
        TYX     
        CPX     $a
        BNE     t127
        CMP     $8
t127    BEQ     t128
        BRL     t128
t128    LDA     $68
        LDY     $6a
        TYX     
        STA     $8
        STX     $a
        LDA     $6c
        LDY     $6e
        TYX     
        CPX     $a
        BNE     t129
        CMP     $8
t129    BCS     t130
        BRA     t135
t130    LDA     $68
        LDY     $6a
        TYX     
        STA     $8
        STX     $a
        LDA     $6c
        LDY     $6e
        TYX     
        CPX     $a
        BNE     t131
        CMP     $8
t131    BNE     t132
        BRA     t135
t132    LDA     $68
        LDY     $6a
        TYX     
        STA     $8
        STX     $a
        LDA     $6c
        LDY     $6e
        TYX     
        CPX     $a
        BNE     t133
        CMP     $8
t133    BCC     t134
        BNE     t135
t134    BRL     t135
t135    LDA     $68
        LDY     $6a
        TYX     
        STA     $8
        STX     $a
        LDA     $6c
        LDY     $6e
        TYX     
        CPX     $a
        BNE     t136
        CMP     $8
t136    BEQ     t137
        BRA     t138
t137    BRA     t144
t138    LDA     $68
        LDY     $6a
        TYX     
        STA     $8
        STX     $a
        LDA     $6c
        LDY     $6e
        TYX     
        CPX     $a
        BNE     t139
        CMP     $8
t139    BCC     t140
        BNE     t141
t140    BRL     t142
t141    BRA     t144
t142    LDA     $68
        LDY     $6a
        TYX     
        STA     $8
        STX     $a
        LDA     $6c
        LDY     $6e
        TYX     
        CPX     $a
        BNE     t143
        CMP     $8
t143    BCC     t144
        BRA     t144
t144    LDA     $74
        DEC     A
        STA     $74
        LDA     $74
        CMP     #$00
        BEQ     t145
        BRL     t121
t145    BRL     donecase

*******

caset anop
        LDX     #$00
        LDA     #$00
        STA     $56
        TXA     
        STA     $58
        LDX     #$3f80
        LDA     #$00
        STA     $52
        TXA     
        STA     $54
        LDA     #$1f4
        STA     $74
t145a   TDC     
        CLC     
        ADC     #$56 ; char='V'
        LDX     #$00
        PHX     
        PHA     
        JSL     >~LOADREAL
        TDC     
        CLC     
        ADC     #$52 ; char='R'
        LDX     #$00
        PHX     
        PHA     
        JSL     >~LOADREAL
        JSL     >~GEQE
        BEQ     t146
        BRA     t146
t146    TDC     
        CLC     
        ADC     #$56 ; char='V'
        LDX     #$00
        PHX     
        PHA     
        JSL     >~LOADREAL
        TDC     
        CLC     
        ADC     #$52 ; char='R'
        LDX     #$00
        PHX     
        PHA     
        JSL     >~LOADREAL
        JSL     >~GRTE
        BNE     t147
        BRA     t147
t147    TDC     
        CLC     
        ADC     #$56 ; char='V'
        LDX     #$00
        PHX     
        PHA     
        JSL     >~LOADREAL
        TDC     
        CLC     
        ADC     #$52 ; char='R'
        LDX     #$00
        PHX     
        PHA     
        JSL     >~LOADREAL
        JSL     >~EQUE
        BNE     t148
        BRA     t148
t148    TDC     
        CLC     
        ADC     #$56 ; char='V'
        LDX     #$00
        PHX     
        PHA     
        JSL     >~LOADREAL
        TDC     
        CLC     
        ADC     #$52 ; char='R'
        LDX     #$00
        PHX     
        PHA     
        JSL     >~LOADREAL
        JSL     >~GEQE
        BNE     t149
        BRA     t151
t149    TDC     
        CLC     
        ADC     #$56 ; char='V'
        LDX     #$00
        PHX     
        PHA     
        JSL     >~LOADREAL
        TDC     
        CLC     
        ADC     #$52 ; char='R'
        LDX     #$00
        PHX     
        PHA     
        JSL     >~LOADREAL
        JSL     >~EQUE
        BEQ     t150
        BRA     t151
t150    TDC     
        CLC     
        ADC     #$56 ; char='V'
        LDX     #$00
        PHX     
        PHA     
        JSL     >~LOADREAL
        TDC     
        CLC     
        ADC     #$52 ; char='R'
        LDX     #$00
        PHX     
        PHA     
        JSL     >~LOADREAL
        JSL     >~GRTE
        BNE     t151
        BRA     t151
t151    TDC     
        CLC     
        ADC     #$56 ; char='V'
        LDX     #$00
        PHX     
        PHA     
        JSL     >~LOADREAL
        TDC     
        CLC     
        ADC     #$52 ; char='R'
        LDX     #$00
        PHX     
        PHA     
        JSL     >~LOADREAL
        JSL     >~EQUE
        BNE     t152
        BRA     t153
t152    BRA     t156
t153    TDC     
        CLC     
        ADC     #$56 ; char='V'
        LDX     #$00
        PHX     
        PHA     
        JSL     >~LOADREAL
        TDC     
        CLC     
        ADC     #$52 ; char='R'
        LDX     #$00
        PHX     
        PHA     
        JSL     >~LOADREAL
        JSL     >~GRTE
        BNE     t154
        BRA     t155
t154    BRA     t156
t155    TDC     
        CLC     
        ADC     #$56 ; char='V'
        LDX     #$00
        PHX     
        PHA     
        JSL     >~LOADREAL
        TDC     
        CLC     
        ADC     #$52 ; char='R'
        LDX     #$00
        PHX     
        PHA     
        JSL     >~LOADREAL
        JSL     >~GEQE
        BEQ     t156
        BRL     t156
t156    LDA     $74
        DEC     A
        STA     $74
        LDA     $74
        CMP     #$00
        BEQ     t157
        BRL     t145a
t157    BRL     donecase

*******

caseu anop
        LDA     #$4e20
        STA     $70
t158    JSL     >Bench2_P
        LDA     $70
        DEC     A
        STA     $70
        LDA     $70
        CMP     #$00
        BEQ     t159
        BRA     t158
t159    BRL     donecase

*******

casev anop
        LDA     #$4e20
        STA     $70
t160    LDA     $74
        PHA     
        LDA     $72
        PHA     
        LDA     $70
        PHA     
        JSL     >Bench2_Q
        LDA     $70
        DEC     A
        STA     $70
        LDA     $70
        CMP     #$00
        BEQ     t161
        BRA     t160
t161    BRL     donecase

*******

casew anop
        LDA     #$3e8
        STA     $72
t162    LDA     #$00
        STA     $20
        LDA     #$00
        STA     $74
t162a   LDA     #$01
        STA     $10
        LDA     $74
        BEQ     t164
        TAY     
t163    ASL     $10
        DEY     
        BNE     t163
t164    LDA     $10
        STA     $10
        LDA     $20
        ORA     $10
        STA     $20
        LDA     $74
        INC     A
        STA     $74
        LDA     $74
        CMP     #$10
        BEQ     t165
        BRA     t162a
t165    LDA     #$00
        STA     $74
t165a   LDA     #$01
        STA     $10
        LDA     $74
        BEQ     t167
        TAY     
t166    ASL     $10
        DEY     
        BNE     t166
t167    LDA     $10
        EOR     #$ffff
        STA     $10
        LDA     $20
        AND     $10
        STA     $20
        LDA     $74
        INC     A
        STA     $74
        LDA     $74
        CMP     #$10
        BEQ     t168
        BRA     t165a
t168    LDA     $20
        STA     $10
        LDA     $1e
        ORA     $10
        STA     $20
        LDA     $20
        EOR     #$ffff
        STA     $10
        LDA     $1e
        AND     $10
        STA     $20
        LDA     $20
        STA     $10
        LDA     $1e
        AND     $10
        STA     $20
        LDA     $72
        DEC     A
        STA     $72
        LDA     $72
        CMP     #$00
        BEQ     t169
        BRL     t162
t169    BRL     donecase

*******

casex anop
        LDA     #$3e8
        STA     $72
t170    LDX     #$00
        LDA     #$00
        STA     $1a
        TXA     
        STA     $1c
        LDA     #$00
        STA     $74
t171    LDX     #$00
        LDA     #$01
        STA     $8
        STX     $a
        LDA     $74
        BEQ     t173
        TAY     
t172    ASL     $a
        ROL     $8
        DEY     
        BNE     t172
t173    LDA     $8
        LDX     $a
        STA     $8
        STX     $a
        LDA     $1a
        LDY     $1c
        TYX     
        ORA     $8
        TAY     
        TXA     
        ORA     $a
        TAX     
        TYA     
        STA     $1a
        TXA     
        STA     $1c
        LDA     $74
        INC     A
        STA     $74
        LDA     $74
        CMP     #$20 ; char=' '
        BEQ     t174
        BRA     t171
t174    LDA     #$00
        STA     $74
        LDX     #$00
t174a   LDA     #$01
        STA     $8
        STX     $a
        LDA     $74
        BEQ     t176
        TAY     
t175    ASL     $a
        ROL     $8
        DEY     
        BNE     t175
t176    LDA     $8
        LDX     $a
        EOR     #$ffff
        TAY     
        TXA     
        EOR     #$ffff
        TAX     
        TYA     
        STA     $8
        STX     $a
        LDA     $1a
        LDY     $1c
        TYX     
        AND     $8
        TAY     
        TXA     
        AND     $a
        TAX     
        TYA     
        STA     $1a
        TXA     
        STA     $1c
        LDA     $74
        INC     A
        STA     $74
        LDA     $74
        CMP     #$20 ; char=' '
        BEQ     t177
        BRA     t174a
t177    LDA     $1a
        LDY     $1c
        TYX     
        STA     $8
        STX     $a
        LDA     $16
        LDY     $18
        TYX     
        ORA     $8
        TAY     
        TXA     
        ORA     $a
        TAX     
        TYA     
        STA     $1a
        TXA     
        STA     $1c
        LDA     $1a
        LDY     $1c
        TYX     
        EOR     #$ffff
        TAY     
        TXA     
        EOR     #$ffff
        TAX     
        TYA     
        STA     $8
        STX     $a
        LDA     $16
        LDY     $18
        TYX     
        AND     $8
        TAY     
        TXA     
        AND     $a
        TAX     
        TYA     
        STA     $1a
        TXA     
        STA     $1c
        LDA     $1a
        LDY     $1c
        TYX     
        STA     $8
        STX     $a
        LDA     $16
        LDY     $18
        TYX     
        AND     $8
        TAY     
        TXA     
        AND     $a
        TAX     
        TYA     
        STA     $1a
        TXA     
        STA     $1c
        LDA     $72
        DEC     A
        STA     $72
        LDA     $72
        CMP     #$00
        BEQ     t178
        BRL     t170
t178    BRL     donecase

casebad anop
        REP     #$20
        LONGA   ON
        PHK     
        PER     casebad
        PEA     |$4
        JMP     >M2Lib__Crash

labellist anop
        DC      I2'casea-1'
        DC      I2'caseb-1'
        DC      I2'casec-1'
        DC      I2'cased-1'
        DC      I2'casee-1'
        DC      I2'casef-1'
        DC      I2'caseg-1'
        DC      I2'caseh-1'
        DC      I2'casei-1'
        DC      I2'casej-1'
        DC      I2'casek-1'
        DC      I2'casel-1'
        DC      I2'casem-1'
        DC      I2'casen-1'
        DC      I2'caseo-1'
        DC      I2'casep-1'
        DC      I2'caseq-1'
        DC      I2'caser-1'
        DC      I2'cases-1'
        DC      I2'caset-1'
        DC      I2'caseu-1'
        DC      I2'casev-1'
        DC      I2'casew-1'
        DC      I2'casex-1'
        DC      I2'casebad-1'

docase anop
        SEP     #$20
        LONGA   OFF
        PLA     
        REP     #$20
        LONGA   ON
        AND     #$ff
        SEP     #$20
        LONGA   OFF
        CMP     #$61 ; char='a'
        REP     #$20
        LONGA   ON
        BCC     badrng
        SEP     #$20
        LONGA   OFF
        CMP     #$78 ; char='x'
        REP     #$20
        LONGA   ON
        BEQ     okrng
        BCC     okrng

badrng anop
        LDA     #$18
        BRA     entercase

okrng anop
        STA     $10
        LDA     #$61 ; char='a'
        STA     $12
        LDA     $10
        AND     #$ff
        SEP     #$20
        LONGA   OFF
        SEC     
        SBC     $12
        REP     #$20
        LONGA   ON

entercase anop
        ASL     A
        TAX     
        LDA     >labellist,X
        PHA     
        RTS     

donecase anop
        LDA     M2Lib_StackFramePointer
        TCS     
        PLD     
        PLA     
        STA     M2Lib_Display
        PLA     
        STA     M2Lib_StackFramePointer
        LDA     $2,S
        TAY     
        LDA     $1,S
        TAX     
        TSC     
        CLC     
        ADC     #$01
        TCS     
        TYA     
        STA     $2,S
        TXA     
        STA     $1,S
        RTL     

  END

Bench2__Initialisation START

        USING   ~__Globals
        USING   M2Lib__Globals

        LDA     M2Lib_StackFramePointer
        PHA     
        LDA     M2Lib_Display
        PHA     
        PHD     
        TSC     
        STA     M2Lib_StackFramePointer
        SEC     
        SBC     #$16
        TCS     
        INC     A
        TCD     
        STA     M2Lib_Display
        LDA     >M2Lib__Bootup
        BNE     i2
        LDA     >i3
        BNE     i1
        BRL     endofi
i1      LDA     #$00
        STA     >i3
        BRA     i5
i2      LDA     >i3
        BEQ     i4
        BRL     endofi
i3      DS      2
i4      LDA     #$01
        STA     >i3
i5      JSL     >EZDump__Initialisation
        JSL     >Terminal__Initialisation
        JSL     >Storage__Initialisation
        JSL     >InOut__Initialisation
        JSL     >FileSystem__Initialisation
        LDA     >M2Lib__Bootup
        BNE     i6
        BRL     endofi
i6      LDX     #Bench2_f|-16
        LDA     #Bench2_f
        PHX     
        PHA     
        LDA     #$09
        PHA     
        LDA     #Bench2__String0000
        LDX     #Bench2__String0000|-16
        PHX     
        PHA     
        LDA     #$01
        PHA     
        JSL     >FileSystem_Lookup
        LDX     #$00
        LDA     #$00
        TXY     
        STA     Bench2_head
        TYA     
        STA     Bench2_head+2
        LDA     #$64 ; char='d'
        STA     Bench2_n
i7      LDA     Bench2_head+2
        TAY     
        LDA     Bench2_head
        TYX     
        TXY     
        STA     Bench2_q
        TYA     
        STA     Bench2_q+2
        LDX     #Bench2_head|-16
        LDA     #Bench2_head
        PHX     
        PHA     
        LDA     #$08
        PHA     
        JSL     >Storage_ALLOCATE
        LDA     Bench2_head+2
        TAX     
        LDA     Bench2_head
        STA     $8
        STX     $a
        LDA     Bench2_q+2
        TAY     
        LDA     Bench2_q
        TYX     
        STA     [$8]
        TXA     
        LDY     #$02
        STA     [$8],Y
        LDA     Bench2_n
        DEC     A
        STA     Bench2_n
        LDA     Bench2_n
        CMP     #$00
        BEQ     i8
        BRL     i7
i8      LDA     #Bench2__String0001
        LDX     #Bench2__String0001|-16
        STA     $8
        STX     $a
        LDA     #$16
        PHA     
        LDA     $8
        LDX     $a
        PHX     
        PHA     
        JSL     >Terminal_WriteString
        JSL     >Terminal_WriteLn
        JSL     >Terminal_WriteLn
        LDA     #Bench2__String0002
        LDX     #Bench2__String0002|-16
        STA     $8
        STX     $a
        LDA     #$4d ; char='M'
        PHA     
        LDA     $8
        LDX     $a
        PHX     
        PHA     
        JSL     >Terminal_WriteString
        JSL     >Terminal_WriteLn
        JSL     >Terminal_WriteLn
        LDA     #Bench2__String0003
        LDX     #Bench2__String0003|-16
        STA     $8
        STX     $a
        LDA     #$0b
        PHA     
        LDA     $8
        LDX     $a
        PHX     
        PHA     
        JSL     >Terminal_WriteString
        LDX     #Bench2_ch|-16
        LDA     #Bench2_ch
        PHX     
        PHA     
        JSL     >Terminal_Read
beginwhile      LDA     Bench2_ch
        AND     #$ff
        STA     $10
        LDA     #$61 ; char='a'
        SEP     #$20
        LONGA   OFF
        CMP     $10
        REP     #$20
        LONGA   ON
        BCC     i9
        BEQ     i9
        BRL     endofwhile
i9      LDA     Bench2_ch
        AND     #$ff
        SEP     #$20
        LONGA   OFF
        CMP     #$78 ; char='x'
        REP     #$20
        LONGA   ON
        BCC     i10
        BEQ     i10
        BRL     endofwhile
i10     LDA     #$00
        STA     Bench2_n
beginrepeat     LDA     Bench2_n
        INC     A
        STA     Bench2_n
        LDA     Bench2_ch
        AND     #$ff
        SEP     #$20
        LONGA   OFF
        PHA     
        REP     #$20
        LONGA   ON
        JSL     >Bench2_Test
        PHA
        JSL     >Bench2_KeyPressed
        PLA     
        BEQ     beginrepeat
i11     LDA     #Bench2__String0004
        LDX     #Bench2__String0004|-16
        STA     $8
        STX     $a
        LDA     #$04
        PHA     
        LDA     $8
        LDX     $a
        PHX     
        PHA     
        JSL     >Terminal_WriteString
        LDA     Bench2_n
        PHA     
        LDA     #$04
        PHA     
        JSL     >InOut_WriteCard
        LDA     #Bench2__String0005
        LDX     #Bench2__String0005|-16
        STA     $8
        STX     $a
        LDA     #$07
        PHA     
        LDA     $8
        LDX     $a
        PHX     
        PHA     
        JSL     >Terminal_WriteString
        JSL     >Terminal_WriteLn
        LDA     #Bench2__String0006
        LDX     #Bench2__String0006|-16
        STA     $8
        STX     $a
        LDA     #$0b
        PHA     
        LDA     $8
        LDX     $a
        PHX     
        PHA     
        JSL     >Terminal_WriteString
        LDX     #Bench2_ch|-16
        LDA     #Bench2_ch
        PHX     
        PHA     
        JSL     >Terminal_Read
        BRL     beginwhile

endofwhile      JSL     >Terminal_WriteLn
        LDX     #Bench2_f|-16
        LDA     #Bench2_f
        PHX     
        PHA     
        JSL     >FileSystem_Close
        LDA     #$00
        STA     >M2Lib__Bootup
        JSL     >Bench2__Initialisation
        LDA     #$01
        STA     >M2Lib__Bootup
endofi  LDA     M2Lib_StackFramePointer
        TCS     
        PLD     
        PLA     
        STA     M2Lib_Display
        PLA     
        STA     M2Lib_StackFramePointer
        RTL     

        END

~__Globals DATA ~globals

Bench2_A      ENTRY
              DS 512
Bench2_B      ENTRY
              DS 512
Bench2_C      ENTRY
              DS 512
Bench2_D      ENTRY
              DS 256
Bench2_E      ENTRY
              DS 256
Bench2_M      ENTRY
              DS 20000
Bench2_head   ENTRY
              DS 4
Bench2_f      ENTRY
              DS 517
Bench2_ch     ENTRY
              DS 1
Bench2_n      ENTRY
              DS 2
Bench2_q      ENTRY
              DS 4


Bench2__String0000      ENTRY
                        DC      I1'$42,$65,$6e,$63,$68,$46,$69,$6c,$65,$00' ; Data: BenchFile.      
Bench2__String0001      ENTRY
                        DC      I1'$57,$65,$6c,$63,$6f,$6d,$65,$20,$74,$6f,$20,$42,$65,$6e,$63,$68' ; Data: Welcome to Bench
                        DC      I1'$4d,$61,$72,$6b,$20,$21,$00' ; Data: Mark !.         
Bench2__String0002      ENTRY
                        DC      I1'$53,$65,$6c,$65,$63,$74,$20,$61,$20,$42,$65,$6e,$63,$68,$6d,$61' ; Data: Select a Benchma
                        DC      I1'$72,$6b,$20,$26,$20,$70,$72,$65,$73,$73,$20,$6f,$70,$65,$6e,$2d' ; Data: rk & press open-
                        DC      I1'$61,$70,$70,$6c,$65,$2d,$70,$65,$72,$69,$6f,$64,$20,$61,$66,$74' ; Data: apple-period aft
                        DC      I1'$65,$72,$20,$61,$20,$73,$65,$6c,$65,$63,$74,$65,$64,$20,$70,$65' ; Data: er a selected pe
                        DC      I1'$72,$69,$6f,$64,$20,$6f,$66,$20,$74,$69,$6d,$65,$2e,$00' ; Data: riod of time..  
Bench2__String0003      ENTRY
                        DC      I1'$5b,$22,$61,$22,$2e,$2e,$22,$78,$22,$5d,$3e,$00' ; Data: ["a".."x"]>.    
Bench2__String0004      ENTRY
                        DC      I1'$2d,$2d,$3e,$20,$00' ; Data: --> .           
Bench2__String0005      ENTRY
                        DC      I1'$20,$6c,$6f,$6f,$70,$73,$2e,$00' ; Data:  loops..        
Bench2__String0006      ENTRY
                        DC      I1'$5b,$22,$61,$22,$2e,$2e,$22,$78,$22,$5d,$3e,$00' ; Data: ["a".."x"]>.    

  END
