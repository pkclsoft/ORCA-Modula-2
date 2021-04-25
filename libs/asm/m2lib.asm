
* M2Lib
*
* This library consists of routines called by the Modula-2 run-time.  Examples
* of such routines are block move and compare routines.  These are routines
* that would be inefficient to generate as inline code within the compiler, and
* as such the compiler (where necessary) generates calls to these routines.
*
* Copyright 1989...End Of Time
* EZ-Soft, Cameron Computer Systems
*
* Written by: P.C. Easdown

          CASE      ON
          65816     ON
          LONGI     ON
          LONGA     ON
          OBJCASE   ON

          KEEP      M2Lib

          MLOAD     M2Lib.Macros

*
* Dummy is a tacky way of forcing all of the routines into the .A file. The ORCA
* assembler will place Dummy in the .ROOT file and then place all other segments
* into the .A file.
*
Dummy     start
          end Dummy

*
* M2Lib_CopyBytes
*
* This routine will copy one long addressed variable to another long addressed
* variable.  The four byte addresses of the source and destination are passed
* on the stack, followed by the number of bytes to copy.
*
* Note: The maximum number of bytes transferrable is 65536 bytes.
*

M2Lib_CopyBytes START

          LONGI     ON
          LONGA     ON

Length    EQU       $00
Dest      EQU       $02
Source    EQU       $06

          TSC                     ;Point the Direct page register at our
          PHD                     ;parameters
          CLC
          ADC       #$0004
          TCD

          LDA       Length
          CMP       #$0000
          BNE       SomeBytes

ExitCopy  PLD                     ;Get rid of parameters, and return to
          LDA       $0002,S       ;caller.
          STA       $000C,S
          LDA       $0001,S
          STA       $000B,S
          TSC
          CLC
          ADC       #$000A
          TCS
          RTL

SomeBytes PHB                     ;Preserve the Bank register
          PHK                     ;Set the Data Bank to be the same as the
          PLB                     ;program bank.  We are about to execute some
;                                  self modifying code.

          DEC       A             ;The accumulator holds the length - 1

          LDX       Source+2      ;Check to see if both variables are in the
          CPX       Dest+2        ;same bank.
          BNE       DoMVN2        ; No; Thus the two variables don't overlap

          PHA                     ;We need the address of the end of the source
          CLC                     ;range so that we can check for overlap.
          ADC       Source

          LDX       Source        ;The X register holds the source address
          LDY       Dest          ;The X register holds the destination address

          CPY       Source        ;Is the source lower than the destination
          BMI       DoMVN         ; Yes; It's ok to do the MVN instruction
          CMP       Dest          ; No; Is the destination within the source
          BMI       DoMVN         ; No; It's ok to do the MVN instruction

; An overlap where the destination begins within the source range has been
; detected.  This means we can't use the MVN instruction as it would overwrite
; the source before completely copying it.  We must therefor adjust the source
; and destination addresses, and use the MVP instruction instead.

          TAX                     ;Place the new source address in X
          LDA       1,s           ;Load the byte count from the stack
          CLC
          ADC       Dest          ;Calculate the end of destination address
          TAY                     ;Place the new destnation address in Y
          PLA                     ;Get the byte count from the stack
          XBA

          SHORTM
          LDA       Dest+2
          STA       MVPDest
          LDA       Source+2
          STA       MVPSource
          LDA       #$00
          LONGM

          XBA

          DC        I1'$44'
MVPDest   DS        1
MVPSource DS        1

          PLB                     ;Restore the bank register
          BRA       ExitCopy

DoMVN     PLA                     ;Get the byte count from the stack
DoMVN2    XBA                     ;Banks weren't the same

          LDX       Source
          LDY       Dest

          SHORTM
          LDA       Dest+2
          STA       MVNDest
          LDA       Source+2
          STA       MVNSource
          LDA       #$00
          LONGM

          XBA

          DC        I1'$54'
MVNDest   DS        1
MVNSource DS        1

          PLB                     ;Restore the bank register
          BRA       ExitCopy

          END       M2Lib_CopyBytes

*
* M2Lib_CopyString
*
* Will copy the source string to the destination address.  The copy process is
* done byte-by-byte, and ends when either a NUL byte is found, or the specified
* length is reached.
*

M2Lib_CopyString START

          LONGI     ON
          LONGA     ON

Dest      EQU       $00
DesLen    EQU       $04
Source    EQU       $06
SrcLen    EQU       $0A

          TSC                     ;Point the Direct page register at our
          PHD                     ;parameters
          CLC
          ADC       #$0004
          TCD

SomeBytes LDY       #$0000        ;Place the byte count in Y

          SHORTM

Loop1     LDA       [Source],Y    ;Get a byte
          STA       [Dest],Y
          CMP       #$00          ;Is this a nul terminator?
          BEQ       EndLoop1      ;Yes.
          INY
          CPY       SrcLen        ;Have we copied the entire array?
          BEQ       EndLoop1      ;Yes, no nul terminator!
          CPY       DesLen        ;Can we fit any more in?
          BNE       Loop1         ;Yes, do another byte.

EndLoop1  LONGM

          PLD                     ;Get rid of parameters, and return to
          LDA       $0002,S       ;caller.
          STA       $000E,S
          LDA       $0001,S
          STA       $000D,S
          TSC
          CLC
          ADC       #$000C
          TCS
          RTL

          END       M2Lib_CopyString

*
* M2Lib_CompareBytes
*
* This routine will compare two long addressed variables for equality.  The
* parameters and result are passed using the Modula-2 calling convention 
* in the same manner as Tool Calls.
*
* The stack contains:
*   o Word space for the result
*   o Longword address of first variable
*   o Longword address of second variable
*   o Word space containing the length of the comparison
*   o Return address
*
* The result is a signed integer.  It's value is:
*   -1 If the first variable < second variable
*    0 If they are equal
*    1 If the first variable > second variable
*
* Note: 1)  The maximum number of bytes that can be compared is 65535, NOT
*           65536 bytes.
*

M2Lib_CompareBytes START

          LONGI     ON
          LONGA     ON

Length    EQU       $00
Second    EQU       $02
First     EQU       $06
Result    EQU       $0A

Smaller   EQU       $FFFF
Equal     EQU       $0000
Bigger    EQU       $0001

          TSC                     ;Point the Direct page register at our
          PHD                     ;parameters
          CLC
          ADC       #$0004
          TCD

          LDA       Length
          CMP       #$0000
          BNE       SomeBytes

ExitComp  STA       Result        ;Save result
          PLD                     ;Get rid of parameters, and return to
          LDA       $0002,S       ;caller.
          STA       $000C,S
          LDA       $0001,S
          STA       $000B,S
          TSC
          CLC
          ADC       #$000A
          TCS
          RTL

SomeBytes LDY       #$0000        ;We start the compare at the first byte

          SHORTM                  ;This is a byte-by-byte arrangement.

Loop      LDA       [First],Y     ;Get a byte
          CMP       [Second],Y    ;Is it the same?
          BNE       NotEqual      ; No; Exit routine
          INY                     ; Yes; Goto next byte
          CPY       Length        ;At end of loop?
          BNE       Loop          ; No; Compare next byte

          LONGM
          LDA       #Equal        ;Byte ranges were the same
          BRA       ExitComp

NotEqual  LONGM                   ;Ensure 16 bit Accumulator and Memory
          BCS       Greater       ;Differentiate between < and >
          LDA       #Smaller
          BRA       ExitComp
Greater   LDA       #Bigger
          BRA       ExitComp

          END       M2Lib_CompareBytes

*
* M2Lib_CompareStr
*
* This routine will compare two modula2 strings for equality.  The
* parameters and result are passed using the Modula-2 calling convention 
* in the same manner as Tool Calls.
*
* The stack contains:
*   o Word space for the result
*   o Word containing the high boundary of the first string
*   o Longword address of first string
*   o Word containing the high boundary of the second string
*   o Longword address of second string
*   o Return address
*
* The result is a signed integer.  It's value is:
*   -1 If the first variable < second variable
*    0 If they are equal
*    1 If the first variable > second variable
*
* Note: 1)  The maximum number of bytes that can be compared is 65535, NOT
*           65536 bytes.
*

M2Lib_CompareStr START

          LONGI     ON
          LONGA     ON

Second    EQU       $00
SecHigh   EQU       $04
First     EQU       $06
FirHigh   EQU       $0A
Result    EQU       $0C

Smaller   EQU       $FFFF
Equal     EQU       $0000
Bigger    EQU       $0001

          TSC                     ;Point the Direct page register at our
          PHD                     ;parameters
          CLC
          ADC       #$0004
          TCD

          INC       SecHigh
          INC       FirHigh

SomeBytes LDY       #$0000        ;We start the compare at the first byte

          SHORTM                  ;This is a byte-by-byte arrangement.

Loop      LDA       [First],Y
          CMP       [Second],Y    ;Is it the same?
          BNE       NotEqual      ; No; Exit routine
          CMP       #$00
          BEQ       TheSame
          INY                     ; Yes; Goto next byte
          CPY       FirHigh       ;End of the first string?
          BEQ       HighFirst     ; Yes;
          CPY       SecHigh       ; No; End of the second string?
          BNE       Loop          ;Compare next byte
          BRA       Greater

Lessor    LONGM
          LDA       #Smaller
          BRA       ExitComp
 
HighFirst CPY       SecHigh
          BEQ       TheSame
          LDA       [Second],Y
          BEQ       TheSame
          BRA       Lessor

EndSecond LDA       [First],Y
          BEQ       TheSame

          LONGM
          LDA       #Bigger
          BRA       ExitComp

TheSame   LONGM
          LDA       #Equal
          BRA       ExitComp

NotEqual  LONGM                   ;Ensure 16 bit Accumulator and Memory
          BCS       Greater       ;Differentiate between < and >
          LDA       #Smaller
          BRA       ExitComp

Greater   LDA       #Bigger

ExitComp  STA       Result        ;Save result
          PLD                     ;Get rid of parameters, and return to
          LDA       $0002,S       ;caller.
          STA       $000E,S
          LDA       $0001,S
          STA       $000D,S
          TSC
          CLC
          ADC       #$000C
          TCS
          RTL

          END       M2Lib_CompareStr

*
* M2Lib_FormatReal
*
* This procedure acts as a glue to the ORCA routine ~CnvES.  It provides
* a modula-2 interface, thus allowing any modula-2 program to access the
* ORCA routine simply.
*
* PROCEDURE FormatReal(x: REAL; n, k: CARDINAL; VAR str: ARRAY OF CHAR);
* (*
*   OPERATION:
*     Convert the REAL number x into a string with max characters n, and
*     k digits after the decimal point.  If n is zero, then the number is
*     converted to exponential format.
* *)
M2Lib_FormatReal  START

          TSC
          PHD
          TCD

          PEA       0               ;push the reals address
          CLC
          ADC       #14
          PHA
          JSL       >~LOADREAL
          LDA       24,S
          PHA
          LDA       24,S
          PHA
          LDA       24,S
          PHA
          LDA       24,S
          PHA
          LDA       24,S
          PHA
          JSL       >~M2FormatReal

          LDA       4,S
          STA       18,S
          LDA       3,S
          STA       17,S
          PLD
          TSC
          CLC
          ADC       #14
          TCS

          RTL

          END M2Lib_FormatReal

*
* M2Lib_FormatLongReal
*
* This procedure acts as a glue to the ORCA routine ~FormatReal.  It provides
* a modula-2 interface, thus allowing any modula-2 program to access the
* ORCA routine simply.
*
* PROCEDURE FormatLongReal(x: LONGREAL; n, k: CARDINAL; VAR str: ARRAY OF CHAR);
* (*
*   OPERATION:
*     Convert the REAL number x into a string with max characters n, and
*     k digits after the decimal point.  If n is zero, then the number is
*     converted to exponential format.
* *)
M2Lib_FormatLongReal  START

          TSC
          PHD
          TCD

          PEA       0               ;push the reals address
          CLC
          ADC       #14
          PHA
          JSL       >~LOADDOUBLE
          LDA       24,S
          PHA
          LDA       24,S
          PHA
          LDA       24,S
          PHA
          LDA       24,S
          PHA
          LDA       24,S
          PHA
          JSL       >~M2FormatReal

          LDA       4,S
          STA       22,S
          LDA       3,S
          STA       21,S
          PLD
          TSC
          CLC
          ADC       #18
          TCS

          RTL

          END M2Lib_FormatLongReal

*
* M2Lib_UserID
*
* The application may use this procedure to obtain the user id supplied by the
* system loader.
*

M2Lib_UserID  START

Result    EQU       $0004

          LDA       >~USER_ID
          STA       Result,S
          RTL

          END       M2Lib_UserID
*
* M2Lib_ToolError
*
* This routine is by the application when it wishes to determine the error
* code returned by the last TOOL or GSOS call.
*

M2Lib_ToolError  START

Result    EQU       $0004

          LDA       >~TOOLERROR
          STA       Result,S
          RTL

          END       M2Lib_ToolError

*
* M2Lib_HighWORD
*
* This procedure may be used to obtain the high word of a longword structure.
*

M2Lib_HighWORD  START

Result    EQU       $0008
HighWord  EQU       $0006

          LDA       HighWord,S
          STA       Result,S
          LDA       $0002,S
          STA       HighWord,S
          LDA       $0001,S
          STA       HighWord-1,S
          TSC
          CLC
          ADC       #$0004
          TCS
          RTL

          END       M2Lib_HighWORD

*
* M2Lib_LoWORD
*
* This procedure may be used to obtain the low word of a longword structure.
*

M2Lib_LoWORD  START

Result    EQU       $0008
LowWord   EQU       $0004

          LDA       LowWord,S
          STA       Result,S
          LDA       $0002,S
          STA       LowWord+2,S
          LDA       $0001,S
          STA       LowWord+1,S
          TSC
          CLC
          ADC       #$0004
          TCS
          RTL

          END       M2Lib_LoWORD

*
* M2Lib_LongWORD
*
* This procedure may be used to obtain the low word of a longword structure.
*

M2Lib_LongWORD  START

Result    EQU       $0008
LowWord   EQU       $0006
HighWord  EQU       $0004

          LDA       LowWord,S
          STA       Result,S
          LDA       HighWord,S
          STA       Result+2,S
          LDA       $0002,S
          STA       LowWord,S
          LDA       $0001,S
          STA       LowWord-1,S
          TSC
          CLC
          ADC       #$0004
          TCS
          RTL

          END       M2Lib_LongWORD

*
* PutChar(ch: CHAR);
*
* OPERATION:
*   Provides a glue to the ORCA PutC routine for using the standard IO 
*   routines.
*
M2Lib_PutChar START
ch        EQU       4
newreturn EQU       2
oldreturn EQU       1

          TSC
          PHD
          TCD
          PHB
          PEA       SYSCHAROUT|-8
          PLB
          PLB
          LDA       ch
          AND       #$ff
          PHA
          JSL       >SYSCHAROUT
          PLB
          LDY       oldreturn+1   ;Shuffle return address up
          STY       newreturn+1
          LDY       oldreturn
          STY       newreturn
          PLD                     ;Restore D register
          TSC
          INC       A
          TCS
          RTL

          END       M2Lib_PutChar

*
* PutChar(ch: CHAR);
*
* OPERATION:
*   Provides a glue to the ORCA PutC routine for using the standard IO 
*   routines.
*
M2Lib_PutCharToError START
ch        EQU       4
newreturn EQU       2
oldreturn EQU       1

          TSC
          PHD
          TCD
          PHB
          PEA       SYSCHARERROUT|-8
          PLB
          PLB
          LDA       ch
          AND       #$ff
          PHA
          JSL       >SYSCHARERROUT
          PLB
          LDY       oldreturn+1   ;Shuffle return address up
          STY       newreturn+1
          LDY       oldreturn
          STY       newreturn
          PLD                     ;Restore D register
          TSC
          INC       A
          TCS
          RTL

          END       M2Lib_PutCharToError

*
* GetChar(VAR ch: CHAR; VAR done: BOOLEAN);
*
* OPERATION:
*   Provides a glue to the ORCA GetC routine for using the standard IO 
*   routines.
*
M2Lib_GetChar START

newreturn EQU       9
ch        EQU       8
done      EQU       4
oldreturn EQU       1
true      EQU       1

          TSC
          PHD
          TCD

          PHB
          PEA       SYSKEYAVAIL|-8
          PLB
          PLB
          JSL       >SYSKEYAVAIL
          PLB
          BNE       getthekey
          STA       [done]
          SHORTM
          STA       [ch]
          LONGM
          BRA       complete

getthekey ANOP

          PHB
          PEA       SYSKEYIN|-8
          PLB
          PLB
          JSL       >SYSKEYIN
          PLB
          SHORTM
          STA       [ch]
          LONGM
          LDA       #true         ;Set the done flag to true
          STA       [done]

complete  LDY       oldreturn+1   ;Shuffle return address up
          STY       newreturn+1
          LDY       oldreturn
          STY       newreturn
          PLD                     ;Restore D register
          TSC
          CLC
          ADC       #newreturn-oldreturn
          TCS
          RTL

          END       M2Lib_GetChar

*
* M2Lib__Crash
*
* This routine is called whenever a runtime error occurs that forces execution
* to cease.  It is responsible for creating a Post Mortem Dump, and closing
* down the application.
*
* Note: Enter this routine using a JMP instruction, not a JSR or JSL.
*
*       Although a JMP is used to enter this routine, the stack is assumed to
*       take the form:
*
*       o Bank number of the address at which the application terminated (BYTE)
*       o Address within the bank at which the application terminated (WORD)
*       o WORD value that is the reason code for the termination
*

M2Lib__Crash  START

TermBank    EQU     $0004
TermAddr    EQU     $0002
TermReason  EQU     $0000

*
* It is possible that while processing the termination, another crash may
* occur.  This may happen in one of two places: 1) The Stackdump procedure
* and 2) The Application Installed Termination Procedure.
*
* To handle this problem, we have a flag which is preset to zero by the
* assembler.  When this code is first executed, we set the flag to one so that
* any further jumps cause the terminate code to skip both the Stackdump and
* Termination Procedure Call.
*
          TSC
          INC       A
          TCD

          LDA       >M2Lib__Terminated      ;Check to ensure that we dont get
          BNE       QuitDontLoop            ;into an eternal loop.

          LDA       #$0001
          STA       >M2Lib__Terminated

          LDA       >M2Lib_StackFramePointer  ;Save the value of the stack
          STA       >M2Lib_CrashStackFrame    ;pointer for the PMD (if any).

          LDA       >M2Lib_TermProc         ;Check for an Application installed
          ORA       >M2Lib_TermProc+$0002   ;Termination procedure.
          BEQ       NoTermProcInstalled

          LDA       TermBank                ;Push the address of the
          AND       #$00FF                  ;termination point.
          TAX
          LDA       TermAddr
          PHX
          PHA
          LDA       TermReason              ;Push the termination reason.
          SHORTM
          PHA
          LONGM

          LDA       >M2Lib_TermProc         ;Setup call to the application
          STA       >CallTerm+$0001         ;installed termination procedure.
          LDA       >M2Lib_TermProc+$0002
          SHORTM
          STA       >CallTerm+$0003
          LONGM

CallTerm  JSL       >$000000                ;Call the application installed
;                                            termination procedure.

NoTermProcInstalled ANOP
QuitDontLoop        ANOP

          LDA       >M2Lib__InitProc        ;Check for an Application installed
          ORA       >M2Lib__InitProc+$0002  ;Termination procedure.
          BEQ       NoInitProcInstalled

          LDA       #$0000
          STA       >M2Lib__Bootup
          LDA       >M2Lib__InitProc        ;Setup call to the application
          STA       >CallInit+$0001         ;installed termination procedure.
          LDA       >M2Lib__InitProc+$0002
          SHORTM
          STA       >CallInit+$0003
          LONGM

CallInit  JSL       >$000000                ;Call the application installed
;                                            termination procedure.

          LDA       #$0001
          STA       >M2Lib__Bootup

NoInitProcInstalled ANOP

          PEA       M2Lib__CloseParms|-16
          PEA       M2Lib__CloseParms       ;Close all open files.
          PEA       $2014
          JSL       $E100B0

          PEA       M2Lib__SetLInfoDCB|-16  ;Make sure that the shell doesn't
          PEA       M2Lib__SetLInfoDCB      ;try to do anything else.
          PEA       $0142
          JSL       $E100B0

          LDX       TermReason              ;Get the M2 Termination code
          LDA       >OrcaErrorCode,X        ;xlate to an ORCA code
          AND       #$00FF

          CMP       #$0000
          BEQ       OKQuit                  ;If no error, then use normal quit
          PHA                               ; otherwise push the return code,
          JSL       >SYSTEMERROR            ; and use ORCA SystemError
OKQuit    JML       >~QUIT

OrcaErrorCode ENTRY
          DC    H'00 0C 01 05 08 0D 01 11 12 0F 13 14'

          END       M2Lib__Crash

*
* M2Lib_Terminate
*
* Provides a modula style entry point to the M2Lib_Crash routine.
*
* At entry, the stack contains:
*
*  o Reason code (WORD)
*  o Return address (3 BYTES)
*
* Since Crash requires a termination address, we supply the address of this
* routine, as well as the supplied reason code.
*
M2Lib_Terminate START

Reason    EQU       $0007

          LDA       #M2Lib_Terminate|-16
          SHORTM
          PHA
          LONGM
          PEA       M2Lib_Terminate
          LDA       Reason,S
          AND       #$00FF
          PHA
          JMP       >M2Lib__Crash

          END M2Lib_Terminate

procDescSize        GEQU    $002A

*
* M2Lib_NEWPROCESS
*
* This procedure provides the Modula-2 standard NEWPROCESS call.  It creates
* a new process descriptor, and stores the relevant information within it in
* preparation for a TRANSFER call (see M2Lib_TRANSFER).
*
* Note that the process descriptor that is created is the same as the format
* described in M2Lib.DEF as "aProcessDescriptor".
*
* Parameters:
*
*    P:  PROC       The address of the Coroutine entry point.
*    A:  ADDRESS    The base address of the workSpace (stack).
*    n:  CARDINAL   The size in bytes of the workSpace (stack).
*    p1: ADDRESS    The result parameter (address of actual).
*
M2Lib_NEWPROCESS    START

returnAddress       EQU     15
P                   EQU     14
Adr                 EQU     10
n                   EQU      8
p1                  EQU      4
suppliedRetAdr      EQU      1

                    TSC                     ;Make DP point to our parameters
                    PHD
                    TCD

                    LDX     p1+2            ;Allocate the memory for the new
                    LDA     p1              ;process descriptor
                    PHX
                    PHA
                    PEA     procDescSize    ;Size of aProcessDescriptor
                    JSL     >Storage_ALLOCATE

                    LDY     #$0002
                    LDA     [p1],Y
                    TAX
                    LDA     [p1]
                    STA     p1
                    STX     p1+2

; NOTE.  We assume that the workSpace is in bank zero, and hence ignore the
; high word of the address.

                    LDA     Adr             ;Take the low word of A, and use
                    LDY     #$0002          ;it as the base of the stack
                    STA     [p1],Y          ;area.

                    LDY     #$0006          ;Initially, the direct page will
                    STA     [p1],Y          ;be at the bottom of the workspace

                    CLC                     ;Now setup the top of stack field
                    ADC     n
                    STA     [p1]

                    LDY     #$0004          ;Top of stack is also the initial
                    STA     [p1],Y          ;value of the stack register

                    LDA     P               ;Now place the address of the
                    BNE     noCross         ;procedure in the progCounter
                    DEC     P+2             ;field, noting that it must be
noCross             DEC     A               ;1 less than the actual address
                    LDY     #$0008          ;since we use RTL to transfer from
                    STA     [p1],Y          ;one process to another.
                    INY
                    INY
                    LDA     P+2
                    STA     [p1],Y

                    LDY     #$0022          ;The process has not been
                    LDA     #$0000          ;activated as yet.
                    STA     [p1],Y

                    LDA     suppliedRetAdr+1  ;Now move the return address
                    STA     returnAddress+1
                    LDA     suppliedRetAdr
                    STA     returnAddress

                    PLD                     ;Restore the DP register
                    TSC                     ;Move stack to just below the
                    CLC                     ;newly moved return address.
                    ADC     #14
                    TCS
                    RTL

                    END M2Lib_NEWPROCESS

*
* M2Lib_TRANSFER
*
* This procedure provides the Modula-2 standard TRANSFER call.  It saves
* the following run-time variables in the process descriptor whose address
* is 'p1':
*
*       StackTop
*       StackBottom
*       M2Lib_StackFramePointer
*       M2Lib_Display
*       Stack register
*       Direct page register
*       24 Bit program counter
*
* The Data Bank register is not currently saved.
*
* After saving this information, these same variables are loaded from the
* process descriptor whose address is 'p2'.
*
* Note that although this describes the resulting affect, the actual order
* may be different.  It may be that both 'p1' and 'p2' point to the same
* descriptor, as such the actual code must preserve the original descriptor
* before overwriting it with the current run-time variables.
*
M2Lib_TRANSFER      START

returnAddress       EQU     9
p1                  EQU     8
p2                  EQU     4
suppliedRetAdr      EQU     1

                    TSC                     ;Make DP point to our parameters
                    PHD
                    TCD

                    LDY     #$0002
                    LDA     [p2],Y
                    TAX
                    LDA     [p2]
                    STA     p2
                    STX     p2+2

                    LDY     #$0022          ;Has 'p2' been activated before?
                    LDA     [p2],Y
                    STA     >activeFlag     ;Save the flag
                    BNE     activated       ;Already activated
                    LDA     #$0001          ;This is the first time, set the
                    STA     [p2],Y          ;flag in the descriptor

activated           ANOP
                    PEA     tempDescriptor|-16
                    PEA     tempDescriptor
                    LDA     p2              ;Copy p2 to temporary area
                    LDX     p2+2
                    PHX
                    PHA
                    PEA     procDescSize    ;Size of aProcessDescriptor
                    JSL     >~MOVE

                    LDA     >M2Lib_activeProcess
                    STA     [p1]
                    TAX
                    LDY     #$0002
                    LDA     >M2Lib_activeProcess+2
                    STA     [p1],Y
                    STX     p1
                    STA     p1+2

                    LDA     >M2Lib_StackTop ;Preserve top of stack
                    STA     [p1]

                    LDA     >M2Lib_StackBottom  ;Preserve bottom of stack
                    LDY     #$0002
                    STA     [p1],Y

                    LDA     >M2Lib_StackFramePointer ;Preserve stack frame pointer
                    LDY     #$000C
                    STA     [p1],Y

                    LDA     p1
                    LDX     p1+2
                    CLC     
                    ADC     #$000E
                    PHX
                    PHA
                    PEA     M2Lib_Display|-16    ;Copy M2Lib_Display to copy in the
                    PEA     M2Lib_Display        ;process descriptor
                    PEA     20
                    JSL     >~MOVE

                    TSC                     ;Preserve the stack register
                    LDY     #$0004
                    STA     [p1],Y

                    TDC                     ;Preserve the DP register
                    INY
                    INY
                    STA     [p1],Y

                    LDA     #afterTransfer-1  ;Now put the address of the 
                    INY                       ;instruction after the actual
                    INY
                    STA     [p1],Y
                    INY
                    INY
                    LDA     #afterTransfer|-16
                    STA     [p1],Y

                    LDY     #$0022
                    LDA     [p1],Y          ;Load the current value of the flag
                    TAX
                    LDA     #$0001
                    STA     [p1],Y          ;Activate the process no matter what

                    TXA                     ;If we were already active then
                    BNE     dontPatch       ;don't patch the return address.

                    LDA     >M2Lib_StackFramePointer   ;Now patch the return address
                    CLC                           ;of the current procedure so
                    ADC     #$0007                ;that if it reaches it's
                    STA     $08                   ;completion code, it doesn't
                    STZ     $0A                   ;return to nowhere.  We want
                    LDY     #$0002                ;it to "return" to the
                    LDA     #~Terminate|-16       ;program termination code.
                    SHORTM
                    STA     [$08],Y
                    LONGM
                    LDA     #~Terminate-1
                    STA     [$08]

dontPatch           ANOP
                    LDA     >tempDescriptor       ;Setup the top of stack
                    STA     >M2Lib_StackTop       ;for the active process

                    LDA     >tempDescriptor+2     ;Setup the bottom of stack
                    STA     >M2Lib_StackBottom    ;for the active process

                    LDA     >tempDescriptor+12    ;Set up the Stack Frame
                    STA     >M2Lib_StackFramePointer   ;pointer

                    PEA     M2Lib_Display|-16        ;active display.
                    PEA     M2Lib_Display
                    PEA     tempDescriptor|-16  ;Copy display from the
                    PEA     tempDescriptor+14   ;process descriptor to the
                    PEA     20
                    JSL     >~MOVE

                    LDA     p2                    ;Make activeProcess
                    STA     >M2Lib_activeProcess  ;reflect the process we
                    LDA     p2+2                  ;are resuming
                    STA     >M2Lib_activeProcess+2

                    LDA     >tempDescriptor+4     ;Setup the stack register
                    TCS

                    LDA     >tempDescriptor+6     ;Setup the DP register
                    TCD

                    LDA     >activeFlag     ;Has this process been
                    BNE     active          ;activated already?

                    PHB                     ;Push three bytes. These are a fake
                    PHA                     ;return address, as if we are
;                                            JSLing to the process.  Normally
;                                            we don't use it.  Only if the
;                                            process terminates without a
;                                            transfer, and in that case we
;                                            seed this address to point to
;                                            ~Terminate.

active              ANOP
                    LDA     >tempDescriptor+10    ;Push the resumation
                    SHORTM                        ;address onto the stack
                    PHA
                    LONGM
                    LDA     >tempDescriptor+8
                    PHA

                    RTL                     ;Jump to the new process using
;                                            the address we pushed by RTLing
;                                            to it.

afterTransfer       ANOP
                    LDA     suppliedRetAdr+1
                    STA     returnAddress+1
                    LDA     suppliedRetAdr
                    STA     returnAddress

                    PLD                     ;Restore DP register
                    TSC                     ;Move the stack to just below the
                    CLC                     ;newly moved return address
                    ADC     #$0008
                    TCS

                    RTL


activeFlag          DS      2
tempDescriptor      DS      procDescSize    ;Size of aProcessDescriptor

                    END M2Lib_TRANSFER

*
* PROCEDURE IOTRANSFER(VAR p1, p2: ADDRESS; I: CARDINAL);
*
*   This procedure provides the Modula-2 standard IOTRANSFER call.  A brief
*   lineup of the actions taken by this call are:
*
*     * Save current environment in p1
*     * Bind Interrupt to the specified vector
*     * Resume the process described by p2
*
*   When an interrupt occurs, the system will return control a run-time 
*   interrupt handler which:
*     
*     * Save any registers that Apple Inc. wants restored
*     * Save value of C in boolean variable M2Lib_IntHandled
*     * Save current environment in p2
*     * Resume the process described by p1
*     * RTL back out of IOTRANSFER.
*     
*   That process should handle the interrupt, and execute another IOTRANSFER 
*   as quickly as possible.  When it does, the actions are slightly 
*   different:
*
*     * Save current environment in p1
*     * Restore environment saved in p2
*     * Restore any registers that were saved for Apple Inc.
*     * Restore C flag from M2Lib_IntHandled (in case handler changed it)
*     * RTL back to GS/OS (in effect, this resumes p2)
*   
*   By Wirths definition, an interrupt that is bound by IOTRANSFER is good
*   for one instance of the interrupt only.  Also because of the definition
*   of the procedure IOTRANSFER, "I" could well be a variable.  This would
*   allow the programmer to be sneaky and change from one interrupt to another
*   midstream by changing the vector in each execution of IOTRANSFER.
*
*   This implementation of IOTRANSFER does not support Wirths Definition simply
*   because the way interrupts are handled under GS/OS makes it very very hard.
*
*   EZModula2 states:  An interrupt bound by IOTRANSFER stays bound until such
*   time as a normal co-routine explicitly Un-binds the interrupt.  This in
*   turn means that the value of "I" is only relevant for the initial call to
*   a specific instance IOTRANSFER.
*
*   Another restriction this implementation places relates to the binding of
*   several interrupt handlers to one VRN.  This is not supported.  If you
*   want several handlers for the one VRN, then you must do so by installing
*   one that decides which is then interrupting, and then branch to some
*   conditional code to handle that particular flavor.  We see no other clean
*   way of handling it.
*
M2Lib_IOTRANSFER      START

returnAddress       EQU     11              ;location of return adr ar exit
p1                  EQU     10              ;handle of p1
p2                  EQU      6              ;handle of p2 (at entry)
ivn                 EQU      4              ;location of VRN
suppliedRetAdr      EQU      1              ;location of return adr on entry

lowestVRN           EQU      8              ;The smallest legal vrn value
VRNCount            EQU     16              ;# of legal VRNs

                    TSC                     ;Make DP point to our parameters
                    PHD
                    TCD

                    LDA     >M2Lib_activeProcess    ;Copy activeProcess to the
                    STA     [p1]                    ;address variable pointed to
                    TAX                             ;by [p1]
                    LDY     #$0002
                    LDA     >M2Lib_activeProcess+2
                    STA     [p1],Y
                    STX     p1                      ;Now store in p1 so that we
                    STA     p1+2                    ;can change the descriptor
;                                                    itself.

                    LDY     #$0022          ;Has 'p1' been activated before?
                    LDA     [p1],Y
                    STA     >activeFlag     ;Save the flag
                    BNE     activated       ;Already activated
                    LDA     #$0001          ;This is the first time, set the
                    STA     [p1],Y          ;flag in the descriptor

activated           ANOP

; Save current environment in p1

                    LDA     >M2Lib_StackTop ;Preserve top of stack
                    STA     [p1]

                    LDA     >M2Lib_StackBottom  ;Preserve bottom of stack
                    LDY     #$0002
                    STA     [p1],Y

                    LDA     >M2Lib_StackFramePointer ;Preserve StackFramePointer
                    LDY     #$000C
                    STA     [p1],Y

                    LDA     p1
                    LDX     p1+2
                    CLC     
                    ADC     #$000E
                    PHX
                    PHA
                    PEA     M2Lib_Display|-16    ;Copy M2Lib_Display to copy in the
                    PEA     M2Lib_Display        ;process descriptor
                    PEA     20
                    JSL     >~MOVE

                    TSC                     ;Preserve the stack register
                    LDY     #$0004
                    STA     [p1],Y

; Instead of saving the DP register in the process descriptor, we save it in
; the intStackFrames array for the appropriate VRN.  This allows us to restore
; our DP without having to know anything other than the VRN.  From there, the
; locals (p1, p2, ivn) supply the rest of the required information.

                    LDA     ivn             ;Compute index into entry point
                    SEC                     ;table.
                    SBC     #lowestVRN
                    ASL     A
                    TAX                       ;Save the DP register so that we
                    TDC                       ;can find our locals (eg. p1, p2)
                    STA     >intStackFrames,X ;easily at interrupt time.

; NOTE.  THE X REGISTER IS USED WHEN WE BIND THE INTERRUPT, AND YOU SHOULD NOT
;        ALTER IT AT ALL UNTIL AFTER THE BINDINT CALL.  WE DON'T SAVE THE VALUE
;        OF X IN ORDER TO SAVE TIME.

; Since, when the interrupt occurs, the entry code handles the saving and
; restoring of environments, it can also simply flow on into the exit code
; for IORANSFER after is has restored the environment we just saved.  In order
; to save time, we don't bother saving an address that won't be used.

                    LDA     >active
                    BNE     alreadyActive

; This is the first time that we have called IOTRANSFER for this interrupt
; handler.  This is where we must Bind the interrupt using the GS/OS BindInt
; call.
;
; We bind the interrupt to one of the specific entry points listed in the table
; inEntryTable.  Each one represents an entry point for a specific VRN as
; defined by Apple Inc. (p265 of the GS/OS Reference)  When the interrupt occurs
; the appropriate entry point loads A with the VRN and jumps to some common code
; which does the real work behind the interrupt handling.
;
; This method allows us to use the VRN to restore the correct stack frame
; pointer from the inStackFrames table.
;

                    TXA
                    ASL     A               ;Adjust offset for a 4 byte array.
                    TAX

                    LDA     #$0003          ;Setup the BindInt parameter block
                    STA     >bindpCount
                    LDA     ivn
                    STA     >bindvrn
                    LDA     >intEntryTable,X
                    STA     >bindintCode
                    INX
                    INX
                    LDA     >intEntryTable,X
                    STA     >bindintCode+2
                    PEA     $2031
                    JSL     >$E100B0

alreadyActive       ANOP

; Restore the environment in p2

                    LDA     [p2]
                    TAX
                    LDY     #$0002
                    LDA     [p2],Y
                    STA     p2+2
                    STX     p2
                    STA     >M2Lib_activeProcess+2  ;Make activeProcess
                    TXA                             ;reflect the process we
                    STA     >M2Lib_activeProcess    ;are resuming

                    LDA     [p2]                  ;Setup the top of stack
                    STA     >M2Lib_StackTop       ;for the active process

                    LDA     [p2],Y                ;Setup the bottom of stack
                    STA     >M2Lib_StackBottom    ;for the active process

                    LDY     #$000C
                    LDA     [p2],Y                     ;Set up the Stack Frame
                    STA     >M2Lib_StackFramePointer   ;pointer

                    PEA     M2Lib_Display|-16        ;active display.
                    PEA     M2Lib_Display
                    LDX     p2+2
                    LDA     p2
                    CLC
                    ADC     #$000E
                    PHX
                    PHA
                    PEA     20
                    JSL     >~MOVE

                    LDY     #$0004
                    LDA     [p2],Y                ;Setup the stack register
                    TCS

                    INY
                    INY
                    LDA     [p2],Y                ;Setup the DP register
                    TCD

                    LDA     >activeFlag     ;Has this process been
                    BNE     active          ;activated already?

                    LDY     #$000A
                    LDA     [p2],Y                ;Push the resumation
                    SHORTM                        ;address onto the stack
                    PHA
                    LONGM
                    DEY
                    DEY
                    LDA     [p2],Y
                    PHA

                    RTL                     ;Jump to the new process using
;                                            the address we pushed by RTLing
;                                            to it.

active              ANOP
                    PLP                     ;Restore the registers required by
                    PLD                     ;Apple Inc. (p267 of GS/OS Ref)
                    PLB
                    LDA     >M2Lib_IntHandled ;Set up C flag to represent the
                    ROR     A                 ;value of IntHandled.
                    RTL                     ;RTL back to GS/OS.

MainIntEntry        ENTRY
                    PHB
                    PHD
                    PHP
                    LDA     #$0000            ;Save C flag as a boolean value in
                    ROL     A                 ;M2Lib_IntHandled so that M2 code
                    STA     >M2Lib_IntHandled ;can access it.

; The X register was set up by the appropriate interrupt entry point.  It may
; be assumed at this point that it contains the (VRN - 8) * 2 and can be used
; as an index into the inStackFrames array.

                    LDA     >intStackFrames,X ;Restore our Direct page so that
                    TCD                       ;we can access the locals. (eg. p1
;                                              p2, ivn)

; Save current environment in p2.

                    LDA     >M2Lib_StackTop ;Preserve top of stack
                    STA     [p2]

                    LDY     #$0002
                    LDA     >M2Lib_StackBottom  ;Preserve bottom of stack
                    STA     [p2],Y

                    LDY     #$000C
                    LDA     >M2Lib_StackFramePointer ;Preserve StackFramePointer
                    STA     [p2],Y

                    LDX     p2+2
                    LDA     p2
                    CLC
                    ADC     #$000E
                    PHX
                    PHA
                    PEA     M2Lib_Display|-16    ;Copy M2Lib_Display to copy in the
                    PEA     M2Lib_Display        ;process descriptor
                    PEA     20
                    JSL     >~MOVE

                    LDY     #$0004
                    TSC                     ;Preserve the stack register
                    STA     [p2],Y

                    INY
                    INY
                    TDC                     ;Preserve the DP register
                    STA     [p2],Y

; Normally, we would also store an address, but there is no need since we
; will be RTLing back to GS/OS.  In order to save time, we won't do so.

; Restore the environment in p1, and let the interrupt handler do it's stuff.

                    LDA     [p1]                  ;Setup the top of stack
                    STA     >M2Lib_StackTop       ;for the active process

                    LDA     [p1],Y                ;Setup the bottom of stack
                    STA     >M2Lib_StackBottom    ;for the active process

                    LDY     #$000C
                    LDA     [p1],Y                     ;Set up the Stack Frame
                    STA     >M2Lib_StackFramePointer   ;pointer

                    PEA     M2Lib_Display|-16        ;active display.
                    PEA     M2Lib_Display
                    LDX     p1+2
                    LDA     p1
                    CLC
                    ADC     #$000E
                    PHX
                    PHA
                    PEA     20
                    JSL     >~MOVE

                    LDA     p1                    ;Make activeProcess
                    STA     >M2Lib_activeProcess  ;reflect the process we
                    LDA     p1+2                  ;are resuming
                    STA     >M2Lib_activeProcess+2

                    LDY     #$0004
                    LDA     [p1],Y                ;Setup the stack register
                    TCS

; We previously retrieved the DP register from the intStackFrames array, so
; don't waste time getting it from the process descriptor.

                    LDA     suppliedRetAdr+1
                    STA     returnAddress+1
                    LDA     suppliedRetAdr
                    STA     returnAddress

                    PLD                     ;Restore DP register
                    TSC                     ;Move the stack to just below the
                    CLC                     ;newly moved return address
                    ADC     #$000A
                    TCS

                    RTL                     ;Return to the modula2 code.

*
* These entry points load their respective VRNs - 8, and jump to the common
* interrupt handler code at "MainIntEntry".  Note that the values are
* multiplied by 2 so that they can immediately be used as indexes into the
* intStackFrames array.
*

entAppleTalk        ENTRY
                    LDX     #$0000 * 2
                    JMP     >MainIntEntry
entSerial           ENTRY
                    LDX     #$0001 * 2
                    JMP     >MainIntEntry
entScanLine         ENTRY
                    LDX     #$0002 * 2
                    JMP     >MainIntEntry
entWaveComplete     ENTRY
                    LDX     #$0003 * 2
                    JMP     >MainIntEntry
entVBL              ENTRY
                    LDX     #$0004 * 2
                    JMP     >MainIntEntry
entMouse            ENTRY
                    LDX     #$0005 * 2
                    JMP     >MainIntEntry
entQuarterTimer     ENTRY
                    LDX     #$0006 * 2
                    JMP     >MainIntEntry
entKeyboard         ENTRY
                    LDX     #$0007 * 2
                    JMP     >MainIntEntry
entADB              ENTRY
                    LDX     #$0008 * 2
                    JMP     >MainIntEntry
entSRQ              ENTRY
                    LDX     #$0009 * 2
                    JMP     >MainIntEntry
entDeskManager      ENTRY
                    LDX     #$000A * 2
                    JMP     >MainIntEntry
entFlush            ENTRY
                    LDX     #$000B * 2
                    JMP     >MainIntEntry
entAbort            ENTRY
                    LDX     #$000C * 2
                    JMP     >MainIntEntry
ent1SecondClock     ENTRY
                    LDX     #$000D * 2
                    JMP     >MainIntEntry
entVGC              ENTRY
                    LDX     #$000E * 2
                    JMP     >MainIntEntry
entOther            ENTRY
                    LDX     #$000F * 2
                    JMP     >MainIntEntry

intEntryTable       ANOP
                    DC      I4'entAppleTalk'
                    DC      I4'entSerial'
                    DC      I4'entScanLine'
                    DC      I4'entWaveComplete'
                    DC      I4'entVBL'
                    DC      I4'entMouse'
                    DC      I4'entQuarterTimer'
                    DC      I4'entKeyboard'
                    DC      I4'entADB'
                    DC      I4'entSRQ'
                    DC      I4'entDeskManager'
                    DC      I4'entFlush'
                    DC      I4'entAbort'
                    DC      I4'ent1SecondClock'
                    DC      I4'entVGC'
                    DC      I4'entOther'

intStackFrames      ANOP                    ;The saved stack frames of each
                    DS      2 * VRNCount    ;interrupt handler. In reality
;                                            the array contains DP registers.

activeFlag          DS      2               ;Temp flag to say if we are calling
;                                            an instance of IOTRANSFER the first
;                                            time.

bindParms           ANOP                    ;The BindInt parameter block.
bindpCount          DS      2
bindintNum          DS      2
bindvrn             DS      2
bindintCode         DS      4

                    END M2Lib_IOTRANSFER

*
* ~Terminate
*
* This is used purely for the case where a co-routine reaches the end of it's
* procedure.
*
~Terminate          START
                    PEA     $000A
                    JSL     >M2Lib_Terminate
                    END     ~Terminate

*
* M2Lib__Initialisation
*
* This procedure is called up at initialisation time by calls generated by the
* compiler.
*
M2Lib__2F7900D87530 START
                    LDA     >M2Lib__Bootup
                    BNE     Start

                    LDA     #$0000
                    STA     >M2Lib__Inited

                    BRA     Done

Start               LDA     >M2Lib__Inited
                    BNE     Done

                    LDA     #$0001
                    STA     >M2Lib__Inited

                    LDA     #$0000
                    STA     >M2Lib_TermProc
                    STA     >M2Lib_TermProc+2
                    STA     >M2Lib__InitProc
                    STA     >M2Lib__InitProc+2
                    STA     >M2Lib__Terminated

Done                RTL

                    END     M2Lib__2F7900D87530

M2Lib__Globals      DATA    ~globals

M2Lib__InitProc     ENTRY
                    DC      H'00 00 00 00'

M2Lib_TermProc      ENTRY
                    DC      H'00 00 00 00'

M2Lib__Inited       ENTRY
                    DC      H'00 00'

M2Lib_MasterID      ENTRY
                    DS      $0002

M2Lib_StackTop      ENTRY
                    DS      $0002

M2Lib_StackBottom   ENTRY
                    DS      $0002

M2Lib_Display       ENTRY                   ;The run-time Display
                    DS      20

M2Lib_StackFramePointer ENTRY               ;The run-time stack frame pointer
                    DC      H'00 00'

M2Lib_CrashStackFrame ENTRY                 ;The run-time stack frame pointer
                    DC      H'00 00'        ;saved at crash time.

M2Lib_CommandLine   ENTRY
                    DC      H'00 00 00 00'

M2Lib_IntHandled    ENTRY                   ;This flag is set to the value of
                    DC      H'00 00'        ;the C flag when an interrupt occurs
;                                            that has been bound by IOTRANSFER.
;                                            The handler may in turn set or
;                                            clear the flag before executing
;                                            another IOTRANSFER which is then
;                                            placed into the C flag before
;                                            returning to GS/OS.

M2Lib_activeProcess ENTRY
                    DC      H'00 00 00 00'  ;Address of active process
;                                            descriptor.

M2Lib__sysDescriptor ENTRY                  ;The actual system process
                     DS     24              ;descriptor


M2Lib__Bootup       ENTRY
                    DC      H'00 01'

M2Lib_NDACode       ENTRY
                    DC      H'00 00'

M2Lib__Terminated   ENTRY
                    DC      H'00 00'

M2Lib__QuitParms    ENTRY
M2Lib__quitPCount   DC      H'00 00'
M2Lib__quitPath     DC      H'00 00 00 00'
M2Lib__quitFlags    DC      H'40 00'

M2Lib__CloseParms   ENTRY
                    DC      H'00 01'        ;pcount = 1
                    DC      H'00 00'        ;refNum = 0, close all files

M2Lib__SetLInfoDCB  ENTRY
                    DC      H'00 0B'        ;pcount = 11
                    DC      H'00 00 00 00'  ;sfile = NIL
                    DC      H'00 00 00 00'  ;dfile = NIL
                    DC      H'00 00 00 00'  ;parms = NIL
                    DC      H'00 00 00 00'  ;istring = NIL
                    DC      H'00'           ;merr = 1
                    DC      H'01'           ;merrf = 128
                    DC      H'00'           ;lops = 0
                    DC      H'00'           ;kflag = 0
                    DC      H'00 00 00 00'  ;mflags = 0
                    DC      H'00 00 00 00'  ;pflags = 0
                    DC      H'00 00 00 00'  ;org = 0

          END M2Lib__Globals
