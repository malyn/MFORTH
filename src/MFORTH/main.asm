; Copyright (c) 2009-2010, Michael Alyn Miller <malyn@strangeGizmo.com>.
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:
;
; 1. Redistributions of source code must retain the above copyright notice
;    unmodified, this list of conditions, and the following disclaimer.
; 2. Redistributions in binary form must reproduce the above copyright notice,
;    this list of conditions and the following disclaimer in the documentation
;    and/or other materials provided with the distribution.
; 3. Neither the name of Michael Alyn Miller nor the names of the contributors
;    to this software may be used to endorse or promote products derived from
;    this software without specific prior written permission.
;
; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS "AS IS" AND ANY
; EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
; DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE FOR ANY
; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


; ======================================================================
; MFORTH Version Information
; ======================================================================

MFORTH_MAJOR    .EQU    1
MFORTH_MINOR    .EQU    0
;MFORTH_CHANGE  .EQU    Supplied on the TASM command line.



; ======================================================================
; MFORTH Register Usage
; ======================================================================
;
; HL - Word Pointer (W); only used by the Address Interpreter, available
;      for general use by CODE words.
; DE - Instruction Pointer (IP); must be preserved with SAVEDE/RESTOREDE.
; BC - Return Stack Pointer (RSP); must be preserved if used (usually by
;      pushing BC onto the stack).  B always contains the TASK page.
; SP - Parameter Stack Pointer (PSP).
; A  - Unused.



; ======================================================================
; MFORTH Memory Maps
; ======================================================================
;
; Model 100 Memory Map:
;
;   +--------------------------------+  0000h (32k) or 2000h (24k)
;   |  Files, BASIC variables, etc.  |
;   +--------------------------------+  [FRETOP]
;   |                                |
;   |          Free Memory           |
;   |                                |
;   +--------------------------------+  SP
;   |             Stack              |
;   +--------------------------------+  [STKTOP]
;   |         System variables       |
;   +--------------------------------+  FFFFh
;
;
; MFORTH Memory Map:
;
;   +--------------------------------+  0000h (32k) or 2000h (24k)
;   |  Files, BASIC variables, etc.  |
;   +--------------------------------+  [FRETOP] aka [TICKTIB]
;   |     Terminal Input Buffer      |
;   +--------------------------------+  [TICKTIB] + TIBSIZE aka [DP] aka HERE
;   |           Dictionary           |
;   |  (see below for temp regions)  |
;   |              vvvv              |
;   |                                |
;   |                                |
;   |              ^^^^              |
;   |  Additional Tasks (256b each)  |
;   +--------------------------------+  xx00h (first whole page below BOPSTK)
;   |  Initial MFORTH Task (Task #0) |
;   +--------------------------------+  [BOPSTK]
;   | Original Stack (unused by us)  |
;   +--------------------------------+  [STKTOP]
;   |         System variables       |
;   +--------------------------------+  FFFFh
;
;
; Transient Regions (based on HERE):
;
;   +--------------------------------+  [TICKTIB] + TIBSIZE aka [DP] aka HERE
;   | Empty; space for the header and|
;   | name of the next definition so |
;   | that CREATE can reference the  |
;   | WORD buffer and not clobber it.|
;   +--------------------------------+  [DP] + WORDOFFSET
;   |      Buffer used by WORD       |
;   |              vvvv              |
;   |              ^^^^              |
;   |       Buffer used by HLD       |
;   +--------------------------------+  [DP] + SQOFFSET aka [DP] + HLDEND
;   |           S" buffer            |
;   +--------------------------------+  [DP] + PADOFFSET
;   |     PAD buffer for Task #0     |
;   +--------------------------------+  [DP] + PADOFFSET + tasknum*PADSIZE
;   | Additional per-task PAD buffers|
;   |              vvvv              |
;   +--------------------------------+
;
;
; Task Page (at xx00h in high memory):
;
;   +--------------------------------+  xx00h
;   |         User Variables         |
;   +--------------------------------+  xx40h
;   |              ^^^^              |
;   |          Return Stack          |
;   +--------------------------------+  xx7Fh
;   | Return/Param Stack Guard Cell  |
;   +--------------------------------+  xx81h
;   |              ^^^^              |
;   |        Parameter Stack         |
;   +--------------------------------+  xxFFh
;
;
; Dictionary Header:
;
;   The MFORTH dictionary uses the layout described by Robert L. Smith
;   in his Forth Dimensions I/5 article titled "A Modest Proposal for
;   Dictionary Headers".
;
;   The 00h byte is called the Name Field Address in MFORTH, even though
;   it only points to the name count and flags.  The Link Field Address
;   points to the Name Field Address of the preceding definition.
;
;   The Profiler Execution Count (the dotted cell at 03h) is only used
;   when the profiler is enabled at build time.  That cell is not used,
;   nor is it even allocated (the fixed size of the dictionary header is
;   only three bytes, in other words) when the profiler is disabled.  The
;   Profiler Execution Count stores the number of times that the word
;   was executed by NEXT when the PROFILING variable is non-zero.
;
;   The header below is for the word DUP; note that the word name is
;   stored in reverse order in memory.
;
;   +--------------------------------+ -03h
;   | 1 |          'P'               |
;   +--------------------------------+ -02h
;   | 0 |          'U'               |
;   +--------------------------------+ -01h
;   | 0 |          'D'               |
;   +--------------------------------+  00h
;   | P | S |     Count              |
;   +--------------------------------+  01h
;   |             Link               |
;   |             Field              |
;   +--------------------------------+  03h
;   .            Profiler            .
;   .        Execution Count         .
;   +--------------------------------+  03h/05h
;   |             Code               |
;   |             Field              |
;   |          (JMP $xxxx)           |
;   +--------------------------------+  06h/08h
;   .           Parameter            .
;   .             Field              .
;   . . . . . . . .  . . . . . . . . .



; ======================================================================
; Undocumented 8085 Opcodes
; ======================================================================

;           INSTR   ARGS OPCODE BYTES   MOD CLASS
.ADDINSTR   DSUB    ""   08     1       NOP 1
.ADDINSTR   LDEH    *    28     2       NOP 1
.ADDINSTR   LDES    *    38     2       NOP 1
.ADDINSTR   LHLX    ""   ED     1       NOP 1
.ADDINSTR   RDEL    ""   18     1       NOP 1
.ADDINSTR   SHLX    ""   D9     1       NOP 1



; ======================================================================
; Model 100/102 Constants
; ======================================================================

FRETOP:     .EQU    0FBB6h      ; Beginning of free space on the machine
                                ; (after all files, BASIC vars, etc.)
STKTOP:     .EQU    0F678h      ; Top of the system stack; system stack
                                ; grows downward, although we don't
                                ; use this stack for anything.
ALTBGN:     .EQU    0FCC0h      ; Beginning of the Alternate LCD buffer
ALTEND:     .EQU    0FDFFh      ; End of the Alternate LCD buffer

STACKGUARD: .EQU    076h        ; The guard value used to see if a Task
                                ; has over-/under-flown a stack.  $76
                                ; is "HLT" just in case someone tries
                                ; to execute the stack pointer.



; ======================================================================
; MFORTH Dictionary Constants
; ======================================================================

NFASZ:      .EQU    1           ; Size of the Name Field.
#DEFINE     INXNFATOLFA(r) INX r

LFASZ:      .EQU    2           ; Size of the Link Field.

#IFNDEF PROFILER
NFATOCFASZ: .EQU    NFASZ+LFASZ
#DEFINE     INXNFATOCFA(r) INX r\ INX r\ INX r
#ELSE
NFATOPECSZ: .EQU    NFASZ+LFASZ
PECSZ:      .EQU    2           ; Size of the Profiler Execution Count.
NFATOCFASZ: .EQU    NFASZ+LFASZ+PECSZ
#DEFINE     INXNFATOCFA(r) INX r\ INX r\ INX r\ INX r\ INX r
#ENDIF

CFASZ:      .EQU    3           ; Size of the Code Field.
#DEFINE     INXCFATOPFA(r) INX r\ INX r\ INX r



; ======================================================================
; MFORTH Constants
; ======================================================================

TIBSIZE:    .EQU    80          ; Length of the Terminal Input Buffer.

WORDOFFSET: .EQU    64          ; Offset from HERE to WORD buffer.
WORDSIZE:   .EQU    258         ; Size of WORD buffer.
HLDEND:     .EQU    WORDOFFSET+WORDSIZE ; End (and thus, start) of HLD.
SQOFFSET:   .EQU    HLDEND      ; Offset from HERE to S" buffer.
SQSIZE:     .EQU    80          ; Size of S" buffer.
PADOFFSET:  .EQU    SQOFFSET+SQSIZE ; Offset from HERE to first PAD buffer.
PADSIZE:    .EQU    256         ; Size of each task's PAD buffer.

MAXICBS:    .EQU    8           ; Maximum number of active input sources.
MAXFCBS:    .EQU    8           ; Maximum number of files open at one time.



; ======================================================================
; Global variables (stored in the Alternate LCD buffer)
; Max of 320 bytes.
;
; OPON and the variables used by OPON take up the first 23 bytes.
; That leaves 297 bytes remaining.
;
; These globals are shared across all of the active tasks, so they must
; either be safe (not used from multiple tasks at a time; DP is such a
; variable as we atomically add new dictionary entries without an
; intervening PAUSE) or be defined in the manual as not being usable from
; multiple tasks.  All of the terminal input variables fall into this
; category as we specifically state that only a single terminal/operator
; is supported.
; ======================================================================

OPON        .EQU    ALTBGN +  0

#IF (OPONLEN > 16)
#ECHO "\t*** ERROR: OPON is too large; Global Variables expect OPONLEN<=16 ***\n"
#ENDIF

HOLDH:      .EQU    ALTBGN + 16
HOLDD:      .EQU    ALTBGN + 18
INTH:       .EQU    ALTBGN + 20
INTD:       .EQU    ALTBGN + 22

DP:         .EQU    ALTBGN + 24 ; Data Pointer.
BOPSTK:     .EQU    ALTBGN + 26 ; Initial value of SP on entry to MFORTH.
TICKLATEST: .EQU    ALTBGN + 28 ; Pointer to the latest word added to the dict.
TICKSTATE:  .EQU    ALTBGN + 30 ; Compilation-state flag.
TICKTIB:    .EQU    ALTBGN + 32 ; Terminal Input Buffer pointer.
TICKHLD:    .EQU    ALTBGN + 34 ; Pictured Numeric Output hold pointer.
TICKNUMTASKS:.EQU   ALTBGN + 36 ; Number of tasks
TICKFIRSTTASK:.EQU  ALTBGN + 38 ; Address of the first task (always XX00)
TICKNUMUSERVARS:.EQU ALTBGN+ 40 ; Number of USER variables
SAVED:      .EQU    ALTBGN + 42 ; DE is saved here when used as a temporary.
TICKPREVLEAVE:.EQU  ALTBGN + 44 ; Pointer to the previous LEAVE in a DO..LOOP.
TICKPREVENDB:.EQU   ALTBGN + 46 ; Pointer to the previous ?ENDB in a FORB..NEXTB
TICKICB:    .EQU    ALTBGN + 48 ; Address of the current Input Control Block.
PROFILING:  .EQU    ALTBGN + 50 ; Non-zero if the profiler is on.
TICKTICKS:  .EQU    ALTBGN + 52 ; (double) Number of ticks (4ms) since start.
SAVEB:      .EQU    ALTBGN + 56 ; DE is saved here when used as a temporary.
; 6 bytes free.
ICBSTART:   .EQU    ALTBGN + 64 ; Start of the 8, 8-byte Input Control Blocks.
FCBSTART:   .EQU    ALTBGN + 128; Start of the 8, 8-byte File Control Blocks.
; 128 bytes free.



; ======================================================================
; System-provided user variables
;
; User variable offsets are the absolute offset into the Task Page and
; are not an index or count or anything like that.  In other words, the
; third User Variable is 4 (0, 2, 4) and not 2 (0, 1, 2).
; ======================================================================

USERSAVEDSP:.EQU    0           ; Saved stack pointer
USERBASE:   .EQU    2           ; Number-conversion radix pointer.
USERB:      .EQU    4           ; "B" register
USERBEND:   .EQU    6           ; "B" register end
NUMUSERVARS:.EQU    4           ; Total number of user variables.



; ======================================================================
; MFORTH Macros
; ======================================================================

; ----------------------------------------------------------------------
; Save and restore DE (corrupts HL).

#DEFINE     SAVEDE      XCHG\ SHLD SAVED
#DEFINE     RESTOREDE   LHLD SAVED\ XCHG


; ----------------------------------------------------------------------
; Save and restore BC (corrupts HL).

#DEFINE     SAVEBC      MOV H,B\ MOV L,C\ SHLD SAVEB
#DEFINE     RESTOREBC   LHLD SAVEB\ MOV B,H\ MOV C,L



; ======================================================================
; Interrupt Vectors
; ======================================================================

RST0:       .ORG    00000h
            INX     SP
            INX     SP
            JMP     INIT

RST1:       .ORG    00008h
            RET

RST2:       .ORG    00010h
            RET

RST3:       .ORG    00018h
            RET

RST4:       .ORG    00020h
            RET

; Power down interrupt.
TRAP:       .ORG    00024h
            DI
            JMP     INTRAP

RST5:       .ORG    00028h
            RET

RST55:      .ORG    0002Ch
            DI
            JMP     INT55

RST6:       .ORG    00030h
            JMP     STDCALL

RST65:      .ORG    00034h
            DI
            JMP     INT65

RST7:       .ORG    00038h
            RET

RST75:      .ORG    0003Ch
            DI
            JMP     INT75



; ======================================================================
; The Main ROM tests the two bytes at 40h to see if they contain the
; values "A" and "B".  If so, then the Main ROM creates an Option ROM
; trigger file with the name of the remaining six bytes at 42h-47h.
;
; Many Option ROMs use these eight bytes to contain OPON, but we use a
; much more thorough (and thus slightly too-long) variant of OPON that
; protects OPON from interrupts.
;
; In any event, the "AB" mechanism causes problems if you are using REX,
; as the Main ROM will detect that the ROM has changed (between MFORTH
; and REX) and cold boot the computer.  We obviously do not want this to
; happen, so we install/overwrite the trigger file ourselves during our
; initialization process.
; ======================================================================

            .ORG    00040h
            HLT\ HLT\ HLT\ HLT
            HLT\ HLT\ HLT\ HLT



; ======================================================================
; Interrupt Handlers
; ======================================================================

; Power down interrupt
INTRAP:     CALL    INTCALL     ; Let the Main ROM turn off the computer
            .WORD   00024h      ; ..and handle the wake-up sequence.
            RET

INT55:      CALL    INTCALL
            .WORD   0002Ch
            RET

INT65:      CALL    INTCALL
            .WORD   00034h
            RET

INT75:      CALL    INTCALL
            .WORD   0003Ch
            PUSH    H
            PUSH    PSW
            CALL    INCTICKS
            POP     PSW
            POP     H
            RET
INCTICKS:   LXI     H,TICKTICKS
            INR     M
            RNZ
            INX     H
            INR     M
            RNZ
            INX     H
            INR     M
            RNZ
            INR     M
            RET

        
; ======================================================================
; Option ROM Support Code
; ======================================================================

; ----------------------------------------------------------------------
; STDON: Switches to the Main ROM.
;
; Here is how the stack looks on entry into this call:
;
;   TOS: $0363 (STDCALL: Main ROM code that happens to be "EI; RET")
;   T-1: Main ROM routine to call
;   T-2: RAM address to return to after the call (OPON in AltLCD)
;   T-3: Option ROM address to return to after the call through OPON
;
; This call needs to preserve all registers on entry into the Main ROM.
; Some clever manipulation of the stack is done to ensure that this
; happens.  First, we push A+PSW onto the stack since we will use A for
; manipulating the contents of the Cassette Port register at $FF45.  This
; means that we need the Main ROM to call POP PSW, RET in order to first
; POP PSW before RETurning to the Main ROM routine.  Thankfully the Main ROM
; has those exact two instructions at $26C8.
;
; So we need to get $26C8 onto the stack as the initial return address, but
; to do that we need to use HL.  That means that we have to preserve HL
; without pushing it to the stack (since the stack has to be in a specific
; configuration on entry into Main ROM).  To do that we first push HL to
; the stack, put $26C8 in HL, and then swap the top-of-stack with HL, thus
; putting $26C8 on the stack and restoring HL.
;
; Here is how the stack looks on entry into the Main ROM:
;
;   TOS: $26C8 (Main ROM code that happens to be "POP PSW; RET")
;   T-1: PSW
;   T-2: $0363 (STDCALL: Main ROM code that happens to be "EI; RET")
;   T-3: Main ROM routine to call
;   T-4: RAM address to return to after the call (OPON in AltLCD)
;   T-5: Option ROM address to return to after the call through OPON
;
; The final RET statement is supplied here for posterity, but in fact by
; the point that the PC gets to that location we will be in the Main ROM.
; Thankfully the Main ROM (both M100 and T200) has a RET statement at $008E,
; so STDON is ORGed in such a way as to ensure that the final RET statement
; falls at $008E.

STDON:      .ORG    0007Eh
            PUSH    PSW         ; Preserve A+PSW (which $26C8 POPs).
            PUSH    H           ; Get $26C8 onto the stack
            LXI     H,026C8h    ; ..by swapping the value through HL
            XTHL                ; ..and the top-of-stack.
            LDA     0FF45h      ; Get the saved contents of port $E8,
            ANI     0FEh        ; ..clear the low bit to switch to the Main ROM,
            STA     0FF45h      ; ..updated the saved contents of the port,
            OUT     0E8h        ; ..and then actually modify the port.
STDONRET:   RET                 ; Now in Main ROM; return to $26C8.

#IF (STDONRET != $008E)
#ECHO "\t*** ERROR: STDON is not ORGed properly; RET statement not at $008E ***\n"
#ENDIF


; ----------------------------------------------------------------------
; Switches to the Option ROM after a call to the Main ROM.
;
; Here is how the stack looks on entry into this call:
;
;   TOS: Option ROM address to return to after the call through OPON
;
; INIT copies this routine into RAM so that the OPON routine is available to
; the Main ROM (which is where it is used).  STDON sets up the stack in such
; a way as to ensure that the Main ROM calls this routine after the Main ROM
; call has completed.
;
; This code is interrupt-sensitive, so we disable interrupts until after we
; have properly restore the registers (we only use A+PSW here) and enable
; the Option ROM.

OPONIMG:    DI
            PUSH    PSW
            LDA     0FF45h      ; Get the saved contents of port $E8,
            ORI     001h        ; ..set the low bit to switch to the Option ROM,
            STA     0FF45h      ; ..updated the saved contents of the port,
            OUT     0E8h        ; ..and then actually modify the port.
            POP     PSW         ; Now in Option ROM; restore A+PSW.
            EI
            RET                 ; Return to whatever called STDCALL/INTCALL.
OPONLEN:    .EQU    $-OPONIMG   ; Calculate length of OPON for use by INIT.



; ----------------------------------------------------------------------
; Calls a routine in the Main ROM.
;
; This call needs to preserve all registers on entry into the Main ROM
; and cannot use a register to specify the address of the Main ROM routine
; that is being called.  Instead, the Main ROM address is contained in the
; next two bytes of the instruction stream, which are accessed by popping
; the return address from the stack and dereferencing that pointer.  The
; real return address is then computed by skipping over the Main ROM address
; that was embedded in the instruction stream and pushing that value onto
; the stack.
;
; This entire process is interrupt-sensitive, so we disable interrupts here
; and then have the Main ROM enable interrupts before it calls the Main ROM
; routine that we are trying to invoke.  The Main ROM has the instructions
; "EI; RET" at address $0363, so we push that onto the stack as the initial
; address to return to once in Main ROM.  After that we proceed to RET to
; the Main ROM routine that we want to call, then OPON, and then back into
; the Option ROM code that CALLed STDCALL.
;
; Here is the layout of the stack at the point where we JMP STDON:
;
;   TOS: $0363 (Main ROM code that happens to be "EI; RET")
;   T-1: Main ROM routine to call
;   T-2: RAM address to return to after the call (OPON in AltLCD)
;   T-3: Option ROM address to return to after the call through OPON
;
; That stack is obviously clear of all of those items on return from STDCALL.

STDCALL:    DI

            SHLD    HOLDH       ; Preserve HL
            XCHG                ; ..and DE
            SHLD    HOLDD       ; ..since we will need them as temporaries.

            POP     H           ; Get the STDCALL return address,
            MOV     E,M         ; ..which is actually the address of the
            INX     H           ; ..Main ROM routine that we want to call,
            MOV     D,M         ; ..and put that address in DE.  Then point
            INX     H           ; ..HL at the actual Option ROM return address
            PUSH    H           ; ..and push that address (T-3) to the stack.

            LXI     H,OPON      ; Return from Main ROM through OPON, which we
            PUSH    H           ; ..push onto the stack (T-2).

            PUSH    D           ; Push the Main ROM routine to call (T-1).

            LHLD    HOLDD       ; Restore DE
            XCHG                ; ..and HL
            LHLD    HOLDH       ; ..so that they can be used as Main ROM args.

            PUSH    H           ; Get $0363 onto the stack
            LXI     H,00363h    ; ..by swapping the value through HL
            XTHL                ; ..and the top-of-stack.

            JMP     STDON       ; Jump to STDON to get into Main ROM.


; ----------------------------------------------------------------------
; Calls an interrupt handler routine in the Main ROM.
;
; This code is almost identical to STDCALL except that we do not need to
; disable interrupts and we definitely do not want to re-enable them as
; part of the jump into the Main ROM (which STDCALL accomplishes by pushing
; $0363 onto the stack).
;
; Here is the layout of the stack at the point where we JMP STDON:
;
;   TOS: Main ROM routine to call
;   T-1: RAM address to return to after the call (OPON in AltLCD)
;   T-2: Option ROM address to return to after the call through OPON
;
; That stack is obviously clear of all of those items on return from INTCALL.


INTCALL:    SHLD    INTH        ; Preserve HL
            XCHG                ; ..and DE
            SHLD    INTD        ; ..since we will need them as temporaries.

            POP     H           ; Get the INTCALL return address,
            MOV     E,M         ; ..which is actually the address of the
            INX     H           ; ..Main ROM routine that we want to call,
            MOV     D,M         ; ..and put that address in DE.  Then point
            INX     H           ; ..HL at the actual Option ROM return address
            PUSH    H           ; ..and push that address (T-2) to the stack.

            LXI     H,OPON      ; Return from Main ROM through OPON, which we
            PUSH    H           ; ..push onto the stack (T-1).

            PUSH    D           ; Push the Main ROM routine to call (TOS).

            LHLD    INTD        ; Restore DE
            XCHG                ; ..and HL
            LHLD    INTH        ; ..so that they can be used as Main ROM args.

            JMP     STDON       ; Jump to STDON to get into Main ROM.



; ======================================================================
; MFORTH Initialization
; ======================================================================

; ----------------------------------------------------------------------
; Initialize MFORTH.
;
; This routine is called on a MFORTH cold-start each time that MFORTH is
; activated from the Main ROM Menu.  Note that MFORTH does not maintain
; any state between invocations.
;
; We disable interrupts, copy OPON into high memory, call LNKFIL to ensure
; that all of the cached file start addresses are accurate, set up our memory
; map (by figuring out where free space starts and ends), initialize the
; return stack, and JMP to the MFORTH COLD word.

INIT:       DI

            ; Copy OPON into high memory.
            LXI     D,OPONIMG   ; First source byte in DE
            LXI     H,OPON      ; First target byte in HL
            MVI     B,OPONLEN   ; Number of bytes to copy in B
_init1:     LDAX    D           ; Load value from [DE]
            MOV     M,A         ; Save value to [HL]
            INX     D           ; Next source byte
            INX     H           ; Next target byte
            DCR     B           ; Decrement counter
            JNZ     _init1      ; Keep looping until all bytes are copied

            ; Call LNKFIL to ensure that the file addresses are accurate.
            CALL    STDCALL     ; Call the
            .WORD   02146h      ; .."LNKFIL" routine.
            
            ; Set up our memory map.
            LHLD    FRETOP      ; Store the beginning of free memory to HL
            SHLD    TICKTIB     ; ..and then to the Terminal Input Buffer.
            LXI     B,TIBSIZE   ; Add TIBSIZE
            DAD     B           ; ..to HL
            SHLD    DP          ; ..and then store that in the Data Pointer.
            LXI     H,0         ; Get the location of the
            DAD     SP          ; ..OS stack pointer on entry into MFORTH
            SHLD    BOPSTK      ; ..and store it in in BOPSTK.
            
            ; Initialize LATEST.
            LXI     H,_latest-NFATOCFASZ
            SHLD    TICKLATEST
            
            ; Four USER variables are currently in use (SavedSP, Base, B, Bend).
            LXI     H,NUMUSERVARS
            SHLD    TICKNUMUSERVARS
            
            ; Create the first task, which needs to be in place for us
            ; to enter in to the Inner Interpreter (which needs a return
            ; stack, for example).  The first task begins at the page
            ; that is below the page where the SP currently resides.  This
            ; could put us right below the system stack (if SP is at xx00,
            ; for example), but that's okay because we don't actually use
            ; the system stack for anything (each task has its own stack).
            LXI     H,1
            SHLD    TICKNUMTASKS

            LXI     H,0         ; Get SP
            DAD     SP          ; ..into HL,
            LXI     B,0100h     ; ..then subtract 256
            DSUB                ; ..from HL to get into the middle of the page,
            MVI     L,0         ; ..then clear L to get to the start of the page
            SHLD    TICKFIRSTTASK ;.and then store address of the first task.

            MOV     B,H         ; Put the current task's page into its home in B
            MVI     C,07fh      ; ..and initialize the return stack to xx7F.

            MVI     M,STACKGUARD; Set the Saved Stack Pointer to the guard
            INX     H           ; ..value since this task will be running (not
            MVI     M,STACKGUARD; ..resumed by PAUSE) after the JMP to COLD.

            MVI     L,080h      ; Set HL to the parameter stack guard cell
            MVI     M,STACKGUARD; ..then move the guard value
            INX     H           ; ..into both bytes
            MVI     M,STACKGUARD; ..of that cell.

            MVI     L,0ffh      ; Initialize the SP, which begins at xxFF.
            SPHL
            
            ; Disable the profiler.
            LXI     H,0
            SHLD    PROFILING
            
            ; Reset the tick counter.
            LXI     H,0
            SHLD    TICKTICKS
            SHLD    TICKTICKS+2
            
            ; Enable interrupts and jump to COLD (which never returns).
            EI
            LXI     H,COLD      ; Point W at COLD (needed by ENTER).
            LXI     D,COLD      ; Point IP at COLD (needed by NEXT).
            JMP     COLD



; ======================================================================
; MFORTH Kernel and Supported Wordsets
; ======================================================================

; Kernel routines and macros.
#include "kernel.asm"

; ANS word sets.
LINK_CORE       .EQU    NFATOCFASZ
#include "answords/core.asm"

LINK_COREEXT    .EQU    LAST_CORE
#include "answords/core-ext.asm"

LINK_DOUBLE     .EQU    LAST_COREEXT
#include "answords/double.asm"

LINK_FACILITY   .EQU    LAST_DOUBLE
#include "answords/facility.asm"

LINK_FILE       .EQU    LAST_FACILITY
#include "answords/file.asm"

LINK_STRING     .EQU    LAST_FILE
#include "answords/string.asm"

LINK_TOOLS      .EQU    LAST_STRING
#include "answords/tools.asm"

LINK_TOOLSEXT   .EQU    LAST_TOOLS
#include "answords/tools-ext.asm"

; MFORTH word sets.
LINK_BREG       .EQU    LAST_TOOLSEXT
#include "mforthwords/breg.asm"

LINK_MFORTH     .EQU    LAST_BREG
#include "mforthwords/mforth.asm"

LINK_TASK       .EQU    LAST_MFORTH
#include "mforthwords/task.asm"

#IFNDEF PROFILER
_latest         .EQU    LAST_TASK
#ELSE
LINK_PROFILER   .EQU    LAST_TASK
#include "mforthwords/profiler.asm"

_latest         .EQU    LAST_PROFILER
#ENDIF



; ======================================================================
; Display ROM statistics.
; ======================================================================

#ECHO "\tEnd of ROM: "
#ECHO $
#ECHO "\n"



; ======================================================================
; Zero-out the rest of the ROM.
; ======================================================================

            .ORG    07FFFh
            .BYTE   0



; ======================================================================
; Perfect Hash of ROM Dictionary
; ======================================================================

#IFNDEF PHASH
; Temporarily store _latest at 07FFE for use by phashgen.exe.
            .ORG    07FFEh
            .WORD   _latest-NFATOCFASZ
#ELSE
; phash.asm already generated.
#include "phash.asm"
#ENDIF



; ======================================================================
; End of ROM.
; ======================================================================

            .END
