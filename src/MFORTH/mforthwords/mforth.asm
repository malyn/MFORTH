; Copyright (c) 2009-2012, Michael Alyn Miller <malyn@strangeGizmo.com>.
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
; MFORTH Words
; ======================================================================

; ----------------------------------------------------------------------
; -ROT [MFORTH] "dash-rote" ( x1 x2 x3 -- x3 x1 x2 )
;
; Reverse-rotate the top three stack entries.

            LINKTO(LINK_MFORTH,0,4,'T',"OR-")
DASHROT:    SAVEDE
            POP     H           ; Pop x3 into HL.
            POP     D           ; Pop x2 into DE.
            XTHL                ; Swap TOS (x1) with HL (x3).
            PUSH    H           ; Push x1 back onto the stack.
            PUSH    D           ; Push x2 back onto the stack.
            RESTOREDE
            NEXT


; ----------------------------------------------------------------------
; .VER [MFORTH] "dot-ver" ( -- )
;
; Display the MFORTH version as a string of 16 characters ("MFORTH 1.0.0000 ").
; The space in the 16th position will be replaced with a "P" if this is a
; build that includes the profiler.
;
; ---
; : .VER ( --)
;   ." MFORTH "
;   MFORTH_MAJOR [CHAR] 0 + EMIT  [CHAR] . EMIT
;   MFORTH_MINOR [CHAR] 0 + EMIT  [CHAR] . EMIT
;   S" <<MFORTH_COMMIT>>" DROP 4 TYPE
;   [ PROFILER ] [IF] [CHAR] P EMIT [ELSE] SPACE [THEN] ;

            LINKTO(DASHROT,0,4,'R',"EV.")
DOTVER:     JMP     ENTER
            .WORD   PSQUOTE,7
            .BYTE   "MFORTH "
            .WORD   TYPE
            .WORD   LIT,MFORTH_MAJOR,LIT,'0',PLUS,EMIT,LIT,'.',EMIT
            .WORD   LIT,MFORTH_MINOR,LIT,'0',PLUS,EMIT,LIT,'.',EMIT
            .WORD   PSQUOTE,7
            .BYTE   MFORTH_COMMIT
            .WORD   DROP,LIT,4,TYPE
#IFDEF PROFILER
            .WORD   LIT,'P',EMIT
#ELSE
            .WORD   SPACE
#ENDIF
            .WORD   EXIT


; ----------------------------------------------------------------------
; 2NIP [MFORTH] "two-nip" ( x1 x2 x3 x4 -- x3 x4 )
;
; Drop the first cell pair below the cell pair at the top of the stock.

            LINKTO(DOTVER,0,4,'P',"IN2")
TWONIP:     SAVEDE
            POP     H           ; Pop x4.
            POP     D           ; Pop x3.
            POP     PSW         ; Pop x2.
            POP     PSW         ; Pop x1.
            PUSH    D           ; Push x3 back onto the stack.
            PUSH    H           ; Push x4 back onto the stack.
            RESTOREDE
            NEXT


; ----------------------------------------------------------------------
; 8* [MFORTH] "eight-star" ( x1 -- x2 )
;
; x2 is the result of shifting x1 three bits toward the most-significant
; bit, filling the vacated least-significant bit with zero.

            LINKTO(TWONIP,0,2,'*',"8")
EIGHTSTAR:  POP     H           ; Pop x1.
            DAD     H           ; Shift
            DAD     H           ; ..x1
            DAD     H           ; ..three times.
            PUSH    H           ; Push the result onto the stack.
            NEXT


; ----------------------------------------------------------------------
; GET-XY [MFORTH] "get-x-y" ( -- u1 u2 )
;
; Return the current cursor position (column u1, row u2) from the
; current input device, the upper left corner of which is column zero,
; row zero.

            LINKTO(EIGHTSTAR,0,6,'Y',"X-TEG")
GETXY:      MVI     H,0         ; Initialize H with zero.
            LDA     0F63Ah      ; Get the column into A,
            DCR     A           ; ..subtract one,
            MOV     L,A         ; ..move it to L,
            PUSH    H           ; ..and push the result to the stack.
            LDA     0F639h      ; Get the row into A,
            DCR     A           ; ..subtract one,
            MOV     L,A         ; ..move it to L,
            PUSH    H           ; ..and push the result to the stack.
            NEXT


; ----------------------------------------------------------------------
; COLD [MFORTH] ( i*x -- ) ( R: j*x -- )
;
; Clear the screen, display our copyright/help message, (re)insert our ROM
; trigger file, initialize our File Control Blocks, then jump to ABORT (which
; clears the stack and calls QUIT, which clears the return stack and enters
; the infinite text interpreter loop).
;
; ---
; : COLD ( i*x --; R: j*x --)
;   PAGE  .VER 2 SPACES ." (C)Michael Alyn Miller"
;   INS-ROMTRIG INIT-FCBS ABORT ;
;
; ABORT should never return, but we HALT anyway just in case someone
; messes with the return stack.

            LINKTO(GETXY,0,4,'D',"LOC")
COLD:       JMP     ENTER
            .WORD   PAGE,DOTVER,LIT,2,SPACES
            .WORD   PSQUOTE,22
            .BYTE   "(C)Michael Alyn Miller"
            .WORD   TYPE,INSROMTRIG,INITFCBS,ABORT
            .WORD   HALT


; ----------------------------------------------------------------------
; COPY-LINE [MFORTH] ( addr1 addr2 u1 -- u2 u3 )
;
; If u1 is greater than zero, copy the contents of u1 consecutive address
; units at addr1 to the u1 consecutive address units at addr2, stopping if
; a CRLF sequence is found or end-of-file is reached before u1 address units
; have been copied.  After COPY-LINE completes, the u2 consecutive address
; units at addr2 contain exactly what the u2 consecutive address units at
; addr1 contained before the move.  u2 is the number of address units that
; were copied.  u3 is the number of address units that should be skipped
; in addr1 before the next call to COPY-LINE.  u3 will normally be u2+2 if
; CRLF was reached before u1 or EOF was reached.  Note that EOF is not
; included in u3, only CRLF is included in u3.
;
; ---
; : COPY-LINE ( addr1 addr2 u1 -- u2 u3)
;   ROT SWAP 2>B DUP ( addr2 addr2') FORB
;   B@ 26 = ?ENDB
;   B@ 13 = B# 1 > AND IF B 1+ C@ 10 = IF SWAP - DUP 1+ 1+ EXIT THEN THEN
;   B@ OVER C! 1+ NEXTB
;   SWAP - DUP ;

            LINKTO(COLD,0,9,'E',"NIL-YPOC")
COPYLINE:   JMP     ENTER
            .WORD   ROT,SWAP,TWOTOB,DUP
_copyline1: .WORD   BQUES,zbranch,_copyline3
            .WORD   BFETCH,LIT,26,EQUALS,INVERT,zbranch,_copyline3
            .WORD   BFETCH,LIT,13,EQUALS,BNUMBER,ONE,GREATERTHAN,AND
            .WORD       zbranch,_copyline2
            .WORD   B,ONEPLUS,CFETCH,LIT,10,EQUALS,zbranch,_copyline2
            .WORD   SWAP,MINUS,DUP,ONEPLUS,ONEPLUS,EXIT
_copyline2: .WORD   BFETCH,OVER,CSTORE,ONEPLUS,BPLUS,branch,_copyline1
_copyline3: .WORD   SWAP,MINUS,DUP,EXIT


; ----------------------------------------------------------------------
; HALT [MFORTH] ( -- )
;
; Halt the processor.

            LINKTO(COPYLINE,0,4,'T',"LAH")
HALT:       HLT                 ; Halt the processor.
            NEXT


; ----------------------------------------------------------------------
; INITRP [MFORTH] "init-r-p" ( -- ) ( R:  i*x -- )
;
; Empty the return stack.

            LINKTO(HALT,0,6,'P',"RTINI")
INITRP:     MVI     C,07Fh
            NEXT


; ----------------------------------------------------------------------
; INS-ROMTRIG [MFORTH] ( -- )
;
; Insert our ROM Trigger file, replacing an existing ROM Trigger file with
; our own if one is found.
;
; ---
; : INS-ROMTRIG ( --)
;   FIND-ROMTRIG  DUP 0= IF FREDIR THEN  >B
;   240 B!+  255 B!+ 255 B!+  S" MFORTH" B SWAP  DUP B + >B  MOVE
;   BL B!+ BL B!+ ;
; : FIND-ROMTRIG ( -- 0 | addr)
;   [ USRDIR 11 - ] LITERAL  BEGIN NXTDIR DUP WHILE
;       DUP C@ 16 AND IF EXIT THEN REPEAT ;

            LINKTO(INITRP,0,11,'G',"IRTMOR-SNI")
INSROMTRIG: JMP     ENTER
            .WORD   FINDROMTRIG,DUP,ZEROEQUALS,zbranch,_insromtrig1,FREDIR
_insromtrig1:.WORD  TOB,LIT,240,BSTOREPLUS,LIT,255,BSTOREPLUS,LIT,255,BSTOREPLUS
            .WORD   PSQUOTE,6
            .BYTE   "MFORTH"
            .WORD   B,SWAP,DUP,B,PLUS,TOB,MOVE,BL,BSTOREPLUS,BL,BSTOREPLUS
            .WORD   EXIT

            LINKTO(INSROMTRIG,0,12,'G',"IRTMOR-DNIF")
FINDROMTRIG:JMP     ENTER
            .WORD   LIT,0F9BAh-11
_findromtrig1:.WORD NXTDIR,DUP,zbranch,_findromtrig3
            .WORD   DUP,CFETCH,LIT,16,AND,zbranch,_findromtrig2
            .WORD   EXIT
_findromtrig2:.WORD branch,_findromtrig1
_findromtrig3:.WORD EXIT

            LINKTO(FINDROMTRIG,0,6,'R',"IDTXN")
NXTDIR:     POP     H           ; Get the entry prior to the start position.
            CALL    STDCALL     ; Call the
            .WORD   020D5h      ; .."NXTDIR" routine.
            JZ      _nxtdirZERO ; Jump if zero to where we push zero/not found.
            JMP     _nxtdirFOUND; We're done.
_nxtdirZERO:LXI     H,0         ; Put zero in HL.
_nxtdirFOUND:PUSH   H           ; Push the location (or zero) to the stack.
            NEXT

            LINKTO(NXTDIR,0,6,'R',"IDERF")
FREDIR:     PUSH    B           ; Save BC (corrupted by FREDIR).
            CALL    STDCALL     ; Call the
            .WORD   020ECh      ; .."FREDIR" routine.
            POP     B           ; Restore BC.
            PUSH    H           ; Push the location of the free entry.
            NEXT



; ----------------------------------------------------------------------
; LCD [MFORTH] "l-c-d" ( -- )
;
; Select the LCD display as the output device.

            LINKTO(FREDIR,0,3,'D',"CL")
LCD:        CALL    STDCALL     ; Call the
            .WORD   04B92h      ; .."Reinitialize back to LCD" routine.
            NEXT


; ----------------------------------------------------------------------
; PARSE-WORD [MFORTH] ( "<spaces>name<space>" -- c-addr u )
;
; Skip leading spaces and parse name delimited by a space. c-addr is the
; address within the input buffer and u is the length of the selected
; string. If the parse area is empty, the resulting string has a zero length. 
;
; ---
; : PARSE-WORD ( "<spaces>name<space>" -- c-addr u) TRUE BL (parse) ;

            LINKTO(LCD,0,10,'D',"ROW-ESRAP")
PARSEWORD:  JMP     ENTER
            .WORD   TRUE,BL,PPARSE,EXIT


; ----------------------------------------------------------------------
; PRN [MFORTH] "p-r-n" ( -- )
;
; Select the printer as the output device.

            LINKTO(PARSEWORD,0,3,'N',"RP")
PRN:        JMP     ENTER
            .WORD   ONE,LIT,0F675h,STORE,EXIT


; ----------------------------------------------------------------------
; SP [MFORTH] ( -- a-addr )
;
; a-addr is the value of the stack pointer before a-addr was placed on
; the stack.

            LINKTO(PRN,0,2,'P',"S")
SP:         LXI     H,0
            DAD     SP
            PUSH    H
            NEXT


; ----------------------------------------------------------------------
; SP! [MFORTH] ( i*x a-addr -- )
;
; Set the stack pointer to a-addr.

            LINKTO(SP,0,3,'!',"PS")
SPSTORE:    POP     H
            SPHL
            NEXT


; ----------------------------------------------------------------------
; TICKS [MFORTH] ( -- ud )
;
; ud is the number of ticks that have elapsed since MFORTH was started.

            LINKTO(SPSTORE,0,5,'S',"KCIT")
TICKS:      DI
            LHLD    TICKTICKS
            PUSH    H
            LHLD    TICKTICKS+2
            EI
            PUSH    H
            NEXT


; ----------------------------------------------------------------------
; TICKS>MS [MFORTH] "ticks-to-m-s" ( ud1 -- ud2 )
;
; Convert a tick count (ud1) to a value in milliseconds (ud2).
;
; ---
; : TICKS>MS ( ud1 -- ud2)   D2* D2* ;

            LINKTO(TICKS,0,8,'S',"M>SKCIT")
TICKSTOMS:  JMP     ENTER
            .WORD   DTWOSTAR,DTWOSTAR,EXIT


; ----------------------------------------------------------------------
; TIMED-EXECUTE [MFORTH] ( i*x xt -- j*x ud )
;
; Execute the given xt and return the approximate number of milliseconds
; required for execution.
;
; ---
; : TIMED-EXECUTE ( i*x xt -- j*x ud)
;   TICKS 2>R  EXECUTE  TICKS 2R>  D- ;

            LINKTO(TICKSTOMS,0,13,'E',"TUCEXE-DEMIT")
TIMEDEXECUTE:JMP    ENTER
            .WORD   TICKS,TWOTOR,EXECUTE,TICKS,TWORFROM,DMINUS,EXIT


; ----------------------------------------------------------------------
; VOCABULARY [MFORTH] ( "<spaces>name" -- )
;
; Skip leading space delimiters.  Parse name delimited by a space.  Create
; a definition for name with the execution semantics defined below.
;
; name is referred to as a "word list".
;
; name Execution: ( -- )
;   Replace the first word list in the search order with name.
;
; ---
; : VOCABULARY ( "<spaces>name" -- )
;   CREATE WORDLIST DOES> SOESTART ! ;

            LINKTO(TIMEDEXECUTE,0,10,'Y',"RALUBACOV")
VOCABULARY: JMP     ENTER
            .WORD   CREATE,LIT,-CFASZ,ALLOT,LIT,195,CCOMMA,LIT,pvocabulary,COMMA
            .WORD   WORDLIST
            .WORD   EXIT
pvocabulary:CALL    DODOES
            .WORD   LIT,SOESTART,STORE,EXIT


; ----------------------------------------------------------------------
; [HEX] [MFORTH] "bracket-hex"
;
; Interpretation:
;   Interpretation semantics for this word are undefined.
;
; Compilation: ( "<spaces>hexnum" -- )
;   Skip leading space delimiters.  Parse hexnum, a base 16 number
;   delimited by a space.  Append the run-time semantics given below to
;   the current definition.
;
; Run-time: ( -- u )
;   Place u, the value of hexnum, on the stack.
;
; ---
; : [HEX] ( "<spaces>name" -- u)
;   BASE @  HEX PARSE-WORD  ( savedbase ca u)
;   NUMBER? IF ['] LIT COMPILE, , BASE ! EXIT THEN
;   ABORT" Not a hex number" ; IMMEDIATE

            LINKTO(VOCABULARY,1,5,']',"XEH[")
LAST_MFORTH:
BRACKETHEX: JMP     ENTER
            .WORD   BASE,FETCH,HEX,PARSEWORD
            .WORD   NUMBERQ,zbranch,_brackethex1
            .WORD   LIT,LIT,COMPILECOMMA,COMMA,BASE,STORE,EXIT
_brackethex1:.WORD  PSQUOTE,16
            .BYTE   "Not a hex number"
            .WORD   TYPE,ABORT
