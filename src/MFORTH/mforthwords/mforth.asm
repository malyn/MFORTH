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
; Display the MFORTH version as a string of 15 characters ("MFORTH 1.0.0000").
;
; : .VER ( --)
;   ." MFORTH v"
;   MFORTH_MAJOR [CHAR] 0 + EMIT  [CHAR] . EMIT
;   MFORTH_MINOR [CHAR] 0 + EMIT  [CHAR] . EMIT
;   BASE @  HEX  MFORTH_CHANGE 0 <# # # # # #> TYPE  BASE ! ;

            LINKTO(DASHROT,0,4,'R',"EV.")
DOTVER:     JMP     ENTER
            .WORD   PSQUOTE,7
            .BYTE   "MFORTH "
            .WORD   TYPE
            .WORD   LIT,MFORTH_MAJOR,LIT,'0',PLUS,EMIT,LIT,'.',EMIT
            .WORD   LIT,MFORTH_MINOR,LIT,'0',PLUS,EMIT,LIT,'.',EMIT
            .WORD   BASE,FETCH,HEX,LIT,MFORTH_CHANGE,ZERO
            .WORD   LESSNUMSIGN,NUMSIGN,NUMSIGN,NUMSIGN,NUMSIGN,NUMSIGNGRTR
            .WORD   TYPE,BASE,STORE,EXIT


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
; COLD [MFORTH] ( i*x -- ) ( R: j*x -- )
;
; Clear the screen, display our copyright/help message, (re)insert our ROM
; trigger file, initialize our File Control Blocks, then jump to ABORT (which
; clears the stack and calls QUIT, which clears the return stack and enters
; the infinite text interpreter loop).
;
; : COLD ( i*x --; R: j*x --)
;   PAGE  .VER 3 SPACES ." (C)Michael Alyn Miller"
;   INS-ROMTRIG INIT-FCBS ABORT ;
;
; ABORT should never return, but we HALT anyway just in case someone
; messes with the return stack.

            LINKTO(TWONIP,0,4,'D',"LOC")
COLD:       JMP     ENTER
            .WORD   PAGE,DOTVER,LIT,3,SPACES
            .WORD   PSQUOTE,22
            .BYTE   "(C)Michael Alyn Miller"
            .WORD   TYPE,INSROMTRIG,INITFCBS,ABORT
            .WORD   HALT


; ----------------------------------------------------------------------
; COPY [MFORTH] ( addr1 addr2 u -- u )
;
; If u is greater than zero, copy the contents of u consecutive address
; units at addr1 to the u consecutive address units at addr2.  After COPY
; completes, the u consecutive address units at addr2 contain exactly what
; the u consecutive address units at addr1 contained before the move.  u
; is the number of address units that were copied.
;
; : COPY ( addr1 addr2 u -- u)   DUP >R MOVE R> ;

            LINKTO(COLD,0,4,'Y',"POC")
COPY:       JMP     ENTER
            .WORD   DUP,TOR,MOVE,RFROM,EXIT


; ----------------------------------------------------------------------
; HALT [MFORTH] ( -- )
;
; Halt the processor.

            LINKTO(COPY,0,4,'T',"LAH")
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
; : INS-ROMTRIG ( --)
;   FIND-ROMTRIG  DUP 0= IF FREDIR THEN  >B
;   240 B!+  255 B!+ 255 B!+  S" MFORTH" B SWAP MOVE  BL B!+ BL B!+ ;
; : FIND-ROMTRIG ( -- 0 | addr)
;   [ USRDIR 11 - ] LITERAL  BEGIN NXTDIR DUP WHILE
;       DUP C@ 16 AND IF EXIT THEN REPEAT ;

            LINKTO(INITRP,0,11,'G',"IRTMOR-SNI")
INSROMTRIG: JMP     ENTER
            .WORD   FINDROMTRIG,DUP,ZEROEQUALS,zbranch,_insromtrig1,FREDIR
_insromtrig1:.WORD  TOB,LIT,240,BSTOREPLUS,LIT,255,BSTOREPLUS,LIT,255,BSTOREPLUS
            .WORD   PSQUOTE,6
            .BYTE   "MFORTH"
            .WORD   B,SWAP,MOVE,BL,BSTOREPLUS,BL,BSTOREPLUS
            .WORD   EXIT

FINDROMTRIG:JMP     ENTER
            .WORD   LIT,0F9BAh-11
_findromtrig1:.WORD NXTDIR,DUP,zbranch,_findromtrig3
            .WORD   DUP,CFETCH,LIT,16,AND,zbranch,_findromtrig2
            .WORD   EXIT
_findromtrig2:.WORD branch,_findromtrig1
_findromtrig3:.WORD EXIT

NXTDIR:     POP     H           ; Get the entry prior to the start position.
            CALL    STDCALL     ; Call the
            .WORD   020D5h      ; .."NXTDIR" routine.
            JZ      _nxtdirZERO ; Jump if zero to where we push zero/not found.
            JMP     _nxtdirFOUND; We're done.
_nxtdirZERO:LXI     H,0         ; Put zero in HL.
_nxtdirFOUND:PUSH   H           ; Push the location (or zero) to the stack.
            NEXT

FREDIR:     PUSH    B           ; Save BC (corrupted by FREDIR).
            CALL    STDCALL     ; Call the
            .WORD   020ECh      ; .."FREDIR" routine.
            POP     B           ; Restore BC.
            PUSH    H           ; Push the location of the free entry.
            NEXT



; ----------------------------------------------------------------------
; SP [MFORTH] ( -- a-addr )
;
; a-addr is the value of the stack pointer before a-addr was placed on
; the stack.

            LINKTO(INSROMTRIG,0,2,'P',"S")
SP:         LXI     H,0
            DAD     SP
            PUSH    H
            NEXT


; ----------------------------------------------------------------------
; SP! [MFORTH] ( i*x a-addr -- )
;
; Set the stack pointer to a-addr.

            LINKTO(SP,0,3,'!',"PS")
LAST_MFORTH:
SPSTORE:    POP     H
            SPHL
            NEXT
