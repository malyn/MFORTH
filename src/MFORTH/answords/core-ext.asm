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
; CORE EXT Words
; ======================================================================

; ----------------------------------------------------------------------
; 0<> [CORE EXT] 6.2.0260 "zero-not-equals" ( x -- flag )
;
; flag is true if and only if x is not equal to zero.

            LINKTO(LINK_COREEXT,0,3,'>',"<0")
ZERONOTEQUALS:POP   H           ; Pop the value.
            MOV     A,H         ; See if the flag is zero by moving H to A
            ORA     L           ; ..and then ORing A with L.
            JZ      _zneqFALSE  ; Jump if zero to where we push false.
            LXI     H,0FFFFh    ; Put true in HL.
            JMP     _zneqDONE   ; We're done.
_zneqFALSE: LXI     H,0         ; Put false in HL.
_zneqDONE:  PUSH    H           ; Push the flag to the stack.
            NEXT


; ----------------------------------------------------------------------
; 2R> [CORE EXT] 6.2.0410 "two-r-from"
;
; Interpretation:
;   Interpretation semantics for this word are undefined.
;
; Execution: ( -- x1 x2 ) ( R: x1 x2 -- )
;   Transfer cell pair x1 x2 from the return stack.  Semantically equivalent
;   to R> R> SWAP.

            LINKTO(ZERONOTEQUALS,0,3,'>',"R2")
TWORFROM:   RSPOP(H,L)          ; Pop x2 from the return stack
            PUSH    H           ; ..and push it to the stack (which is wrong).
            RSPOP(H,L)          ; Pop x1 from the return stack,
            XTHL                ; ..then swap x2 and x1 to fix things up,
            PUSH    H           ; ..and finally push x2 back onto the stack.
            NEXT


; ----------------------------------------------------------------------
; <> [CORE EXT] 6.2.0500 "not-equals" ( x1 x2 -- flag )
;
; flag is true if and only if x1 is not bit-for-bit the same as x2.

            LINKTO(TWORFROM,0,2,'>',"<")
NOTEQUALS:  SAVEDE
            POP     H           ; Pop x2.
            POP     D           ; Pop x1.
            PUSH    B           ; Save BC.
            MOV     B,D         ; Move x1
            MOV     C,E         ; ..to BC.
            DSUB                ; HL=HL-BC
            POP     B           ; Restore BC.
            JZ      _neqFALSE   ; Jump if zero (equals) to where we push false.
            LXI     H,0FFFFh    ; Put true in HL.
            JMP     _neqDONE    ; We're done.
_neqFALSE:  LXI     H,0         ; Put false in HL.
_neqDONE:   PUSH    H           ; Push the flag to the stack.
            RESTOREDE
            NEXT


; ----------------------------------------------------------------------
; AGAIN [CORE EXT] 6.2.0700
;
; Compilation: ( C: dest -- )
;   Append the run-time semantics given below to the current definition,
;   resolving the backward reference dest.
;
; Run-time: ( -- )
;   Continue execution at the location specified by dest.  If no other
;   control flow words are used, any program code after AGAIN will not
;   be executed.
;
; : AGAIN   ['] branch COMPILE,  , ; IMMEDIATE

            LINKTO(NOTEQUALS,1,5,'N',"IAGA")
AGAIN:      JMP     ENTER
            .WORD   LIT,branch,COMPILECOMMA,COMMA,EXIT


; ----------------------------------------------------------------------
; COMPILE, [CORE EXT] 6.2.0945 "compile-comma" ( xt -- )
;
; Interpretation:
;   Interpretation semantics for this word are undefined.
;
; Execution: ( xt -- )
;   Append the execution semantics of the definition represented by xt to
;   the execution semantics of the current definition.  

            LINKTO(AGAIN,0,8,02Ch,"ELIPMOC")
COMPILECOMMA:JMP    ENTER
            .WORD   COMMA,EXIT


; ----------------------------------------------------------------------
; HEX [CORE EXT] 6.2.1660 ( -- )
;
; Set contents of BASE to sixteen.

            LINKTO(COMPILECOMMA,0,3,'X',"EH")
HEX:        JMP     ENTER
            .WORD   LIT,16,BASE,STORE,EXIT


; ----------------------------------------------------------------------
; NIP [CORE EXT] 6.2.1930 ( x1 x2 -- x2 )
;
; Drop the first item below the top of stack.

            LINKTO(HEX,0,3,'P',"IN")
NIP:        POP     H           ; Pop x2 into HL.
            POP     PSW         ; Pop x1 into A+PSW.
            PUSH    H           ; Push x2 back onto the stack.
            NEXT


; ----------------------------------------------------------------------
; PAD [CORE EXT] 6.2.2000 ( -- c-addr )
;
; c-addr is the address of a transient region that can be used to hold
; data for intermediate processing.
;
; : PAD ( -- c-addr)   HERE  PADOFFSET +  TASK 'FIRSTTASK @ -  8 LSHIFT  + ;

            LINKTO(NIP,0,3,'D',"AP")
PAD:        PUSH    D           ; Save DE.
            LHLD    DP          ; Get HERE into HL.
            LXI     D,PADOFFSET ; Get the base PAD offset into DE
            DAD     D           ; ..and add the offset to HL.
            XCHG                ; Save HL in DE.
            LHLD    TICKFIRSTTASK;Get the address of the first task,
            MOV     A,H         ; ..move the page address into A,
            SUB     B           ; ..then calculate the task number (first-B).
            XCHG                ; Get HERE+PADOFFSET back into HL.
            MOV     D,A         ; Set DE to the
            MVI     E,0         ; ..task number * 256,
            DAD     D           ; ..and add that value to HL.
            XTHL                ; Swap PAD with IP,
            XCHG                ; ..and put IP back into DE.
            NEXT


; ----------------------------------------------------------------------
; PICK [CORE EXT] 6.2.2030 ( xu ... x1 x0 u -- xu ... x1 x0 xu )
;
; Remove u.  Copy the xu to the top of the stack.  An ambiguous condition
; exists if there are less than u+2 items on the stack before PICK is
; executed.

            LINKTO(PAD,0,4,'K',"CIP")
PICK:       POP     H           ; Get u into HL,
            DAD     H           ; ..double the value to get a cell offset,
            DAD     SP          ; ..then add SP to get the stack offset.
            MOV     A,M         ; Get the low byte of the stack value in A,
            INX     H           ; ..then increment to the high byte,
            MOV     H,M         ; ..get the high byte into H,
            MOV     L,A         ; ..move the low byte into L,
            PUSH    H           ; ..and push xu to the stack.
            NEXT


; ----------------------------------------------------------------------
; SOURCE-ID [CORE EXT] 6.2.2218 "source-i-d" ( -- 0 | -1 )
;
; Identifies the input source as follows:
;
;   =================================
;   SOURCE-ID	Input source
;   ---------------------------------
;    fileid     Text file "fileid"
;      -1       String (via EVALUATE)
;       0       User input device
;   =================================
;
; ---
; : SOURCE-ID ( -- 0 | -1)   ICB ICBSOURCEID + @ ;

            LINKTO(PICK,0,9,'D',"I-ECRUOS")
SOURCEID:   JMP     ENTER
            .WORD   ICB,LIT,ICBSOURCEID,PLUS,FETCH,EXIT


; ----------------------------------------------------------------------
; TIB [CORE EXT] 6.2.2290 "t-i-b" ( -- c-addr )
;
; c-addr is the address of the terminal input buffer.
;
; Note: This word is obsolescent and is included as a concession to
; existing implementations.

            LINKTO(SOURCEID,0,3,'B',"IT")
TIB:        LHLD    TICKTIB
            PUSH    H
            NEXT


; ----------------------------------------------------------------------
; TUCK [CORE EXT] 6.2.2300 ( x1 x2 -- x2 x1 x2 )
;
; Copy the first (top) stack item below the second stack item.

            LINKTO(TIB,0,4,'K',"CUT")
LAST_COREEXT:
TUCK:       SAVEDE
            POP     D           ; Pop x2 into DE.
            POP     H           ; Pop x1 into HL.
            PUSH    D           ; Push x2 onto the stack.
            PUSH    H           ; Push x1 onto the stack.
            PUSH    D           ; Push x2 onto the stack again.
            RESTOREDE
            NEXT
