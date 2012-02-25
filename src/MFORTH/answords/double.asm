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
; DOUBLE Words
; ======================================================================

; ----------------------------------------------------------------------
; D. [DOUBLE] 8.6.1.0160 "d-dot" ( d -- )
;
; Display d in free field format.
;
; : D. ( d -- )
;   BASE @ 10 <>  IF UD. EXIT THEN
;   2DUP D0< >R DABS <# #S R> SIGN #> TYPE SPACE ;

            LINKTO(LINK_DOUBLE,0,2,'.',"D")
DDOT:       JMP     ENTER
            .WORD   BASE,FETCH,LIT,10,NOTEQUALS,zbranch,_ddot1,UDDOT,EXIT
_ddot1:     .WORD   TWODUP,DZEROLESS,TOR
            .WORD   DABS,LESSNUMSIGN,NUMSIGNS,RFROM,SIGN,NUMSIGNGRTR
            .WORD   TYPE,SPACE
            .WORD   EXIT


; ----------------------------------------------------------------------
; D- [DOUBLE] 8.6.1.1050 "d-minus" ( d1|ud1 d2|ud2 -- d3|ud3 )
;
; Subtract d2|ud2 from d1|ud1, giving the difference d3|ud3.

            LINKTO(DDOT,0,2,'-',"D")
DMINUS:     SAVEDE
            LDES    0           ; Get the address of d2
            XCHG                ; ..and move that address into HL
            LDES    4           ; Get the address of d1 into DE.
            ANA     A           ; Clear the carry flag.
            LDAX    D           ; Get d1ll into A,
            SUB     M           ; ..subtract d2ll from d1ll,
            STAX    D           ; ..and put the result into d1ll.
            INX     D           ; Increment to d1lh.
            INX     H           ; Increment to d2lh.
            LDAX    D           ; Get d1lh into A,
            SBB     M           ; ..subtract d2lh from d1l,
            STAX    D           ; ..and put the result into d1lh.
            INX     D           ; Increment to d1hl.
            INX     H           ; Increment to d2hl.
            LDAX    D           ; Get d1hl into A,
            SBB     M           ; ..subtract d2hl from d1hl,
            STAX    D           ; ..and put the result into d1hl.
            INX     D           ; Increment to d1hh.
            INX     H           ; Increment to d2hh.
            LDAX    D           ; Get d1hh into A,
            SBB     M           ; ..subtract d2hh from d1hh,
            STAX    D           ; ..and put the result into d1hh.
            POP     H           ; Pop d2l.
            POP     H           ; Pop d2h.
            RESTOREDE
            NEXT


; ----------------------------------------------------------------------
; D0< [DOUBLE] 8.6.1.1075 "d-zero-less" ( d -- flag )
;
; flag is true if and only if d is less than zero.
;
; : D0< ( d -- flag)   SWAP DROP 0< ;

            LINKTO(DMINUS,0,3,'<',"0D")
DZEROLESS:  JMP     ENTER
            .WORD   SWAP,DROP,ZEROLESS,EXIT


; ----------------------------------------------------------------------
; D2* [DOUBLE] 8.6.1.1090 "d-two-star" ( xd1 -- xd2 )
;
; xd2 is the result of shifting xd1 one bit toward the most-significant
; bit, filling the vacated least-significant bit with zero.

            LINKTO(DZEROLESS,0,3,'*',"2D")
DTWOSTAR:   POP     H           ; Pop xd1h,
            XTHL                ; ..then swap it for xd1l.
            DAD     H           ; Double xd1l.
            XTHL                ; Swap xd2l with xd1h.
            JNC     _dtwostar1  ; No carry?  Then just double xd1h,
            DAD     H           ; ..otherwise double xd1h
            INX     H           ; ..and then propagate the carry.
            JMP     _dwostarDONE; We're done.
_dtwostar1: DAD     H           ; No carry bit, so just double xd1h.
_dwostarDONE:PUSH    H          ; Push xd2h to the stack.
            NEXT


; ----------------------------------------------------------------------
; DABS [DOUBLE] 8.6.1.1160 "d-abs" ( d -- ud )
;
; ud is the absolute value of d.
;
; : DABS ( d -- ud )   DUP ?DNEGATE ;

            LINKTO(DTWOSTAR,0,4,'S',"BAD")
DABS:       JMP     ENTER
            .WORD   DUP,QDNEGATE,EXIT


; ----------------------------------------------------------------------
; DNEGATE [DOUBLE] 8.6.1.1230 ( d1 -- d2 )
;
; d2 is the negation of d1.
;
; : DNEGATE ( d1 -- d2)   INVERT SWAP INVERT SWAP 1 M+ ;

            LINKTO(DABS,0,7,'E',"TAGEND")
DNEGATE:    JMP     ENTER
            .WORD   INVERT,SWAP,INVERT,SWAP,ONE,MPLUS,EXIT


; ----------------------------------------------------------------------
; M+ [DOUBLE] 8.6.1.1830 "m-plus" ( d1|ud1 n -- d2|ud2 )
;
; Add n to d1|ud1, giving the sum d2|ud2.

            LINKTO(DNEGATE,0,2,'+',"M")
LAST_DOUBLE:
MPLUS:      SAVEDE
            POP     D           ; Pop n into DE.
            POP     H           ; Pop the high 16-bits of d1|ud1 into HL
            XTHL                ; ..and swap that value with the low 16-bits.
            DAD     D           ; Add n to the low 16-bits of d1|ud1.
            XTHL                ; Swap the high and low 16-bits again.
            JNC     _mplusDONE  ; We're done if there was no carry.
            INX     H           ; Increment the high 16-bits on carry.
_mplusDONE: PUSH    H           ; Push the high 16-bits back onto the stack.
            RESTOREDE
            NEXT
