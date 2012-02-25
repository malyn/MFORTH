; Copyright (c) 2011, Michael Alyn Miller <malyn@strangeGizmo.com>.
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
; FACILITY-EXT Words
; ======================================================================

; ----------------------------------------------------------------------
; TIME&DATE [FACILITY EXT] 10.6.2.2292 "time-and-date" ( -- +n1 +n2 +n3 +n4 +n5 +n6 )
;
; Return the current time and date.  +n1 is the second {0...59}, +n2 is
; the minute {0...59}, +n3 is the hour {0...23}, +n4 is the day {1...31}
; +n5 is the month {1...12}, and +n6 is the year (e.g., 1991).

            LINKTO(LINK_FACILITYEXT,0,9,'E',"TAD&EMIT")
LAST_FACILITYEXT:
TIMEANDDATE:PUSH    B           ; Update
            PUSH    D           ; ..the
            CALL    STDCALL     ; ..clock
            .WORD   019A0h      ; ..chip
            POP     D           ; ..values
            POP     B           ; ..(which corrupts every register).

            DI                  ; Disable interrupts while we copy date/time.

            ; Get the seconds.
            LDA     0F924h
            MOV     H,A
            LDA     0F923h
            MOV     L,A
            CALL    _tadCalc
            PUSH    H

            ; Get the minutes.
            LDA     0F926h
            MOV     H,A
            LDA     0F925h
            MOV     L,A
            CALL    _tadCalc
            PUSH    H

            ; Get the hours.
            LDA     0F928h
            MOV     H,A
            LDA     0F927h
            MOV     L,A
            CALL    _tadCalc
            PUSH    H

            ; Get the date.
            LDA     0F92Ah
            MOV     H,A
            LDA     0F929h
            MOV     L,A
            CALL    _tadCalc
            PUSH    H

            ; Get the month.
            LDA     0F92Ch
            MVI     H,0
            MOV     L,A
            PUSH    H

            ; Get the year.
            LDA     0F92Eh
            MOV     H,A
            LDA     0F92Dh
            MOV     L,A
            CALL    _tadCalc
            XCHG                ; Swap yy with DE
            PUSH    H           ; ..and push HL (DE) to the stack.
            LXI     H,2000      ; Add 2000
            DAD     D           ; ..to yy.
            XTHL                ; Put yyyy on the stack and get HL (DE),
            XCHG                ; ..then swap DE back in to place.

            EI                  ; Enable interrupts now that we're done.
            NEXT

            ; Calculates the following: L = H*10 + L
_tadCalc:   MOV     A,H         ; Move the tens value into A.
            ADD     H           ; M
            ADD     H           ; u
            ADD     H           ; l
            ADD     H           ; t
            ADD     H           ; i
            ADD     H           ; p
            ADD     H           ; l
            ADD     H           ; y
            ADD     H           ; by 10.
            ADD     L           ; Add the units value to A.
            MVI     H,0         ; Clear H
            MOV     L,A         ; ..and put the calculated value in L.
            RET
