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
; FACILITY Words
; ======================================================================

; ----------------------------------------------------------------------
; AT-XY [FACILITY] 10.6.1.0742 "at-x-y" ( u1 u2 -- )
;
; Perform implementation-dependent steps so that the next character
; displayed will appear in column u1, row u2 of the user output device,
; the upper left corner of which is column zero, row zero.  An ambiguous
; condition exists if the operation cannot be performed on the user
; output device with the specified parameters.

            LINKTO(LINK_FACILITY,0,5,'Y',"X-TA")
ATXY:       POP     H           ; Pop the row into L,
            MOV     A,L         ; ..move it to A,
            INR     A           ; ..and add one.
            POP     H           ; Pop the column into L,
            MOV     H,L         ; ..move it to H,
            INR     H           ; ..and add one.
            MOV     L,A         ; Move the row into L.
            CALL    STDCALL     ; Call the
            .WORD   0427Ch      ; ..SETCUR routine.
            NEXT


; ----------------------------------------------------------------------
; KEY? [FACILITY] 10.6.1.2005 "key-question" ( -- )
;
; If a character is available, return true.  Otherwise, return false.
; If non-character keyboard events are available before the first valid
; character, they are discarded and are subsequently unavailable.  The
; character shall be returned by the next execution of KEY.
;
; After KEY? returns with a value of true, subsequent executions of KEY?
; prior to the execution of KEY or EKEY also return true, without
; discarding keyboard events.

            LINKTO(ATXY,0,4,'?',"YEK")
KEYQ:       CALL    STDCALL     ; Call the
            .WORD   013DBh      ; ..CHSNS routine.
            JNZ     _keyqTRUE   ; Jump if not zero to where we push true.
            CALL    STDCALL     ; No character, so let the Main ROM
            .WORD   013C2h      ; ..flash the cursor if necessary.
            LXI     H,0         ; Put false in HL.
            JMP     _keyqDONE   ; ..and jump to where we push the flag.
_keyqTRUE:  LXI     H,0FFFFh    ; Put true in HL.
_keyqDONE:  PUSH    H           ; Push the flag to the stack.
            NEXT


; ----------------------------------------------------------------------
; PAGE [FACILITY] 10.6.1.2005 ( -- )
;
; Move to another page for output.  Actual function depends on the output
; device.  On a terminal, PAGE clears the screen and resets the cursor
; position to the upper left corner.  On a printer, PAGE performs a form feed.

            LINKTO(KEYQ,0,4,'E',"GAP")
LAST_FACILITY:
PAGE:       CALL    STDCALL     ; Call the
            .WORD   04231h      ; ..CLS routine.
            NEXT
