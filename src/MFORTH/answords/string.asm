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
; STRING Words
; ======================================================================

; ----------------------------------------------------------------------
; /STRING [STRING] 17.6.1.0245 "slash-string" ( c-addr1 u1 n -- c-addr2 u2 )
;
; Adjust the character string at c-addr1 by n characters.  The resulting
; character string, specified by c-addr2 u2, begins at c-addr1 plus n
; characters and is u1 minus n characters long.

            LINKTO(LINK_STRING,0,7,'G',"NIRTS/")
LAST_STRING:
SLASHSTRING:SAVEDE
            MOV     D,B         ; Save BC
            MOV     E,C         ; ..in DE.
            POP     B           ; Pop the adjustment value into BC.
            POP     H           ; Pop the length into HL.
            DSUB                ; Adjust the length.
            XTHL                ; Swap the adjusted length with the address.
            DAD     B           ; Adjust the address.
            XTHL                ; Swap the address and length again.
            PUSH    H           ; Push the length onto the stack.
            MOV     B,D         ; Restore BC
            MOV     C,E         ; ..from DE.
            RESTOREDE
            NEXT
