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
; TOOLS EXT Words
; ======================================================================

; ----------------------------------------------------------------------
; ASSEMBLER [TOOLS EXT] 15.6.2.0740 ( -- )
;
; Replace the first word list in the search order with the ASSEMBLER
; word list.

            LINKTO(LINK_TOOLSEXT,0,9,'R',"ELBMESSA")
ASSEMBLER:  JMP     ENTER
            .WORD   LIT,ASSEMBLERWL,LIT,SOESTART,STORE,EXIT


; ----------------------------------------------------------------------
; BYE [TOOLS EXT] 15.6.2.0830 ( -- )
;
; Return control to the host operating system, if any.

            LINKTO(ASSEMBLER,0,3,'E',"YB")
BYE:        LHLD    BOPSTK      ; Load the SP on entry into MFORTH
            SPHL                ; ..and restore that SP.
            CALL    STDCALL     ; Call the
            .WORD   5797h       ; ..main menu routine (never returns).


; ----------------------------------------------------------------------
; CODE [TOOLS EXT] 15.6.2.0930 ( "<spaces>name" -- )
;
; Skip leading space delimiters.  Parse name delimited by a space.
; Create a definition for name, called a "code definition", with the
; execution semantics defined below.  
;
; Subsequent characters in the parse area typically represent source
; code in a programming language, usually some form of assembly
; language.  Those characters are processed in an implementation-defined
; manner, generating the corresponding machine code.  The process
; continues, refilling the input buffer as needed, until an
; implementation-defined ending sequence is processed. 
;
;   name Execution: ( -- a-addr )
;       Execute the machine code sequence that was generated following
;       CODE.
;
; ---
; : CODE ( "<spaces>name" -- )
;   CREATE  CFASZ NEGATE ALLOT  ALSO ASSEMBLER ;

            LINKTO(BYE,0,4,'E',"DOC")
LAST_TOOLSEXT:
CODE:       JMP     ENTER
            .WORD   CREATE,LIT,-CFASZ,ALLOT,ALSO,ASSEMBLER
            .WORD   EXIT
