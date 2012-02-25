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
; MFORTH Address Interpreter
; ======================================================================

; ----------------------------------------------------------------------
; Return stack macros
;
; The Return Stack Pointer is stored in BC and always points at the next
; available location in the return stack.  The return stack grows downward
; and is confined to the 256 bytes (128 cells) between $FDFF and $FD00.
; TODO: RSPUSH and RSPOP check that the return stack has not left $FDxx
; and ABORT (with a message) if the return stack has over- or under-flowed.

;RSPUSH:    MOV     A,E         ;[1. 5]
;           STAX    B           ;[1. 7]
;           DCX     B           ;[1. 5]
;           MOV     A,D         ;[1. 5]
;           STAX    B           ;[1. 7]
;           DCX     B           ;[1. 5]
;                               ;[6.34]
#DEFINE     RSPUSH(hi,lo) MOV A,hi\ STAX B\ DCX B\ MOV A,lo\ STAX B\ DCX B

;RSPOP:     INX     B           ;[1. 5]
;           LDAX    B           ;[1. 7]
;           MOV     D,A         ;[1. 5]
;           INX     B           ;[1. 5]
;           LDAX    B           ;[1. 7]
;           MOV     E,A         ;[1. 5]
;                               ;[6.34]
#DEFINE     RSPOP(hi,lo) INX B\ LDAX B\ MOV lo,A\ INX B\ LDAX B\ MOV hi,A

;RSFETCH:   INX     B           ;[1. 5]
;           LDAX    B           ;[1. 7]
;           MOV     D,A         ;[1. 5]
;           INX     B           ;[1. 5]
;           LDAX    B           ;[1. 7]
;           MOV     E,A         ;[1. 5]
;           DCX     B           ;[1. 5]
;           DCX     B           ;[1. 5]
;                               ;[8.44]
#DEFINE     RSFETCH(hi,lo) RSPOP(hi,lo)\ DCX B\ DCX B

;RSPICK2:   INX     B           ;[1. 5]
;           INX     B           ;[1. 5]
;           INX     B           ;[1. 5]
;           INX     B           ;[1. 5]
;           INX     B           ;[1. 5]
;           LDAX    B           ;[1. 7]
;           MOV     D,A         ;[1. 5]
;           INX     B           ;[1. 5]
;           LDAX    B           ;[1. 7]
;           MOV     E,A         ;[1. 5]
;           DCX     B           ;[1. 5]
;           DCX     B           ;[1. 5]
;           DCX     B           ;[1. 5]
;           DCX     B           ;[1. 5]
;           DCX     B           ;[1. 5]
;           DCX     B           ;[1. 5]
;                               ;[8.44]
#DEFINE     RSPICK2(hi,lo) INX B\ INX B\ INX B\ INX B\ RSPOP(hi,lo)\ DCX B\ DCX B\ DCX B\ DCX B\ DCX B\ DCX B


; ----------------------------------------------------------------------
; Direct-threaded code interpreter
;
; DE is the Instruction Pointer and an undocumented 8085 opcode is used
; to transfer the location pointed to be DE into HL, the Word pointer.

#IFNDEF PROFILER
;NEXT:      LHLX                ;[1.10] (IP) -> W
;           INX     D           ;[1. 5] IP+1 -> IP
;           INX     D           ;[1. 5] IP+1 -> IP
;           PCHL                ;[1. 5] JMP W
;                               ;[4.25]
#DEFINE     NEXT    LHLX\ INX D\ INX D\ PCHL
#ELSE
PROFILENEXT:LHLD    PROFILING   ; Don't increment the Execution Count
            MOV     A,L         ; ..if the profiling
            ORA     H           ; ..flag
            JZ      _profnext1  ; ..is zero.
            LHLX                ; (IP) -> W
            DCX     H           ; Decrement HL
            DCX     H           ; ..to the low byte of the Execution Count.
            INR     M           ; Increment the low byte of the count
            JNZ     _profnext1  ; ..and skip the high byte if we didn't wrap.
            INX     H           ; Increment to the high byte of the count.
            INR     M           ; Increment the high byte of the count.
_profnext1: LHLX                ; (IP) -> W
            INX     D           ; IP+1 -> IP
            INX     D           ; IP+1 -> IP
            PCHL                ; JMP W
#DEFINE     NEXT    JMP PROFILENEXT
#ENDIF


; ----------------------------------------------------------------------
; ENTER (a.k.a. DOCOLON) for use by high-level definitions

DOCOLON:
ENTER:      RSPUSH(D,E)         ;[6.34]
            LDEH CFASZ          ;[2.10] W+CFASZ -> IP
            NEXT                ;[4.25]
                                ;[12.69]


; ----------------------------------------------------------------------
; DODOES for use by high-level CREATE..DOES> definitions

DODOES:     INXCFATOPFA(H)      ; Skip over the CFA so that HL points to PFA.
            XTHL                ; Swap PFA with the address of the high-level
                                ; ..word that appears after DOES>.  That addr
                                ; ..is on the stack because we CALL DODOES.
            RSPUSH(D,E)         ; We're about to call a new word, so push IP.
            XCHG                ; Move address of high-level thread to IP.
            NEXT


; ----------------------------------------------------------------------
; DOCREATE and DOVARIABLE for use by high-level CREATE and VARIABLE definitions.

DOCREATE:
DOVARIABLE: INXCFATOPFA(H)      ; Skip over the CFA so that HL points to PFA.
            PUSH    H           ; Push PFA to the stack.
            NEXT


; ----------------------------------------------------------------------
; DOCONSTANT for use by high-level CONSTANT definitions.

DOCONSTANT: INXCFATOPFA(H)      ; Skip over the CFA so that HL points to PFA.
            MOV     A,M         ; Get the low byte of the constant in A,
            INX     H           ; ..then increment to the high byte,
            MOV     H,M         ; ..get the high byte into H,
            MOV     L,A         ; ..move the low byte into L,
            PUSH    H           ; ..and push the constant to the stack.
            NEXT


; ----------------------------------------------------------------------
; DOUSER for use by high-level USER variables.

DOUSER:     INXCFATOPFA(H)      ; Skip over the CFA so that HL points to PFA.
            MOV     L,M         ; Put USER variable offset into L.
            MOV     H,B         ; Put Task Page into H.
            PUSH    H           ; Push USER variable address onto stack.
            NEXT



; ======================================================================
; MFORTH Dictionary
; ======================================================================

; ----------------------------------------------------------------------
; Dictionary-linking macro
;
; LINKTO is used like so (for a word named "FOUND?" that links to FIND):
;
;   LINKTO(FIND,0,6,'?',"DNUOF")
;
; Notice that the name is in reverse order and that the last character is
; specified as a separate byte.

#IFNDEF PROFILER
#DEFINE     LINKTO(prev,isimm,len,lastchar,revchars) .BYTE 10000000b|lastchar,revchars\ .BYTE (isimm<<7)|len\ .WORD prev-NFATOCFASZ
#ELSE
#DEFINE     LINKTO(prev,isimm,len,lastchar,revchars) .BYTE 10000000b|lastchar,revchars\ .BYTE (isimm<<7)|len\ .WORD prev-NFATOCFASZ\ .WORD 0
#ENDIF
