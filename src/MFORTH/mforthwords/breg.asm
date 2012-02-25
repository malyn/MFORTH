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
; B ("byte") Register Words
; ======================================================================

; ----------------------------------------------------------------------
; 'B [MFORTH] ( -- c-addr)
;
; Push the address of the B register (a USER variable) to the stack.

            LINKTO(LINK_BREG,0,2,'B',"\'")
TICKB:      JMP     DOUSER
            .BYTE   USERB


; ----------------------------------------------------------------------
; 'Bend [MFORTH] ( -- c-addr)
;
; Push the address of the Bend register (a USER variable) to the stack.

            LINKTO(TICKB,0,5,'d',"neB\'")
TICKBEND:   JMP     DOUSER
            .BYTE   USERBEND


; ----------------------------------------------------------------------
; 2>B [MFORTH] "two-to-b" ( c-addr u -- )
;
; Pop an address and length from the stack and store the range in B.
;
; ---
; : 2>B  ( c-addr u --)   OVER + 'Bend !  'B ! ;

            LINKTO(TICKBEND,0,3,'B',">2")
TWOTOB:     JMP     ENTER
            .WORD   OVER,PLUS,TICKBEND,STORE,TICKB,STORE,EXIT


; ----------------------------------------------------------------------
; >B [MFORTH] "to-b" ( c-addr -- )
;
; Pop an address from the stack and put the address into the B register.
;
; ---
; : >B ( c-addr --)   'B ! ;

            LINKTO(TWOTOB,0,2,'B',">")
TOB:        JMP     ENTER
            .WORD   TICKB,STORE,EXIT


; ----------------------------------------------------------------------
; ?ENDB [MFORTH] "question-end-b"
;
; Interpretation:
;   Interpretation semantics for this word are undefined.
;
; Execution: ( flag -- )
;   Continue execution immediately following the innermost syntactically
;   enclosing FORB ... NEXTB if flag is true.  Otherwise continue execution
;   at the next instruction.
;
; ---
; ?ENDB   POSTPONE IF
;   ['] branch COMPILE,  HERE  'PREVENDB @ ,  'PREVENDB !
;   POSTPONE THEN ; IMMEDIATE

            LINKTO(TOB,1,5,'B',"DNE?")
QENDB:      JMP     ENTER
            .WORD   IF,LIT,branch,COMPILECOMMA
            .WORD   HERE,LIT,TICKPREVENDB,FETCH,COMMA,LIT,TICKPREVENDB,STORE
            .WORD   THEN,EXIT


; ----------------------------------------------------------------------
; B [MFORTH] ( -- c-addr)
;
; Push the B register to the stack.
;
; ---
; : B ( -- c-addr)  'B @ ;

            LINKTO(QENDB,0,1,'B',"")
B:          MOV     H,B
            MVI     L,USERB
            MOV     A,M         ; Load LSB of cell value into A
            INX     H           ; Increment to MSB of the cell value
            MOV     H,M         ; Load MSB of the cell value into H
            MOV     L,A         ; Move LSB of cell value from A to L
            PUSH    H           ; Push cell value onto stack.
            NEXT


; ----------------------------------------------------------------------
; B! [MFORTH] "b-store" ( c -- )
;
; Pop a byte from the stack and store it at B.
;
; ---
; : B! (c --)   B C! ;

            LINKTO(B,0,2,'!',"B")
BSTORE:     MOV     H,B
            MVI     L,USERB
            MOV     A,M         ; Load LSB of cell value into A
            INX     H           ; Increment to MSB of the cell value
            MOV     H,M         ; Load MSB of the cell value into H
            MOV     L,A         ; Move LSB of cell value from A to L
            XTHL                ; Get c into L,
            MOV     A,L         ; ..then move c into A,
            POP     H           ; ..restore store HL.
            MOV     M,A         ; ..and store c at HL.
            NEXT


; ----------------------------------------------------------------------
; B!+ [MFORTH] "b-store-plus" ( c -- )
;
; Pop a byte from the stack, store it at B, then increment the B register
; by one (byte address location).
;
; ---
; : B!+ ( c --)   B! B+ ;

            LINKTO(BSTORE,0,3,'+',"!B")
BSTOREPLUS: JMP     ENTER
            .WORD   BSTORE,BPLUS,EXIT


; ----------------------------------------------------------------------
; B# [MFORTH] "b-number" ( -- u )
;
; Push the number of bytes remaining in the range defined by B.
;
; ---
; : B# ( -- u)   'Bend @  B  - ;

            LINKTO(BSTOREPLUS,0,2,'#',"B")
BNUMBER:    PUSH    B
            MOV     H,B
            MVI     L,USERB
            MOV     C,M         ; Load LSB of cell value into C
            INX     H           ; Increment to MSB of the cell value
            MOV     B,M         ; Load MSB of the cell value into B.
            MVI     L,USERBEND
            MOV     A,M         ; Load LSB of cell value into A
            INX     H           ; Increment to MSB of the cell value
            MOV     H,M         ; Load MSB of the cell value into H
            MOV     L,A         ; Move LSB of cell value from A to L
            DSUB
            XTHL
            MOV     B,H
            MOV     C,L
            NEXT


; ----------------------------------------------------------------------
; B+ [MFORTH] "b-plus" ( -- c-addr)
;
; Increment the B register by one (byte address location).
;
; ---
; : B+ ( --)  1 CHARS 'B +! ;

            LINKTO(BNUMBER,0,2,'+',"B")
BPLUS:      MOV     H,B         ; Get the address of the B user variable
            MVI     L,USERB     ; ..into HL.
            INR     M           ; Increment the B user variable;
            JNZ     _bplusDONE  ; ..we're done if the low byte didn't roll.
            INX     H           ; Otherwise increment to the high byte
            INR     M           ; ..and propagate the overflow.
_bplusDONE: NEXT


; ----------------------------------------------------------------------
; B? [MFORTH] "b-question" ( -- flag )
;
; flag is true if and only if there are more bytes in B.
;
; ---
; : B? ( -- f)   B# 0 > ;

            LINKTO(BPLUS,0,2,'?',"B")
BQUES:      PUSH    B
            MOV     H,B
            MVI     L,USERB
            MOV     C,M         ; Load LSB of cell value into C
            INX     H           ; Increment to MSB of the cell value
            MOV     B,M         ; Load MSB of the cell value into B.
            MVI     L,USERBEND
            MOV     A,M         ; Load LSB of cell value into A
            INX     H           ; Increment to MSB of the cell value
            MOV     H,M         ; Load MSB of the cell value into H
            MOV     L,A         ; Move LSB of cell value from A to L
            DSUB
            JZ      _bquesDONE  ; Leave zero in HL and we're done; otherwise
            LXI     H,0FFFFh    ; ..put true in HL.
_bquesDONE: XTHL
            MOV     B,H
            MOV     C,L
            NEXT


; ----------------------------------------------------------------------
; B@ [MFORTH] "b-fetch" ( -- c-addr)
;
; Fetch the byte at B.
;
; ---
; : B@ ( -- c)   B C@ ;

            LINKTO(BQUES,0,2,'@',"B")
BFETCH:     MOV     H,B
            MVI     L,USERB
            MOV     A,M         ; Load LSB of cell value into A
            INX     H           ; Increment to MSB of the cell value
            MOV     H,M         ; Load MSB of the cell value into H
            MOV     L,A         ; Move LSB of cell value from A to L
            MOV     A,M         ; Load target byte into A,
            MOV     L,A         ; ..put target byte into L,
            MVI     H,0         ; ..clear the high byte,
            PUSH    H           ; ..and then push the byte to the stack.
            NEXT


; ----------------------------------------------------------------------
; B@+ [MFORTH] "b-fetch-plus" ( -- c )
;
; Fetch the byte at B, then increment the B register by one (byte address
; location.
;
; ---
; : B@+ ( -- c)   B@ B+ ;

            LINKTO(BFETCH,0,3,'+',"@B")
BFETCHPLUS: JMP     ENTER
            .WORD   BFETCH,BPLUS,EXIT


; ----------------------------------------------------------------------
; FORB [MFORTH] "for-b"
;
; Interpretation:
;   Interpretation semantics for this word are undefined.
;
; Compilation: ( C: -- forb-sys )
;   Place forb-sys onto the control-flow stack.  Append the run-time
;   semantics given below to the current definition.  The semantics are
;   incomplete until resolved by a consumer of forb-sys such as NEXTB.
;
; Run-time: ( -- )
;   Set up a loop that iterates over the bytes from 'B to 'Bend.
;
; ---
; forb-sys in MFORTH is ( forb-orig ).  ?ENDB locations chain from the most
; recent ?ENDB to the oldest ?ENDB and then to zero, which signifies the
; end of the ?ENDB list.  NEXTB goes through the ?ENDB list and fixes up
; the addresses.
;
; : FORB   0 'PREVENDB !  POSTPONE BEGIN
;   POSTPONE B# POSTPONE 0= POSTPONE ?ENDB
; ; IMMEDIATE

            LINKTO(BFETCHPLUS,1,4,'B',"ROF")
FORB:       JMP     ENTER
            .WORD   ZERO,LIT,TICKPREVENDB,STORE,BEGIN
            .WORD   LIT,BNUMBER,COMPILECOMMA,LIT,ZEROEQUALS,COMPILECOMMA,QENDB
            .WORD   EXIT


; ----------------------------------------------------------------------
; NEXTB [MFORTH] "next-b"
;
; Interpretation:
;   Interpretation semantics for this word are undefined.
;
; Compilation: ( C: dest -- )
;   Append the run-time semantics given below to the current definition.
;   Resolve the destination of all unresolved occurrences of ?ENDB between
;   the location given by dest and the next location for a transfer of
;   control, to execute the words following the NEXTB.
;
; Run-time: ( -- )
;   Increment 'B and continue execution at the location specified by dest.
;
; ---
; NEXTB   POSTPONE B+ POSTPONE AGAIN  'PREVENDB @ HERE>CHAIN ; IMMEDIATE

            LINKTO(FORB,1,5,'B',"TXEN")
LAST_BREG:
NEXTB:      JMP     ENTER
            .WORD   LIT,BPLUS,COMPILECOMMA,AGAIN
            .WORD   LIT,TICKPREVENDB,FETCH,HERETOCHAIN,EXIT
