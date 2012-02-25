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
; SEARCH-ORDER-EXT Words
; ======================================================================

; ----------------------------------------------------------------------
; ALSO [SEARCH EXT] 16.6.2.0715 ( -- )
;
; Transform the search order consisting of widn, ... wid2, wid1 (where
; wid1 is searched first) into widn, ... wid2, wid1, wid1.  An ambiguous
; condition exists if there are too many word lists in the search order.
;
; ---
; : ALSO ( -- )   SOESTART  DUP CELL+  #SOES CELLS  MOVE ;

            LINKTO(LINK_SEARCHEXT,0,4,'O',"SLA")
ALSO:       JMP     ENTER
            .WORD   LIT,SOESTART,DUP,CELLPLUS,NUMSOES,CELLS,MOVE,EXIT


; ----------------------------------------------------------------------
; FORTH [SEARCH EXT] 16.6.2.1590 ( -- )
;
; Transform the search order consisting of widn, ... wid2, wid1 (where
; wid1 is searched first) into widn, ... wid2, widFORTH-WORDLIST.
;
; ---
; : FORTH ( -- )   FORTH-WORDLIST SOESTART ! ;

            LINKTO(ALSO,0,5,'H',"TROF")
FORTH:      JMP     ENTER
            .WORD   FORTHWORDLIST,LIT,SOESTART,STORE
            .WORD   EXIT


; ----------------------------------------------------------------------
; ONLY [SEARCH EXT] 16.6.2.1965 ( -- )
;
; Set the search order to the implementation-defined minimum search
; order.  The minimum search order shall include the words
; FORTH-WORDLIST and SET-ORDER.
;
; ---
; : ONLY ( -- )   SOESTART [ MAXSOES CELLS ] 0 FILL  FORTH ;

            LINKTO(FORTH,0,4,'Y',"LNO")
ONLY:       JMP     ENTER
            .WORD   LIT,SOESTART,LIT,MAXSOES,CELLS,ZERO,FILL,FORTH
            .WORD   EXIT


; ----------------------------------------------------------------------
; ORDER [SEARCH EXT] 16.6.2.1985 ( -- )
;
; Display the word lists in the search order in their search order
; sequence, from first searched to last searched.  Also display the word
; list into which new definitions will be placed.  The display format is
; implementation dependent.
;
; ORDER may be implemented using pictured numeric output words.
; Consequently, its use may corrupt the transient region identified by
; #>.
;
; ---
; : ORDER ( -- )
;   GET-ORDER 0 DO HEXCELL SPACE LOOP
;   [CHAR] [ EMIT GET-CURRENT HEXCELL [CHAR] ] EMIT ;

            LINKTO(ONLY,0,5,'R',"EDRO")
ORDER:      JMP     ENTER
            .WORD   GETORDER,ZERO,pdo
_order1:    .WORD   HEXCELL,SPACE,ploop,_order1
_order2:    .WORD   LIT,'[',EMIT,GETCURRENT,HEXCELL,LIT,']',EMIT
            .WORD   EXIT


; ----------------------------------------------------------------------
; PREVIOUS [SEARCH EXT] 16.6.2.2037 ( -- )
;
; Transform the search order consisting of widn, ... wid2, wid1 (where
; wid1 is searched first) into widn, ... wid2.  An ambiguous condition
; exists if the search order was empty before PREVIOUS was executed.
;
; ---
; : PREVIOUS ( -- )
;   SOESTART  DUP CELL+  SWAP  #SOES 1- CELLS  MOVE
;   0  SOESTART  #SOES 1- CELLS  +  ! ;

            LINKTO(ORDER,0,8,'S',"UOIVERP")
LAST_SEARCHEXT:
PREVIOUS:   JMP     ENTER
            .WORD   LIT,SOESTART,DUP,CELLPLUS,SWAP
            .WORD   NUMSOES,ONEMINUS,CELLS,MOVE
            .WORD   ZERO,LIT,SOESTART,NUMSOES,ONEMINUS,CELLS,PLUS,STORE
            .WORD   EXIT
