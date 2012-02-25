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
; SEARCH-ORDER Words
; ======================================================================

; ----------------------------------------------------------------------
; DEFINITIONS [SEARCH] 16.6.1.1180 ( -- )
;
; Make the compilation word list the same as the first word list in the
; search order.  Specifies that the names of subsequent definitions will
; be placed in the compilation word list.  Subsequent changes in the
; search order will not affect the compilation word list.
;
; ---
; : DEFINITIONS ( -- )   CONTEXT @ SET-CURRENT ;

            LINKTO(LINK_SEARCH,0,11,'S',"NOITINIFED")
DEFINITIONS:JMP     ENTER
            .WORD   CONTEXT,FETCH,SETCURRENT,EXIT


; ----------------------------------------------------------------------
; FORTH-WORDLIST [SEARCH] 16.6.1.1595 ( -- wid )
;
; Return wid, the identifier of the word list that includes all standard
; words provided by the implementation.  This word list is initially the
; compilation word list and is part of the initial search order.

            LINKTO(DEFINITIONS,0,14,'T',"SILDROW-HTROF")
FORTHWORDLIST:JMP   ENTER
            .WORD   LIT,FORTHWL,EXIT


; ----------------------------------------------------------------------
; GET-CURRENT [SEARCH] 16.6.1.1643 ( -- wid )
;
; Return wid, the identifier of the compilation word list.

            LINKTO(FORTHWORDLIST,0,11,'T',"NERRUC-TEG")
GETCURRENT: JMP     ENTER
            .WORD   CURRENT,FETCH,EXIT


; ----------------------------------------------------------------------
; GET-ORDER [SEARCH] 16.6.1.1647 ( -- widn ... wid1 n )
;
; Returns the number of word lists n in the search order and the word
; list identifiers widn ... wid1 identifying these word lists.  wid1
; identifies the word list that is searched first, and widn the word
; list that is searched last.  The search order is unaffected.
;
; ---
; : GET-ORDER ( -- widn ... wid1 n)
;   0 #SOES 1- DO  SOESTART I CELLS + @  -1 +LOOP  #SOES ;

            LINKTO(GETCURRENT,0,9,'R',"EDRO-TEG")
GETORDER:   JMP     ENTER
            .WORD   ZERO,NUMSOES,ONEMINUS,pdo
_getorder1: .WORD   LIT,SOESTART,I,CELLS,PLUS,FETCH,LIT,-1,pplusloop,_getorder1
            .WORD   NUMSOES
            .WORD   EXIT


; ----------------------------------------------------------------------
; SEARCH-WORDLIST [SEARCH] 16.1.2192 ( c-addr u wid -- 0 | xt 1 | xt -1 )
;
; Find the definition identified by the string c-addr u in the word list
; identified by wid.  If the definition is not found, return zero.  If
; the definition is found, return its execution token xt and one (1) if
; the definition is immediate, minus-one (-1) otherwise.
;
; ---
; This word traverses the dictionary's linked list until the traversal
; enters ROM.  At that point (FIND) stops using the linked list and tries
; to locate the word using the perfect hash table generated at build
; time.  This only happens if wid points to the FORTH word list, which
; is tracked by B (FORTH=0; other=-1).  The _phash subroutine generates
; two hash values, H and L.  The target word, if it exists in ROM, will
; be found at one of two locations: HL or LH.  HL is the more likely
; location and is searched first.  C maintains the state of the search
; location.  -1 indicates that the search should use HL, 0 indicates
; that the search should use LH, and 1 indicates that the search failed.

            LINKTO(GETORDER,0,15,'T',"SILDROW-HCRAES")
SEARCHWORDLIST:SAVEDE
            SAVEBC
            POP     D           ; Get wid from the stack,
            MOV     B,D         ; ..copy wid into B
            MOV     C,E         ; ..and C,
            LXI     H,FORTHWL   ; ..get the FORTH wid in HL,
            DSUB                ; ..then compare wid to the FORTH wid;
            JZ      _swlFORTH   ; ..jump to where we clear B if FORTH,
            MVI     B,-1        ; ..otherwise set B to -1
            JMP     _swlLATEST  ; ..and load LATEST.
_swlFORTH:  MVI     B,0         ; FORTH wid, so clear B.
_swlLATEST: LHLX                ; Get the latest word in wid
            XCHG                ; ..and put that value in DE.
            MVI     C,-1        ; Initialize our phash flag to -1.
            POP     H           ; Pop the length of the string
            SHLD    HOLDD       ; ..and cache the value.
            POP     H           ; Pop the string pointer
            SHLD    HOLDH       ; ..and cache the value.
_swlAGAIN:
#IFDEF PHASH
            MOV     A,D         ; See if we are still in RAM (the
            ANI     80h         ; ..high bit of the addr is not zero) -- or
            ORA     B           ; ..we are not in the FORTH word list -- and
            JNZ     _swlAGAIN1  ; ..keep traversing the linked list if so.
_swlPHASH:  PUSH    H           ; Save the string pointer on the stack,
            LDA     HOLDD       ; ..get the string length into A,
            CALL    _phash      ; ..then hash the string.
            MOV     A,C         ; Move our phash flag to A,
            ORA     A           ; ..then check the state of the flag:
            JM      _swlPHASHH1 ; ..use H1 if the value is negative;
            JZ      _swlPHASHH2 ; ..use H2 if the value is zero;
            POP     H           ; ..otherwise no match, pop the counted string
            JMP     _swlFAIL    ; ..and fail.
_swlPHASHH2:MOV     A,L         ; Move H2 to A,
            MOV     L,H         ; ..move H1 to L,
            JMP     _swlPHASH1  ; ..then continue.
_swlPHASHH1:MOV     A,H         ; Move H1 to A
_swlPHASH1: ANI     PHASHMASK   ; ..and mask off the high bits of H1.
            MOV     H,A         ; Get the masked off bits of H1 back into H.
            DAD     H           ; HL=HL<<1 to convert from hash to cell offset.
            MOV     A,H         ; Move the high byte of the offset to A,
            ADI     PHASHTAB>>8 ; ..add the high byte of PHASHTAB to A,
            MOV     H,A         ; ..and then put the PHASHTAB address into H.
            MOV     E,M         ; Get the low byte of the hash cell in E,
            INX     H           ; ..increment to the high byte,
            MOV     D,M         ; ..then get the low byte into D.
            POP     H           ; Restore the string pointer.
            INR     C           ; Increment our phash flag.
            MOV     A,D         ; Move D to A,
            ORA     E           ; ..then OR A and E to see if the cell is zero;
            JZ      _swlPHASH   ; ..try to phash again if so.
#ENDIF
_swlAGAIN1: LDAX    D           ; Get the name length into A.
            ANI     01111111b   ; Strip the immediate bit.
            LXI     H,HOLDD     ; Point HL at the string length,
            CMP     M           ; ..then compare the two lengths+smudge bits.
            JNZ     _swlNEXTWORD;Jump if not zero (not equal) to the next word.
            PUSH    D           ; Save DE since we are about to scan through it.
            DCX     D           ; Go to the first dictionary char (prev byte).
            LHLD    HOLDH       ; Point HL at the first string character.
_swlNEXTCHAR:LDAX   D           ; Get the next dictionary value into A.
            ANI     01111111b   ; Strip the end-of-name bit.
            CMP     M           ; Compare the two characters.
            JZ      _swlMATCHCHAR;Jump if zero (equal) to match.
            XRI     00100000b   ; Try switching the case
            CMP     M           ; ..and then repeating the match.
            JNZ     _swlNEXTWORDDE;.Not a match if not zero (not equal).
            ORI     00100000b   ; Only a match if A-Z/a-z.  Force to lower,
            CPI     'a'         ; ..then see if less than 'a'.
            JM      _swlNEXTWORDDE;.If so, this is not a match.
            CPI     'z'+1       ; If greater than 'z'+1,
            JP      _swlNEXTWORDDE;.then this is also not a match.
_swlMATCHCHAR:LDAX  D           ; The strings are a match if this is the last
            ANI     10000000b   ; ..character in the name (high bit set).
            JNZ     _swlMATCH   ; We're done if this is a match.
            DCX     D           ; Go to the next dictionary char (prev byte).
            INX     H           ; Go to the next string character.
            JMP     _swlNEXTCHAR;Evaluate the next character.
_swlMATCH:  POP     D           ; Restore DE (which is now pointing at a char)
            LDAX    D           ; Get the flags into A
            ANI     10000000b   ; ..and focus on just the immediate flag.
            INXNFATOCFA(D)      ; Skip ahead to the CFA (xt)
            PUSH    D           ; ..and push xt to the stack.
            JNZ     _swlIMM     ; Immediate gets a 1 pushed to the stack,
            LXI     H,0FFFFh    ; ..non-immediate gets a -1
            PUSH    H           ; ..pushed to the stack.
            JMP     _swlDONE    ; We're done.
_swlIMM:    LXI     H,1         ; Immediate word, so push 1
            PUSH    H           ; ..to the stack.
            JMP     _swlDONE    ; We're done.
_swlNEXTWORDDE:POP  D           ; Restore DE (which is now pointing at a char).
_swlNEXTWORD:INXNFATOLFA(D)     ; Move to the word's LFA,
            LHLX                ; ..get the LFA in HL,
            XCHG                ; ..put the LFA into DE,
            LHLD    HOLDH       ; ..and restore HL.
#IFDEF PHASH
            MOV     A,D         ; The phash routine ignores the LFA, so
            ANI     80h         ; ..see if we are in RAM -- or
            ORA     B           ; ..we are not in the FORTH word list -- and
            JNZ     _swlNEXTWORD1;..keep traversing the linked list if so;
            JMP     _swlPHASH   ; ..continue the phash process otherwise.
#ENDIF
_swlNEXTWORD1:MOV   A,D         ; Keep searching for a match
            ORA     E           ; ..if the LFA
            JNZ     _swlAGAIN   ; ..is not zero.
_swlFAIL:   LXI     H,0         ; Push false
            PUSH    H           ; ..to the stack.
_swlDONE:   RESTOREDE
            RESTOREBC
            NEXT

#IFDEF PHASH
; Entry: HL=c-addr A=u (all registers are used)
; Exit : HL=hash values (H1 in H, H2 in L)
_phash:     PUSH    B           ; Save BC
            PUSH    D           ; ..and DE.
            LXI     D,0         ; Clear the hash values,
            PUSH    D           ; ..which are stored on the stack.
            ORA     A           ; See if the string is zero-length;
            JZ      _phashDONE  ; ..and exit if so.
            MOV     C,A         ; Otherwise move the length to A.
_phashNEXT: MOV     A,M         ; Get the next character into A,
            CPI     'a'         ; ..then see if less than 'a';
            JM      _phashNEXT1 ; ..if so, don't uppercase.
            CPI     'z'+1       ; If greater than 'z'+1,
            JP      _phashNEXT1 ; ..don't uppercase.
            ANI     11011111b   ; Convert uppercase to lowercase.
_phashNEXT1:XTHL                ; Swap the string pos with the hashes.
            MOV     B,A         ; Save a copy of the character.
            XRA     H           ; XOR the character with the H1,
            MOV     E,A         ; ..move the PHASHAUX offset into E,
            MVI     D,PHASHAUX1>>8;.put the PHASHAUX1 base offset into D,
            LDAX    D           ; ..then lookup the new hash value,
            MOV     H,A         ; ..and move the hash value to H.
            MOV     A,B         ; Get the cached copy of the character.
            XRA     L           ; XOR the character with the H2,
            MOV     E,A         ; ..move the PHASHAUX offset into E,
            MVI     D,PHASHAUX2>>8;.put the PHASHAUX2 base offset into D,
            LDAX    D           ; ..then lookup the new hash value,
            MOV     L,A         ; ..and move the hash value to L.
            XTHL                ; Swap the hashes with the string pos.
            INX     H           ; Increment to the next character,
            DCR     C           ; ..decrement the count,
            JNZ     _phashNEXT  ; ..and keep looping if we count is not zero.
_phashDONE: POP     H           ; Pop the hash values into HL.
            POP     D           ; Restore DE
            POP     B           ; ..and BC.
            RET                 ; We're done.
#ENDIF


; ----------------------------------------------------------------------
; SET-CURRENT [SEARCH] 16.6.1.2195 ( wid -- )
;
; Set the compilation word list to the word list identified by wid.

            LINKTO(SEARCHWORDLIST,0,11,'T',"NERRUC-TES")
SETCURRENT: JMP     ENTER
            .WORD   CURRENT,STORE,EXIT


; ----------------------------------------------------------------------
; SET-ORDER [SEARCH] 16.6.1.2197 ( widn ... wid1 n -- )
;
; Set the search order to the word lists identified by widn ... wid1.
; Subsequently, word list wid1 will be searched first, and word list
; widn searched last.  If n is zero, empty the search order.  If n is
; minus one, set the search order to the implementation-defined minimum
; search order.  The minimum search order shall include the words
; FORTH-WORDLIST and SET-ORDER.  A system shall allow n to be at least
; eight.
;
; ---
; : SET-ORDER ( widn ... wid1 n --)   0 DO SOESTART I CELLS + ! LOOP ;

            LINKTO(SETCURRENT,0,9,'R',"EDRO-TES")
SETORDER:   JMP     ENTER
            .WORD   ZERO,pdo
_setorder1: .WORD   LIT,SOESTART,I,CELLS,PLUS,STORE,ploop,_setorder1
_setorder2: .WORD   EXIT


; ----------------------------------------------------------------------
; WORDLIST [SEARCH] 16.6.1.2460 ( -- wid )
;
; Create a new empty word list, returning its word list identifier wid.
; The new word list may be returned from a pool of preallocated word
; lists or may be dynamically allocated in data space.  A system shall
; allow the creation of at least 8 new word lists in addition to any
; provided as part of the system.

            LINKTO(SETORDER,0,8,'T',"SILDROW")
WORDLIST:   JMP     ENTER
            .WORD   HERE,ZERO,COMMA,EXIT



; ======================================================================
; SEARCH Words (implementation details)
; ======================================================================

; ----------------------------------------------------------------------
; #SOES [MFORTH] "num-s-o-es" ( -- n )
;
; Returns the number of word lists n in the search order.
;
; ---
; : #SOES ( --n)
;   CONTEXT DUP  BEGIN DUP @ 0<> WHILE CELL+ REPEAT  SWAP - 2/ ;

            LINKTO(WORDLIST,0,5,'S',"EOS#")
NUMSOES:    JMP     ENTER
            .WORD   CONTEXT,DUP
_numsoes1:  .WORD   DUP,FETCH,ZERONOTEQUALS,zbranch,_numsoes2
            .WORD   CELLPLUS,branch,_numsoes1
_numsoes2:  .WORD   SWAP,MINUS,TWOSLASH,EXIT


; ----------------------------------------------------------------------
; CONTEXT [MFORTH] ( -- a-addr )
;
; a-addr is the address of a cell that contains a pointer to the first
; word list in the search order.

            LINKTO(NUMSOES,0,7,'T',"XETNOC")
CONTEXT:    LXI     H,SOESTART
            PUSH    H
            NEXT


; ----------------------------------------------------------------------
; CURRENT [MFORTH] ( -- a-addr )
;
; a-addr is the address of a cell that contains a pointer to the current
; compilation word list.

            LINKTO(CONTEXT,0,7,'T',"NERRUC")
LAST_SEARCH:
CURRENT:    LXI     H,TICKCURRENT
            PUSH    H
            NEXT
