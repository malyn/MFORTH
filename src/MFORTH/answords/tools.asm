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
; TOOLS Words
; ======================================================================

; ----------------------------------------------------------------------
; .S [TOOLS] 15.6.1.0220 "dot-s" ( -- )
;
; Copy and display the values currently on the data stack. The format of
; the display is implementation-dependent.
;
; .S may be implemented using pictured numeric output words.  Consequently,
; its use may corrupt the transient region identified by #>.
;
; .S ( --)   DEPTH BEGIN ?DUP WHILE DUP PICK . 1- REPEAT ;

            LINKTO(LINK_TOOLS,0,2,'S',".")
DOTS:       JMP     ENTER
            .WORD   DEPTH
_dots1:     .WORD   QDUP,zbranch,_dots2,DUP,PICK,DOT,ONEMINUS,branch,_dots1
_dots2:     .WORD   EXIT


; ----------------------------------------------------------------------
; DUMP [TOOLS] 15.6.1.1280 ( addr u -- )
;
; Display the contents of u consecutive addresses starting at addr. The
; format of the display is implementation dependent. 
;
; DUMP may be implemented using pictured numeric output words. Consequently,
; its use may corrupt the transient region identified by #>.
;
; MFORTH Output Format (screen represented by the box):
; +----------------------------------------+
; |0000  00 01 02 03 04 05 06 07  Hello th |
; |0008  08 09 0a 0b 0c 0d        ere!..   |
; |...                                     |
; +----------------------------------------+
;
; : HEXCELL ( u --)  BASE @ SWAP HEX 0 <# # # # # #> TYPE BASE ! ;
; : HEXCHAR ( c --)  BASE @ SWAP HEX 0 <# # # #> TYPE BASE ! ;
; : EMITVALID ( c --)  DUP 32 < OVER 127 = OR  [CHAR] . AND OR  EMIT ;
; : DUMPLINE ( addr u --)
;   OVER HEXCELL 2 SPACES                                       -- address
;   DUP 0 DO OVER I + C@ HEXCHAR SPACE LOOP                     -- hex vals
;   8 OVER - 3 * SPACES  SPACE                                  -- padding
;   0 DO DUP I + C@ EMITVALID LOOP  DROP;
; : DUMP ( addr u --)
;   DUP 0 ?DO  CR  OVER I +  OVER I - 8 MIN  DUMPLINE  8 +LOOP  2DROP ;

            LINKTO(DOTS,0,4,'P',"MUD")
DUMP:       JMP     ENTER
            .WORD   DUP,ZERO,pqdo,_dump2
_dump1:     .WORD   CR,OVER,I,PLUS,OVER,I,MINUS,LIT,8,MIN,DUMPLINE
            .WORD       LIT,8,pplusloop,_dump1
_dump2:     .WORD   TWODROP,EXIT
HEXCELL:    JMP     ENTER
            .WORD   BASE,FETCH,SWAP,HEX,ZERO
            .WORD   LESSNUMSIGN,NUMSIGN,NUMSIGN,NUMSIGN,NUMSIGN,NUMSIGNGRTR,TYPE
            .WORD   BASE,STORE,EXIT
HEXCHAR:    JMP     ENTER
            .WORD   BASE,FETCH,SWAP,HEX,ZERO
            .WORD   LESSNUMSIGN,NUMSIGN,NUMSIGN,NUMSIGNGRTR,TYPE
            .WORD   BASE,STORE,EXIT
EMITVALID:  JMP     ENTER
            .WORD   DUP,LIT,32,LESSTHAN,OVER,LIT,127,EQUALS,OR
            .WORD   LIT,'.',AND,OR,EMIT,EXIT
DUMPLINE:   JMP     ENTER
            .WORD   OVER,HEXCELL,LIT,2,SPACES
            .WORD   DUP,ZERO,pdo
_dumpline1: .WORD   OVER,I,PLUS,CFETCH,HEXCHAR,SPACE,ploop,_dumpline1
            .WORD   LIT,8,OVER,MINUS,LIT,3,STAR,SPACES,SPACE
            .WORD   ZERO,pdo
_dumpline2: .WORD   DUP,I,PLUS,CFETCH,EMITVALID,ploop,_dumpline2
            .WORD   DROP,EXIT


; ----------------------------------------------------------------------
; WORDS [TOOLS] 15.6.1.2465 ( -- )
;
; List the definition names in the first word list of the search order.
; The format of the display is implementation-dependent.
;
; WORDS may be implemented using pictured numeric output words.
; Consequently, its use may corrupt the transient region identified by #>.
;
; : WORDS ( -- )
;   LATEST @  BEGIN  DUP HIDDEN? 0=  IF SPACE DUP .NAME THEN
;   NFA>LFA @  DUP 0= UNTIL DROP ;

            LINKTO(DUMP,0,5,'S',"DROW")
WORDS:      JMP     ENTER
            .WORD   LATEST,FETCH
_words1:    .WORD   DUP,HIDDENQ,ZEROEQUALS,zbranch,_words2
            .WORD   SPACE,DUP,DOTNAME
_words2:    .WORD   NFATOLFA,FETCH,DUP,ZEROEQUALS,zbranch,_words1
            .WORD   DROP,EXIT



; ======================================================================
; TOOLS Words (implementation details)
; ======================================================================

; ----------------------------------------------------------------------
; .NAME [MFORTH] "dot-name" ( nfa-addr -- )
;
; Display the name of the dictionary entry pointed to by nfa-addr (which
; points to the length field).
;
; : .NAME ( nfa-addr -- )
;   BEGIN  1- DUP C@  DUP 127 AND EMIT  128 AND UNTIL DROP ;

            LINKTO(WORDS,0,5,'E',"MAN.")
LAST_TOOLS:
DOTNAME:    JMP     ENTER
_dotname1:  .WORD   ONEMINUS,DUP,CFETCH,DUP,LIT,127,AND,EMIT
            .WORD       LIT,128,AND,zbranch,_dotname1
            .WORD   DROP,EXIT


