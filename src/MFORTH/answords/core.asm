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
; CORE Words
; ======================================================================

; ----------------------------------------------------------------------
; ! [CORE] 6.1.0010 "store" ( x a-addr -- )
;
; Store x at a-addr.

            LINKTO(LINK_CORE,0,1,'!',"")
STORE:      SAVEDE
            POP     D           ; Pop a-addr
            POP     H           ; Pop x.
            SHLX                ; Store x into a-addr.
            RESTOREDE
            NEXT


; ----------------------------------------------------------------------
; # [CORE] 6.1.0030 "number-sign" ( ud1 -- ud2 )
;
; Divide ud1 by the number in BASE giving the quotient ud2 and the remainder
; n.  (n is the least-significant digit of ud1.)  Convert n to external form
; and add the resulting character to the beginning of the pictured numeric
; output string.  An ambiguous condition exists if # executes outside of a
; <# #> delimited number conversion.
;
; ---
; : # ( ud1 -- ud2 )   BASE @ UD/MOD ROT >digit HOLD ;

            LINKTO(STORE,0,1,'#',"")
NUMSIGN:    JMP     ENTER
            .WORD   BASE,FETCH,UDSLASHMOD,ROT,TODIGIT,HOLD
            .WORD   EXIT


; ----------------------------------------------------------------------
; #> [CORE] 6.1.0040 "number-sign-greater" ( xd -- c-addr u )
;
; Drop xd.  Make the pictured numeric output string available as a character
; string.  c-addr and u specify the resulting character string.  A program
; may replace characters within the string.
;
; ---
; : #> ( xd -- c-addr u ) DROP DROP  HLD @  HERE HLDEND +  OVER - ;

            LINKTO(NUMSIGN,0,2,'>',"#")
NUMSIGNGRTR:JMP     ENTER
            .WORD   DROP,DROP,HLD,FETCH,HERE,LIT,HLDEND,PLUS,OVER,MINUS
            .WORD   EXIT


; ----------------------------------------------------------------------
; #S [CORE] 6.1.0050 "number-sign-s" ( ud1 -- ud2 )
;
; Convert one digit of ud1 according to the rule for #.  Continue conversion
; until the quotient is zero.  ud2 is zero.  An ambiguous condition exists
; if #S executes outside of a <# #> delimited number conversion.
;
; ---
; : #S ( ud1 -- 0 )   BEGIN # 2DUP OR WHILE REPEAT ;

            LINKTO(NUMSIGNGRTR,0,2,'S',"#")
NUMSIGNS:   JMP     ENTER
_numsigns1: .WORD   NUMSIGN,TWODUP,OR,zbranch,_numsigns2,branch,_numsigns1
_numsigns2: .WORD   EXIT


; ----------------------------------------------------------------------
; ' [CORE] 6.1.0070 "tick" ( "<spaces>name" -- xt )
;
; Skip leading space delimiters.  Parse name delimited by a space.  Find
; name and return xt, the execution token for name.  An ambiguous condition
; exists if name is not found.
;
; When interpreting, ' xyz EXECUTE is equivalent to xyz.
;
; ---
; : ' ( "<spaces>name" -- xt)
;   PARSE-WORD (FIND)  0= IF TYPE SPACE [CHAR] ? EMIT CR ABORT THEN ;

            LINKTO(NUMSIGNS,0,1,027h,"")
TICK:       JMP     ENTER
            .WORD   PARSEWORD,PFIND,ZEROEQUALS,zbranch,_tick1
            .WORD   TYPE,SPACE,LIT,'?',EMIT,CR,ABORT
_tick1:     .WORD   EXIT


; ----------------------------------------------------------------------
; ( [CORE] 6.1.0080 "paren"
;
; Compilation:
;   Perform the execution semantics given below.
;
; Execution: ( "ccc<paren>" -- )
;   Parse ccc delimited by ) (right parenthesis).  ( is an immediate word.
;   The number of characters in ccc may be zero to the number of characters
;   in the parse area.
;
; Extended by FILE:
;   When parsing from a text file, if the end of the parse area is reached
;   before a right parenthesis is found, refill the input buffer from the
;   next line of the file, set >IN to zero, and resume parsing, repeating
;   this process until either a right parenthesis is found or the end of the
;   file is reached.
;
; ---
; TODO: Need to implement the extended FILE logic.  I recommend that we modify
;       this code to use REFILL.  We could also avoid PARSE altogether and
;       just go through the input source on our own.  Note that we need to
;       rewrite this in assembly language so that we don't get hit by the
;       perf issues of processing one byte at a time in high-level code.
;
; : ( ( "ccc<quote>" --)   CHAR] ) PARSE 2DROP ;

            LINKTO(TICK,1,1,028h,"")
PAREN:      JMP     ENTER
            .WORD   LIT,029h,PARSE,TWODROP,EXIT


; ----------------------------------------------------------------------
; * [CORE] 6.1.0090 "star" ( n1|u1 n2|u2 -- n3|u3 )
;
; Multiply n1|u1 by n2|u2 giving the product n3|u3.
;
; ---
; : * ( n1|u1 n2|u2 -- n3|u3 )   UM* DROP ;

            LINKTO(PAREN,0,1,'*',"")
STAR:       JMP     ENTER
            .WORD   UMSTAR,DROP,EXIT


; ----------------------------------------------------------------------
; */ [CORE] 6.1.0100 "star-slash" ( n1 n2 n3 -- n4 )
;
; Multiply n1 by n2 producing the intermediate double-cell result d.
; Divide d by n3 giving the single-cell quotient n4.  An ambiguous
; condition exists if n3 is zero or if the quotient n4 lies outside the
; range of a signed number.  If d and n3 differ in sign, the
; implementation-defined result returned will be the same as that returned
; by either the phrase >R M* R> FM/MOD SWAP DROP or the phrase
; >R M* R> SM/REM SWAP DROP.
;
; ---
; : */ ( n1 n2 n3 -- n4)   */MOD NIP ;

            LINKTO(STAR,0,2,'/',"*")
STARSLASH:  JMP     ENTER
            .WORD   STARSLASHMOD,NIP,EXIT


; ----------------------------------------------------------------------
; */MOD [CORE] 6.1.0110 "star-slash-mod" ( n1 n2 n3 -- n4 n5 )
;
; Multiply n1 by n2 producing the intermediate double-cell result d.
; Divide d by n3 producing the single-cell remainder n4 and the
; single-cell quotient n5.  An ambiguous condition exists if n3 is zero,
; or if the quotient n5 lies outside the range of a single-cell signed
; integer.  If d and n3 differ in sign, the implementation-defined result
; returned will be the same as that returned by either the phrase
; >R M* R> FM/MOD or the phrase >R M* R> SM/REM. 
;
; ---
; : */MOD ( n1 n2 n3 -- n4 n5)   >R M* R> SM/REM ;

            LINKTO(STARSLASH,0,5,'D',"OM/*")
STARSLASHMOD:JMP    ENTER
            .WORD   TOR,MSTAR,RFROM,SMSLASHREM,EXIT


; ----------------------------------------------------------------------
; + [CORE] 6.1.0120 "plus" ( n1|u1 n2|u2 -- n3|u3 )
;
; Add n2|u2 to n1|u1, giving the sum n3|u3.

            LINKTO(STARSLASHMOD,0,1,'+',"")
PLUS:       SAVEDE
            POP     D           ; Pop n2|u2.
            POP     H           ; Pop n1|u1.
            DAD     D           ; HL=HL+DE
            PUSH    H           ; Push the result onto the stack.
            RESTOREDE
            NEXT


; ----------------------------------------------------------------------
; +! [CORE] 6.1.0130 "plus-store" ( n|u a-addr -- )
;
; Add n|u to the single-cell number at a-addr.

            LINKTO(PLUS,0,2,'!',"+")
PLUSSTORE:  SAVEDE
            POP     D           ; Pop a-addr.
            POP     H           ; Pop n|u.
            PUSH    B           ; Save BC.
            MOV     B,H         ; Move n|u
            MOV     C,L         ; ..to BC.
            LHLX                ; Fetch the number at a-addr.
            DAD     B           ; Add n|u to the number.
            SHLX                ; Store the updated number.
            POP     B           ; Restore BC.
            RESTOREDE
            NEXT


; ----------------------------------------------------------------------
; +LOOP [CORE] 6.1.0140 "plus-loop"
;
; Interpretation:
;   Interpretation semantics for this word are undefined.
;
; Compilation: ( C: do-sys -- )
;   Append the run-time semantics given below to the current definition.
;   Resolve the destination of all unresolved occurrences of LEAVE between
;   the location given by do-sys and the next location for a transfer of
;   control, to execute the words following +LOOP.  
;
; Run-time: ( n -- ) ( R: loop-sys1 -- | loop-sys2 )
;   An ambiguous condition exists if the loop control parameters are
;   unavailable.  Add n to the loop index.  If the loop index did not cross
;   the boundary between the loop limit minus one and the loop limit,
;   continue execution at the beginning of the loop.  Otherwise, discard the
;   current loop control parameters and continue execution immediately
;   following the loop.
;
; ---
; +LOOP   ['] (pplusloop) END-LOOP ; IMMEDIATE

            LINKTO(PLUSSTORE,1,5,'P',"OOL+")
PLUSLOOP    JMP     ENTER
            .WORD   LIT,pplusloop,ENDLOOP,EXIT


; ----------------------------------------------------------------------
; COMMA [CORE] 6.1.0150 "comma" ( x -- )
;
; Reserve one cell of data space and store x in the cell.  If the
; data-space pointer is aligned when , begins execution, it will remain
; aligned when , finishes execution.  An ambiguous condition exists if the
; data-space pointer is not aligned prior to execution of ,.
;
; ---
; : , ( x -- )   HERE !  1 CELLS ALLOT ;

            LINKTO(PLUSLOOP,0,1,02Ch,"")
COMMA:      JMP     ENTER
            .WORD   HERE,STORE,ONE,CELLS,ALLOT,EXIT


; ----------------------------------------------------------------------
; - [CORE] 6.1.0160 "minus" ( n1|u1 n2|u2 -- n3|u3 )
;
; Subtract n2|u2 from n1|u1, giving the difference n3|u3.

            LINKTO(COMMA,0,1,'-',"")
MINUS:      SAVEDE
            POP     D           ; Pop n2|u2.
            POP     H           ; Pop n1|u1.
            PUSH    B           ; Save BC.
            MOV     B,D         ; Move n2|u2
            MOV     C,E         ; ..to BC.
            DSUB                ; HL=HL-BC
            POP     B           ; Restore BC.
            PUSH    H           ; Push the result onto the stack.
            RESTOREDE
            NEXT


; ----------------------------------------------------------------------
; . [CORE] 6.1.0180 "dot" ( n -- )
;
; Display n in free field format.
;
; ---
; : . ( n -- )
;   BASE @ 10 <>  IF U. EXIT THEN
;   DUP ABS 0 <# #S ROT SIGN #> TYPE SPACE ;

            LINKTO(MINUS,0,1,'.',"")
DOT:        JMP     ENTER
            .WORD   BASE,FETCH,LIT,10,NOTEQUALS,zbranch,_dot1,UDOT,EXIT
_dot1:      .WORD   DUP,ABS,ZERO,LESSNUMSIGN,NUMSIGNS,ROT,SIGN,NUMSIGNGRTR
            .WORD   TYPE,SPACE
            .WORD   EXIT


; ----------------------------------------------------------------------
; ." [CORE] 6.1.0190 "dot-quote"
;
; Interpretation:
;   Interpretation semantics for this word are undefined.
;
; Compilation: ( "ccc<quote>" -- )
;   Parse ccc delimited by " (double-quote).  Append the run-time
;   semantics given below to the current definition.
;
; Run-time: ( -- )
;   Display ccc.
;
; ---
; : ." ( "ccc<quote>" --)   POSTPONE S" POSTPONE TYPE ; IMMEDIATE

            LINKTO(DOT,1,2,022h,".")
DOTQUOTE:   JMP     ENTER
            .WORD   SQUOTE,LIT,TYPE,COMPILECOMMA,EXIT


; ----------------------------------------------------------------------
; / [CORE] 6.1.0230 "slash" ( n1 n2 -- n3 )
;
; Divide n1 by n2, giving the single-cell quotient n3.  An ambiguous
; condition exists if n2 is zero.  If n1 and n2 differ in sign, the
; implementation-defined result returned will be the same as that returned
; by either the phrase >R S>D R> FM/MOD SWAP DROP or the phrase
; >R S>D R> SM/REM SWAP DROP.
;
; ---
; : / ( n1 n2 -- n3)   /MOD NIP ;

            LINKTO(DOTQUOTE,0,1,'/',"")
SLASH:      JMP     ENTER
            .WORD   SLASHMOD,NIP,EXIT


; ----------------------------------------------------------------------
; /MOD [CORE] 6.1.0240 "slash-mod" ( n1 n2 -- n3 n4 )
;
; Divide n1 by n2, giving the single-cell remainder n3 and the single-cell
; quotient n4.  An ambiguous condition exists if n2 is zero.  If n1 and n2
; differ in sign, the implementation-defined result returned will be the
; same as that returned by either the phrase >R S>D R> FM/MOD or the phrase
; >R S>D R> SM/REM.
;
; ---
; : /MOD ( n1 n2 -- n3 n4)   >R S>D R> SM/REM ;

            LINKTO(SLASH,0,4,'D',"OM/")
SLASHMOD:   JMP     ENTER
            .WORD   TOR,STOD,RFROM,SMSLASHREM,EXIT


; ----------------------------------------------------------------------
; 0< [CORE] 6.1.0250 "zero-less" ( b -- flag )
;
; flag is true if and only if n is less than zero.

            LINKTO(SLASHMOD,0,2,'<',"0")
ZEROLESS:   POP     H           ; Pop the value.
            MOV     A,H         ; See if the number is < 0 by moving H to A
            ORA     A           ; ..and then ORing A with itself.
            JP      _zlessFALSE ; Jump if positive to where we push false.
            LXI     H,0FFFFh    ; Put true in HL.
            JMP     _zlessDONE  ; We're done.
_zlessFALSE:LXI     H,0         ; Put false in HL.
_zlessDONE: PUSH    H           ; Push the flag to the stack.
            NEXT


; ----------------------------------------------------------------------
; 0= [CORE] 6.1.0270 "zero-equals" ( x -- flag )
;
; flag is true if and only if x is equal to zero.

            LINKTO(ZEROLESS,0,2,'=',"0")
ZEROEQUALS: POP     H           ; Pop the value.
            MOV     A,H         ; See if the flag is zero by moving H to A
            ORA     L           ; ..and then ORing A with L.
            JNZ     _zeqFALSE   ; Jump if not zero to where we push false.
            LXI     H,0FFFFh    ; Put true in HL.
            JMP     _zeqDONE    ; We're done.
_zeqFALSE:  LXI     H,0         ; Put false in HL.
_zeqDONE:   PUSH    H           ; Push the flag to the stack.
            NEXT


; ----------------------------------------------------------------------
; 1+ [CORE] 6.1.0290 "one-plus" ( n1|u1 -- n2|u2 )
;
; Add one (1) to n1|u1 giving the sum n2|u2.

            LINKTO(ZEROEQUALS,0,2,'+',"1")
ONEPLUS:    POP     H           ; Pop the value.
            INX     H           ; Increment the value.
            PUSH    H           ; Push the result onto the stack.
            NEXT


; ----------------------------------------------------------------------
; 1- [CORE] 6.1.0300 "one-minus" ( n1|u1 -- n2|u2 )
;
; Subtract one (1) from n1|u1 giving the difference n2|u2.

            LINKTO(ONEPLUS,0,2,'-',"1")
ONEMINUS:   POP     H           ; Pop the value.
            DCX     H           ; Decrement the value.
            PUSH    H           ; Push the result onto the stack.
            NEXT


; ----------------------------------------------------------------------
; 2! [CORE] 6.1.0310 "two-store" ( x1 x2 a-addr -- )
;
; Store the cell pair x1 x2 at a-addr, with x2 at a-addr and x1 at the
; next consecutive cell.  It is equivalent to the sequence
; SWAP OVER ! CELL+ !.

            LINKTO(ONEMINUS,0,2,'!',"2")
TWOSTORE:   SAVEDE
            POP     D           ; Pop a-addr.
            POP     H           ; Pop x2
            SHLX                ; Save x2.
            INX     D           ; Increment to the
            INX     D           ; ..next cell.
            POP     H           ; Pop x1.
            SHLX                ; Save x1.
            RESTOREDE
            NEXT


; ----------------------------------------------------------------------
; 2* [CORE] 6.1.0320 "two-star" ( x1 -- x2 )
;
; x2 is the result of shifting x1 one bit toward the most-significant bit,
; filling the vacated least-significant bit with zero.

            LINKTO(TWOSTORE,0,2,'*',"2")
TWOSTAR:    POP     H           ; Pop x1.
            DAD     H           ; Double x1.
            PUSH    H           ; Push the result onto the stack.
            NEXT


; ----------------------------------------------------------------------
; 2/ [CORE] 6.1.0330 "two-slash" ( x1 -- x2 )
;
; x2 is the result of shifting x1 one bit toward the least-significant bit,
; leaving the most-significant bit unchanged.

            LINKTO(TWOSTAR,0,2,'/',"2")
TWOSLASH:   POP     H           ; Pop x1.
            ANA     A           ; Clear the carry flag.
            MOV     A,H         ; Move the high byte into A,
            RLC                 ; ..rotate it left
            RRC                 ; ..and then right through carry, then
            RAR                 ; ..divide the high byte,
            MOV     H,A         ; ..and put the high byte back into H.
            MOV     A,L         ; Move the low byte into A,
            RAR                 ; ..divide the low byte,
            MOV     L,A         ; ..and put the low byte back into H.
            PUSH    H           ; Push the result onto the stack.
            NEXT


; ----------------------------------------------------------------------
; 2@ [CORE] 6.1.0350 "two-fetch" ( a-addr -- x1 x2 )
;
; Fetch the cell pair x1 x2 stored at a-addr.  x2 is stored at a-addr
; and x1 at the next consecutive cell.  It is equivalent to the sequence
; DUP CELL+ @ SWAP @.

            LINKTO(TWOSLASH,0,2,'@',"2")
TWOFETCH:   SAVEDE
            POP     D           ; Pop a-addr.
_twofetchDE:LHLX                ; Fetch x2.
            PUSH    H           ; Push x2 (which is wrong, but we'll fix it).
            INX     D           ; Increment
            INX     D           ; ..to x1,
            LHLX                ; ..and fetch x1.
            XTHL                ; Swap TOS (x2) with x1.
            PUSH    H           ; Push x2.
            RESTOREDE
            NEXT


; ----------------------------------------------------------------------
; 2DROP [CORE] 6.1.0370 "two-drop" ( x1 x2 -- )
;
; Drop cell pair x1 x2 from the stack.

            LINKTO(TWOFETCH,0,5,'P',"ORD2")
TWODROP:    POP     H
            POP     H
            NEXT


; ----------------------------------------------------------------------
; 2DUP [CORE] 6.1.0380 "two-dupe" ( x1 x2 -- x1 x2 x1 x2 )
;
; Duplicate cell pair x1 x2.

            LINKTO(TWODROP,0,4,'P',"UD2")
TWODUP:     SAVEDE
            POP     H           ; Pop x2.
            POP     D           ; Pop x1.
            PUSH    D           ; Push x1 back onto the stack.
            PUSH    H           ; Push x2 back onto the stack.
            PUSH    D           ; Push another copy of x1 onto the stack.
            PUSH    H           ; Push another copy of x2 onto the stack.
            RESTOREDE
            NEXT


; ----------------------------------------------------------------------
; 2OVER [CORE] 6.1.0400 "two-over" ( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 )
;
; Copy cell pair x1 x2 to the top of the stack.

            LINKTO(TWODUP,0,5,'R',"EVO2")
TWOOVER:    SAVEDE
            LDES    6           ; Get the address of the fourth stack item.
            LHLX                ; Load the fourth stack item into HL.
            PUSH    H           ; Push the fourth stack item onto the stack.
            LDES    6           ; Get the address of the third (now fourth) stack item.
            LHLX                ; Load the third stack item into HL.
            PUSH    H           ; Push the third stack item onto the stack.
            RESTOREDE
            NEXT


; ----------------------------------------------------------------------
; 2SWAP [CORE] 6.1.0430 "two-swap" ( x1 x2 x3 x4 -- x3 x4 x1 x2 )
;
; Exchange the top two cell pairs.

            LINKTO(TWOOVER,0,5,'P',"AWS2")
TWOSWAP:    SAVEDE
            POP     H           ; Pop x4.
            POP     D           ; Pop x3.
            XTHL                ; Swap x4 with x2.
            XCHG                ; Put x2 in DE, x3 in HL.
            DI                  ; Disable interrupts while we mess with SP.
            INX     SP          ; Increment SP
            INX     SP          ; ..to x1.
            XTHL                ; Swap x3 with x1.
            DCX     SP          ; Decrement back
            DCX     SP          ; ..to x4.
            EI                  ; Enable interrupts now that we're done with SP.
            XCHG                ; Put x1 in DE, x2 in HL.
            PUSH    D           ; Push x1.
            PUSH    H           ; Push x2.
            RESTOREDE
            NEXT


; ----------------------------------------------------------------------
; : [CORE] 6.1.0450 "colon" ( C: "<spaces>name" -- colon-sys )
;
; Skip leading space delimiters.  Parse name delimited by a space.  Create
; a definition for name, called a "colon definition".  Enter compilation
; state and start the current definition, producing colon-sys.  Append the
; initiation semantics given below to the current definition.
;
; The execution semantics of name will be determined by the words compiled
; into the body of the definition.  The current definition shall not be
; findable in the dictionary until it is ended (or until the execution of
; DOES> in some systems).
;
; Initiation: ( i*x -- i*x ) ( R: -- nest-sys )
;   Save implementation-dependent information nest-sys about the calling
;   definition.  The stack effects i*x represent arguments to name.
;
; name Execution: ( i*x -- j*x )
;       Execute the definition name.  The stack effects i*x and j*x
;       represent arguments to and results from name, respectively.

; ---
; : : ( "<spaces>name" -- )
;   CREATE HIDE ]  CFASZ NEGATE ALLOT  195 C, DOCOLON , ; -- JMP DOCOLON

            LINKTO(TWOSWAP,0,1,03Ah,"")
COLON:      JMP     ENTER
            .WORD   CREATE,HIDE,RTBRACKET
            .WORD   LIT,-CFASZ,ALLOT,LIT,195,CCOMMA,LIT,DOCOLON,COMMA
            .WORD   EXIT


; ----------------------------------------------------------------------
; ; [CORE] 6.1.0460 "semicolon"
;
; Interpretation:
;   Interpretation semantics for this word are undefined.
;
; Compilation: ( C: colon-sys -- )
;   Append the run-time semantics below to the current definition.  End
;   the current definition, allow it to be found in the dictionary and
;   enter interpretation state, consuming colon-sys.  If the data-space
;   pointer is not aligned, reserve enough data space to align it.
;
; Run-time: ( -- ) ( R: nest-sys -- )
;   Return to the calling definition specified by nest-sys.
;
; ---
; : ; ( -- )   REVEAL  ['] EXIT COMPILE,  POSTPONE [ ; IMMEDIATE

            LINKTO(COLON,1,1,';',"")
SEMICOLON:  JMP     ENTER
            .WORD   REVEAL,LIT,EXIT,COMPILECOMMA,LTBRACKET,EXIT


; ----------------------------------------------------------------------
; < [CORE] 6.1.0480 "less-than" ( n1 n2 -- flag )
;
; flag is true if and only if n1 is less than n2.

            LINKTO(SEMICOLON,0,1,'<',"")
LESSTHAN:   SAVEDE
            POP     D           ; Pop n2.
            POP     H           ; Pop n1.
            MOV     A,D         ; Put n2's high byte into A,
            XRA     H           ; ..XOR that with n1's high byte,
            JM      _lt1        ; ..then skip the DSUB if the signs differ.
            PUSH    B           ; Save BC.
            MOV     B,D         ; Move n2
            MOV     C,E         ; ..to BC.
            DSUB                ; HL=n1-n2
            POP     B           ; Restore BC.
_lt1:       INR     H           ; Increment HL,
            DCR     H           ; ..then decrement HL to check the sign;
            JM      _ltTRUE     ; ..n1 < n2 if HL is negative.
            LXI     H,0         ; Put false in HL.
            JMP     _ltDONE     ; We're done.
_ltTRUE:    LXI     H,0FFFFh    ; Put true in HL.
_ltDONE:    PUSH    H           ; Push the flag to the stack.
            RESTOREDE
            NEXT


; ----------------------------------------------------------------------
; <# [CORE] 6.1.0490 "less-number-sign" ( -- )
;
; Initialize the pictured numeric output conversion process.
;
; ---
; : <# ( -- )   HERE HLDEND + HLD ! ;

            LINKTO(LESSTHAN,0,2,'#',"<")
LESSNUMSIGN:JMP     ENTER
            .WORD   HERE,LIT,HLDEND,PLUS,HLD,STORE
            .WORD   EXIT


; ----------------------------------------------------------------------
; = [CORE] 6.2.0530 "equals" ( x1 x2 -- flag )
;
; flag is true if and only if x1 is bit-for-bit the same as x2.

            LINKTO(LESSNUMSIGN,0,1,'=',"")
EQUALS:     SAVEDE
            POP     H           ; Pop x2.
            POP     D           ; Pop x1.
            PUSH    B           ; Save BC.
            MOV     B,D         ; Move x1
            MOV     C,E         ; ..to BC.
            DSUB                ; HL=HL-BC
            POP     B           ; Restore BC.
            JNZ     _eqFALSE    ; Jump if not equals to where we push false.
            LXI     H,0FFFFh    ; Put true in HL.
            JMP     _eqDONE     ; We're done.
_eqFALSE:   LXI     H,0         ; Put false in HL.
_eqDONE:    PUSH    H           ; Push the flag to the stack.
            RESTOREDE
            NEXT


; ----------------------------------------------------------------------
; > [CORE] 6.1.0540 "greater-than" ( n1 n2 -- flag )
;
; flag is true if and only if n1 is greater than n2.
;
; ---
; : > ( n1 n2 -- flag)   SWAP < ;

            LINKTO(EQUALS,0,1,'>',"")
GREATERTHAN:JMP     ENTER
            .WORD   SWAP,LESSTHAN,EXIT


; ----------------------------------------------------------------------
; >BODY [CORE] 6.1.0550 "to-body" ( xt -- a-addr )
;
; a-addr is the data-field address corresponding to xt.  An ambiguous
; condition exists if xt is not for a word defined via CREATE.
;
; ---
; : >BODY ( xt -- a-addr)   CFASZ + ;

            LINKTO(GREATERTHAN,0,5,'Y',"DOB>")
TOBODY:     JMP     ENTER
            .WORD   LIT,CFASZ,PLUS,EXIT


; ----------------------------------------------------------------------
; >IN [CORE] 6.1.0560 "to-in" ( -- a-addr )
;
; a-addr is the address of a cell containing the offset in characters
; from the start of the input buffer to the start of the parse area.
;
; ---
; : >IN ( -- a-addr)  ICB ICBTOIN + ;

            LINKTO(TOBODY,0,3,'N',"I>")
TOIN:       JMP     ENTER
            .WORD   ICB,LIT,ICBTOIN,PLUS,EXIT


; ----------------------------------------------------------------------
; >NUMBER [CORE] 6.1.0567 "to-number" ( ud1 c-addr1 u1 -- ud2 c-addr2 u2 )
;
; ud2 is the unsigned result of converting the characters within the string
; specified by c-addr1 u1 into digits, using the number in BASE, and adding
; each into ud1 after multiplying ud1 by the number in BASE.  Conversion
; continues left-to-right until a character that is not convertible,
; including any "+" or "-", is encountered or the string is entirely
; converted.  c-addr2 is the location of the first unconverted character or
; the first character past the end of the string if the string was entirely
; converted.  u2 is the number of unconverted characters in the string.  An
; ambiguous condition exists if ud2 overflows during the conversion.
;
; ---
; : >NUMBER ( ud1 c-addr1 u1 -- ud2 c-addr2 u2)
;   2>B  BEGIN B? WHILE
;       B@ DIGIT? 0= IF B B# EXIT THEN
;       ( ud1 u) >R BASE @ UD* R> M+
;   B+ AGAIN  B B# ;

            LINKTO(TOIN,0,7,'R',"EBMUN>")
TONUMBER:   JMP     ENTER
            .WORD   TWOTOB
_tonumber1: .WORD   BQUES,zbranch,_tonumber3
            .WORD   BFETCH,DIGITQ,ZEROEQUALS,zbranch,_tonumber2
            .WORD   B,BNUMBER,EXIT
_tonumber2: .WORD   TOR,BASE,FETCH,UDSTAR,RFROM,MPLUS,BPLUS,branch,_tonumber1
_tonumber3: .WORD   B,BNUMBER
            .WORD   EXIT


; ----------------------------------------------------------------------
; >R [CORE] 6.1.0580 "to-r"
;
; Interpretation:
;   Interpretation semantics for this word are undefined.
;
; Execution: ( x -- ) ( R: -- x )
;   Move x to the return stack.

            LINKTO(TONUMBER,0,2,'R',">")
TOR:        POP     H
            RSPUSH(H,L)
            NEXT


; ----------------------------------------------------------------------
; ?DUP [CORE] 6.1.0630 "question-dupe" ( x -- 0 | x x )
;
; Duplicate x if it is non-zero.

            LINKTO(TOR,0,4,'P',"UD?")
QDUP:       POP     H           ; Pop x into HL.
            MOV     A,H         ; See if the value is zero by moving H to A
            ORA     L           ; ..and then ORing A with L.
            JZ      _qdupONCE   ; Jump if zero to where we push once.
            PUSH    H           ; Push the value (this is the second copy).
_qdupONCE:  PUSH    H           ; Push the value.
            NEXT


; ----------------------------------------------------------------------
; @ [CORE] 6.1.0650 "fetch" ( a-addr -- x )
;
; x is the value stored at a-addr.

            LINKTO(QDUP,0,1,'@',"")
FETCH:      POP     H           ; Pop address to fetch into HL
            MOV     A,M         ; Load LSB of cell value into A
            INX     H           ; Increment to MSB of the cell value
            MOV     H,M         ; Load MSB of the cell value into H
            MOV     L,A         ; Move LSB of cell value from A to L
            PUSH    H           ; Push cell value onto stack.
            NEXT


; ----------------------------------------------------------------------
; ABORT [CORE] 6.1.0670 ( i*x -- ) ( R: j*x -- )
;
; Empty the data stack and perform the function of QUIT, which includes
; emptying the return stack, without displaying a message.
;
; ---
; : ABORT ( i*x -- ) ( R: j*x -- )
;   TASK-PAGE [HEX] FF OR SP!  10 BASE !
;   TASK-PAGE 'FIRSTTASK @ = IF ONLY QUIT ELSE ['] BL STOPPED THEN ;
;
; Our multitasking-aware version of ABORT enters the QUIT loop if this
; is the initial task, otherwise the STOPPED loop is invoked and the
; task effectively becomes inert.  STOPPED needs an xt to call, so we
; give it BL.  That puts a value on the stack, but since STOPPED will
; never exit the value won't bother anyone.
;
; Note that ABORT will also (re-)initialize the search order if it is
; called from the initial task.
;
; The idle word should never return, but we HALT anyway just in case
; someone messes with the return stack.

            LINKTO(FETCH,0,5,'T',"ROBA")
ABORT:      JMP     ENTER
            .WORD   TASKPAGE,LIT,0ffh,OR,SPSTORE
            .WORD   LIT,10,BASE,STORE
            .WORD   TASKPAGE,LIT,TICKFIRSTTASK,FETCH,EQUALS,zbranch,_abort1
            .WORD   ONLY,QUIT,HALT
_abort1:    .WORD   LIT,BL,STOPPED,HALT


; ----------------------------------------------------------------------
; ABORT" [CORE] 6.1.0680 "abort-quote"
;
; Interpretation:
;   Interpretation semantics for this word are undefined.
;
; Compilation: ( "ccc<quote>" -- )
;   Parse ccc delimited by a " (double-quote).  Append the run-time
;   semantics given below to the current definition.
;
; Run-time: ( i*x x1 --  | i*x ) ( R: j*x --  | j*x )
;   Remove x1 from the stack.  If any bit of x1 is not zero, display ccc
;   and perform an implementation-defined abort sequence that includes the
;   function of ABORT.
;
; ---
; : ABORT"   POSTPONE IF POSTPONE ." POSTPONE ABORT THEN ; IMMEDIATE

            LINKTO(ABORT,1,6,022h,"TROBA")
ABORTQUOTE: JMP     ENTER
            .WORD   IF,DOTQUOTE,LIT,ABORT,COMPILECOMMA,THEN,EXIT


; ----------------------------------------------------------------------
; ABS [CORE] 6.1.0690 "abs" ( n -- u )
;
; u is the absolute value of n.
;
; ---
; : ABS ( n -- u )   DUP ?NEGATE ;

            LINKTO(ABORTQUOTE,0,3,'S',"BA")
ABS:        JMP     ENTER
            .WORD   DUP,QNEGATE,EXIT


; ----------------------------------------------------------------------
; ACCEPT [CORE] 6.1.0695 ( c-addr +n1 -- +n2 )
;
; Receive a string of at most +n1 characters.  An ambiguous condition
; exists if +n1 is zero or greater than 32,767.  Display graphic
; characters as they are received.  A program that depends on the
; presence or absence of non-graphic characters in the string has an
; environmental dependency.  The editing functions, if any, that the
; system performs in order to construct the string are
; implementation-defined.
;
; Input terminates when an implementation-defined line terminator is
; received.  When input terminates, nothing is appended to the string,
; and the display is maintained in an implementation-defined way.
;
; +n2 is the length of the string stored at c-addr.
;
; ---
; : ACCEPT ( c-addr max -- n)
;   2DUP 2>B DROP  ( ca-start)
;   BEGIN  KEY  DUP 13 <> WHILE
;       DUP 8 = IF
;           ( ca-start bs) DROP  B OVER - IF 8 EMIT BL EMIT 8 EMIT -1 'B +! THEN
;       ELSE
;           B? IF DUP EMIT B!+ ELSE DROP THEN
;       THEN
;   REPEAT
;   ( ca-start cr) DROP  B SWAP - ;

            LINKTO(ABS,0,6,'T',"PECCA")
ACCEPT:     JMP     ENTER
            .WORD   TWODUP,TWOTOB,DROP
_accept1:   .WORD   KEY,DUP,LIT,13,NOTEQUALS,zbranch,_accept5
            .WORD   DUP,LIT,8,EQUALS,zbranch,_accept2
            .WORD   DROP,B,OVER,MINUS,zbranch,_accept4
            .WORD   LIT,8,EMIT,BL,EMIT,LIT,8,EMIT
            .WORD       LIT,-1,TICKB,PLUSSTORE,branch,_accept4
_accept2:   .WORD   BQUES,zbranch,_accept3,DUP,EMIT,BSTOREPLUS,branch,_accept4
_accept3:   .WORD   DROP
_accept4:   .WORD   branch,_accept1
_accept5:   .WORD   DROP,B,SWAP,MINUS
            .WORD   EXIT


; ----------------------------------------------------------------------
; ALIGN [CORE] 6.1.0705 ( -- )
;
; If the data-space pointer is not aligned, reserve enough space to align it.

            LINKTO(ACCEPT,0,5,'N',"GILA")
ALIGN:      NEXT                ; No-op in MFORTH; no alignment needed.


; ----------------------------------------------------------------------
; ALIGNED [CORE] 6.1.0706 ( addr -- a-addr )
;
; a-addr is the first aligned address greater than or equal to addr.

            LINKTO(ALIGN,0,7,'D',"ENGILA")
ALIGNED:    NEXT                ; No-op in MFORTH; no alignment needed.


; ----------------------------------------------------------------------
; ALLOT [CORE] 6.1.0710 ( n -- )
;
; If n is greater than zero, reserve n address units of data space.  If
; n is less than zero, release |n| address units of data space.  If n is
; zero, leave the data-space pointer unchanged.
;
; If the data-space pointer is aligned and n is a multiple of the size of
; a cell when ALLOT begins execution, it will remain aligned when ALLOT
; finishes execution.
;
; If the data-space pointer is character aligned and n is a multiple of
; the size of a character when ALLOT begins execution, it will remain
; character aligned when ALLOT finishes execution.
;
; ---
; : ALLOT ( n -- )   DP +! ;

            LINKTO(ALIGNED,0,5,'T',"OLLA")
ALLOT:      JMP     ENTER
            .WORD   LIT,DP,PLUSSTORE,EXIT


; ----------------------------------------------------------------------
; AND [CORE] 6.1.0720 ( x1 x2 -- x3 )
;
; x3 is the bit-by-bit logical "and" of x1 with x2.

            LINKTO(ALLOT,0,3,'D',"NA")
AND:        SAVEDE
            POP     H           ; Pop x2.
            POP     D           ; Pop x1.
            MOV     A,H         ; Put x2's high byte into A,
            ANA     D           ; ..then AND x1's high byte with A,
            MOV     H,A         ; ..and put the result into H.
            MOV     A,L         ; Put x2's low byte into A,
            ANA     E           ; ..then AND x1's low byte with A,
            MOV     L,A         ; ..and put the result into L.
            PUSH    H           ; Push the result (HL).
            RESTOREDE
            NEXT


; ----------------------------------------------------------------------
; BASE [CORE] 6.1.0750 ( -- a-addr )
;
; a-addr is the address of a cell containing the current number-conversion
; radix {{2...36}}.

            LINKTO(AND,0,4,'E',"SAB")
BASE:       JMP     DOUSER
            .BYTE   USERBASE


; ----------------------------------------------------------------------
; BEGIN [CORE] 6.1.0760
;
; Interpretation:
;   Interpretation semantics for this word are undefined.
;
; Compilation: ( C: -- dest )
;   Put the next location for a transfer of control, dest, onto the control
;   flow stack.  Append the run-time semantics given below to the current
;   definition.
;
; Run-time: ( -- )
;   Continue execution.
;
; ---
; : BEGIN   HERE ; IMMEDIATE

            LINKTO(BASE,1,5,'N',"IGEB")
BEGIN:      JMP     ENTER
            .WORD   HERE,EXIT


; ----------------------------------------------------------------------
; BL [CORE] 6.1.0770 "b-l" ( -- char )
;
; char is the character value for a space.

            LINKTO(BEGIN,0,2,'L',"B")
BL:         LXI     H,020h
            PUSH    H
            NEXT


; ----------------------------------------------------------------------
; C! [CORE] 6.1.0850 "c-store" ( char c-addr -- )
;
; Store char at c-addr.  When character size is smaller than cell size,
; only the number of low-order bits corresponding to character size are
; transferred.

            LINKTO(BL,0,2,'!',"C")
CSTORE:     SAVEDE
            POP     H           ; Pop c-addr.
            POP     D           ; Pop char
            MOV     M,E         ; Store LSB of char value.
            RESTOREDE
            NEXT


; ----------------------------------------------------------------------
; C, [CORE] 6.1.0860 "c-comma" ( char -- )
;
; Reserve space for one character in the data space and store char in the
; space.  If the data-space pointer is character aligned when C, begins
; execution, it will remain character aligned when C, finishes execution.
; An ambiguous condition exists if the data-space pointer is not
; character-aligned prior to execution of C,.
;
; ---
; : C, ( char -- )   HERE C!  1 CHARS ALLOT ;

            LINKTO(CSTORE,0,2,02Ch,"C")
CCOMMA:     JMP     ENTER
            .WORD   HERE,CSTORE,ONE,CHARS,ALLOT,EXIT


; ----------------------------------------------------------------------
; C@ [CORE] 6.1.0870 "c-fetch" ( c-addr -- char )
;
; Fetch the character stored at c-addr.  When the cell size is greater
; than character size, the unused high-order bits are all zeroes.

            LINKTO(CCOMMA,0,2,'@',"C")
CFETCH:     POP     H           ; Pop address to fetch into HL.
            MOV     L,M         ; Load character into low byte of HL.
            MVI     H,0         ; Clear high byte of HL.
            PUSH    H           ; Push character value onto stack.
            NEXT


; ----------------------------------------------------------------------
; CELL+ [CORE] 6.1.0880 "cell-plus" ( a-addr1 -- a-addr2 )
;
; Add the size in address units of a cell to a-addr1, giving a-addr2.

            LINKTO(CFETCH,0,5,'+',"LLEC")
CELLPLUS:   POP     H           ; Pop a-addr1.
            INX     H           ; Add two (the size of a cell)
            INX     H           ; ..to a-addr1.
            PUSH    H           ; Push the result to the stack.
            NEXT


; ----------------------------------------------------------------------
; CELLS [CORE] 6.1.0890 ( n1 -- n2 )
;
; n2 is the size in address units of n1 cells.

            LINKTO(CELLPLUS,0,5,'S',"LLEC")
CELLS:      POP     H           ; Pop x1.
            DAD     H           ; Double x1 (cells are two bytes wide).
            PUSH    H           ; Push the result onto the stack.
            NEXT


; ----------------------------------------------------------------------
; CHAR [CORE] 6.1.0895 "char" ( "<spaces>name" -- char )
;
; Skip leading space delimiters.  Parse name delimited by a space.  Put
; the value of its first character onto the stack.
;
; ---
; : CHAR ( "<spaces>name" -- char)   PARSE-WORD DROP C@ ;

            LINKTO(CELLS,0,4,'R',"AHC")
CHAR:       JMP     ENTER
            .WORD   PARSEWORD,DROP,CFETCH,EXIT


; ----------------------------------------------------------------------
; CHAR+ [CORE] 6.1.0897 "char-plus" ( c-addr1 -- c-addr2 )
;
; Add the size in address units of a character to c-addr1, giving c-addr2.

            LINKTO(CHAR,0,5,'+',"RAHC")
CHARPLUS:   POP     H           ; Pop c-addr1.
            INX     H           ; Add one (the size of a char) to c-addr1.
            PUSH    H           ; Push the result to the stack.
            NEXT


; ----------------------------------------------------------------------
; CHARS [CORE] 6.1.0898 "chars" ( n1 -- n2 )
;
; n2 is the size in address units of n1 characters.

            LINKTO(CHARPLUS,0,5,'S',"RAHC")
CHARS:      NEXT                ; No-op in MFORTH, because chars are 1 byte.


; ----------------------------------------------------------------------
; CONSTANT [CORE] 6.1.0950 ( x "<spaces>name" -- )
;
; Skip leading space delimiters.  Parse name delimited by a space.  Create
; a definition for name with the execution semantics defined below.
;
; name is referred to as a "constant".
;
; name Execution: ( -- x )
;   Place x on the stack.
;
; ---
; : CONSTANT ( x "<spaces>name" -- )
;   CREATE  CFASZ NEGATE ALLOT  195 C, DOCONSTANT ,  , ; -- JMP DOCONSTANT

            LINKTO(CHARS,0,8,'T',"NATSNOC")
CONSTANT:   JMP     ENTER
            .WORD   CREATE,LIT,-CFASZ,ALLOT,LIT,195,CCOMMA,LIT,DOCONSTANT,COMMA
            .WORD   COMMA,EXIT


; ----------------------------------------------------------------------
; COUNT [CORE] 6.1.0980 ( c-addr1 -- c-addr2 u )
;
; Return the character string specification for the counted string stored
; at c-addr1.  c-addr2 is the address of the first character after c-addr1.
; u is the contents of the character at c-addr1, which is the length in
; characters of the string at c-addr2.

            LINKTO(CONSTANT,0,5,'T',"NUOC")
COUNT:      POP     H           ; Pop the address into HL.
            MOV     A,M         ; Fetch the string count into A.
            INX     H           ; Increment HL to the address of the string.
            PUSH    H           ; Push the address of the string to the stack.
            MVI     H,0         ; Clear the high byte of HL,
            MOV     L,A         ; ..set the low byte to the count,
            PUSH    H           ; ..and push the count to the stack.
            NEXT


; ----------------------------------------------------------------------
; CR [CORE] 6.1.0990 "c-r" ( -- )
;
; Cause subsequent output to appear at the beginning of the next line.

            LINKTO(COUNT,0,2,'R',"C")
CR:         CALL    STDCALL     ; Call the
            .WORD   04222h      ; .."Send CRLF" routine.
            NEXT


; ----------------------------------------------------------------------
; CREATE [CORE] 6.1.1000 ( "<spaces>name" -- )
;
; Skip leading space delimiters.  Parse name delimited by a space.  Create
; a definition for name with the execution semantics defined below.  If
; the data-space pointer is not aligned, reserve enough data space to
; align it.  The new data-space pointer defines name's data field.  CREATE
; does not allocate data space in name's data field.
;
;   name Execution: ( -- a-addr )
;       a-addr is the address of name's data field.  The execution
;       semantics of name may be extended by using DOES>.
;
; ---
; : CREATE ( "<spaces>name" -- )
;   PARSE-WORD  DUP 0= IF ABORT THEN DUP 63 > IF ABORT THEN
;   2>B  B# 1+ ALLOT  HERE 1-  B# OVER C!
;   FORB 1- B@ OVER C! NEXTB  DUP C@ 128 OR SWAP C!
;   LATEST @ ,  [ PROFILER ] [IF] 0 , [THEN]
;   HERE NFATOCFASZ - LATEST !  195 C, DOCREATE , -- JMP DOCREATE
;

            LINKTO(CR,0,6,'E',"TAERC")
CREATE:     JMP     ENTER
            .WORD   PARSEWORD,DUP,ZEROEQUALS,zbranch,_create1,ABORT
_create1:   .WORD   DUP,LIT,63,GREATERTHAN,zbranch,_create2,ABORT
_create2:   .WORD   TWOTOB,BNUMBER,ONEPLUS,ALLOT,HERE,ONEMINUS
            .WORD       BNUMBER,OVER,CSTORE
_create3:   .WORD   BQUES,zbranch,_create4,ONEMINUS,BFETCH,OVER,CSTORE
            .WORD       BPLUS,branch,_create3
_create4:   .WORD   DUP,CFETCH,LIT,128,OR,SWAP,CSTORE
            .WORD   LATEST,FETCH,COMMA
#IFDEF PROFILER
            .WORD   ZERO,COMMA
#ENDIF
            .WORD   HERE,LIT,NFATOCFASZ,MINUS,LATEST,STORE
            .WORD       LIT,195,CCOMMA,LIT,DOCREATE,COMMA
            .WORD   EXIT


; ----------------------------------------------------------------------
; DECIMAL [CORE] 6.1.1170 ( -- )
;
; Set the numeric conversion radix to ten (decimal).

            LINKTO(CREATE,0,7,'L',"AMICED")
DECIMAL:    JMP     ENTER
            .WORD   LIT,10,BASE,STORE,EXIT


; ----------------------------------------------------------------------
; DEPTH [CORE] 6.1.1200 ( -- +n )
;
; +n is the number of single-cell values contained in the data stack
; before +n was placed on the stack.
;
; ---
; : DEPTH ( -- +n)   SP  TASK-PAGE [HEX] FF OR  SWAP - 2/ ;

            LINKTO(DECIMAL,0,5,'H',"TPED")
DEPTH:      JMP     ENTER
            .WORD   SP,TASKPAGE,LIT,0ffh,OR,SWAP,MINUS,TWOSLASH,EXIT


; ----------------------------------------------------------------------
; DO [CORE] 6.1.1240
;
; Interpretation:
;   Interpretation semantics for this word are undefined.
;
; Compilation: ( C: -- do-sys )
;   Place do-sys onto the control-flow stack.  Append the run-time
;   semantics given below to the current definition.  The semantics are
;   incomplete until resolved by a consumer of do-sys such as LOOP.
;
; Run-time: ( n1|u1 n2|u2 -- ) ( R: -- loop-sys )
;   Set up loop control parameters with index n2|u2 and limit n1|u1. An
;   ambiguous condition exists if n1|u1 and n2|u2 are not both the same
;   type.  Anything already on the return stack becomes unavailable until
;   the loop-control parameters are discarded.
;
; ---
; do-sys in MFORTH is ( do-orig ).  LEAVE locations chain from the most
; recent LEAVE to the oldest LEAVE and then to zero, which signifies the
; end of the LEAVE list.  LOOP/+LOOP go through the LEAVE list and fix
; up the addresses.
;
; : DO   0 'PREVLEAVE !  ['] (do) COMPILE,  HERE ; IMMEDIATE

            LINKTO(DEPTH,1,2,'O',"D")
DO:         JMP     ENTER
            .WORD   ZERO,LIT,TICKPREVLEAVE,STORE,LIT,pdo,COMPILECOMMA,HERE,EXIT


; ----------------------------------------------------------------------
; DOES> [CORE] 6.1.1250
;
; Interpretation:
;   Interpretation semantics for this word are undefined.
;
; Compilation: ( C: colon-sys1 -- colon-sys2 )
;   Append the run-time semantics below to the current definition.  Whether
;   or not the current definition is rendered findable in the dictionary by
;   the compilation of DOES> is implementation defined.  Consume colon-sys1
;   and produce colon-sys2.  Append the initiation semantics given below to
;   the current definition.
;
; Run-time: ( -- ) ( R: nest-sys1 -- )
;   Replace the execution semantics of the most recent definition, referred
;   to as name, with the name execution semantics given below.  Return
;   control to the calling definition specified by nest-sys1.  An ambiguous
;   condition exists if name was not defined with CREATE or a user-defined
;   word that calls CREATE.
;
; Initiation: ( i*x -- i*x a-addr ) ( R: -- nest-sys2 )
;   Save implementation-dependent information nest-sys2 about the calling
;   definition.  Place name's data field address on the stack.  The stack
;   effects i*x represent arguments to name.
;
; name Execution: ( i*x -- j*x )
;   Execute the portion of the definition that begins with the initiation
;   semantics appended by the DOES> which modified name.  The stack effects
;   i*x and j*x represent arguments to and results from name, respectively.
;
; ---
; : (does>)
;   R>                  -- Get the new CFA for this def'n, which also exits
;                       -- the current def'n since we just popped the defining
;                       -- word's address from the return stack.
;   LATEST @ NFA>CFA    -- Get address of LATEST's CFA.
;   195 OVER C!  1+ !   -- Replace CFA with a JMP (195) to the code after DOES>
;                       -- which in our implementation is CALL DODOES and then
;                       -- the high-level thread after DOES>.
; ;
;
; : DOES> ( -- )
;   ['] (does>) COMPILE,  205 C, DODOES , -- CALL DODOES
; ; IMMEDIATE

            LINKTO(DO,1,5,'>',"SEOD")
DOES:       JMP     ENTER
            .WORD   LIT,pdoes,COMPILECOMMA,LIT,205,CCOMMA,LIT,DODOES,COMMA,EXIT
pdoes:      JMP     ENTER
            .WORD   RFROM,LATEST,FETCH,NFATOCFA
            .WORD   LIT,195,OVER,CSTORE,ONEPLUS,STORE,EXIT


; ----------------------------------------------------------------------
; DROP [CORE] 6.1.1260  ( x -- )
;
; Remove x from the stack.

            LINKTO(DOES,0,4,'P',"ORD")
DROP:       POP     H
            NEXT


; ----------------------------------------------------------------------
; DUP [CORE] 6.1.1290 "dupe" ( x -- x x )
;
; Duplicate x.

            LINKTO(DROP,0,3,'P',"UD")
DUP:        POP     H
            PUSH    H
            PUSH    H
            NEXT


; ----------------------------------------------------------------------
; ELSE [CORE] 6.1.1310
;
; Interpretation:
;   Interpretation semantics for this word are undefined.
;
; Compilation: ( C: orig1 -- orig2 )
;   Put the location of a new unresolved forward reference orig2 onto the
;   control flow stack.  Append the run-time semantics given below to the
;   current definition.  The semantics will be incomplete until orig2 is
;   resolved (e.g., by THEN).  Resolve the forward reference orig1 using
;   the location following the appended  run-time semantics.
;
; Run-time: ( -- )
;   Continue execution at the location given by the resolution of orig2.
;
; ---
; : ELSE   ['] branch COMPILE,  HERE DUP ,  SWAP POSTPONE THEN ; IMMEDIATE

            LINKTO(DUP,1,4,'E',"SLE")
ELSE:       JMP     ENTER
            .WORD   LIT,branch,COMPILECOMMA,HERE,DUP,COMMA,SWAP,THEN,EXIT


; ----------------------------------------------------------------------
; EMIT [CORE] 6.1.1320 ( x -- )
;
; If x is a graphic character in the implementation-defined character set,
; display x.  The effect of EMIT for all other values of x is
; implementation-defined.
;
; When passed a character whose character-defining bits have a value between
; hex 20 and 7E inclusive, the corresponding standard character, specified by
; 3.1.2.1 Graphic characters, is displayed.  Because different output devices 
; can respond differently to control characters, programs that use control
; characters to perform specific functions have an environmental dependency.
; Each EMIT deals with only one character.

            LINKTO(ELSE,0,4,'T',"IME")
EMIT:       POP     H           ; Pop the character into HL
            MOV     A,L         ; ..and then move it into A.
            CALL    STDCALL     ; Call the
            .WORD   04B44h      ; .."character output" routine.
            NEXT


; ----------------------------------------------------------------------
; ENVIRONMENT? [CORE] 6.1.1345 "environment-query" ( c-addr u -- false | i*x true )
;
; c-addr is the address of a character string and u is the string's character
; count.  u may have a value in the range from zero to an implementation-defined
; maximum which shall not be less than 31.  The character string should contain
; a keyword from 3.2.6 Environmental queries or the optional word sets to be
; checked for correspondence with an attribute of the present environment.  If
; the system treats the attribute as unknown, the returned flag is false;
; otherwise, the flag is true and the i*x returned is of the type specified in
; the table for the attribute queried.
;
; TODO: Implement ENVIRONMENT?


; ----------------------------------------------------------------------
; EVALUATE [CORE] 6.2.1360 ( i*x c-addr u -- j*x )
;
; Save the current input source specification.  Store minus-one (-1) in
; SOURCE-ID if it is present.  Make the string described by c-addr and u
; both the input source and input buffer, set >IN to zero, and interpret.
; When the parse area is empty, restore the prior input source specification.
; Other stack effects are due to the words EVALUATEd.
;
; ---
; : EVALUATE ( i*x c-addr u -- j*x)
;   PUSHICB  OVER + ICB 2!  -1 ICB ICBSOURCEID + !  INTERPRET  POPICB ;

            LINKTO(EMIT,0,8,'E',"TAULAVE")
EVALUATE:   JMP     ENTER
            .WORD   PUSHICB,OVER,PLUS,ICB,TWOSTORE
            .WORD   LIT,-1,ICB,LIT,ICBSOURCEID,PLUS,STORE
            .WORD   INTERPRET,POPICB,EXIT


; ----------------------------------------------------------------------
; EXECUTE [CORE] 6.1.1370 ( i*x xt -- j*x )
;
; Remove xt from the stack and perform the semantics identified by it.
; Other stack effects are due to the word EXECUTEd.

            LINKTO(EVALUATE,0,7,'E',"TUCEXE")
EXECUTE:    POP     H           ; Pop xt.
            PCHL                ; Execute xt.


; ----------------------------------------------------------------------
; EXIT [CORE] 6.1.1380
;
; Interpretation:
;   Interpretation semantics for this word are undefined.
;
; Execution: ( -- ) ( R: nest-sys -- )
;   Return control to the calling definition specified by nest-sys.  Before
;   executing EXIT within a do-loop, a program shall discard the loop-control
;   parameters by executing UNLOOP.

            LINKTO(EXECUTE,0,4,'T',"IXE")
EXIT:       RSPOP(D,E)
            NEXT


; ----------------------------------------------------------------------
; FILL [CORE] 6.1.1540 ( c-addr u char -- )
;
; If u is greater than zero, store char in each of u consecutive
; characters of memory beginning at c-addr.
;
; ---
; : FILL ( c-addr u char --)   ROT ROT 2>B FORB DUP B! NEXTB DROP ;

            LINKTO(EXIT,0,4,'L',"LIF")
FILL:       JMP     ENTER
            .WORD   ROT,ROT,TWOTOB
_fill1:     .WORD   BQUES,zbranch,_fill2
            .WORD   DUP,BSTORE,BPLUS,branch,_fill1
_fill2:     .WORD   DROP,EXIT


; ----------------------------------------------------------------------
; FIND [CORE] 6.1.1550 ( c-addr -- c-addr 0 | xt 1 | xt -1 )
;
; Find the definition named in the counted string at c-addr.  If the
; definition is not found after searching all the word lists in the
; search order, return c-addr and zero.  If the definition is found,
; return xt.  If the definition is immediate, also return one (1);
; otherwise also return minus-one (-1).  For a given string, the values
; returned by FIND while compiling may differ from those returned while
; not compiling.
;
; ---
; : FIND ( c-addr -- c-addr 0 | xt 1 | xt -1 )
;   COUNT (FIND) ?DUP 0= IF DROP 1- 0 THEN ;

            LINKTO(FILL,0,4,'D',"NIF")
FIND:       JMP     ENTER
            .WORD   COUNT,PFIND,QDUP,ZEROEQUALS,zbranch,_find1
            .WORD   DROP,ONEMINUS,ZERO
_find1:     .WORD   EXIT


; ----------------------------------------------------------------------
; FM/MOD [CORE] 6.1.1561 "f-m-slash-mod" ( d1 n1 -- n2 n3 )
;
; Divide d1 by n1, giving the floored quotient n3 and the remainder n2.
; Input and output stack arguments are signed.  An ambiguous condition
; exists if n1 is zero or if the quotient lies outside the range of a
; single-cell signed integer. 
;
; ---
; Floored division is integer division in which the remainder carries the
; sign of the divisor or is zero, and the quotient is rounded to its
; arithmetic floor.
;
; ---
; : FM/MOD ( d1 n1 -- n2 n3)
;   DUP >R ( num den R:signrem) 2DUP XOR ( num den signquo R:signrem)
;   SWAP ABS DUP >R ( num signquo +den R:signrem +den)
;   SWAP >R >R DABS R> ( num +den R:signrem +den signquo) SM/REM ( rem quo R:..)
;   R> 0< IF NEGATE OVER 0<> IF 1- SWAP R> SWAP - SWAP ELSE R> DROP THEN
;       ELSE R> DROP THEN
;   R> 0< IF SWAP NEGATE SWAP THEN ;

            LINKTO(FIND,0,6,'D',"OM/MF")
FMSLASHMOD: JMP     ENTER
            .WORD   DUP,TOR,TWODUP,XOR,SWAP,ABS,DUP,TOR
            .WORD   SWAP,TOR,TOR,DABS,RFROM,UMSLASHMOD
            .WORD   RFROM,ZEROLESS,zbranch,_fmslashmod1
            .WORD   NEGATE,OVER,ZERONOTEQUALS,zbranch,_fmslashmod1
            .WORD   ONEMINUS,SWAP,RFROM,SWAP,MINUS,SWAP,branch,_fmslashmod2
_fmslashmod1:.WORD  RFROM,DROP
_fmslashmod2:.WORD  RFROM,ZEROLESS,zbranch,_fmslashmod3
            .WORD   SWAP,NEGATE,SWAP
_fmslashmod3:.WORD  EXIT


; ----------------------------------------------------------------------
; HERE [CORE] 6.1.1650 ( -- addr )
;
; addr is the data-space pointer.

            LINKTO(FMSLASHMOD,0,4,'E',"REH")
HERE:       LHLD    DP
            PUSH    H
            NEXT


; ----------------------------------------------------------------------
; HOLD [CORE] 6.1.1670 ( char -- )
;
; Add char to the beginning of the pictured numeric output string.  An
; ambiguous condition exists if HOLD executes outside of a <# #> delimited
; number conversion.
;
; ---
; : HOLD ( c -- )   HLD @ 1- DUP HLD ! C! ;

            LINKTO(HERE,0,4,'D',"LOH")
HOLD:       JMP     ENTER
            .WORD   HLD,FETCH,ONEMINUS,DUP,HLD,STORE,CSTORE
            .WORD   EXIT


; ----------------------------------------------------------------------
; I [CORE] 6.1.1680
;
; Interpretation:
;   Interpretation semantics for this word are undefined.
;
; Execution: ( -- n|u ) ( R:  loop-sys -- loop-sys )
;   n|u is a copy of the current (innermost) loop index.  An ambiguous
;   condition exists if the loop control parameters are unavailable.

            LINKTO(HOLD,0,1,'I',"")
I:          RSFETCH(H,L)        ; Get the loop index into HL
            PUSH    H           ; ..and push it onto the stack.
            NEXT


; ----------------------------------------------------------------------
; IF [CORE] 6.1.1700
;
; Interpretation:
;   Interpretation semantics for this word are undefined.
;
; Compilation: ( C: -- orig )
;   Put the location of a new unresolved forward reference orig onto the
;   control flow stack. Append the run-time semantics given below to the
;   current definition.  The semantics are incomplete until orig is resolved,
;   e.g., by THEN or ELSE.
;
; Run-time: ( x -- )
;   If all bits of x are zero, continue execution at the location specified
;   by the resolution of orig.
;
; ---
; : IF   ['] 0branch COMPILE,  HERE DUP , ; IMMEDIATE

            LINKTO(I,1,2,'F',"I")
IF:         JMP     ENTER
            .WORD   LIT,zbranch,COMPILECOMMA,HERE,DUP,COMMA,EXIT


; ----------------------------------------------------------------------
; IMMEDIATE [CORE] 6.1.1710 ( -- )
;
; Make the most recent definition an immediate word.  An ambiguous
; condition exists if the most recent definition does not have a name.
;
; ---
; : IMMEDIATE ( -- )   LATEST @  DUP C@ [HEX] 80 OR  SWAP C! ;

            LINKTO(IF,0,9,'E',"TAIDEMMI")
IMMEDIATE:  JMP     ENTER
            .WORD   LATEST,FETCH,DUP,CFETCH,LIT,080h,OR,SWAP,CSTORE,EXIT


; ----------------------------------------------------------------------
; INVERT [CORE] 6.1.1720 ( x1 -- x2 )
;
; Invert all bits of x1, giving its logical inverse x2.

            LINKTO(IMMEDIATE,0,6,'T',"REVNI")
INVERT:     POP     H           ; Pop x1.
            MOV     A,H         ; Put x1's high byte into A,
            CMA                 ; ..then complement x1's high byte,
            MOV     H,A         ; ..and put the result back into H.
            MOV     A,L         ; Put x1's low byte into A,
            CMA                 ; ..then complement x1's low byte,
            MOV     L,A         ; ..and put the result back into L.
            PUSH    H           ; Push the result (HL).
            NEXT


; ----------------------------------------------------------------------
; J [CORE] 6.1.1730
;
; Interpretation:
;   Interpretation semantics for this word are undefined.
;
; Execution: ( -- n|u ) ( R: loop-sys1 loop-sys2 -- loop-sys1 loop-sys2 )
;   n|u is a copy of the next-outer loop index.  An ambiguous condition
;   exists if the loop control parameters of the next-outer loop, loop-sys1,
;   are unavailable.

            LINKTO(INVERT,0,1,'J',"")
J:          RSPICK2(H,L)        ; Get the second loop index (the 3rd RS item)
            PUSH    H           ; ..into HL and push it onto the stack.
            NEXT


; ----------------------------------------------------------------------
; KEY [CORE] 6.1.1750 ( -- char )
;
; Receive one character char, a member of the implementation-defined
; character set.  Keyboard events that do not correspond to such characters
; are discarded until a valid character is received, and those events are
; subsequently unavailable.
;
; All standard characters can be received.  Characters received by KEY are
; not displayed.
;
; Any standard character returned by KEY has the numeric value specified in
; 3.1.2.1 Graphic characters.  Programs that require the ability to receive
; control characters have an environmental dependency.
;
; ---
; NOTE: Wake up from power off generates a null key event, which we need
; to ignore.
;
; : KEY ( -- char)   BEGIN  BEGIN PAUSE KEY? UNTIL  (KEY) ?DUP 0<> UNTIL ;

            LINKTO(J,0,3,'Y',"EK")
KEY:        JMP     ENTER
_key1:      .WORD   PAUSE,KEYQ,zbranch,_key1
            .WORD   PKEY,QDUP,ZERONOTEQUALS,zbranch,_key1
            .WORD   EXIT


; ----------------------------------------------------------------------
; LEAVE [CORE] 6.1.1760
;
; Interpretation:
;   Interpretation semantics for this word are undefined.
;
; Execution: ( -- ) ( R: loop-sys -- )
;   Discard the current loop control parameters.  An ambiguous condition
;   exists if they are unavailable.  Continue execution immediately
;   following the innermost syntactically enclosing DO ... LOOP or
;   DO ... +LOOP.
;
; ---
; LEAVE ( do-orig)
;   ['] UNLOOP COMPILE,  ['] branch COMPILE,
;   HERE  'PREVLEAVE @ ,  'PREVLEAVE !
; ; IMMEDIATE

            LINKTO(KEY,1,5,'E',"VAEL")
LEAVE:      JMP     ENTER
            .WORD   LIT,UNLOOP,COMPILECOMMA,LIT,branch,COMPILECOMMA
            .WORD   HERE,LIT,TICKPREVLEAVE,FETCH,COMMA
            .WORD   LIT,TICKPREVLEAVE,STORE,EXIT


; ----------------------------------------------------------------------
; LITERAL [CORE] 6.1.1780
;
; Interpretation:
;   Interpretation semantics for this word are undefined.
;
; Compilation: ( x -- )
;   Append the run-time semantics given below to the current definition.
;
; Run-time: ( -- x )
;   Place x on the stack.
;
; ---
; : LITERAL ( x -- )   ['] LIT COMPILE,  ,  ; IMMEDIATE

            LINKTO(LEAVE,1,7,'L',"ARETIL")
LITERAL:    JMP     ENTER
            .WORD   LIT,LIT,COMPILECOMMA,COMMA,EXIT


; ----------------------------------------------------------------------
; LOOP [CORE] 6.1.1800
;
; Interpretation:
;   Interpretation semantics for this word are undefined.
;
; Compilation: ( C: do-sys -- )
;   Append the run-time semantics given below to the current definition.
;   Resolve the destination of all unresolved occurrences of LEAVE between
;   the location given by do-sys and the next location for a transfer of
;   control, to execute the words following the LOOP.
;
; Run-time: ( -- ) ( R: loop-sys1 -- | loop-sys2 )
;   An ambiguous condition exists if the loop control parameters are
;   unavailable.  Add one to the loop index.  If the loop index is then
;   equal to the loop limit, discard the loop parameters and continue
;   execution immediately following the loop.  Otherwise continue execution
;   at the beginning of the loop.
;
; ---
; : LOOP   ['] (loop) END-LOOP ; IMMEDIATE

            LINKTO(LITERAL,1,4,'P',"OOL")
LOOP:       JMP     ENTER
            .WORD   LIT,ploop,ENDLOOP,EXIT


; ----------------------------------------------------------------------
; LSHIFT [CORE] 6.1.1805 "l-shift" ( x1 u -- x2 )
;
; Perform a logical left shift of u bit-places on x1, giving x2.  Put
; zeroes into the least significant bits vacated by the shift.  An
; ambiguous condition exists if u is greater than or equal to the number
; of bits in a cell.

            LINKTO(LOOP,0,6,'T',"FIHSL")
LSHIFT:     POP     H           ; Pop u into HL,
            MOV     A,L         ; ..then move the low byte into H.
            POP     H           ; Pop x1 into HL.
_lshift1:   ANA     A           ; See if the count is zero;
            JZ      _lshiftDONE ; ..we're done if so.
            DAD     H           ; Left-shift HL by adding HL to itself.
            DCR     A           ; Decrement the counter
            JMP     _lshift1    ; ..and continue looping.
_lshiftDONE:PUSH    H           ; Push the result (HL).
            NEXT


; ----------------------------------------------------------------------
; M* [CORE] 6.1.1810 "m-star" ( n1 n2 -- d )
;
; d is the signed product of n1 times n2.
;
; ---
; : M* ( n1 n2 -- d )   2DUP XOR 0< >R  ABS SWAP ABS UM*  R> ?DNEGATE ;

            LINKTO(LSHIFT,0,2,'*',"M")
MSTAR:      JMP     ENTER
            .WORD   TWODUP,XOR,ZEROLESS,TOR,ABS,SWAP,ABS,UMSTAR
            .WORD   RFROM,QDNEGATE,EXIT


; ----------------------------------------------------------------------
; MAX [CORE] 6.1.1870 ( n1 n2 -- n3 )
;
; n3 is the greater of n1 and n2.
;
; ---
; : MAX ( n1 n2 -- n3 )   2DUP < IF SWAP THEN DROP ;

            LINKTO(MSTAR,0,3,'X',"AM")
MAX:        JMP     ENTER
            .WORD   TWODUP,LESSTHAN,zbranch,_maxDONE,SWAP
_maxDONE:   .WORD   DROP,EXIT


; ----------------------------------------------------------------------
; MIN [CORE] 6.1.1880 ( n1 n2 -- n3 )
;
; n3 is the lesser of n1 and n2.
;
; ---
; : MIN ( n1 n2 -- n3 )   2DUP > IF SWAP THEN DROP ;

            LINKTO(MAX,0,3,'N',"IM")
MIN:        JMP     ENTER
            .WORD   TWODUP,GREATERTHAN,zbranch,_minDONE,SWAP
_minDONE:   .WORD   DROP,EXIT


; ----------------------------------------------------------------------
; MOD [CORE] 6.1.1890 ( n1 n2 -- n3 )
;
; Divide n1 by n2, giving the single-cell remainder n3.  An ambiguous
; condition exists if n2 is zero.  If n1 and n2 differ in sign, the
; implementation-defined result returned will be the same as that returned
; by either the phrase >R S>D R> FM/MOD DROP or the phrase
; >R S>D R> SM/REM DROP.
;
; ---
; : MOD ( n1 n2 -- n3)   /MOD DROP ;

            LINKTO(MIN,0,3,'D',"OM")
MOD:        JMP     ENTER
            .WORD   SLASHMOD,DROP,EXIT


; ----------------------------------------------------------------------
; MOVE [CORE] 6.1.1900 ( addr1 addr2 u -- )
;
; If u is greater than zero, copy the contents of u consecutive address
; units at addr1 to the u consecutive address units at addr2.  After MOVE
; completes, the u consecutive address units at addr2 contain exactly what
; the u consecutive address units at addr1 contained before the move.
;
; ---
; : MOVE ( addr1 addr2 u --)
;   >R 2DUP SWAP DUP R@ + WITHIN R> SWAP IF CMOVE> ELSE CMOVE THEN ;

            LINKTO(MOD,0,4,'E',"VOM")
MOVE:       JMP     ENTER
            .WORD   TOR,TWODUP,SWAP,DUP,RFETCH,PLUS,WITHIN
            .WORD   RFROM,SWAP,zbranch,_move1
            .WORD   CMOVEUP,EXIT
_move1:     .WORD   CMOVE,EXIT


; ----------------------------------------------------------------------
; NEGATE [CORE] 6.1.1910 ( n1 -- n2 )
;
; Negate n1, giving its arithmetic inverse n2.  

            LINKTO(MOVE,0,6,'E',"TAGEN")
NEGATE:     POP     H
            MOV     A,L
            CMA
            MOV     L,A
            MOV     A,H
            CMA
            MOV     H,A
            INX     H
            PUSH    H
            NEXT


; ----------------------------------------------------------------------
; OR [CORE] 6.1.1980 ( x1 x2 -- x3 )
;
; x3 is the bit-by-bit inclusive-or of x1 with x2.

            LINKTO(NEGATE,0,2,'R',"O")
OR:         SAVEDE
            POP     H           ; Pop x2.
            POP     D           ; Pop x1.
            MOV     A,H         ; Put x2's high byte into A,
            ORA     D           ; ..then OR x1's high byte with A,
            MOV     H,A         ; ..and put the result into H.
            MOV     A,L         ; Put x2's low byte into A,
            ORA     E           ; ..then OR x1's low byte with A,
            MOV     L,A         ; ..and put the result into L.
            PUSH    H           ; Push the result (HL).
            RESTOREDE
            NEXT


; ----------------------------------------------------------------------
; OVER [CORE] 6.1.1990 ( x1 x2 -- x1 x2 x1 )
;
; Place a copy of x1 on top of the stack.

            LINKTO(OR,0,4,'R',"EVO")
OVER:       PUSH    D           ; Save DE on the stack.
            LDES    4           ; Get the address of the third stack item.
            LHLX                ; Load the third stack item into HL.
            POP     D           ; Restore DE.
            PUSH    H           ; Push the third stack item onto the stack.
            NEXT


; ----------------------------------------------------------------------
; POSTPONE [CORE] 6.1.2033
;
; Interpretation:
;   Interpretation semantics for this word are undefined.
;
; Compilation: ( "<spaces>name" -- )
;   Skip leading space delimiters.  Parse name delimited by a space.  Find
;   name.  Append the compilation semantics of name to the current
;   definition.  An ambiguous condition exists if name is not found.
;
; ---
; Postponing a non-immediate word requires the compiler to add code to the
; current definition that compiles the postponed word into the then-current
; definition when the current definition is executed.  Postponing an
; immediate word requires the word to be compiled directly into the current
; definition.
;
; : POSTPONE ( "<spaces>name" --)
;   PARSE-WORD (FIND)  DUP 0= IF DROP TYPE SPACE [CHAR] ? EMIT CR ABORT THEN
;   0< IF  ['] LIT COMPILE,  ,  ['] COMPILE, COMPILE, ELSE COMPILE, THEN
; ; IMMEDIATE

            LINKTO(OVER,1,8,'E',"NOPTSOP")
POSTPONE:   JMP     ENTER
            .WORD   PARSEWORD,PFIND,DUP,ZEROEQUALS,zbranch,_postpone1
            .WORD   DROP,TYPE,SPACE,LIT,'?',EMIT,CR,ABORT
_postpone1: .WORD   ZEROLESS,zbranch,_postpone2
            .WORD   LIT,LIT,COMPILECOMMA,COMMA,LIT,COMPILECOMMA,COMPILECOMMA
            .WORD       branch,_postpone3
_postpone2: .WORD   COMPILECOMMA
_postpone3: .WORD   EXIT


; ----------------------------------------------------------------------
; QUIT [CORE] 6.1.2050 ( -- ) ( R:  i*x -- )
;
; Empty the return stack, store zero in SOURCE-ID if it is present, make
; the user input device the input source, and enter interpretation state.
; Do not display a message.  Repeat the following:
;   - Accept a line from the input source into the input buffer, set >IN
;     to zero, and interpret.
;   - Display the implementation-defined system prompt if in interpretation
;     state, all processing has been completed, and no ambiguous condition
;     exists.
;
; ---
; : QUIT  ( --; R: i*x --)
;   INITRP  0 STATE !  INIT-ICBS  TIB  ICB ICBLINESTART +  !
;   BEGIN
;       TIB TIBSIZE  ACCEPT  TIB +  ICB ICBLINEEND + !
;       SPACE INTERPRET
;       CR  STATE @ 0= IF ." ok " THEN
;   AGAIN ;

            LINKTO(POSTPONE,0,4,'T',"IUQ")
QUIT:       JMP     ENTER
            .WORD   INITRP
            .WORD   ZERO,STATE,STORE
            .WORD   INITICBS,TIB,ICB,LIT,ICBLINESTART,PLUS,STORE
_quit1:     .WORD   TIB,LIT,TIBSIZE,ACCEPT
            .WORD   TIB,PLUS,ICB,LIT,ICBLINEEND,PLUS,STORE
            .WORD   SPACE,INTERPRET
            .WORD   CR,STATE,FETCH,ZEROEQUALS,zbranch,_quit2
            .WORD   PSQUOTE,3
            .BYTE   "ok "
            .WORD   TYPE
_quit2:     .WORD   branch,_quit1


; ----------------------------------------------------------------------
; R> [CORE] 6.1.2060 "r-from"
;
; Interpretation:
;   Interpretation semantics for this word are undefined.
;
; Execution: ( -- x ) ( R: x -- )
;   Move x from the return stack to the data stack.

            LINKTO(QUIT,0,2,'>',"R")
RFROM:      RSPOP(H,L)
            PUSH    H
            NEXT


; ----------------------------------------------------------------------
; R@ [CORE] 6.1.2070 "r-fetch"
;
; Interpretation:
;   Interpretation semantics for this word are undefined.
;
; Execution: ( -- x ) ( R: x -- x )
;   Copy x from the return stack to the data stack.

            LINKTO(RFROM,0,2,'@',"R")
RFETCH:     RSFETCH(H,L)
            PUSH    H
            NEXT


; ----------------------------------------------------------------------
; RECURSE [CORE] 6.1.2120
;
; Interpretation:
;   Interpretation semantics for this word are undefined.
;
; Compilation: ( -- )
;   Append the execution semantics of the current definition to the current
;   definition.  An ambiguous condition exists if RECURSE appears in a
;   definition after DOES>.
;
; ---
; RECURSE   LATEST @ NFA>CFA , ; IMMEDIATE

            LINKTO(RFETCH,1,7,'E',"SRUCER")
RECURSE:    JMP     ENTER
            .WORD   LATEST,FETCH,NFATOCFA,COMMA,EXIT


; ----------------------------------------------------------------------
; REPEAT [CORE] 6.1.2140
;
; Interpretation:
;   Interpretation semantics for this word are undefined.
;
; Compilation: ( C: orig dest -- )
;   Append the run-time semantics given below to the current definition,
;   resolving the backward reference dest.  Resolve the forward reference
;   orig using the location following the appended run-time semantics.
;
; Run-time: ( -- )
;   Continue execution at the location given by dest.
;
; ---
; REPEAT   POSTPONE AGAIN  POSTPONE THEN ; IMMEDIATE

            LINKTO(RECURSE,1,6,'T',"AEPER")
REPEAT:     JMP     ENTER
            .WORD   AGAIN,THEN,EXIT


; ----------------------------------------------------------------------
; ROT [CORE] 6.1.2160 "rote" ( x1 x2 x3 -- x2 x3 x1 )
;
; Rotate the top three stack entries.

            LINKTO(REPEAT,0,3,'T',"OR")
ROT:        SAVEDE
            POP     D           ; Pop x3 into DE.
            POP     H           ; Pop x2 into HL.
            XTHL                ; Swap TOS (x1) with HL (x2).
            PUSH    D           ; Push x3 back onto the stack.
            PUSH    H           ; Push x1 back onto the stack.
            RESTOREDE
            NEXT


; ----------------------------------------------------------------------
; RSHIFT [CORE] 6.1.1805 "l-shift" ( x1 u -- x2 )
;
; Perform a logical left shift of u bit-places on x1, giving x2.  Put
; zeroes into the least significant bits vacated by the shift.  An
; ambiguous condition exists if u is greater than or equal to the number
; of bits in a cell.

            LINKTO(ROT,0,6,'T',"FIHSR")
RSHIFT:     SAVEDE
            POP     D           ; Pop u into DE, although we only care about E.
            POP     H           ; Pop x1 into HL.
            INR     E           ; Increment E so that the loop can pre-test.
_rshift1:   DCR     E           ; Decrement E and see if the count is zero;
            JZ      _rshiftDONE ; ..we're done if so.
            ANA     A           ; Clear carry.
            MOV     A,H         ; Move the high byte into A,
            RAR                 ; ..rotate right with carry,
            MOV     H,A         ; ..then put the high byte back into H.
            MOV     A,L         ; Move the low byte into A,
            RAR                 ; ..rotate right with carry,
            MOV     L,A         ; ..then put the low byte back into L.
            JMP     _rshift1    ; Continue looping.
_rshiftDONE:PUSH    H           ; Push the result (HL).
            RESTOREDE
            NEXT


; ----------------------------------------------------------------------
; S" [CORE] 6.1.2165 "s-quote"
;
; Interpretation:
;   Interpretation semantics for this word are undefined.
;
;   Extended by FILE: ( "ccc<quote>" -- c-addr u )
;       Parse ccc delimited by " (double quote).  Store the resulting
;       string c-addr u at a temporary location.  The maximum length of
;       the temporary buffer is implementation-dependent but shall be no
;       less than 80 characters.  Subsequent uses of S" may overwrite the
;       temporary buffer.  At least one such buffer shall be provided.
;
; Compilation: ( "ccc<quote>" -- )
;   Parse ccc delimited by " (double-quote).  Append the run-time
;   semantics given below to the current definition.
;
; Run-time: ( -- c-addr u )
;   Return c-addr and u describing a string consisting of the characters
;   ccc.  A program shall not alter the returned string.
;
; ---
; : S" ( "ccc<quote>" --)
;   [CHAR] " PARSE ( caS uS)
;   STATE @ 0= IF  DUP 'S"SIZE > ABORT" String too long"
;       'S" OVER 2SWAP ( caD uS caS uS)  'S"
;   ELSE ['] (S") COMPILE,  DUP ,  HERE OVER ALLOT THEN
;   ( caS uS caD) SWAP CMOVE ;

            LINKTO(RSHIFT,1,2,022h,"S")
SQUOTE:     JMP     ENTER
            .WORD   LIT,022h,PARSE,STATE,FETCH,ZEROEQUALS,zbranch,_squote2
            .WORD   DUP,LIT,SQSIZE,GREATERTHAN,zbranch,_squote1
            .WORD   PSQUOTE,12
            .BYTE   "String too long"
            .WORD   TYPE,ABORT
_squote1:   .WORD   TICKSQUOTE,OVER,TWOSWAP,TICKSQUOTE,branch,_squote3
_squote2:   .WORD   LIT,PSQUOTE,COMPILECOMMA,DUP,COMMA,HERE,OVER,ALLOT
_squote3:   .WORD   SWAP,CMOVE,EXIT


; ----------------------------------------------------------------------
; S>D [CORE] 6.1.2170 "s-to-d" ( n -- d )
;
; Convert the number n to the double-cell number d with the same numerical value.
;
; ---
; : S>D ( n -- d)   DUP 0< ;

            LINKTO(SQUOTE,0,3,'D',">S")
STOD:       JMP     ENTER
            .WORD   DUP,ZEROLESS,EXIT


; ----------------------------------------------------------------------
; SIGN [CORE] 6.1.2210 ( n -- )
;
; If n is negative, add a minus sign to the beginning of the pictured
; numeric output string.  An ambiguous condition exists if SIGN executes
; outside of a <# #> delimited number conversion.
;
; ---
; : SIGN ( n -- )   0< IF 45 HOLD THEN ;

            LINKTO(STOD,0,4,'N',"GIS")
SIGN:       JMP     ENTER
            .WORD   ZEROLESS,zbranch,_signDONE,LIT,45,HOLD
_signDONE:  .WORD   EXIT


; ----------------------------------------------------------------------
; SM/REM [CORE] 6.1.2214 "s-m-slash-rem" ( d1 n1 -- n2 n3 )
;
; Divide d1 by n1, giving the symmetric quotient n3 and the remainder n2.
; Input and output stack arguments are signed.  An ambiguous condition
; exists if n1 is zero or if the quotient lies outside the range of a
; single-cell signed integer.
;
; ---
; : SM/REM ( d1 n1 -- n2 n3)
;   OVER >R  2DUP XOR >R  ( R:remsign quosign)
;   ABS >R DABS R>
;   UM/MOD ( +rem +quo)
;   R> ?NEGATE ( +rem +-quo)  SWAP R> ?NEGATE SWAP ( +-rem +-quo) ;

            LINKTO(SIGN,0,6,'M',"ER/MS")
SMSLASHREM: JMP     ENTER
            .WORD   OVER,TOR,TWODUP,XOR,TOR,ABS,TOR,DABS,RFROM,UMSLASHMOD
            .WORD   RFROM,QNEGATE,SWAP,RFROM,QNEGATE,SWAP,EXIT


; ----------------------------------------------------------------------
; SOURCE [CORE] 6.1.2216 ( -- c-addr u )
;
; c-addr is the address of, and u is the number of characters in, the
; input buffer.
;
; ---
; : SOURCE ( -- c-addr u)   ICB 2@ OVER - ;

            LINKTO(SMSLASHREM,0,6,'E',"CRUOS")
SOURCE:     JMP     ENTER
            .WORD   ICB,TWOFETCH,OVER,MINUS,EXIT


; ----------------------------------------------------------------------
; SPACE [CORE] 6.1.2220 ( -- )
;
; Display one space.

            LINKTO(SOURCE,0,5,'E',"CAPS")
SPACE:      MVI     A,020h      ; Put the space character in A.
            CALL    STDCALL     ; Call the
            .WORD   04B44h      ; .."character output" routine.
            NEXT


; ----------------------------------------------------------------------
; SPACES [CORE] 6.1.2230 ( n -- )
;
; If n is greater than zero, display n spaces.
;
; ---
; : SPACES ( n -- )   DUP IF SPACE 1- THEN DROP ;

            LINKTO(SPACE,0,6,'S',"ECAPS")
SPACES:     JMP     ENTER
_spaces1:   .WORD   DUP,zbranch,_spacesDONE,SPACE,ONEMINUS,branch,_spaces1
_spacesDONE:.WORD   DROP
            .WORD   EXIT


; ----------------------------------------------------------------------
; STATE [CORE] 6.1.2250 ( -- a-addr )
;
; a-addr is the address of a cell containing the compilation-state flag.
; STATE is true when in compilation state, false otherwise.  The true
; value in STATE is non-zero, but is otherwise implementation-defined.
; Only the following standard words alter the value in STATE:  : (colon),
; ; (semicolon), ABORT, QUIT, :NONAME, [ (left-bracket), and ] (right-bracket).
;
; Note: A program shall not directly alter the contents of STATE.

            LINKTO(SPACES,0,5,'E',"TATS")
STATE:      LXI     H,TICKSTATE
            PUSH    H
            NEXT


; ----------------------------------------------------------------------
; SWAP [CORE] 6.1.2260 "two-dupe" ( x1 x2 -- x2 x1 )
;
; Exchange the top two stack items.

            LINKTO(STATE,0,4,'P',"AWS")
SWAP:       POP     H           ; Pop x2 into HL.
            XTHL                ; Swap TOS (x1) with HL (x2).
            PUSH    H           ; Push x1 back onto the stack.
            NEXT


; ----------------------------------------------------------------------
; THEN [CORE] 6.1.2270
;
; Interpretation:
;   Interpretation semantics for this word are undefined.
;
; Compilation: ( C: orig -- )
;   Append the run-time semantics given below to the current definition.
;   Resolve the forward reference orig using the location of the appended
;   run-time semantics.
;
; Run-time: ( -- )
;   Continue execution.
;
; ---
; : THEN   HERE SWAP ! ; IMMEDIATE

            LINKTO(SWAP,1,4,'N',"EHT")
THEN:       JMP     ENTER
            .WORD   HERE,SWAP,STORE,EXIT


; ----------------------------------------------------------------------
; TYPE [CORE] 6.1.2310 ( c-addr u -- )
;
; If u is greater than zero, display the character string specified by
; c-addr and u.
;
; When passed a character in a character string whose character-defining
; bits have a value between hex 20 and 7E inclusive, the corresponding
; standard character, specified by 3.1.2.1 graphic characters, is displayed.
; Because different output devices can respond differently to control
; characters, programs that use control characters to perform specific
; functions have an environmental dependency.

            LINKTO(THEN,0,4,'E',"PYT")
TYPE:       SAVEDE
            POP     D           ; Pop the count into DE.
            POP     H           ; Pop the address into HL.
_type1:     MOV     A,D         ; See if the count is zero by moving D to A
            ORA     E           ; ..and then ORing A with E.
            JZ      _typeDONE   ; We're done if the count is zero.
            MOV     A,M         ; Get the current character.
            CALL    STDCALL     ; Call the
            .WORD   04B44h      ; .."character output" routine.
            INX     H           ; Move to the next character.
            DCX     D           ; Decrement the remaining count.
            JMP     _type1      ; Keep going.
_typeDONE:  RESTOREDE
            NEXT


; ----------------------------------------------------------------------
; U. [CORE] 6.1.2320 "u-dot" ( u -- )
;
; Display u in free field format.
;
; ---
; : U. ( u -- )   0 UD.

            LINKTO(TYPE,0,2,'.',"U")
UDOT:       JMP     ENTER
            .WORD   ZERO,UDDOT,EXIT


; ----------------------------------------------------------------------
; U< [CORE] 6.1.2340 "u-less-than" ( u1 u2 -- flag )
;
; flag is true if and only if u1 is less than u2.

            LINKTO(UDOT,0,2,'<',"U")
ULESSTHAN:  SAVEDE
            POP     D           ; Pop u2.
            POP     H           ; Pop u1.
            PUSH    B           ; Save BC.
            MOV     B,D         ; Move u2
            MOV     C,E         ; ..to BC.
            DSUB                ; HL=u1-u2
            POP     B           ; Restore BC.
            SBB     A           ; Propagate carry throughout A
            MOV     H,A         ; ..and fill HL
            MOV     L,A         ; ..with the contents of A (0000 or FFFF).
            PUSH    H           ; Push the flag to the stack.
            RESTOREDE
            NEXT


; ----------------------------------------------------------------------
; UM* [CORE] 6.1.2360 "u-m-star" ( u1 u2 -- ud )
;
; Multiply u1 by u2, giving the unsigned double-cell product ud.  All
; values and arithmetic are unsigned
;
; ---
; This is U* copied verbatim from fig-FORTH 8080 v1.3.
; The only changes were to save and restore DE in HOLDD.  BC was already
; getting saved since that is the fig-FORTH Instruction Pointer.  The
; fig-FORTH comments are unchanged, so replace "IP" with "RSP".

            LINKTO(ULESSTHAN,0,3,'*',"MU")
UMSTAR:     SAVEDE

            ; fig-FORTH code:
            POP     D           ; (DE) <- MPLIER
            POP     H           ; (HL) <- MPCAND
            PUSH    B           ; SAVE IP
            MOV     B,H
            MOV     A,L         ; (BA) <- MPCAND
            CALL    MPYX        ; (AHL)1 <- MPCAND.LB * MPLIER
            ; 1ST PARTIAL PRODUCT
            PUSH    H           ; SAVE (HL)1
            MOV     H,A
            MOV     A,B
            MOV     B,H         ; SAVE (A)1
            CALL    MPYX        ; (AHL)2 <- MPCAND.HB * MPLIER
            ; 2ND PARTIAL PRODUCT
            POP     D           ; (DE) <- (HL)1
            MOV     C,D         ; (BC) <- (AH)1
            ; FORM SUM OF PARTIALS:
            ;      (AHL) 1
            ;    + (AHL) 2
            ;   --------
            ;     (AHLE)
            DAD     B           ; (HL) <- (HL)2 + (AH)1
            ACI     0           ; (AHLE) <- (BA) * (DE)
            MOV     D,L
            MOV     L,H
            MOV     H,A         ; (HLDE) <- MPLIER * MPCAND
            POP     B           ; RESTORE IP
            PUSH    D           ; (S2) <- PRODUCT.LW

            ; MFORTH code:
            PUSH    H           ; (S1) <- PRODUCT.HW
            RESTOREDE
            NEXT
            ;
            ;   MULTIPLY PRIMITIVE
            ;           (AHL) <- (A) * (DE)
            ;   #BITS =   24      8     16
MPYX:       LXI     H,0         ; (HL) <- 0 = PARTIAL PRODUCT.LW
            MVI     C,4         ; LOOP COUNTER
MPYX1:      DAD     H           ; LEFT SHIFT (AHL) 24 BITS
            RAL
            JNC     MPYX2       ; IF NEXT MPLIER BIT = 1
            DAD     D           ; THEN ADD MPCAND
            ACI     0
MPYX2:      DAD     H
            RAL
            JNC     MPYX3
            DAD     D
            ACI     0
MPYX3:      DCR     C           ; IF NOT LAST MPLIER BIT
            JNZ     MPYX1       ; THEN LOOP AGAIN
            RET                 ; ELSE DONE


; ----------------------------------------------------------------------
; UM/MOD [CORE] 6.1.2370 "u-m-slash-mod" ( ud u1 -- u2 u3 )
;
; Divide ud by u1, giving the quotient u3 and the remainder u2.  All values
; and arithmetic are unsigned.  An ambiguous condition exists if u1 is zero
; or if the quotient lies outside the range of a single-cell unsigned integer.
;
; ---
; This is U/ copied verbatim from fig-FORTH 8080 v1.3.
; The only changes were to save and restore DE in HOLDD.  BC was already
; getting saved since that is the fig-FORTH Instruction Pointer.  The
; fig-FORTH comments are unchanged, so replace "IP" with "RSP".

            LINKTO(UMSTAR,0,6,'D',"OM/MU")
UMSLASHMOD: SAVEDE

            ; fig-FORTH code:
            MOV     H,B
            MOV     L,C         ; (HL) <- (IP)
            POP     B           ; (BC) <- (S1) = DENOMINATOR
            POP     D           ; (DE) <- (S2) = NUMERATOR.HIGH
            XTHL                ; (S1) <- (IP)
            XCHG                ; (HLDE) = NUMERATOR, 32 BITS
            MOV     A,L
            SUB     C
            MOV     A,H         ; IF OVERFLOW
            SBB     B
            JNC     USBAD       ; THEN RETURN BAD VALUE
            MOV     A,H
            MOV     H,L
            MOV     L,D         ; (AHL) <- 24 BITS OF NUMERATOR
            MVI     D,8         ; (D) <- INIT COUNTER
            PUSH    D           ; SAVE D & E
            CALL    USLA        ; PARTIAL DIVISION
            POP     D           ; RESTORE COUNTER & NUM.MSBYTE
            PUSH    H           ; (S1) <- (L) = BYTE OF QUOTIENT
            MOV     L,E
            CALL    USLA
            MOV     D,A
            MOV     E,H         ; (DE) <- REMAINDER
            POP     B           ; RESTORE QUOTIENT.HIGH
            MOV     H,C         ; (HL) <- QUOTIENT
            POP     B           ; RESTORE (IP)

            ; MFORTH code:
            PUSH    D
            PUSH    H
            RESTOREDE
            NEXT

USL0:       MOV     E,A
            MOV     A,H
            SUB     C
            MOV     H,A
            MOV     A,E
            SBB     B
            JNC     USL1        ; IF CARRY
            MOV     A,H         ; THEN ADD (BC) INTO (AH)
            ADD     C
            MOV     H,A
            MOV     A,E
            DCR     D
            RZ                  ; RETURN FROM USLA

USLA:       DAD     H           ; 24BIT LEFT-SHIFT ( *2 )
            RAL
            JNC     USL0        ; SUBTRACT & TEST
            MOV     E,A
            MOV     A,H
            SUB     C           ; (AH) <- (AH) - (BC)
            MOV     H,A
            MOV     A,E
            SBB     B
USL1:       INR     L           ; 1 BIT OF QUOT INTO RIGHT SIDE
            DCR     D           ;   OF (AHL)
            JNZ     USLA        ; CONTINUE DIVISION
            RET                 ; ALL 8 TRIAL COMPLETE

USBAD:      LXI     H,0FFFFh    ; OVERFLOW, RETURN 32BIT -1
            POP     B           ; RESTORE (IP)

            ; MFORTH code:
            PUSH    H
            RESTOREDE
            NEXT


; ----------------------------------------------------------------------
; UNLOOP [CORE] 6.1.2380
;
; Interpretation:
;   Interpretation semantics for this word are undefined.
;
; Execution: ( -- ) ( R: loop-sys -- )
;   Discard the loop-control parameters for the current nesting level.
;   An UNLOOP is required for each nesting level before the definition may
;   be EXITed.  An ambiguous condition exists if the loop-control parameters
;   are unavailable.
;
; ---
; UNLOOP  R> DROP R> DROP ;

            LINKTO(UMSLASHMOD,0,6,'P',"OOLNU")
UNLOOP:     RSPOP(H,L)
            RSPOP(H,L)
            NEXT


; ----------------------------------------------------------------------
; UNTIL [CORE] 6.1.2390
;
; Compilation: ( C: dest -- )
;   Append the run-time semantics given below to the current definition,
;   resolving the backward reference dest.
;
; Run-time: ( x -- )
;   If all bits of x are zero, continue execution at the location specified
;   by dest.
;
; ---
; : UNTIL   ['] 0branch COMPILE,  , ; IMMEDIATE

            LINKTO(UNLOOP,1,5,'L',"ITNU")
UNTIL:      JMP     ENTER
            .WORD   LIT,zbranch,COMPILECOMMA,COMMA,EXIT


; ----------------------------------------------------------------------
; VARIABLE [CORE] 6.1.2410 ( "<spaces>name" -- )
;
; Skip leading space delimiters.  Parse name delimited by a space.  Create
; a definition for name with the execution semantics defined below.  Reserve
; one cell of data space at an aligned address.
;
; name is referred to as a "variable".
;
; name Execution: ( -- a-addr )
;   a-addr is the address of the reserved cell.  A program is responsible
;   for initializing the contents of the reserved cell.
;
; ---
; : VARIABLE ( "<spaces>name" -- )
;   CREATE  CFASZ NEGATE ALLOT  195 C, DOVARIABLE ,  0 , ; -- JMP DOVARIABLE

            LINKTO(UNTIL,0,8,'E',"LBAIRAV")
VARIABLE:   JMP     ENTER
            .WORD   CREATE,LIT,-CFASZ,ALLOT,LIT,195,CCOMMA,LIT,DOVARIABLE,COMMA
            .WORD   ZERO,COMMA,EXIT


; ----------------------------------------------------------------------
; WHILE [CORE] 6.1.2430
;
; Compilation: ( C: dest -- orig dest )
;   Put the location of a new unresolved forward reference orig onto the
;   control flow stack, under the existing dest.  Append the run-time
;   semantics given below to the current definition.  The semantics are
;   incomplete until orig and dest are resolved (e.g., by REPEAT).
;
; Run-time: ( x -- )
;   If all bits of x are zero, continue execution at the location specified
;   by the resolution of orig.
;
; ---
; : WHILE   POSTPONE IF  SWAP ; IMMEDIATE

            LINKTO(VARIABLE,1,5,'E',"LIHW")
WHILE:      JMP     ENTER
            .WORD   IF,SWAP,EXIT


; ----------------------------------------------------------------------
; WORD [CORE] 6.1.2450 ( char "<chars>ccc<char>" -- c-addr )
;
; Skip leading delimiters.  Parse characters ccc delimited by char.  An
; ambiguous condition exists if the length of the parsed string is greater
; than the implementation-defined length of a counted string.
;
; c-addr is the address of a transient region containing the parsed word as
; a counted string.  If the parse area was empty or contained no characters
; other than the delimiter, the resulting string has a zero length.  A space,
; not included in the length, follows the string.  A program may replace
; characters within the string.
;
; Note: The requirement to follow the string with a space is obsolescent and
; is included as a concession to existing programs that use CONVERT.  A
; program shall not depend on the existence of the space.
;
; ---
; : WORD ( char "<chars>ccc<char>" -- c-addr)
;   TRUE SWAP (parse) >R 'WORD 1+ R@ CMOVE
;   R@ 'WORD C!  BL 'WORD 1+ R> + C!
;   'WORD ;

            LINKTO(WHILE,0,4,'D',"ROW")
WORD:       JMP     ENTER
            .WORD   TRUE,SWAP,PPARSE,TOR,TICKWORD,ONEPLUS,RFETCH,CMOVE
            .WORD   RFETCH,TICKWORD,CSTORE,BL,TICKWORD,ONEPLUS,RFROM,PLUS,CSTORE
            .WORD   TICKWORD,EXIT


; ----------------------------------------------------------------------
; XOR [CORE] 6.1.2490 ( x1 x2 -- x3 )
;
; x3 is the bit-by-bit exclusive-or of x1 with x2.

            LINKTO(WORD,0,3,'R',"OX")
XOR:        SAVEDE
            POP     H           ; Pop x2.
            POP     D           ; Pop x1.
            MOV     A,H         ; Put x2's high byte into A,
            XRA     D           ; ..then XOR x1's high byte with A,
            MOV     H,A         ; ..and put the result into H.
            MOV     A,L         ; Put x2's low byte into A,
            XRA     E           ; ..then XOR x1's low byte with A,
            MOV     L,A         ; ..and put the result into L.
            PUSH    H           ; Push the result (HL).
            RESTOREDE
            NEXT


; ----------------------------------------------------------------------
; [ [CORE] 6.1.2500 "left-bracket" ( -- )
;
; Interpretation:
;   Interpretation semantics for this word are undefined.
;
; Compilation:
;   Perform the execution semantics given below.
;
; Execution: ( -- )
;   Enter interpretation state.  [ is an immediate word.

            LINKTO(XOR,1,1,'[',"")
LTBRACKET:  LXI     H,0
            SHLD    TICKSTATE
            NEXT


; ----------------------------------------------------------------------
; ['] [CORE] 6.1.2510 "bracket-tick"
;
; Interpretation:
;   Interpretation semantics for this word are undefined.
;
; Compilation: ( "<spaces>name" -- )
;   Skip leading space delimiters.  Parse name delimited by a space.
;   Find name.  Append the run-time semantics given below to the current
;   definition.
;
;   An ambiguous condition exists if name is not found.
;
; Run-time: ( -- xt )
;   Place name's execution token xt on the stack.  The execution token
;   returned by the compiled phrase "['] X " is the same value returned by
;   "' X " outside of compilation state.
;
; ---
; : ['] ( "<spaces>name" -- )   '  ['] LIT COMPILE,  , ; IMMEDIATE

            LINKTO(LTBRACKET,1,3,']',"\'[")
BRACKETTICK:JMP     ENTER
            .WORD   TICK,LIT,LIT,COMPILECOMMA,COMMA,EXIT


; ----------------------------------------------------------------------
; [CHAR] [CORE] 6.1.2520 "bracket-char"
;
; Interpretation:
;   Interpretation semantics for this word are undefined.
;
; Compilation: ( "<spaces>name" -- )
;   Skip leading space delimiters.  Parse name delimited by a space.
;   Append the run-time semantics given below to the current definition.
;
; Run-time: ( -- char )
;   Place char, the value of the first character of name, on the stack.
;
; ---
; : [CHAR] ( "<spaces>name" -- char)   CHAR  ['] LIT COMPILE,  , ; IMMEDIATE

            LINKTO(BRACKETTICK,1,6,']',"RAHC[")
BRACKETCHAR:JMP     ENTER
            .WORD   CHAR,LIT,LIT,COMPILECOMMA,COMMA,EXIT


; ----------------------------------------------------------------------
; ] [CORE] 6.1.2540 "right-bracket" ( -- )
;
; Enter compilation state.

            LINKTO(BRACKETCHAR,0,1,']',"")
RTBRACKET:  LXI     H,0FFFFh
            SHLD    TICKSTATE
            NEXT



; ======================================================================
; CORE Constants (implementation details)
; ======================================================================

; ----------------------------------------------------------------------
; Input Control Block
;
; Stores information about an input source.

ICBLINEEND: .EQU    0           ; Offset to end of line.
ICBLINESTART:.EQU   2           ; Offset from ICB to start of line cell.
ICBSOURCEID:.EQU    4           ; Offset to SOURCE-ID for this source.
ICBTOIN:    .EQU    6           ; Offset to >IN value.


; ======================================================================
; CORE Words (implementation details)
; ======================================================================

; ----------------------------------------------------------------------
; 'S" [MFORTH] "tick-s-quote" ( -- addr )
;
; addr is the address of the start of the S" buffer.

            LINKTO(RTBRACKET,0,3,022h,"S\'")
TICKSQUOTE: LHLD    DP
            PUSH    D
            LXI     D,SQOFFSET
            DAD     D
            XTHL
            XCHG
            NEXT


; ----------------------------------------------------------------------
; 'WORD [MFORTH] "tick-word" ( -- addr )
;
; addr is the address of the start of the WORD buffer.

            LINKTO(TICKSQUOTE,0,5,'D',"ROW\'")
TICKWORD:   LHLD    DP
            PUSH    D
            LXI     D,WORDOFFSET
            DAD     D
            XTHL
            XCHG
            NEXT


; ----------------------------------------------------------------------
; (?do) [MFORTH] "paren-question-do-paren" ( n1|u1 n2|u2 -- ) ( R: -- | loop-sys )
;
; If n1|u1 is equal to n2|u2, continue execution at the location given by
; the consumer of do-sys.  Otherwise set up loop control parameters with
; index n2|u2 and limit n1|u1 and continue executing immediately following
; ?DO.  Anything already on the return stack becomes unavailable until the
; loop control parameters are discarded.  An ambiguous condition exists if
; n1|u1 and n2|u2 are not both of the same type.

            LINKTO(TICKWORD,0,5,029h,"od?(")
pqdo:       SAVEDE
            POP     H           ; Pop index into HL.
            POP     D           ; Pop limit into DE.
            RSPUSH(D,E)         ; Push limit onto return stack.
            RSPUSH(H,L)         ; Push index onto return stack.
            PUSH    B           ; Save BC.
            MOV     B,D         ; Move the limit
            MOV     C,E         ; ..to BC.
            DSUB                ; HL=HL-BC
            POP     B           ; Restore BC.
            RESTOREDE
            JNZ     _pqdoBEGIN  ; Begin the loop if the values are not equal.
            RSPOP(H,L)          ; Remove the loop
            RSPOP(H,L)          ; ..items from the return stack.
            LHLX                ; Get the branch address into HL.
            XCHG                ; Swap the branch address into DE.
            JMP     _pqdoDONE   ; We're done.
_pqdoBEGIN: INX     D           ; Skip the
            INX     D           ; ..branch address.
_pqdoDONE:  NEXT


; ----------------------------------------------------------------------
; (do) [MFORTH] "paren-do-paren" ( n1|u1 n2|u2 -- ) ( R: -- loop-sys )
;
; Set up loop control parameters with index n2|u2 and limit n1|u1. An
; ambiguous condition exists if n1|u1 and n2|u2 are not both the same type.
; Anything already on the return stack becomes unavailable until the
; loop-control parameters are discarded.

            LINKTO(pqdo,0,4,029h,"od(")
pdo:        POP     H           ; Pop index into HL,
            XTHL                ; ..swap the index and the limit,
            RSPUSH(H,L)         ; ..and push the limit onto the return stack.
            POP     H           ; Pop index into HL
            RSPUSH(H,L)         ; ..and push the index onto the return stack.
            NEXT


; ----------------------------------------------------------------------
; (+loop) [MFORTH] "plus-loop" ( n -- ) ( R: loop-sys1 -- | loop-sys2 )
;
; An ambiguous condition exists if the loop control parameters are
; unavailable.  Add n to the loop index.  If the loop index did not cross
; the boundary between the loop limit minus one and the loop limit, continue
; execution at the beginning of the loop.  Otherwise, discard the current
; loop control parameters and continue execution immediately following the loop.  

            LINKTO(pdo,0,7,029h,"pool+(")
pplusloop:  SAVEDE
            RSPOP(H,L)          ; Get the current loop index from the RS.
            POP     D           ; Get the increment from the stack.
            MOV     A,D         ; Move the high byte of the increment to A,
            ORA     A           ; ..see if the increment is positive or zero,
            PUSH    PSW         ; ..and then store the result on the stack.
            DAD     D           ; Increment the loop index.
            RSFETCH(D,E)        ; Get the loop limit from the RS
            RSPUSH(H,L)         ; ..and put the new loop index back onto the RS.
            POP     PSW         ; See if the increment is positive or zero
            JP      _pplPOSINCR ; ..and then calculate the flag appropriately.
_pplsNEGINCR:PUSH   B           ; Save BC.
            MOV     B,D         ; Move the loop limit
            MOV     C,E         ; ..into BC.
            DSUB                ; Subtract the limit from the index.
            POP     B           ; Restore BC.
            JP      _pplCONTINUE; Continue if index was >= limit,
            JMP     _pplUNLOOP  ; ..otherwise unloop.
_pplPOSINCR:PUSH    B           ; Save BC.
            MOV     B,D         ; Move the loop limit
            MOV     C,E         ; ..into BC.
            DSUB                ; Subtract the limit from the index.
            POP     B           ; Restore BC.
            JP      _pplUNLOOP  ; Unloop if index was >= limit.
_pplCONTINUE:RESTOREDE
            LHLX                ; Get the branch address into HL.
            XCHG                ; Swap the branch address into DE.
            JMP     _pplDONE    ; We're done.
_pplUNLOOP: RESTOREDE
            RSPOP(H,L)          ; Pop the loop index.
            RSPOP(H,L)          ; Pop the loop limit.
            INX     D           ; Skip the
            INX     D           ; ..branch address.
_pplDONE:   NEXT


; ----------------------------------------------------------------------
; (loop) [MFORTH] "paren-loop-paren" ( -- ) ( R: loop-sys1 -- | loop-sys2 )
;
; An ambiguous condition exists if the loop control parameters are
; unavailable.  Add one to the loop index.  If the loop index is then equal
; to the loop limit, discard the loop parameters and continue execution
; immediately following the loop.  Otherwise continue execution at the
; beginning of the loop.

            LINKTO(pplusloop,0,6,029h,"pool(")
ploop:      SAVEDE
            RSPOP(H,L)          ; Get the current loop index from the RS
            INX     H           ; ..and increment the loop index.
            RSFETCH(D,E)        ; Get the loop limit from the RS
            RSPUSH(H,L)         ; ..and put the new loop index back onto the RS.
            PUSH    B           ; Save BC.
            MOV     B,D         ; Move the loop limit
            MOV     C,E         ; ..into BC.
            DSUB                ; Subtract the limit from the index.
            POP     B           ; Restore BC.
            RESTOREDE
            JZ      _ploopUNLOOP; Loop is done if the values are equal (zero).
            LHLX                ; Get the branch address into HL.
            XCHG                ; Swap the branch address into DE.
            JMP     _ploopDONE  ; We're done.
_ploopUNLOOP:RSPOP(H,L)         ; Pop the loop index.
            RSPOP(H,L)          ; Pop the loop limit.
            INX     D           ; Skip the
            INX     D           ; ..branch address.
_ploopDONE: NEXT


; ----------------------------------------------------------------------
; (s") [MFORTH] "paren-s-quote-paren" ( -- c-addr u )
;
; Runtime behavior of S": return c-addr and u.

            LINKTO(ploop,0,4,029h,"\"s(")
PSQUOTE:    LHLX                ; Read string count from instruction stream.
            INX     D           ; Skip over count
            INX     D           ; ..in instruction stream.
            PUSH    D           ; Push string address onto the stack.
            PUSH    H           ; Push string count onto the stack.
            XCHG                ; IP to HL, count to DE.
            DAD     D           ; Add count to address to skip over string.
            XCHG                ; Put IP back in DE (pointing after string).
            NEXT


; ----------------------------------------------------------------------
; 0 [MFORTH] "zero" ( -- 0 )
;
; Push zero onto the stack.

            LINKTO(PSQUOTE,0,1,'0',"")
ZERO:       LXI     H,0
            PUSH    H
            NEXT


; ----------------------------------------------------------------------
; 0branch [MFORTH] "zero-branch" ( flag -- )
;
; If flag is false, then set the instruction pointer to the address that is
; in the next cell of the instruction stream, otherwise skip over the branch
; address and continue processing instructions.

            LINKTO(ZERO,0,7,'h',"cnarb0")
zbranch:    POP     H           ; Get the flag.
            MOV     A,H         ; See if the flag is zero by moving H to A
            ORA     L           ; ..and then ORing A with L.
            JNZ     _zbraTRUE   ; True?  Skip the branch.
            LHLX                ; Get the branch address into HL.
            XCHG                ; Swap the branch address into DE.
            JMP     _zbraDONE   ; We're done.
_zbraTRUE:  INX     D           ; Skip the
            INX     D           ; ..branch address.
_zbraDONE:  NEXT


; ----------------------------------------------------------------------
; 1 [MFORTH] "one" ( -- 1 )
;
; Push one onto the stack.

            LINKTO(zbranch,0,1,'1',"")
ONE:        LXI     H,1
            PUSH    H
            NEXT


; ----------------------------------------------------------------------
; >DIGIT [MFORTH] "to-digit" ( u -- char )
;
; char is the digit u converted to the values 0-9A-Z.
;
; ---
; >DIGIT ( u -- c ) DUP 9 > 7 AND + 48 + ;

            LINKTO(ONE,0,6,'T',"IGID>")
TODIGIT:    POP     H
            MOV     A,L
            CPI     00Ah
            JC       _todigit2  ; u is < 10, so just add 030h for 0-9.
            ADI     7           ; u is >= 10, add an extra 7 to get to A-Z.
_todigit2:  ADI     030h
            MOV     L,A
            PUSH    H
            NEXT


; ----------------------------------------------------------------------
; ?DNEGATE [MFORTH] ( d1 n -- d2 )
;
; Negate d1 if n is negative.
;
; ---
; : ?DNEGATE ( d1 n -- d2)   0< IF DNEGATE THEN ;

            LINKTO(TODIGIT,0,8,'E',"TAGEND?")
QDNEGATE:   JMP     ENTER
            .WORD   ZEROLESS,zbranch,_dnegate1,DNEGATE
_dnegate1:  .WORD   EXIT


; ----------------------------------------------------------------------
; ?NEGATE [MFORTH] ( n1 n2 -- n3 )
;
; Negate n1 if n2 is negative.
;
; ---
; : ?NEGATE ( n1 n2 -- n3)   0< IF NEGATE THEN ;

            LINKTO(QDNEGATE,0,7,'E',"TAGEN?")
QNEGATE:    JMP     ENTER
            .WORD   ZEROLESS,zbranch,_negate1,NEGATE
_negate1:   .WORD   EXIT


; ----------------------------------------------------------------------
; branch [MFORTH] ( -- )
;
; Set the instruction pointer to the address that is in the next cell of
; the instruction stream.

            LINKTO(QNEGATE,0,6,'h',"cnarb")
branch:     LHLX                ; Get the branch address into HL.
            XCHG                ; Swap the branch address into DE.
            NEXT


; ----------------------------------------------------------------------
; DIGIT? [MFORTH] "digit-question" ( char -- u -1 | 0 )
;
; Attempts to convert char to a numeric value using the current BASE.
; Pushes the numeric value and -1 to the stack if the value was converted,
; otherwise pushes 0 to the stack.

            LINKTO(branch,0,6,'?',"TIGID")
DIGITQ:     MOV     H,B         ; Get the contents
            MVI     L,USERBASE  ; ..of the BASE
            MOV     L,M         ; ..user variable in L.
            XTHL                ; Swap the character with the BASE,
            MOV     A,L         ; ..move the character into A,
            POP     H           ; ..and then get the BASE back into L.
            SUI     030h        ; Is char > "0"
            CPI     00Ah        ; ..and > "9"?
            JC      _digitq1    ; ..No: check the base and continue.
            SUI     7           ; Yes: subtract 7,
            CPI     00Ah        ; ..make sure that the char is > "9"
            JC      _digitqFLSE ; ..and fail if not (char between "9" and "A").
_digitq1:   CMP     L           ; Make sure that digit is less than BASE
            JNC     _digitqFLSE ; ..and fail if not.
            MOV     L,A         ; Move the digit to L,
            MVI     H,0         ; ..clear H,
            PUSH    H           ; ..and push the digit.
            LXI     H,0FFFFh    ; Put true in HL.
            JMP     _digitqDONE ; We're done.
_digitqFLSE:LXI     H,0         ; Put false in HL.
_digitqDONE:PUSH    H           ; Push the flag to the stack.
            NEXT


; ----------------------------------------------------------------------
; END-LOOP [MFORTH] ( do-orig pdo-xt -- )
;
; Completes the loop whose loop-sys parameters on the stack.  pdo-xt
; points to either (loop) or (+loop) and is compiled into the end of
; the loop.
;
; ---
; : END-LOOP ( do-orig pdo-xt)
;   COMPILE, ,  'PREVLEAVE @ HERE>CHAIN ; IMMEDIATE

ENDLOOP:    JMP     ENTER
            .WORD   COMPILECOMMA,COMMA
            .WORD   LIT,TICKPREVLEAVE,FETCH,HERETOCHAIN,EXIT


; ----------------------------------------------------------------------
; (FIND) [MFORTH] "paren-find-paren" ( c-addr u -- c-addr u 0 | xt 1 | xt -1 )
;
; Find the definition named in the string at c-addr with length u in the
; word list whose latest definition is pointed to by nfa.  If the
; definition is not found, return the string and zero.  If the
; definition is found, return its execution token xt.  If the definition
; is immediate, also return one (1), otherwise also return minus-one
; (-1).  For a given string, the values returned by FIND while compiling
; may differ from those returned while not compiling.
;
; ---
; : (FIND) ( c-addr u -- c-addr u 0 | xt 1 | xt -1 )
;   CONTEXT >R  BEGIN
;       2DUP R@ @ SEARCH-WORDLIST ( ca u 0 | ca u xt 1 | ca u xt -1)
;       ?DUP 0<> IF 2NIP R> DROP EXIT THEN ( ca u)
;       R> CELL+  DUP @ 0= IF DROP 0 EXIT THEN  >R
;   AGAIN ;

            LINKTO(DIGITQ,0,6,029h,"DNIF(")
PFIND:      JMP     ENTER
            .WORD   CONTEXT,TOR
_pfind1:    .WORD   TWODUP,RFETCH,FETCH,SEARCHWORDLIST,
            .WORD   QDUP,ZERONOTEQUALS,zbranch,_pfind2
            .WORD   TWONIP,RFROM,DROP,EXIT
_pfind2:    .WORD   RFROM,CELLPLUS,DUP,FETCH,ZEROEQUALS,zbranch,_pfind3
            .WORD   DROP,ZERO,EXIT
_pfind3:    .WORD   TOR,branch,_pfind1


; ----------------------------------------------------------------------
; (KEY) [MFORTH] "paren-key-paren" ( -- char )
;
; Receive one character char, a member of the implementation-defined
; character set.  Keyboard events that do not correspond to such characters
; are discarded until a valid character is received, and those events are
; subsequently unavailable.
;
; All standard characters can be received.  Characters received by KEY are
; not displayed.
;
; Any standard character returned by KEY has the numeric value specified in
; 3.1.2.1 Graphic characters.  Programs that require the ability to receive
; control characters have an environmental dependency.
;
; ---
; TODO: Apparently the Model 100 does magical, special things here and can
; convert function keys to text (maybe we can use this as a macro feature?),
; and sets Carry when the key is "special".  We probably want to avoid "special"
; keys and just accept non-special keys.  For now we just take the easy route.

            LINKTO(PFIND,0,5,029h,"YEK(")
PKEY:       CALL    STDCALL     ; Call the
            .WORD   012CBh      ; ..CHGET routine.
            MVI     H,0         ; Clear H,
            MOV     L,A         ; ..put the character in L,
            PUSH    H           ; ..and push the character onto the stack.
            NEXT


; ----------------------------------------------------------------------
; HERE>CHAIN [MFORTH] "here-to-chain" ( addr -- )
;
; Store HERE in the zero-terminated chain beginning at addr.  Each addr
; is expected to contain the addr of the previous element in the chain.
; The last element in the chain (which could be addr itself) should
; contain zero.
;
; ---
; HERE>CHAIN ( addr -- )
;   BEGIN ?DUP WHILE DUP @ HERE ( a a' h) ROT ! REPEAT ;

            LINKTO(PKEY,0,10,'N',"IAHC>EREH")
HERETOCHAIN:JMP     ENTER
_htc1:      .WORD   QDUP,zbranch,_htc2
            .WORD   DUP,FETCH,HERE,ROT,STORE,branch,_htc1
_htc2:      .WORD   EXIT


; ----------------------------------------------------------------------
; HIDDEN? [MFORTH] ( dict-addr -- flag )
;
; flag is true if and only if the given dictionary word is hidden.
;
; ---
; : HIDDEN ( dict-addr -- f )   C@ 64 AND 0<> ;

            LINKTO(HERETOCHAIN,0,7,'?',"NEDDIH")
HIDDENQ:    JMP     ENTER
            .WORD   CFETCH,LIT,64,AND,ZERONOTEQUALS,EXIT


; ----------------------------------------------------------------------
; HIDE [MFORTH] ( -- )
;
; Prevent the most recent definition from being found in the dictionary.
; ---
; : HIDE ( -- )   LATEST @  DUP C@ [HEX] 40 OR  SWAP C! ;

            LINKTO(HIDDENQ,0,4,'E',"DIH")
HIDE:       JMP     ENTER
            .WORD   LATEST,FETCH,DUP,CFETCH,LIT,040h,OR,SWAP,CSTORE,EXIT


; ----------------------------------------------------------------------
; HLD [MFORTH] "h-l-d" ( -- c-addr )
;
; c-addr is the address of the cell containing the current location in
; the Pictured Numeric Output hold buffer.

            LINKTO(HIDE,0,3,'D',"LH")
HLD:        LXI     H,TICKHLD
            PUSH    H
            NEXT


; ----------------------------------------------------------------------
; ICB [MFORTH] "i-c-b" ( -- c-addr )
;
; c-addr is the address of the current Input Control Block.

            LINKTO(HLD,0,3,'B',"CI")
ICB:        LHLD    TICKICB
            PUSH    H
            NEXT


; ----------------------------------------------------------------------
; INIT-ICBS [MFORTH] "init-icbs" ( -- )
;
; Initialize all of the Input Control Blocks.  The current Input Control
; Block should be configured immediately after executing this word.
;
; ---
; : INIT-ICBS ( -- )
;   ICBSTART [ MAXICBS 2* 2* 2* ] 0 FILL  ICBSTART TO ICB ;

            LINKTO(ICB,0,9,'S',"BCI-TINI")
INITICBS:   JMP     ENTER
            .WORD   LIT,ICBSTART,LIT,MAXICBS*8,ZERO,FILL
            .WORD   LIT,ICBSTART,LIT,TICKICB,STORE,EXIT


; ----------------------------------------------------------------------
; INTERPRET [MFORTH] ( i*x -- j*x )
;
; Interpret the line in the current Input Control Block.
;
; : INTERPRET ( i*x -- j*x )
;   0 >IN !
;   BEGIN  PARSE-WORD  DUP WHILE
;       (FIND) ( ca u 0=notfound | xt 1=imm | xt -1=interp)
;       ?DUP IF ( xt 1=imm | xt -1=interp)
;           1+  STATE @ 0=  OR ( xt 2=imm | xt 0=interp)
;           IF EXECUTE ELSE COMPILE, THEN
;       ELSE
;           NUMBER? IF
;               STATE @ IF POSTPONE LITERAL THEN
;               -- Interpreting; leave number on stack.
;           ELSE
;               TYPE  SPACE  [CHAR] ? EMIT  CR  ABORT
;           THEN
;       THEN
;   REPEAT ( j*x ca u) 2DROP ;

            LINKTO(INITICBS,0,9,'T',"ERPRETNI")
INTERPRET:  JMP     ENTER
            .WORD   ZERO,TOIN,STORE
_interpret1:.WORD   PARSEWORD,DUP,zbranch,_interpret6
            .WORD   PFIND,QDUP,zbranch,_interpret3
            .WORD   ONEPLUS,STATE,FETCH,ZEROEQUALS,OR,zbranch,_interpret2
            .WORD   EXECUTE,branch,_interpret5
_interpret2:.WORD   COMPILECOMMA,branch,_interpret5
_interpret3:.WORD   NUMBERQ,zbranch,_interpret4
            .WORD   STATE,FETCH,zbranch,_interpret5
            .WORD   LITERAL,branch,_interpret5
_interpret4:.WORD   TYPE,SPACE,LIT,'?',EMIT,CR,ABORT
_interpret5:.WORD   branch,_interpret1
_interpret6:.WORD   TWODROP
            .WORD   EXIT


; ----------------------------------------------------------------------
; LATEST [MFORTH] "latest" ( -- a-addr )
;
; a-addr is the address of a cell containing the address of the link
; field of the latest word added to the dictionary.

            LINKTO(INTERPRET,0,6,'T',"SETAL")
LATEST:     JMP     ENTER
            .WORD   GETCURRENT,EXIT


; ----------------------------------------------------------------------
; LIT [MFORTH] ( -- x)
;
; Push the next value in the PFA to the stack.

            LINKTO(LATEST,0,3,'T',"IL")
LIT:        LHLX            ; Read constant from instruction stream.
            PUSH    H       ; ..and push constant to stack.
            INX     D       ; Skip over constant
            INX     D       ; ..in instruction stream.
            NEXT


; ----------------------------------------------------------------------
; NFA>CFA [MFORTH] "n-f-a-to-c-f-a" ( nfa-addr -- cfa-addr )
;
; cfa-addr is the Code Field Address for the word whose Name Field Address
; is nfa-addr.
;
; ---
; : NFA>CFA ( nfa-addr -- cfa-addr)   NFATOCFASZ + ;

            LINKTO(LIT,0,7,'A',"FC>AFN")
NFATOCFA:   POP     H
            INXNFATOCFA(H)
            PUSH    H
            NEXT


; ----------------------------------------------------------------------
; NFA>LFA [MFORTH] "n-f-a-to-l-f-a" ( nfa-addr -- lfa-addr )
;
; lfa-addr is the Link Field Address for the word whose Name Field Address
; is nfa-addr.
;
; ---
; : NFA>LFA ( nfa-addr -- lfa-addr)   1+ ;

            LINKTO(NFATOCFA,0,7,'A',"FL>AFN")
NFATOLFA:   POP     H
            INX     H
            PUSH    H
            NEXT


; ----------------------------------------------------------------------
; NUMBER? [MFORTH] "number-question" ( c-addr u -- c-addr u 0 | n -1 )
;
; Attempt to convert a string at c-addr of length u into digits, using
; the radix in BASE.  The number and -1 is returned if the conversion
; was successful, otherwise 0 is returned.
;
; ---
; : NUMBER? ( ca u -- ca u 0 | n -1 )
;   SIGN? >R  2DUP 0 0 2SWAP  >NUMBER  ( ca u ud ca2 u2)
;   IF DROP 2DROP  R> DROP  0 ELSE
;      DROP 2NIP DROP  >R ?NEGATE  -1 THEN ;

            LINKTO(NFATOLFA,0,7,'?',"REBMUN")
NUMBERQ:    JMP     ENTER
            .WORD   SIGNQ,TOR,TWODUP,ZERO,ZERO,TWOSWAP
            .WORD       TONUMBER,zbranch,_numberq1
            .WORD   DROP,TWODROP,RFROM,DROP,ZERO,branch,_numberq2
_numberq1:  .WORD   DROP,TWONIP,DROP,RFROM,QNEGATE,LIT,0FFFFh
_numberq2:  .WORD   EXIT


; ----------------------------------------------------------------------
; POPICB [MFORTH] "push-i-c-b" ( -- )
;
; Point ICB at the previous Input Control Block.
;
; ---
; : POPICB ( --)  ICB 8 - TO ICB ;

            LINKTO(NUMBERQ,0,6,'B',"CIPOP")
POPICB:     JMP     ENTER
            .WORD   ICB,LIT,8,MINUS,LIT,TICKICB,STORE,EXIT


; ----------------------------------------------------------------------
; PUSHICB [MFORTH] "push-i-c-b" ( -- )
;
; Point ICB at the next Input Control Block.
;
; ---
; : PUSHICB ( --)  ICB 8 + TO ICB ;

            LINKTO(POPICB,0,7,'B',"CIHSUP")
PUSHICB:    JMP     ENTER
            .WORD   ICB,LIT,8,PLUS,LIT,TICKICB,STORE,EXIT


; ----------------------------------------------------------------------
; REVEAL [MFORTH] ( -- )
;
; Allow the most recent definition to be found in the dictionary.
;
; ---
; : REVEAL ( -- )   LATEST @  DUP C@ [HEX] BF AND  SWAP C! ;

            LINKTO(PUSHICB,0,6,'L',"AEVER")
REVEAL:     JMP     ENTER
            .WORD   LATEST,FETCH,DUP,CFETCH,LIT,0BFh,AND,SWAP,CSTORE,EXIT


; ----------------------------------------------------------------------
; SIGN? [MFORTH] "sign-question" ( c-addr1 u1 -- c-addr2 u2 flag )
;
; 
; Attempt to convert a string at c-addr of length u into digits, using
; the radix in BASE.  The number and -1 is returned if the conversion
; was successful, otherwise 0 is returned.
;
; ---
; : SIGN? ( ca1 u1 -- ca2 u2 f )
;   OVER  C@  DUP [CHAR] - =  OVER [CHAR] + = OR  IF
;       [CHAR] - = IF -1 ELSE 0 THEN  >R 1 /STRING R>
;   ELSE DROP 0 THEN ;

            LINKTO(REVEAL,0,5,'?',"NGIS")
SIGNQ:      JMP     ENTER
            .WORD   OVER,CFETCH,DUP,LIT,'-',EQUALS,OVER,LIT,'+',EQUALS,OR
            .WORD       zbranch,_signq3
            .WORD   LIT,'-',EQUALS,zbranch,_signq1,LIT,0FFFFh,branch,_signq2
_signq1:    .WORD   ZERO
_signq2:    .WORD   TOR,ONE,SLASHSTRING,RFROM,branch,_signq4
_signq3:    .WORD   DROP,ZERO
_signq4:    .WORD   EXIT


; ----------------------------------------------------------------------
; UD* [MFORTH] "u-d-star" ( ud1 u1 -- ud2 )
;
; Multiply ud1 by u1, giving the unsigned double-cell product ud2.
;
; ---
; UD* ( ud1 u1 -- ud2)   DUP >R UM* DROP  SWAP R> UM* ROT + ;

            LINKTO(SIGNQ,0,3,'*',"DU")
UDSTAR:     JMP     ENTER
            .WORD   DUP,TOR,UMSTAR,DROP
            .WORD   SWAP,RFROM,UMSTAR,ROT,PLUS
            .WORD   EXIT


; ----------------------------------------------------------------------
; UD. [MFORTH] "u-d-dot" ( ud -- )
;
; Display ud in free field format.
;
; ---
; : UD. ( ud -- )   <# #S #> TYPE SPACE ;

            LINKTO(UDSTAR,0,3,'.',"DU")
UDDOT:      JMP     ENTER
            .WORD   LESSNUMSIGN,NUMSIGNS,NUMSIGNGRTR,TYPE,SPACE
            .WORD   EXIT


; ----------------------------------------------------------------------
; UD/MOD [MFORTH] "u-d-slash-mod" ( ud1 u1 -- n ud2 )
;
; Divide ud1 by u1 giving the quotient ud2 and the remainder n.
;
; ---
; UD/MOD ( ud1 u1 -- n ud2 )   >R 0 R@ UM/MOD  R> SWAP >R UM/MOD R> ;

            LINKTO(UDDOT,0,6,'D',"OM/DU")
LAST_CORE:
UDSLASHMOD: JMP     ENTER
            .WORD   TOR,ZERO,RFETCH,UMSLASHMOD
            .WORD   RFROM,SWAP,TOR,UMSLASHMOD,RFROM
            .WORD   EXIT
