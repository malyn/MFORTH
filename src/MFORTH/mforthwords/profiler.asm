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
; PROFILER Words
; ======================================================================

; ----------------------------------------------------------------------
; PRINT-PROFILE [MFORTH] ( -- )
;
; Print the execution counts and word names for each word in the system
; that was executed at least once in the previous profile run.
;
; ---
; : PRINT-PROFILE ( -- )
;   PRN  LATEST @  BEGIN  DUP PROFOFFSET + @  ?DUP IF U. DUP .NAME CR THEN
;   NFA>LFA @  DUP 0= UNTIL DROP  [HEX] 0C EMIT  LCD ;

            LINKTO(LINK_PROFILER,0,13,'E',"LIFORP-TNIRP")
PRINTPROFILE:JMP    ENTER
            .WORD   PRN,LATEST,FETCH
_printprof1:.WORD   DUP,LIT,PROFOFFSET,PLUS,FETCH,QDUP,zbranch,_printprof2
            .WORD   UDOT,DUP,DOTNAME,CR
_printprof2:.WORD   NFATOLFA,FETCH,DUP,ZEROEQUALS,zbranch,_printprof1
            .WORD   DROP,LIT,0Ch,EMIT,LCD,EXIT


; ----------------------------------------------------------------------
; PROFILE [MFORTH] ( xt -- )
;
; Profile the given xt.
;
; ---
; : PROFILE ( xt -- )
;   CLEAR-PROFILER
;   1 PROFILING !  0 PROFILETICKS !  EXECUTE  PROFILETICKS @  0 PROFILING !
;   CR ." Total time:" 2* 2* U. ." us" ;

            LINKTO(PRINTPROFILE,0,7,'E',"LIFORP")
PROFILE:    JMP     ENTER
            .WORD   CLEARPROFILE
            .WORD   ONE,LIT,PROFILING,STORE,ZERO,LIT,PROFILETICKS,STORE
            .WORD   EXECUTE,LIT,PROFILETICKS,FETCH,ZERO,LIT,PROFILING,STORE
            .WORD   CR,PSQUOTE,12
            .BYTE   "Total time: "
            .WORD   TYPE,TWOSTAR,TWOSTAR,UDOT,PSQUOTE,2
            .BYTE   "us"
            .WORD   TYPE,EXIT



; ======================================================================
; PROFILER Words (implementation details)
; ======================================================================

; ----------------------------------------------------------------------
; CLEAR-PROFILE [MFORTH] ( -- )
;
; Clear all of the current profiler execution counts.
;
; ---
; : CLEAR-PROFILE ( -- )
;   LATEST @  BEGIN  0 OVER PROFOFFSET + !  NFA>LFA @  DUP 0= UNTIL DROP ;

            LINKTO(PROFILE,0,13,'E',"LIFORP-RAELC")
LAST_PROFILER:
CLEARPROFILE:JMP    ENTER
            .WORD   LATEST,FETCH
_clearprof1:.WORD   ZERO,OVER,LIT,PROFOFFSET,PLUS,STORE
            .WORD   NFATOLFA,FETCH,DUP,ZEROEQUALS,zbranch,_clearprof1
            .WORD   DROP,EXIT
