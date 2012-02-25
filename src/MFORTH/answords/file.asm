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
; FILE Constants
; ======================================================================

FCBGENNUM:  .EQU    0           ; Offset from FCB to generation number.
FCBADDR:    .EQU    2           ; Offset to address of file.
FCBPOS:     .EQU    4           ; Offset to position in file.
FCBLEN      .EQU    6           ; Offset to length of file.

IOROK       .EQU    0           ; No error.
IORFNF      .EQU    1           ; File not found or invalid filename (>6 chars)
IORRDONLY   .EQU    2           ; Files cannot be opened R/W.
IORBADFILEID .EQU   3           ; Bad fileid.



; ======================================================================
; FILE Words
; ======================================================================

; ----------------------------------------------------------------------
; CLOSE-FILE [FILE] 11.6.1.0900 ( fileid -- ior )
;
; Close the file identified by fileid.  ior is the implementation-defined
; I/O result code.
;
; : CLOSE-FILE ( fileid -- ior)
;   FILEID>FCB? ?DUP IF EXIT THEN
;   FCBADDR +  0 SWAP !  IOROK ;

            LINKTO(LINK_FILE,0,10,'E',"LIF-ESOLC")
CLOSEFILE:  JMP     ENTER
            .WORD   FILEIDTOFCBQ,QDUP,zbranch,_closefile1,EXIT
_closefile1:.WORD   LIT,FCBADDR,PLUS,ZERO,SWAP,STORE,LIT,IOROK,EXIT


; ----------------------------------------------------------------------
; OPEN-FILE [FILE] 11.6.1.1970 ( c-addr u fam -- fileid ior )
;
; Open the file named in the character string specified by c-addr u,
; with file access method indicated by fam.  The meaning of values of
; fam is implementation defined.
;
; If the file is successfully opened, ior is zero, fileid is its identifier,
; and the file has been positioned to the start of the file.
;
; Otherwise, ior is the implementation-defined I/O result code and fileid
; is undefined.
;
; : OPEN-FILE ( c-addr u fam -- fileid ior)
;   R/O <> IF 2DROP 0 IORRDONLY EXIT THEN
;   FIND-FILE ?DUP IF 0 SWAP EXIT THEN  NEW-FCB >R  ( file-addr file-len  R:fcb)
;   R@ FCBGENNUM + DUP C@ 1+ SWAP C!
;   R@ FCBLEN + !
;   DUP R@ FCBADDR + !  R@ FCBPOS + !
;   R> FCB>FILEID IOROK ;

            LINKTO(CLOSEFILE,0,9,'E',"LIF-NEPO")
OPENFILE:   JMP     ENTER
            .WORD   RO,NOTEQUALS,zbranch,_openfile1
            .WORD   TWODROP,ZERO,LIT,IORRDONLY,EXIT
_openfile1: .WORD   FINDFILE,QDUP,zbranch,_openfile2
            .WORD   ZERO,SWAP,EXIT
_openfile2: .WORD   NEWFCB,TOR
            .WORD   RFETCH,LIT,FCBGENNUM,PLUS,DUP,CFETCH,ONEPLUS,SWAP,CSTORE
            .WORD   RFETCH,LIT,FCBLEN,PLUS,STORE
            .WORD   DUP,RFETCH,LIT,FCBADDR,PLUS,STORE
            .WORD       RFETCH,LIT,FCBPOS,PLUS,STORE
            .WORD   RFROM,FCBTOFILEID,LIT,IOROK,EXIT


; ----------------------------------------------------------------------
; R/O [FILE] 11.6.1.2054 "r-o" ( -- fam )
;
; fam is the implementation-defined value for selecting the "read only"
; file access method.

            LINKTO(OPENFILE,0,3,'O',"/R")
RO:         JMP     ENTER
            .WORD   LIT,00000001b,EXIT


; ----------------------------------------------------------------------
; R/W [FILE] 11.6.1.2056 "r-w" ( -- fam )
;
; fam is the implementation-defined value for selecting the "read/write"
; file access method.

            LINKTO(RO,0,3,'W',"/R")
RW:         JMP     ENTER
            .WORD   LIT,00000011b,EXIT



; ======================================================================
; FILE Words (implementation details)
; ======================================================================

; ----------------------------------------------------------------------
; FCB>FILEID [MFORTH] "fcb-to-fileid" ( fcb-addr -- fileid )
;
; Return fileid given a valid File Control Block address (fcb-addr).
;
; Note that we add one to the zero-based file address in order to support
; the semantics of SOURCE-ID, which requires that zero refer to the user
; input device.  The first FCB, when its generation number is zero, would
; otherwise produce a fileid of zero.
;
; : FCB>FILEID ( fcb-addr -- fileid)
;   DUP FCBGENNUM + C@ 8 LSHIFT  SWAP FCBSTART - 1+  OR ;

            LINKTO(RW,0,10,'D',"IELIF>BCF")
FCBTOFILEID:JMP     ENTER
            .WORD   DUP,LIT,FCBGENNUM,PLUS,CFETCH,LIT,8,LSHIFT
            .WORD   SWAP,LIT,FCBSTART,MINUS,ONEPLUS,OR,EXIT


; ----------------------------------------------------------------------
; FILEID>FCB? [MFORTH] "fileid-to-fcb-question" ( fileid -- ior | fcb-addr 0 )
;
; Return the address of the File Control Block for the given fileid, or
; ior if the fileid is invalid.
;
; : FILEID>FCB? ( fileid -- ior | fcb-addr 0 )
;   DUP 8 RSHIFT  SWAP 255 AND
;   DUP 0=  OVER MAXFCBS >  OR IF 2DROP IORBADFILEID EXIT THEN
;   1- FCBSTART +  DUP FCBGENNUM + C@ ROT <> IF DROP IORBADFILEID EXIT THEN
;   DUP FCBADDR + @ 0= IF DROP IORBADFILEID EXIT THEN
;   IOROK ;

            LINKTO(FCBTOFILEID,0,11,'?',"BCF>DIELIF")
FILEIDTOFCBQ:JMP    ENTER
            .WORD   DUP,LIT,8,RSHIFT,SWAP,LIT,255,AND
            .WORD   DUP,ZEROEQUALS,OVER,LIT,MAXFCBS,GREATERTHAN
            .WORD       OR,zbranch,_fileidtofcbq1
            .WORD   TWODROP,LIT,IORBADFILEID,EXIT
_fileidtofcbq1:.WORD ONEMINUS,LIT,FCBSTART,PLUS,DUP,LIT,FCBGENNUM,PLUS
            .WORD       CFETCH,ROT,NOTEQUALS,zbranch,_fileidtofcbq2
            .WORD   DROP,LIT,IORBADFILEID,EXIT
_fileidtofcbq2:.WORD DUP,LIT,FCBADDR,PLUS,FETCH,ZEROEQUALS,zbranch,_fileidtofcbq3
            .WORD       DROP,LIT,IORBADFILEID,EXIT
_fileidtofcbq3:.WORD LIT,IOROK,EXIT


; ----------------------------------------------------------------------
; FIND-FILE [MFORTH] "find-file" ( c-addr u -- ior | file-addr file-len 0 )
;
; Find the file named in the character string specified by c-addr u.  No
; extension is expected, FIND-FILE will append the ".DO" extension.  If
; the file is found the address and length of the file and 0 will be
; returned, otherwise an ior will be returned.
;
; : FIND-FILE ( ca u -- ior | fa fl 0)
;   DUP 6 > IF 2DROP IORFNF EXIT THEN
;   TUCK  FILNAME SWAP MOVE  6 SWAP ?DO BL OVER C! 1+ LOOP
;   FILNAME 6 +  [CHAR] D OVER C!  [CHAR] O SWAP 1+ C!
;   SRCNAM ;

            LINKTO(FILEIDTOFCBQ,0,9,'E',"LIF-DNIF")
FINDFILE:   JMP     ENTER
            .WORD   DUP,LIT,6,GREATERTHAN,zbranch,_findfile1
            .WORD   TWODROP,LIT,IORFNF,EXIT
_findfile1: .WORD   TUCK,LIT,0FC93h,SWAP,MOVE
            .WORD   LIT,6,SWAP,pqdo,_findfile3
_findfile2: .WORD   BL,OVER,CSTORE,ONEPLUS,ploop,_findfile2
_findfile3: .WORD   LIT,0FC93h,LIT,6,PLUS
            .WORD   LIT,'D',OVER,CSTORE,LIT,'O',SWAP,ONEPLUS,CSTORE
            .WORD   SRCNAM,EXIT


; ----------------------------------------------------------------------
; INIT-FCBS [MFORTH] "init-fcbs" ( -- )
;
; Initialize all of the File Control Blocks.  This has the effect of
; "closing" any open files.
;
; : INIT-FCBS ( -- )   FCBSTART [ MAXFCBS 2* 2* 2* ] 0 FILL ;

            LINKTO(FINDFILE,0,9,'S',"BCF-TINI")
INITFCBS:   JMP     ENTER
            .WORD   LIT,FCBSTART,LIT,MAXFCBS*8,ZERO,FILL,EXIT


; ----------------------------------------------------------------------
; NEW-FCB [MFORTH] "new-fcb" ( -- 0 | fcb-addr )
;
; Find and return the address of an unused (and uninitialized) File
; Control Block.  Return 0 if the system has run out of File Control
; Blocks.
;
; : NEWFCB ( -- fcb-addr)
;   [ MAXFCBS 2* 2* 2* ] LITERAL 0 DO
;   FCBSTART I +  DUP FCBADDR + @ 0= IF UNLOOP EXIT THEN  DROP  8 +LOOP
;   0 ;

            LINKTO(INITFCBS,0,7,'B',"CF-WEN")
NEWFCB:     JMP     ENTER
            .WORD   LIT,MAXFCBS*8,ZERO,pdo
_newfcb1:   .WORD   LIT,FCBSTART,I,PLUS,DUP,LIT,FCBADDR,PLUS,FETCH
            .WORD   ZEROEQUALS,zbranch,_newfcb2,UNLOOP,EXIT
_newfcb2:   .WORD   DROP,LIT,8,pplusloop,_newfcb1
            .WORD   ZERO,EXIT


; ----------------------------------------------------------------------
; SRCNAM [MFORTH] ( -- ior | file-addr file-len 0 )
;
; Call the Main ROM's SRCNAM routine.  FILNAM has already been populated
; by the caller.

            LINKTO(NEWFCB,0,6,'M',"ANCRS")
LAST_FILE:
SRCNAM:     SAVEDE              ; Save DE
            PUSH    B           ; ..and BC, both of which are corrupted.
            CALL    STDCALL     ; Call the
            .WORD   20AFh       ; .."SRCNAM" routine.
            POP     B           ; Restore BC.
            JZ      _srcnamFAIL ; Zero indicates not found.
            PUSH    D           ; Push file-addr to the stack.
            XCHG                ; Get file-addr in HL.
            LXI     D,0         ; Initialize file-len to zero.
_srcnam1:   MOV     A,M         ; Get the next byte of the file into A,
            CPI     01Ah        ; ..see if it is EOF,
            JZ      _srcnam2    ; ..and exit the loop if so.
            INX     D           ; Increment the file-len,
            INX     H           ; ..increment the file pointer,
            JMP     _srcnam1    ; ..and continue looping.
_srcnam2:   PUSH    D           ; Push file-len onto the stack.
            LXI     H,IOROK     ; Put IOROK in HL.
            JMP     _srcnamDONE ; We're done.
_srcnamFAIL:LXI     H,IORFNF    ; Put the IOR in HL.
_srcnamDONE:PUSH    H           ; Push the flag to the stack.
            RESTOREDE
            NEXT
