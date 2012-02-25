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

; ----------------------------------------------------------------------
; I/O Result Codes
;
IOROK       .EQU    0           ; No error.
IORFNF      .EQU    1           ; File not found or invalid filename (>6 chars)
IORRDONLY   .EQU    2           ; Files cannot be opened R/W.
IORBADFILEID .EQU   3           ; Bad fileid.



; ======================================================================
; FILE Words
; ======================================================================

; ----------------------------------------------------------------------
; BIN [FILE] 11.6.1.0765 ( fam1 -- fam2 )
;
; Modify the implementation-defined file access method fam1 to additionally
; select a "binary", i.e., not line oriented, file access method, giving
; access method fam2.
;
; ---
; No-op in MFORTH as files are not opened in any specific mode.

            LINKTO(LINK_FILE,0,3,'N',"IB")
BIN:        NEXT


; ----------------------------------------------------------------------
; CLOSE-FILE [FILE] 11.6.1.0900 ( fileid -- ior )
;
; Close the file identified by fileid.  ior is the implementation-defined
; I/O result code.
;
; : CLOSE-FILE ( fileid -- ior)
;   FILEID>FCB? ?DUP IF EXIT THEN
;   FCBADDR +  0 SWAP !  IOROK ;

            LINKTO(BIN,0,10,'E',"LIF-ESOLC")
CLOSEFILE:  JMP     ENTER
            .WORD   FILEIDTOFCBQ,QDUP,zbranch,_closefile1,EXIT
_closefile1:.WORD   LIT,FCBADDR,PLUS,ZERO,SWAP,STORE,LIT,IOROK,EXIT


; ----------------------------------------------------------------------
; CREATE-FILE [FILE] 11.6.1.1010 ( c-addr u fam -- fileid ior )
;
; Create the file named in the character string specified by c-addr and u,
; and open it with file access method fam.  The meaning of values of fam is
; implementation defined.  If a file with the same name already exists,
; recreate it as an empty file.
;
; If the file was successfully created and opened, ior is zero, fileid is
; its identifier, and the file has been positioned to the start of the file.
;
; Otherwise, ior is the implementation-defined I/O result code and fileid is
; undefined.
;
; ---
; Always fails in MFORTH as the file system is read-only.

            LINKTO(CLOSEFILE,0,11,'E',"LIF-ETAERC")
CREATEFILE: JMP     ENTER
            .WORD   DROP,TWODROP,ZERO,LIT,IORRDONLY,EXIT


; ----------------------------------------------------------------------
; DELETE-FILE [FILE] 11.6.1.1190 ( c-addr u -- ior )
;
; Delete the file named in the character string specified by c-addr u.
; ior is the implementation-defined I/O result code.
;
; ---
; Always fails in MFORTH as the file system is read-only.

            LINKTO(CREATEFILE,0,11,'E',"LIF-ETELED")
DELETEFILE: JMP     ENTER
            .WORD   TWODROP,LIT,IORRDONLY,EXIT


; ----------------------------------------------------------------------
; FILE-POSITION [FILE] 11.6.1.1520 ( fileid -- ud ior )
;
; ud is the current file position for the file identified by fileid.  ior
; is the implementation-defined I/O result code.  ud is undefined if ior is
; non-zero.
;
; : FILE-POSITION ( fileid -- ud ior)
;   FILEID>FCB? ?DUP IF 0 SWAP EXIT THEN
;   DUP FCBPOS + @  SWAP FCBADDR + @  -  IOROK ;

            LINKTO(DELETEFILE,0,13,'N',"OITISOP-ELIF")
FILEPOSITION:JMP    ENTER
            .WORD   FILEIDTOFCBQ,QDUP,zbranch,_filepos1,ZERO,SWAP,EXIT
_filepos1:  .WORD   DUP,LIT,FCBPOS,PLUS,FETCH,SWAP,LIT,FCBADDR,PLUS,FETCH
            .WORD   MINUS,LIT,IOROK,EXIT


; ----------------------------------------------------------------------
; FILE-SIZE [FILE] 11.6.1.1522 ( fileid -- ud ior )
;
; ud is the size, in characters, of the file identified by fileid.  ior
; is the implementation-defined I/O result code.  This operation does not
; affect the value returned by FILE-POSITION.  ud is undefined if ior is
; non-zero.
;
; : FILE-SIZE ( fileid -- ud ior)
;   FILEID>FCB? ?DUP IF 0 SWAP EXIT THEN
;   DUP FCBEND + @  SWAP FCBADDR + @  -  IOROK ;

            LINKTO(FILEPOSITION,0,9,'E',"ZIS-ELIF")
FILESIZE:    JMP    ENTER
            .WORD   FILEIDTOFCBQ,QDUP,zbranch,_filesize1,ZERO,SWAP,EXIT
_filesize1: .WORD   DUP,LIT,FCBEND,PLUS,FETCH,SWAP,LIT,FCBADDR,PLUS,FETCH
            .WORD   MINUS,LIT,IOROK,EXIT


; ----------------------------------------------------------------------
; INCLUDE-FILE [FILE] 11.6.1.1717 ( i*x fileid -- j*x )
;
; Remove fileid from the stack.  Save the current input source specification,
; including the current value of SOURCE-ID.  Store fileid in SOURCE-ID.  Make
; the file specified by fileid the input source.  Store zero in BLK.  Other
; stack effects are due to the words INCLUDEd.
;
; Repeat until end of file:  read a line from the file, fill the input buffer
; from the contents of that line, set >IN to zero, and interpret.
;
; Text interpretation begins at the file position where the next file read
; would occur.
;
; When the end of the file is reached, close the file and restore the input
; source specification to its saved value.
;
; An ambiguous condition exists if fileid is invalid, if there is an I/O
; exception reading fileid, or if an I/O exception occurs while closing
; fileid.  When an ambiguous condition exists, the status (open or closed)
; of any files that were being interpreted is implementation-defined.
;
; ---
; \ TODO: Store zero in BLK.
; \ TODO? Use SOURCE-ID (FILEID>FCB) instead of storing FCB on the return
; \       stack (INCLUDE nesting could use up a lot of the return stack).
; \ TODO? Have NEXT-LINE return addresses instead of lengths so that we
; \       don't have to do addition here.
; : INCLUDE-FILE  ( i*x fileid -- j*x)
;   DUP FILEID>FCB? ABORT" Invalid fileid"  >R
;   PUSHICB  ICB ICBSOURCEID + !
;   BEGIN  R@ 2@ -  WHILE
;       R@ NEXT-LINE ( len len+CRLF R:fcb)
;       R@ @ ( len len+CRLF pos R:fcb) ROT OVER + ( len+CRLF pos end) ICB 2!
;       R@ +!
;       INTERPRET
;   REPEAT
;   R> FCB>FILEID CLOSE-FILE DROP  POPICB ;

            LINKTO(FILESIZE,0,12,'E',"LIF-EDULCNI")
INCLUDEFILE:JMP     ENTER
            .WORD   DUP,FILEIDTOFCBQ,zbranch,_includefile1
            .WORD   PSQUOTE,14
            .BYTE   "Invalid fileid"
            .WORD   TYPE,ABORT
_includefile1:.WORD TOR,PUSHICB,ICB,LIT,ICBSOURCEID,PLUS,STORE
_includefile2:.WORD RFETCH,TWOFETCH,MINUS,zbranch,_includefile3
            .WORD   RFETCH,NEXTLINE,RFETCH,FETCH,ROT,OVER,PLUS,ICB,TWOSTORE
            .WORD   RFETCH,PLUSSTORE,INTERPRET,branch,_includefile2
_includefile3:.WORD RFROM,FCBTOFILEID,CLOSEFILE,DROP,POPICB,EXIT



; ----------------------------------------------------------------------
; INCLUDED [FILE] 11.6.1.1718 ( i*x c-addr u -- j*x )
;
; Remove c-addr u from the stack.  Save the current input source specification,
; including the current value of SOURCE-ID.  Open the file specified by
; c-addr u, store the resulting fileid in SOURCE-ID, and make it the input
; source.  Store zero in BLK.  Other stack effects are due to the words
; included.
;
; Repeat until end of file:  read a line from the file, fill the input buffer
; from the contents of that line, set >IN to zero, and interpret.
;
; Text interpretation begins at the file position where the next file read
; would occur.
;
; When the end of the file is reached, close the file and restore the input
; source specification to its saved value.
;
; An ambiguous condition exists if the named file can not be opened, if an I/O
; exception occurs reading the file, or if an I/O exception occurs while
; closing the file.  When an ambiguous condition exists, the status (open or
; closed) of any files that were being interpreted is implementation-defined.
;
; ---
; : INCLUDED ( i*x c-addr u -- j*x)
;   R/O OPEN-FILE ABORT" Unknown file" INCLUDE-FILE ;

            LINKTO(INCLUDEFILE,0,8,'D',"EDULCNI")
INCLUDED:   JMP     ENTER
            .WORD   RO,OPENFILE,zbranch,_included1
            .WORD   PSQUOTE,12
            .BYTE   "Unknown file"
            .WORD   TYPE,ABORT
_included1: .WORD   INCLUDEFILE,EXIT


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
;   OVER + R@ FCBEND + !
;   DUP R@ FCBADDR + !  R@ FCBPOS + !
;   R@ FCBGENNUM + DUP C@ 1+ SWAP C!
;   R> FCB>FILEID IOROK ;

            LINKTO(INCLUDED,0,9,'E',"LIF-NEPO")
OPENFILE:   JMP     ENTER
            .WORD   RO,NOTEQUALS,zbranch,_openfile1
            .WORD   TWODROP,ZERO,LIT,IORRDONLY,EXIT
_openfile1: .WORD   FINDFILE,QDUP,zbranch,_openfile2
            .WORD   ZERO,SWAP,EXIT
_openfile2: .WORD   NEWFCB,TOR
            .WORD   OVER,PLUS,RFETCH,LIT,FCBEND,PLUS,STORE
            .WORD   DUP,RFETCH,LIT,FCBADDR,PLUS,STORE
            .WORD       RFETCH,LIT,FCBPOS,PLUS,STORE
            .WORD   RFETCH,LIT,FCBGENNUM,PLUS,DUP,CFETCH,ONEPLUS,SWAP,CSTORE
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


; ----------------------------------------------------------------------
; READ-FILE [FILE] 11.6.1.2080 ( c-addr u1 fileid -- u2 ior )
;
; Read u1 consecutive characters to c-addr from the current position of the
; file identified by fileid.
;
; If u1 characters are read without an exception, ior is zero and u2 is equal
; to u1.
;
; If the end of the file is reached before u1 characters are read, ior is zero
; and u2 is the number of characters actually read.
;
; If the operation is initiated when the value returned by FILE-POSITION is
; equal to the value returned by FILE-SIZE for the file identified by fileid,
; ior is zero and u2 is zero.
;
; If an exception occurs, ior is the implementation-defined I/O result code,
; and u2 is the number of characters transferred to c-addr without an exception.
;
; An ambiguous condition exists if the operation is initiated when the value
; returned by FILE-POSITION is greater than the value returned by FILE-SIZE
; for the file identified by fileid, or if the requested operation attempts
; to read portions of the file not written.
;
; At the conclusion of the operation, FILE-POSITION returns the next file
; position after the last character read.
;
; ---
; \ u1 bytes are copied first, then u2 is determined later.  We might "read"
; \ more bytes than are remaining, but that's not an issue for us given that
; \ all of our files are in memory anyway.
; : READ-FILE ( c-addr u1 fileid -- u2 ior)
;   FILEID>FCB? ?DUP IF NIP NIP 0 SWAP EXIT THEN  ( ca u1 fcb)
;   DUP >R @ ( ca u1 pos R:fcb) -ROT DUP >R MOVE R> ( u1 R:fcb)
;   R@ 2@ - ( u1 rem R:fcb) MIN  DUP R> ( cnt cnt fcb) +!  IOROK ;

            LINKTO(RW,0,9,'E',"LIF-DAER")
READFILE:    JMP    ENTER
            .WORD   FILEIDTOFCBQ,QDUP,zbranch,_readfile1,NIP,NIP,ZERO,SWAP,EXIT
_readfile1: .WORD   DUP,TOR,FETCH,DASHROT,DUP,TOR,MOVE,RFROM
            .WORD   RFETCH,TWOFETCH,MINUS,MIN
            .WORD   DUP,RFROM,PLUSSTORE,LIT,IOROK,EXIT


; ----------------------------------------------------------------------
; READ-LINE [FILE] 11.6.1.2090 ( c-addr u1 fileid -- u2 flag ior )
;
; Read the next line from the file specified by fileid into memory at the
; address c-addr.  At most u1 characters are read.  Up to two implementation-
; defined line-terminating characters may be read into memory at the end of
; the line, but are not included in the count u2.  The line buffer provided
; by c-addr should be at least u1+2 characters long.
;
; If the operation succeeded, flag is true and ior is zero.  If a line
; terminator was received before u1 characters were read, then u2 is the
; number of characters, not including the line terminator, actually read
; (0 <= u2 <= u1).  When u1 = u2, the line terminator has yet to be reached.
;
; If the operation is initiated when the value returned by FILE-POSITION is
; equal to the value returned by FILE-SIZE for the file identified by fileid,
; flag is false, ior is zero, and u2 is zero.  If ior is non-zero, an
; exception occurred during the operation and ior is the implementation-
; defined I/O result code.
;
; An ambiguous condition exists if the operation is initiated when the value
; returned by FILE-POSITION is greater than the value returned by FILE-SIZE
; for the file identified by fileid, or if the requested operation attempts
; to read portions of the file not written.
;
; At the conclusion of the operation, FILE-POSITION returns the next file
; position after the last character read.
;
; ---
; : READ-LINE ( c-addr u1 fileid -- u2 flag ior)
;   FILEID>FCB? ?DUP IF NIP NIP 0 SWAP EXIT THEN  ( ca u1 fcb)
;   DUP 2@ - 0= IF DROP 2DROP 0 0 IOROK EXIT THEN
;   DUP >R @ ( ca u1 pos R:fcb) -ROT COPY-LINE ( u2 cnt R:fcb)
;   R> +! -1 IOROK ;

            LINKTO(READFILE,0,9,'E',"NIL-DAER")
READLINE:    JMP    ENTER
            .WORD   FILEIDTOFCBQ,QDUP,zbranch,_readline1,NIP,NIP,ZERO,SWAP,EXIT
_readline1: .WORD   DUP,TWOFETCH,MINUS,ZEROEQUALS,zbranch,_readline2
            .WORD       DROP,TWODROP,ZERO,ZERO,LIT,IOROK,EXIT
_readline2: .WORD   DUP,TOR,FETCH,DASHROT,COPYLINE
            .WORD   RFROM,PLUSSTORE,LIT,-1,LIT,IOROK,EXIT


; ----------------------------------------------------------------------
; REPOSITION-FILE [FILE] 11.6.1.2142 ( ud fileid -- ior )
;
; Reposition the file identified by fileid to ud.  ior is the implementation-
; defined I/O result code.  An ambiguous condition exists if the file is
; positioned outside the file boundaries.
;
; At the conclusion of the operation, FILE-POSITION returns the value ud.
;
; ---
; : REPOSITION-FILE ( ud fileid -- ior)
;   FILEID>FCB? ?DUP IF DROP EXIT THEN  DUP  FCBADDR + @ ROT +  SWAP !  IOROK ;

            LINKTO(READLINE,0,15,'E',"LIF-NOITISOPER")
REPOSFILE:   JMP    ENTER
            .WORD   FILEIDTOFCBQ,QDUP,zbranch,_reposfile,DROP,EXIT
_reposfile: .WORD   DUP,LIT,FCBADDR,PLUS,FETCH,ROT,PLUS,SWAP,STORE
            .WORD   LIT,IOROK,EXIT


; ----------------------------------------------------------------------
; RESIZE-FILE [FILE] 11.6.1.2147 ( ud fileid -- ior )
;
; Set the size of the file identified by fileid to ud.  ior is the
; implementation-defined I/O result code.
;
; If the resultant file is larger than the file before the operation, the
; portion of the file added as a result of the operation might not have been
; written.
;
; At the conclusion of the operation, FILE-SIZE returns the value ud and
; FILE-POSITION returns an unspecified value.
;
; ---
; Always fails in MFORTH as the file system is read-only.

            LINKTO(REPOSFILE,0,11,'E',"LIF-EZISER")
RESIZEFILE: JMP     ENTER
            .WORD   TWODROP,LIT,IORRDONLY,EXIT


; ----------------------------------------------------------------------
; W/O [FILE] 11.6.1.2425 "w-o" ( -- fam )
;
; fam is the implementation-defined value for selecting the "write only"
; file access method.

            LINKTO(RESIZEFILE,0,3,'O',"/W")
WO:         JMP     ENTER
            .WORD   LIT,00000010b,EXIT


; ----------------------------------------------------------------------
; WRITE-FILE [FILE] 11.6.1.2480 ( c-addr u fileid -- ior )
;
; Write u characters from c-addr to the file identified by fileid starting
; at its current position.  ior is the implementation-defined I/O result code.
;
; At the conclusion of the operation, FILE-POSITION returns the next file
; position after the last character written to the file, and FILE-SIZE returns
; a value greater than or equal to the value returned by FILE-POSITION.
;
; ---
; Always fails in MFORTH as the file system is read-only.

            LINKTO(WO,0,10,'E',"LIF-ETIRW")
WRITEFILE:  JMP     ENTER
            .WORD   DROP,TWODROP,LIT,IORRDONLY,EXIT


; ----------------------------------------------------------------------
; WRITE-LINE [FILE] 11.6.1.2485 ( c-addr u fileid -- ior )
;
; Write u characters from c-addr followed by the implementation-dependent
; line terminator to the file identified by fileid starting at its current
; position.  ior is the implementation-defined I/O result code.
;
; At the conclusion of the operation, FILE-POSITION returns the next file
; position after the last character written to the file, and FILE-SIZE returns
; a value greater than or equal to the value returned by FILE-POSITION.
;
; ---
; Always fails in MFORTH as the file system is read-only.

            LINKTO(WRITEFILE,0,10,'E',"NIL-ETIRW")
WRITELINE:  JMP     ENTER
            .WORD   DROP,TWODROP,LIT,IORRDONLY,EXIT



; ======================================================================
; FILE Constants (implementation details)
; ======================================================================

; ----------------------------------------------------------------------
; File Control Block
;
; Stores information about an open file.  The elements are ordered such
; that 2@ on an FCB will put END and POS on the stack in that order.  You
; can then calculate the remaining bytes in the file (the most common
; operation on an FCB if you assume that READ-* is the most commonly-called
; FILE word) with "2@ -" and without any indexing into the FCB.  This is
; also the reason that we store the end address of the file instead of the
; length of the file; we only need the length for FILE-SIZE, but we need
; the end address for every READ-* (to ensure that we do not read past the
; end of the file).  Finally, storing POS first means that you can access
; the absolute file position without having to adjust the FCB address.  In
; other words, an FCB address is also the address of the POS cell for that
; FCB.

FCBPOS:     .EQU    0           ; Offset from FCB to position in file.
FCBEND:     .EQU    2           ; Offset to end address of file.
FCBADDR:    .EQU    4           ; Offset to address of file.
FCBGENNUM:  .EQU    6           ; Offset to generation number.


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

            LINKTO(WRITELINE,0,10,'D',"IELIF>BCF")
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
;   DUP 8 RSHIFT  SWAP 255 AND ( gen fcboff)
;   DUP 0=  OVER [ MAXFCBS 2* 2* 2* ] >  OR IF 2DROP IORBADFILEID EXIT THEN
;   1- FCBSTART +  DUP FCBGENNUM + C@ ROT <> IF DROP IORBADFILEID EXIT THEN
;   DUP FCBADDR + @ 0= IF DROP IORBADFILEID EXIT THEN
;   IOROK ;

            LINKTO(FCBTOFILEID,0,11,'?',"BCF>DIELIF")
FILEIDTOFCBQ:JMP    ENTER
            .WORD   DUP,LIT,8,RSHIFT,SWAP,LIT,255,AND
            .WORD   DUP,ZEROEQUALS,OVER,LIT,MAXFCBS*8,GREATERTHAN
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
;   TUCK  FILNAME SWAP MOVE
;   6 SWAP ( 6 u) ?DO BL FILNAME I + C! LOOP
;   FILNAME 6 +  [CHAR] D OVER C!  [CHAR] O SWAP 1+ C!
;   SRCNAM ;

            LINKTO(FILEIDTOFCBQ,0,9,'E',"LIF-DNIF")
FINDFILE:   JMP     ENTER
            .WORD   DUP,LIT,6,GREATERTHAN,zbranch,_findfile1
            .WORD   TWODROP,LIT,IORFNF,EXIT
_findfile1: .WORD   TUCK,LIT,0FC93h,SWAP,MOVE
            .WORD   LIT,6,SWAP,pqdo,_findfile3
_findfile2: .WORD   BL,LIT,0FC93h,I,PLUS,CSTORE,ploop,_findfile2
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
; NEXT-LINE [MFORTH] ( fcb -- u1 u2 )
;
; Read forward from the current position in the file identified by fcb
; until either a CRLF sequence is found or the end of file is reached.
; u1 is the number of bytes in the current line of the file (ignoring the
; CRLF sequence, if any).  u2 is the number of bytes actually read, which
; will normally be u1+2 if the line was terminated with a CRLF.
;
; ---
; \ The loop in this method takes advantage of the fact that all M100 files
; \ end with an EOF and so we can always read the next two characters in the
; \ file, even if this is the last character in the file.  We will never read
; \ a byte from another file in this situation, because the second character
; \ will be EOF.
; : NEXT-LINE ( fcb -- u1 u2)
;   2@ TUCK - 2>B 0 FORB B @ 0x0A0D = IF DUP 1+ 1+ EXIT THEN 1+ NEXTB DUP ;

            LINKTO(NEWFCB,0,9,'E',"NIL-TXEN")
NEXTLINE:   JMP     ENTER
            .WORD   TWOFETCH,TUCK,MINUS,TWOTOB,ZERO
_nextline1: .WORD   BQUES,zbranch,_nextline3
            .WORD   B,FETCH,LIT,0A0Dh,EQUALS,zbranch,_nextline2
            .WORD   DUP,ONEPLUS,ONEPLUS,EXIT
_nextline2: .WORD   ONEPLUS,BPLUS,branch,_nextline1
_nextline3: .WORD   DUP,EXIT


; ----------------------------------------------------------------------
; SRCNAM [MFORTH] ( -- ior | file-addr file-len 0 )
;
; Call the Main ROM's SRCNAM routine.  FILNAM has already been populated
; by the caller.

            LINKTO(NEXTLINE,0,6,'M',"ANCRS")
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
