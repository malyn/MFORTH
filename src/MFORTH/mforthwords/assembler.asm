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
; Assembler Words
; ======================================================================

; ----------------------------------------------------------------------
; END-CODE [MFORTH] ( -- )
;
; End the current code definition.  If the data-space pointer is not
; aligned, reserve enough data space to align it.  Restore the previous
; search order.
;
; ---
; : END-CODE ( -- )   PREVIOUS ;

            LINKTO(LINK_ASSEMBLER,0,8,'E',"DOC-DNE")
ENDCODE:    JMP     ENTER
            .WORD   PREVIOUS
            .WORD   EXIT


; ----------------------------------------------------------------------
; NEXT [MFORTH]
;
; Compile the code for NEXT into the current code definition.

            LINKTO(ENDCODE,0,4,'T',"XEN")
asmNEXT:    JMP     ENTER
#IFNDEF PROFILER
            .WORD   asmLHLX
            .WORD   asmOpD,asmINX
            .WORD   asmOpD,asmINX
            .WORD   asmPCHL
#ELSE
            .WORD   LIT,PROFILENEXT,asmJMP
#ENDIF
            .WORD   EXIT


; ----------------------------------------------------------------------
; RESTOREREGS [MFORTH]
;
; Restore BC and DE (corrupts HL).

            LINKTO(asmNEXT,0,11,'S',"GEREROTSER")
asmRESTOREREGS:JMP  ENTER
            .WORD   LIT,SAVEB,asmLHLD
            .WORD   asmOpH,asmOpB,asmMOV
            .WORD   asmOpL,asmOpC,asmMOV
            .WORD   LIT,SAVED,asmLHLD
            .WORD   asmXCHG
            .WORD   EXIT


; ----------------------------------------------------------------------
; SAVEREGS [MFORTH]
;
; Save BC and DE (corrupts HL).

            LINKTO(asmRESTOREREGS,0,8,'S',"GEREVAS")
asmSAVEREGS:JMP     ENTER
            .WORD   asmOpB,asmOpH,asmMOV
            .WORD   asmOpC,asmOpL,asmMOV
            .WORD   LIT,SAVEB,asmSHLD
            .WORD   asmXCHG
            .WORD   LIT,SAVED,asmSHLD
            .WORD   EXIT



; ======================================================================
; Control Flow Words
; ======================================================================

            LINKTO(asmSAVEREGS,0,2,'=',"0")
asmZEROEQUALS:JMP   ENTER
            .WORD   LIT,$C2,EXIT

            LINKTO(asmZEROEQUALS,0,2,'>',"0")
asmZEROGREATER:JMP  ENTER
            .WORD   LIT,$FA,EXIT

            LINKTO(asmZEROGREATER,0,2,'<',"0")
asmZEROLESS:JMP     ENTER
            .WORD   LIT,$F2,EXIT

            LINKTO(asmZEROLESS,0,3,'>',"<0")
asmZERONOTEQUALS:JMP ENTER
            .WORD   LIT,$CA,EXIT

            LINKTO(asmZERONOTEQUALS,0,2,'C',"C")
asmCC:      JMP     ENTER
            .WORD   LIT,$DA,EXIT

            LINKTO(asmCC,0,2,'S',"C")
asmCS:      JMP     ENTER
            .WORD   LIT,$D2,EXIT

            LINKTO(asmCS,0,5,'N',"IGEB")
asmBEGIN:   JMP     ENTER
            .WORD   HERE,EXIT

            LINKTO(asmBEGIN,0,4,'E',"SLE")
asmELSE:    JMP     ENTER
            .WORD   LIT,$C3,asmIF,SWAP,asmTHEN,EXIT

            LINKTO(asmELSE,0,2,'F',"I")
asmIF:      JMP     ENTER
            .WORD   CCOMMA,HERE,ZERO,COMMA,EXIT

            LINKTO(asmIF,0,4,'N',"EHT")
asmTHEN:    JMP     ENTER
            .WORD   HERE,SWAP,STORE,EXIT

            LINKTO(asmTHEN,0,6,'T',"AEPER")
asmREPEAT:  JMP     ENTER
            .WORD   SWAP,LIT,$C3,CCOMMA,COMMA,asmTHEN,EXIT

            LINKTO(asmREPEAT,0,5,'L',"ITNU")
asmUNTIL:   JMP     ENTER
            .WORD   CCOMMA,COMMA,EXIT

            LINKTO(asmUNTIL,0,5,'E',"LIHW")
asmWHILE:   JMP     ENTER
            .WORD   asmIF,EXIT


; ======================================================================
; 8085 Assembly Instructions
; ======================================================================

; ----------------------------------------------------------------------
; Operands
;

#DEFINE     ASM_OP(value) LXI H,value\ PUSH H\ NEXT

            LINKTO(asmWHILE,0,1,'A',"")
asmOpA:     ASM_OP(7)

            LINKTO(asmOpA,0,1,'B',"")
asmOpB:     ASM_OP(0)

            LINKTO(asmOpB,0,1,'C',"")
asmOpC:     ASM_OP(1)

            LINKTO(asmOpC,0,1,'D',"")
asmOpD:     ASM_OP(2)

            LINKTO(asmOpD,0,1,'E',"")
asmOpE:     ASM_OP(3)

            LINKTO(asmOpE,0,1,'H',"")
asmOpH:     ASM_OP(4)

            LINKTO(asmOpH,0,1,'L',"")
asmOpL:     ASM_OP(5)

            LINKTO(asmOpL,0,1,'M',"")
asmOpM:     ASM_OP(6)

            LINKTO(asmOpM,0,3,'W',"SP")
asmOpPSW:   ASM_OP(6)

            LINKTO(asmOpPSW,0,2,'P',"S")
asmOpSP:    ASM_OP(6)


; ----------------------------------------------------------------------
; Zero-operand instructions
;

#DEFINE     ASM_ZERO_OP(opcode) JMP ENTER\ .WORD LIT,opcode,CCOMMA,EXIT

            LINKTO(asmOpSP,0,4,'R',"HSA")
asmASHR:    ASM_ZERO_OP($10)

            LINKTO(asmASHR,0,3,'A',"MC")
asmCMA:     ASM_ZERO_OP($2F)

            LINKTO(asmCMA,0,3,'C',"MC")
asmCMC:     ASM_ZERO_OP($3F)

            LINKTO(asmCMC,0,3,'A',"AD")
asmDAA:     ASM_ZERO_OP($27)

            LINKTO(asmDAA,0,2,'I',"D")
asmDI:      ASM_ZERO_OP($F3)

            LINKTO(asmDI,0,4,'B',"USD")
asmDSUB:    ASM_ZERO_OP($08)

            LINKTO(asmDSUB,0,2,'I',"E")
asmEI:      ASM_ZERO_OP($FB)

            LINKTO(asmEI,0,3,'T',"LH")
asmHLT:     ASM_ZERO_OP($76)

            LINKTO(asmHLT,0,4,'X',"LHL")
asmLHLX:    ASM_ZERO_OP($ED)

            LINKTO(asmLHLX,0,3,'P',"ON")
asmNOP:     ASM_ZERO_OP($00)

            LINKTO(asmNOP,0,4,'L',"HCP")
asmPCHL:    ASM_ZERO_OP($E9)

            LINKTO(asmPCHL,0,3,'L',"AR")
asmRAL:     ASM_ZERO_OP($17)

            LINKTO(asmRAL,0,3,'R',"AR")
asmRAR:     ASM_ZERO_OP($1F)

            LINKTO(asmRAR,0,4,'L',"EDR")
asmRDEL:    ASM_ZERO_OP($18)

            LINKTO(asmRDEL,0,3,'T',"ER")
asmRET:     ASM_ZERO_OP($C9)

            LINKTO(asmRET,0,3,'M',"IR")
asmRIM:     ASM_ZERO_OP($20)

            LINKTO(asmRIM,0,3,'C',"LR")
asmRLC:     ASM_ZERO_OP($07)

            LINKTO(asmRLC,0,3,'C',"RR")
asmRRC:     ASM_ZERO_OP($0F)

            LINKTO(asmRRC,0,4,'X',"LHS")
asmSHLX:    ASM_ZERO_OP($D9)

            LINKTO(asmSHLX,0,3,'M',"IS")
asmSIM:     ASM_ZERO_OP($30)

            LINKTO(asmSIM,0,4,'L',"HPS")
asmSPHL:    ASM_ZERO_OP($F9)

            LINKTO(asmSPHL,0,3,'C',"TS")
asmSTC:     ASM_ZERO_OP($37)

            LINKTO(asmSTC,0,4,'G',"HCX")
asmXCHG:    ASM_ZERO_OP($EB)

            LINKTO(asmXCHG,0,4,'L',"HTX")
asmXTHL:    ASM_ZERO_OP($E3)


; ----------------------------------------------------------------------
; Register instructions
;

#DEFINE     ASM_REG_OP(opcode) JMP ENTER\ .WORD LIT,opcode,PLUS,CCOMMA,EXIT

            LINKTO(asmXTHL,0,3,'C',"DA")
asmADC:     ASM_REG_OP($88)

            LINKTO(asmADC,0,3,'D',"DA")
asmADD:     ASM_REG_OP($80)

            LINKTO(asmADD,0,3,'A',"NA")
asmANA:     ASM_REG_OP($A0)

            LINKTO(asmANA,0,3,'P',"MC")
asmCMP:     ASM_REG_OP($B8)

            LINKTO(asmCMP,0,3,'R',"DC")
asmDCR:     JMP     ENTER
            .WORD   EIGHTSTAR,LIT,$05,PLUS,CCOMMA,EXIT

            LINKTO(asmDCR,0,3,'R',"NI")
asmINR:     JMP     ENTER
            .WORD   EIGHTSTAR,LIT,$04,PLUS,CCOMMA,EXIT

            LINKTO(asmINR,0,3,'A',"RO")
asmORA:     ASM_REG_OP($B0)

            LINKTO(asmORA,0,3,'B',"BS")
asmSBB:     ASM_REG_OP($98)

            LINKTO(asmSBB,0,3,'B',"US")
asmSUB:     ASM_REG_OP($90)

            LINKTO(asmSUB,0,3,'A',"RX")
asmXRA:     ASM_REG_OP($A8)


; ----------------------------------------------------------------------
; Register pair instructions
;

#DEFINE     ASM_REGPAIR_OP(opcode) JMP ENTER\ .WORD EIGHTSTAR,LIT,opcode,PLUS,CCOMMA,EXIT

            LINKTO(asmXRA,0,3,'D',"AD")
asmDAD:     ASM_REGPAIR_OP($09)

            LINKTO(asmDAD,0,3,'X',"CD")
asmDCX:     ASM_REGPAIR_OP($0B)

            LINKTO(asmDCX,0,3,'X',"NI")
asmINX:     ASM_REGPAIR_OP($03)

            LINKTO(asmINX,0,4,'X',"ADL")
asmLDAX:    ASM_REGPAIR_OP($0A)

            LINKTO(asmLDAX,0,3,'P',"OP")
asmPOP:     ASM_REGPAIR_OP($C1)

            LINKTO(asmPOP,0,4,'H',"SUP")
asmPUSH:    ASM_REGPAIR_OP($C5)

            LINKTO(asmPUSH,0,4,'X',"ATS")
asmSTAX:    ASM_REGPAIR_OP($02)


; ----------------------------------------------------------------------
; Byte operand instructions
;

#DEFINE     ASM_BYTE_OP(opcode) JMP ENTER\ .WORD LIT,opcode,CCOMMA,CCOMMA,EXIT

            LINKTO(asmSTAX,0,3,'I',"CA")
asmACI:     ASM_BYTE_OP($CE)

            LINKTO(asmACI,0,3,'I',"DA")
asmADI:     ASM_BYTE_OP($C6)

            LINKTO(asmADI,0,3,'I',"NA")
asmANI:     ASM_BYTE_OP($E6)

            LINKTO(asmANI,0,3,'I',"PC")
asmCPI:     ASM_BYTE_OP($FE)

            LINKTO(asmCPI,0,2,'N',"I")
asmIN:      ASM_BYTE_OP($DB)

            LINKTO(asmIN,0,3,'I',"RO")
asmORI:     ASM_BYTE_OP($F6)

            LINKTO(asmORI,0,3,'T',"UO")
asmOUT:     ASM_BYTE_OP($D3)

            LINKTO(asmOUT,0,3,'T',"SR")
asmRST:     JMP     ENTER
            .WORD   EIGHTSTAR,LIT,$C7,PLUS,CCOMMA,EXIT

            LINKTO(asmRST,0,3,'I',"BS")
asmSBI:     ASM_BYTE_OP($DE)

            LINKTO(asmSBI,0,3,'I',"US")
asmSUI:     ASM_BYTE_OP($D6)

            LINKTO(asmSUI,0,3,'I',"RX")
asmXRI:     ASM_BYTE_OP($EE)


; ----------------------------------------------------------------------
; Word operand instructions
;

#DEFINE     ASM_WORD_OP(opcode) JMP ENTER\ .WORD LIT,opcode,CCOMMA,COMMA,EXIT

            LINKTO(asmXRI,0,4,'L',"LAC")
asmCALL:    ASM_WORD_OP($CD)

            LINKTO(asmCALL,0,3,'P',"MJ")
asmJMP:     ASM_WORD_OP($C3)

            LINKTO(asmJMP,0,3,'A',"DL")
asmLDA:     ASM_WORD_OP($3A)

            LINKTO(asmLDA,0,4,'D',"LHL")
asmLHLD:    ASM_WORD_OP($2A)

            LINKTO(asmLHLD,0,4,'D',"LHS")
asmSHLD:    ASM_WORD_OP($22)

            LINKTO(asmSHLD,0,3,'A',"TS")
asmSTA:     ASM_WORD_OP($32)


; ----------------------------------------------------------------------
; Move and Load Immediate instructions
;

            LINKTO(asmSTA,0,3,'I',"XL")
asmLXI:     JMP     ENTER
            .WORD   EIGHTSTAR,ONEPLUS,CCOMMA,COMMA,EXIT

            LINKTO(asmLXI,0,3,'V',"OM")
asmMOV:     JMP     ENTER
            .WORD   EIGHTSTAR,LIT,$40,PLUS,PLUS,CCOMMA,EXIT

            LINKTO(asmMOV,0,3,'I',"VM")
LAST_ASSEMBLER:
asmMVI:     JMP     ENTER
            .WORD   EIGHTSTAR,LIT,$06,PLUS,CCOMMA,CCOMMA,EXIT
