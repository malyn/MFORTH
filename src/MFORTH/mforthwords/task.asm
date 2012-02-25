; Copyright (c) 2009-2011, Michael Alyn Miller <malyn@strangeGizmo.com>.
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
; TASK Words
; ======================================================================

; ----------------------------------------------------------------------
; PAUSE [MFORTH] ( -- )
;
; Suspend the current task and resume execution of the next task in the
; task list.  PAUSE will return to the caller when all of the tasks in
; the task list have had a chance to execute (and called PAUSE in order
; to relinquish execution to their next task).

            LINKTO(LINK_TASK,0,5,'E',"SUAP")
PAUSE:      ; Suspend the current task.
            PUSH    B           ; Push the return stack pointer.
            PUSH    D           ; Push the instruction pointer.
            LXI     H,0         ; Clear HL
            DAD     SP          ; ..and get SP in HL.
            MVI     E,USERSAVEDSP;Put SAVEDSP variable offset into E
            MOV     D,B         ; ..and put the Task Page into D.
            SHLX                ; Save the SP in SAVEDSP.

            ; Select the next task (which could be this task).
            LHLD    TICKFIRSTTASK;Get the address of the first task,
            MOV     A,H         ; ..move the page address into A,
            LHLD    TICKNUMTASKS; ..get the number of tasks,
            SUB     L           ; ..and calc the page after the last task.

            DCR     B           ; Point B at the presumed next task,
            CMP     B           ; ..and see if we have gone too far;
            JNZ     _pause1     ; ..resume that task if not,
            LHLD    TICKFIRSTTASK;..otherwise get the first task
            MOV     B,H         ; ..and resume that task.

            ; Resume the next task.
_pause1:    MVI     E,USERSAVEDSP;Get SAVEDSP variable offset into E
            MOV     D,B         ; ..and put the Task Page into D.
            LHLX                ; Get the saved SP from SAVEDSP
            SPHL                ; ..and restore SP.
            MVI     H,STACKGUARD; Put the stack guard into H
            MVI     L,STACKGUARD; ..and L,
            SHLX                ; ..and then save the guard to SAVEDSP.
            POP     D           ; Pop the instruction pointer.
            POP     B           ; Pop the return stack pointer.

            NEXT


; ----------------------------------------------------------------------
; TASK [MFORTH] ( xt -- )
;
; Create a new task and prepare the task to execute xt when it is first
; resumed.  xt should never return, but if it does then the task will be
; put into an infinite PAUSE loop in order to prevent the system from
; freezing.
;
; ---
; : TASK ( xt -- )
;   'FIRSTTASK @ 'NUMTASKS @ 8 LSHIFT - ( xt a-newtaskpage)  1 'NUMTASKS +!
;   [HEX] 7676       OVER [HEX] 80 OR  !     \ Initialize stack guard.
;   10               OVER USERBASE OR  !     \ Initialize BASE.
;   DUP [HEX] FA OR  OVER USERSAVEDSP OR  !  \ Initialize SAVEDSP.
;   SWAP             OVER [HEX] FE OR  !     \ Push xt to the stack.
;   DUP [HEX] 7F OR  OVER [HEX] FC OR  !     \ Push RSP to the stack.
;   ['] STOPPED CFASZ +  OVER [HEX] FA OR  ! \ Push initial IP to the stack.
;   DROP ;

            LINKTO(PAUSE,0,4,'K',"SAT")
TASK:       JMP     ENTER
            .WORD   LIT,TICKFIRSTTASK,FETCH,LIT,TICKNUMTASKS,FETCH
            .WORD       LIT,8,LSHIFT,MINUS
            .WORD   ONE,LIT,TICKNUMTASKS,PLUSSTORE
            .WORD   LIT,07676h,OVER,LIT,080h,OR,STORE
            .WORD   LIT,10,OVER,LIT,USERBASE,OR,STORE
            .WORD   DUP,LIT,0FAh,OR,OVER,LIT,USERSAVEDSP,OR,STORE
            .WORD   SWAP,OVER,LIT,0FEh,OR,STORE
            .WORD   DUP,LIT,07Fh,OR,OVER,LIT,0FCh,OR,STORE
            .WORD   LIT,STOPPED,LIT,CFASZ,PLUS,OVER,LIT,0FAh,OR,STORE
            .WORD   DROP
            .WORD   EXIT


; ----------------------------------------------------------------------
; TASK-PAGE [MFORTH] ( -- a-addr )
;
; a-addr is the base address of the Task Page for the current task.

            LINKTO(TASK,0,9,'E',"GAP-KSAT")
TASKPAGE:   MOV     H,B
            MVI     L,0
            PUSH    H
            NEXT



; ======================================================================
; TASK Words (implementation details)
; ======================================================================

; ----------------------------------------------------------------------
; STOPPED [MFORTH] ( xt -- )
;
; This word never returns, but instead executes xt and then calls PAUSE
; in an infinite loop if/when that xt returns.  The word is named
; "STOPPED" because the only time it will be the running word is if xt
; returns.
;
; ---
; : STOPPED ( xt -- )   EXECUTE  BEGIN PAUSE AGAIN ;

            LINKTO(TASKPAGE,0,7,'D',"EPPOTS")
LAST_TASK:
STOPPED:    JMP     ENTER
            .WORD   EXECUTE
_stopped1:  .WORD   PAUSE,branch,_stopped1
