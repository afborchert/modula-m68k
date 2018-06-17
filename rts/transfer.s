#------------------------------------------------------------------------------
# Modula-2 runtime library -- NEWPROCESS and TRANSFER
# (c) University of Ulm, Sektion Informatik, D-7900 Ulm
# afb 3/88 on SUN hr 2/89
#------------------------------------------------------------------------------
	.text
#	.stabs	"transfer.s",0144,0,0,L$0
#L$0:
	.globl	transfer_
transfer_:
	.lcomm	G_AREA,0
	.lcomm	_FLAG,4
	transfer_V0_  =  G_AREA
	_transfer_V0_  =  G_AREA
	.globl	transfer_V0_
	.globl	_transfer_V0_
#------------------------------------------------------------------------------
#   TYPE
#      PROCESS = POINTER TO ProcessDesc;
#      ProcessDesc =
#	  RECORD
#	     limit, base, top: ADDRESS;
#	     p: PROC;
#	     started: BOOLEAN; (* 4 bytes *)
#	     sigmask: BITSET;  (* [32..1] *)
#	  END;
#
#   PROCEDURE NEWPROCESS(P: PROC;              (* 20(%fp) *)
#                        A: ADDRESS;           (* 16(%fp) -> %a0 *)
#                        n: CARDINAL;          (* 12(%fp) -> %d2 *)
#                        VAR new: PROCESS);    (*  8(%fp) *)
#------------------------------------------------------------------------------
	pdsize  =  24			| SIZE(ProcessDesc)
LP$1:
#	.stabs	"transfer_P1_",044,0,0,transfer_P1_
transfer_P1_:
_transfer_P1_:
	.globl	transfer_P1_
	.globl	_transfer_P1_
_newproc:
	.globl	_newproc
	linkw	a6,#0
#	.stabn	0244,0,0,b$1
#b$1:
#
# enough memory for allocating the process descriptor?
#
	movl	a6@(12),d2
	cmpl	#pdsize,d2
	blel	.stack			| run time error
#
# fill and allocate process descriptor
#
	movl	a6@(16),a0
	movl	a0,a1
	addl	d2,a1			| now %a1 -> stack top
	movl	#0,a1@-			| new^.sigmask := {}
	movl	#0,a1@-			| new^.started := FALSE
	movl	a6@(20),a1@-		| new^.p := P;
	movl	a0,a1@-			| new^.top := A;
	addl	d2,a1@			| INC(new^.top, n);
	movl	#0,a1@-			| new^.base := 0
	movl	a0,a1@-			| new^.limit := A;
#
# return address of process descriptor
#
	movl	a1,a6@(8:l)@
#
	.stabn	0244,0,1,e$1
e$1:
	unlk	a6
	rtd	#16
#------------------------------------------------------------------------------
#   PROCEDURE TRANSFER(VAR src,        (* 12(%fp) *)
#                      dest: ADDRESS); (*  8(%fp) *)
#------------------------------------------------------------------------------
LP$2:
	.stabs	"tranfer_P2_",044,0,0,transfer_P2_
transfer_P2_:
_transfer_P2_:
	.globl	transfer_P2_
	.globl	_transfer_P2_
_transfer:
	.globl	_transfer
	linkw	a6,#0
	.stabn	0244,0,0,b$2
b$2:
#
# save ADR(dest^) in %a0 because src and dest may be the same variable
#
	movl	a6@(8:l)@,a0
#
# get sigmask
#
	movl	#0,sp@-
	subl	#4,sp
	movl	#0x6d,sp@-
	trap	#0			| sigblock(0)
	addl	#8,sp			| current mask in d0
#
# save old process context
#
	movl	d0,sp@-			| sigmask := sigblock(0)
	movl	#1,sp@-			| started := TRUE
	movl	a6@(4),sp@-		| p
	movl	a6,sp@-			| top
 	movl	a6@(0),sp@-		| base
	movl	a5,sp@-			| limit
	movl	sp,a6@(12:l)@

# context switch
#
	moveml	a0@(0),#0xe000		| get limit, base, and top
#
# restore sigmask
#
	subl	#pdsize,sp
	movl	a0@(20),sp@-
	subl	#4,sp
	movl	#0x6e,sp@-		| sigsetmask(sigmask)
	trap	#0
	addl	#8+pdsize,sp
#
	tstl	a0@(16)			| coroutine already started?
	beq	newcr
#
# continue coroutine
#
	addl	#16,sp			| deallocate link and parameters
	jmp	a0@(12:l)@
#
# start new coroutine
#
newcr:
	movl	a0@(12),a5@+		| save procedure address
	movl	#0,a5@+			| push priority 0
	jsr	a0@(12:l)@
#
# a coroutine must not return
#
	subl	#4,a5			| pop priority 0
	movl	a5@-,sp@-		| push procedure address
	jmp	.crend			| now it looks like jsr
	.stabn	0244,0,1,e$2
e$2:
#
#
#
#
LP$0:
	.stabs	"transfer_P0_",044,0,0,transfer_P0_
transfer_P0_:
_transfer_P0_:
	.globl	transfer_P0_
	.globl	_transfer_P0_
	linkw	a6,#0
	.stabn	0244,0,0,b$0
b$0:
	.stabn	0244,0,1,e$0
e$0:
	unlk	a6
	rts
