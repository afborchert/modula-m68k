#-------------------------------------------------------------------------
# Modula-2 runtime library -- signal handling
# (c) University of Ulm, Sektio Informatik, D-7900 Ulm
# afb 3/88
# on SUN hr 2/89
# rev afb 12/90: SV_INTERRUPT
#-------------------------------------------------------------------------
	.stabs	"signal.s",0144,0,0,L$0
L$0:
	nsigs	=	0x20
	.text
	.globl	signal_
	.globl	_signal_
signal_:
_signal_:
	.lcomm	G_AREA,nsigs*4 + 4
	.lcomm	_FLAG,4
	signal_V0_=	G_AREA
	_signal_V0_=	G_AREA
	.globl	signal_V0_
	.globl	_signal_V0_
	sigtab	=	G_AREA
#	.globl	sigtab
#-------------------------------------------------------------------------
#       
# PROCEDURE Signal(sig : CARDINAL;		(* a6@(20) -> d2 *)
#		   p : PROC;			(* a6@(16) -> d3 *)
#		   VAR old: PROC;		(* a6@(12) <- d4 *)
#		   VAR error : CARDINAL)	(* a6@( 8) <- d0 *)
#		   :BOOLEAN;			(*     d0  <- d5 *)
#
#-------------------------------------------------------------------------
	.stabs	"signal_P1_",044,0,0,_signal_P1_
	.globl	_signal
_signal:
signal_P1_:
_signal_P1_:
	.globl 	signal_P1_
	.globl 	_signal_P1_
	linkw	a6,#-24
	.stabn	0244,0,0,b$1
b$1:
# check given signal number
	movl	a6@(20),d2
	ble	badsig1
# store function adress in sigtab[]
	movl 	@(sigtab,d2:l:4),d4	| get old function address
	movl	a6@(16),d3
	movl	d3,@(sigtab,d2:l:4)	| store new function address
#check for SIG_IGN and SIG_DFL
	beq	doit			| function address=0: SIG_DFL
	btst	#0,d3			| odd address: SIG_IGN
	bne	doit
	movl	#_sigtramp,d3
	movl	d3,a6@(-24)		| vec->sv_handler = _sigtramp
	movl	#0,a6@(-20)		| vec->sv_mask = 0
	movl	#2,a6@(-16)		| vec->sv_flags = SV_INTERRUPT
doit:
	pea	a6@(-12)		| address of ovec
	pea	a6@(-24)		| address of vec (see sigvec)
	movl	d2,sp@-			| signal number on stack
	movl	#0,sp@-
	pea	0x6c			| system call sigvec
	trap	#0
# return from system call
	scc	d5
	negb	d5
	beq	fail
	cmpl	#_sigtramp,a6@(-12)	| ovec->sv_handler == _sigtramp?
	beq	setold
	movl	a6@(-12),d4		| return value of system call
setold:
	movl	d4,a6@(12)@(0)
	bra	return
#
# bad signal number
#
badsig1:
	movl	#22,a6@(8)@(0)		| EINVAL	
	movl	#0,d5			| return FALSE
#
# restore old function address in sigtab[] on failure
fail:
	movl	d4,@(sigtab,d2:l:4)
	movl	d0,a6@(8)@(0)		| error number
#
# return
#
return:
	movl	d5,d0
	.stabn	0244,0,1,e$1
e$1:
	unlk	a6
	rtd	#16
#
#
#--------------------------------------------------------------------------
#
# general signal catching routine
#
LP$2:
	.stabs	"_signal_P2",044,0,0,_signal_P2_
signal_P2_:
_signal_P2_:
	.globl	signal_P2_
	.globl	_signal_P2_
_sigtramp:
	linkw	a6,#-64		| link and allocate memory for register saving
	.stabn	0244,0,0,b$2
b$2:
	moveml	#0xffff,a6@(-64)
# signal number should be in [1..nsigs]
	movl	a6@(4),d0
	ble	badsig2
	cmpl	#nsigs,d0
	bgt	badsig2
# check for priority, call isr only if interrupt priority > current priority
#valid limit (priority stack)?
	tstl	a5			| valid limit (priority stack)?
	beq	callfunc
	.globl	_PrioTab_V0
	movl	@(_PrioTab_V0:l,d0:l:4),d1	| prio of signal
#compare with priority level
	cmpl	a5@(-4),d1	
	bgt	callfunc
# system call with
	linkw	a6,#-24
	movl	#_sigtramp,a6@(-24)
	pea	a6@(-12)
	pea	a6@(-24)
	movl	d0,sp@-
	movl	#0,sp@-
	pea	0x6c:w			| system call sigvec
	trap	#0
	unlk	a6
	bra	ignore
# call Modula-2 signal catching procedure
callfunc:
	addl	#1,a6			| indication for mdb
	jsr	@(sigtab,d0:l:4)@
	subl	#1,a6
ignore:
badsig2:
	moveml	a6@(-64),#0xffff
	.stabn	0244,0,1,e$2
e$2:
	unlk	a6
# return from interrupt
	addl	#8,sp
	pea	0x8b:w
	trap	#0
LP$0:
signal_P0_:
_signal_P0_:
	.globl	signal_P0_
	.globl	_signal_P0_
	linkw	a6,#I$3
	tas	_FLAG
	bne	B$2
	.globl	_PrioTab_P0
	jsr	_PrioTab_P0			| initialize PrioTab
B$2:
	I$3	= 0
	unlk	a6
	rts
	.even
