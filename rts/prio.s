#-------------------------------------------------------------------------
# Modula-2 runtime library -- enter and exit priority
# (c) University of Ulm , Sektion Informatik, D-7900 Ulm
# afb 4/88 on SUN hr 2/89
#-------------------------------------------------------------------------
	.text
	.globl  .entprio	| enter priority
	.globl  _entprio	| enter priority
.entprio:			| priority on stack sp@(4)
_entprio:			| priority on stack sp@(4)
	tstl	a5
	beq	.prio		| cannot push priority (wrong runtime start)
	movl	sp@(4),d0	| get priority
	cmpl	a5@(-4),d0
	bltl	.prio		| priority error
	cmpl	a5,sp
	blsl	.stack		|stack overflow check
	movl	d0,a5@+		|push priority
	rtd	#4
#-------------------------------------------------------------------------
	.globl  .exprio
	.globl  _exprio
.exprio:
_exprio:
	subl	#4,a5
	rts

