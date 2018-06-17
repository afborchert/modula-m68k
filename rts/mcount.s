#------------------------------------------------------------------------------
# Modula-2 runtime library -- mcount: count subroutine calls during profiling
# (c) University of Ulm, Sektion Informatik, D-7900 Ulm
# afb 4/88 on SUN hr 2/89
#------------------------------------------------------------------------------
	.text
	.stabs	"mcount.s",0144,0,0,L$0
L$0:
	.globl	_mcount			| looks like a Modula-2 module
mcount:
	.lcomm	G_AREA,0
	.lcomm	_FLAG,4
	_mcount_V0 = G_AREA
	.globl	_mcount_V0
	.even
#------------------------------------------------------------------------------
# called at function entry (after the save sequence) by
#	mov.l	#LABEL,sp@-
#	jsr	rf,_mcount
# where LABEL is a word in the bss segment pointing to the function's
# entry in the count buffer
# each count buffer entry is two words: a pointer to the function entry
# point (actually pointing after the 'jsr' to mcount, and initialized
# at the first call of mcount), and the count of calls
#
# ref to libc/crt/mcount.s	Unix Edition VII
#------------------------------------------------------------------------------
_mcount:
	.stabs	"_mcount_P1",044,0,0,_mcount_P1
_mcount_P1:
	.globl	_mcount_P1
	.globl	_mcount
	.stabn	0244,0,0,b$1
b$1:
#------------------------------------------------------------------------------
	movl	sp@(4),a0	| a0: pointer to pointer to function entry
	movl	a0@,a1		| get pointer to function entry
	movl	a1,d0		| set condition codes
	bne	count		| skip if already initialized
	movl	.countb,a1	| a1: next function entry
	movl	a1,d0		| set condition codes
	beq	return		| no buffer - give up
	movl	a1,d0
	addl	#8,d0		| allocate buffer entry for this func
	movl	d0,.countb
	movl	sp@(0),a1@(0)	| store pointer to function
	addl	#4,a1		| a1: pointer to #calls
	movl	a1,a0@		| set forward pointer to #calls entry
count:
	addl	#1,a1@
#------------------------------------------------------------------------------
	.stabn	0244,0,1,e$1
e$1:
return:
	rtd	#4
	.even
#------------------------------------------------------------------------------
	.stabs	"_mcount_P0",044,0,0,_mcount_P0
_mcount_P0:
	.globl	_mcount_P0
	linkl	a6,#0
	.stabn	0244,0,0,b$0
b$0:
	.stabn	0244,0,1,e$0
e$0:
	unlk	a6
	rts
