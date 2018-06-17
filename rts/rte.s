#------------------------------------------------------------------------------
# Modula-2 runtime library -- runtime error aborts
# (c) University of Ulm, Sektion Informatik, D-7900 Ulm
# afb 2/88 on SUN hr 2/89
#------------------------------------------------------------------------------
	HALT = 1		| call to procedure halt
	CASE = 2		| no case label
	STACK = 3		| stack overflow
	CREND = 4		| coroutine end
	PRIO = 5		| priority error
	FRET = 6		| function returns no value
	RANGE = 7		| range check failure
	.data
	.comm	.base,4
	.comm	.top,4
	.comm	.code,4		| see above
	.lcomm	_FLAG,4		| check for looping
#------------------------------------------------------------------------------
# type of range checks
#------------------------------------------------------------------------------
	unsigned = 1
	signed = 2
	sign = 3		| check for sign bit
	dyn = 4		| check for dynamic array bounds
	.data
	.globl	.rgeerr
	.lcomm	.rgeerr,4	| see above
	.lcomm	pc_,4		| error position
	.lcomm	value,4		| checked value
	.lcomm	low,4		| bounds
	.lcomm	high,4
#------------------------------------------------------------------------------
# range check aborts
#------------------------------------------------------------------------------
	.text
	.globl	.chksig
.chksig:
	movl	#signed,.rgeerr
	bra	chkrge
	.globl	.chkusig
.chkusig:
	movl	#unsigned,.rgeerr
chkrge:
	movl	d0,value
	movl	sp@(4:l)@,low
	movl	sp@(4:l)@(4),high
	movl	#RANGE,.code
	jmp	.abort
	.globl	.chksign
.chksign:
	movl	#sign,.rgeerr
	movl	#RANGE,.code
	jmp	.abort
	.globl	.chkdyn
.chkdyn:
	movl	#dyn,.rgeerr
	movl	#RANGE,.code
	jmp	.abort
#------------------------------------------------------------------------------
	.text
	.globl	.halt
.halt:
	movl	#HALT,.code
	jmp	.abort
#------------------------------------------------------------------------------
	.text
	.globl	.fret
.fret:
	movl	#FRET,.code
	jmp	.abort
#------------------------------------------------------------------------------
	.text
	.globl	.case
.case:
	movl	#CASE,.code
	jmp	.abort
#------------------------------------------------------------------------------
	.text
	.globl	.stack
.stack:
	movl	#STACK,.code
	jmp	.abort
#------------------------------------------------------------------------------
	.text
	.globl	.crend
.crend:
	movl	#CREND,.code
	jmp	.abort
#------------------------------------------------------------------------------
	.text
	.globl	.prio
.prio:
	movl	#PRIO,.code
	jmp	.abort
#------------------------------------------------------------------------------
.abort:
	movl	a6,.base
	movl	sp@,pc_
	subl	#4,sp
	movl	sp,.top
#	-------------------------------------------
	subl	#4,sp
	movl	#0x14,sp@-	| getpid
	trap	#0
	movl	#6,sp@-		| SIGIOT
	movl	d0,sp@-		| process id
	subl	#4,sp
	movl	#0x25,sp@-	| kill
	trap	#0
	trap	#2		| last chance
