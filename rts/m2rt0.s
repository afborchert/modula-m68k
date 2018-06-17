#-----------------------------------------------------------------
# Modula-2 runtime library -- runtime initializations
# (c) University of Ulm, Sektion Informatik, D-7900
# hr 2/89
#-----------------------------------------------------------------
	.text
	trapf			| make sure we have MC6800
#
# make argc, argv and environment available for Modula-2
#
	movl	sp@,_ARGC
	lea	sp@(4:L),a0
	movl	a0,_ARGV
findenv:
	tstl	a0@+
	bne	findenv
	cmpl	sp@(4:L),a0	| environment given?
	blt	envfound
	subl	#4,a0		| now: _ENV^ =0
envfound:
	movl	a0,_ENV
	movl	#_end,_BREAK
#
# start Modula-2
#
	movl	#0,a5		| no limit supported in main coroutine
	jsr	_SysExit_P0	| initialize SysExit
	jsr	_M2START	| call main module
#
# call SysExit.Exit(0)
#
	movl	#0,sp@-		| push argument
	jsr	_SysExit_P1
#
# not reached -- but we want to be sure
#
	movl	#0,sp@-
	movl	#1,d0
	trap	#0		| exit(0)
#
	.data
	.comm	_ARGC,4
	.comm	_ARGV,4
	.comm	_ENV,4
	.comm	_BREAK,4
