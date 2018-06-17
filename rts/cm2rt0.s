#-----------------------------------------------------------------
# Modula-2 runtime library -- runtime initializations
# (c) University of Ulm, Sektion Informati, D-7900
# afb 4/88 on SUN hr 2/89 -- cm2rt0.s: runtime startoff for programs with
# stack checks and/or priorities
#-----------------------------------------------------------------
	.text
	trapf			| make sure we have MC68000 and up
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
# allocate priority stack
#
	movl	#priostack,a5	| a5 : limit
	movl	#0,a5@+		| push priority 0
	jsr	signal_P0_	| initialize signal handler
#
# start Modula-2
#
	jsr	_SysExit_P0	| initialize SysExit
	jsr	_M2START	| call main module
#
# call SysExit.Exit(0)
#
	movl	#0,sp@-
	jsr	_SysExit_P1
#
# not reached -- but we want to be sure...
#
	movl	#0,sp@-		| push argument
	movl	#1,d0
	trap	#0		| exit(0)
	.data
	.comm	_ARGC,4
	.comm	_ARGV,4
	.comm	_ENV,4
	.comm	_BREAK,4
	.lcomm	priostack,1024
