#------------------------------------------------------------------------------
# Modula-2 runtime library -- runtime initializations
# (c) University of Ulm, Sektion Informatik, D-7900 Ulm
# afb 4/88 on SUN hr 1/89 -- mm2rt0.s: runtime startoff for profiling
#------------------------------------------------------------------------------
	cbufs  =  1000		| no. of words in call-count buffer
	factor  =  1		| mapping factor (must be power of 2)
	ldfactor  =  0		| factor = 2^ldfactor
	infosize  =  12		| filled by Monitor and read by mprof
	sbrk = 17		| undocumented but used by /lib/libc.a
#------------------------------------------------------------------------------
	.text
	trapf			| make sure we have MC68020 and up
#
# make argc, argv and environment available for Modula-2
#
	movl	sp@,_ARGC
	lea	sp@(4),a0
	movl	a0,_ARGV
findenv:
	tstl	a0@+
	bne	findenv
	cmpl	sp@(4),a0	| environment given?
	blt	envfound
	subl	#4,a0		| now: _ENV^ = 0
envfound:
	movl	a0,_ENV
#
# calculate profile buffer size:
#    infosize + cbufs*2*4 +
#         (ADR(etext) - ADR(eprol) + factor*4-1) / (factor*4) * 4
#
	movl	#_etext,d3
	subl	#eprol,d3
	addl	#factor*4-1,d3
	lsrl	#ldfactor+2,d3
	lsll	#2,d3
	addl	#infosize,d3
	addl	#cbufs*8,d3
#
# get storage for profile buffer
#
	movl	#_end,a0
	addl	d3,a0
	movl	a0,sp@-		| sbrk(& end + buffersize)
	subl	#4,sp
	movl	#sbrk,sp@-
	trap	#0
	scc	d2
	negb	d2
	addl	#8,sp
	tstb	d2
	bne	ok
	jsr	.stack		| cannot allocate profile buffer
ok:
	movl	a0,_BREAK
#
# start profiling
#
	movl	#_end,d0
	addl	#infosize,d0
	movl	d0,.countb
#
# Monitor(ADR(eprol), ADR(etext), buf (&end), bufsiz (in %d3), cbufs)
#
	jsr	_SysExit_P0	| initialize SysExit
	jsr	_SysMonitor_P0	| initialize SysMonitor
	movl	#eprol,sp@-
	movl	#_etext,sp@-
	movl	#_end,sp@-
	movl	d3,sp@-
	movl	#cbufs,sp@-
	jsr	_SysMonitor_P1
#
# start Modula-2
#
	movl	#0,a5		| no limit supported in main coroutine
	jsr	_M2START	| call main module
#
# call SysExit.Exit(0)
#
	movl	#0,sp@-		| push argument
	jsr	_SysExit_P1
#
# not reached -- but we want to be sure...
#
	movl	#0,sp@-
	movl	#1,d0
	trap	#0		| exit(0)
#
# end of prologue
#
eprol:
#
	.data
	.comm	_ARGC,4
	.comm	_ARGV,4
	.comm	_ENV,4
	.comm	_BREAK,4
	.comm	.countb,4
