#------------------------------------------------------------------------
#Modula-2 runtime library -- interface to math funcs of MC68881
# (c) University of Ulm, Sektion Informatik, D-7900 Ulm
# afb 3/88 on SUN hr 1/89
#------------------------------------------------------------------------
# IMPLEMENTATION MODULE MathLib1; (* Modula-2 compatible implementation*)
#------------------------------------------------------------------------
	.text
	.stabs	"MathLib1.m2",0144,0,0,L$0
L$0:
	.globl	_MathLib1
_MathLib1:
	.lcomm	G_AREA,0
	.lcomm	_FLAG,4
	_MathLib1_V0 = G_AREA
	.globl	_MathLib1_V0
	.even
#------------------------------------------------------------------------
#  PROCEDURE arcsin(arg: REAL) : REAL;
#------------------------------------------------------------------------
LP$1:
	.stabs	"MathLib1",044,0,0,_MathLib1_P1
_MathLib1_P1:
	.globl	_MathLib1_P1
	linkw	a6,#0
	.stabn	0244,0,0,b$1
b$1:
	fasind	a6@(8),fp0
	.stabn	0244,0,1,b$1
e$1:
	unlk	a6
	rtd	#8
	.even
#------------------------------------------------------------------------
#  PROCEDURE arccos(arg: REAL) : REAL;
#------------------------------------------------------------------------
LP$2:
	.stabs	"MathLib1",044,0,0,_MathLib1_P2
_MathLib1_P2:
	.globl	_MathLib1_P2
	linkw	a6,#0
	.stabn	0244,0,0,b$2
b$2:
	facosd	a6@(8),fp0
	.stabn	0244,0,1,e$2
e$2:
	unlk	a6
	rtd	#8
	.even
#------------------------------------------------------------------------
#  initialisation 
#------------------------------------------------------------------------
LP$0:
_MathLib1_P0:
	.globl	_MathLib1_P0
	linkw	a6,#I$5
	tas	_FLAG
	I$5	= 0
	unlk	a6
	rts
	.even

