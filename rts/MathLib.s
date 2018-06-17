#------------------------------------------------------------------------
#Modula-2 runtime library -- interface to math funcs of MC68881
# (c) University of Ulm, Sektion Informatik, D-7900 Ulm
# afb 3/88 on SUN hr 1/89
#------------------------------------------------------------------------
# IMPLEMENTATION MODULE MathLib; (* Modula-2 compatible implementation*)
#------------------------------------------------------------------------
	.text
	.stabs	"MathLib.m2",0144,0,0,L$0
L$0:
	.globl	_MathLib
_MathLib:
	.lcomm	G_AREA,0
	.lcomm	_FLAG,4
	_MathLib_V0 = G_AREA
	.globl	_MathLib_V0
	.even
#------------------------------------------------------------------------
#  PROCEDURE arctan(arg: REAL) : REAL;
#------------------------------------------------------------------------
LP$1:
	.stabs	"MathLib_P1",044,0,0,_MathLib_P1
_MathLib_P1:
	.globl	_MathLib_P1
	linkw	a6,#0
	.stabn	0244,0,0,b$1
b$1:
	fatand	a6@(8),fp0
	.stabn	0244,0,1,e$1
e$1:
	unlk	a6
	rtd	#8
	.even
#------------------------------------------------------------------------
#  PROCEDURE exp(arg: REAL) : REAL;
#------------------------------------------------------------------------
LP$2:
	.stabs	"MathLib_P2",044,0,0,_MathLib_P2
_MathLib_P2:
	.globl	_MathLib_P2
	linkw	a6,#0
	.stabn	0244,0,0,b$2
b$2:
	fetoxd	a6@(8),fp0
	.stabn	0244,0,1,e$2
e$2:
	unlk	a6
	rtd	#8
	.even
#------------------------------------------------------------------------
#  PROCEDURE ln(arg: REAL) : REAL;
#------------------------------------------------------------------------
LP$3:
	.stabs	"MathLib_P3",044,0,0,_MathLib_P3
_MathLib_P3:
	.globl	_MathLib_P3
	linkw	a6,#0
	.stabn	0244,0,0,b$3
b$3:
	flognd	a6@(8),fp0
	.stabn	0244,0,1,e$3
e$3:
	unlk	a6
	rtd	#8
	.even
#------------------------------------------------------------------------
#  PROCEDURE sin(arg: REAL) : REAL;
#------------------------------------------------------------------------
LP$4:
	.stabs	"MathLib_P4",044,0,0,_MathLib_P4
_MathLib_P4:
	.globl	_MathLib_P4
	linkw	a6,#0
	.stabn	0244,0,0,b$4
b$4:
	fsind	a6@(8),fp0
	.stabn	0244,0,1,e$4
e$4:
	unlk	a6
	rtd	#8
	.even
#------------------------------------------------------------------------
#  PROCEDURE cos(arg: REAL) : REAL;
#------------------------------------------------------------------------
LP$5:
	.stabs	"MathLib_P5",044,0,0,_MathLib_P5
_MathLib_P5:
	.globl	_MathLib_P5
	linkw	a6,#0
	.stabn	0244,0,0,b$5
b$5:
	fcosd	a6@(8),fp0
	.stabn	0244,0,1,e$5
e$5:
	unlk	a6
	rtd	#8
	.even
#------------------------------------------------------------------------
#  PROCEDURE sqrt(arg: REAL) : REAL;
#------------------------------------------------------------------------
LP$6:
	.stabs	"MathLib_P6",044,0,0,_MathLib_P6
_MathLib_P6:
	.globl	_MathLib_P6
	linkw	a6,#0
	.stabn	0244,0,0,b$6
b$6:
	fsqrtd	a6@(8),fp0
	.stabn	0244,0,1,e$6
e$6:
	unlk	a6
	rtd	#8
	.even
#------------------------------------------------------------------------
#  initialisation 
#------------------------------------------------------------------------
LP$0:
	.stabs	"MathLib_P0",044,0,0,_MathLib_P0
_MathLib_P0:
	.globl	_MathLib_P0
	linkw	a6,#0
	tas	_FLAG
	unlk	a6
	rts
	.even

