	.text
	.stabs	"SysLocations.m2",0144,0,0,L$0
L$0:
	.globl _SysLocations
_SysLocations:
	.lcomm	G_AREA,20
	.lcomm	_FLAG,4
	_SysLocations_V0 = G_AREA
	_SysLocations_V  = G_AREA
	.globl	_SysLocations_V0
	.globl	_SysLocations_V
	.even
LP$0:
	.stabs	"SysLocations_P0",044,0,0,_SysLocations_P0
_SysLocations_P0:
	.globl	_SysLocations_P0	
	linkw	a6,#I$1
	.stabn	0244,0,0,b$1
b$1:
	tas	_FLAG
	bne	B$0

	movl	#_end,G_AREA
	movl	#_etext,G_AREA+4
	movl	#_edata,G_AREA+8
	movl	_BREAK,G_AREA+12
	movl	_ENV,G_AREA+16
B$0:
	I$1 = 0
	.stabn	0244,0,0,e$1
e$1:
	unlk	a6
	rts
