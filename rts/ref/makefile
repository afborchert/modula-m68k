# Modula-2 runtime library -- reference files
# (c) University of Ulm, Sektion Informatik, D-7900 Ulm
# afb 4/88
.SUFFIXES:
.SUFFIXES:	.r .m2

M2C =		m2c
.m2.a:		
		$(M2C) -c $*.m2
		rm -f $*.o

REF:		REF(MathLib.r) REF(sig.r) REF(signal.r) REF(transfer.r) \
		REF(mcount.r) REF(MathLib1.r)
