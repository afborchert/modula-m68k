# Modula-2 runtime library -- makefile
# (c) University of Ulm, Sektion Informatik, D-7900 Ulm
# afb 4/88
AS =		/bin/as # must be 68020 assembler!
ASFLAGS =	# -m

.s.a:		
		$(AS) -o $*.o $(ASFLAGS) $*.s
		ar rv $@ $*.o
		ranlib $@
		rm -f $*.o

LIB =		libm2.a
PLIB =		plibm2.a
all:		$(LIB) $(PLIB) m2rt0.o cm2rt0.o mm2rt0.o post.o
$(LIB):		$(LIB)(MathLib.o) $(LIB)(SysLocations.o) \
		$(LIB)(signal.o) $(LIB)(transfer.o) $(LIB)(rte.o) \
		$(LIB)(prio.o) $(LIB)(mcount.o) $(LIB)(MathLib1.o)
$(PLIB):	$(PLIB)(MathLib.o) $(PLIB)(SysLocations.o) \
		$(PLIB)(signal.o) $(PLIB)(transfer.o) $(PLIB)(rte.o) \
		$(PLIB)(prio.o) $(PLIB)(mcount.o) $(PLIB)(MathLib1.o)
