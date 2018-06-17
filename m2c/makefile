#------------------------------------------------------------------------------
# Modula-2 driver -- makefile for SUN
# (c) University of Ulm, Sektion Informatik, D-7900 Ulm
# afb 4/88
#------------------------------------------------------------------------------
# don't add comments or spaces to the macro definitions following!

# where to find m2c
BINDIR=/usr/local/bin

# default library directory (with trailing "/")
LIBDIR=/usr/local/lib/modula

# MC68020 assembler
M20AS=/usr/local/bin/gas

# output filter for -LP option
PR=/usr/bin/pr

# version number (3-6 chars)
VERSION=2.2.1

# additional defs
DEFS=-DGAS136 -DFP=1


#------------------------------------------------------------------------------
CFLAGS =	-DSUN $(DEFS) \
		-DPR='"$(PR)"' -DVERSION='"$(VERSION)"' \
		-DLIBDIR='"$(LIBDIR)/"' \
		-DM20AS='"$(M20AS)"'
$(BINDIR)/m2c:	m2c.c
		$(CC) -s $(CFLAGS) -o m2c m2c.c
		cp m2c $(BINDIR)
		rm m2c
