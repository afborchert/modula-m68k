#
# Makefile for m2b
#
# (c) Andreas Borchert 1983, University of Ulm
#
CFLAGS=-DULM -DSYSV
# CC=m10cc
BINDIR=/usr/bin

OBJS=	m2b_debug.o m2b_errnr.o m2b_error.o m2b_format.o m2b_ignore.o \
	m2b_in.o m2b_main.o m2b_match.o m2b_optinit.o m2b_options.o \
	m2b_parser.o m2b_pass2.o m2b_shutdown.o m2b_ssyminit.o m2b_temp.o

make:	m2b

cmp:	m2b
	strip m2b
	cmp m2b ${BINDIR}/m2b
	rm m2b ${OBJS}

cp:	m2b
	strip m2b
	cp m2b ${BINDIR}/m2b

m2b:	${OBJS}
	cc -o m2b ${CFLAGS} ${OBJS}

clean:
	rm -f ${OBJS}
	rm -f m2b
	rm -f a.out core
	rm -f out
	rm -f cat_m2b.1

man:	m2b.1
	nroff -man m2b.1 > cat_m2b.1

m2b.1:	/usr/man/man1/m2b.1
	cp /usr/man/man1/m2b.1 m2b.1

tags:	${SRCFILES}
	ctags ${SRCFILES}

m2b_main.o:	m2b_symbols.h
m2b_parser.o:	m2b_symbols.h
m2b_ssyminit.o:	m2b_symbols.h
