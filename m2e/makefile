BINDIR = /usr/spez/diplom/borchert/bin
OBJS   = m2e.o m2e_errnr.o m2e_source.o
CFLAGS = -DSUN -DULM

m2e:	${OBJS}
	${CC} -o m2e ${OBJS}

m2e.o:	m2e_symbols.h

cp:	m2e
	strip m2e
	cp m2e ${BINDIR}
	rm m2e

clean:
	rm -f ${OBJS} a.out core
