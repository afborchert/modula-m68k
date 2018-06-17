#------------------------------------------------------------------------------
# GNU-makefile for mprof
# afb 2/93
#------------------------------------------------------------------------------
T :=		mprof
MANPAGE :=	mprof.1
BINDIR :=	/usr/local/bin
LIBDIR :=	/usr/local/lib/modula
MANDIR :=	/usr/local/man/man1
#------------------------------------------------------------------------------
DEFINES :=	-DMPROF
#------------------------------------------------------------------------------
CC :=		gcc
CFLAGS :=	-g -O $(DEFINES) -Wall
LDFLAGS :=	-g
#------------------------------------------------------------------------------
SRCS :=		mp_aout.c mp_main.c mp_map.c mp_path.c mp_ref.c \
		mp_refparse.c mp_statistic.c mp_strsave.c
OBJS :=		mp_aout.o mp_main.o mp_map.o mp_path.o mp_ref.o \
		mp_refparse.o mp_statistic.o mp_strsave.o
#------------------------------------------------------------------------------
.PHONY:		all
all:		$T
$T:		$(OBJS)
		$(CC) -o $@ $(LDFLAGS) $(OBJS)
#------------------------------------------------------------------------------
.PHONY:		depend
depend:		$(SRCS)
		makedepend -- $(CFLAGS) -- $(SRCS)
#------------------------------------------------------------------------------
.PHONY:		install
install:	$(BINDIR)/$T $(MANDIR)/$(MANPAGE)
$(BINDIR)/$T:	$T
		cp $^ $@
$(MANDIR)/$(MANPAGE):	$(MANPAGE)
		cp $^ $@
#------------------------------------------------------------------------------
.PHONY:		clean
clean:		; rm -f $(OBJS) core
.PHONY:		realclean
realclean:	clean
		rm $T
#------------------------------------------------------------------------------
# DO NOT DELETE THIS LINE -- make depend depends on it.

mp_aout.o: mp.h mp_config.h /usr/include/sys/param.h
mp_aout.o: /usr/include/machine/param.h /usr/include/sys/signal.h
mp_aout.o: /usr/include/vm/faultcode.h /usr/include/stdio.h
mp_aout.o: /usr/include/ctype.h /usr/include/string.h
mp_aout.o: /usr/include/sys/stdtypes.h /usr/include/a.out.h mp_map.h
mp_main.o: /usr/include/stdio.h
mp_map.o: /usr/include/stdio.h mp.h mp_config.h /usr/include/sys/param.h
mp_map.o: /usr/include/machine/param.h /usr/include/sys/signal.h
mp_map.o: /usr/include/vm/faultcode.h mp_map.h
mp_path.o: /usr/include/stdio.h mp.h mp_config.h /usr/include/sys/param.h
mp_path.o: /usr/include/machine/param.h /usr/include/sys/signal.h
mp_path.o: /usr/include/vm/faultcode.h
mp_ref.o: /usr/include/sys/types.h /usr/include/sys/stdtypes.h
mp_ref.o: /usr/include/sys/sysmacros.h /usr/include/sys/dir.h
mp_ref.o: /usr/include/stdio.h /usr/include/ar.h mp.h mp_config.h
mp_ref.o: /usr/include/sys/param.h /usr/include/machine/param.h
mp_ref.o: /usr/include/sys/signal.h /usr/include/vm/faultcode.h mp_ref.h
mp_refparse.o: /usr/include/stdio.h /usr/include/setjmp.h mp_ref.h mp.h
mp_refparse.o: mp_config.h /usr/include/sys/param.h
mp_refparse.o: /usr/include/machine/param.h /usr/include/sys/signal.h
mp_refparse.o: /usr/include/vm/faultcode.h
mp_statistic.o: /usr/include/stdio.h /usr/include/sys/types.h
mp_statistic.o: /usr/include/sys/stdtypes.h /usr/include/sys/sysmacros.h
mp_statistic.o: /usr/include/sys/stat.h mp.h mp_config.h
mp_statistic.o: /usr/include/sys/param.h /usr/include/machine/param.h
mp_statistic.o: /usr/include/sys/signal.h /usr/include/vm/faultcode.h
mp_strsave.o: /usr/include/stdio.h
