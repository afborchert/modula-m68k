# {updated by mmm: Sat Mar 11 02:57:45 1989 GMT}
# {flags: -a+ -e- -l- -L- -m- -1- -v-}
# {rules}
.SUFFIXES:
.SUFFIXES:	.o .m2
MFLAGS =	
M2C =		m2c
.m2.o:
		$(M2C) -c $(MFLAGS) $*.m2
# {macros}
SRC =		MCSymFile.m2
TAGSRC =	MCSymFile.m2
OBJS =		MCSymFile.o
SYMS =		
REFS =		MCSymFile.r
# {precious}
.PRECIOUS:	SYM
# {all}
T =		msym
all:		$T
# {linkage}
LIBS =		m68.a
LDFLAGS =	
$T:		MCSymFile.o
		$(M2C) $(MFLAGS) $(LDFLAGS) MCSymFile.o $(LIBS)
		cp a.out $@ && rm a.out
# {dependencies}
MCSymFile.o:	
