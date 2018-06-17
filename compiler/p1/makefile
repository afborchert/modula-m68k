# {updated by mmm: Sat Mar 11 02:49:37 1989 GMT}
# {flags: -a+ -e- -l+ -L- -m- -1- -v-}
# {rules}
.SUFFIXES:
.SUFFIXES:	.o .sy .m2 .d
MFLAGS =	
M2C =		m2c
.m2.o:
		$(M2C) -c $(MFLAGS) $*.m2
.d.a:
		$(M2C) $(MFLAGS) $*.d
# {macros}
SRC =		MCP1IO.d MCP1IO.m2 MCP1Ident.d MCP1Ident.m2 MCP1Public.d \
		MCP1Public.m2 MCP1Reals.d MCP1Reals.m2 MCPass1.m2
TAGSRC =	MCP1IO.m2 MCP1Ident.m2 MCP1Public.m2 MCP1Reals.m2 MCPass1.m2
OBJS =		MCPass1.o
SYMS =		
REFS =		MCP1IO.r MCP1Ident.r MCP1Public.r MCP1Reals.r MCPass1.r
# {precious}
LIB =		p1.a
.PRECIOUS:	$(LIB) SYM
# {all}
T =		m0
all:		$(LIB) $T
# {linkage}
LIBS =		m68.a
LDFLAGS =	
$T:		MCPass1.o $(LIB)(MCP1IO.o) $(LIB)(MCP1Ident.o) \
		$(LIB)(MCP1Public.o) $(LIB)(MCP1Reals.o)
		$(M2C) $(MFLAGS) $(LDFLAGS) MCPass1.o $(LIB) $(LIBS)
		cp a.out $@ && rm a.out
# {dependencies}
$(LIB):		$(LIB)(MCP1IO.o) $(LIB)(MCP1Ident.o) $(LIB)(MCP1Public.o) \
		$(LIB)(MCP1Reals.o)
SYM(MCP1IO.sy):	MCP1IO.d; $(M2C) $(MFLAGS) MCP1IO.d
$(LIB)(MCP1IO.o): SYM(MCP1IO.sy) SYM(MCP1Public.sy) SYM(MCP1Reals.sy) \
		MCP1IO.m2
		$(M2C) $(MFLAGS) -c MCP1IO.m2
		ar rv $(LIB) MCP1IO.o && ranlib $(LIB) && rm -f MCP1IO.o
SYM(MCP1Ident.sy): MCP1Ident.d; $(M2C) $(MFLAGS) MCP1Ident.d
$(LIB)(MCP1Ident.o): SYM(MCP1IO.sy) SYM(MCP1Ident.sy) MCP1Ident.m2
		$(M2C) $(MFLAGS) -c MCP1Ident.m2
		ar rv $(LIB) MCP1Ident.o && ranlib $(LIB) && rm -f \
		MCP1Ident.o
SYM(MCP1Public.sy): MCP1Public.d; $(M2C) $(MFLAGS) MCP1Public.d
$(LIB)(MCP1Public.o): SYM(MCP1Public.sy) MCP1Public.m2
		$(M2C) $(MFLAGS) -c MCP1Public.m2
		ar rv $(LIB) MCP1Public.o && ranlib $(LIB) && rm -f \
		MCP1Public.o
SYM(MCP1Reals.sy): MCP1Reals.d; $(M2C) $(MFLAGS) MCP1Reals.d
$(LIB)(MCP1Reals.o): SYM(MCP1Reals.sy) MCP1Reals.m2
		$(M2C) $(MFLAGS) -c MCP1Reals.m2
		ar rv $(LIB) MCP1Reals.o && ranlib $(LIB) && rm -f \
		MCP1Reals.o
MCPass1.o:	SYM(MCP1IO.sy) SYM(MCP1Ident.sy) SYM(MCP1Public.sy)
