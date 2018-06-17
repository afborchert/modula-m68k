# {updated by mmm: Sat Mar 11 02:55:24 1989 GMT}
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
SRC =		MCP2IO.d MCP2IO.m2 MCP2Ident.d MCP2Ident.m2 MCP2Public.d \
		MCP2Public.m2 MCP2Referen.d MCP2Referen.m2 MCPass2.m2
TAGSRC =	MCP2IO.m2 MCP2Ident.m2 MCP2Public.m2 MCP2Referen.m2 \
		MCPass2.m2
OBJS =		MCPass2.o
SYMS =		
REFS =		MCP2IO.r MCP2Ident.r MCP2Public.r MCP2Referen.r MCPass2.r
# {precious}
LIB =		p2.a
.PRECIOUS:	$(LIB) SYM
# {all}
T =		m1
all:		$(LIB) $T
# {linkage}
LIBS =		m68.a
LDFLAGS =	
$T:		MCPass2.o $(LIB)(MCP2IO.o) $(LIB)(MCP2Ident.o) \
		$(LIB)(MCP2Public.o) $(LIB)(MCP2Referen.o)
		$(M2C) $(MFLAGS) $(LDFLAGS) MCPass2.o $(LIB) $(LIBS)
		cp a.out $@ && rm a.out
# {dependencies}
$(LIB):		$(LIB)(MCP2IO.o) $(LIB)(MCP2Ident.o) $(LIB)(MCP2Public.o) \
		$(LIB)(MCP2Referen.o)
SYM(MCP2IO.sy):	MCP2IO.d; $(M2C) $(MFLAGS) MCP2IO.d
$(LIB)(MCP2IO.o): SYM(MCP2IO.sy) SYM(MCP2Public.sy) MCP2IO.m2
		$(M2C) $(MFLAGS) -c MCP2IO.m2
		ar rv $(LIB) MCP2IO.o && ranlib $(LIB) && rm -f MCP2IO.o
SYM(MCP2Ident.sy): MCP2Ident.d SYM(MCP2IO.sy); $(M2C) $(MFLAGS) MCP2Ident.d
$(LIB)(MCP2Ident.o): SYM(MCP2IO.sy) SYM(MCP2Ident.sy) MCP2Ident.m2
		$(M2C) $(MFLAGS) -c MCP2Ident.m2
		ar rv $(LIB) MCP2Ident.o && ranlib $(LIB) && rm -f \
		MCP2Ident.o
SYM(MCP2Public.sy): MCP2Public.d; $(M2C) $(MFLAGS) MCP2Public.d
$(LIB)(MCP2Public.o): SYM(MCP2Public.sy) MCP2Public.m2
		$(M2C) $(MFLAGS) -c MCP2Public.m2
		ar rv $(LIB) MCP2Public.o && ranlib $(LIB) && rm -f \
		MCP2Public.o
SYM(MCP2Referen.sy): MCP2Referen.d; $(M2C) $(MFLAGS) MCP2Referen.d
$(LIB)(MCP2Referen.o): SYM(MCP2IO.sy) SYM(MCP2Public.sy) SYM(MCP2Referen.sy) \
		MCP2Referen.m2
		$(M2C) $(MFLAGS) -c MCP2Referen.m2
		ar rv $(LIB) MCP2Referen.o && ranlib $(LIB) && rm -f \
		MCP2Referen.o
MCPass2.o:	SYM(MCP2IO.sy) SYM(MCP2Ident.sy) SYM(MCP2Public.sy) \
		SYM(MCP2Referen.sy)
