# {updated by mmm: Sat Mar 11 02:58:56 1989 GMT}
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
SRC =		MCBase.d MCBase.m2 MCBigSet.d MCBigSet.m2 MCDump.d MCDump.m2 \
		MCHalfword.d MCHalfword.m2 MCOperation.d MCOperation.m2 \
		MCStop.d MCStop.m2 MCSymFileDe.d MCSymFileDe.m2 MCTypes.d \
		MCTypes.m2 Storage.d Storage.m2
TAGSRC =	MCBase.m2 MCBigSet.m2 MCDump.m2 MCHalfword.m2 MCOperation.m2 \
		MCStop.m2 MCSymFileDe.m2 MCTypes.m2 Storage.m2
OBJS =		
SYMS =		
REFS =		MCBase.r MCBigSet.r MCDump.r MCHalfword.r MCOperation.r \
		MCStop.r MCSymFileDe.r MCTypes.r Storage.r
# {precious}
LIB =		m68.a
.PRECIOUS:	$(LIB) SYM
# {all}
all:		$(LIB)
# {linkage}
LIBS =
LDFLAGS =	
# {dependencies}
$(LIB):		$(LIB)(MCBase.o) $(LIB)(MCBigSet.o) $(LIB)(MCDump.o) \
		$(LIB)(MCHalfword.o) $(LIB)(MCOperation.o) $(LIB)(MCStop.o) \
		$(LIB)(MCSymFileDe.o) $(LIB)(MCTypes.o) $(LIB)(Storage.o)
SYM(MCBase.sy):	MCBase.d; $(M2C) $(MFLAGS) MCBase.d
$(LIB)(MCBase.o): SYM(MCBase.sy) MCBase.m2
		$(M2C) $(MFLAGS) -c MCBase.m2
		ar rv $(LIB) MCBase.o && ranlib $(LIB) && rm -f MCBase.o
SYM(MCBigSet.sy): MCBigSet.d SYM(MCBase.sy); $(M2C) $(MFLAGS) MCBigSet.d
$(LIB)(MCBigSet.o): SYM(MCBase.sy) SYM(MCBigSet.sy) SYM(Storage.sy) \
		MCBigSet.m2
		$(M2C) $(MFLAGS) -c MCBigSet.m2
		ar rv $(LIB) MCBigSet.o && ranlib $(LIB) && rm -f MCBigSet.o
SYM(MCDump.sy):	MCDump.d SYM(MCBase.sy); $(M2C) $(MFLAGS) MCDump.d
$(LIB)(MCDump.o): SYM(MCBase.sy) SYM(MCDump.sy) SYM(MCTypes.sy) MCDump.m2
		$(M2C) $(MFLAGS) -c MCDump.m2
		ar rv $(LIB) MCDump.o && ranlib $(LIB) && rm -f MCDump.o
SYM(MCHalfword.sy): MCHalfword.d; $(M2C) $(MFLAGS) MCHalfword.d
$(LIB)(MCHalfword.o): SYM(MCHalfword.sy) SYM(MCStop.sy) MCHalfword.m2
		$(M2C) $(MFLAGS) -c MCHalfword.m2
		ar rv $(LIB) MCHalfword.o && ranlib $(LIB) && rm -f \
		MCHalfword.o
SYM(MCOperation.sy): MCOperation.d SYM(MCBase.sy); $(M2C) $(MFLAGS) \
		MCOperation.d
$(LIB)(MCOperation.o): SYM(MCBase.sy) SYM(MCBigSet.sy) SYM(MCOperation.sy) \
		SYM(MCTypes.sy) MCOperation.m2
		$(M2C) $(MFLAGS) -c MCOperation.m2
		ar rv $(LIB) MCOperation.o && ranlib $(LIB) && rm -f \
		MCOperation.o
SYM(MCStop.sy):	MCStop.d; $(M2C) $(MFLAGS) MCStop.d
$(LIB)(MCStop.o): SYM(MCStop.sy) MCStop.m2
		$(M2C) $(MFLAGS) -c MCStop.m2
		ar rv $(LIB) MCStop.o && ranlib $(LIB) && rm -f MCStop.o
SYM(MCSymFileDe.sy): MCSymFileDe.d; $(M2C) $(MFLAGS) MCSymFileDe.d
$(LIB)(MCSymFileDe.o): SYM(MCSymFileDe.sy) MCSymFileDe.m2
		$(M2C) $(MFLAGS) -c MCSymFileDe.m2
		ar rv $(LIB) MCSymFileDe.o && ranlib $(LIB) && rm -f \
		MCSymFileDe.o
SYM(MCTypes.sy): MCTypes.d SYM(MCBase.sy); $(M2C) $(MFLAGS) MCTypes.d
$(LIB)(MCTypes.o): SYM(MCBase.sy) SYM(MCTypes.sy) SYM(Storage.sy) MCTypes.m2
		$(M2C) $(MFLAGS) -c MCTypes.m2
		ar rv $(LIB) MCTypes.o && ranlib $(LIB) && rm -f MCTypes.o
SYM(Storage.sy): Storage.d; $(M2C) $(MFLAGS) Storage.d
$(LIB)(Storage.o): SYM(MCBase.sy) SYM(Storage.sy) Storage.m2
		$(M2C) $(MFLAGS) -c Storage.m2
		ar rv $(LIB) Storage.o && ranlib $(LIB) && rm -f Storage.o
