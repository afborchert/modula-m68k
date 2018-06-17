# {updated by mmm: Tue Aug 22 23:56:28 1989 GMT}
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
SRC =		MCMnemonics.d MCMnemonics.m2 MCP4Address.d MCP4Address.m2 \
		MCP4Assign.d MCP4Assign.m2 MCP4Attribu.d MCP4Attribu.m2 \
		MCP4Block.d MCP4Block.m2 MCP4CallSys.d MCP4CallSys.m2 \
		MCP4CaseSys.d MCP4CaseSys.m2 MCP4CodeSys.d MCP4CodeSys.m2 \
		MCP4ConstAr.d MCP4ConstAr.m2 MCP4Designa.d MCP4Designa.m2 \
		MCP4Express.d MCP4Express.m2 MCP4FPEmula.d MCP4FPEmula.m2 \
		MCP4ForSys.d MCP4ForSys.m2 MCP4Global.d MCP4Global.m2 \
		MCP4IfSys.d MCP4IfSys.m2 MCP4In.d MCP4In.m2 MCP4Init.d \
		MCP4Init.m2 MCP4Labels.d MCP4Labels.m2 MCP4Load.d \
		MCP4Load.m2 MCP4LoopSys.d MCP4LoopSys.m2 MCP4Out.d \
		MCP4Out.m2 MCP4Public.d MCP4Public.m2 MCP4RTS.d MCP4RTS.m2 \
		MCP4RangeCh.d MCP4RangeCh.m2 MCP4Registe.d MCP4Registe.m2 \
		MCP4RepeatS.d MCP4RepeatS.m2 MCP4Scanner.d MCP4Scanner.m2 \
		MCP4SetCons.d MCP4SetCons.m2 MCP4Stack.d MCP4Stack.m2 \
		MCP4StatSys.d MCP4StatSys.m2 MCP4Strings.d MCP4Strings.m2 \
		MCP4Test.d MCP4Test.m2 MCP4Types.d MCP4Types.m2 \
		MCP4WhileSy.d MCP4WhileSy.m2 MCP4WithSys.d MCP4WithSys.m2 \
		MCPass4.m2
TAGSRC =	MCMnemonics.m2 MCP4Address.m2 MCP4Assign.m2 MCP4Attribu.m2 \
		MCP4Block.m2 MCP4CallSys.m2 MCP4CaseSys.m2 MCP4CodeSys.m2 \
		MCP4ConstAr.m2 MCP4Designa.m2 MCP4Express.m2 MCP4FPEmula.m2 \
		MCP4ForSys.m2 MCP4Global.m2 MCP4IfSys.m2 MCP4In.m2 \
		MCP4Init.m2 MCP4Labels.m2 MCP4Load.m2 MCP4LoopSys.m2 \
		MCP4Out.m2 MCP4Public.m2 MCP4RTS.m2 MCP4RangeCh.m2 \
		MCP4Registe.m2 MCP4RepeatS.m2 MCP4Scanner.m2 MCP4SetCons.m2 \
		MCP4Stack.m2 MCP4StatSys.m2 MCP4Strings.m2 MCP4Test.m2 \
		MCP4Types.m2 MCP4WhileSy.m2 MCP4WithSys.m2 MCPass4.m2
OBJS =		MCPass4.o
SYMS =		
REFS =		MCMnemonics.r MCP4Address.r MCP4Assign.r MCP4Attribu.r \
		MCP4Block.r MCP4CallSys.r MCP4CaseSys.r MCP4CodeSys.r \
		MCP4ConstAr.r MCP4Designa.r MCP4Express.r MCP4FPEmula.r \
		MCP4ForSys.r MCP4Global.r MCP4IfSys.r MCP4In.r MCP4Init.r \
		MCP4Labels.r MCP4Load.r MCP4LoopSys.r MCP4Out.r MCP4Public.r \
		MCP4RTS.r MCP4RangeCh.r MCP4Registe.r MCP4RepeatS.r \
		MCP4Scanner.r MCP4SetCons.r MCP4Stack.r MCP4StatSys.r \
		MCP4Strings.r MCP4Test.r MCP4Types.r MCP4WhileSy.r \
		MCP4WithSys.r MCPass4.r
# {precious}
LIB =		p4.a
.PRECIOUS:	$(LIB) SYM
# {all}
T =		m3
all:		$(LIB) $T
# {linkage}
LIBS =		m68.a
LDFLAGS =	
$T:		MCPass4.o $(LIB)(MCMnemonics.o) $(LIB)(MCP4Address.o) \
		$(LIB)(MCP4Assign.o) $(LIB)(MCP4Attribu.o) \
		$(LIB)(MCP4Block.o) $(LIB)(MCP4CallSys.o) \
		$(LIB)(MCP4CaseSys.o) $(LIB)(MCP4CodeSys.o) \
		$(LIB)(MCP4ConstAr.o) $(LIB)(MCP4Designa.o) \
		$(LIB)(MCP4Express.o) $(LIB)(MCP4FPEmula.o) \
		$(LIB)(MCP4ForSys.o) $(LIB)(MCP4Global.o) \
		$(LIB)(MCP4IfSys.o) $(LIB)(MCP4In.o) $(LIB)(MCP4Init.o) \
		$(LIB)(MCP4Labels.o) $(LIB)(MCP4Load.o) \
		$(LIB)(MCP4LoopSys.o) $(LIB)(MCP4Out.o) $(LIB)(MCP4Public.o) \
		$(LIB)(MCP4RTS.o) $(LIB)(MCP4RangeCh.o) \
		$(LIB)(MCP4Registe.o) $(LIB)(MCP4RepeatS.o) \
		$(LIB)(MCP4Scanner.o) $(LIB)(MCP4SetCons.o) \
		$(LIB)(MCP4Stack.o) $(LIB)(MCP4StatSys.o) \
		$(LIB)(MCP4Strings.o) $(LIB)(MCP4Test.o) $(LIB)(MCP4Types.o) \
		$(LIB)(MCP4WhileSy.o) $(LIB)(MCP4WithSys.o)
		$(M2C) $(MFLAGS) $(LDFLAGS) MCPass4.o $(LIB) $(LIBS)
		cp a.out $@ && rm a.out
# {dependencies}
$(LIB):		$(LIB)(MCMnemonics.o) $(LIB)(MCP4Address.o) \
		$(LIB)(MCP4Assign.o) $(LIB)(MCP4Attribu.o) \
		$(LIB)(MCP4Block.o) $(LIB)(MCP4CallSys.o) \
		$(LIB)(MCP4CaseSys.o) $(LIB)(MCP4CodeSys.o) \
		$(LIB)(MCP4ConstAr.o) $(LIB)(MCP4Designa.o) \
		$(LIB)(MCP4Express.o) $(LIB)(MCP4FPEmula.o) \
		$(LIB)(MCP4ForSys.o) $(LIB)(MCP4Global.o) \
		$(LIB)(MCP4IfSys.o) $(LIB)(MCP4In.o) $(LIB)(MCP4Init.o) \
		$(LIB)(MCP4Labels.o) $(LIB)(MCP4Load.o) \
		$(LIB)(MCP4LoopSys.o) $(LIB)(MCP4Out.o) $(LIB)(MCP4Public.o) \
		$(LIB)(MCP4RTS.o) $(LIB)(MCP4RangeCh.o) \
		$(LIB)(MCP4Registe.o) $(LIB)(MCP4RepeatS.o) \
		$(LIB)(MCP4Scanner.o) $(LIB)(MCP4SetCons.o) \
		$(LIB)(MCP4Stack.o) $(LIB)(MCP4StatSys.o) \
		$(LIB)(MCP4Strings.o) $(LIB)(MCP4Test.o) $(LIB)(MCP4Types.o) \
		$(LIB)(MCP4WhileSy.o) $(LIB)(MCP4WithSys.o)
SYM(MCMnemonics.sy): MCMnemonics.d; $(M2C) $(MFLAGS) MCMnemonics.d
$(LIB)(MCMnemonics.o): SYM(MCMnemonics.sy) MCMnemonics.m2
		$(M2C) $(MFLAGS) -c MCMnemonics.m2
		ar rv $(LIB) MCMnemonics.o && ranlib $(LIB) && rm -f \
		MCMnemonics.o
SYM(MCP4Address.sy): MCP4Address.d SYM(MCP4Attribu.sy); $(M2C) $(MFLAGS) \
		MCP4Address.d
$(LIB)(MCP4Address.o): SYM(MCP4Labels.sy) SYM(MCP4Public.sy) \
		SYM(MCP4Registe.sy) SYM(MCP4Attribu.sy) SYM(MCP4Block.sy) \
		SYM(MCP4Stack.sy) SYM(MCP4Address.sy) MCP4Address.m2
		$(M2C) $(MFLAGS) -c MCP4Address.m2
		ar rv $(LIB) MCP4Address.o && ranlib $(LIB) && rm -f \
		MCP4Address.o
SYM(MCP4Assign.sy): MCP4Assign.d SYM(MCP4Attribu.sy); $(M2C) $(MFLAGS) \
		MCP4Assign.d
$(LIB)(MCP4Assign.o): SYM(MCMnemonics.sy) SYM(MCP4Labels.sy) \
		SYM(MCP4Public.sy) SYM(MCP4Registe.sy) SYM(MCP4Scanner.sy) \
		SYM(MCP4Attribu.sy) SYM(MCP4CodeSys.sy) SYM(MCP4Designa.sy) \
		SYM(MCP4Express.sy) SYM(MCP4FPEmula.sy) SYM(MCP4Load.sy) \
		SYM(MCP4RangeCh.sy) SYM(MCP4Types.sy) SYM(MCP4Address.sy) \
		SYM(MCP4Assign.sy) MCP4Assign.m2
		$(M2C) $(MFLAGS) -c MCP4Assign.m2
		ar rv $(LIB) MCP4Assign.o && ranlib $(LIB) && rm -f \
		MCP4Assign.o
SYM(MCP4Attribu.sy): MCP4Attribu.d SYM(MCP4Labels.sy) SYM(MCP4Registe.sy); \
		$(M2C) $(MFLAGS) MCP4Attribu.d
$(LIB)(MCP4Attribu.o): SYM(MCP4Attribu.sy) MCP4Attribu.m2
		$(M2C) $(MFLAGS) -c MCP4Attribu.m2
		ar rv $(LIB) MCP4Attribu.o && ranlib $(LIB) && rm -f \
		MCP4Attribu.o
SYM(MCP4Block.sy): MCP4Block.d SYM(MCP4Labels.sy); $(M2C) $(MFLAGS) \
		MCP4Block.d
$(LIB)(MCP4Block.o): SYM(MCMnemonics.sy) SYM(MCP4Global.sy) \
		SYM(MCP4Labels.sy) SYM(MCP4Public.sy) SYM(MCP4RTS.sy) \
		SYM(MCP4Registe.sy) SYM(MCP4Scanner.sy) SYM(MCP4StatSys.sy) \
		SYM(MCP4Attribu.sy) SYM(MCP4Block.sy) SYM(MCP4CodeSys.sy) \
		SYM(MCP4ConstAr.sy) SYM(MCP4Stack.sy) SYM(MCP4Types.sy) \
		MCP4Block.m2
		$(M2C) $(MFLAGS) -c MCP4Block.m2
		ar rv $(LIB) MCP4Block.o && ranlib $(LIB) && rm -f \
		MCP4Block.o
SYM(MCP4CallSys.sy): MCP4CallSys.d SYM(MCP4Attribu.sy); $(M2C) $(MFLAGS) \
		MCP4CallSys.d
$(LIB)(MCP4CallSys.o): SYM(MCMnemonics.sy) SYM(MCP4Global.sy) \
		SYM(MCP4Labels.sy) SYM(MCP4Public.sy) SYM(MCP4RTS.sy) \
		SYM(MCP4Registe.sy) SYM(MCP4Scanner.sy) SYM(MCP4Attribu.sy) \
		SYM(MCP4Block.sy) SYM(MCP4CallSys.sy) SYM(MCP4CodeSys.sy) \
		SYM(MCP4ConstAr.sy) SYM(MCP4Designa.sy) SYM(MCP4Express.sy) \
		SYM(MCP4FPEmula.sy) SYM(MCP4Load.sy) SYM(MCP4RangeCh.sy) \
		SYM(MCP4Stack.sy) SYM(MCP4Types.sy) SYM(MCP4Address.sy) \
		MCP4CallSys.m2
		$(M2C) $(MFLAGS) -c MCP4CallSys.m2
		ar rv $(LIB) MCP4CallSys.o && ranlib $(LIB) && rm -f \
		MCP4CallSys.o
SYM(MCP4CaseSys.sy): MCP4CaseSys.d; $(M2C) $(MFLAGS) MCP4CaseSys.d
$(LIB)(MCP4CaseSys.o): SYM(MCMnemonics.sy) SYM(MCP4CaseSys.sy) \
		SYM(MCP4Global.sy) SYM(MCP4Labels.sy) SYM(MCP4RTS.sy) \
		SYM(MCP4Registe.sy) SYM(MCP4Scanner.sy) SYM(MCP4StatSys.sy) \
		SYM(MCP4Attribu.sy) SYM(MCP4CodeSys.sy) SYM(MCP4Express.sy) \
		SYM(MCP4Load.sy) SYM(MCP4Types.sy) MCP4CaseSys.m2
		$(M2C) $(MFLAGS) -c MCP4CaseSys.m2
		ar rv $(LIB) MCP4CaseSys.o && ranlib $(LIB) && rm -f \
		MCP4CaseSys.o
SYM(MCP4CodeSys.sy): MCP4CodeSys.d SYM(MCMnemonics.sy) SYM(MCP4Labels.sy) \
		SYM(MCP4Registe.sy); $(M2C) $(MFLAGS) MCP4CodeSys.d
$(LIB)(MCP4CodeSys.o): SYM(MCMnemonics.sy) SYM(MCP4Global.sy) \
		SYM(MCP4Labels.sy) SYM(MCP4Out.sy) SYM(MCP4Public.sy) \
		SYM(MCP4RTS.sy) SYM(MCP4Registe.sy) SYM(MCP4Attribu.sy) \
		SYM(MCP4Block.sy) SYM(MCP4CodeSys.sy) MCP4CodeSys.m2
		$(M2C) $(MFLAGS) -c MCP4CodeSys.m2
		ar rv $(LIB) MCP4CodeSys.o && ranlib $(LIB) && rm -f \
		MCP4CodeSys.o
SYM(MCP4ConstAr.sy): MCP4ConstAr.d SYM(MCMnemonics.sy) SYM(MCP4Registe.sy) \
		SYM(MCP4Attribu.sy); $(M2C) $(MFLAGS) MCP4ConstAr.d
$(LIB)(MCP4ConstAr.o): SYM(MCMnemonics.sy) SYM(MCP4Global.sy) \
		SYM(MCP4Registe.sy) SYM(MCP4Attribu.sy) SYM(MCP4CodeSys.sy) \
		SYM(MCP4ConstAr.sy) SYM(MCP4Express.sy) SYM(MCP4Load.sy) \
		SYM(MCP4Address.sy) MCP4ConstAr.m2
		$(M2C) $(MFLAGS) -c MCP4ConstAr.m2
		ar rv $(LIB) MCP4ConstAr.o && ranlib $(LIB) && rm -f \
		MCP4ConstAr.o
SYM(MCP4Designa.sy): MCP4Designa.d SYM(MCP4Attribu.sy); $(M2C) $(MFLAGS) \
		MCP4Designa.d
$(LIB)(MCP4Designa.o): SYM(MCMnemonics.sy) SYM(MCP4Global.sy) \
		SYM(MCP4Labels.sy) SYM(MCP4RTS.sy) SYM(MCP4Registe.sy) \
		SYM(MCP4Scanner.sy) SYM(MCP4Attribu.sy) SYM(MCP4Block.sy) \
		SYM(MCP4CodeSys.sy) SYM(MCP4ConstAr.sy) SYM(MCP4Designa.sy) \
		SYM(MCP4Express.sy) SYM(MCP4Load.sy) SYM(MCP4RangeCh.sy) \
		SYM(MCP4Types.sy) SYM(MCP4WithSys.sy) SYM(MCP4Address.sy) \
		MCP4Designa.m2
		$(M2C) $(MFLAGS) -c MCP4Designa.m2
		ar rv $(LIB) MCP4Designa.o && ranlib $(LIB) && rm -f \
		MCP4Designa.o
SYM(MCP4Express.sy): MCP4Express.d SYM(MCP4Attribu.sy); $(M2C) $(MFLAGS) \
		MCP4Express.d
$(LIB)(MCP4Express.o): SYM(MCMnemonics.sy) SYM(MCP4Global.sy) \
		SYM(MCP4Labels.sy) SYM(MCP4Public.sy) SYM(MCP4Registe.sy) \
		SYM(MCP4Scanner.sy) SYM(MCP4Attribu.sy) SYM(MCP4Block.sy) \
		SYM(MCP4CallSys.sy) SYM(MCP4CodeSys.sy) SYM(MCP4ConstAr.sy) \
		SYM(MCP4Designa.sy) SYM(MCP4Express.sy) SYM(MCP4FPEmula.sy) \
		SYM(MCP4Load.sy) SYM(MCP4RangeCh.sy) SYM(MCP4Stack.sy) \
		SYM(MCP4Test.sy) SYM(MCP4Types.sy) SYM(MCP4Address.sy) \
		SYM(MCP4Assign.sy) MCP4Express.m2
		$(M2C) $(MFLAGS) -c MCP4Express.m2
		ar rv $(LIB) MCP4Express.o && ranlib $(LIB) && rm -f \
		MCP4Express.o
SYM(MCP4FPEmula.sy): MCP4FPEmula.d SYM(MCP4Attribu.sy); $(M2C) $(MFLAGS) \
		MCP4FPEmula.d
$(LIB)(MCP4FPEmula.o): SYM(MCMnemonics.sy) SYM(MCP4Labels.sy) \
		SYM(MCP4Registe.sy) SYM(MCP4Attribu.sy) SYM(MCP4Block.sy) \
		SYM(MCP4CodeSys.sy) SYM(MCP4Designa.sy) SYM(MCP4FPEmula.sy) \
		SYM(MCP4Load.sy) SYM(MCP4Test.sy) SYM(MCP4Address.sy) \
		MCP4FPEmula.m2
		$(M2C) $(MFLAGS) -c MCP4FPEmula.m2
		ar rv $(LIB) MCP4FPEmula.o && ranlib $(LIB) && rm -f \
		MCP4FPEmula.o
SYM(MCP4ForSys.sy): MCP4ForSys.d; $(M2C) $(MFLAGS) MCP4ForSys.d
$(LIB)(MCP4ForSys.o): SYM(MCMnemonics.sy) SYM(MCP4ForSys.sy) \
		SYM(MCP4Global.sy) SYM(MCP4Labels.sy) SYM(MCP4Registe.sy) \
		SYM(MCP4Scanner.sy) SYM(MCP4StatSys.sy) SYM(MCP4Attribu.sy) \
		SYM(MCP4CodeSys.sy) SYM(MCP4Designa.sy) SYM(MCP4Express.sy) \
		SYM(MCP4Load.sy) SYM(MCP4Stack.sy) SYM(MCP4Test.sy) \
		SYM(MCP4Types.sy) SYM(MCP4Address.sy) MCP4ForSys.m2
		$(M2C) $(MFLAGS) -c MCP4ForSys.m2
		ar rv $(LIB) MCP4ForSys.o && ranlib $(LIB) && rm -f \
		MCP4ForSys.o
SYM(MCP4Global.sy): MCP4Global.d; $(M2C) $(MFLAGS) MCP4Global.d
$(LIB)(MCP4Global.o): SYM(MCP4Global.sy) SYM(MCP4Public.sy) \
		SYM(MCP4Scanner.sy) MCP4Global.m2
		$(M2C) $(MFLAGS) -c MCP4Global.m2
		ar rv $(LIB) MCP4Global.o && ranlib $(LIB) && rm -f \
		MCP4Global.o
SYM(MCP4IfSys.sy): MCP4IfSys.d; $(M2C) $(MFLAGS) MCP4IfSys.d
$(LIB)(MCP4IfSys.o): SYM(MCMnemonics.sy) SYM(MCP4IfSys.sy) \
		SYM(MCP4Labels.sy) SYM(MCP4Registe.sy) SYM(MCP4Scanner.sy) \
		SYM(MCP4StatSys.sy) SYM(MCP4Attribu.sy) SYM(MCP4CodeSys.sy) \
		SYM(MCP4Express.sy) SYM(MCP4Load.sy) SYM(MCP4Test.sy) \
		MCP4IfSys.m2
		$(M2C) $(MFLAGS) -c MCP4IfSys.m2
		ar rv $(LIB) MCP4IfSys.o && ranlib $(LIB) && rm -f \
		MCP4IfSys.o
SYM(MCP4In.sy):	MCP4In.d; $(M2C) $(MFLAGS) MCP4In.d
$(LIB)(MCP4In.o): SYM(MCP4In.sy) SYM(MCP4Public.sy) MCP4In.m2
		$(M2C) $(MFLAGS) -c MCP4In.m2
		ar rv $(LIB) MCP4In.o && ranlib $(LIB) && rm -f MCP4In.o
SYM(MCP4Init.sy): MCP4Init.d; $(M2C) $(MFLAGS) MCP4Init.d
$(LIB)(MCP4Init.o): SYM(MCP4Global.sy) SYM(MCP4Init.sy) SYM(MCP4Public.sy) \
		SYM(MCP4RTS.sy) SYM(MCP4CodeSys.sy) MCP4Init.m2
		$(M2C) $(MFLAGS) -c MCP4Init.m2
		ar rv $(LIB) MCP4Init.o && ranlib $(LIB) && rm -f MCP4Init.o
SYM(MCP4Labels.sy): MCP4Labels.d; $(M2C) $(MFLAGS) MCP4Labels.d
$(LIB)(MCP4Labels.o): SYM(MCP4Labels.sy) SYM(MCP4Public.sy) MCP4Labels.m2
		$(M2C) $(MFLAGS) -c MCP4Labels.m2
		ar rv $(LIB) MCP4Labels.o && ranlib $(LIB) && rm -f \
		MCP4Labels.o
SYM(MCP4Load.sy): MCP4Load.d SYM(MCP4Registe.sy) SYM(MCP4Attribu.sy); \
		$(M2C) $(MFLAGS) MCP4Load.d
$(LIB)(MCP4Load.o): SYM(MCMnemonics.sy) SYM(MCP4Global.sy) \
		SYM(MCP4Labels.sy) SYM(MCP4Public.sy) SYM(MCP4RTS.sy) \
		SYM(MCP4Registe.sy) SYM(MCP4Attribu.sy) SYM(MCP4Block.sy) \
		SYM(MCP4CodeSys.sy) SYM(MCP4FPEmula.sy) SYM(MCP4Load.sy) \
		SYM(MCP4Stack.sy) SYM(MCP4Test.sy) SYM(MCP4Types.sy) \
		SYM(MCP4Address.sy) SYM(MCP4Assign.sy) MCP4Load.m2
		$(M2C) $(MFLAGS) -c MCP4Load.m2
		ar rv $(LIB) MCP4Load.o && ranlib $(LIB) && rm -f MCP4Load.o
SYM(MCP4LoopSys.sy): MCP4LoopSys.d; $(M2C) $(MFLAGS) MCP4LoopSys.d
$(LIB)(MCP4LoopSys.o): SYM(MCMnemonics.sy) SYM(MCP4Global.sy) \
		SYM(MCP4Labels.sy) SYM(MCP4LoopSys.sy) SYM(MCP4Registe.sy) \
		SYM(MCP4Scanner.sy) SYM(MCP4StatSys.sy) SYM(MCP4CodeSys.sy) \
		MCP4LoopSys.m2
		$(M2C) $(MFLAGS) -c MCP4LoopSys.m2
		ar rv $(LIB) MCP4LoopSys.o && ranlib $(LIB) && rm -f \
		MCP4LoopSys.o
SYM(MCP4Out.sy): MCP4Out.d; $(M2C) $(MFLAGS) MCP4Out.d
$(LIB)(MCP4Out.o): SYM(MCP4Out.sy) SYM(MCP4Public.sy) MCP4Out.m2
		$(M2C) $(MFLAGS) -c MCP4Out.m2
		ar rv $(LIB) MCP4Out.o && ranlib $(LIB) && rm -f MCP4Out.o
SYM(MCP4Public.sy): MCP4Public.d; $(M2C) $(MFLAGS) MCP4Public.d
$(LIB)(MCP4Public.o): SYM(MCP4Public.sy) MCP4Public.m2
		$(M2C) $(MFLAGS) -c MCP4Public.m2
		ar rv $(LIB) MCP4Public.o && ranlib $(LIB) && rm -f \
		MCP4Public.o
SYM(MCP4RTS.sy): MCP4RTS.d; $(M2C) $(MFLAGS) MCP4RTS.d
$(LIB)(MCP4RTS.o): SYM(MCP4RTS.sy) MCP4RTS.m2
		$(M2C) $(MFLAGS) -c MCP4RTS.m2
		ar rv $(LIB) MCP4RTS.o && ranlib $(LIB) && rm -f MCP4RTS.o
SYM(MCP4RangeCh.sy): MCP4RangeCh.d SYM(MCP4Attribu.sy); $(M2C) $(MFLAGS) \
		MCP4RangeCh.d
$(LIB)(MCP4RangeCh.o): SYM(MCMnemonics.sy) SYM(MCP4Global.sy) \
		SYM(MCP4Labels.sy) SYM(MCP4Public.sy) SYM(MCP4RTS.sy) \
		SYM(MCP4Registe.sy) SYM(MCP4Attribu.sy) SYM(MCP4CodeSys.sy) \
		SYM(MCP4Load.sy) SYM(MCP4RangeCh.sy) SYM(MCP4Stack.sy) \
		SYM(MCP4Types.sy) MCP4RangeCh.m2
		$(M2C) $(MFLAGS) -c MCP4RangeCh.m2
		ar rv $(LIB) MCP4RangeCh.o && ranlib $(LIB) && rm -f \
		MCP4RangeCh.o
SYM(MCP4Registe.sy): MCP4Registe.d; $(M2C) $(MFLAGS) MCP4Registe.d
$(LIB)(MCP4Registe.o): SYM(MCMnemonics.sy) SYM(MCP4Global.sy) \
		SYM(MCP4Public.sy) SYM(MCP4Registe.sy) SYM(MCP4CodeSys.sy) \
		SYM(MCP4Stack.sy) MCP4Registe.m2
		$(M2C) $(MFLAGS) -c MCP4Registe.m2
		ar rv $(LIB) MCP4Registe.o && ranlib $(LIB) && rm -f \
		MCP4Registe.o
SYM(MCP4RepeatS.sy): MCP4RepeatS.d; $(M2C) $(MFLAGS) MCP4RepeatS.d
$(LIB)(MCP4RepeatS.o): SYM(MCP4Labels.sy) SYM(MCP4Registe.sy) \
		SYM(MCP4RepeatS.sy) SYM(MCP4Scanner.sy) SYM(MCP4StatSys.sy) \
		SYM(MCP4Attribu.sy) SYM(MCP4CodeSys.sy) SYM(MCP4Express.sy) \
		SYM(MCP4Load.sy) SYM(MCP4Test.sy) MCP4RepeatS.m2
		$(M2C) $(MFLAGS) -c MCP4RepeatS.m2
		ar rv $(LIB) MCP4RepeatS.o && ranlib $(LIB) && rm -f \
		MCP4RepeatS.o
SYM(MCP4Scanner.sy): MCP4Scanner.d; $(M2C) $(MFLAGS) MCP4Scanner.d
$(LIB)(MCP4Scanner.o): SYM(MCP4In.sy) SYM(MCP4Public.sy) SYM(MCP4Scanner.sy) \
		SYM(MCP4CodeSys.sy) MCP4Scanner.m2
		$(M2C) $(MFLAGS) -c MCP4Scanner.m2
		ar rv $(LIB) MCP4Scanner.o && ranlib $(LIB) && rm -f \
		MCP4Scanner.o
SYM(MCP4SetCons.sy): MCP4SetCons.d; $(M2C) $(MFLAGS) MCP4SetCons.d
$(LIB)(MCP4SetCons.o): SYM(MCP4Labels.sy) SYM(MCP4SetCons.sy) \
		SYM(MCP4CodeSys.sy) MCP4SetCons.m2
		$(M2C) $(MFLAGS) -c MCP4SetCons.m2
		ar rv $(LIB) MCP4SetCons.o && ranlib $(LIB) && rm -f \
		MCP4SetCons.o
SYM(MCP4Stack.sy): MCP4Stack.d SYM(MCP4Attribu.sy); $(M2C) $(MFLAGS) \
		MCP4Stack.d
$(LIB)(MCP4Stack.o): SYM(MCP4Global.sy) SYM(MCP4Attribu.sy) \
		SYM(MCP4Stack.sy) MCP4Stack.m2
		$(M2C) $(MFLAGS) -c MCP4Stack.m2
		ar rv $(LIB) MCP4Stack.o && ranlib $(LIB) && rm -f \
		MCP4Stack.o
SYM(MCP4StatSys.sy): MCP4StatSys.d; $(M2C) $(MFLAGS) MCP4StatSys.d
$(LIB)(MCP4StatSys.o): SYM(MCMnemonics.sy) SYM(MCP4CaseSys.sy) \
		SYM(MCP4ForSys.sy) SYM(MCP4IfSys.sy) SYM(MCP4Labels.sy) \
		SYM(MCP4LoopSys.sy) SYM(MCP4Public.sy) SYM(MCP4Registe.sy) \
		SYM(MCP4RepeatS.sy) SYM(MCP4Scanner.sy) SYM(MCP4StatSys.sy) \
		SYM(MCP4WhileSy.sy) SYM(MCP4Attribu.sy) SYM(MCP4CallSys.sy) \
		SYM(MCP4CodeSys.sy) SYM(MCP4Designa.sy) SYM(MCP4Express.sy) \
		SYM(MCP4FPEmula.sy) SYM(MCP4Load.sy) SYM(MCP4Types.sy) \
		SYM(MCP4WithSys.sy) SYM(MCP4Assign.sy) MCP4StatSys.m2
		$(M2C) $(MFLAGS) -c MCP4StatSys.m2
		ar rv $(LIB) MCP4StatSys.o && ranlib $(LIB) && rm -f \
		MCP4StatSys.o
SYM(MCP4Strings.sy): MCP4Strings.d; $(M2C) $(MFLAGS) MCP4Strings.d
$(LIB)(MCP4Strings.o): SYM(MCP4Labels.sy) SYM(MCP4Strings.sy) \
		SYM(MCP4CodeSys.sy) MCP4Strings.m2
		$(M2C) $(MFLAGS) -c MCP4Strings.m2
		ar rv $(LIB) MCP4Strings.o && ranlib $(LIB) && rm -f \
		MCP4Strings.o
SYM(MCP4Test.sy): MCP4Test.d SYM(MCP4Labels.sy) SYM(MCP4Attribu.sy); \
		$(M2C) $(MFLAGS) MCP4Test.d
$(LIB)(MCP4Test.o): SYM(MCMnemonics.sy) SYM(MCP4Labels.sy) \
		SYM(MCP4Registe.sy) SYM(MCP4Attribu.sy) SYM(MCP4CodeSys.sy) \
		SYM(MCP4Test.sy) MCP4Test.m2
		$(M2C) $(MFLAGS) -c MCP4Test.m2
		ar rv $(LIB) MCP4Test.o && ranlib $(LIB) && rm -f MCP4Test.o
SYM(MCP4Types.sy): MCP4Types.d SYM(MCP4Attribu.sy); $(M2C) $(MFLAGS) \
		MCP4Types.d
$(LIB)(MCP4Types.o): SYM(MCP4Global.sy) SYM(MCP4Attribu.sy) \
		SYM(MCP4Types.sy) MCP4Types.m2
		$(M2C) $(MFLAGS) -c MCP4Types.m2
		ar rv $(LIB) MCP4Types.o && ranlib $(LIB) && rm -f \
		MCP4Types.o
SYM(MCP4WhileSy.sy): MCP4WhileSy.d; $(M2C) $(MFLAGS) MCP4WhileSy.d
$(LIB)(MCP4WhileSy.o): SYM(MCMnemonics.sy) SYM(MCP4Labels.sy) \
		SYM(MCP4Registe.sy) SYM(MCP4Scanner.sy) SYM(MCP4StatSys.sy) \
		SYM(MCP4WhileSy.sy) SYM(MCP4Attribu.sy) SYM(MCP4Block.sy) \
		SYM(MCP4CodeSys.sy) SYM(MCP4Express.sy) SYM(MCP4Load.sy) \
		SYM(MCP4Test.sy) MCP4WhileSy.m2
		$(M2C) $(MFLAGS) -c MCP4WhileSy.m2
		ar rv $(LIB) MCP4WhileSy.o && ranlib $(LIB) && rm -f \
		MCP4WhileSy.o
SYM(MCP4WithSys.sy): MCP4WithSys.d SYM(MCP4Attribu.sy); $(M2C) $(MFLAGS) \
		MCP4WithSys.d
$(LIB)(MCP4WithSys.o): SYM(MCMnemonics.sy) SYM(MCP4Global.sy) \
		SYM(MCP4Registe.sy) SYM(MCP4Scanner.sy) SYM(MCP4StatSys.sy) \
		SYM(MCP4Attribu.sy) SYM(MCP4CodeSys.sy) SYM(MCP4Designa.sy) \
		SYM(MCP4Load.sy) SYM(MCP4Stack.sy) SYM(MCP4WithSys.sy) \
		SYM(MCP4Address.sy) MCP4WithSys.m2
		$(M2C) $(MFLAGS) -c MCP4WithSys.m2
		ar rv $(LIB) MCP4WithSys.o && ranlib $(LIB) && rm -f \
		MCP4WithSys.o
MCPass4.o:	SYM(MCP4In.sy) SYM(MCP4Init.sy) SYM(MCP4Public.sy) \
		SYM(MCP4SetCons.sy) SYM(MCP4Strings.sy) SYM(MCP4Block.sy) \
		SYM(MCP4CodeSys.sy)
