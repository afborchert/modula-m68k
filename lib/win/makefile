# {updated by mmm: Tue Feb 02 22:13:18 1993 GMT}
# {flags: -a+ -e- -l+ -L+ -m+ -1- -v-}
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
SRC =		Attributes.d Attributes.m2 Delay.d Delay.m2 FtdWin.d \
		FtdWin.m2 FunctionKey.d FunctionKey.m2 MainWin.d MainWin.m2 \
		Menus.d Menus.m2 Screen.d Screen.m2 TermInfo.d TermInfo.m2 \
		Windows.d Windows.m2
TAGSRC =	Attributes.m2 Delay.m2 FtdWin.m2 FunctionKey.m2 MainWin.m2 \
		Menus.m2 Screen.m2 TermInfo.m2 Windows.m2
OBJS =		
SYMS =		
REFS =		Attributes.r Delay.r FtdWin.r FunctionKey.r MainWin.r \
		Menus.r Screen.r TermInfo.r Windows.r
# {all}
LIB =		libm2.a
PLIB =		plibm2.a
all:		$(LIB) $(PLIB)
# {linkage}
LIBS =		
LDFLAGS =	
#
TermInfo.d:	TermInfo.t /usr/5include/term.h term.sh
		term.sh
# {dependencies}
$(LIB):		$(LIB)(Attributes.o) $(LIB)(Delay.o) $(LIB)(FtdWin.o) \
		$(LIB)(FunctionKey.o) $(LIB)(MainWin.o) $(LIB)(Menus.o) \
		$(LIB)(Screen.o) $(LIB)(TermInfo.o) $(LIB)(Windows.o)
$(PLIB):	$(PLIB)(Attributes.o) $(PLIB)(Delay.o) $(PLIB)(FtdWin.o) \
		$(PLIB)(FunctionKey.o) $(PLIB)(MainWin.o) $(PLIB)(Menus.o) \
		$(PLIB)(Screen.o) $(PLIB)(TermInfo.o) $(PLIB)(Windows.o)
SYM(Attributes.sy): Attributes.d /usr/local/lib/modula/SYM(StdIO.sy) \
		SYM(TermInfo.sy)
		$(M2C) $(MFLAGS) Attributes.d
$(LIB)(Attributes.o): /usr/local/lib/modula/SYM(StdIO.sy) \
		/usr/local/lib/modula/SYM(Storage.sy) SYM(TermInfo.sy) \
		SYM(Attributes.sy) SYM(Delay.sy) Attributes.m2
		$(M2C) $(MFLAGS) -c Attributes.m2
		ar rv $(LIB) Attributes.o && ranlib $(LIB) && rm -f \
		Attributes.o
$(PLIB)(Attributes.o): /usr/local/lib/modula/SYM(StdIO.sy) \
		/usr/local/lib/modula/SYM(Storage.sy) SYM(TermInfo.sy) \
		SYM(Attributes.sy) SYM(Delay.sy) Attributes.m2
		$(M2C) $(MFLAGS) -c -p Attributes.m2
		ar rv $(PLIB) Attributes.o && ranlib $(PLIB) && rm -f \
		Attributes.o
SYM(Delay.sy):	Delay.d /usr/local/lib/modula/SYM(StdIO.sy)
		$(M2C) $(MFLAGS) Delay.d
$(LIB)(Delay.o): /usr/local/lib/modula/SYM(StdIO.sy) SYM(Delay.sy) Delay.m2
		$(M2C) $(MFLAGS) -c Delay.m2
		ar rv $(LIB) Delay.o && ranlib $(LIB) && rm -f Delay.o
$(PLIB)(Delay.o): /usr/local/lib/modula/SYM(StdIO.sy) SYM(Delay.sy) Delay.m2
		$(M2C) $(MFLAGS) -c -p Delay.m2
		ar rv $(PLIB) Delay.o && ranlib $(PLIB) && rm -f Delay.o
SYM(FtdWin.sy):	FtdWin.d SYM(Windows.sy)
		$(M2C) $(MFLAGS) FtdWin.d
$(LIB)(FtdWin.o): /usr/local/lib/modula/SYM(ASCII.sy) \
		/usr/local/lib/modula/SYM(Conversions.sy) SYM(Windows.sy) \
		SYM(FtdWin.sy) FtdWin.m2
		$(M2C) $(MFLAGS) -c FtdWin.m2
		ar rv $(LIB) FtdWin.o && ranlib $(LIB) && rm -f FtdWin.o
$(PLIB)(FtdWin.o): /usr/local/lib/modula/SYM(ASCII.sy) \
		/usr/local/lib/modula/SYM(Conversions.sy) SYM(Windows.sy) \
		SYM(FtdWin.sy) FtdWin.m2
		$(M2C) $(MFLAGS) -c -p FtdWin.m2
		ar rv $(PLIB) FtdWin.o && ranlib $(PLIB) && rm -f FtdWin.o
SYM(FunctionKey.sy): FunctionKey.d /usr/local/lib/modula/SYM(StdIO.sy) \
		SYM(TermInfo.sy)
		$(M2C) $(MFLAGS) FunctionKey.d
$(LIB)(FunctionKey.o): /usr/local/lib/modula/SYM(StdIO.sy) \
		/usr/local/lib/modula/SYM(Storage.sy) \
		/usr/local/lib/modula/SYM(Strings.sy) \
		/usr/local/lib/modula/SYM(SysAlarm.sy) \
		/usr/local/lib/modula/SYM(SysSignal.sy) \
		/usr/local/lib/modula/SYM(SystemTypes.sy) SYM(TermInfo.sy) \
		SYM(Delay.sy) SYM(FunctionKey.sy) FunctionKey.m2
		$(M2C) $(MFLAGS) -c FunctionKey.m2
		ar rv $(LIB) FunctionKey.o && ranlib $(LIB) && rm -f \
		FunctionKey.o
$(PLIB)(FunctionKey.o): /usr/local/lib/modula/SYM(StdIO.sy) \
		/usr/local/lib/modula/SYM(Storage.sy) \
		/usr/local/lib/modula/SYM(Strings.sy) \
		/usr/local/lib/modula/SYM(SysAlarm.sy) \
		/usr/local/lib/modula/SYM(SysSignal.sy) \
		/usr/local/lib/modula/SYM(SystemTypes.sy) SYM(TermInfo.sy) \
		SYM(Delay.sy) SYM(FunctionKey.sy) FunctionKey.m2
		$(M2C) $(MFLAGS) -c -p FunctionKey.m2
		ar rv $(PLIB) FunctionKey.o && ranlib $(PLIB) && rm -f \
		FunctionKey.o
SYM(MainWin.sy): MainWin.d SYM(Windows.sy)
		$(M2C) $(MFLAGS) MainWin.d
$(LIB)(MainWin.o): /usr/local/lib/modula/SYM(ASCII.sy) \
		/usr/local/lib/modula/SYM(FtdIO.sy) \
		/usr/local/lib/modula/SYM(StdIO.sy) \
		/usr/local/lib/modula/SYM(SysExit.sy) SYM(Windows.sy) \
		SYM(FtdWin.sy) SYM(MainWin.sy) MainWin.m2
		$(M2C) $(MFLAGS) -c MainWin.m2
		ar rv $(LIB) MainWin.o && ranlib $(LIB) && rm -f MainWin.o
$(PLIB)(MainWin.o): /usr/local/lib/modula/SYM(ASCII.sy) \
		/usr/local/lib/modula/SYM(FtdIO.sy) \
		/usr/local/lib/modula/SYM(StdIO.sy) \
		/usr/local/lib/modula/SYM(SysExit.sy) SYM(Windows.sy) \
		SYM(FtdWin.sy) SYM(MainWin.sy) MainWin.m2
		$(M2C) $(MFLAGS) -c -p MainWin.m2
		ar rv $(PLIB) MainWin.o && ranlib $(PLIB) && rm -f MainWin.o
SYM(Menus.sy):	Menus.d SYM(Windows.sy)
		$(M2C) $(MFLAGS) Menus.d
$(LIB)(Menus.o): /usr/local/lib/modula/SYM(ASCII.sy) \
		/usr/local/lib/modula/SYM(Storage.sy) \
		/usr/local/lib/modula/SYM(Strings.sy) SYM(Attributes.sy) \
		SYM(FunctionKey.sy) SYM(Windows.sy) SYM(FtdWin.sy) \
		SYM(Menus.sy) Menus.m2
		$(M2C) $(MFLAGS) -c Menus.m2
		ar rv $(LIB) Menus.o && ranlib $(LIB) && rm -f Menus.o
$(PLIB)(Menus.o): /usr/local/lib/modula/SYM(ASCII.sy) \
		/usr/local/lib/modula/SYM(Storage.sy) \
		/usr/local/lib/modula/SYM(Strings.sy) SYM(Attributes.sy) \
		SYM(FunctionKey.sy) SYM(Windows.sy) SYM(FtdWin.sy) \
		SYM(Menus.sy) Menus.m2
		$(M2C) $(MFLAGS) -c -p Menus.m2
		ar rv $(PLIB) Menus.o && ranlib $(PLIB) && rm -f Menus.o
SYM(Screen.sy):	Screen.d /usr/local/lib/modula/SYM(StdIO.sy) \
		SYM(TermInfo.sy)
		$(M2C) $(MFLAGS) Screen.d
$(LIB)(Screen.o): /usr/local/lib/modula/SYM(CallShell.sy) \
		/usr/local/lib/modula/SYM(Environment.sy) \
		/usr/local/lib/modula/SYM(StdIO.sy) \
		/usr/local/lib/modula/SYM(Storage.sy) \
		/usr/local/lib/modula/SYM(SysIoctl.sy) SYM(TermInfo.sy) \
		SYM(Delay.sy) SYM(Screen.sy) Screen.m2
		$(M2C) $(MFLAGS) -c Screen.m2
		ar rv $(LIB) Screen.o && ranlib $(LIB) && rm -f Screen.o
$(PLIB)(Screen.o): /usr/local/lib/modula/SYM(CallShell.sy) \
		/usr/local/lib/modula/SYM(Environment.sy) \
		/usr/local/lib/modula/SYM(StdIO.sy) \
		/usr/local/lib/modula/SYM(Storage.sy) \
		/usr/local/lib/modula/SYM(SysIoctl.sy) SYM(TermInfo.sy) \
		SYM(Delay.sy) SYM(Screen.sy) Screen.m2
		$(M2C) $(MFLAGS) -c -p Screen.m2
		ar rv $(PLIB) Screen.o && ranlib $(PLIB) && rm -f Screen.o
SYM(TermInfo.sy): TermInfo.d
		$(M2C) $(MFLAGS) TermInfo.d
$(LIB)(TermInfo.o): /usr/local/lib/modula/SYM(Clock.sy) \
		/usr/local/lib/modula/SYM(Conversions.sy) \
		/usr/local/lib/modula/SYM(Environment.sy) \
		/usr/local/lib/modula/SYM(StdIO.sy) \
		/usr/local/lib/modula/SYM(Strings.sy) \
		/usr/local/lib/modula/SYM(SysPanic.sy) SYM(TermInfo.sy) \
		TermInfo.m2
		$(M2C) $(MFLAGS) -c TermInfo.m2
		ar rv $(LIB) TermInfo.o && ranlib $(LIB) && rm -f TermInfo.o
$(PLIB)(TermInfo.o): /usr/local/lib/modula/SYM(Clock.sy) \
		/usr/local/lib/modula/SYM(Conversions.sy) \
		/usr/local/lib/modula/SYM(Environment.sy) \
		/usr/local/lib/modula/SYM(StdIO.sy) \
		/usr/local/lib/modula/SYM(Strings.sy) \
		/usr/local/lib/modula/SYM(SysPanic.sy) SYM(TermInfo.sy) \
		TermInfo.m2
		$(M2C) $(MFLAGS) -c -p TermInfo.m2
		ar rv $(PLIB) TermInfo.o && ranlib $(PLIB) && rm -f \
		TermInfo.o
SYM(Windows.sy): Windows.d /usr/local/lib/modula/SYM(StdIO.sy) \
		SYM(TermInfo.sy) SYM(Attributes.sy) SYM(FunctionKey.sy) \
		SYM(Screen.sy)
		$(M2C) $(MFLAGS) Windows.d
$(LIB)(Windows.o): /usr/local/lib/modula/SYM(ASCII.sy) \
		/usr/local/lib/modula/SYM(Environment.sy) \
		/usr/local/lib/modula/SYM(FtdIO.sy) \
		/usr/local/lib/modula/SYM(StdIO.sy) \
		/usr/local/lib/modula/SYM(Storage.sy) \
		/usr/local/lib/modula/SYM(StrToNum.sy) \
		/usr/local/lib/modula/SYM(SysFcntl.sy) \
		/usr/local/lib/modula/SYM(SysIoctl.sy) \
		/usr/local/lib/modula/SYM(SysTermIO.sy) \
		/usr/local/lib/modula/SYM(SystemTypes.sy) SYM(TermInfo.sy) \
		SYM(Attributes.sy) SYM(FunctionKey.sy) SYM(Screen.sy) \
		SYM(Windows.sy) Windows.m2
		$(M2C) $(MFLAGS) -c Windows.m2
		ar rv $(LIB) Windows.o && ranlib $(LIB) && rm -f Windows.o
$(PLIB)(Windows.o): /usr/local/lib/modula/SYM(ASCII.sy) \
		/usr/local/lib/modula/SYM(Environment.sy) \
		/usr/local/lib/modula/SYM(FtdIO.sy) \
		/usr/local/lib/modula/SYM(StdIO.sy) \
		/usr/local/lib/modula/SYM(Storage.sy) \
		/usr/local/lib/modula/SYM(StrToNum.sy) \
		/usr/local/lib/modula/SYM(SysFcntl.sy) \
		/usr/local/lib/modula/SYM(SysIoctl.sy) \
		/usr/local/lib/modula/SYM(SysTermIO.sy) \
		/usr/local/lib/modula/SYM(SystemTypes.sy) SYM(TermInfo.sy) \
		SYM(Attributes.sy) SYM(FunctionKey.sy) SYM(Screen.sy) \
		SYM(Windows.sy) Windows.m2
		$(M2C) $(MFLAGS) -c -p Windows.m2
		ar rv $(PLIB) Windows.o && ranlib $(PLIB) && rm -f Windows.o
#
tags:		$(TAGSRC)
		mtags $(TAGSRC)
#
.PRECIOUS:	Pr.date
SPOOL =		lp
pr:		Pr.date
Pr.date:	$(SRC)
		pr $? | $(SPOOL)
		-touch Pr.date
#
clean:		; rm -f $(OBJS) $(SYMS) $(REFS)
