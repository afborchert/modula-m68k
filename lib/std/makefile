# {updated by mmm: Tue Feb 02 22:16:00 1993 GMT}
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
SRC =		ASCII.d ASCII.m2 Archive.d Archive.m2 Arguments.d \
		Arguments.m2 Calendar.d Calendar.m2 CallShell.d CallShell.m2 \
		Clock.d Clock.m2 CoExpressio.d CoExpressio.m2 Conversions.d \
		Conversions.m2 Directories.d Directories.m2 Environment.d \
		Environment.m2 EtcGroup.d EtcGroup.m2 Files.d Files.m2 \
		FtdIO.d FtdIO.m2 Functions.d Functions.m2 GetPass.d \
		GetPass.m2 InOut.d InOut.m2 MathLib.d MathLib1.d Passwd.d \
		Passwd.m2 PipeIO.d PipeIO.m2 Plot.d Plot.m2 PrioTab.d \
		PrioTab.m2 Processes.d Processes.m2 RandomGener.d \
		RandomGener.m2 ReadIntCard.d ReadIntCard.m2 RealConv.d \
		RealConv.m2 RealInOut.d RealInOut.m2 ScanPwfile.d \
		ScanPwfile.m2 StdFuncs.d StdFuncs.m2 StdIO.d StdIO.m2 \
		Storage.d Storage.m2 StrSpec.d StrSpec.m2 StrToNum.d \
		StrToNum.m2 Strings.d Strings.m2 SysPerror.d SysPerror.m2 \
		SysSema.d SysSema.m2 Terminal.d Terminal.m2 TimeIO.d \
		TimeIO.m2
TAGSRC =	ASCII.m2 Archive.m2 Arguments.m2 Calendar.m2 CallShell.m2 \
		Clock.m2 CoExpressio.m2 Conversions.m2 Directories.m2 \
		Environment.m2 EtcGroup.m2 Files.m2 FtdIO.m2 Functions.m2 \
		GetPass.m2 InOut.m2 Passwd.m2 PipeIO.m2 Plot.m2 PrioTab.m2 \
		Processes.m2 RandomGener.m2 ReadIntCard.m2 RealConv.m2 \
		RealInOut.m2 ScanPwfile.m2 StdFuncs.m2 StdIO.m2 Storage.m2 \
		StrSpec.m2 StrToNum.m2 Strings.m2 SysPerror.m2 SysSema.m2 \
		Terminal.m2 TimeIO.m2
OBJS =		
SYMS =		
REFS =		ASCII.r Archive.r Arguments.r Calendar.r CallShell.r Clock.r \
		CoExpressio.r Conversions.r Directories.r Environment.r \
		EtcGroup.r Files.r FtdIO.r Functions.r GetPass.r InOut.r \
		Passwd.r PipeIO.r Plot.r PrioTab.r Processes.r RandomGener.r \
		ReadIntCard.r RealConv.r RealInOut.r ScanPwfile.r StdFuncs.r \
		StdIO.r Storage.r StrSpec.r StrToNum.r Strings.r SysPerror.r \
		SysSema.r Terminal.r TimeIO.r
# {precious}
LIB =		libm2.a
PLIB =		plibm2.a
.PRECIOUS:	$(LIB) $(PLIB) SYM
# {all}
all:		SYM(MathLib.sy) SYM(MathLib1.sy) $(LIB) $(PLIB)
# {linkage}
LIBS =		
LDFLAGS =	
# {dependencies}
$(LIB):		$(LIB)(ASCII.o) $(LIB)(Archive.o) $(LIB)(Arguments.o) \
		$(LIB)(Calendar.o) $(LIB)(CallShell.o) $(LIB)(Clock.o) \
		$(LIB)(CoExpressio.o) $(LIB)(Conversions.o) \
		$(LIB)(Directories.o) $(LIB)(Environment.o) \
		$(LIB)(EtcGroup.o) $(LIB)(Files.o) $(LIB)(FtdIO.o) \
		$(LIB)(Functions.o) $(LIB)(GetPass.o) $(LIB)(InOut.o) \
		$(LIB)(Passwd.o) $(LIB)(PipeIO.o) $(LIB)(Plot.o) \
		$(LIB)(PrioTab.o) $(LIB)(Processes.o) $(LIB)(RandomGener.o) \
		$(LIB)(ReadIntCard.o) $(LIB)(RealConv.o) $(LIB)(RealInOut.o) \
		$(LIB)(ScanPwfile.o) $(LIB)(StdFuncs.o) $(LIB)(StdIO.o) \
		$(LIB)(Storage.o) $(LIB)(StrSpec.o) $(LIB)(StrToNum.o) \
		$(LIB)(Strings.o) $(LIB)(SysPerror.o) $(LIB)(SysSema.o) \
		$(LIB)(Terminal.o) $(LIB)(TimeIO.o)
$(PLIB):	$(PLIB)(ASCII.o) $(PLIB)(Archive.o) $(PLIB)(Arguments.o) \
		$(PLIB)(Calendar.o) $(PLIB)(CallShell.o) $(PLIB)(Clock.o) \
		$(PLIB)(CoExpressio.o) $(PLIB)(Conversions.o) \
		$(PLIB)(Directories.o) $(PLIB)(Environment.o) \
		$(PLIB)(EtcGroup.o) $(PLIB)(Files.o) $(PLIB)(FtdIO.o) \
		$(PLIB)(Functions.o) $(PLIB)(GetPass.o) $(PLIB)(InOut.o) \
		$(PLIB)(Passwd.o) $(PLIB)(PipeIO.o) $(PLIB)(Plot.o) \
		$(PLIB)(PrioTab.o) $(PLIB)(Processes.o) \
		$(PLIB)(RandomGener.o) $(PLIB)(ReadIntCard.o) \
		$(PLIB)(RealConv.o) $(PLIB)(RealInOut.o) \
		$(PLIB)(ScanPwfile.o) $(PLIB)(StdFuncs.o) $(PLIB)(StdIO.o) \
		$(PLIB)(Storage.o) $(PLIB)(StrSpec.o) $(PLIB)(StrToNum.o) \
		$(PLIB)(Strings.o) $(PLIB)(SysPerror.o) $(PLIB)(SysSema.o) \
		$(PLIB)(Terminal.o) $(PLIB)(TimeIO.o)
SYM(ASCII.sy):	ASCII.d
		$(M2C) $(MFLAGS) ASCII.d
$(LIB)(ASCII.o): SYM(ASCII.sy) ASCII.m2
		$(M2C) $(MFLAGS) -c ASCII.m2
		ar rv $(LIB) ASCII.o && ranlib $(LIB) && rm -f ASCII.o
$(PLIB)(ASCII.o): SYM(ASCII.sy) ASCII.m2
		$(M2C) $(MFLAGS) -c -p ASCII.m2
		ar rv $(PLIB) ASCII.o && ranlib $(PLIB) && rm -f ASCII.o
SYM(Archive.sy): Archive.d /usr/local/lib/modula/SYM(SystemTypes.sy)
		$(M2C) $(MFLAGS) Archive.d
$(LIB)(Archive.o): SYM(ASCII.sy) SYM(Storage.sy) SYM(StrToNum.sy) \
		SYM(Strings.sy) /usr/local/lib/modula/SYM(SysPanic.sy) \
		/usr/local/lib/modula/SYM(SystemTypes.sy) SYM(Archive.sy) \
		SYM(StdIO.sy) SYM(FtdIO.sy) Archive.m2
		$(M2C) $(MFLAGS) -c Archive.m2
		ar rv $(LIB) Archive.o && ranlib $(LIB) && rm -f Archive.o
$(PLIB)(Archive.o): SYM(ASCII.sy) SYM(Storage.sy) SYM(StrToNum.sy) \
		SYM(Strings.sy) /usr/local/lib/modula/SYM(SysPanic.sy) \
		/usr/local/lib/modula/SYM(SystemTypes.sy) SYM(Archive.sy) \
		SYM(StdIO.sy) SYM(FtdIO.sy) Archive.m2
		$(M2C) $(MFLAGS) -c -p Archive.m2
		ar rv $(PLIB) Archive.o && ranlib $(PLIB) && rm -f Archive.o
SYM(Arguments.sy): Arguments.d
		$(M2C) $(MFLAGS) Arguments.d
$(LIB)(Arguments.o): SYM(ASCII.sy) SYM(Arguments.sy) SYM(StrSpec.sy) \
		SYM(StrToNum.sy) SYM(Strings.sy) \
		/usr/local/lib/modula/SYM(SysExit.sy) SYM(StdIO.sy) \
		Arguments.m2
		$(M2C) $(MFLAGS) -c Arguments.m2
		ar rv $(LIB) Arguments.o && ranlib $(LIB) && rm -f \
		Arguments.o
$(PLIB)(Arguments.o): SYM(ASCII.sy) SYM(Arguments.sy) SYM(StrSpec.sy) \
		SYM(StrToNum.sy) SYM(Strings.sy) \
		/usr/local/lib/modula/SYM(SysExit.sy) SYM(StdIO.sy) \
		Arguments.m2
		$(M2C) $(MFLAGS) -c -p Arguments.m2
		ar rv $(PLIB) Arguments.o && ranlib $(PLIB) && rm -f \
		Arguments.o
SYM(Calendar.sy): Calendar.d /usr/local/lib/modula/SYM(SystemTypes.sy)
		$(M2C) $(MFLAGS) Calendar.d
$(LIB)(Calendar.o): SYM(Environment.sy) SYM(Strings.sy) \
		/usr/local/lib/modula/SYM(SysTime.sy) SYM(Calendar.sy) \
		Calendar.m2
		$(M2C) $(MFLAGS) -c Calendar.m2
		ar rv $(LIB) Calendar.o && ranlib $(LIB) && rm -f Calendar.o
$(PLIB)(Calendar.o): SYM(Environment.sy) SYM(Strings.sy) \
		/usr/local/lib/modula/SYM(SysTime.sy) SYM(Calendar.sy) \
		Calendar.m2
		$(M2C) $(MFLAGS) -c -p Calendar.m2
		ar rv $(PLIB) Calendar.o && ranlib $(PLIB) && rm -f \
		Calendar.o
SYM(CallShell.sy): CallShell.d
		$(M2C) $(MFLAGS) CallShell.d
$(LIB)(CallShell.o): SYM(CallShell.sy) /usr/local/lib/modula/SYM(SysExec.sy) \
		/usr/local/lib/modula/SYM(SysExit.sy) \
		/usr/local/lib/modula/SYM(SysFork.sy) \
		/usr/local/lib/modula/SYM(SysSignal.sy) \
		/usr/local/lib/modula/SYM(SysWait.sy) \
		/usr/local/lib/modula/SYM(SystemTypes.sy) \
		/usr/local/lib/modula/SYM(UnixString.sy) CallShell.m2
		$(M2C) $(MFLAGS) -c CallShell.m2
		ar rv $(LIB) CallShell.o && ranlib $(LIB) && rm -f \
		CallShell.o
$(PLIB)(CallShell.o): SYM(CallShell.sy) \
		/usr/local/lib/modula/SYM(SysExec.sy) \
		/usr/local/lib/modula/SYM(SysExit.sy) \
		/usr/local/lib/modula/SYM(SysFork.sy) \
		/usr/local/lib/modula/SYM(SysSignal.sy) \
		/usr/local/lib/modula/SYM(SysWait.sy) \
		/usr/local/lib/modula/SYM(SystemTypes.sy) \
		/usr/local/lib/modula/SYM(UnixString.sy) CallShell.m2
		$(M2C) $(MFLAGS) -c -p CallShell.m2
		ar rv $(PLIB) CallShell.o && ranlib $(PLIB) && rm -f \
		CallShell.o
SYM(Clock.sy):	Clock.d /usr/local/lib/modula/SYM(SystemTypes.sy)
		$(M2C) $(MFLAGS) Clock.d
$(LIB)(Clock.o): /usr/local/lib/modula/SYM(Sys.sy) \
		/usr/local/lib/modula/SYM(SysTime.sy) \
		/usr/local/lib/modula/SYM(SystemTypes.sy) SYM(Clock.sy) \
		Clock.m2
		$(M2C) $(MFLAGS) -c Clock.m2
		ar rv $(LIB) Clock.o && ranlib $(LIB) && rm -f Clock.o
$(PLIB)(Clock.o): /usr/local/lib/modula/SYM(Sys.sy) \
		/usr/local/lib/modula/SYM(SysTime.sy) \
		/usr/local/lib/modula/SYM(SystemTypes.sy) SYM(Clock.sy) \
		Clock.m2
		$(M2C) $(MFLAGS) -c -p Clock.m2
		ar rv $(PLIB) Clock.o && ranlib $(PLIB) && rm -f Clock.o
SYM(CoExpressio.sy): CoExpressio.d
		$(M2C) $(MFLAGS) CoExpressio.d
$(LIB)(CoExpressio.o): SYM(CoExpressio.sy) SYM(Storage.sy) CoExpressio.m2
		$(M2C) $(MFLAGS) -c CoExpressio.m2
		ar rv $(LIB) CoExpressio.o && ranlib $(LIB) && rm -f \
		CoExpressio.o
$(PLIB)(CoExpressio.o): SYM(CoExpressio.sy) SYM(Storage.sy) CoExpressio.m2
		$(M2C) $(MFLAGS) -c -p CoExpressio.m2
		ar rv $(PLIB) CoExpressio.o && ranlib $(PLIB) && rm -f \
		CoExpressio.o
SYM(Conversions.sy): Conversions.d
		$(M2C) $(MFLAGS) Conversions.d
$(LIB)(Conversions.o): SYM(Conversions.sy) Conversions.m2
		$(M2C) $(MFLAGS) -c Conversions.m2
		ar rv $(LIB) Conversions.o && ranlib $(LIB) && rm -f \
		Conversions.o
$(PLIB)(Conversions.o): SYM(Conversions.sy) Conversions.m2
		$(M2C) $(MFLAGS) -c -p Conversions.m2
		ar rv $(PLIB) Conversions.o && ranlib $(PLIB) && rm -f \
		Conversions.o
SYM(Directories.sy): Directories.d /usr/local/lib/modula/SYM(SystemTypes.sy)
		$(M2C) $(MFLAGS) Directories.d
$(LIB)(Directories.o): SYM(Storage.sy) SYM(Strings.sy) \
		/usr/local/lib/modula/SYM(Sys.sy) \
		/usr/local/lib/modula/SYM(SysClose.sy) \
		/usr/local/lib/modula/SYM(SysLseek.sy) \
		/usr/local/lib/modula/SYM(SysOpen.sy) \
		/usr/local/lib/modula/SYM(SysStat.sy) \
		/usr/local/lib/modula/SYM(SystemTypes.sy) \
		SYM(Directories.sy) Directories.m2
		$(M2C) $(MFLAGS) -c Directories.m2
		ar rv $(LIB) Directories.o && ranlib $(LIB) && rm -f \
		Directories.o
$(PLIB)(Directories.o): SYM(Storage.sy) SYM(Strings.sy) \
		/usr/local/lib/modula/SYM(Sys.sy) \
		/usr/local/lib/modula/SYM(SysClose.sy) \
		/usr/local/lib/modula/SYM(SysLseek.sy) \
		/usr/local/lib/modula/SYM(SysOpen.sy) \
		/usr/local/lib/modula/SYM(SysStat.sy) \
		/usr/local/lib/modula/SYM(SystemTypes.sy) \
		SYM(Directories.sy) Directories.m2
		$(M2C) $(MFLAGS) -c -p Directories.m2
		ar rv $(PLIB) Directories.o && ranlib $(PLIB) && rm -f \
		Directories.o
SYM(Environment.sy): Environment.d
		$(M2C) $(MFLAGS) Environment.d
$(LIB)(Environment.o): SYM(Environment.sy) \
		/usr/local/lib/modula/SYM(SysLocation.sy) Environment.m2
		$(M2C) $(MFLAGS) -c Environment.m2
		ar rv $(LIB) Environment.o && ranlib $(LIB) && rm -f \
		Environment.o
$(PLIB)(Environment.o): SYM(Environment.sy) \
		/usr/local/lib/modula/SYM(SysLocation.sy) Environment.m2
		$(M2C) $(MFLAGS) -c -p Environment.m2
		ar rv $(PLIB) Environment.o && ranlib $(PLIB) && rm -f \
		Environment.o
SYM(EtcGroup.sy): EtcGroup.d
		$(M2C) $(MFLAGS) EtcGroup.d
$(LIB)(EtcGroup.o): SYM(EtcGroup.sy) SYM(Strings.sy) SYM(StdIO.sy) \
		SYM(ScanPwfile.sy) EtcGroup.m2
		$(M2C) $(MFLAGS) -c EtcGroup.m2
		ar rv $(LIB) EtcGroup.o && ranlib $(LIB) && rm -f EtcGroup.o
$(PLIB)(EtcGroup.o): SYM(EtcGroup.sy) SYM(Strings.sy) SYM(StdIO.sy) \
		SYM(ScanPwfile.sy) EtcGroup.m2
		$(M2C) $(MFLAGS) -c -p EtcGroup.m2
		ar rv $(PLIB) EtcGroup.o && ranlib $(PLIB) && rm -f \
		EtcGroup.o
SYM(Files.sy):	Files.d /usr/local/lib/modula/SYM(SystemTypes.sy) \
		SYM(StdIO.sy)
		$(M2C) $(MFLAGS) Files.d
$(LIB)(Files.o): /usr/local/lib/modula/SYM(SysLink.sy) \
		/usr/local/lib/modula/SYM(SysUnlink.sy) \
		/usr/local/lib/modula/SYM(SystemTypes.sy) SYM(StdIO.sy) \
		SYM(Files.sy) Files.m2
		$(M2C) $(MFLAGS) -c Files.m2
		ar rv $(LIB) Files.o && ranlib $(LIB) && rm -f Files.o
$(PLIB)(Files.o): /usr/local/lib/modula/SYM(SysLink.sy) \
		/usr/local/lib/modula/SYM(SysUnlink.sy) \
		/usr/local/lib/modula/SYM(SystemTypes.sy) SYM(StdIO.sy) \
		SYM(Files.sy) Files.m2
		$(M2C) $(MFLAGS) -c -p Files.m2
		ar rv $(PLIB) Files.o && ranlib $(PLIB) && rm -f Files.o
SYM(FtdIO.sy):	FtdIO.d SYM(StdIO.sy)
		$(M2C) $(MFLAGS) FtdIO.d
$(LIB)(FtdIO.o): SYM(Conversions.sy) SYM(ReadIntCard.sy) SYM(StdIO.sy) \
		SYM(FtdIO.sy) FtdIO.m2
		$(M2C) $(MFLAGS) -c FtdIO.m2
		ar rv $(LIB) FtdIO.o && ranlib $(LIB) && rm -f FtdIO.o
$(PLIB)(FtdIO.o): SYM(Conversions.sy) SYM(ReadIntCard.sy) SYM(StdIO.sy) \
		SYM(FtdIO.sy) FtdIO.m2
		$(M2C) $(MFLAGS) -c -p FtdIO.m2
		ar rv $(PLIB) FtdIO.o && ranlib $(PLIB) && rm -f FtdIO.o
SYM(Functions.sy): Functions.d
		$(M2C) $(MFLAGS) Functions.d
$(LIB)(Functions.o): SYM(ASCII.sy) SYM(Functions.sy) SYM(Storage.sy) \
		SYM(Strings.sy) Functions.m2
		$(M2C) $(MFLAGS) -c Functions.m2
		ar rv $(LIB) Functions.o && ranlib $(LIB) && rm -f \
		Functions.o
$(PLIB)(Functions.o): SYM(ASCII.sy) SYM(Functions.sy) SYM(Storage.sy) \
		SYM(Strings.sy) Functions.m2
		$(M2C) $(MFLAGS) -c -p Functions.m2
		ar rv $(PLIB) Functions.o && ranlib $(PLIB) && rm -f \
		Functions.o
SYM(GetPass.sy): GetPass.d
		$(M2C) $(MFLAGS) GetPass.d
$(LIB)(GetPass.o): SYM(ASCII.sy) SYM(GetPass.sy) SYM(RandomGener.sy) \
		/usr/local/lib/modula/SYM(SysIoctl.sy) \
		/usr/local/lib/modula/SYM(SysOpen.sy) SYM(StdIO.sy) \
		SYM(FtdIO.sy) GetPass.m2
		$(M2C) $(MFLAGS) -c GetPass.m2
		ar rv $(LIB) GetPass.o && ranlib $(LIB) && rm -f GetPass.o
$(PLIB)(GetPass.o): SYM(ASCII.sy) SYM(GetPass.sy) SYM(RandomGener.sy) \
		/usr/local/lib/modula/SYM(SysIoctl.sy) \
		/usr/local/lib/modula/SYM(SysOpen.sy) SYM(StdIO.sy) \
		SYM(FtdIO.sy) GetPass.m2
		$(M2C) $(MFLAGS) -c -p GetPass.m2
		ar rv $(PLIB) GetPass.o && ranlib $(PLIB) && rm -f GetPass.o
SYM(InOut.sy):	InOut.d
		$(M2C) $(MFLAGS) InOut.d
$(LIB)(InOut.o): SYM(Conversions.sy) SYM(InOut.sy) SYM(ReadIntCard.sy) \
		SYM(Terminal.sy) InOut.m2
		$(M2C) $(MFLAGS) -c InOut.m2
		ar rv $(LIB) InOut.o && ranlib $(LIB) && rm -f InOut.o
$(PLIB)(InOut.o): SYM(Conversions.sy) SYM(InOut.sy) SYM(ReadIntCard.sy) \
		SYM(Terminal.sy) InOut.m2
		$(M2C) $(MFLAGS) -c -p InOut.m2
		ar rv $(PLIB) InOut.o && ranlib $(PLIB) && rm -f InOut.o
SYM(MathLib.sy): MathLib.d
		$(M2C) $(MFLAGS) MathLib.d
SYM(MathLib1.sy): MathLib1.d
		$(M2C) $(MFLAGS) MathLib1.d
SYM(Passwd.sy):	Passwd.d
		$(M2C) $(MFLAGS) Passwd.d
$(LIB)(Passwd.o): SYM(Passwd.sy) SYM(Strings.sy) SYM(StdIO.sy) \
		SYM(ScanPwfile.sy) Passwd.m2
		$(M2C) $(MFLAGS) -c Passwd.m2
		ar rv $(LIB) Passwd.o && ranlib $(LIB) && rm -f Passwd.o
$(PLIB)(Passwd.o): SYM(Passwd.sy) SYM(Strings.sy) SYM(StdIO.sy) \
		SYM(ScanPwfile.sy) Passwd.m2
		$(M2C) $(MFLAGS) -c -p Passwd.m2
		ar rv $(PLIB) Passwd.o && ranlib $(PLIB) && rm -f Passwd.o
SYM(PipeIO.sy):	PipeIO.d SYM(StdIO.sy)
		$(M2C) $(MFLAGS) PipeIO.d
$(LIB)(PipeIO.o): /usr/local/lib/modula/SYM(SysClose.sy) \
		/usr/local/lib/modula/SYM(SysDup.sy) \
		/usr/local/lib/modula/SYM(SysExec.sy) \
		/usr/local/lib/modula/SYM(SysExit.sy) \
		/usr/local/lib/modula/SYM(SysFork.sy) \
		/usr/local/lib/modula/SYM(SysPipe.sy) \
		/usr/local/lib/modula/SYM(SysWait.sy) \
		/usr/local/lib/modula/SYM(UnixString.sy) SYM(StdIO.sy) \
		SYM(PipeIO.sy) PipeIO.m2
		$(M2C) $(MFLAGS) -c PipeIO.m2
		ar rv $(LIB) PipeIO.o && ranlib $(LIB) && rm -f PipeIO.o
$(PLIB)(PipeIO.o): /usr/local/lib/modula/SYM(SysClose.sy) \
		/usr/local/lib/modula/SYM(SysDup.sy) \
		/usr/local/lib/modula/SYM(SysExec.sy) \
		/usr/local/lib/modula/SYM(SysExit.sy) \
		/usr/local/lib/modula/SYM(SysFork.sy) \
		/usr/local/lib/modula/SYM(SysPipe.sy) \
		/usr/local/lib/modula/SYM(SysWait.sy) \
		/usr/local/lib/modula/SYM(UnixString.sy) SYM(StdIO.sy) \
		SYM(PipeIO.sy) PipeIO.m2
		$(M2C) $(MFLAGS) -c -p PipeIO.m2
		ar rv $(PLIB) PipeIO.o && ranlib $(PLIB) && rm -f PipeIO.o
SYM(Plot.sy):	Plot.d SYM(StdIO.sy)
		$(M2C) $(MFLAGS) Plot.d
$(LIB)(Plot.o):	SYM(SysPerror.sy) SYM(StdIO.sy) SYM(FtdIO.sy) SYM(Plot.sy) \
		Plot.m2
		$(M2C) $(MFLAGS) -c Plot.m2
		ar rv $(LIB) Plot.o && ranlib $(LIB) && rm -f Plot.o
$(PLIB)(Plot.o): SYM(SysPerror.sy) SYM(StdIO.sy) SYM(FtdIO.sy) SYM(Plot.sy) \
		Plot.m2
		$(M2C) $(MFLAGS) -c -p Plot.m2
		ar rv $(PLIB) Plot.o && ranlib $(PLIB) && rm -f Plot.o
SYM(PrioTab.sy): PrioTab.d /usr/local/lib/modula/SYM(SystemTypes.sy)
		$(M2C) $(MFLAGS) PrioTab.d
$(LIB)(PrioTab.o): /usr/local/lib/modula/SYM(SystemTypes.sy) SYM(PrioTab.sy) \
		PrioTab.m2
		$(M2C) $(MFLAGS) -c PrioTab.m2
		ar rv $(LIB) PrioTab.o && ranlib $(LIB) && rm -f PrioTab.o
$(PLIB)(PrioTab.o): /usr/local/lib/modula/SYM(SystemTypes.sy) \
		SYM(PrioTab.sy) PrioTab.m2
		$(M2C) $(MFLAGS) -c -p PrioTab.m2
		ar rv $(PLIB) PrioTab.o && ranlib $(PLIB) && rm -f PrioTab.o
SYM(Processes.sy): Processes.d
		$(M2C) $(MFLAGS) Processes.d
$(LIB)(Processes.o): SYM(Processes.sy) SYM(Storage.sy) \
		/usr/local/lib/modula/SYM(SysPanic.sy) Processes.m2
		$(M2C) $(MFLAGS) -c Processes.m2
		ar rv $(LIB) Processes.o && ranlib $(LIB) && rm -f \
		Processes.o
$(PLIB)(Processes.o): SYM(Processes.sy) SYM(Storage.sy) \
		/usr/local/lib/modula/SYM(SysPanic.sy) Processes.m2
		$(M2C) $(MFLAGS) -c -p Processes.m2
		ar rv $(PLIB) Processes.o && ranlib $(PLIB) && rm -f \
		Processes.o
SYM(RandomGener.sy): RandomGener.d
		$(M2C) $(MFLAGS) RandomGener.d
$(LIB)(RandomGener.o): SYM(RandomGener.sy) /usr/local/lib/modula/SYM(Sys.sy) \
		/usr/local/lib/modula/SYM(SysTime.sy) \
		/usr/local/lib/modula/SYM(SystemTypes.sy) RandomGener.m2
		$(M2C) $(MFLAGS) -c RandomGener.m2
		ar rv $(LIB) RandomGener.o && ranlib $(LIB) && rm -f \
		RandomGener.o
$(PLIB)(RandomGener.o): SYM(RandomGener.sy) \
		/usr/local/lib/modula/SYM(Sys.sy) \
		/usr/local/lib/modula/SYM(SysTime.sy) \
		/usr/local/lib/modula/SYM(SystemTypes.sy) RandomGener.m2
		$(M2C) $(MFLAGS) -c -p RandomGener.m2
		ar rv $(PLIB) RandomGener.o && ranlib $(PLIB) && rm -f \
		RandomGener.o
SYM(ReadIntCard.sy): ReadIntCard.d
		$(M2C) $(MFLAGS) ReadIntCard.d
$(LIB)(ReadIntCard.o): SYM(ReadIntCard.sy) ReadIntCard.m2
		$(M2C) $(MFLAGS) -c ReadIntCard.m2
		ar rv $(LIB) ReadIntCard.o && ranlib $(LIB) && rm -f \
		ReadIntCard.o
$(PLIB)(ReadIntCard.o): SYM(ReadIntCard.sy) ReadIntCard.m2
		$(M2C) $(MFLAGS) -c -p ReadIntCard.m2
		ar rv $(PLIB) ReadIntCard.o && ranlib $(PLIB) && rm -f \
		ReadIntCard.o
SYM(RealConv.sy): RealConv.d
		$(M2C) $(MFLAGS) RealConv.d
$(LIB)(RealConv.o): SYM(RealConv.sy) RealConv.m2
		$(M2C) $(MFLAGS) -c RealConv.m2
		ar rv $(LIB) RealConv.o && ranlib $(LIB) && rm -f RealConv.o
$(PLIB)(RealConv.o): SYM(RealConv.sy) RealConv.m2
		$(M2C) $(MFLAGS) -c -p RealConv.m2
		ar rv $(PLIB) RealConv.o && ranlib $(PLIB) && rm -f \
		RealConv.o
SYM(RealInOut.sy): RealInOut.d SYM(StdIO.sy)
		$(M2C) $(MFLAGS) RealInOut.d
$(LIB)(RealInOut.o): SYM(RealConv.sy) SYM(Strings.sy) SYM(Terminal.sy) \
		SYM(StdIO.sy) SYM(FtdIO.sy) SYM(RealInOut.sy) RealInOut.m2
		$(M2C) $(MFLAGS) -c RealInOut.m2
		ar rv $(LIB) RealInOut.o && ranlib $(LIB) && rm -f \
		RealInOut.o
$(PLIB)(RealInOut.o): SYM(RealConv.sy) SYM(Strings.sy) SYM(Terminal.sy) \
		SYM(StdIO.sy) SYM(FtdIO.sy) SYM(RealInOut.sy) RealInOut.m2
		$(M2C) $(MFLAGS) -c -p RealInOut.m2
		ar rv $(PLIB) RealInOut.o && ranlib $(PLIB) && rm -f \
		RealInOut.o
SYM(ScanPwfile.sy): ScanPwfile.d SYM(ASCII.sy) SYM(StdIO.sy)
		$(M2C) $(MFLAGS) ScanPwfile.d
$(LIB)(ScanPwfile.o): SYM(StdIO.sy) SYM(ScanPwfile.sy) ScanPwfile.m2
		$(M2C) $(MFLAGS) -c ScanPwfile.m2
		ar rv $(LIB) ScanPwfile.o && ranlib $(LIB) && rm -f \
		ScanPwfile.o
$(PLIB)(ScanPwfile.o): SYM(StdIO.sy) SYM(ScanPwfile.sy) ScanPwfile.m2
		$(M2C) $(MFLAGS) -c -p ScanPwfile.m2
		ar rv $(PLIB) ScanPwfile.o && ranlib $(PLIB) && rm -f \
		ScanPwfile.o
SYM(StdFuncs.sy): StdFuncs.d
		$(M2C) $(MFLAGS) StdFuncs.d
$(LIB)(StdFuncs.o): SYM(Functions.sy) SYM(MathLib.sy) SYM(StdFuncs.sy) \
		StdFuncs.m2
		$(M2C) $(MFLAGS) -c StdFuncs.m2
		ar rv $(LIB) StdFuncs.o && ranlib $(LIB) && rm -f StdFuncs.o
$(PLIB)(StdFuncs.o): SYM(Functions.sy) SYM(MathLib.sy) SYM(StdFuncs.sy) \
		StdFuncs.m2
		$(M2C) $(MFLAGS) -c -p StdFuncs.m2
		ar rv $(PLIB) StdFuncs.o && ranlib $(PLIB) && rm -f \
		StdFuncs.o
SYM(StdIO.sy):	StdIO.d /usr/local/lib/modula/SYM(SystemTypes.sy)
		$(M2C) $(MFLAGS) StdIO.d
$(LIB)(StdIO.o): /usr/local/lib/modula/SYM(Errno.sy) SYM(Storage.sy) \
		/usr/local/lib/modula/SYM(SysClose.sy) \
		/usr/local/lib/modula/SYM(SysCreat.sy) \
		/usr/local/lib/modula/SYM(SysExit.sy) \
		/usr/local/lib/modula/SYM(SysIoctl.sy) \
		/usr/local/lib/modula/SYM(SysLseek.sy) \
		/usr/local/lib/modula/SYM(SysOpen.sy) \
		/usr/local/lib/modula/SYM(SysRead.sy) \
		/usr/local/lib/modula/SYM(SysWrite.sy) \
		/usr/local/lib/modula/SYM(SystemTypes.sy) SYM(StdIO.sy) \
		StdIO.m2
		$(M2C) $(MFLAGS) -c StdIO.m2
		ar rv $(LIB) StdIO.o && ranlib $(LIB) && rm -f StdIO.o
$(PLIB)(StdIO.o): /usr/local/lib/modula/SYM(Errno.sy) SYM(Storage.sy) \
		/usr/local/lib/modula/SYM(SysClose.sy) \
		/usr/local/lib/modula/SYM(SysCreat.sy) \
		/usr/local/lib/modula/SYM(SysExit.sy) \
		/usr/local/lib/modula/SYM(SysIoctl.sy) \
		/usr/local/lib/modula/SYM(SysLseek.sy) \
		/usr/local/lib/modula/SYM(SysOpen.sy) \
		/usr/local/lib/modula/SYM(SysRead.sy) \
		/usr/local/lib/modula/SYM(SysWrite.sy) \
		/usr/local/lib/modula/SYM(SystemTypes.sy) SYM(StdIO.sy) \
		StdIO.m2
		$(M2C) $(MFLAGS) -c -p StdIO.m2
		ar rv $(PLIB) StdIO.o && ranlib $(PLIB) && rm -f StdIO.o
SYM(Storage.sy): Storage.d
		$(M2C) $(MFLAGS) Storage.d
$(LIB)(Storage.o): SYM(Storage.sy) /usr/local/lib/modula/SYM(SysBreak.sy) \
		/usr/local/lib/modula/SYM(SysPanic.sy) Storage.m2
		$(M2C) $(MFLAGS) -c Storage.m2
		ar rv $(LIB) Storage.o && ranlib $(LIB) && rm -f Storage.o
$(PLIB)(Storage.o): SYM(Storage.sy) /usr/local/lib/modula/SYM(SysBreak.sy) \
		/usr/local/lib/modula/SYM(SysPanic.sy) Storage.m2
		$(M2C) $(MFLAGS) -c -p Storage.m2
		ar rv $(PLIB) Storage.o && ranlib $(PLIB) && rm -f Storage.o
SYM(StrSpec.sy): StrSpec.d
		$(M2C) $(MFLAGS) StrSpec.d
$(LIB)(StrSpec.o): SYM(ASCII.sy) SYM(Storage.sy) SYM(StrSpec.sy) \
		SYM(Strings.sy) StrSpec.m2
		$(M2C) $(MFLAGS) -c StrSpec.m2
		ar rv $(LIB) StrSpec.o && ranlib $(LIB) && rm -f StrSpec.o
$(PLIB)(StrSpec.o): SYM(ASCII.sy) SYM(Storage.sy) SYM(StrSpec.sy) \
		SYM(Strings.sy) StrSpec.m2
		$(M2C) $(MFLAGS) -c -p StrSpec.m2
		ar rv $(PLIB) StrSpec.o && ranlib $(PLIB) && rm -f StrSpec.o
SYM(StrToNum.sy): StrToNum.d
		$(M2C) $(MFLAGS) StrToNum.d
$(LIB)(StrToNum.o): SYM(ASCII.sy) SYM(StrToNum.sy) StrToNum.m2
		$(M2C) $(MFLAGS) -c StrToNum.m2
		ar rv $(LIB) StrToNum.o && ranlib $(LIB) && rm -f StrToNum.o
$(PLIB)(StrToNum.o): SYM(ASCII.sy) SYM(StrToNum.sy) StrToNum.m2
		$(M2C) $(MFLAGS) -c -p StrToNum.m2
		ar rv $(PLIB) StrToNum.o && ranlib $(PLIB) && rm -f \
		StrToNum.o
SYM(Strings.sy): Strings.d
		$(M2C) $(MFLAGS) Strings.d
$(LIB)(Strings.o): SYM(Strings.sy) Strings.m2
		$(M2C) $(MFLAGS) -c Strings.m2
		ar rv $(LIB) Strings.o && ranlib $(LIB) && rm -f Strings.o
$(PLIB)(Strings.o): SYM(Strings.sy) Strings.m2
		$(M2C) $(MFLAGS) -c -p Strings.m2
		ar rv $(PLIB) Strings.o && ranlib $(PLIB) && rm -f Strings.o
SYM(SysPerror.sy): SysPerror.d
		$(M2C) $(MFLAGS) SysPerror.d
$(LIB)(SysPerror.o): /usr/local/lib/modula/SYM(Errno.sy) SYM(SysPerror.sy) \
		SYM(StdIO.sy) SYM(FtdIO.sy) SysPerror.m2
		$(M2C) $(MFLAGS) -c SysPerror.m2
		ar rv $(LIB) SysPerror.o && ranlib $(LIB) && rm -f \
		SysPerror.o
$(PLIB)(SysPerror.o): /usr/local/lib/modula/SYM(Errno.sy) SYM(SysPerror.sy) \
		SYM(StdIO.sy) SYM(FtdIO.sy) SysPerror.m2
		$(M2C) $(MFLAGS) -c -p SysPerror.m2
		ar rv $(PLIB) SysPerror.o && ranlib $(PLIB) && rm -f \
		SysPerror.o
SYM(SysSema.sy): SysSema.d
		$(M2C) $(MFLAGS) SysSema.d
$(LIB)(SysSema.o): /usr/local/lib/modula/SYM(Errno.sy) SYM(Storage.sy) \
		/usr/local/lib/modula/SYM(Sys.sy) SYM(SysSema.sy) SysSema.m2
		$(M2C) $(MFLAGS) -c SysSema.m2
		ar rv $(LIB) SysSema.o && ranlib $(LIB) && rm -f SysSema.o
$(PLIB)(SysSema.o): /usr/local/lib/modula/SYM(Errno.sy) SYM(Storage.sy) \
		/usr/local/lib/modula/SYM(Sys.sy) SYM(SysSema.sy) SysSema.m2
		$(M2C) $(MFLAGS) -c -p SysSema.m2
		ar rv $(PLIB) SysSema.o && ranlib $(PLIB) && rm -f SysSema.o
SYM(Terminal.sy): Terminal.d
		$(M2C) $(MFLAGS) Terminal.d
$(LIB)(Terminal.o): SYM(Terminal.sy) SYM(StdIO.sy) Terminal.m2
		$(M2C) $(MFLAGS) -c Terminal.m2
		ar rv $(LIB) Terminal.o && ranlib $(LIB) && rm -f Terminal.o
$(PLIB)(Terminal.o): SYM(Terminal.sy) SYM(StdIO.sy) Terminal.m2
		$(M2C) $(MFLAGS) -c -p Terminal.m2
		ar rv $(PLIB) Terminal.o && ranlib $(PLIB) && rm -f \
		Terminal.o
SYM(TimeIO.sy):	TimeIO.d SYM(Calendar.sy) SYM(StdIO.sy)
		$(M2C) $(MFLAGS) TimeIO.d
$(LIB)(TimeIO.o): SYM(ASCII.sy) SYM(Conversions.sy) SYM(Environment.sy) \
		SYM(Storage.sy) SYM(Strings.sy) SYM(Calendar.sy) \
		SYM(StdIO.sy) SYM(TimeIO.sy) SYM(FtdIO.sy) TimeIO.m2
		$(M2C) $(MFLAGS) -c TimeIO.m2
		ar rv $(LIB) TimeIO.o && ranlib $(LIB) && rm -f TimeIO.o
$(PLIB)(TimeIO.o): SYM(ASCII.sy) SYM(Conversions.sy) SYM(Environment.sy) \
		SYM(Storage.sy) SYM(Strings.sy) SYM(Calendar.sy) \
		SYM(StdIO.sy) SYM(TimeIO.sy) SYM(FtdIO.sy) TimeIO.m2
		$(M2C) $(MFLAGS) -c -p TimeIO.m2
		ar rv $(PLIB) TimeIO.o && ranlib $(PLIB) && rm -f TimeIO.o
#
tags:		$(TAGSRC)
		mtags $(TAGSRC)
#
SPOOL =		lp
pr:		Pr.date
Pr.date:	$(SRC)
		pr $? | $(SPOOL)
		-touch Pr.date
#
clean:		; rm -f $(OBJS) $(SYMS) $(REFS)
