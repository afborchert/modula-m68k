# {updated by mmm: Tue Feb 02 22:19:36 1993 GMT}
# {flags: -a+ -e- -l+ -L- -m+ -1- -v-}
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
SRC =		Errno.d Errno.m2 Sys.d Sys.m2 SysAccess.d SysAccess.m2 \
		SysAlarm.d SysAlarm.m2 SysBreak.d SysBreak.m2 SysClose.d \
		SysClose.m2 SysCreat.d SysCreat.m2 SysDup.d SysDup.m2 \
		SysExec.d SysExec.m2 SysExit.d SysExit.m2 SysFcntl.d \
		SysFcntl.m2 SysFork.d SysFork.m2 SysGetpid.d SysGetpid.m2 \
		SysGetuid.d SysGetuid.m2 SysIoctl.d SysIoctl.m2 SysKill.d \
		SysKill.m2 SysLink.d SysLink.m2 SysLocation.d SysLockf.d \
		SysLockf.m2 SysLseek.d SysLseek.m2 SysMknod.d SysMknod.m2 \
		SysMonitor.d SysMonitor.m2 SysOpen.d SysOpen.m2 SysPanic.d \
		SysPanic.m2 SysPause.d SysPause.m2 SysPipe.d SysPipe.m2 \
		SysProfil.d SysProfil.m2 SysRead.d SysRead.m2 SysSema.d \
		SysSema.m2 SysSetuid.d SysSetuid.m2 SysSignal.d SysSignal.m2 \
		SysStat.d SysStat.m2 SysTermIO.d SysTermIO.m2 SysTime.d \
		SysTime.m2 SysUnlink.d SysUnlink.m2 SysWait.d SysWait.m2 \
		SysWrite.d SysWrite.m2 SystemTypes.d SystemTypes.m2 \
		UnixString.d UnixString.m2
TAGSRC =	Errno.m2 Sys.m2 SysAccess.m2 SysAlarm.m2 SysBreak.m2 \
		SysClose.m2 SysCreat.m2 SysDup.m2 SysExec.m2 SysExit.m2 \
		SysFcntl.m2 SysFork.m2 SysGetpid.m2 SysGetuid.m2 SysIoctl.m2 \
		SysKill.m2 SysLink.m2 SysLockf.m2 SysLseek.m2 SysMknod.m2 \
		SysMonitor.m2 SysOpen.m2 SysPanic.m2 SysPause.m2 SysPipe.m2 \
		SysProfil.m2 SysRead.m2 SysSema.m2 SysSetuid.m2 SysSignal.m2 \
		SysStat.m2 SysTermIO.m2 SysTime.m2 SysUnlink.m2 SysWait.m2 \
		SysWrite.m2 SystemTypes.m2 UnixString.m2
OBJS =		
SYMS =		
REFS =		Errno.r Sys.r SysAccess.r SysAlarm.r SysBreak.r SysClose.r \
		SysCreat.r SysDup.r SysExec.r SysExit.r SysFcntl.r SysFork.r \
		SysGetpid.r SysGetuid.r SysIoctl.r SysKill.r SysLink.r \
		SysLockf.r SysLseek.r SysMknod.r SysMonitor.r SysOpen.r \
		SysPanic.r SysPause.r SysPipe.r SysProfil.r SysRead.r \
		SysSema.r SysSetuid.r SysSignal.r SysStat.r SysTermIO.r \
		SysTime.r SysUnlink.r SysWait.r SysWrite.r SystemTypes.r \
		UnixString.r
# {precious}
LIB =		libm2.a
PLIB =		plibm2.a
.PRECIOUS:	$(LIB) $(PLIB) SYM
# {all}
all:		SYM(SysLocation.sy) $(LIB) $(PLIB)
# {linkage}
LIBS =		
LDFLAGS =	
# {dependencies}
$(LIB):		$(LIB)(Errno.o) $(LIB)(Sys.o) $(LIB)(SysAccess.o) \
		$(LIB)(SysAlarm.o) $(LIB)(SysBreak.o) $(LIB)(SysClose.o) \
		$(LIB)(SysCreat.o) $(LIB)(SysDup.o) $(LIB)(SysExec.o) \
		$(LIB)(SysExit.o) $(LIB)(SysFcntl.o) $(LIB)(SysFork.o) \
		$(LIB)(SysGetpid.o) $(LIB)(SysGetuid.o) $(LIB)(SysIoctl.o) \
		$(LIB)(SysKill.o) $(LIB)(SysLink.o) $(LIB)(SysLockf.o) \
		$(LIB)(SysLseek.o) $(LIB)(SysMknod.o) $(LIB)(SysMonitor.o) \
		$(LIB)(SysOpen.o) $(LIB)(SysPanic.o) $(LIB)(SysPause.o) \
		$(LIB)(SysPipe.o) $(LIB)(SysProfil.o) $(LIB)(SysRead.o) \
		$(LIB)(SysSema.o) $(LIB)(SysSetuid.o) $(LIB)(SysSignal.o) \
		$(LIB)(SysStat.o) $(LIB)(SysTermIO.o) $(LIB)(SysTime.o) \
		$(LIB)(SysUnlink.o) $(LIB)(SysWait.o) $(LIB)(SysWrite.o) \
		$(LIB)(SystemTypes.o) $(LIB)(UnixString.o)
$(PLIB):	$(PLIB)(Errno.o) $(PLIB)(Sys.o) $(PLIB)(SysAccess.o) \
		$(PLIB)(SysAlarm.o) $(PLIB)(SysBreak.o) $(PLIB)(SysClose.o) \
		$(PLIB)(SysCreat.o) $(PLIB)(SysDup.o) $(PLIB)(SysExec.o) \
		$(PLIB)(SysExit.o) $(PLIB)(SysFcntl.o) $(PLIB)(SysFork.o) \
		$(PLIB)(SysGetpid.o) $(PLIB)(SysGetuid.o) \
		$(PLIB)(SysIoctl.o) $(PLIB)(SysKill.o) $(PLIB)(SysLink.o) \
		$(PLIB)(SysLockf.o) $(PLIB)(SysLseek.o) $(PLIB)(SysMknod.o) \
		$(PLIB)(SysMonitor.o) $(PLIB)(SysOpen.o) $(PLIB)(SysPanic.o) \
		$(PLIB)(SysPause.o) $(PLIB)(SysPipe.o) $(PLIB)(SysProfil.o) \
		$(PLIB)(SysRead.o) $(PLIB)(SysSema.o) $(PLIB)(SysSetuid.o) \
		$(PLIB)(SysSignal.o) $(PLIB)(SysStat.o) $(PLIB)(SysTermIO.o) \
		$(PLIB)(SysTime.o) $(PLIB)(SysUnlink.o) $(PLIB)(SysWait.o) \
		$(PLIB)(SysWrite.o) $(PLIB)(SystemTypes.o) \
		$(PLIB)(UnixString.o)
SYM(Errno.sy):	Errno.d
		$(M2C) $(MFLAGS) Errno.d
$(LIB)(Errno.o): SYM(Errno.sy) Errno.m2
		$(M2C) $(MFLAGS) -c Errno.m2
		ar rv $(LIB) Errno.o && ranlib $(LIB) && rm -f Errno.o
$(PLIB)(Errno.o): SYM(Errno.sy) Errno.m2
		$(M2C) $(MFLAGS) -c -p Errno.m2
		ar rv $(PLIB) Errno.o && ranlib $(PLIB) && rm -f Errno.o
SYM(Sys.sy):	Sys.d
		$(M2C) $(MFLAGS) Sys.d
$(LIB)(Sys.o):	SYM(Sys.sy) Sys.m2
		$(M2C) $(MFLAGS) -c Sys.m2
		ar rv $(LIB) Sys.o && ranlib $(LIB) && rm -f Sys.o
$(PLIB)(Sys.o):	SYM(Sys.sy) Sys.m2
		$(M2C) $(MFLAGS) -c -p Sys.m2
		ar rv $(PLIB) Sys.o && ranlib $(PLIB) && rm -f Sys.o
SYM(SysAccess.sy): SysAccess.d
		$(M2C) $(MFLAGS) SysAccess.d
$(LIB)(SysAccess.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysAccess.sy) \
		SYM(UnixString.sy) SysAccess.m2
		$(M2C) $(MFLAGS) -c SysAccess.m2
		ar rv $(LIB) SysAccess.o && ranlib $(LIB) && rm -f \
		SysAccess.o
$(PLIB)(SysAccess.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysAccess.sy) \
		SYM(UnixString.sy) SysAccess.m2
		$(M2C) $(MFLAGS) -c -p SysAccess.m2
		ar rv $(PLIB) SysAccess.o && ranlib $(PLIB) && rm -f \
		SysAccess.o
SYM(SysAlarm.sy): SysAlarm.d
		$(M2C) $(MFLAGS) SysAlarm.d
$(LIB)(SysAlarm.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysAlarm.sy) SysAlarm.m2
		$(M2C) $(MFLAGS) -c SysAlarm.m2
		ar rv $(LIB) SysAlarm.o && ranlib $(LIB) && rm -f SysAlarm.o
$(PLIB)(SysAlarm.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysAlarm.sy) SysAlarm.m2
		$(M2C) $(MFLAGS) -c -p SysAlarm.m2
		ar rv $(PLIB) SysAlarm.o && ranlib $(PLIB) && rm -f \
		SysAlarm.o
SYM(SysBreak.sy): SysBreak.d
		$(M2C) $(MFLAGS) SysBreak.d
$(LIB)(SysBreak.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysBreak.sy) \
		SYM(SysLocation.sy) SysBreak.m2
		$(M2C) $(MFLAGS) -c SysBreak.m2
		ar rv $(LIB) SysBreak.o && ranlib $(LIB) && rm -f SysBreak.o
$(PLIB)(SysBreak.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysBreak.sy) \
		SYM(SysLocation.sy) SysBreak.m2
		$(M2C) $(MFLAGS) -c -p SysBreak.m2
		ar rv $(PLIB) SysBreak.o && ranlib $(PLIB) && rm -f \
		SysBreak.o
SYM(SysClose.sy): SysClose.d
		$(M2C) $(MFLAGS) SysClose.d
$(LIB)(SysClose.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysClose.sy) SysClose.m2
		$(M2C) $(MFLAGS) -c SysClose.m2
		ar rv $(LIB) SysClose.o && ranlib $(LIB) && rm -f SysClose.o
$(PLIB)(SysClose.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysClose.sy) SysClose.m2
		$(M2C) $(MFLAGS) -c -p SysClose.m2
		ar rv $(PLIB) SysClose.o && ranlib $(PLIB) && rm -f \
		SysClose.o
SYM(SysCreat.sy): SysCreat.d
		$(M2C) $(MFLAGS) SysCreat.d
$(LIB)(SysCreat.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysCreat.sy) \
		SYM(UnixString.sy) SysCreat.m2
		$(M2C) $(MFLAGS) -c SysCreat.m2
		ar rv $(LIB) SysCreat.o && ranlib $(LIB) && rm -f SysCreat.o
$(PLIB)(SysCreat.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysCreat.sy) \
		SYM(UnixString.sy) SysCreat.m2
		$(M2C) $(MFLAGS) -c -p SysCreat.m2
		ar rv $(PLIB) SysCreat.o && ranlib $(PLIB) && rm -f \
		SysCreat.o
SYM(SysDup.sy):	SysDup.d
		$(M2C) $(MFLAGS) SysDup.d
$(LIB)(SysDup.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysClose.sy) SYM(SysDup.sy) \
		SysDup.m2
		$(M2C) $(MFLAGS) -c SysDup.m2
		ar rv $(LIB) SysDup.o && ranlib $(LIB) && rm -f SysDup.o
$(PLIB)(SysDup.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysClose.sy) SYM(SysDup.sy) \
		SysDup.m2
		$(M2C) $(MFLAGS) -c -p SysDup.m2
		ar rv $(PLIB) SysDup.o && ranlib $(PLIB) && rm -f SysDup.o
SYM(SysExec.sy): SysExec.d
		$(M2C) $(MFLAGS) SysExec.d
$(LIB)(SysExec.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysExec.sy) \
		SYM(SysLocation.sy) SYM(UnixString.sy) SysExec.m2
		$(M2C) $(MFLAGS) -c SysExec.m2
		ar rv $(LIB) SysExec.o && ranlib $(LIB) && rm -f SysExec.o
$(PLIB)(SysExec.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysExec.sy) \
		SYM(SysLocation.sy) SYM(UnixString.sy) SysExec.m2
		$(M2C) $(MFLAGS) -c -p SysExec.m2
		ar rv $(PLIB) SysExec.o && ranlib $(PLIB) && rm -f SysExec.o
SYM(SysExit.sy): SysExit.d
		$(M2C) $(MFLAGS) SysExit.d
$(LIB)(SysExit.o): SYM(Sys.sy) SYM(SysExit.sy) SysExit.m2
		$(M2C) $(MFLAGS) -c SysExit.m2
		ar rv $(LIB) SysExit.o && ranlib $(LIB) && rm -f SysExit.o
$(PLIB)(SysExit.o): SYM(Sys.sy) SYM(SysExit.sy) SysExit.m2
		$(M2C) $(MFLAGS) -c -p SysExit.m2
		ar rv $(PLIB) SysExit.o && ranlib $(PLIB) && rm -f SysExit.o
SYM(SysFcntl.sy): SysFcntl.d
		$(M2C) $(MFLAGS) SysFcntl.d
$(LIB)(SysFcntl.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysFcntl.sy) SysFcntl.m2
		$(M2C) $(MFLAGS) -c SysFcntl.m2
		ar rv $(LIB) SysFcntl.o && ranlib $(LIB) && rm -f SysFcntl.o
$(PLIB)(SysFcntl.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysFcntl.sy) SysFcntl.m2
		$(M2C) $(MFLAGS) -c -p SysFcntl.m2
		ar rv $(PLIB) SysFcntl.o && ranlib $(PLIB) && rm -f \
		SysFcntl.o
SYM(SysFork.sy): SysFork.d
		$(M2C) $(MFLAGS) SysFork.d
$(LIB)(SysFork.o): SYM(Errno.sy) SYM(SysFork.sy) SysFork.m2
		$(M2C) $(MFLAGS) -c SysFork.m2
		ar rv $(LIB) SysFork.o && ranlib $(LIB) && rm -f SysFork.o
$(PLIB)(SysFork.o): SYM(Errno.sy) SYM(SysFork.sy) SysFork.m2
		$(M2C) $(MFLAGS) -c -p SysFork.m2
		ar rv $(PLIB) SysFork.o && ranlib $(PLIB) && rm -f SysFork.o
SYM(SysGetpid.sy): SysGetpid.d SYM(SystemTypes.sy)
		$(M2C) $(MFLAGS) SysGetpid.d
$(LIB)(SysGetpid.o): SYM(Sys.sy) SYM(SystemTypes.sy) SYM(SysGetpid.sy) \
		SysGetpid.m2
		$(M2C) $(MFLAGS) -c SysGetpid.m2
		ar rv $(LIB) SysGetpid.o && ranlib $(LIB) && rm -f \
		SysGetpid.o
$(PLIB)(SysGetpid.o): SYM(Sys.sy) SYM(SystemTypes.sy) SYM(SysGetpid.sy) \
		SysGetpid.m2
		$(M2C) $(MFLAGS) -c -p SysGetpid.m2
		ar rv $(PLIB) SysGetpid.o && ranlib $(PLIB) && rm -f \
		SysGetpid.o
SYM(SysGetuid.sy): SysGetuid.d
		$(M2C) $(MFLAGS) SysGetuid.d
$(LIB)(SysGetuid.o): SYM(Sys.sy) SYM(SysGetuid.sy) SysGetuid.m2
		$(M2C) $(MFLAGS) -c SysGetuid.m2
		ar rv $(LIB) SysGetuid.o && ranlib $(LIB) && rm -f \
		SysGetuid.o
$(PLIB)(SysGetuid.o): SYM(Sys.sy) SYM(SysGetuid.sy) SysGetuid.m2
		$(M2C) $(MFLAGS) -c -p SysGetuid.m2
		ar rv $(PLIB) SysGetuid.o && ranlib $(PLIB) && rm -f \
		SysGetuid.o
SYM(SysIoctl.sy): SysIoctl.d
		$(M2C) $(MFLAGS) SysIoctl.d
$(LIB)(SysIoctl.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysIoctl.sy) SysIoctl.m2
		$(M2C) $(MFLAGS) -c SysIoctl.m2
		ar rv $(LIB) SysIoctl.o && ranlib $(LIB) && rm -f SysIoctl.o
$(PLIB)(SysIoctl.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysIoctl.sy) SysIoctl.m2
		$(M2C) $(MFLAGS) -c -p SysIoctl.m2
		ar rv $(PLIB) SysIoctl.o && ranlib $(PLIB) && rm -f \
		SysIoctl.o
SYM(SysKill.sy): SysKill.d SYM(SystemTypes.sy)
		$(M2C) $(MFLAGS) SysKill.d
$(LIB)(SysKill.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SystemTypes.sy) \
		SYM(SysKill.sy) SysKill.m2
		$(M2C) $(MFLAGS) -c SysKill.m2
		ar rv $(LIB) SysKill.o && ranlib $(LIB) && rm -f SysKill.o
$(PLIB)(SysKill.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SystemTypes.sy) \
		SYM(SysKill.sy) SysKill.m2
		$(M2C) $(MFLAGS) -c -p SysKill.m2
		ar rv $(PLIB) SysKill.o && ranlib $(PLIB) && rm -f SysKill.o
SYM(SysLink.sy): SysLink.d
		$(M2C) $(MFLAGS) SysLink.d
$(LIB)(SysLink.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysLink.sy) \
		SYM(UnixString.sy) SysLink.m2
		$(M2C) $(MFLAGS) -c SysLink.m2
		ar rv $(LIB) SysLink.o && ranlib $(LIB) && rm -f SysLink.o
$(PLIB)(SysLink.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysLink.sy) \
		SYM(UnixString.sy) SysLink.m2
		$(M2C) $(MFLAGS) -c -p SysLink.m2
		ar rv $(PLIB) SysLink.o && ranlib $(PLIB) && rm -f SysLink.o
SYM(SysLocation.sy): SysLocation.d
		$(M2C) $(MFLAGS) SysLocation.d
SYM(SysLockf.sy): SysLockf.d SYM(SystemTypes.sy)
		$(M2C) $(MFLAGS) SysLockf.d
$(LIB)(SysLockf.o): SYM(Sys.sy) SYM(SysFcntl.sy) SYM(SystemTypes.sy) \
		SYM(SysLockf.sy) SysLockf.m2
		$(M2C) $(MFLAGS) -c SysLockf.m2
		ar rv $(LIB) SysLockf.o && ranlib $(LIB) && rm -f SysLockf.o
$(PLIB)(SysLockf.o): SYM(Sys.sy) SYM(SysFcntl.sy) SYM(SystemTypes.sy) \
		SYM(SysLockf.sy) SysLockf.m2
		$(M2C) $(MFLAGS) -c -p SysLockf.m2
		ar rv $(PLIB) SysLockf.o && ranlib $(PLIB) && rm -f \
		SysLockf.o
SYM(SysLseek.sy): SysLseek.d SYM(SystemTypes.sy)
		$(M2C) $(MFLAGS) SysLseek.d
$(LIB)(SysLseek.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SystemTypes.sy) \
		SYM(SysLseek.sy) SysLseek.m2
		$(M2C) $(MFLAGS) -c SysLseek.m2
		ar rv $(LIB) SysLseek.o && ranlib $(LIB) && rm -f SysLseek.o
$(PLIB)(SysLseek.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SystemTypes.sy) \
		SYM(SysLseek.sy) SysLseek.m2
		$(M2C) $(MFLAGS) -c -p SysLseek.m2
		ar rv $(PLIB) SysLseek.o && ranlib $(PLIB) && rm -f \
		SysLseek.o
SYM(SysMknod.sy): SysMknod.d
		$(M2C) $(MFLAGS) SysMknod.d
$(LIB)(SysMknod.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysMknod.sy) \
		SYM(UnixString.sy) SYM(SysStat.sy) SysMknod.m2
		$(M2C) $(MFLAGS) -c SysMknod.m2
		ar rv $(LIB) SysMknod.o && ranlib $(LIB) && rm -f SysMknod.o
$(PLIB)(SysMknod.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysMknod.sy) \
		SYM(UnixString.sy) SYM(SysStat.sy) SysMknod.m2
		$(M2C) $(MFLAGS) -c -p SysMknod.m2
		ar rv $(PLIB) SysMknod.o && ranlib $(PLIB) && rm -f \
		SysMknod.o
SYM(SysMonitor.sy): SysMonitor.d
		$(M2C) $(MFLAGS) SysMonitor.d
$(LIB)(SysMonitor.o): SYM(SysClose.sy) SYM(SysCreat.sy) SYM(SysExit.sy) \
		SYM(SysMonitor.sy) SYM(SysProfil.sy) SYM(SysWrite.sy) \
		SysMonitor.m2
		$(M2C) $(MFLAGS) -c SysMonitor.m2
		ar rv $(LIB) SysMonitor.o && ranlib $(LIB) && rm -f \
		SysMonitor.o
$(PLIB)(SysMonitor.o): SYM(SysClose.sy) SYM(SysCreat.sy) SYM(SysExit.sy) \
		SYM(SysMonitor.sy) SYM(SysProfil.sy) SYM(SysWrite.sy) \
		SysMonitor.m2
		$(M2C) $(MFLAGS) -c -p SysMonitor.m2
		ar rv $(PLIB) SysMonitor.o && ranlib $(PLIB) && rm -f \
		SysMonitor.o
SYM(SysOpen.sy): SysOpen.d
		$(M2C) $(MFLAGS) SysOpen.d
$(LIB)(SysOpen.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysOpen.sy) \
		SYM(UnixString.sy) SysOpen.m2
		$(M2C) $(MFLAGS) -c SysOpen.m2
		ar rv $(LIB) SysOpen.o && ranlib $(LIB) && rm -f SysOpen.o
$(PLIB)(SysOpen.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysOpen.sy) \
		SYM(UnixString.sy) SysOpen.m2
		$(M2C) $(MFLAGS) -c -p SysOpen.m2
		ar rv $(PLIB) SysOpen.o && ranlib $(PLIB) && rm -f SysOpen.o
SYM(SysPanic.sy): SysPanic.d
		$(M2C) $(MFLAGS) SysPanic.d
$(LIB)(SysPanic.o): SYM(SysExit.sy) SYM(SysPanic.sy) SYM(SysWrite.sy) \
		SysPanic.m2
		$(M2C) $(MFLAGS) -c SysPanic.m2
		ar rv $(LIB) SysPanic.o && ranlib $(LIB) && rm -f SysPanic.o
$(PLIB)(SysPanic.o): SYM(SysExit.sy) SYM(SysPanic.sy) SYM(SysWrite.sy) \
		SysPanic.m2
		$(M2C) $(MFLAGS) -c -p SysPanic.m2
		ar rv $(PLIB) SysPanic.o && ranlib $(PLIB) && rm -f \
		SysPanic.o
SYM(SysPause.sy): SysPause.d
		$(M2C) $(MFLAGS) SysPause.d
$(LIB)(SysPause.o): SYM(Sys.sy) SYM(SysPause.sy) SysPause.m2
		$(M2C) $(MFLAGS) -c SysPause.m2
		ar rv $(LIB) SysPause.o && ranlib $(LIB) && rm -f SysPause.o
$(PLIB)(SysPause.o): SYM(Sys.sy) SYM(SysPause.sy) SysPause.m2
		$(M2C) $(MFLAGS) -c -p SysPause.m2
		ar rv $(PLIB) SysPause.o && ranlib $(PLIB) && rm -f \
		SysPause.o
SYM(SysPipe.sy): SysPipe.d
		$(M2C) $(MFLAGS) SysPipe.d
$(LIB)(SysPipe.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysPipe.sy) SysPipe.m2
		$(M2C) $(MFLAGS) -c SysPipe.m2
		ar rv $(LIB) SysPipe.o && ranlib $(LIB) && rm -f SysPipe.o
$(PLIB)(SysPipe.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysPipe.sy) SysPipe.m2
		$(M2C) $(MFLAGS) -c -p SysPipe.m2
		ar rv $(PLIB) SysPipe.o && ranlib $(PLIB) && rm -f SysPipe.o
SYM(SysProfil.sy): SysProfil.d
		$(M2C) $(MFLAGS) SysProfil.d
$(LIB)(SysProfil.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysProfil.sy) \
		SysProfil.m2
		$(M2C) $(MFLAGS) -c SysProfil.m2
		ar rv $(LIB) SysProfil.o && ranlib $(LIB) && rm -f \
		SysProfil.o
$(PLIB)(SysProfil.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysProfil.sy) \
		SysProfil.m2
		$(M2C) $(MFLAGS) -c -p SysProfil.m2
		ar rv $(PLIB) SysProfil.o && ranlib $(PLIB) && rm -f \
		SysProfil.o
SYM(SysRead.sy): SysRead.d
		$(M2C) $(MFLAGS) SysRead.d
$(LIB)(SysRead.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysRead.sy) SysRead.m2
		$(M2C) $(MFLAGS) -c SysRead.m2
		ar rv $(LIB) SysRead.o && ranlib $(LIB) && rm -f SysRead.o
$(PLIB)(SysRead.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysRead.sy) SysRead.m2
		$(M2C) $(MFLAGS) -c -p SysRead.m2
		ar rv $(PLIB) SysRead.o && ranlib $(PLIB) && rm -f SysRead.o
SYM(SysSema.sy): SysSema.d
		$(M2C) $(MFLAGS) SysSema.d
$(LIB)(SysSema.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysSema.sy) SysSema.m2
		$(M2C) $(MFLAGS) -c SysSema.m2
		ar rv $(LIB) SysSema.o && ranlib $(LIB) && rm -f SysSema.o
$(PLIB)(SysSema.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysSema.sy) SysSema.m2
		$(M2C) $(MFLAGS) -c -p SysSema.m2
		ar rv $(PLIB) SysSema.o && ranlib $(PLIB) && rm -f SysSema.o
SYM(SysSetuid.sy): SysSetuid.d
		$(M2C) $(MFLAGS) SysSetuid.d
$(LIB)(SysSetuid.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysSetuid.sy) \
		SysSetuid.m2
		$(M2C) $(MFLAGS) -c SysSetuid.m2
		ar rv $(LIB) SysSetuid.o && ranlib $(LIB) && rm -f \
		SysSetuid.o
$(PLIB)(SysSetuid.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysSetuid.sy) \
		SysSetuid.m2
		$(M2C) $(MFLAGS) -c -p SysSetuid.m2
		ar rv $(PLIB) SysSetuid.o && ranlib $(PLIB) && rm -f \
		SysSetuid.o
SYM(SysSignal.sy): SysSignal.d SYM(SystemTypes.sy)
		$(M2C) $(MFLAGS) SysSignal.d
$(LIB)(SysSignal.o): SYM(Errno.sy) SYM(SystemTypes.sy) SYM(SysSignal.sy) \
		SysSignal.m2
		$(M2C) $(MFLAGS) -c SysSignal.m2
		ar rv $(LIB) SysSignal.o && ranlib $(LIB) && rm -f \
		SysSignal.o
$(PLIB)(SysSignal.o): SYM(Errno.sy) SYM(SystemTypes.sy) SYM(SysSignal.sy) \
		SysSignal.m2
		$(M2C) $(MFLAGS) -c -p SysSignal.m2
		ar rv $(PLIB) SysSignal.o && ranlib $(PLIB) && rm -f \
		SysSignal.o
SYM(SysStat.sy): SysStat.d SYM(SystemTypes.sy)
		$(M2C) $(MFLAGS) SysStat.d
$(LIB)(SysStat.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SystemTypes.sy) \
		SYM(UnixString.sy) SYM(SysStat.sy) SysStat.m2
		$(M2C) $(MFLAGS) -c SysStat.m2
		ar rv $(LIB) SysStat.o && ranlib $(LIB) && rm -f SysStat.o
$(PLIB)(SysStat.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SystemTypes.sy) \
		SYM(UnixString.sy) SYM(SysStat.sy) SysStat.m2
		$(M2C) $(MFLAGS) -c -p SysStat.m2
		ar rv $(PLIB) SysStat.o && ranlib $(PLIB) && rm -f SysStat.o
SYM(SysTermIO.sy): SysTermIO.d
		$(M2C) $(MFLAGS) SysTermIO.d
$(LIB)(SysTermIO.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysTermIO.sy) \
		SysTermIO.m2
		$(M2C) $(MFLAGS) -c SysTermIO.m2
		ar rv $(LIB) SysTermIO.o && ranlib $(LIB) && rm -f \
		SysTermIO.o
$(PLIB)(SysTermIO.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysTermIO.sy) \
		SysTermIO.m2
		$(M2C) $(MFLAGS) -c -p SysTermIO.m2
		ar rv $(PLIB) SysTermIO.o && ranlib $(PLIB) && rm -f \
		SysTermIO.o
SYM(SysTime.sy): SysTime.d SYM(SystemTypes.sy)
		$(M2C) $(MFLAGS) SysTime.d
$(LIB)(SysTime.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SystemTypes.sy) \
		SYM(SysTime.sy) SysTime.m2
		$(M2C) $(MFLAGS) -c SysTime.m2
		ar rv $(LIB) SysTime.o && ranlib $(LIB) && rm -f SysTime.o
$(PLIB)(SysTime.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SystemTypes.sy) \
		SYM(SysTime.sy) SysTime.m2
		$(M2C) $(MFLAGS) -c -p SysTime.m2
		ar rv $(PLIB) SysTime.o && ranlib $(PLIB) && rm -f SysTime.o
SYM(SysUnlink.sy): SysUnlink.d
		$(M2C) $(MFLAGS) SysUnlink.d
$(LIB)(SysUnlink.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysUnlink.sy) \
		SYM(UnixString.sy) SysUnlink.m2
		$(M2C) $(MFLAGS) -c SysUnlink.m2
		ar rv $(LIB) SysUnlink.o && ranlib $(LIB) && rm -f \
		SysUnlink.o
$(PLIB)(SysUnlink.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysUnlink.sy) \
		SYM(UnixString.sy) SysUnlink.m2
		$(M2C) $(MFLAGS) -c -p SysUnlink.m2
		ar rv $(PLIB) SysUnlink.o && ranlib $(PLIB) && rm -f \
		SysUnlink.o
SYM(SysWait.sy): SysWait.d
		$(M2C) $(MFLAGS) SysWait.d
$(LIB)(SysWait.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysWait.sy) SysWait.m2
		$(M2C) $(MFLAGS) -c SysWait.m2
		ar rv $(LIB) SysWait.o && ranlib $(LIB) && rm -f SysWait.o
$(PLIB)(SysWait.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysWait.sy) SysWait.m2
		$(M2C) $(MFLAGS) -c -p SysWait.m2
		ar rv $(PLIB) SysWait.o && ranlib $(PLIB) && rm -f SysWait.o
SYM(SysWrite.sy): SysWrite.d
		$(M2C) $(MFLAGS) SysWrite.d
$(LIB)(SysWrite.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysWrite.sy) SysWrite.m2
		$(M2C) $(MFLAGS) -c SysWrite.m2
		ar rv $(LIB) SysWrite.o && ranlib $(LIB) && rm -f SysWrite.o
$(PLIB)(SysWrite.o): SYM(Errno.sy) SYM(Sys.sy) SYM(SysWrite.sy) SysWrite.m2
		$(M2C) $(MFLAGS) -c -p SysWrite.m2
		ar rv $(PLIB) SysWrite.o && ranlib $(PLIB) && rm -f \
		SysWrite.o
SYM(SystemTypes.sy): SystemTypes.d
		$(M2C) $(MFLAGS) SystemTypes.d
$(LIB)(SystemTypes.o): SYM(SystemTypes.sy) SystemTypes.m2
		$(M2C) $(MFLAGS) -c SystemTypes.m2
		ar rv $(LIB) SystemTypes.o && ranlib $(LIB) && rm -f \
		SystemTypes.o
$(PLIB)(SystemTypes.o): SYM(SystemTypes.sy) SystemTypes.m2
		$(M2C) $(MFLAGS) -c -p SystemTypes.m2
		ar rv $(PLIB) SystemTypes.o && ranlib $(PLIB) && rm -f \
		SystemTypes.o
SYM(UnixString.sy): UnixString.d
		$(M2C) $(MFLAGS) UnixString.d
$(LIB)(UnixString.o): SYM(UnixString.sy) UnixString.m2
		$(M2C) $(MFLAGS) -c UnixString.m2
		ar rv $(LIB) UnixString.o && ranlib $(LIB) && rm -f \
		UnixString.o
$(PLIB)(UnixString.o): SYM(UnixString.sy) UnixString.m2
		$(M2C) $(MFLAGS) -c -p UnixString.m2
		ar rv $(PLIB) UnixString.o && ranlib $(PLIB) && rm -f \
		UnixString.o
