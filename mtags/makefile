# {updated by mmm: Sun Mar 27 20:03:29 1988 MSZ}
# {flags: -a- -l- -L- -1- -v-}
# {rules}
.SUFFIXES:
.SUFFIXES:	.o .sy .m2 .d
MFLAGS =	
M2C =		m2c
.m2.o:
		$(M2C) -c $(MFLAGS) $*.m2
.d.sy:
		$(M2C) $(MFLAGS) $*.d
# {macros}
SRC =		Scan.d Scan.m2 SymTab.d SymTab.m2 mtags.m2
TAGSRC =	Scan.m2 SymTab.m2 mtags.m2
OBJS =		Scan.o SymTab.o mtags.o
SYMS =		Scan.sy SymTab.sy
REFS =		Scan.r SymTab.r mtags.r
# {all}
all:		mtags
# {linkage}
LDFLAGS =	
mtags:		Scan.o SymTab.o mtags.o
		$(M2C) -o $@ $(LDFLAGS) Scan.o SymTab.o mtags.o
# {dependencies}
Scan.sy:	SymTab.sy
Scan.o:		Scan.sy SymTab.sy
SymTab.sy:	
SymTab.o:	SymTab.sy
mtags.o:	Scan.sy SymTab.sy
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
