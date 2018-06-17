(* Modula-2 makefile generator - UNIX System V - AFB 3/87 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
IMPLEMENTATION MODULE Makefile; (* AFB *)

   FROM SymTab IMPORT SourceKind, SourceRef, srclist, mtab, SourceKindSet,
      ModuleSet, ModuleRange, LookForMod, modules, defs, defset, Closure,
      LookForModR;
   FROM Suffix IMPORT SourceKindToSuffix, Sm2impl, Sm2def, Smrimpl, Smrdef,
      Om2impl, Om2def, Omrimpl, Omrdef, Archive, Ref;
   FROM FileNames IMPORT maxsuffix, filenmlen, ConstructFileName;
   FROM Macros IMPORT WriteMacro;
   FROM Write IMPORT WriteNameAndSuffix, WriteModuleSet,
      WriteSortedModuleSetInArchive, WriteArchiveAndSuffix,
      WriteModuleSetInArchive;
   FROM Options IMPORT SYMarchive, aronce, versmanag, VersionsManagement,
      library, profile;
   FROM Out IMPORT WriteTab, WriteString, Write, WriteLn;
   IMPORT Conversions, SymTab, FileNames, Macros, Strings, Out;

   VAR
      LIBdefined: BOOLEAN;

   PROCEDURE PrintRules;
      VAR
	 srcset: SourceKindSet;
	 src: SourceRef;

      PROCEDURE PrintSuffixes;

	 PROCEDURE WriteSuffix(suffix: ARRAY OF CHAR);
	 BEGIN
	    Write(" "); Write("."); WriteString(suffix);
	 END WriteSuffix;

      BEGIN
	 (* .SUFFIXES : .o .sy .m2 .mr .d .dr *)
	 WriteString(".SUFFIXES:"); WriteLn;
	 WriteString(".SUFFIXES:"); WriteTab;
	 WriteSuffix(Om2impl);
	 IF srcset * defs <> SourceKindSet{} THEN
	    WriteSuffix(Om2def);
	 END;
	 IF srcset * SourceKindSet{m2impl, m2main} <> SourceKindSet{} THEN
	    WriteSuffix(Sm2impl);
	    IF versmanag = rcs THEN
	       WriteSuffix(Sm2impl); WriteString(",v");
	    ELSIF versmanag = sccs THEN
	       WriteSuffix(Sm2impl); Write("~");
	    END;
	 END;
	 IF srcset * SourceKindSet{mrimpl, mrmain} <> SourceKindSet{} THEN
	    WriteSuffix(Smrimpl);
	    IF versmanag = rcs THEN
	       WriteSuffix(Smrimpl); WriteString(",v");
	    ELSIF versmanag = sccs THEN
	       WriteSuffix(Smrimpl); Write("~");
	    END;
	 END;
	 IF m2def IN srcset THEN
	    WriteSuffix(Sm2def);
	    IF versmanag = rcs THEN
	       WriteSuffix(Sm2def); WriteString(",v");
	    ELSIF versmanag = sccs THEN
	       WriteSuffix(Sm2def); Write("~");
	    END;
	 END;
	 IF mrdef IN srcset THEN
	    WriteSuffix(Smrdef);
	    IF versmanag = rcs THEN
	       WriteSuffix(Smrdef); WriteString(",v");
	    ELSIF versmanag = sccs THEN
	       WriteSuffix(Smrdef); Write("~");
	    END;
	 END;
	 WriteLn;
      END PrintSuffixes;

      PROCEDURE PrintRule(for: SourceKindSet; from, to: ARRAY OF CHAR;
			  rule: ARRAY OF CHAR);
      BEGIN
	 IF srcset * for <> SourceKindSet{} THEN
	    IF versmanag = rcs THEN
	       Write("."); WriteString(from); WriteString(",v");
	       Write("."); WriteString(to); Write(":");
	       WriteLn; WriteTab;
	       WriteString("$(CO) $(COFLAGS) $<"); WriteLn;
	       IF rule[0] <> 0C THEN
		  WriteTab; WriteString(rule); WriteString(from);
		  WriteLn;
	       END;

	       Write("."); WriteString(from); WriteString(",v");
	       Write("."); WriteString(from); Write(":");
	       WriteLn; WriteTab;
	       WriteString("$(CO) $(COFLAGS) $<"); WriteLn;
	    ELSIF versmanag = sccs THEN
	       Write("."); WriteString(from); WriteString("~.");
	       WriteString(to); Write(":"); WriteLn; WriteTab;
	       WriteString("$(GET) $(GFLAGS) -p $< >$*."); WriteString(from);
	       WriteLn;
	       IF rule[0] <> 0C THEN
		  WriteTab; WriteString(rule); WriteString(from);
		  WriteLn;
	       END;

	       Write("."); WriteString(from); WriteString("~.");
	       WriteString(from); Write(":"); WriteLn; WriteTab;
	       WriteString("$(GET) $(GFLAGS) -p $< >$*."); WriteString(from);
	       WriteLn;
	    END;
	    Write("."); WriteString(from);
	    Write("."); WriteString(to);
	    Write(":");
	    IF rule[0] <> 0C THEN
	       WriteLn; WriteTab; WriteString(rule);
	       WriteString(from);
	       WriteLn;
	    ELSE
	       WriteTab; Write(";"); WriteLn;
	    END;
	 END;
      END PrintRule;

   BEGIN
      srcset := SourceKindSet{};
      src := srclist;
      WHILE src <> NIL DO
	 INCL(srcset, src^.kind);
	 src := src^.link;
      END;

      PrintSuffixes;

      IF srcset * SourceKindSet{m2main, m2impl, m2def} <> SourceKindSet{} THEN
	 WriteMacro("MFLAGS", "");
	 WriteMacro("M2C", "m2c");
      END;
      IF srcset * SourceKindSet{mrmain, mrimpl, mrdef} <> SourceKindSet{} THEN
	 WriteMacro("MRFLAGS", "");
	 WriteMacro("MRC", "mrc");
      END;
      IF versmanag = rcs THEN
	 WriteMacro("COFLAGS", "");
	 WriteMacro("CO", "co");
      END;

      PrintRule(SourceKindSet{m2main, m2impl}, Sm2impl, Om2impl,
		"$(M2C) -c $(MFLAGS) $*.");
      IF SYMarchive THEN
	 IF aronce THEN
	    PrintRule(SourceKindSet{m2def}, Sm2def, Archive, "");
	 ELSE
	    PrintRule(SourceKindSet{m2def}, Sm2def, Archive,
		      "$(M2C) $(MFLAGS) $*.");
	 END;
      ELSE
	 PrintRule(SourceKindSet{m2def}, Sm2def, Om2def,
		   "$(M2C) $(MFLAGS) $*.");
      END;
      PrintRule(SourceKindSet{mrmain, mrimpl}, Smrimpl, Omrimpl,
		"$(MRC) -c $(MRFLAGS) $*.");
      IF SYMarchive THEN
	 IF aronce THEN
	    PrintRule(SourceKindSet{mrdef}, Smrdef, Archive, "");
	 ELSE
	    PrintRule(SourceKindSet{mrdef}, Smrdef, Archive,
		      "$(MRC) $(MRFLAGS) $*.");
	 END;
      ELSE
	 PrintRule(SourceKindSet{mrdef}, Smrdef, Omrdef,
		   "$(MRC) $(MRFLAGS) $*.");
      END;
   END PrintRules;

   PROCEDURE PrintMacros;
      VAR
	 src: SourceRef;
	 fname: ARRAY[0..filenmlen-1] OF CHAR;
   BEGIN
      WriteString("SRC ="); WriteTab;
      src := srclist;
      WHILE src <> NIL DO
	 IF NOT src^.extern THEN
	    ConstructFileName(fname, src^.filename);
	    Write(" "); WriteString(fname);
	 END;
	 src := src^.link;
      END;
      WriteLn;

      WriteString("TAGSRC ="); WriteTab;
      src := srclist;
      WHILE src <> NIL DO
	 WITH src^ DO
	    IF NOT extern AND NOT (kind IN defs) THEN
	       ConstructFileName(fname, filename);
	       Write(" "); WriteString(fname);
	    END;
	 END;
	 src := src^.link;
      END;
      WriteLn;

      IF versmanag <> none THEN
	 CASE versmanag OF
	 | rcs:  WriteString("RCSFILES =");
	 | sccs: WriteString("SCCSFILES =");
	 END;
	 WriteTab;
	 src := srclist;
	 WHILE src <> NIL DO
	    WITH src^ DO
	       IF NOT extern AND NOT (kind IN defs) THEN
		  ConstructFileName(fname, filename);
		  Write(" ");
		  IF versmanag = sccs THEN WriteString("s.") END;
		  WriteString(fname);
		  IF versmanag = rcs THEN WriteString(",v") END;
	       END;
	    END;
	    src := src^.link;
	 END;
	 WriteLn;
      END;

      WriteString("OBJS ="); WriteTab;
      src := srclist;
      WHILE src <> NIL DO
	 WITH src^ DO
	    IF NOT extern AND
	       (NOT library AND NOT (kind IN defs) OR
		library AND (kind IN SourceKindSet{m2main, mrmain})) THEN
	       Write(" "); WriteNameAndSuffix(filename, Om2impl);
	    END;
	 END;
	 src := src^.link;
      END;
      WriteLn;

      WriteString("SYMS ="); WriteTab;
      IF NOT SYMarchive THEN
	 src := srclist;
	 WHILE src <> NIL DO
	    WITH src^ DO
	       IF NOT extern AND (kind IN defs) THEN
		  Write(" "); WriteNameAndSuffix(filename, Om2def);
	       END;
	    END;
	    src := src^.link;
	 END;
      END;
      WriteLn;

      WriteString("REFS ="); WriteTab;
      src := srclist;
      WHILE src <> NIL DO
	 WITH src^ DO
	    IF NOT extern AND NOT (kind IN defs) THEN
	       Write(" "); WriteNameAndSuffix(filename, Ref);
	    END;
	 END;
	 src := src^.link;
      END;
      WriteLn;
   END PrintMacros;

   MODULE Mains;

      (* it's a good idea to name the main module like the final  *)
      (* target file to be executed. But this is not always true. *)
      (* So we give all targets macro names - this allows to      *)
      (* rename target files without losing the new names on      *)
      (* updates.                                                 *)

      FROM SymTab IMPORT srclist, SourceKind, SourceKindSet, SourceRef;
      FROM FileNames IMPORT ConstructFileName, filenmlen, FileName, dirsiz;
      FROM Macros IMPORT WriteMacro;
      FROM Out IMPORT WriteString, Write, WriteTab, WriteLn;
      FROM Strings IMPORT StrCat;
      FROM Conversions IMPORT ConvertCardinal;

      EXPORT WriteMainMacros, WriteMain;

      CONST
	 prefix = "T";		(* prefix of macro names *)
      VAR
	 macroswritten: BOOLEAN;
	 maincnt: CARDINAL;	(* number of main modules *)
	 main: SourceRef;	(* if maincnt=1: main module *)
	 init: BOOLEAN;		(* Init called yet? *)

      PROCEDURE Convert(VAR fn: ARRAY OF CHAR; filename: FileName);
      BEGIN
	 filename.suffix[0] := 0C;
	 ConstructFileName(fn, filename);
      END Convert;

      PROCEDURE Init;
	 VAR
	    src: SourceRef;
      BEGIN
	 IF init THEN RETURN END;
	 init := TRUE;
	 maincnt := 0;
	 src := srclist;
	 WHILE src <> NIL DO
	    IF src^.kind IN SourceKindSet{m2main, mrmain} THEN
	       INC(maincnt);
	       main := src;
	    END;
	    src := src^.link;
	 END;
      END Init;

      PROCEDURE WriteMainMacros;
	 VAR
	    src: SourceRef;
	    mac: ARRAY [0..dirsiz-1+2] OF CHAR;
	    fname: ARRAY [0..filenmlen-1] OF CHAR;
      BEGIN
	 IF macroswritten THEN RETURN END;
	 macroswritten := TRUE;
	 Init;
	 IF maincnt = 1 THEN
	    Convert(fname, main^.filename);
	    WriteMacro(prefix, fname);
	 ELSIF maincnt > 1 THEN
	    src := srclist;
	    WHILE src <> NIL DO
	       IF src^.kind IN SourceKindSet{m2main, mrmain} THEN
		  mac := prefix;
		  Convert(fname, src^.filename);
		  StrCat(mac, "_");
		  StrCat(mac, src^.filename.basename);
		  WriteMacro(mac, fname);
	       END;
	       src := src^.link;
	    END;
	    WriteString("T ="); WriteTab;
	    src := srclist;
	    WHILE src <> NIL DO
	       IF src^.kind IN SourceKindSet{m2main, mrmain} THEN
		  WriteString("$(");
		  WriteString(prefix); Write("_");
		  WriteString(src^.filename.basename);
		  WriteString(") ");
	       END;
	       src := src^.link;
	    END;
	    WriteLn;
	 END;
      END WriteMainMacros;

      PROCEDURE WriteMain(src: SourceRef);
      BEGIN
	 Init;
	 IF maincnt = 1 THEN
	    WriteString("$T");
	 ELSE
	    WriteString("$(");
	    WriteString(prefix); Write("_");
	    WriteString(src^.filename.basename);
	    Write(")");
	 END;
      END WriteMain;

   BEGIN
      init := FALSE;
      macroswritten := FALSE;
   END Mains;

   PROCEDURE PrintAll;
      VAR
	 src: SourceRef;
	 mainfound: BOOLEAN;
   BEGIN
      IF library AND NOT LIBdefined THEN
	 WriteMacro("LIB", "lib.a");
	 IF profile THEN
	    WriteMacro("PLIB", "plib.a");
	 END;
	 LIBdefined := TRUE;
      END;
      WriteMainMacros;
      WriteString("all:"); WriteTab;
      mainfound := FALSE;
      src := srclist;
      WHILE src <> NIL DO
	 WITH src^ DO
	    IF (kind = mrmain) OR (kind = m2main) THEN
	       mainfound := TRUE;
	    ELSIF (kind IN defs) AND NOT extern AND NOT associated THEN
	       IF SYMarchive THEN
		  WriteArchiveAndSuffix(filename, "SYM", Om2def);
	       ELSE
		  WriteNameAndSuffix(filename, Om2def);
	       END;
	       Write(" ");
	    END;
	 END;
	 src := src^.link;
      END;

      IF library THEN
	 WriteString("$(LIB)");
	 IF profile THEN
	    WriteString(" $(PLIB)");
	 END;
      END;
      IF mainfound THEN
	 Write(" "); WriteString("$T");
      ELSIF NOT library THEN
	 src := srclist;
	 WHILE src <> NIL DO
	    WITH src^ DO
	       IF (kind = mrimpl) OR (kind = m2impl) THEN
		  Write(" ");
		  WriteNameAndSuffix(filename, Om2impl);
	       END;
	    END;
	    src := src^.link;
	 END;
      END;
      WriteLn;
   END PrintAll;

   PROCEDURE PrintPrecious;
   BEGIN
      IF library AND NOT LIBdefined THEN
	 WriteMacro("LIB", "lib.a");
	 IF profile THEN
	    WriteMacro("PLIB", "plib.a");
	 END;
	 LIBdefined := TRUE;
      END;
      IF library OR SYMarchive THEN
	 WriteString(".PRECIOUS:"); WriteTab;
	 IF library THEN
	    WriteString("$(LIB)");
	    IF profile THEN
	       WriteString(" $(PLIB)");
	    END;
	 END;
	 IF SYMarchive THEN
	    Write(" "); WriteString("SYM");
	 END;
	 WriteLn;
      END;
   END PrintPrecious;

   PROCEDURE PrintLinkage;
      VAR
	 src: SourceRef;
	 objsuffix: ARRAY [0..maxsuffix-1] OF CHAR;
	 closure: ModuleSet;
	 itself: ModuleRange;
   BEGIN
      WriteMainMacros;
      IF library AND NOT LIBdefined THEN
	 WriteMacro("LIB", "lib.a");
	 IF profile THEN
	    WriteMacro("PLIB", "plib.a");
	 END;
	 LIBdefined := TRUE;
      END;
      WriteMacro("LIBS", "");
      WriteMacro("LDFLAGS", "");


      SourceKindToSuffix(m2impl, objsuffix);
      src := srclist;
      WHILE src <> NIL DO
	 WITH src^ DO
	    IF (kind = m2main) OR (kind = mrmain) THEN
	       WriteMain(src); Write(":");
	       Closure(depset, closure);
	       IF NOT library THEN
		  IF (kind = m2main) AND LookForMod(modulename, itself) OR
		     (kind = mrmain) AND LookForModR(modulename, itself) THEN
		     INCL(closure, itself);
		  END;
	       END;
	       WriteTab;
	       IF library THEN
		  WriteNameAndSuffix(filename, objsuffix);
		  WriteModuleSetInArchive(closure, "$(LIB)", objsuffix);
	       ELSE
		  WriteModuleSet(closure, objsuffix);
	       END;
	       WriteLn;
	       WriteTab;
	       IF kind = mrmain THEN
		  WriteString("$(MRC) $(MRFLAGS)");
	       ELSE
		  WriteString("$(M2C) $(MFLAGS)");
	       END;
	       Write(" "); WriteString("-o $@ $(LDFLAGS) ");
	       IF library THEN
		  WriteNameAndSuffix(filename, objsuffix);
		  Write(" "); WriteString("$(LIB)");
	       ELSE
		  WriteModuleSet(closure, objsuffix);
	       END;
	       Write(" "); WriteString("$(LIBS)"); WriteLn;
	    END;
	 END;
	 src := src^.link;
      END;
   END PrintLinkage;

   PROCEDURE PrintDependencies;
      CONST SYM = "SYM";
      VAR
	 src: SourceRef;
	 suffix, symsuffix: ARRAY [0..maxsuffix-1] OF CHAR;

      PROCEDURE PrintLibDep(libname: ARRAY OF CHAR);
      BEGIN
	 WriteString(libname); Write(":"); WriteTab;
	 src := srclist;
	 WHILE src <> NIL DO
	    WITH src^ DO
	       IF (kind = m2impl) OR (kind = mrimpl) THEN
		  SourceKindToSuffix(kind, suffix);
		  Write(" ");
		  WriteArchiveAndSuffix(filename, libname, suffix);
	       END;
	    END;
	    src := src^.link;
	 END;
	 WriteLn;
      END PrintLibDep;

      PROCEDURE PrintObjOfLibDep(libname: ARRAY OF CHAR;
				 profile: BOOLEAN;
				 src: SourceRef);
	 VAR
	    Ssuffix, Osuffix: ARRAY [0..maxsuffix-1] OF CHAR;
      BEGIN
	 WITH src^ DO
	    (* kind = m2impl or kind = mrimpl *)
	    SourceKindToSuffix(kind, Osuffix);
	    WriteArchiveAndSuffix(filename, libname, Osuffix);
	    Write(":"); WriteTab;
	    IF SYMarchive THEN
	       WriteSortedModuleSetInArchive(depset, SYM, symsuffix);
	    ELSE
	       WriteModuleSet(depset, symsuffix);
	    END;
	    IF kind = m2impl THEN
	       Ssuffix := Sm2impl;
	    ELSE
	       Ssuffix := Smrimpl;
	    END;
	    Write(" "); WriteNameAndSuffix(filename, Ssuffix);
	    WriteLn; WriteTab;
	    IF kind = m2impl THEN
	       WriteString("$(M2C) $(MFLAGS) ");
	    ELSE
	       WriteString("$(MRC) $(MRFLAGS) ");
	    END;
	    WriteString("-c ");
	    IF profile THEN
	       WriteString("-p ");
	    END;
	    WriteNameAndSuffix(filename, Ssuffix); WriteLn;
	    WriteTab; WriteString("ar rv ");
	    WriteString(libname); Write(" ");
	    WriteNameAndSuffix(filename, Osuffix);
	    Write(" "); WriteString("&& rm -f ");
	    WriteNameAndSuffix(filename, Osuffix);
	    WriteLn;
	 END;
      END PrintObjOfLibDep;

   BEGIN
      SourceKindToSuffix(m2def, symsuffix);

      IF SYMarchive AND aronce THEN
	 WriteString(SYM); Write(":"); WriteTab;
	 WriteSortedModuleSetInArchive(defset, SYM, symsuffix);
	 WriteLn;
	 WriteTab; WriteString("$(M2C) -a $(MFLAGS) $(?:.");
	    WriteString(symsuffix); WriteString("=.d)"); WriteLn;
	 WriteTab; WriteString("ar rv "); WriteString(SYM);
	    WriteString(" $?"); WriteLn;
	 WriteTab; WriteString("rm $?"); WriteLn; WriteLn;
      END;

      IF library THEN
	 PrintLibDep("$(LIB)");
	 IF profile THEN
	    PrintLibDep("$(PLIB)");
	 END;
      END;

      src := srclist;
      WHILE src <> NIL DO
	 WITH src^ DO
	    IF NOT extern THEN
	       IF library AND ((kind = m2impl) OR (kind = mrimpl)) THEN
		  PrintObjOfLibDep("$(LIB)", (* profile = *) FALSE, src);
		  IF profile THEN
		     PrintObjOfLibDep("$(PLIB)", (* profile = *) TRUE, src);
		  END;
	       ELSE
		  SourceKindToSuffix(kind, suffix);
		  IF SYMarchive AND (kind IN defs) THEN
		     WriteArchiveAndSuffix(filename, SYM, suffix);
		  ELSE
		     WriteNameAndSuffix(filename, suffix);
		  END;
		  Write(":"); WriteTab;
		  IF SYMarchive THEN
		     IF aronce AND NOT (kind IN defs) THEN
			IF depset <> ModuleSet{} THEN
			   WriteString(SYM);
			END;
		     ELSE
			IF kind IN defs THEN
			   IF kind = m2def THEN
			      suffix := Sm2def;
			   ELSE
			      suffix := Smrdef;
			   END;
			END;
			IF kind IN defs THEN
			   WriteNameAndSuffix(filename, suffix);
			END;
			WriteSortedModuleSetInArchive(depset, SYM, symsuffix);
			IF kind IN defs THEN
			   WriteLn; WriteTab;
			   IF kind = m2def THEN
			      WriteString("$(M2C) $(MFLAGS) ");
			   ELSE
			      WriteString("$(MRC) $(MRFLAGS) ");
			   END;
			   WriteNameAndSuffix(filename, suffix);
			END;
		     END;
		  ELSE
		     WriteModuleSet(depset, symsuffix);
		  END;
		  WriteLn;
	       END;
	    END;
	 END;
	 src := src^.link;
      END;
   END PrintDependencies;

BEGIN
   LIBdefined := FALSE;
END Makefile.
