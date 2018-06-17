(* Modula-2 makefile generator - UNIX System V - AFB 3/87 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
MODULE mmm;

   FROM Arguments IMPORT GetFlag, InitArgs, Usage, GetArg, FetchString;
   FROM SymTab IMPORT InitSymTab, InitModules, CheckFileNames;
   FROM FileNames IMPORT filenmlen, EnterFileName, FileName,
      ConvertFileName;
   FROM Errors IMPORT Fatal, Errors, Error, Warning;
   FROM Sources IMPORT ScanSources;
   FROM Library IMPORT ScanLibraries;
   FROM TopSort IMPORT TopSort, PrintSortedArgs, PrintDepsForTsort,
      ProcCallsTopSort;
   FROM Makefile IMPORT PrintRules, PrintLinkage, PrintDependencies;
   FROM Options IMPORT SYMarchive, aronce, versmanag, VersionsManagement,
      library, scanlibs, profile, lookforenv;
   FROM Update IMPORT FirstMakefile, Update, FirstScan;
   FROM Suffix IMPORT Sm2impl, Sm2def, Smrimpl, Smrdef;
   FROM SysExit IMPORT Exit;
   FROM Files IMPORT Delete, Rename, Done;
   FROM Environment IMPORT GetEnv;
   FROM Strings IMPORT StrCat, StrCpy, StrCmp;
   FROM StdIO IMPORT Fopen, read, Fclose, FILE;

   CONST
      usage = "[-aelLmtP1] [-v(r|s|-)] [-p file] [(-c|-C|-u) file] file...";
   VAR
      create: BOOLEAN;		(* if on: create new makefile *)
      update: BOOLEAN;		(* if on: update given makefile *)
      proto: BOOLEAN;		(* if on: protofile given *)
      noproto: BOOLEAN;		(* if on: don't look for proto file *)
      lookforSRC: BOOLEAN;	(* look for SRC-macro on update *)
      topsort: BOOLEAN;		(* if on: give arguments in topsorted order *)
      psort: BOOLEAN;           (* if on: report calling reference cycles *)
      tsortout: BOOLEAN;	(* if on: produce output suitable for tsort *)
      protofile, updatefile: ARRAY [0..filenmlen-1] OF CHAR;

   PROCEDURE WorkupArguments;
      VAR
	 flag: CHAR;
	 filenm: ARRAY [0..filenmlen-1] OF CHAR;
	 arg: ARRAY [0..3] OF CHAR;
   BEGIN
      create := FALSE; update := FALSE; proto := FALSE; noproto := FALSE;

      InitArgs(usage);
      WHILE GetFlag(flag) DO
	 CASE flag OF
	 | 'a', 'e', '1', 'l', 'L', 'm', 't', 'T', 'P': (* later *)
	 | 'C': FetchString(updatefile); create := TRUE; noproto := TRUE;
	 | 'c': FetchString(updatefile); create := TRUE;
	 | 'p': FetchString(protofile); proto := TRUE;
	 | 'u': FetchString(updatefile); update := TRUE;
	 | 'v': FetchString(arg); (* later *)
	 ELSE
	    Usage;
	 END;
      END;

      IF update AND create THEN
	 Fatal("-u and -c", "cannot be combined");
      END;
      IF update AND proto THEN
	 Fatal("-u and -p", "cannot be combined");
      END;
      IF proto AND NOT create THEN
	 Fatal("-p", "cannot be given without -c");
      END;
      IF proto AND noproto THEN
	 Fatal("-C", "cannot be combined with -p");
      END;

      lookforSRC := TRUE;
      WHILE GetArg(filenm) DO
	 EnterFileName(filenm);
	 lookforSRC := FALSE;
      END;
   END WorkupArguments;

   PROCEDURE WorkupFlags;
      VAR
	 flag: CHAR;
	 arg: ARRAY [0..filenmlen-1] OF CHAR;
   BEGIN
      topsort := FALSE; tsortout := FALSE; psort := FALSE;
      InitArgs(usage);
      WHILE GetFlag(flag) DO
	 CASE flag OF
	 | 'C', 'c', 'p', 'u': FetchString(arg); (* already done *)
	 | 'a': SYMarchive := NOT SYMarchive;
	 | 'e': lookforenv := NOT lookforenv;
	 | 'l': library := NOT library;
	 | 'L': scanlibs := NOT scanlibs;
	 | 'm': profile := NOT profile;
	 | 'P': psort := TRUE;
	 | 't': topsort := TRUE;
	 | 'T': tsortout := TRUE;
	 | 'v': FetchString(arg);
		CASE arg[0] OF
		| 'r': versmanag := rcs;
		| 's': versmanag := sccs;
		| '-': versmanag := none;
		ELSE
		  Usage;
		END;
	 | '1': aronce := NOT aronce;
	 ELSE
	    Usage;
	 END;
      END;
      IF topsort AND tsortout THEN
	 Fatal("-t", "cannot be combined with -T");
      END;
      IF topsort AND (create OR update) THEN
	 Fatal("-t", "cannot be given together with -c or -u");
      END;
      IF tsortout AND (create OR update) THEN
	 Fatal("-T", "cannot be given together with -c or -u");
      END;
      IF psort AND (create OR update) THEN
	 Fatal("-P", "cannot be given together with -c or -u");
      END;
      IF psort AND (topsort OR tsortout) THEN
	 Fatal("-P", "cannot be given together with -t or -T");
      END;
      IF profile AND NOT library THEN
	 Warning("-m", "implies -l");
	 library := TRUE;
      END;
   END WorkupFlags;

   PROCEDURE LookForProtoFile(VAR protofile: ARRAY OF CHAR) : BOOLEAN;
      VAR
	 ok: BOOLEAN;

      PROCEDURE Readable(file: ARRAY OF CHAR) : BOOLEAN;
	 VAR fp: FILE;
      BEGIN
	 IF Fopen(fp, file, read, (* buffered = *) FALSE) THEN
	    IF NOT Fclose(fp) THEN END;
	    RETURN TRUE
	 END;
	 RETURN FALSE
      END Readable;

   BEGIN
      GetEnv("HOME", protofile, ok);
      IF ok THEN
	 StrCat(protofile, "/.mmm_proto");
	 IF Readable(protofile) THEN RETURN TRUE END;
      END;
      StrCpy(protofile, "/u/lib/mmm_proto");
      IF Readable(protofile) THEN RETURN TRUE END;
      StrCpy(protofile, "/usr/lib/mmm_proto");
      IF Readable(protofile) THEN RETURN TRUE END;
      GetEnv("MODLIB", protofile, ok);
      IF ok THEN
	 StrCat(protofile, "/mmm_proto");
	 IF Readable(protofile) THEN RETURN TRUE END;
      END;
      RETURN FALSE
   END LookForProtoFile;

   PROCEDURE CheckForSourceSuffix(filename: ARRAY OF CHAR) : BOOLEAN;
      VAR
	 fn: FileName;

      PROCEDURE Equal(s1, s2: ARRAY OF CHAR) : BOOLEAN;
      BEGIN
	 RETURN StrCmp(s1, s2) = 0
      END Equal;

   BEGIN
      ConvertFileName(filename, fn);
      WITH fn DO
	 RETURN Equal(suffix, Sm2impl) OR Equal(suffix, Sm2def) OR
	        Equal(suffix, Smrimpl) OR Equal(suffix, Smrdef)
      END;
   END CheckForSourceSuffix;

BEGIN
   WorkupArguments;

   IF update OR create THEN
      (* check against mmm -c *.d *.m2 *)
      IF CheckForSourceSuffix(updatefile) THEN
	 Fatal(updatefile, "not destroyed. Specify a makefile to -u or -c.");
      END;
   END;

   IF create AND NOT noproto AND NOT proto AND LookForProtoFile(protofile) THEN
      proto := TRUE;
   END;
   IF update THEN
      FirstScan(updatefile, lookforSRC);
   ELSIF proto THEN
      FirstScan(protofile, (* lookforSRC = *) FALSE);
   END;
   WorkupFlags;

   IF scanlibs THEN
      ScanLibraries;
   END;
   InitSymTab;
   ScanSources;
   InitModules;
   CheckFileNames;
   TopSort;

   IF update THEN
      Update(updatefile, "mmm_tmp");
      IF Errors() = 0 THEN
	 Rename("mmm_tmp", updatefile);
      ELSE
	 Delete("mmm_tmp");
      END;
      IF NOT Done THEN Error(updatefile, "cannot update") END;
   ELSIF proto THEN
      Update(protofile, updatefile);
   ELSIF create THEN
      FirstMakefile(updatefile);
   ELSIF topsort THEN
      PrintSortedArgs;
   ELSIF tsortout THEN
      PrintDepsForTsort;
   ELSIF psort THEN
      ProcCallsTopSort;
   ELSE
      PrintRules;
      PrintLinkage;
      PrintDependencies;
   END;

   Exit(Errors());
END mmm.
