(* Modula-2 makefile generator - UNIX System V - AFB 3/87 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
IMPLEMENTATION MODULE FileNames; (* AFB 2/88 *)

   FROM Errors IMPORT Warning, Error;
   FROM Strings IMPORT StrLen, StrCpy, StrCmp, StrCat;
   FROM StrSpec IMPORT StrPartCpy;
   FROM SymTab IMPORT ModuleName;
   FROM StdIO IMPORT FILE;
   FROM FtdIO IMPORT FwriteString, FwriteChar;
   FROM Storage IMPORT ALLOCATE, DEALLOCATE;

   (* (* exported from definition module *)
   CONST
      filenmlen = 512;		(* maximal file name length *)
      dirsiz    = 14;		(* maximal file name length in directory *)
      maxsuffix = 3;		(* maximal suffix length *)

   TYPE
      FileName = 
	 RECORD
	    dirname: ARRAY [0..filenmlen-1] OF CHAR;
	    archive: ARRAY [0..dirsiz-1] OF CHAR;
	    basename: ARRAY [0..dirsiz-1] OF CHAR;
	    suffix: ARRAY [0..maxsuffix-1] OF CHAR;
	 END;
   *)

   TYPE
      FileList = POINTER TO File;
      File =
	 RECORD
	    filename: FileName;
	    CASE extern: BOOLEAN OF
	    | TRUE: mname: ModuleName; modulaR: BOOLEAN;
	    END;
	    link: FileList;
	 END;
   VAR
      filelist: FileList;
      sorted: BOOLEAN;

   PROCEDURE ConvertFileName(filename: ARRAY OF CHAR;
			     VAR newfn: FileName);
      VAR
	 new: FileList;
	 index: CARDINAL;
	 suffixlen: CARDINAL;
	 fnlen: CARDINAL;

      PROCEDURE Find(VAR i: CARDINAL; ch: CHAR) : BOOLEAN;
	 VAR index: CARDINAL;
      BEGIN
	 FOR index := i TO 0 BY -1 DO
	    IF filename[index] = ch THEN i := index; RETURN TRUE END;
	    IF filename[index] = '/' THEN i := index; RETURN FALSE END;
	 END;
	 i := 0;
	 RETURN FALSE
      END Find;

   BEGIN
      WITH newfn DO
	 dirname[0] := 0C;
	 archive[0] := 0C;
	 basename[0] := 0C;
	 suffix[0] := 0C;
      END;
      IF filename[0] = 0C THEN
	 RETURN
      END;
      WITH newfn DO
	 fnlen := StrLen(filename);
	 index := fnlen - 1;
	 IF Find(index, '.') THEN
	    suffixlen := fnlen - index - 1;
	    IF suffixlen > 0 THEN
	       StrPartCpy(suffix, filename, index+1, suffixlen);
	       filename[index] := 0C; (* zap suffix *)
	    END;
	 END;
	 IF Find(index, '/') THEN
	    StrPartCpy(basename, filename, index+1, fnlen-index-1);
	    StrPartCpy(dirname, filename, 0, index); (* without last '/' *)
	 ELSE
	    StrCpy(basename, filename);
	    dirname[0] := 0C;
	 END;
      END;
   END ConvertFileName;

   PROCEDURE EnterFileName(filename: ARRAY OF CHAR);
      VAR
	 new: FileList;
	 index: CARDINAL;
	 suffixlen: CARDINAL;
	 fnlen: CARDINAL;
	 newfn: FileName;

      PROCEDURE Find(VAR i: CARDINAL; ch: CHAR) : BOOLEAN;
	 VAR index: CARDINAL;
      BEGIN
	 FOR index := i TO 0 BY -1 DO
	    IF filename[index] = ch THEN i := index; RETURN TRUE END;
	    IF filename[index] = '/' THEN i := index; RETURN FALSE END;
	 END;
	 i := 0;
	 RETURN FALSE
      END Find;

   BEGIN
      IF filename[0] = 0C THEN
	 Warning(filename, "empty file name ignored"); RETURN
      END;
      WITH newfn DO
	 archive[0] := 0C;
	 fnlen := StrLen(filename);
	 index := fnlen - 1;
	 IF Find(index, '.') THEN
	    suffixlen := fnlen - index - 1;
	    IF (suffixlen > maxsuffix) OR (suffixlen = 0) THEN
	       Error(filename, "unknown suffix"); RETURN
	    END;
	    StrPartCpy(suffix, filename, index+1, suffixlen);
	    filename[index] := 0C; (* zap suffix *)
	 ELSE
	    Error(filename, "suffix missing"); RETURN
	 END;
	 IF Find(index, '/') THEN
	    StrPartCpy(basename, filename, index+1, fnlen-index-1);
	    StrPartCpy(dirname, filename, 0, index); (* without last '/' *)
	 ELSE
	    StrCpy(basename, filename);
	    dirname[0] := 0C;
	 END;
      END;
      NEW(new);
      WITH new^ DO
	 filename := newfn;
	 extern := FALSE;
	 link := filelist;
      END;
      filelist := new;
   END EnterFileName;

   PROCEDURE EnterExtern(newfn: FileName; member: ARRAY OF CHAR;
			 modR: BOOLEAN;
			 modulename: ARRAY OF CHAR);
      VAR
	 new: FileList;
	 index: CARDINAL;

      PROCEDURE LookForExternMod(modulename: ARRAY OF CHAR) : BOOLEAN;
	 (* check for multiple module definitions *)
	 (* whatever comes first remains; that's  *)
	 (* correct because EnterExtern is called *)
	 (* in MODPATH order.                     *)
	 VAR
	    fl: FileList;
      BEGIN
	 fl := filelist;
	 WHILE fl <> NIL DO
	    WITH fl^ DO
	       IF extern AND (StrCmp(modulename, mname) = 0) AND
		  (modulaR = modR) THEN
		  RETURN TRUE
	       END;
	    END;
	    fl := fl^.link;
	 END;
	 RETURN FALSE
      END LookForExternMod;

   BEGIN
      IF LookForExternMod(modulename) THEN RETURN END;
      NEW(new);
      WITH new^ DO
	 filename := newfn;
	 IF member[0] <> 0C THEN
	    WITH filename DO
	       StrCpy(archive, basename);
	       IF suffix[0] <> 0C THEN
		  StrCat(archive, ".");
		  StrCat(archive, suffix);
	       END;
	       index := StrLen(member) - 1;
	       WHILE (index > 0) AND (member[index] <> '.') DO
		  DEC(index);
	       END;
	       IF member[index] = '.' THEN
		  StrPartCpy(suffix, member, index+1, maxsuffix);
		  member[index] := 0C;
	       END;
	       StrCpy(basename, member);
	    END;
	 END;
	 extern := TRUE;
	 modulaR := modR;
	 StrCpy(mname, modulename);
	 link := filelist;
      END;
      filelist := new;
   END EnterExtern;

   PROCEDURE GetFileName(VAR filename: FileName; VAR extern: BOOLEAN;
			 modR: BOOLEAN;
			 VAR modulename: ARRAY OF CHAR) : BOOLEAN;
      VAR old: FileList;
   BEGIN
      IF filelist = NIL THEN RETURN FALSE END;
      IF NOT sorted THEN
	 SortFiles(filelist);
	 sorted := TRUE;
      END;
      filename := filelist^.filename;
      extern := filelist^.extern;
      IF extern THEN
	 StrCpy(modulename, filelist^.mname);
	 modR := filelist^.modulaR;
      END;
      old := filelist;
      filelist := filelist^.link;
      DISPOSE(old);
      RETURN TRUE
   END GetFileName;

   PROCEDURE WriteFileName(fp: FILE; filename: FileName);
   BEGIN
      WITH filename DO
	 IF dirname[0] <> 0C THEN
	    FwriteString(fp, dirname);
	    FwriteChar(fp, "/");
	 END;
	 IF archive[0] <> 0C THEN
	    FwriteString(fp, archive);
	    FwriteChar(fp, "(");
	 END;
	 FwriteString(fp, basename);
	 IF archive[0] <> 0C THEN
	    FwriteChar(fp, ")");
	 END;
	 IF suffix[0] <> 0C THEN
	    FwriteChar(fp, ".");
	    FwriteString(fp, suffix);
	 END;
      END;
   END WriteFileName;

   PROCEDURE ConstructFileName(VAR fn: ARRAY OF CHAR; filename: FileName);
   BEGIN
      WITH filename DO
	 IF dirname[0] <> 0C THEN
	    StrCpy(fn, dirname);
	    StrCat(fn, "/");
	 ELSE
	    fn[0] := 0C;
	 END;
	 IF archive[0] <> 0C THEN
	    StrCat(fn, archive);
	    StrCat(fn, "(");
	 END;
	 StrCat(fn, basename);
	 IF suffix[0] <> 0C THEN
	    StrCat(fn, ".");
	    StrCat(fn, suffix);
	 END;
	 IF archive[0] <> 0C THEN
	    StrCat(fn, ")");
	 END;
      END;
   END ConstructFileName;

   PROCEDURE ConstructArchiveName(VAR fn: ARRAY OF CHAR;
				  archivefile: ARRAY OF CHAR;
				  filename: FileName);
   BEGIN
      WITH filename DO
	 IF dirname[0] <> 0C THEN
	    StrCpy(fn, dirname);
	    StrCat(fn, "/");
	 ELSE
	    fn[0] := 0C;
	 END;
	 StrCat(fn, archivefile);
	 StrCat(fn, "(");
	 StrCat(fn, basename);
	 IF suffix[0] <> 0C THEN
	    StrCat(fn, ".");
	    StrCat(fn, suffix);
	 END;
	 StrCat(fn, ")");
      END;
   END ConstructArchiveName;

   PROCEDURE SortFiles(VAR l: FileList);
      VAR
	 run, end: FileList;
	 elem, prev: FileList;

      PROCEDURE Greater(f1, f2: FileList) : BOOLEAN;
      BEGIN
	 RETURN StrCmp(f1^.filename.basename, f2^.filename.basename) < 0
      END Greater;

      PROCEDURE Insert;
	 (* elem wird zwischen prev und run eingefuegt *)
      BEGIN
	 (* pre: run <> NIL and elem <> NIL *)
	 IF run = end THEN
	    elem^.link := end;
	 ELSE
	    elem^.link := run^.link;
	 END;
	 IF prev = NIL THEN
	    l := elem;
	 ELSE
	    prev^.link := elem;
	 END;
      END Insert;

   BEGIN (* SortFiles *)
      end := NIL;

      WHILE l <> end DO
	 elem := l;
	 l := l^.link;
	 run := l;
	 prev := NIL;

	 WHILE run <> end DO
	    IF Greater(elem, run) THEN
	       Insert;
	       prev := elem;
	       elem := run;
	    ELSE
	       prev := run;
	    END;
	    run := run^.link;
	 END;

	 Insert;
	 end := elem;
      END;
   END SortFiles;

BEGIN
   filelist := NIL;
   sorted := FALSE;
END FileNames.
