(* Modula-2 makefile generator - UNIX System V - AFB 3/87 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
IMPLEMENTATION MODULE Write;

   FROM SymTab IMPORT ModuleSet, mtab, ModuleRange, modules, srclist,
      SourceRef;
   FROM TopSort IMPORT defindex, defmodules;
   FROM FileNames IMPORT filenmlen, FileName, ConstructFileName,
      ConstructArchiveName;
   FROM Strings IMPORT StrCpy;
   FROM Out IMPORT Write, WriteLn, WriteString;

   PROCEDURE WriteNameAndSuffix(fn: FileName; suffix: ARRAY OF CHAR);
      VAR filename: ARRAY[0..filenmlen-1] OF CHAR;
   BEGIN
      StrCpy(fn.suffix, suffix);
      ConstructFileName(filename, fn);
      WriteString(filename);
   END WriteNameAndSuffix;

   PROCEDURE WriteArchiveAndSuffix(fn: FileName;
                                   archive, suffix: ARRAY OF CHAR);
      VAR filename: ARRAY[0..filenmlen-1] OF CHAR;
   BEGIN
      StrCpy(fn.suffix, suffix);
      ConstructArchiveName(filename, archive, fn);
      WriteString(filename);
   END WriteArchiveAndSuffix;

   PROCEDURE WriteModuleSet(mset: ModuleSet; suffix: ARRAY OF CHAR);
      VAR module: ModuleRange;
   BEGIN
      IF modules > 0 THEN
	 FOR module := MIN(ModuleRange) TO modules-1 DO
	    IF module IN mset THEN
	       Write(" ");
	       WriteNameAndSuffix(mtab[module]^.filename, suffix);
	    END;
	 END;
      END;
   END WriteModuleSet;

   PROCEDURE WriteModuleSetInArchive(mset: ModuleSet;
                                     archive, suffix: ARRAY OF CHAR);
      VAR module: ModuleRange;
   BEGIN
      IF modules > 0 THEN
	 FOR module := MIN(ModuleRange) TO modules-1 DO
	    IF module IN mset THEN
	       Write(" ");
	       IF mtab[module]^.extern THEN
		  WriteNameAndSuffix(mtab[module]^.filename, suffix);
	       ELSE
		  WriteArchiveAndSuffix(mtab[module]^.filename,
		                        archive, suffix);
	       END;
	    END;
	 END;
      END;
   END WriteModuleSetInArchive;

   PROCEDURE WriteSortedModuleSetInArchive(mset: ModuleSet;
                                           archive, sfx: ARRAY OF CHAR);
      VAR module: ModuleRange;
   BEGIN
      IF defmodules > 0 THEN
	 FOR module := MIN(ModuleRange) TO defmodules-1 DO
	    IF defindex[module] IN mset THEN
	       Write(" ");
	       WITH mtab[defindex[module]]^ DO
		  IF extern THEN
		     WriteNameAndSuffix(filename, sfx);
		  ELSE
		     WriteArchiveAndSuffix(filename, archive, sfx);
		  END;
	       END;
	    END;
	 END;
      END;
   END WriteSortedModuleSetInArchive;

END Write.
