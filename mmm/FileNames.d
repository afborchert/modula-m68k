(* Modula-2 makefile generator - UNIX System V - AFB 3/87 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
DEFINITION MODULE FileNames;

   FROM StdIO IMPORT FILE;

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

   PROCEDURE ConvertFileName(filename: ARRAY OF CHAR;
			     VAR newfn: FileName);

   PROCEDURE EnterFileName(filename: ARRAY OF CHAR);

   PROCEDURE EnterExtern(newfn: FileName; member: ARRAY OF CHAR;
			 modR: BOOLEAN;
			 modulename: ARRAY OF CHAR);

   PROCEDURE GetFileName(VAR filename: FileName; VAR extern: BOOLEAN;
			 modR: BOOLEAN;
			 VAR modulename: ARRAY OF CHAR) : BOOLEAN;

   PROCEDURE WriteFileName(fp: FILE; filename: FileName);

   PROCEDURE ConstructFileName(VAR fn: ARRAY OF CHAR; filename: FileName);

   PROCEDURE ConstructArchiveName(VAR fn: ARRAY OF CHAR;
				  archive: ARRAY OF CHAR;
				  filename: FileName);

END FileNames.
