(* Modula-2 makefile generator - UNIX System V - AFB 3/87 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
IMPLEMENTATION MODULE Library;

   FROM Errors IMPORT Warning;
   FROM Environment IMPORT GetEnv;
   FROM FileNames IMPORT filenmlen, ConvertFileName, FileName, maxsuffix;
   FROM Suffix IMPORT SourceKindToSuffix;
   FROM SymTab IMPORT SourceKind;
   FROM Strings IMPORT StrCat, StrCpy, StrCmp, StrLen;
   FROM StrSpec IMPORT StrPartCpy;
   FROM SystemTypes IMPORT DirSize;
   FROM FtdIO IMPORT Fread, Done;
   FROM StdIO IMPORT FILE, Fopen, read, Fclose;
   FROM SYSTEM IMPORT ADR;
   FROM Directories IMPORT DIR, Direct, OpenDir, ReadDir, CloseDir;
   IMPORT Archive, StdIO, SymTab, FileNames, Errors;

   MODULE SymFile;

      (* symbol file reading: get module name *)

      FROM Archive IMPORT AFILE, ArchiveOpen, ArchiveClose, ArchiveRead,
	 ArchiveReopen, AStat, ArchiveStat;
      FROM StdIO IMPORT FILE, Fopen, read, Fclose, Fgetc;
      FROM Errors IMPORT Warning;
      FROM FileNames IMPORT EnterExtern, FileName;
      FROM SymTab IMPORT ModuleName;

      EXPORT DoSymbolfile, DoSYMArchive;

      CONST
         symFileKeyM2 = 107B;
         symFileKeyMR = 40B;
      TYPE
	 ReadProc = PROCEDURE(VAR CHAR) : BOOLEAN;
	 Byte = [0..377B];
	 SymFileSymbols =
	   (endfileSS,
	    unitSS, endunitSS,
	    importSS, exportSS,
	    constSS, normalconstSS, realconstSS, stringconstSS, bigsetconstSS,
	    intcarconstSS,
	    typSS, arraytypSS, recordtypSS, settypSS, pointertypSS, hiddentypSS,
	    varSS,
	    procSS, funcSS,
	    identSS,
	    periodSS, colonSS, rangeSS,
	    lparentSS, rparentSS,
	    lbracketSS, rbracketSS,
	    caseSS, ofSS, elseSS, endSS,
	    relationtypSS, transSS, lrelbrSS, rrelbrSS, inSS, inoutSS, dbSS);
      VAR
	 symfp: FILE;
	 afile: AFILE;
	 Read: ReadProc;
	 eof: BOOLEAN;
	 highbyte: BOOLEAN;
	 buffword: CARDINAL;

      PROCEDURE Init;
      BEGIN
	 eof := FALSE;
	 highbyte := FALSE;
      END Init;

      PROCEDURE ReadFromArchive(VAR ch: CHAR) : BOOLEAN;
      BEGIN
	 RETURN ArchiveRead(afile, ch)
      END ReadFromArchive;

      PROCEDURE ReadFromFile(VAR ch: CHAR) : BOOLEAN;
      BEGIN
	 RETURN Fgetc(ch, symfp)
      END ReadFromFile;

      PROCEDURE ReadByte(VAR b: Byte);
         VAR ch: CHAR;
      BEGIN 
	 IF eof THEN
	    b := ORD(endfileSS);
	 ELSE
	    IF highbyte THEN 
	       b := buffword DIV 400B;
	    ELSE 
	       IF Read(ch) THEN
		  buffword := ORD(ch) * 400B;
		  IF Read(ch) THEN
		     buffword := buffword + ORD(ch);
		     b := buffword MOD 400B;
		  ELSE
		     b := buffword DIV 400B;
		     eof := TRUE;
		  END;
	       ELSE
		  eof := TRUE;
		  b := Byte(endfileSS);
	       END;
	    END;
	    highbyte := NOT highbyte;
	 END;
      END ReadByte;

      PROCEDURE ReadSym(VAR sym: SymFileSymbols);
	 VAR byte: Byte;
      BEGIN
	 ReadByte(byte);
	 sym := VAL(SymFileSymbols, byte);
      END ReadSym;

      PROCEDURE SymFileOK(VAR modR: BOOLEAN): BOOLEAN;
         VAR 
            b   : Byte;
            key : CARDINAL;
      BEGIN 
         ReadByte(b);
         IF b = ORD(normalconstSS) THEN 
	    ReadByte(b); IF b <> 0 THEN RETURN FALSE END;
	    ReadByte(b); IF b <> 0 THEN RETURN FALSE END;
            ReadByte(b); key := b;
            ReadByte(b); key := key * 400B + b;
	    modR := key = symFileKeyMR;
            RETURN (key = symFileKeyM2) OR (key = symFileKeyMR);
         ELSE 
            RETURN FALSE;
         END;
      END SymFileOK;

      PROCEDURE ReadValue;
	 VAR b: Byte; sy: SymFileSymbols;
      BEGIN
	 ReadSym(sy);
	 ReadByte(b); ReadByte(b); ReadByte(b); ReadByte(b);
      END ReadValue;

      PROCEDURE ReadIdent(VAR ident: ARRAY OF CHAR);
	 VAR
	    sy: SymFileSymbols;
	    index: CARDINAL;
	    b: Byte;
      BEGIN
	 ReadSym(sy);
	 IF sy = identSS THEN
	    index := 0;
	    ReadByte(b);
	    WHILE b <> 0 DO
	       IF index <= HIGH(ident) THEN
		  ident[index] := VAL(CHAR, b);
		  INC(index);
	       END;
	       ReadByte(b);
	    END;
	    IF index <= HIGH(ident) THEN
	       ident[index] := 0C;
	    END;
	 ELSE
	    ident[0] := 0C;
	 END;
      END ReadIdent;

      PROCEDURE GetModuleIdent(VAR ident: ARRAY OF CHAR);
	 VAR sy: SymFileSymbols;
      BEGIN
	 (* ReadValue; ReadValue; ReadValue; (* ModuleKey *) *)
	 ReadIdent(ident);
      END GetModuleIdent;

      PROCEDURE DoSymbolfile(filename: ARRAY OF CHAR;
			     fname: FileName);
	 VAR
	    modulename: ModuleName;
	    modR: BOOLEAN;
      BEGIN
	 Init; Read := ReadFromFile;
	 IF Fopen(symfp, filename, read, (* buffered = *) TRUE) THEN
	    IF SymFileOK(modR) THEN
	       GetModuleIdent(modulename);
	       EnterExtern(fname, "", modR, modulename);
	    ELSE
	       Warning(filename, "no valid symbol file");
	    END;
	 ELSE
	    Warning(filename, "cannot open for reading");
	 END;
      END DoSymbolfile;

      PROCEDURE DoSYMArchive(filename: ARRAY OF CHAR;
			     VAR fname: FileName);
	 VAR
	    statbuf: AStat;
	    modulename: ModuleName;
	    modR: BOOLEAN;
      BEGIN
	 Read := ReadFromArchive;
	 IF ArchiveOpen(afile, filename, "") THEN
	    REPEAT
	       Init;
	       ArchiveStat(afile, statbuf);
	       IF SymFileOK(modR) THEN
		  GetModuleIdent(modulename);
		  EnterExtern(fname, statbuf.name, modR, modulename);
	       ELSE
		  Warning(filename, "no valid symbol file");
	       END;
	    UNTIL NOT ArchiveReopen(afile, "");
	 ELSE
	    Warning(filename, "cannot open for reading / no archive");
	 END;
      END DoSYMArchive;

   END SymFile;

   PROCEDURE ScanLibrary(directory: ARRAY OF CHAR);
      TYPE
	 File = ARRAY[0..DirSize-1] OF CHAR;
	 FileRef = POINTER TO File;
      VAR
	 filename: ARRAY[0..filenmlen-1] OF CHAR;
	 fname: FileName;
	 dirbuf: Direct;
	 dirfile: File;
	 len: CARDINAL;
	 symsuffix: ARRAY[0..maxsuffix-1] OF CHAR;
	 dirp: DIR;

      PROCEDURE ReadFile() : BOOLEAN;
	 VAR fref: FileRef;
      BEGIN
	 LOOP
	    
	    IF ReadDir(dirp, dirbuf) THEN
	       IF (dirbuf.name[0] <> 0C)  THEN
		  fref := FileRef(ADR(dirbuf.name));
		  dirfile := fref^;
		  IF (StrCmp(dirfile, ".") <> 0) AND
		     (StrCmp(dirfile, "..") <> 0) THEN
		     RETURN TRUE
		  END;
	       END;
	    ELSE
	       RETURN FALSE
	    END;
	 END;
      END ReadFile;

   BEGIN
      IF NOT OpenDir(dirp, directory) THEN
	 (* Warning(directory, "cannot open for reading"); *)
	 RETURN
      END;

      StrCpy(filename, directory);
      StrCat(filename, "/");
      len := StrLen(filename);
      SourceKindToSuffix(m2def, symsuffix);

      WHILE ReadFile() DO
	 StrCat(filename, dirfile);
	 ConvertFileName(filename, fname);

	 WITH fname DO
	    IF suffix[0] = 0C THEN
	       IF StrCmp(basename, "SYM") = 0 THEN
		  DoSYMArchive(filename, fname);
	       END;
	    ELSIF StrCmp(suffix, symsuffix) = 0 THEN
	       DoSymbolfile(filename, fname);
	    (*
	    ELSIF StrCmp(suffix, Archive) = 0 THEN
	       DoLib(filename);
	    *)
	    END;
	 END;

	 IF len <= HIGH(filename) THEN
	    filename[len] := 0C;
	 END;
      END;
      CloseDir(dirp);
   END ScanLibrary;

   PROCEDURE ScanLibraries;
      CONST
	 MODPATH = "MODPATH";
	 MODRPATH = "MODRPATH";
	 MODLIB = "MODLIB";
	 MODRLIB = "MODRLIB";
      VAR
	 libdir: ARRAY[0..filenmlen-1] OF CHAR;
	 ok: BOOLEAN;

      PROCEDURE DoPath(pathname: ARRAY OF CHAR);
	 TYPE
	    CharSet = SET OF CHAR;
	 CONST
	    delim = CharSet{0C, ':'};
	 VAR
	    path: ARRAY[0..2047] OF CHAR;
	    ok: BOOLEAN;
	    start, index: CARDINAL;
	    stop: BOOLEAN;
      BEGIN
	 GetEnv(pathname, path, ok);
	 IF ok THEN
	    start := 0; index := 0; stop := FALSE;
	    WHILE (index <= HIGH(path)) AND NOT stop DO
	       IF path[index] IN delim THEN
		  IF (index = start) OR
		     (index = start+1) AND (path[start] = '.') THEN
		     (* exclude current directory *)
		  ELSE
		     StrPartCpy(libdir, path, start, index-start);
		     ScanLibrary(libdir);
		  END;
		  start := index+1;
	       END;
	       stop := path[index] = 0C;
	       INC(index);
	    END;
	 END;
      END DoPath;

   BEGIN
      DoPath(MODPATH); DoPath(MODRPATH);
      GetEnv(MODRLIB, libdir, ok);
      IF ok THEN
	 ScanLibrary(libdir);
      ELSE
	 ScanLibrary("/u/lib/modr");
	 ScanLibrary("/usr/lib/modr");
      END;
      GetEnv(MODLIB, libdir, ok);
      IF ok THEN
	 ScanLibrary(libdir);
      ELSE
	 ScanLibrary("/u/lib/modula");
	 ScanLibrary("/usr/lib/modula");
      END;
   END ScanLibraries;

END Library.
