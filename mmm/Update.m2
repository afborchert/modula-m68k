(* Modula-2 makefile generator - UNIX System V - AFB 3/87 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
IMPLEMENTATION MODULE Update;

   FROM Errors IMPORT Fatal, Error, Warning;
   FROM FileNames IMPORT filenmlen, EnterFileName;
   FROM Macros IMPORT EnterMacro;
   FROM Makefile IMPORT PrintRules, PrintLinkage, PrintDependencies,
      PrintMacros, PrintAll, PrintPrecious;
   FROM Options IMPORT SYMarchive, aronce, versmanag, VersionsManagement,
      library, scanlibs, profile, lookforenv;
   FROM Out IMPORT WriteString, WriteLn, Write, OpenMakefile, CloseMakefile;
   FROM StdIO IMPORT FILE, Fopen, Fclose, read, Fgetc;
   FROM ASCII IMPORT tab, nl;
   FROM Strings IMPORT StrCmp, StrLen, StrCat, StrCpy;
   FROM StrSpec IMPORT StrPartCpy;
   FROM Calendar IMPORT TimeToString, CurrentTime;

   CONST
      linesize = 512;
   TYPE
      Buffer = ARRAY [0..linesize-1] OF CHAR;
      CharSet = SET OF CHAR;
   VAR
      in: FILE; infilenm: ARRAY[0..filenmlen-1] OF CHAR;
      buf: Buffer;
      again: BOOLEAN; lastres: BOOLEAN;

   PROCEDURE ReadLine() : BOOLEAN;
      VAR
	 index: CARDINAL;
	 ch: CHAR;
   BEGIN
      IF again THEN again := FALSE; RETURN lastres END;

      index := 0;
      WHILE (index <= HIGH(buf)) AND Fgetc(ch, in) AND (ch <> nl) DO
	 buf[index] := ch;
	 INC(index);
      END;
      IF index <= HIGH(buf) THEN
	 buf[index] := 0C;
	 lastres := ch = nl;
	 RETURN lastres
      ELSE
	 Error(infilenm, "line too long");
	 lastres := TRUE;
	 RETURN TRUE
      END;
   END ReadLine;

   PROCEDURE SkipWhite(VAR index: CARDINAL);
      CONST
	 WhiteSpace = CharSet{' ', tab};
   BEGIN
      WHILE (index <= HIGH(buf)) AND (buf[index] IN WhiteSpace) DO
	 INC(index);
      END;
   END SkipWhite;

   PROCEDURE SkipAlpha(VAR index: CARDINAL);
      CONST
	 Alpha = CharSet{'a'..'z', 'A'..'Z', '0'..'9', '_'};
   BEGIN
      WHILE (index <= HIGH(buf)) AND (buf[index] IN Alpha) DO
	 INC(index);
      END;
   END SkipAlpha;

   PROCEDURE ScanForUpdate(VAR cmd: ARRAY OF CHAR) : BOOLEAN;
      VAR
	 index: CARDINAL;
	 cmdi: CARDINAL;
   BEGIN
      index := 0; SkipWhite(index);
      IF buf[index] <> '#' THEN RETURN FALSE END;
      INC(index); SkipWhite(index);
      IF buf[index] <> '{' THEN RETURN FALSE END;
      INC(index); cmdi := 0;
      WHILE (cmdi <= HIGH(cmd)) AND (index <= HIGH(buf)) AND
	    (buf[index] <> '}') AND (buf[index] <> 0C) DO
	 cmd[cmdi] := buf[index];
	 INC(cmdi); INC(index);
      END;
      IF (index > HIGH(buf)) OR (buf[index] <> '}') THEN
	 Warning(buf, "bad update directive");
	 RETURN FALSE
      END;
      IF cmdi <= HIGH(cmd) THEN
	 cmd[cmdi] := 0C;
      END;
      RETURN TRUE
   END ScanForUpdate;

   PROCEDURE CheckForMacro(VAR macro, val: ARRAY OF CHAR;
			   VAR again: BOOLEAN) : BOOLEAN;
      (* check for macro definition in buf read by ReadLine() *)
      (* return macro=val and TRUE on success                 *)
      (* set again if something is found which is to be       *)
      (*     considered again (i.e. an update directive)      *)
      (* multiple line macro definitions are correct handled  *)
      CONST
	 WhiteSpace = CharSet{' ', tab};
      VAR
	 index: CARDINAL;	(* of buf *)
	 s1, len: CARDINAL;	(* start of val and length of val *)
   BEGIN
      again := FALSE;
      index := 0; SkipWhite(index);
      IF (index <= HIGH(buf)) AND (buf[index] = '#') THEN
	 again := TRUE;		(* perhabs an update directive *)
	 RETURN FALSE
      END;
      s1 := index; SkipAlpha(index); len := index-s1;
      SkipWhite(index);
      IF (len > 0) AND (index <= HIGH(buf)) AND (buf[index] = '=') THEN
	 INC(index); SkipWhite(index);
	 StrPartCpy(macro, buf, s1, len);
	 StrPartCpy(val, buf, index, HIGH(buf));
	 LOOP (* for multiple line definitions *)
	    index := StrLen(val);
	    IF index = 0 THEN EXIT END;
	    REPEAT
	       DEC(index);
	    UNTIL (index = 0) OR NOT (val[index] IN WhiteSpace);
	    IF val[index] <> '\' THEN EXIT END;
	    val[index] := 0C;
	    IF NOT ReadLine() THEN
	       Error(infilenm, "'\'-terminated line not continued");
	    END;
	    StrCat(val, buf);
	 END;
	 RETURN TRUE
      END;
      RETURN FALSE
   END CheckForMacro;

   PROCEDURE CheckSRC() : BOOLEAN;
      CONST
	 WhiteSpace = CharSet{' ', tab};
      VAR
	 srclist: ARRAY[0..5*1024-1] OF CHAR;
	 macro: ARRAY[0..3] OF CHAR;
	 index, start: CARDINAL;
	 filename: ARRAY[0..filenmlen-1] OF CHAR;
   BEGIN
      IF CheckForMacro(macro, srclist, again) AND
	 (StrCmp(macro, "SRC") = 0) THEN
	 index := 0; start := 0;
	 WHILE (index <= HIGH(srclist)) AND (srclist[index] <> 0C) DO
	    IF srclist[index] IN WhiteSpace THEN
	       StrPartCpy(filename, srclist, start, index-start);
	       EnterFileName(filename);
	       REPEAT
		  INC(index);
	       UNTIL (index > HIGH(srclist)) OR
		     NOT (srclist[index] IN WhiteSpace);
	       start := index;
	    ELSE
	       INC(index);
	    END;
	 END;
	 IF start <> index THEN
	    StrPartCpy(filename, srclist, start, index-start);
	    EnterFileName(filename);
	 END;
	 RETURN TRUE
      END;
      RETURN FALSE
   END CheckSRC;

   PROCEDURE Skip;
      TYPE
	 Text = ARRAY[0..63] OF CHAR;
      VAR
	 index: CARDINAL;
	 s1, len: CARDINAL;
	 macro, val: Text;
   BEGIN
      WHILE ReadLine() DO
	 IF CheckForMacro(macro, val, again) THEN
	    EnterMacro(macro, val);
	 ELSIF again THEN
	    RETURN
	 END;
      END;
      again := TRUE;
   END Skip;

   PROCEDURE PrintFlags;

      PROCEDURE WriteFlag(flag: CHAR; val: BOOLEAN);
      BEGIN
	 WriteString(" -");
	 Write(flag);
	 IF val THEN Write("+") ELSE Write("-") END;
      END WriteFlag;

   BEGIN
      WriteString("# {flags:");
      WriteFlag('a', SYMarchive);
      WriteFlag('e', lookforenv);
      WriteFlag('l', library);
      WriteFlag('L', scanlibs);
      WriteFlag('m', profile);
      WriteFlag('1', aronce);
      WriteString(" -v");
      CASE versmanag OF
      | rcs:  Write('r');
      | sccs: Write('s');
      | none: Write('-');
      END;
      Write('}'); WriteLn;
   END PrintFlags;

   PROCEDURE Update(infile, outfile: ARRAY OF CHAR);
      VAR
	 cmd: ARRAY[0..63] OF CHAR;
	 skip: BOOLEAN;
   BEGIN
      OpenMakefile(outfile);
      IF NOT Fopen(in, infile, read, (* buffered = *) TRUE) THEN
	 Fatal(infile, "cannot open");
      END;
      StrCpy(infilenm, infile);

      again := FALSE;
      WHILE ReadLine() DO
	 IF ScanForUpdate(cmd) THEN
	    CASE cmd[0] OF
	    | 'a': WriteString(buf); WriteLn; Skip; PrintAll;
	    | 'd': WriteString(buf); WriteLn; Skip; PrintDependencies;
	    | 'f': PrintFlags;
	    | 'l': WriteString(buf); WriteLn; Skip; PrintLinkage;
	    | 'm': WriteString(buf); WriteLn; Skip; PrintMacros;
	    | 'p': WriteString(buf); WriteLn; Skip; PrintPrecious;
	    | 'r': WriteString(buf); WriteLn; Skip; PrintRules;
	    | 'c', 'u': WriteString("# {updated by mmm: ");
		   TimeToString(CurrentTime(), cmd);
		   WriteString(cmd); Write("}"); WriteLn;
	    ELSE
	       Warning(cmd, "bad update directive");
	       WriteString(buf); WriteLn;
	    END;
	 ELSE
	    WriteString(buf); WriteLn;
	 END;
      END;
      CloseMakefile;
   END Update;

   PROCEDURE FirstMakefile(outfile: ARRAY OF CHAR);
      VAR timestamp: ARRAY [0..31] OF CHAR;
   BEGIN
      OpenMakefile(outfile);

      WriteString("# {created by mmm: ");
      TimeToString(CurrentTime(), timestamp); WriteString(timestamp);
      Write("}"); WriteLn;

      PrintFlags;

      WriteString("# {rules}"); WriteLn; PrintRules;
      WriteString("# {macros}"); WriteLn; PrintMacros;
      WriteString("# {precious}"); WriteLn; PrintPrecious;
      WriteString("# {all}"); WriteLn; PrintAll;
      WriteString("# {linkage}"); WriteLn; PrintLinkage;
      WriteString("# {dependencies}"); WriteLn; PrintDependencies;

      CloseMakefile;
   END FirstMakefile;

   PROCEDURE FirstScan(infile: ARRAY OF CHAR; lookforSRC: BOOLEAN);
      VAR
	 cmd: ARRAY[0..63] OF CHAR;
	 flagsfound, SRCfound: BOOLEAN;

      PROCEDURE SetFlags(cmd: ARRAY OF CHAR);
	 VAR
	    index: CARDINAL;
	    ch: CHAR;

	 PROCEDURE Read(VAR ch: CHAR) : BOOLEAN;
	 BEGIN
	    IF (index <= HIGH(cmd)) AND (cmd[index] <> 0C) THEN
	       ch := cmd[index];
	       INC(index);
	       RETURN TRUE
	    ELSE
	       RETURN FALSE
	    END;
	 END Read;

	 PROCEDURE Option(VAR flag: BOOLEAN);
	    VAR ch: CHAR;
	 BEGIN
	    IF Read(ch) THEN
	       CASE ch OF
	       | '+': flag := TRUE;
	       | '-': flag := FALSE;
	       ELSE
		  flag := NOT flag;
		  DEC(index);
	       END;
	    END;
	 END Option;

      BEGIN
	 index := 0;
	 WHILE Read(ch) AND (ch <> ':') DO END;
	 WHILE Read(ch) DO
	    CASE ch OF
	    | ' ', tab, '-':
	    | 'a': Option(SYMarchive);
	    | 'e': Option(lookforenv);
	    | 'l': Option(library);
	    | 'L': Option(scanlibs);
	    | 'm': Option(profile);
	    | '1': Option(aronce);
	    | 'v': IF Read(ch) AND (versmanag = none) THEN
		      CASE ch OF
		      | 'r': versmanag := rcs;
		      | 's': versmanag := sccs;
		      | '-': versmanag := none;
		      ELSE
			 Error(infile, "unknown arg to -v");
		      END;
		   END;
	    ELSE
	       Error(infile, "unknown flag");
	    END;
	 END;
      END SetFlags;

   BEGIN
      IF NOT Fopen(in, infile, read, (* buffered = *) TRUE) THEN
	 Fatal(infile, "cannot open");
      END;
      flagsfound := FALSE; SRCfound := NOT lookforSRC;
      WHILE ReadLine() AND (NOT flagsfound OR NOT SRCfound) DO
	 IF NOT flagsfound AND ScanForUpdate(cmd) THEN
	    IF cmd[0] = 'f' THEN
	       SetFlags(cmd);
	       flagsfound := TRUE;
	    END;
	 ELSIF lookforSRC AND NOT SRCfound THEN
	    IF CheckSRC() THEN SRCfound := TRUE END; again := FALSE;
	 END;
      END;
      IF NOT Fclose(in) THEN END;
      RETURN
   END FirstScan;

END Update.
