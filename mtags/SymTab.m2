IMPLEMENTATION MODULE SymTab;

   FROM Strings IMPORT StrLen, StrCmp;
   FROM Storage IMPORT ALLOCATE;
   FROM StdIO IMPORT stderr;
   FROM FtdIO IMPORT FwriteString, FwriteLn;

   (* (* from definition module *)
   CONST
      FileNameLength = 64;
      LineLength = 128;
      IdentLen = 24;

   TYPE
      FileName = ARRAY[0..FileNameLength-1] OF CHAR;
      Line = ARRAY[0..LineLength-1] OF CHAR;
      Identifier = ARRAY[0..IdentLen-1] OF CHAR;
      SymEntry =
	 RECORD
	    name: Identifier;  (* name of procedure *)
	    file: FileName;
	    line: Line;
	    multdecl: BOOLEAN; (* multiple declared ? *)
	    father: SymRef;
	    link: SymRef;
	 END;
   *)

   CONST
      HashConst = 1009; (* must be prime number *)
      MaxNest = 32;
   VAR
      Bucket: ARRAY[0..HashConst-1] OF SymRef;
      Nest: ARRAY[0..MaxNest-1] OF SymRef;
      NestLevel: CARDINAL;
      CurrentFile: FileName;
      index: CARDINAL;

   PROCEDURE HashValue(str: ARRAY OF CHAR) : CARDINAL;
      VAR right: CARDINAL;
   BEGIN
      right := StrLen(str)-1;
      RETURN (ORD(str[0]) + ORD(str[right])*32 + (right-0)*128) MOD HashConst;
   END HashValue;

   PROCEDURE EnterProc(pname: Identifier; ln: Line);
      VAR
	 index: CARDINAL;
	 sr: SymRef;

      PROCEDURE PrintWarning(sr: SymRef);

	 PROCEDURE PrintSequence(sr: SymRef);
	 BEGIN
	    WITH sr^ DO
	       IF father <> NIL THEN
		  PrintSequence(father);
		  FwriteString(stderr, ".");
	       END;
	       FwriteString(stderr, name);
	    END;
	 END PrintSequence;

      BEGIN
	 FwriteString(stderr, sr^.file); FwriteString(stderr, ": ");
	 PrintSequence(sr);
	 FwriteString(stderr, " must be qualified (warning only)");
	 FwriteLn(stderr);
      END PrintWarning;

      PROCEDURE MultDecl() : BOOLEAN;
	 VAR sr: SymRef;
      BEGIN
	 sr := Bucket[index];
	 WHILE sr <> NIL DO
	    IF StrCmp(sr^.name, pname) = 0 THEN
	       IF NOT sr^.multdecl THEN
		  PrintWarning(sr);
		  sr^.multdecl := TRUE;
	       END;
	       RETURN TRUE;
	    END;
	    sr := sr^.link;
	 END;
	 RETURN FALSE;
      END MultDecl;

   BEGIN
      index := HashValue(pname);
      NEW(sr);
      IF NestLevel <= HIGH(Nest) THEN
	 Nest[NestLevel] := sr;
      END;
      WITH sr^ DO
	 name := pname;
	 file := CurrentFile;
	 line := ln;
	 multdecl := MultDecl();
	 IF NestLevel > 0 THEN
	    father := Nest[NestLevel-1];
	 ELSE
	    father := NIL;
	 END;
	 link := Bucket[index];
	 IF multdecl THEN PrintWarning(sr) END;
      END;
      Bucket[index] := sr;
      INC(NestLevel);
   END EnterProc;

   PROCEDURE EndProc;
   BEGIN
      IF NestLevel > 0 THEN
	 DEC(NestLevel);
      END;
   END EndProc;

   PROCEDURE EnterFile(fname: FileName);
   BEGIN
      CurrentFile := fname;
      NestLevel := 0;
   END EnterFile;

   PROCEDURE EndFile;
   BEGIN
      CurrentFile[0] := 0C;
   END EndFile;

   MODULE ReturnProcs;

      IMPORT Bucket, SymRef;
      EXPORT FirstProc, NextProc;

      VAR
	 index: CARDINAL;
	 sr: SymRef;

      PROCEDURE FirstProc(VAR sym: SymRef);
      BEGIN
	 index := 0; sr := Bucket[index];
	 NextProc(sym);
      END FirstProc;

      PROCEDURE NextProc(VAR sym: SymRef);
      BEGIN
	 WHILE sr = NIL DO
	    INC(index);
	    IF index > HIGH(Bucket) THEN
	       sym := NIL; RETURN
	    END;
	    sr := Bucket[index];
	 END;
	 sym := sr;
	 sr := sr^.link;
      END NextProc;

   END ReturnProcs;

BEGIN
   FOR index := 0 TO HIGH(Bucket) DO
      Bucket[index] := NIL;
   END;
   NestLevel := 0;
   CurrentFile := "";
END SymTab.
