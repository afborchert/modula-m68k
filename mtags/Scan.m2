IMPLEMENTATION MODULE Scan;

   FROM SymTab IMPORT FileName, Line, Identifier;
   FROM StdIO IMPORT FILE, Fopen, Fclose, read, stderr, Feof, Fgetc;
   FROM ASCII IMPORT tab, nl;
   FROM FtdIO IMPORT FwriteString, FwriteLn;
   FROM Strings IMPORT StrCmp;
   FROM SysPerror IMPORT Perror;

   (* (* from definition module *)
   TYPE
      Symbol = (endsy, modulesy, procsy);
   *)
   VAR
      fp: FILE;
      filename: FileName;
      fileopen: BOOLEAN;
      LinBuf: Line;
      index: CARDINAL; (* of LinBuf *)
      eof: BOOLEAN;

   PROCEDURE ReadLine() : BOOLEAN;
      VAR
	 ch: CHAR;
   BEGIN
      index := 0;
      WHILE Fgetc(ch, fp) AND (ch <> nl) DO
	 IF index <= HIGH(LinBuf) THEN
	    LinBuf[index] := ch;
	 ELSIF index = HIGH(LinBuf)+1 THEN
	    FwriteString(stderr, filename);
	    FwriteString(stderr, ": line too long (warning only)");
	    FwriteLn(stderr);
	 END;
	 INC(index);
      END;
      IF index <= HIGH(LinBuf) THEN
	 LinBuf[index] := 0C;
      END;
      index := 0;
      eof := Feof(fp);
      RETURN ch = nl;
   END ReadLine;

   PROCEDURE Read(VAR ch: CHAR) : BOOLEAN;
   BEGIN
      IF eof THEN RETURN FALSE END;
      IF (index > HIGH(LinBuf)) OR (LinBuf[index] = 0C) THEN
	 ch := nl;
	 RETURN ReadLine();
      END;
      ch := LinBuf[index];
      INC(index);
      RETURN TRUE;
   END Read;

   PROCEDURE ReadAgain;
   BEGIN
      IF index > 0 THEN
	 DEC(index);
      END;
   END ReadAgain;

   PROCEDURE ReadIdent(VAR id: Identifier) : BOOLEAN;
      TYPE
	 CharSet = SET OF CHAR;
      CONST
	 WhiteSpace = CharSet{" ", tab, nl};
	 AlphaNum = CharSet{'a'..'z', 'A'..'Z', '0'..'9'};
      VAR
	 ch: CHAR;
	 index: CARDINAL; (* into id *)

      PROCEDURE SkipTo(stop: CHAR);
	 VAR ch: CHAR;
      BEGIN
	 WHILE Read(ch) AND (ch <> stop) DO END;
      END SkipTo;

      PROCEDURE CheckComment() : BOOLEAN;
	 (* return TRUE on comments *)
	 VAR ch: CHAR;
      BEGIN
	 IF Read(ch) AND (ch = '*') THEN
	    IF NOT Read(ch) THEN RETURN TRUE END;
	    LOOP
	       WHILE (ch <> '*') AND (ch <> '(') DO
		  IF NOT Read(ch) THEN RETURN TRUE END;
	       END;
	       CASE ch OF
	       | '(': IF CheckComment() THEN END;
	              IF NOT Read(ch) THEN RETURN TRUE END;
	       | '*': IF Read(ch) AND (ch = ')') THEN RETURN TRUE END;
	       END;
	    END;
	 ELSE
	    ReadAgain;
	    RETURN FALSE
	 END;
      END CheckComment;

   BEGIN
      LOOP
	 WHILE Read(ch) AND (ch IN WhiteSpace) DO END;
	 IF eof THEN RETURN FALSE END;
	 CASE ch OF
	 | '"', "'": SkipTo(ch);
	 | '(': IF NOT CheckComment() THEN
		   id[0] := ch; id[1] := 0C; RETURN TRUE
		END;
	 | '.', ';', '|': id[0] := ch; id[1] := 0C; RETURN TRUE;
	 | 'a'..'z', 'A'..'Z':
	    id[0] := ch; index := 1;
	    WHILE Read(ch) AND (ch IN AlphaNum) DO
	       IF index <= HIGH(id) THEN
		  id[index] := ch;
	       END;
	       INC(index);
	    END;
	    IF index <= HIGH(id) THEN
	       id[index] := 0C;
	    END;
	    ReadAgain;
	    RETURN TRUE;
	 ELSE
	 END;
      END;
   END ReadIdent;

   PROCEDURE OpenScan(file: FileName);
   BEGIN
      IF fileopen THEN
	 IF NOT Fclose(fp) THEN END;
	 fileopen := FALSE;
      END;
      IF NOT Fopen(fp, file, read, (* buffered = *) TRUE) THEN
	 Perror(file);
	 RETURN
      END;
      filename := file;
      fileopen := TRUE;
      index := 0;
      eof := FALSE;
      LinBuf[index] := 0C;
   END OpenScan;

   PROCEDURE GetSy(VAR sy: Symbol; VAR id: Identifier;
		   VAR line: Line) : BOOLEAN;
      (* return FALSE on eof *)
   BEGIN
      IF NOT fileopen THEN RETURN FALSE END;
      IF NOT ReadIdent(id) THEN RETURN FALSE END;
      LOOP
	 LOOP
	    IF (StrCmp(id, "PROCEDURE") = 0) OR
	       (StrCmp(id, "TRANSACTION") = 0) THEN
	       sy := procsy;
	       EXIT
	    ELSIF (StrCmp(id, "MODULE") = 0) THEN
	       sy := modulesy;
	       EXIT
	    ELSIF (StrCmp(id, "END") = 0) THEN
	       sy := endsy;
	       EXIT
	    END;
	    IF NOT ReadIdent(id) THEN RETURN FALSE END;
	 END;
	 IF NOT ReadIdent(id) THEN RETURN FALSE END;
	 IF (sy = endsy) AND
	    (StrCmp(id, ".") <> 0) AND
	    (StrCmp(id, ";") <> 0) AND
	    (StrCmp(id, "|") <> 0) AND
	    (StrCmp(id, "END") <> 0) AND
	    (StrCmp(id, "ELSE") <> 0) AND
	    (StrCmp(id, "ELSIF") <> 0) OR
	    (* don't return procedure types! *)
	    (sy = procsy) AND (StrCmp(id, "(") <> 0) OR
	    (sy = modulesy) THEN
	    line := LinBuf;
	    RETURN TRUE;
	 END;
      END;
   END GetSy;

BEGIN
   fileopen := FALSE;
   eof := FALSE;
END Scan.
