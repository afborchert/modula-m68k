(* Modula-2 makefile generator - UNIX System V - AFB 3/87 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
IMPLEMENTATION MODULE Out;

   FROM Errors IMPORT Fatal, Error;
   FROM FileNames IMPORT filenmlen;
   FROM FtdIO IMPORT FwriteString, FwriteLn, FwriteChar;
   FROM StdIO IMPORT FILE, Fopen, Fclose, write, stdout;
   FROM ASCII IMPORT tab;
   FROM Strings IMPORT StrLen, StrCpy, StrCat;

   CONST
      linelength = 78;
   TYPE
      Buffer = ARRAY [0..linelength-1] OF CHAR;
   VAR
      fp: FILE; outfile: ARRAY[0..filenmlen-1] OF CHAR;
      column: CARDINAL;
      white: BOOLEAN;
      append: Buffer;

   PROCEDURE WriteString(text: ARRAY OF CHAR);
      VAR
	 len: CARDINAL;
	 breakok: BOOLEAN;
   BEGIN
      len := StrLen(text);
      IF len > 0 THEN
	 INC(column, len);
	 breakok := white OR (text[0] = " ");
	 IF breakok THEN
	    IF column >= linelength THEN
	       IF append[0] <> 0C THEN
		  FwriteString(fp, append);
		  append[0] := 0C;
	       END;
	       IF NOT white THEN FwriteChar(fp, " ") END;
	       FwriteString(fp, "\"); FwriteLn(fp); column := 1; Tab;
	       FwriteString(fp, text);
	       INC(column, len);
	    ELSE
	       FwriteString(fp, append);
	       StrCpy(append, text);
	    END;
	 ELSIF (column >= linelength) AND (append[0] <> 0C) THEN
	    IF append[0] = " " THEN FwriteChar(fp, " ") END;
	    FwriteString(fp, "\"); FwriteLn(fp); column := 1; Tab;
	    FwriteString(fp, append); append[0] := 0C;
	    FwriteString(fp, text);
	    INC(column, len + StrLen(append));
	 ELSE
	    StrCat(append, text);
	 END;
	 white := text[len-1] = " ";
      END;
   END WriteString;

   PROCEDURE WriteLn;
   BEGIN
      IF append[0] <> 0C THEN FwriteString(fp, append); append[0] := 0C END;
      FwriteLn(fp); column := 1; white := TRUE;
   END WriteLn;

   PROCEDURE Write(ch: CHAR);
      VAR array: ARRAY [0..0] OF CHAR;
   BEGIN
      IF ch = " " THEN
	 IF NOT white THEN
	    IF append[0] <> 0C THEN
	       FwriteString(fp, append);
	       append[0] := 0C;
	    END;
	    FwriteChar(fp, ch);
	    INC(column);
	 END;
	 white := TRUE;
      ELSE
	 array[0] := ch; WriteString(array);
      END;
   END Write;

   PROCEDURE Tab;
      CONST
	 tabs = 8;
	 tab1 = tabs+1;
	 tab2 = 2*tabs+1;
   BEGIN
      IF column < tab1 THEN
	 FwriteChar(fp, tab);
      END;
      IF column < tab2 THEN
	 FwriteChar(fp, tab);
	 column := tab2;
      ELSE
	 FwriteChar(fp, " ");
	 INC(column);
      END;
      white := TRUE;
   END Tab;

   PROCEDURE WriteTab;
   BEGIN
      IF append[0] <> 0C THEN FwriteString(fp, append); append[0] := 0C END;
      Tab;
   END WriteTab;

   PROCEDURE OpenMakefile(filename: ARRAY OF CHAR);
   BEGIN
      IF NOT Fopen(fp, filename, write, (* buffered = *) TRUE) THEN
	 Fatal(filename, "cannot open");
      END;
      StrCpy(outfile, filename);
   END OpenMakefile;

   PROCEDURE CloseMakefile;
   BEGIN
      IF NOT Fclose(fp) THEN
	 Error(outfile, "write error");
      END;
   END CloseMakefile;

BEGIN
   fp := stdout;
   column := 1;
   white := TRUE;
   append[0] := 0C;
END Out.
