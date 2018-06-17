IMPLEMENTATION MODULE Archive; (* AFB 3/84 *)
   (* version for common archive file format (XELOS) *)

   FROM SystemTypes IMPORT TIME, OFF;
   FROM Storage IMPORT ALLOCATE, DEALLOCATE;
   FROM StdIO IMPORT FILE, Fgetc, Fseek, Fclose, Fopen, read, Ftell;
   FROM FtdIO IMPORT FreadWord, Fread, Done;
   FROM StrToNum IMPORT StrToOct, StrToCard, StrToInt;
   FROM Strings IMPORT StrCpy;
   FROM SysPanic IMPORT Panic;
   FROM ASCII IMPORT nl;

   CONST
      ArMag = "!<arch> ";
   TYPE
      AFILE = POINTER TO ArchiveFile;

      ArchiveFile =
         RECORD
            fp: FILE;
            pos: OFF; (* [0..header^.size-1] *)
            nextfile: OFF; (* byte position of next file *)
            header: POINTER TO AStat;
         END;
      ArchiveHeader =
         RECORD (* all components in printable (decimal) format *)
	    name: ARRAY[0..15] OF CHAR;     (* file name *)
	    date: ARRAY[0..11] OF CHAR;
            uidgid: ARRAY[0..11] OF CHAR;
	    mode: ARRAY[0..7] OF CHAR;      (* octal *)
	    sizefmag: ARRAY[0..11] OF CHAR; (* size + header trailing string *)
         END;

   (* local routines *)

   PROCEDURE Convert(hd: ArchiveHeader; VAR stat: AStat) : BOOLEAN;
      VAR
	 index: CARDINAL;
	 m: CARDINAL;
         csize, cdate: CARDINAL;
         ch: CHAR;
   BEGIN
      WITH hd DO
	 (* strip blanks *)
	 index := HIGH(name);
	 IF name[index] # ' ' THEN
	    RETURN FALSE
	 END;
	 (* SunOS: file name filled up with blanks
	    SysV:  file name terminated by '/' and filled up with blanks
	 *)
	 WHILE (index > 0) AND (name[index-1] = ' ') DO
	    DEC(index);
	 END;
	 IF (index > 0) AND (name[index-1] = '/') THEN
	    (* SysV compatibility *)
	    DEC(index);
	 END;
	 name[index] := 0C;
	 StrCpy(stat.name, name);
         sizefmag[10] := 0C;
         ch := uidgid[6]; uidgid[6] := 0C;
         IF NOT StrToCard(uidgid, stat.uid) THEN RETURN FALSE END;
         uidgid[6] := ch;
         FOR index := 0 TO 5 DO
            uidgid[index] := uidgid[index+6];
         END;
         uidgid[6] := 0C;
         IF NOT StrToCard(uidgid, stat.gid) THEN RETURN FALSE END;
	 IF NOT StrToCard(date, cdate) OR
	    NOT StrToCard(sizefmag, csize) OR NOT StrToOct(mode, m) THEN
	    RETURN FALSE
	 END;
         stat.date := cdate; stat.size := csize;
	 stat.mode := BITSET(m);
	 stat.offset := 0;
      END;
      RETURN TRUE
   END Convert;

   PROCEDURE StrEqual(a, b: ARRAY OF CHAR) : BOOLEAN;
      VAR index: CARDINAL;
          min: CARDINAL;
   BEGIN
      IF HIGH(a) < HIGH(b) THEN min := HIGH(a) ELSE min := HIGH(b) END;
      FOR index := 0 TO min DO
         IF a[index] <> b[index] THEN
            RETURN FALSE;
         ELSIF a[index] = 0C THEN
            RETURN TRUE;
         END;
      END;
      RETURN TRUE;
   END StrEqual;

   PROCEDURE DisposeStructure(VAR a: AFILE);
   BEGIN
      DISPOSE(a^.header);
      DISPOSE(a);
   END DisposeStructure;

   PROCEDURE SearchFile(a: AFILE; filename: ARRAY OF CHAR;
                        VAR offset: OFF) : BOOLEAN;
      (* offset = byte position of next file *)
      VAR
	 size: OFF;
	 hd: ArchiveHeader;
   BEGIN
      LOOP
         Fread(a^.fp, hd);
         IF NOT Done OR
	    NOT Convert(hd, a^.header^) OR
            NOT Ftell(a^.fp, offset) THEN
            RETURN FALSE;
         END;
	 a^.header^.offset := offset;
         size := a^.header^.size;
         IF ODD(size) THEN INC(size) END;
         INC(offset, size);
         IF (filename[0] = 0C) OR StrEqual(a^.header^.name, filename) THEN
            RETURN TRUE;
         END;
         IF NOT Fseek(a^.fp, size, (* from current position = *) 1) THEN
            RETURN FALSE;
         END;
      END;
   END SearchFile;

   PROCEDURE ArchiveOpen(VAR a: AFILE; archive: ARRAY OF CHAR;
			 filename: ARRAY OF CHAR) : BOOLEAN;
      (* filename = "": open first file *)
      VAR magic: ARRAY[0..7] OF CHAR;
   BEGIN
      NEW(a);
      NEW(a^.header);
      IF NOT Fopen(a^.fp, archive, read, (* buffered = *) TRUE) THEN
         DisposeStructure(a);
         RETURN FALSE;
      END;
      Fread(a^.fp, magic); IF magic[7] = nl THEN magic[7] := " " END;
      IF NOT Done OR
         NOT StrEqual(ArMag, magic) OR
         NOT SearchFile(a, filename, a^.nextfile) THEN
         IF NOT Fclose(a^.fp) THEN END;
         DisposeStructure(a);
         RETURN FALSE;
      END;
      a^.pos := 0;
      RETURN TRUE;
   END ArchiveOpen;


   PROCEDURE ArchiveReopen(a: AFILE; filename: ARRAY OF CHAR) : BOOLEAN;
      (* in case of an error "a" will be closed *)
      (* filename = "": open next file *)
      VAR
         offset: INTEGER;
   BEGIN
      IF filename[0] = 0C THEN
         offset := a^.nextfile;
      ELSE
         offset := 0;
      END;
      IF NOT Fseek(a^.fp, offset, (* absolute seek = *) 0) OR
         NOT SearchFile(a, filename, a^.nextfile) THEN
         IF NOT Fclose(a^.fp) THEN END;
         DisposeStructure(a);
         RETURN FALSE;
      END;
      a^.pos := 0;
      RETURN TRUE;
   END ArchiveReopen;

   PROCEDURE ArchiveClose(a: AFILE);
   BEGIN
      IF NOT Fclose(a^.fp) THEN END;
      DisposeStructure(a);
   END ArchiveClose;

   PROCEDURE ArchiveRead(a: AFILE; VAR ch: CHAR) : BOOLEAN;
   BEGIN
      INC(a^.pos);
      RETURN (a^.pos <= a^.header^.size) AND Fgetc(ch, a^.fp)
   END ArchiveRead;

   PROCEDURE ArchiveStat(a: AFILE; VAR buf: AStat);
   BEGIN
      buf := a^.header^;
   END ArchiveStat;

END Archive.
