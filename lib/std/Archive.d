DEFINITION MODULE Archive; (* AFB 3/84 *)

   FROM SystemTypes IMPORT TIME, OFF;

   (* routines for reading an archive file *)

   CONST NameLength = 14;

   TYPE AFILE; (* hidden *)
      FileName = ARRAY[0..NameLength-1] OF CHAR;
      AStat =
         RECORD
            name: FileName;
            uid, gid: CARDINAL;
            date: TIME;
            size: OFF;
            mode: BITSET;
	    offset: OFF; (* absolute offset in archive file *)
         END;

   PROCEDURE ArchiveOpen(VAR a: AFILE; archive: ARRAY OF CHAR;
			 filename: ARRAY OF CHAR) : BOOLEAN;

   PROCEDURE ArchiveReopen(a: AFILE; filename: ARRAY OF CHAR) : BOOLEAN;
   (* in case of an error "a" will be closed *)

   PROCEDURE ArchiveClose(a: AFILE);

   PROCEDURE ArchiveRead(a: AFILE; VAR ch: CHAR) : BOOLEAN;

   PROCEDURE ArchiveStat(a: AFILE; VAR buf: AStat);

END Archive.
