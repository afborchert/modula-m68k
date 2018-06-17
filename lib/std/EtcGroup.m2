IMPLEMENTATION MODULE EtcGroup;

   FROM StdIO	IMPORT FILE, MODE, Fopen, Fclose;
   FROM Strings	IMPORT StrCmp;
   FROM ScanPwfile IMPORT fieldsep, linesep, GetText, GetNumber, ReRead;

   CONST
      grfilename  = "/etc/group";

   VAR
      grfile: FILE;
      opened: BOOLEAN;

   PROCEDURE GetGrent(VAR grent: Grent): BOOLEAN;		(* EXPORTED *)
      VAR
	 buf: ARRAY [0..1023] OF CHAR;
   BEGIN
      IF ~opened THEN RETURN FALSE END;
      WITH grent DO
	 members := NIL;		(* members not yet implemented *)
	 RETURN
	    GetText(grfile, grname,fieldsep) &
	    GetText(grfile, password,fieldsep) &
	    GetNumber(grfile, gid,fieldsep) &
	    GetText(grfile, buf,linesep)
      END;
   END GetGrent;

   PROCEDURE OpenGr(filename: ARRAY OF CHAR): BOOLEAN;		(* EXPORTED *)
   BEGIN
      IF opened THEN
	 IF ~Fclose(grfile) THEN END;
      END;
      opened := Fopen(grfile, filename, read, (*buff'd*) TRUE);
      RETURN opened
   END OpenGr;

   PROCEDURE CloseGr(): BOOLEAN;				(* EXPORTED *)
   BEGIN
      IF ~opened THEN
	 RETURN FALSE
      END;
      IF ~Fclose(grfile) THEN END;
      opened := FALSE;
      RETURN TRUE
   END CloseGr;

   PROCEDURE ReopenGr(): BOOLEAN;				(* EXPORTED *)
   BEGIN
      RETURN opened & ReRead(grfile)
   END ReopenGr;

   PROCEDURE GetGrgid(gid: CARDINAL; VAR grent: Grent): BOOLEAN;
   (* EXPORTED *)
   BEGIN
      WHILE GetGrent(grent) DO
	 IF grent.gid = gid THEN
	    RETURN TRUE
	 END;
      END;
      RETURN FALSE
   END GetGrgid;

   PROCEDURE GetGrnam(grn: ARRAY OF CHAR; VAR grent: Grent): BOOLEAN;
   (* EXPORTED *)
   BEGIN
      WHILE GetGrent(grent) DO
	 IF StrCmp(grent.grname,grn) = 0 THEN
	    RETURN TRUE
	 END;
      END;
      RETURN FALSE
   END GetGrnam;

   PROCEDURE FetchGrgid(gid: CARDINAL; VAR grent: Grent): BOOLEAN;
   (* EXPORTED *)
   BEGIN
      RETURN
	 OpenGr(grfilename) &
	 GetGrgid(gid, grent) &
	 CloseGr()
   END FetchGrgid;

   PROCEDURE FetchGrnam(grn: ARRAY OF CHAR; VAR grent: Grent): BOOLEAN;
   (* EXPORTED *)
   BEGIN
      RETURN
	 OpenGr(grfilename) &
	 GetGrnam(grn, grent) &
	 CloseGr()
   END FetchGrnam;

END EtcGroup.
