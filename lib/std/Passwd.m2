IMPLEMENTATION MODULE Passwd;

   FROM StdIO	IMPORT FILE, MODE, Fopen, Fclose;
   FROM Strings	IMPORT StrCmp;
   FROM ScanPwfile IMPORT fieldsep, linesep, ReRead, GetText, GetNumber;

(* TYPE Pwent = RECORD ... END; *)

   CONST
      pwfilename  = "/etc/passwd";

   VAR
      pwfile: FILE;
      opened: BOOLEAN;

   PROCEDURE GetPwent(VAR pwent: Pwent): BOOLEAN;		(* EXPORTED *)
   BEGIN
      IF ~opened THEN RETURN FALSE END;
      WITH pwent DO
	 RETURN
	    GetText(pwfile, logname,fieldsep) &
	    GetText(pwfile, password,fieldsep) &
	    GetNumber(pwfile, uid,fieldsep) &
	    GetNumber(pwfile, gid,fieldsep) &
	    GetText(pwfile, fullname,fieldsep) &
	    GetText(pwfile, dir,fieldsep) &
	    GetText(pwfile, shell,linesep)
      END;
   END GetPwent;

   PROCEDURE OpenPw(filename: ARRAY OF CHAR): BOOLEAN;		(* EXPORTED *)
   BEGIN
      IF opened THEN
	 IF ~Fclose(pwfile) THEN END;
      END;
      opened := Fopen(pwfile, filename, read, (*buff'd*) TRUE);
      RETURN opened
   END OpenPw;

   PROCEDURE ClosePw(): BOOLEAN;				(* EXPORTED *)
   BEGIN
      IF ~opened THEN
	 RETURN FALSE
      END;
      IF ~Fclose(pwfile) THEN END;
      opened := FALSE;
      RETURN TRUE
   END ClosePw;

   PROCEDURE ReopenPw(): BOOLEAN;				(* EXPORTED *)
   BEGIN
      RETURN
	 opened & ReRead(pwfile)
   END ReopenPw;

   PROCEDURE GetPwuid(uid: CARDINAL; VAR pwent: Pwent): BOOLEAN;
   (* EXPORTED *)
   BEGIN
      WHILE GetPwent(pwent) DO
	 IF pwent.uid = uid THEN
	    RETURN TRUE
	 END;
      END;
      RETURN FALSE
   END GetPwuid;

   PROCEDURE GetPwnam(logn: ARRAY OF CHAR; VAR pwent: Pwent): BOOLEAN;
   (* EXPORTED *)
   BEGIN
      WHILE GetPwent(pwent) DO
	 IF StrCmp(pwent.logname,logn) = 0 THEN
	    RETURN TRUE
	 END;
      END;
      RETURN FALSE
   END GetPwnam;

   PROCEDURE FetchPwuid(uid: CARDINAL; VAR pwent: Pwent): BOOLEAN;
   (* EXPORTED *)
   BEGIN
      RETURN
	 OpenPw(pwfilename) &
	 GetPwuid(uid, pwent) &
	 ClosePw()
   END FetchPwuid;

   PROCEDURE FetchPwnam(logn: ARRAY OF CHAR; VAR pwent: Pwent): BOOLEAN;
   (* EXPORTED *)
   BEGIN
      RETURN
	 OpenPw(pwfilename) &
	 GetPwnam(logn, pwent) &
	 ClosePw()
   END FetchPwnam;

END Passwd.
