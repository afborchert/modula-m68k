DEFINITION MODULE Passwd;
(*
 *	scanning and searching the passord file
 *
 *	Martin Hasch, University of Ulm, Nov-29-1988
 *)

   TYPE
      Pwent =
	 RECORD
	    logname:  ARRAY [0..7] OF CHAR;
	    password: ARRAY [0..15] OF CHAR;
	    uid:      CARDINAL;
	    gid:      CARDINAL;
	    fullname: ARRAY [0..31] OF CHAR;
	    dir:      ARRAY [0..31] OF CHAR;
	    shell:    ARRAY [0..31] OF CHAR;
	 END;

   PROCEDURE OpenPw(filename: ARRAY OF CHAR): BOOLEAN;
   (* returns TRUE on success *)

   PROCEDURE GetPwent(VAR pwent: Pwent): BOOLEAN;

   PROCEDURE GetPwuid(uid: CARDINAL; VAR pwent: Pwent): BOOLEAN;

   PROCEDURE GetPwnam(logn: ARRAY OF CHAR; VAR pwent: Pwent): BOOLEAN;

   PROCEDURE ReopenPw(): BOOLEAN;
   (* returns TRUE if passwd file is open and seekable *)

   PROCEDURE ClosePw(): BOOLEAN;
   (* returns TRUE if passwd file was open *)


   PROCEDURE FetchPwuid(uid: CARDINAL; VAR pwent: Pwent): BOOLEAN;
   (* implies OpenPw("/etc/passwd"), and ClosePw() *)

   PROCEDURE FetchPwnam(logn: ARRAY OF CHAR; VAR pwent: Pwent): BOOLEAN;
   (* implies OpenPw("/etc/passwd"), and ClosePw() *)

END Passwd.
