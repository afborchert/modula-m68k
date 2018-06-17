DEFINITION MODULE EtcGroup;
(*
 *	scanning and searching the etc/group file
 *
 *	Martin Hasch, University of Ulm, Dec-06-1988
 *)

   TYPE
      MemberList = POINTER TO Member;
      Member =
	 RECORD
	    logname:  ARRAY [0..7] OF CHAR;
	    nextmem:  MemberList;
	 END;

      Grent =
	 RECORD
	    grname:   ARRAY [0..7] OF CHAR;
	    password: ARRAY [0..15] OF CHAR;
	    gid:      CARDINAL;
	    members:  MemberList;		(* NIL-terminated *)
	 END;

   PROCEDURE OpenGr(filename: ARRAY OF CHAR): BOOLEAN;
   (* returns TRUE on success *)

   PROCEDURE GetGrent(VAR grent: Grent): BOOLEAN;

   PROCEDURE GetGrgid(gid: CARDINAL; VAR grent: Grent): BOOLEAN;

   PROCEDURE GetGrnam(grn: ARRAY OF CHAR; VAR grent: Grent): BOOLEAN;

   PROCEDURE ReopenGr(): BOOLEAN;
   (* returns TRUE if group file is open and seekable *)

   PROCEDURE CloseGr(): BOOLEAN;
   (* returns TRUE if group file was open *)


   PROCEDURE FetchGrgid(gid: CARDINAL; VAR grent: Grent): BOOLEAN;
   (* implies OpenGr("/etc/group"), and CloseGr() *)

   PROCEDURE FetchGrnam(grn: ARRAY OF CHAR; VAR grent: Grent): BOOLEAN;
   (* implies OpenGr("/etc/group"), and CloseGr() *)

END EtcGroup.
