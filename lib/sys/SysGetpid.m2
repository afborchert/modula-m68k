IMPLEMENTATION MODULE SysGetpid;			(* mh 11/1987 *)

   FROM SystemTypes IMPORT ProcessId;
   FROM Sys	    IMPORT getpid;
   FROM SYSTEM	    IMPORT UNIXCALL;

   PROCEDURE Getpid(): ProcessId;
      VAR
	 r0: ProcessId;
	 r1: CARDINAL;			(* not used *)
   BEGIN
      IF ~UNIXCALL(getpid, r0, r1) THEN
	 RETURN -1
      END;
      RETURN r0
   END Getpid;

END SysGetpid.
