IMPLEMENTATION MODULE SysSetuid;

   FROM SYSTEM IMPORT UNIXCALL;
   FROM Sys IMPORT setreuid, setregid;	(*Nixdorf: setuid, setgid*)
   FROM Errno IMPORT errno;

   PROCEDURE Setuid(uid: CARDINAL) : BOOLEAN;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(setreuid, r0, r1, uid, uid) THEN
	 RETURN TRUE
      ELSE
	 errno := r0;
	 RETURN FALSE
      END;
   END Setuid;

   PROCEDURE Setgid(gid: CARDINAL) : BOOLEAN;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(setregid, r0, r1, gid, gid) THEN
	 RETURN TRUE
      ELSE
	 errno := r0;
	 RETURN FALSE
      END;
   END Setgid;

END SysSetuid.
