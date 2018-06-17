IMPLEMENTATION MODULE SysGetuid;

   FROM SYSTEM IMPORT UNIXCALL;
   FROM Sys IMPORT getuid, getgid;

   PROCEDURE Getuid() : CARDINAL;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(getuid, r0, r1) THEN END;
      RETURN r0
   END Getuid;

   PROCEDURE Geteuid() : CARDINAL;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(getuid, r0, r1) THEN END;	(*same as geteuid *)
      RETURN r1
   END Geteuid;

   PROCEDURE Getgid() : CARDINAL;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(getgid, r0, r1) THEN END;
      RETURN r0
   END Getgid;

   PROCEDURE Getegid() : CARDINAL;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(getgid, r0, r1) THEN END;	(*same as getegid *)
      RETURN r1
   END Getegid;

END SysGetuid.
