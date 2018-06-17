IMPLEMENTATION MODULE SysDup;

   FROM Sys IMPORT dup, dup2;		(* dup2 doesn't exists on Nixdorf *)
   FROM Errno IMPORT errno;
   FROM SYSTEM IMPORT UNIXCALL;
   FROM SysClose IMPORT Close;

   PROCEDURE Dup(fd: CARDINAL; VAR newfd: CARDINAL) : BOOLEAN;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(dup, r0, r1, fd) THEN
         newfd := r0;
         RETURN TRUE;
      ELSE
         errno := r0;
         RETURN FALSE;
      END;
   END Dup;

   PROCEDURE Dup2(fd, newfd: CARDINAL) : BOOLEAN;
      VAR fd2: CARDINAL;
	  r0, r1 : CARDINAL;
   BEGIN
      IF UNIXCALL(dup2, r0, r1, fd, newfd) THEN
	    RETURN TRUE;
      ELSE
	 errno := r0;
	 RETURN FALSE;
      END;
   END Dup2;

END SysDup.
