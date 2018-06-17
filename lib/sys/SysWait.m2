IMPLEMENTATION MODULE SysWait;

   FROM Sys IMPORT wait;
   FROM Errno IMPORT errno;
   FROM SYSTEM IMPORT ADR, UNIXCALL;

   PROCEDURE Wait(VAR pid, status: CARDINAL) : BOOLEAN;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(wait, r0, r1) THEN
         pid := r0;
	 status := r1;
         RETURN TRUE
      ELSE
         errno := r0;
	 RETURN FALSE
      END;
   END Wait;

END SysWait.
