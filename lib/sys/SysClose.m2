IMPLEMENTATION MODULE SysClose;

   FROM SYSTEM IMPORT UNIXCALL;
   FROM Sys IMPORT close;
   FROM Errno IMPORT errno;

   PROCEDURE Close(fd: CARDINAL) : BOOLEAN;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(close, r0, r1, fd) THEN
         RETURN TRUE;
      ELSE
         errno := r0;
         RETURN FALSE;
      END;
   END Close;

END SysClose.
