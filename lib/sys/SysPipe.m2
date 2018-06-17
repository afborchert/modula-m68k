IMPLEMENTATION MODULE SysPipe;

   FROM SYSTEM IMPORT UNIXCALL;
   FROM Sys IMPORT pipe;
   FROM Errno IMPORT errno;

   PROCEDURE Pipe(VAR ReadFileDesc, WriteFileDesc: CARDINAL) : BOOLEAN;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(pipe, r0, r1) THEN
         ReadFileDesc := r0;
         WriteFileDesc := r1;
         RETURN TRUE;
      ELSE
         errno := r0;
         RETURN FALSE;
      END;
   END Pipe;

END SysPipe.
