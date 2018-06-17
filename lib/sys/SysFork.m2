IMPLEMENTATION MODULE SysFork;

   FROM SYSTEM IMPORT UNIXFORK;
   FROM Errno IMPORT errno;

   PROCEDURE Fork(VAR pid: CARDINAL) : BOOLEAN;
      VAR result: CARDINAL;
   BEGIN
      IF UNIXFORK(result) THEN
         pid := result;
         RETURN TRUE;
      ELSE
         errno := result;
         RETURN FALSE;
      END;
   END Fork;

END SysFork.
