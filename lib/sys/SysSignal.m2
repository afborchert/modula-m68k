IMPLEMENTATION MODULE SysSignal;

   FROM SYSTEM IMPORT UNIXSIGNAL;
   FROM Errno IMPORT errno;
   FROM SystemTypes IMPORT Sig;

   PROCEDURE Signal(sig: Sig; p: PROC) : BOOLEAN;
      VAR result: CARDINAL;
   BEGIN
      IF UNIXSIGNAL(CARDINAL(sig), p, old, result) THEN
         RETURN TRUE;
      ELSE
         errno := result;
         RETURN FALSE;
      END;
   END Signal;

BEGIN
   default := PROC(0);
   ignore := PROC(1);
END SysSignal.
