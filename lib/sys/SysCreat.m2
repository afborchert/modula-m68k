IMPLEMENTATION MODULE SysCreat;

   FROM Errno IMPORT errno;
   FROM Sys IMPORT creat;
   FROM UnixString IMPORT Buffer, Copy;
   FROM SYSTEM IMPORT UNIXCALL, ADR;

   PROCEDURE Creat(VAR fd: CARDINAL; filename: ARRAY OF CHAR;
                   mode: CARDINAL) : BOOLEAN;
      VAR r0, r1: CARDINAL;
          Buf: Buffer;
   BEGIN
      Copy(Buf, filename);
      IF UNIXCALL(creat, r0, r1, ADR(Buf), mode) THEN
         fd := r0;
         RETURN TRUE;
      ELSE
         errno := r0;
         RETURN FALSE;
      END;
   END Creat;

END SysCreat.
