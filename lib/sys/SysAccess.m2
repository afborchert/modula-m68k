IMPLEMENTATION MODULE SysAccess;

   FROM Errno IMPORT errno;
   FROM Sys IMPORT access;
   FROM UnixString IMPORT Buffer, Copy;
   FROM SYSTEM IMPORT UNIXCALL, ADR;

   PROCEDURE Access(filename: ARRAY OF CHAR; mode: CARDINAL) : BOOLEAN;
      VAR r0, r1: CARDINAL;
          Buf: Buffer;
   BEGIN
      Copy(Buf, filename);
      IF UNIXCALL(access, r0, r1, ADR(Buf), mode) THEN
         RETURN TRUE;
      ELSE
         errno := r0;
         RETURN FALSE;
      END;
   END Access;

END SysAccess.
