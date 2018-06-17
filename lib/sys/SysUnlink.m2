IMPLEMENTATION MODULE SysUnlink;

   FROM SYSTEM IMPORT UNIXCALL, ADR;
   FROM Sys IMPORT unlink;
   FROM Errno IMPORT errno;
   FROM UnixString IMPORT Buffer, Copy;

   PROCEDURE Unlink(name: ARRAY OF CHAR) : BOOLEAN;
      VAR r0, r1: CARDINAL;
          Buf: Buffer;
   BEGIN
      Copy(Buf, name);
      IF UNIXCALL(unlink, r0, r1, ADR(Buf)) THEN
	 RETURN TRUE;
      ELSE
	 errno := r0;
	 RETURN FALSE;
      END;
   END Unlink;

END SysUnlink.
