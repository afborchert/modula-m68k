IMPLEMENTATION MODULE SysLink;

   FROM SYSTEM IMPORT UNIXCALL, ADR;
   FROM Sys IMPORT link;
   FROM Errno IMPORT errno;
   FROM UnixString IMPORT Buffer, Copy;

   PROCEDURE Link(name1, name2: ARRAY OF CHAR) : BOOLEAN;
      VAR r0, r1: CARDINAL;
          Buf1, Buf2: Buffer;
   BEGIN
      Copy(Buf1, name1);
      Copy(Buf2, name2);
      IF UNIXCALL(link, r0, r1, ADR(Buf1), ADR(Buf2)) THEN
	 RETURN TRUE;
      ELSE
	 errno := r0;
	 RETURN FALSE;
      END;
   END Link;

END SysLink.
