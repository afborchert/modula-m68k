IMPLEMENTATION MODULE SysProfil;

   FROM Sys IMPORT profil;
   FROM Errno IMPORT errno;
   FROM SYSTEM IMPORT UNIXCALL, ADDRESS;

   PROCEDURE Profil(buf: ADDRESS; bufsiz, offset, scale: CARDINAL) : BOOLEAN;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(profil, r0, r1, buf, bufsiz, offset, scale) THEN
         RETURN TRUE;
      ELSE
         errno := r0;
         RETURN FALSE;
      END;
   END Profil;

END SysProfil.
