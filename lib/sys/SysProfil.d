DEFINITION MODULE SysProfil;

   FROM SYSTEM IMPORT ADDRESS;

   PROCEDURE Profil(buf: ADDRESS; bufsiz, offset, scale: CARDINAL) : BOOLEAN;

END SysProfil.
