IMPLEMENTATION MODULE SysLseek;

   FROM Errno IMPORT errno;
   FROM Sys IMPORT lseek;
   FROM SystemTypes IMPORT OFF;
   FROM SYSTEM IMPORT UNIXCALL;

   PROCEDURE Lseek(fd: CARDINAL; offset: OFF;
                   whence: CARDINAL) : BOOLEAN;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(lseek, r0, r1, fd, offset, whence) THEN
         RETURN TRUE;
      ELSE
         errno := r0;
         RETURN FALSE;
      END;
   END Lseek;

   PROCEDURE Tell(fd: CARDINAL; VAR offset: OFF) : BOOLEAN;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(lseek, r0, r1, fd, 0, 1) THEN
         offset := r0;
         RETURN TRUE;
      ELSE
         errno := r0;
         RETURN FALSE;
      END;
   END Tell;

END SysLseek.
