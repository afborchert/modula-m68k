IMPLEMENTATION MODULE SysRead;

   FROM Sys IMPORT read;
   FROM Errno IMPORT errno;
   FROM SYSTEM IMPORT UNIXCALL, ADDRESS;

   PROCEDURE Read(fd: CARDINAL; ptr: ADDRESS;
                  VAR bytecount: CARDINAL) : BOOLEAN;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(read, r0, r1, fd, ptr, bytecount) THEN
         bytecount := r0;
         RETURN TRUE;
      ELSE
         errno := r0;
         bytecount := 0;
         RETURN FALSE;
      END;
   END Read;

END SysRead.
