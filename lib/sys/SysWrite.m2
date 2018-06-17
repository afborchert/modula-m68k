IMPLEMENTATION MODULE SysWrite;

   FROM Errno IMPORT errno;
   FROM Sys IMPORT write;
   FROM SYSTEM IMPORT UNIXCALL, ADDRESS;

   PROCEDURE Write(fd: CARDINAL; ptr: ADDRESS;
                   VAR bytecount: CARDINAL) : BOOLEAN;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(write, r0, r1, fd, ptr, bytecount) THEN
         bytecount := r0;
         RETURN TRUE;
      ELSE
         errno := r0;
         bytecount := 0;
         RETURN FALSE;
      END;
   END Write;

END SysWrite.
