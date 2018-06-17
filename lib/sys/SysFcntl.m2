IMPLEMENTATION MODULE SysFcntl;

   FROM SYSTEM IMPORT UNIXCALL, WORD;
   FROM Errno IMPORT errno;
   FROM Sys IMPORT fcntl;

   PROCEDURE Fcntl(fd: CARDINAL; cmd: FcntlRequest; VAR arg: WORD) : BOOLEAN;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(fcntl, r0, r1, fd, cmd, arg) THEN
	 arg := WORD(r0);
	 RETURN TRUE;
      ELSE
	 errno := r0;
	 RETURN FALSE;
      END;
   END Fcntl;

END SysFcntl.
