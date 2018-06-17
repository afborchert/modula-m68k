IMPLEMENTATION MODULE SysOpen;

   FROM SYSTEM IMPORT UNIXCALL, ADR, WORD;
   FROM Sys IMPORT open;
   FROM Errno IMPORT errno;
   FROM UnixString IMPORT Copy, Buffer;

   (* mode = 0 : read, 1 : write, 2 : both *)

   PROCEDURE Open(VAR fd: CARDINAL; filename: ARRAY OF CHAR;
                  oflag: WORD) : BOOLEAN;
   BEGIN
      RETURN OpenCreat(fd, filename, oflag, 0);
   END Open;

   PROCEDURE OpenCreat(VAR fd: CARDINAL; filename: ARRAY OF CHAR;
		       oflag: WORD; mode: CARDINAL) : BOOLEAN;
      VAR r0, r1: CARDINAL;
          Buf: Buffer;
   BEGIN
      Copy(Buf, filename);
      IF UNIXCALL(open, r0, r1, ADR(Buf), oflag, mode) THEN
         fd := r0;
         RETURN TRUE;
      ELSE
         errno := r0;
         RETURN FALSE;
      END;
   END OpenCreat;

END SysOpen.
