IMPLEMENTATION MODULE SysExec;

   FROM SYSTEM IMPORT UNIXCALL, ADR, ADDRESS;
   FROM Sys IMPORT execve;
   FROM Errno IMPORT errno;
   FROM UnixString IMPORT Copy, Buffer;
   FROM SysLocations IMPORT Environment;

   PROCEDURE Exec(name: ARRAY OF CHAR; argv: ADDRESS);
   BEGIN
      Exece(name, argv, Environment);
   END Exec;

   PROCEDURE Exece(name: ARRAY OF CHAR; argv, envp: ADDRESS);
      VAR r0, r1: CARDINAL;
          Buf: Buffer;
   BEGIN
      Copy(Buf, name);
      IF UNIXCALL(execve, r0, r1, ADR(Buf), argv, envp) THEN
         (* can this ever happen ?? *)
      ELSE
         errno := r0;
      END;
   END Exece;

END SysExec.
