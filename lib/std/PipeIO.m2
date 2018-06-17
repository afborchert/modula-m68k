IMPLEMENTATION MODULE PipeIO; (* AFB 6/84 *)

   FROM SYSTEM IMPORT ADR, ADDRESS;
   FROM StdIO IMPORT FILE, MODE, Fdopen, Fclose;
   FROM SysFork IMPORT Fork;
   FROM SysExec IMPORT Exec;
   FROM SysWait IMPORT Wait;
   FROM SysPipe IMPORT Pipe;
   FROM SysExit IMPORT Exit;
   FROM UnixString IMPORT Buffer, Copy;
   IMPORT SysClose;
   FROM SysDup IMPORT Dup2;

   PROCEDURE Close(fd: CARDINAL);
   BEGIN
      IF SysClose.Close(fd) THEN (* ignore result *) END;
   END Close;

   PROCEDURE Popen(VAR f: FILE; cmd: ARRAY OF CHAR; mode: MODE;
                   buffered: BOOLEAN) : BOOLEAN;
      CONST
         stdin = 0;
         stdout = 1;
      VAR
         fd: CARDINAL;
         args: ARRAY[0..3] OF ADDRESS;
         arg1, arg2: ARRAY[0..15] OF CHAR;
         child: CARDINAL;
         ReadFileDesc, WriteFileDesc: CARDINAL;
         cmdbuf: Buffer; (* cmd with terminating 0-byte *)
   BEGIN
      IF NOT Pipe(ReadFileDesc, WriteFileDesc) THEN RETURN FALSE END;
      IF NOT Fork(child) THEN
         Close(ReadFileDesc);
         Close(WriteFileDesc);
         RETURN FALSE;
      END;
      IF child = 0 THEN (* son *)
         IF mode = read THEN
            IF NOT Dup2(WriteFileDesc, stdout) THEN Exit(1) END;
         ELSE
            IF NOT Dup2(ReadFileDesc, stdin) THEN Exit(1) END;
         END;
         Close(ReadFileDesc);
         Close(WriteFileDesc);
         arg1 := "/bin/sh";
         arg2 := "-c";
         args[0] := ADR(arg1);
         args[1] := ADR(arg2);
         Copy(cmdbuf, cmd);
         args[2] := ADR(cmdbuf);
         args[3] := 0;
         Exec(arg1, ADR(args));
         Exit(1);
      END;
      (* father *)
      IF mode = read THEN
         Close(WriteFileDesc);
         fd := ReadFileDesc;
      ELSE
         Close(ReadFileDesc);
         fd := WriteFileDesc;
      END;
      RETURN Fdopen(f, fd, mode, buffered);
   END Popen;

   PROCEDURE Pclose(f: FILE) : BOOLEAN;
      VAR child, status: CARDINAL; resultofclose: BOOLEAN;
   BEGIN
      resultofclose := Fclose(f);
      IF NOT Wait(child, status) THEN (* ignore result *) END;
      RETURN resultofclose AND (status = 0);
   END Pclose;

END PipeIO.
