IMPLEMENTATION MODULE CallShell;

   FROM SysSignal IMPORT Signal, ignore, old;
   FROM SystemTypes IMPORT Sig;
   FROM SysExec IMPORT Exec;
   FROM SysFork IMPORT Fork;
   FROM SysWait IMPORT Wait;
   FROM SysExit IMPORT Exit;
   FROM UnixString IMPORT Buffer, Copy;
   FROM SYSTEM IMPORT ADR, ADDRESS;

   PROCEDURE Shell(cmd: ARRAY OF CHAR; VAR status: CARDINAL) : BOOLEAN;
      CONST shell = "/bin/sh";
         ExecFailed = 255;
      VAR child, pid: CARDINAL;
          args: ARRAY[0..3] OF ADDRESS;
          arg1, arg2: ARRAY[0..15] OF CHAR;
          intproc, quitproc: PROC;
          ign: BOOLEAN;
          CmdBuffer: Buffer; (* cmd with null byte *)
   BEGIN
      IF NOT Fork(child) THEN RETURN FALSE END;
      IF child = 0 THEN (* son *)
         arg1 := shell; arg2 := "-c";
         args[0] := ADR(arg1);
         args[1] := ADR(arg2);
         Copy(CmdBuffer, cmd);
         args[2] := ADR(CmdBuffer);
         args[3] := 0;
         Exec(shell, ADR(args));
         Exit(ExecFailed);
      ELSE (* father *)
	 ign := Signal(SIGINT, ignore); intproc := old;
	 ign := Signal(SIGQUIT, ignore); quitproc := old;
         WHILE Wait(pid, status) AND (pid <> child) DO END;
         ign := Signal(SIGINT, intproc);
         ign := Signal(SIGQUIT, quitproc);
         RETURN status DIV 100H <> ExecFailed;
      END;
   END Shell;

END CallShell.
