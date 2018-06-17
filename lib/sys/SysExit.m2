IMPLEMENTATION MODULE SysExit;

   FROM SYSTEM IMPORT UNIXCALL;
   FROM Sys IMPORT exit;

   CONST
      MaxCleanup = 32;
   VAR
      CleanupProcs: ARRAY[0..MaxCleanup-1] OF PROC;
      index: CARDINAL; (* of CleanupProcs *)
      cleanup: BOOLEAN;

   PROCEDURE Exit(exitCode: CARDINAL);
      VAR r0, r1: CARDINAL; i: CARDINAL;
   BEGIN
      IF NOT cleanup AND (index > 0) THEN
         cleanup := TRUE;
         FOR i := index-1 TO 0 BY -1 DO
            CleanupProcs[i];
         END;
      END;
      IF UNIXCALL(exit, r0, r1, exitCode) THEN
	 (* can this ever happen ??? *)
      END;
      (* NOTREACHED *)
   END Exit;

   PROCEDURE EnterCleanup(p: PROC);
   BEGIN
      IF index < MaxCleanup THEN
         CleanupProcs[index] := p;
         INC(index);
      END;
   END EnterCleanup;

BEGIN
   index := 0;
   cleanup := FALSE;
END SysExit.
