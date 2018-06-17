DEFINITION MODULE SysExit;

   PROCEDURE Exit(errorCode: CARDINAL);

   PROCEDURE EnterCleanup(p: PROC);

END SysExit.
