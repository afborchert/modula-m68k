IMPLEMENTATION MODULE SysPause;

   FROM Sys IMPORT sigblock, sigpause;	(* Nixdorf: pause *)
   FROM SYSTEM IMPORT UNIXCALL;

   PROCEDURE Pause;
      VAR r0, r1: CARDINAL;
	  mask : CARDINAL;
   BEGIN
      IF UNIXCALL(sigblock, r0, r1,0) THEN
	 mask := r0;
	 IF UNIXCALL(sigpause,r0, r1, mask) THEN
	 END;
         (* ignore result *)
      END;
   END Pause;

END SysPause.
