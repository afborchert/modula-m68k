IMPLEMENTATION MODULE SysBreak;

   FROM SYSTEM IMPORT ADDRESS, UNIXCALL;
   FROM Sys IMPORT sbrk;
   FROM Errno IMPORT errno;
   IMPORT SysLocations;

   PROCEDURE Break(addr: ADDRESS) : BOOLEAN;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(sbrk, r0, r1, addr) THEN
	 SysLocations.Break := addr;
	 RETURN TRUE;
      ELSE
	 errno := r0;
	 RETURN FALSE;
      END;
   END Break;

   PROCEDURE Sbreak(incr: CARDINAL) : ADDRESS;
      VAR oldBreak: ADDRESS;
   BEGIN
      oldBreak := SysLocations.Break;
      INC(SysLocations.Break, incr);
      IF Break(SysLocations.Break) THEN
	 RETURN oldBreak;
      ELSE
	 SysLocations.Break := oldBreak;
	 RETURN NIL;
      END;
   END Sbreak;

END SysBreak.
