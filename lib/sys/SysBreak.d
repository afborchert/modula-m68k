DEFINITION MODULE SysBreak;

   FROM SYSTEM IMPORT ADDRESS;

   PROCEDURE Break(addr: ADDRESS) : BOOLEAN;

   PROCEDURE Sbreak(incr: CARDINAL) : ADDRESS;

END SysBreak.
