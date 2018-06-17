IMPLEMENTATION MODULE RandomGenerator;

   FROM SYSTEM IMPORT ADR, UNIXCALL;
   FROM Sys IMPORT getpid, gettimeofday;
   FROM SystemTypes IMPORT TIME;
   FROM SysTime IMPORT Time;

   CONST 
      modulo = 65536;
      add    = 13;
      mult   = 117;

   VAR 
      randzahl : INTEGER;

   PROCEDURE RandomInit;
      VAR seed: TIME;

      PROCEDURE Times(): INTEGER;
	 TYPE Timeval = RECORD sec,usec: CARDINAL END;
	      Timezone = RECORD minwest, dsttime : CARDINAL END;
	 VAR val: Timeval;
	     zone :Timezone;
	     r0, r1: INTEGER;
      BEGIN
	 IF NOT UNIXCALL(gettimeofday,r0,r1,ADR(val),ADR(zone)) THEN END;
	 RETURN val.usec
      END Times;

      PROCEDURE GetPid(): INTEGER;
	 VAR r0, r1: INTEGER;
      BEGIN
	 IF NOT UNIXCALL(getpid, r0, r1) THEN END;
	 RETURN r0 + r1
      END GetPid;

   BEGIN 
      IF NOT Time(seed) THEN
	 randzahl := Times();
      ELSE
	 randzahl := INTEGER(seed) * GetPid() + Times();
      END;
      randzahl := ABS(randzahl) MOD modulo 
   END RandomInit;

   PROCEDURE Random(a, b: INTEGER) : INTEGER;
   BEGIN 
      randzahl := (mult*(randzahl+add)) MOD modulo;
      RETURN a + ((b + 1 - a)*randzahl) DIV modulo;
   END Random;

BEGIN
   RandomInit;
END RandomGenerator. 
