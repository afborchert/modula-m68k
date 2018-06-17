IMPLEMENTATION MODULE SysTime;

   FROM SYSTEM IMPORT UNIXCALL, ADR;
   FROM Sys IMPORT gettimeofday;	(*Nixdorf: time *)
   FROM SystemTypes IMPORT TIME;
   FROM Errno IMPORT errno;

TYPE
   TIMEVAL = RECORD
		tvsec : CARDINAL;
		tvusec : CARDINAL;
	     END;
   TIMEZONE = RECORD
		tzminwes : INTEGER;
		tzdsttim : INTEGER;
	      END;

   PROCEDURE Time(VAR t: TIME) : BOOLEAN;
      VAR d0, d1: CARDINAL;
	   tp : TIMEVAL;
	   tzp : TIMEZONE;
   BEGIN
      IF UNIXCALL(gettimeofday, d0, d1, ADR(tp), ADR(tzp)) THEN
	 t := tp.tvsec;
	 RETURN TRUE
      ELSE
	 errno := d0;
	 RETURN FALSE
      END;
   END Time;

END SysTime.
