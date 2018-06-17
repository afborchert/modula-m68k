IMPLEMENTATION MODULE SysAlarm;

   FROM SYSTEM IMPORT UNIXCALL, ADR;
   FROM Sys IMPORT setitimer;	(* Nixdorf: svc call alarm *)
   FROM Errno IMPORT errno;

   PROCEDURE Alarm(sec: CARDINAL) : BOOLEAN;
      TYPE
	 TIMEVAL = RECORD
			tvsec : CARDINAL;
			tvusc : CARDINAL;
		   END(* RECORD *);
	 ITIMERVAL = RECORD
			itinterval : TIMEVAL;
			itvalue  : TIMEVAL;
		     END (* RECORD *);
      VAR r0, r1: CARDINAL;
	  value : ITIMERVAL;
	  oldvalue: ITIMERVAL;
   BEGIN
      WITH value DO
	 itinterval.tvsec := 0;
	 itinterval.tvusc := 0;
	 itvalue.tvsec := sec;
	 itvalue.tvusc := 0;
      END;
      IF UNIXCALL(setitimer, r0, r1,
		  (* ITIMER_REAL = *) 0,
		  ADR(value), ADR(oldvalue)) THEN
         previous := oldvalue.itvalue.tvsec;
         RETURN TRUE
      ELSE
         errno := r0;
         RETURN FALSE
      END;
   END Alarm;

END SysAlarm.
