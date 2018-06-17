(* Modula-2 Library    -  UNIX System V  -     AFB 6/88 *)
(* (c) Universitaet Ulm, Sektion Informatik, D-7900 Ulm *)
IMPLEMENTATION MODULE Delay;

   FROM StdIO IMPORT FILE, stdout, Fputc;

   (* (* exported from definition module *)
   TYPE
      OutProc = PROCEDURE (CHAR);
   *)
   TYPE
      OutMode = (procedure, filep);
      DelayRec =
	 RECORD
	    baudrate: CARDINAL;
	    padch: CHAR;
	    CASE outmode: OutMode OF
	    | procedure: outc: OutProc;
	    | filep:     fp: FILE;
	    END;
	 END;
   VAR
      delay: DelayRec;

   PROCEDURE InitDelay(baudrate: CARDINAL; padch: CHAR; outc: OutProc);
   BEGIN
      delay.baudrate := baudrate;
      delay.padch := padch;
      delay.outmode := procedure;
      delay.outc := outc;
   END InitDelay;

   PROCEDURE InitDelayFile(baudrate: CARDINAL; padch: CHAR; fp: FILE);
   BEGIN
      delay.baudrate := baudrate;
      delay.padch := padch;
      delay.outmode := filep;
      delay.fp := fp;
   END InitDelayFile;

   PROCEDURE Delay(tenthofmillisecs: CARDINAL);
      CONST
	 second = 10000; (* in tenth of millisecs *)
      VAR
	 cnt: CARDINAL;  (* number of pad chars to be printed *)
	 i: CARDINAL;
   BEGIN
      WITH delay DO
	 IF (tenthofmillisecs > 0) AND (baudrate > 0) THEN
	    cnt := baudrate * tenthofmillisecs DIV second;
	    INC(cnt);
	    FOR i := 1 TO cnt DO
	       IF outmode = filep THEN
		  IF NOT Fputc(padch, fp) THEN END;
	       ELSE (* outmode = procedure *)
		  outc(padch);
	       END;
	    END;
	 END;
      END;
   END Delay;

BEGIN
   InitDelayFile(9600, 0C, stdout);
END Delay.
