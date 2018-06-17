MODULE signal;

   CONST nsigs = 20;

   VAR sigfuncs: ARRAY [0..nsigs] OF PROC;

   PROCEDURE Signal(sig: CARDINAL; p: PROC;
		    VAR oldproc: PROC; VAR error: CARDINAL) : BOOLEAN;
   BEGIN
   END Signal;

   PROCEDURE Catchsig(ps, pc: CARDINAL);
   BEGIN
   END Catchsig;

END signal.
