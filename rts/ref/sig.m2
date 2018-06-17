MODULE signal;

   CONST nsigs = 20;

   VAR sigfuncs: ARRAY [0..nsigs] OF PROC;

   PROCEDURE signal(sig: CARDINAL; p: PROC;
		    VAR oldproc: PROC; VAR error: CARDINAL) : BOOLEAN;
   BEGIN
   END signal;

   PROCEDURE catchsig(ps, pc: CARDINAL);
   BEGIN
   END catchsig;

END signal.
