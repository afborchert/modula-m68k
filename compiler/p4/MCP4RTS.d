DEFINITION MODULE MCP4RTS;

   CONST
      (* local labels *)
      global = "G_AREA";    	(* all global variables are behind this *)
      flag   = "_FLAG"; 	(* module initialized? *)
      startLabel = "_M2START";	(* start execution of main module here *)
      procprefix = "LP$";	(* proc labels consists of LP$ and a number *)

      (* run time subroutines *)
      entprio = "_entprio";	(* enter priority *)
      exprio = "_exprio";	(* exit priority *)
      mcount = "_mcount";	(* count procedure calls for profiling *)
      signal = "_signal";	(* Modula-2 interface to signal() *)
      newproc = "_newproc";	(* NEWPROCESS *)
      transfer = "_transfer";	(* TRANSFER *)

      (* run time error aborts *)
      fret = ".fret";		(* function returns no value error *)
      caseErr = ".case";	(* ELSE-case but no ELSE *)
      halt = ".halt";		(* call to procedure HALT *)
      stack = ".stack";		(* stack overflow *)
      chksigned = ".chksig";	(* signed range check *)
      chkunsigned = ".chkusig";	(* unsigned range check *)
      chkdyn = ".chkdyn";	(* index beyond bounds of dynamic array *)
      chksign = ".chksign";	(* INTEGER/CARDINAL conflict *)

      (* global variables *)
      argcLabel = "_ARGC";	(* argument count *)
      argvLabel = "_ARGV";	(* pointer to argument list *)

      (* symbolic debugging constants *)
      NFUN = "044";
      NSLINE = "0104";
      NSO = "0144";
      NENTRY = "0244";

END MCP4RTS.
