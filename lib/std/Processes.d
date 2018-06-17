DEFINITION MODULE Processes;

   TYPE SIGNAL; (* hidden *)

   PROCEDURE StartProcess(P: PROC; n: CARDINAL);
   (* start a concurrent process with program P and workspace of size n. *)
   (* PROC is a standard type defined as PROC = PROCEDURE                *)

   PROCEDURE SEND(VAR s: SIGNAL);
   (* one process waitung for s is resumed *)

   PROCEDURE WAIT(VAR s: SIGNAL);
   (* wait for some other process to send s *)

   PROCEDURE Awaited(s: SIGNAL) : BOOLEAN;
   (* Awaited(s) = "at least one process is waiting for s" *)

   PROCEDURE Init(VAR s: SIGNAL);
   (* compulsory initialization *)

END Processes.
