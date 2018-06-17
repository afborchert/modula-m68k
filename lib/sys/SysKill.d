(* Modula-2 Library    -  UNIX System V  -     AFB 9/88 *)
(* (c) Universitaet Ulm, Sektion Informatik, D-7900 Ulm *)
DEFINITION MODULE SysKill;

   FROM SystemTypes IMPORT Sig, ProcessId;

   PROCEDURE Kill(pid: ProcessId; sig: Sig) : BOOLEAN;

END SysKill.
