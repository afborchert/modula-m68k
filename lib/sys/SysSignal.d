DEFINITION MODULE SysSignal;

   FROM SystemTypes IMPORT Sig;

   VAR
      default, ignore: PROC;
      old: PROC; (* will be set after each successfull Signal-call *)

   PROCEDURE Signal(sig: Sig; p: PROC) : BOOLEAN;

END SysSignal.
