DEFINITION MODULE SysGetpid;			(* mh 11/1987 *)

   FROM SystemTypes IMPORT ProcessId;

   PROCEDURE Getpid(): ProcessId;

END SysGetpid.
