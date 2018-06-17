DEFINITION MODULE SysFork;

   (* IF pid = 0 THEN son ELSE father END *)

   PROCEDURE Fork(VAR pid: CARDINAL) : BOOLEAN;

END SysFork.
