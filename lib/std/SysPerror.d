DEFINITION MODULE SysPerror; (* AFB 2/84 *)

   PROCEDURE Perror(str: ARRAY OF CHAR);

   PROCEDURE GetErrorString(errno: CARDINAL; VAR str: ARRAY OF CHAR);

END SysPerror.
