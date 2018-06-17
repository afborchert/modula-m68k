DEFINITION MODULE Storage; (* AFB 3/84 *)

   FROM SYSTEM IMPORT ADDRESS;

   (*
   EXPORT QUALIFIED ALLOCATE, DEALLOCATE, Setmode, EndStorage;
   *)

   PROCEDURE ALLOCATE(VAR a:ADDRESS; n: CARDINAL);

   PROCEDURE DEALLOCATE(VAR a:ADDRESS; n: CARDINAL);

   PROCEDURE Setmode(m: CARDINAL);

   (* not implemented *)

   PROCEDURE EndStorage;

   (* save dynamic storage and several global variables into the storage file *)
END Storage. 
