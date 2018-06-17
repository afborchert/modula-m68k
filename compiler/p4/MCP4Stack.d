(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
DEFINITION MODULE MCP4Stack; (* AFB 1/85 *)

   FROM MCP4AttributSys IMPORT Attribut;

   (* size always in fullwords; offset always relative to base *)

   PROCEDURE GetStack(size: CARDINAL; VAR offset: CARDINAL);

   PROCEDURE GetStackAt(VAR at: Attribut); (* at^.typtr^.size is used *)

   PROCEDURE FreeStack(size: CARDINAL; offset: CARDINAL);

   PROCEDURE FreeStackAt(VAR at: Attribut); (* at.mode = stackMod ! *)

   (* following two routines generates no code; *)
   (* they serves for stack use calculation     *)

   PROCEDURE IncTop(incr: CARDINAL);

   PROCEDURE DecTop(dec: CARDINAL);

   PROCEDURE SetStartOffset(offset: CARDINAL);
   (* called by MCP4Block at the begin of a procedure *)

   (* following routines are called at the end of a procedure *)

   PROCEDURE GetStackUse(VAR size: CARDINAL);
   (* return result from GetStack/FreeStack *)

   PROCEDURE GetMaxIncTop(VAR incr: CARDINAL);
   (* return result from IncTop and DecTop *)

END MCP4Stack.
