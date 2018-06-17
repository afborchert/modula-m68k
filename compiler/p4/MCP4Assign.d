(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
DEFINITION MODULE MCP4Assign; (* AFB 9/83 *)

   FROM MCP4AttributSys IMPORT Attribut;

   PROCEDURE Assignment;

   PROCEDURE Assign(VAR desAT, expAT: Attribut);

   PROCEDURE MoveNBytes(VAR desAT, sourceAT: Attribut; n: CARDINAL);

END MCP4Assign.
