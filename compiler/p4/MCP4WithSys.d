(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
DEFINITION MODULE MCP4WithSys; (* AFB 9/83 *)

   FROM MCP4AttributSys IMPORT Attribut;

   PROCEDURE WithStatement;

   PROCEDURE UseWith(i: INTEGER; VAR fat: Attribut);

END MCP4WithSys.
