(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
DEFINITION MODULE MCP4Designator; (* AFB 9/83 *)

   FROM MCP4AttributSys IMPORT Attribut;

   PROCEDURE Designator(VAR fat: Attribut);

   PROCEDURE SwitchAddr(VAR fat: Attribut; x: CARDINAL);
      (* increment effective address of fat *)

END MCP4Designator.
