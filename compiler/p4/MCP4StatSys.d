(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
DEFINITION MODULE MCP4StatSys; (* AFB 9/83 *)

   FROM MCBase IMPORT Symbol;

   PROCEDURE Statement;

   PROCEDURE StatSequ1(s: Symbol);

   PROCEDURE StatSequ3(s1, s2, s3: Symbol);

END MCP4StatSys.
