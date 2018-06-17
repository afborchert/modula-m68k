(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
DEFINITION MODULE MCP4RangeChecks;

   FROM MCBase IMPORT Stptr;
   FROM MCP4AttributSys IMPORT Attribut;

   PROCEDURE RangeCheckForConstant(dest: Stptr; VAR fat: Attribut);

   PROCEDURE RangeCheck(dest: Stptr; VAR fat: Attribut; check: BOOLEAN);

   PROCEDURE CheckStack;

END MCP4RangeChecks.
