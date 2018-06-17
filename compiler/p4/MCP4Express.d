(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
DEFINITION MODULE MCP4ExpressionSys; (* AFB 9/83 *)

   FROM MCP4AttributSys IMPORT Attribut, ArithmeticType;

   PROCEDURE Expression(VAR fat: Attribut);

   PROCEDURE Mult(VAR fat, lat: Attribut; AType: ArithmeticType);

   PROCEDURE Div(VAR fat, lat: Attribut; AType: ArithmeticType);

   PROCEDURE Mod(VAR fat, lat: Attribut; AType: ArithmeticType);

END MCP4ExpressionSys.
