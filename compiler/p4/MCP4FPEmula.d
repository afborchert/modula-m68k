(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
DEFINITION MODULE MCP4FPEmulator;

   FROM MCBase IMPORT Symbol;
   FROM MCP4AttributSys IMPORT Attribut;

   PROCEDURE FPOp(op: Symbol; VAR at1, at2: Attribut);
      (* at1 op at2; result in at1; at2 is subject to Cleanup *)

   PROCEDURE FPFloat(VAR at: Attribut);

   PROCEDURE FPTrunc(VAR at: Attribut);

   PROCEDURE FPAbs(VAR at: Attribut);

   PROCEDURE FPNeg(VAR at: Attribut);

   PROCEDURE FPLoad(VAR at: Attribut);

   PROCEDURE FPStore(VAR at, des: Attribut);
      (* store at to des; no cleanup done *)

   PROCEDURE FPPush(VAR at: Attribut);
      (* push at onto stack; no cleanup done *)

   PROCEDURE FPReturn(VAR at: Attribut);
      (* with cleanup of at *)

   PROCEDURE FPGetReturnVal(VAR at: Attribut);

END MCP4FPEmulator.
