(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
DEFINITION MODULE MCP4Out;

   (*
   EXPORT QUALIFIED
      WriteString, Write, WriteLn, TermOut;
   *)

   PROCEDURE WriteString(str: ARRAY OF CHAR);

   PROCEDURE Write(ch: CHAR);

   PROCEDURE WriteLn;

   PROCEDURE TermOut;

END MCP4Out.
