(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
DEFINITION MODULE MCP4Test; (* AFB 5/85 *)

   FROM MCP4AttributSys IMPORT ArithmeticType, TestType;
   FROM MCP4Labels IMPORT LabelPtr;

   PROCEDURE Invert(t: TestType) : TestType;

   PROCEDURE Test(test: TestType; atype: ArithmeticType;
                  dest: LabelPtr);
      (* emit code for      "bcc  test,dest"   *)

END MCP4Test.
