(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
DEFINITION MODULE MCP4Types; (* AFB 9/83 *)

   FROM MCBase IMPORT Stptr;

   FROM MCP4AttributSys IMPORT
      Attribut, ArithmeticType;

   (*
   EXPORT QUALIFIED
      IsArrayType, ByteSize, IsSetType, BaseType, TestBaseType,
      ResultType, Arithmetic, SimpleType, SizeType;
   *)
   
   VAR intcarptr: Stptr;

   PROCEDURE IsArrayType(ft: Stptr) : BOOLEAN;

   PROCEDURE ByteSize(ft: Stptr) : BOOLEAN;

   PROCEDURE IsSetType(ft: Stptr) : BOOLEAN;

   PROCEDURE BaseType(ft: Stptr) : Stptr;

   PROCEDURE TestBaseType(ft: Stptr) : Stptr;

   PROCEDURE ResultType(VAR fat1, fat2: Attribut) : Stptr;

   PROCEDURE Arithmetic(VAR fat1, fat2: Attribut) : ArithmeticType;

   PROCEDURE SimpleType(VAR fat: Attribut) : BOOLEAN;

   PROCEDURE SizeType(VAR fat: Attribut): INTEGER;   

END MCP4Types.
