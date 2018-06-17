DEFINITION MODULE MCTypes;

   FROM MCBase IMPORT Stptr, Stset, Constval;

   PROCEDURE IsReal(sp: Stptr) : BOOLEAN;

   PROCEDURE IsCard(sp: Stptr) : BOOLEAN;

   PROCEDURE IsInt(sp: Stptr) : BOOLEAN;

   PROCEDURE IsLong(sp: Stptr) : BOOLEAN;
   (* either LONGINT, LONGCARD or LONGREAL; but not INTEGER, CARDINAL or REAL *)

   PROCEDURE ConstType(const: Constval; VAR type: Stptr);

   PROCEDURE GetType(set: Stset) : Stptr;

   (* following procedures only for constants *)

   PROCEDURE TypeSetCompatible(sp1, sp2: Stptr) : BOOLEAN;

   PROCEDURE TypeSetResult(sp1, sp2: Stptr) : Stptr;

   PROCEDURE ResultOfNegation(sp: Stptr) : Stptr;

END MCTypes.
