(****************************************
*                                       *
*     MODULA-2 Multi-Pass Compiler      *
*     ****************************      *
*                                       *
*     Implementation for ENDLICH        *
*                                       *
*                                       *
*     MCP1Reals:                        *
*                                       * 
*     Real constant handling in Pass 1  *
*                                       * 
****************************************)

DEFINITION MODULE MCP1Reals;            (* LG *)

   FROM MCBase IMPORT Constval;

   (*
   EXPORT QUALIFIED InitRealConst, ConvertToFraction, ConvertToExponent,
      TermRealConst;
   *)

   PROCEDURE InitRealConst;

   (* initialise the calculation of a constant real number *)

   PROCEDURE ConvertToFraction(ch: CHAR);

   (* convert a character to the fraction of a constant real number *)

   PROCEDURE ConvertToExponent(ch: CHAR);

   (* convert a character to the exponent of a constant real number *)

   PROCEDURE TermRealConst(VAR cval: Constval; VAR long, err: BOOLEAN);

   (* terminate the calculation of a constant real number *)

END MCP1Reals. 
