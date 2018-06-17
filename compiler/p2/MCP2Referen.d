(****************************************
*                                       *
*     MODULA-2 Multi-Pass Compiler      *
*     ****************************      *
*                                       *
*     Implementation for ENDLICH        *
*                                       *
*                                       *
*     MCP2Reference:                    *
*                                       * 
*     Generation of reference file      *
*                                       * 
****************************************)

DEFINITION MODULE MCP2Reference; (* LG *) 

  FROM MCBase IMPORT Idptr;

  (*
  EXPORT QUALIFIED Reference, EndReference, InitRef,TermRef;
  *)

  PROCEDURE Reference(ip : Idptr);
    (* write reference to identifier *)

  PROCEDURE EndReference(ip : Idptr);
    (* write end of reference to identifier *)

  PROCEDURE InitRef;
    (* initialisation of ref file *)

  PROCEDURE TermRef;
    (* termination of ref file *)

END MCP2Reference.
