(****************************************
*                                       *
*     MODULA-2 Multi-Pass Compiler      *
*     ****************************      *
*                                       *
*     Implementation for ENDLICH        *
*                                       *
*     running on ENDLICH                *
*                                       *
*     MCStop:                           *
*                                       * 
*     abnormal termination              *
*     of the Modula-2 compiler          *
*                                       * 
****************************************)

DEFINITION MODULE MCStop;

   (*
   EXPORT QUALIFIED Stop;
   *)

   PROCEDURE Exit1;

   PROCEDURE Stop(exitCode: CARDINAL);

END MCStop.
