(****************************************
*                                       *
*     MODULA-2 Multi-Pass Compiler      *
*     ****************************      *
*                                       *
*     Implementation for ENDLICH        *
*                                       *
*     MCPublic:                         *
*                                       * 
*     public part of the common base    *
*     of the Modula-2 compiler          *
*                                       * 
****************************************)

DEFINITION MODULE MCP2Public;             (* LG *) (* REV AFB 3/84 *)

   (*
   EXPORT QUALIFIED
      ascName, refName, il1Name, il2Name, ErrorsFound,
      FileNameLength, FileName;
   *)

   CONST
      FileNameLength = 64;
   TYPE
      FileName = ARRAY[0..FileNameLength-1] OF CHAR;
   VAR 
      ascName : FileName;                   (* identifier table file *)
      refName : FileName;                   (* reference file *)
      il1Name : FileName;		    (* interpass file (from m0) *)
      il2Name : FileName;		    (* interpass file (for m2) *)

      ErrorsFound : BOOLEAN;

END MCP2Public. 
