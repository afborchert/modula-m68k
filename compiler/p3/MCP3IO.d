(****************************************
*                                       *
*     MODULA-2 Multi-Pass Compiler      *
*     ****************************      *
*                                       *
*     Implementation for ENDLICH        *
*                                       *
*     MCP3IO:                           *
*                                       * 
*     input / output handling in Pass 3 *
*                                       * 
****************************************)

DEFINITION MODULE MCP3IO;               (* LG *)

   FROM SYSTEM IMPORT WORD;
   FROM MCBase IMPORT Idptr, Symbol, Spellix, Stset;

   (*
   EXPORT QUALIFIED sy, val, length, spix, nptr, PutSy, PutWord, Savepos,
      InitSave, ResetSave, ReleaseSave, Error, ErrorLS, GetSy, PutGetSy,
      TermInOut, typeset;
   *)

   TYPE 
      Savepos;

   VAR 
      sy     : Symbol;
      val    : CARDINAL;                (* value *)
      typeset: Stset;
      length : CARDINAL;                (* string length *)
      spix   : Spellix;                 (* spelling index of identifier *)
      nptr   : Idptr;                   (* pointer to referenced name *)

   PROCEDURE PutSy(s: Symbol);

   (* put symbol s and pos on output-stream *)

   PROCEDURE PutWord(w: WORD);

   (* put word w on output-stream *)

   PROCEDURE InitSave(VAR s: Savepos);

   PROCEDURE ResetSave(s: Savepos);

   PROCEDURE ReleaseSave(s: Savepos);

   PROCEDURE Error(n: CARDINAL);

   (* put error number n to current symbol *)

   PROCEDURE ErrorLS(n: CARDINAL);

   (* put error number n to last symbol *)

   PROCEDURE GetSy;
   (* get next symbol from input-stream *)

   (* symbol is assigned to sy          *)

   PROCEDURE PutGetSy;

   (* put last symbol sy, get next symbol sy *)

   PROCEDURE TermInOut;

   (* termination of input- and output-streams *)

END MCP3IO. 
