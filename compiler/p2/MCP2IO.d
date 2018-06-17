(****************************************
*                                       *
*     MODULA-2 Multi-Pass Compiler      *
*     ****************************      *
*                                       *
*     Implementation for ENDLICH        *
*                                       *
*     MCP2IO:                           *
*                                       * 
*     input / output handling in Pass 2 *
*                                       * 
****************************************)

DEFINITION MODULE MCP2IO;   (* LG *)

  FROM SYSTEM IMPORT WORD;
  FROM MCBase IMPORT Symbol, Spellix, Keyarr, Stset;

  (*
  EXPORT QUALIFIED sy, typeset, PutSy, PutWord, StopOutput, RestartOutput,
    Error, ErrorLS, line, pos, spix, maxspix, val, length, GetSy, PutGetSy,
    TermInOut, GetModuleKey, DefModStatus, AsciiSetPos, AsciiRead,
    SkipConstant, SkipType;
  *)

  CONST maxspix = ORD(MAX(INTEGER)); (* maximal value of legal spix *)

  VAR sy : Symbol;
      line : CARDINAL;
      pos : CARDINAL;
      spix : Spellix;
      val : CARDINAL;
      typeset : Stset;
      length : CARDINAL; (* string length *)

  PROCEDURE PutSy(s : Symbol);
    (* put Symbol ans pos on il2-file *)

  PROCEDURE PutWord(w : WORD);
    (* put word on il2-file *)

  PROCEDURE StopOutput;
  
  PROCEDURE RestartOutput;

  PROCEDURE Error(n: CARDINAL);
    (* no suppression of writing on il2 file *)

  PROCEDURE ErrorLS(n: CARDINAL);

  PROCEDURE GetSy;

  PROCEDURE PutGetSy; 
         
  PROCEDURE TermInOut;

  PROCEDURE GetModuleKey(VAR modkey: Keyarr);

  PROCEDURE DefModStatus;

  PROCEDURE AsciiSetPos(spix: Spellix); 
    (* set position on ASCII file *) 

  PROCEDURE AsciiRead(VAR ch: CHAR);   
    (* read character from ASCII file *) 

  PROCEDURE SkipConstant;
    (* skip a constant in a symbol module *)

  PROCEDURE SkipType;
    (* skip type structures in a symbol module *)

END MCP2IO.
