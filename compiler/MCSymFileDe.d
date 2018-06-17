(****************************************
*                                       *
*     MODULA-2 Multi-Pass Compiler      *
*     ****************************      *
*                                       *
*     Implementation for ENDLICH        *
*                                       *
*                                       *
*     MCSymFileDefs:                    *
*                                       * 
*     definition of symbols             *
*     used on symbol files              *
*                                       * 
****************************************)

DEFINITION MODULE MCSymFileDefs;  (* LG *)
				  (* REV AFB 5/84: new symFileKey *)

  (*
  EXPORT QUALIFIED
    symFileKey, SymFileSymbols;
  *)

  CONST
    (* (* old version *)
    symFileKey = 105B; (* identification of symbol file version *)
    symFileKey = 106B; (* new version: with long constants *)
    *)
    symFileKey = 107B; (* new version: with intcarconstss/bigsetconstss *)

  TYPE
    SymFileSymbols =
      (endfileSS,
       unitSS, endunitSS,
       importSS, exportSS,
       constSS, normalconstSS, realconstSS, stringconstSS, bigsetconstSS,
       intcarconstSS,
       typSS, arraytypSS, recordtypSS, settypSS, pointertypSS, hiddentypSS,
       varSS,
       procSS, funcSS,
       identSS,
       periodSS, colonSS, rangeSS,
       lparentSS, rparentSS,
       lbracketSS, rbracketSS,
       caseSS, ofSS, elseSS, endSS);

END MCSymFileDefs.
