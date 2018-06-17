(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
IMPLEMENTATION MODULE MCP4LoopSys; (* AFB 9/83 *)

   FROM Storage IMPORT DEALLOCATE;
   FROM MCBase IMPORT Symbol;
   FROM MCMnemonics IMPORT Mnemonic;
   FROM MCP4Scanner IMPORT GetSymbol;
   FROM MCP4StatSys IMPORT StatSequ1;
   FROM MCP4Global IMPORT Error;
   FROM MCP4Labels IMPORT LabelType, LabelPtr, GetLabel, PushLabel,
      PopLabel, TopLabel;
   FROM MCP4CodeSys IMPORT EmitLabel, AppendComment, Emit1;
   FROM MCP4Register IMPORT Reg;
   
   VAR
      exitLabel: LabelPtr;
      level: CARDINAL;

   PROCEDURE LoopStatement;   
      VAR loopLabel: LabelPtr;
   BEGIN
      INC(level);
      AppendComment("loop-statement");
      GetLabel('L', loopLabel);
      EmitLabel(loopLabel);
      GetLabel('L', exitLabel);
      PushLabel('L', exitLabel);
      StatSequ1(endsy);
      Emit1(BRA, "%l", loopLabel^);
      GetSymbol; 
      exitLabel := PopLabel('L');
      AppendComment("end-loop");
      EmitLabel(exitLabel);
      DISPOSE(exitLabel);
      DISPOSE(loopLabel);
      DEC(level);
   END LoopStatement;  
   
   PROCEDURE ExitStatement;   
   BEGIN  
      IF level = 0 THEN
	 Error(151);
      ELSE
	 exitLabel := TopLabel('L');
	 Emit1(BRA, "%l", exitLabel^);
      END
   END ExitStatement;  

BEGIN
   level := 0;
END MCP4LoopSys.
