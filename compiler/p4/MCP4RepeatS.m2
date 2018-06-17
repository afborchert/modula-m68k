(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
IMPLEMENTATION MODULE MCP4RepeatSys; (* AFB 9/83 *)
 
   FROM Storage IMPORT DEALLOCATE;
   FROM MCBase IMPORT Symbol;
   FROM MCP4Scanner IMPORT GetSymbol;
   FROM MCP4AttributSys IMPORT Attribut, AtMode;
   FROM MCP4CodeSys IMPORT AppendComment, EmitLabel, EmitEQU;
   FROM MCP4ExpressionSys IMPORT Expression;
   FROM MCP4Labels IMPORT GetLabel, LabelType, LabelPtr;
   FROM MCP4Load IMPORT Load, LoadCond;
   FROM MCP4Register IMPORT Reg, FreeReg;
   FROM MCP4StatSys IMPORT StatSequ1;
   FROM MCP4Test IMPORT Test, Invert;
    
   PROCEDURE RepeatStatement;    
      VAR fat: Attribut;
 	 Label: LabelPtr;
   BEGIN
      AppendComment("repeat-statement");
      GetLabel('R', Label);
      EmitLabel(Label);
      StatSequ1(untilsy);
      GetSymbol;  
      Expression(fat);
      WITH fat DO
	 IF mode <> conditionMod THEN
	    LoadCond(fat);
	 END;
         test := Invert(test);
	 Test(test, atype, Label);
	 IF flabel <> NIL THEN
	    EmitEQU(flabel^, Label^);
	    DISPOSE(flabel);
	 END;
	 IF tlabel <> NIL THEN
	    EmitLabel(tlabel);
	    DISPOSE(tlabel);
	 END;
      END;
      AppendComment("end-repeat");
      DISPOSE(Label);
   END RepeatStatement;  
 
END MCP4RepeatSys.
