(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
IMPLEMENTATION MODULE MCP4WhileSys; (* AFB 9/83 *)

   FROM Storage IMPORT DEALLOCATE;
   FROM MCBase IMPORT Symbol;
   FROM MCP4Block IMPORT EnterEQU;
   FROM MCP4AttributSys IMPORT Attribut, AtMode;
   FROM MCP4Labels IMPORT LabelType, LabelPtr, GetLabel;
   FROM MCP4Register IMPORT Reg, FreeReg;
   FROM MCP4CodeSys IMPORT AppendComment, EmitLabel, Emit1;
   FROM MCP4Scanner IMPORT GetSymbol, Skip;
   FROM MCP4StatSys IMPORT StatSequ1;
   FROM MCP4ExpressionSys IMPORT Expression;
   FROM MCP4Load IMPORT Load, LoadCond;
   FROM MCMnemonics IMPORT Mnemonic;
   FROM MCP4Test IMPORT Test, Invert;

   PROCEDURE WhileStatement;   
      VAR fat: Attribut;
	  endwhileLabel, beginwhileLabel : LabelPtr;
   BEGIN
      AppendComment("while-statement");
      GetLabel('W', beginwhileLabel);
      EmitLabel(beginwhileLabel);

      Expression(fat);
      IF fat.mode = constantMod THEN
         IF VAL(BOOLEAN,fat.value) THEN
            StatSequ1(endsy);
	    Emit1(BRA, "%l", beginwhileLabel^);
         ELSE
            Skip(endsy, endsy);
         END;
      ELSE
	 WITH fat DO
	    IF fat.mode <> conditionMod THEN
	       LoadCond(fat);
	    END;
            IF fat.flabel <> NIL THEN
               endwhileLabel := fat.flabel;
            ELSE
               GetLabel('W', endwhileLabel);
            END;
	    test := Invert(test);
	    Test(test, atype, endwhileLabel);
	    IF tlabel <> NIL THEN
	       EmitLabel(tlabel);
	       DISPOSE(tlabel);
	    END;
	 END;
         StatSequ1(endsy);  
	 Emit1(BRA, "%l", beginwhileLabel^);
      END;
      AppendComment("endwhile");
      IF fat.mode <> constantMod THEN
         EmitLabel(endwhileLabel);
         DISPOSE(endwhileLabel);
      END;
      DISPOSE(beginwhileLabel);
      GetSymbol;
   END WhileStatement;  

END MCP4WhileSys.
