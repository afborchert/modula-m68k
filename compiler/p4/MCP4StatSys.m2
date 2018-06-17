(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
IMPLEMENTATION MODULE MCP4StatSys; (* AFB 9/83 *)

    FROM MCBase IMPORT Symbol, realptr;

    FROM MCP4Scanner IMPORT GetSymbol, sy;
    FROM MCP4AttributSys IMPORT Attribut;
    FROM MCP4ExpressionSys IMPORT Expression;
    FROM MCP4Types IMPORT BaseType;
    FROM MCP4CodeSys IMPORT Emit1;
    FROM MCP4Register IMPORT Reg, FloatReg;
    FROM MCP4Load IMPORT LoadReg, LoadFloatReg;
    FROM MCP4Labels IMPORT LabelPtr, TopLabel;
    FROM MCMnemonics IMPORT Mnemonic;
    FROM MCP4Public IMPORT mc68881;
    FROM MCP4FPEmulator IMPORT FPReturn;

    FROM MCP4IfSys IMPORT IfStatement;
    FROM MCP4WhileSys IMPORT WhileStatement;
    FROM MCP4LoopSys IMPORT LoopStatement, ExitStatement;
    FROM MCP4RepeatSys IMPORT RepeatStatement;
    FROM MCP4ForSys IMPORT ForStatement;
    FROM MCP4CaseSys IMPORT CaseStatement;
    FROM MCP4WithSys IMPORT WithStatement;
    FROM MCP4Assign IMPORT Assignment;
    FROM MCP4Designator IMPORT Designator;
    FROM MCP4CallSys IMPORT ProcFuncCall;

   PROCEDURE ReturnStatement;
      VAR lat: Attribut;
          returnLabel: LabelPtr;
   BEGIN
      IF sy = lparent THEN
         Expression(lat);
         IF BaseType(lat.typtr) = realptr THEN
	    IF mc68881 THEN
	       LoadFloatReg(lat, fr0);
	    ELSE
	       FPReturn(lat);
	    END;
         ELSE
            LoadReg(lat, d0);
         END;
      END;
      returnLabel := TopLabel('B');
      Emit1(BRA, "%l", returnLabel^);
   END ReturnStatement;

   PROCEDURE Statement;
      VAR lat: Attribut;
   BEGIN (* Statement *)
      IF sy = becomes THEN GetSymbol; Assignment
      ELSIF sy = call THEN GetSymbol;
         Designator(lat); GetSymbol(*lparent*); ProcFuncCall(lat);
      ELSIF sy = ifsy THEN GetSymbol; IfStatement;
      ELSIF sy = whilesy THEN GetSymbol; WhileStatement;
      ELSIF sy = loopsy THEN GetSymbol; LoopStatement;
      ELSIF sy = exitsy THEN GetSymbol; ExitStatement;
      ELSIF sy = repeatsy THEN GetSymbol; RepeatStatement;
      ELSIF sy = forsy THEN GetSymbol; ForStatement;
      ELSIF sy = casesy THEN GetSymbol; CaseStatement;
      ELSIF sy = withsy THEN GetSymbol; WithStatement;
      ELSIF sy = returnsy THEN GetSymbol; ReturnStatement;
      (* ELSE empty statement without GetSymbol*)
      END;
   END Statement;

   PROCEDURE StatSequ1(s: Symbol);
   BEGIN
      REPEAT
         Statement;
      UNTIL sy = s;
   END StatSequ1;

   PROCEDURE StatSequ3(s1, s2, s3: Symbol);
   BEGIN
      REPEAT
         Statement;
      UNTIL (sy = s1) OR (sy = s2) OR (sy = s3);
   END StatSequ3;

END MCP4StatSys.
