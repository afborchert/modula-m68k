(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
IMPLEMENTATION MODULE MCP4Assign;       (* AFB 11/83 *)
                                        (* REV AFB 5/84 *)

   FROM Storage IMPORT DEALLOCATE;
   FROM MCBase IMPORT realptr, onebyte, oneword, doubleword, Structform;
   FROM MCP4AttributSys IMPORT Attribut, AtMode;
   FROM MCP4ExpressionSys IMPORT Expression;
   FROM MCP4Designator IMPORT Designator;
   FROM MCP4Scanner IMPORT GetSymbol, arithmeticRangeCheck;
   FROM MCP4Load IMPORT Load, LoadAddr;
   FROM MCP4Types IMPORT SizeType, BaseType, SimpleType, ByteSize;
   FROM MCP4CodeSys IMPORT EmitComment, EmitLabel, Emit1, Emit2;
   FROM MCP4RangeChecks IMPORT RangeCheck;
   FROM MCP4Register IMPORT GetReg, FreeReg, FreeFloatReg, Reg;
   FROM MCP4Labels IMPORT LabelPtr, GetLabel, LabelType, Label;
   FROM MCMnemonics IMPORT Mnemonic;
   FROM MCP4Address IMPORT Cleanup;
   FROM MCP4Public IMPORT mc68881;
   FROM MCP4FPEmulator IMPORT FPStore;

   PROCEDURE MoveNBytes(VAR desAT, sourceAT: Attribut; n: CARDINAL);
      VAR loopLabel: LabelPtr;
   BEGIN
      LoadAddr(desAT);
      LoadAddr(sourceAT);
      IF n MOD oneword = 0 THEN
	 Emit2(MOVE, "%L%c,%r", n DIV oneword - 1, d0);
      ELSE
	 Emit2(MOVE, "%L%c,%r", n - 1, d0);
      END;
      GetLabel('L', loopLabel);
      EmitLabel(loopLabel);
      IF n MOD oneword = 0 THEN
	 Emit2(MOVE, "%L%r@+,%r@+", sourceAT.addrReg, desAT.addrReg);
      ELSE
	 Emit2(MOVE, "%B%r@+,%r@+", sourceAT.addrReg, desAT.addrReg);
      END;
      Emit2(DBF, "%r,%l", d0, loopLabel^);
      DISPOSE(loopLabel);
   END MoveNBytes;

   PROCEDURE Assign(VAR desAT, expAT: Attribut);
      VAR size: CARDINAL;
	  lReg: Reg;
   BEGIN
      size := SizeType(desAT); (* use desAt in case of variants ! *)
      IF (BaseType(expAT.typtr) = realptr)
          (* in case of type conversions *)
          OR (expAT.mode = floatLoadedMod) THEN
	 IF mc68881 THEN
	    Load(expAT);
	    (* expAT.mode = floatLoadedMod *)
	    Emit2(FMOVE, "%A%a,%a", expAT, desAT);
	 ELSE
	    FPStore(expAT, desAT);
	 END;
      ELSIF (size > oneword) OR (expAT.mode = stringConstMod)
                             (* use desAt ! (char := "x") *)
                             AND NOT ByteSize(desAT.typtr) OR
                             (expAT.typtr^.form = bigsets) AND
			     (expAT.mode <> loadedMod) THEN
	 MoveNBytes(desAT, expAT, size);
      ELSE
	 RangeCheck(desAT.typtr, expAT, arithmeticRangeCheck);
	 IF expAT.mode = conditionMod THEN
	    Load(expAT);
	 END;
	 WITH expAT DO
	    IF (mode = constantMod) AND (value = 0) THEN
	       Emit1(CLR, "%A%a", desAT);
	    ELSE
	       Emit2(MOVE, "%A%#a,%a", expAT, desAT);
	    END;
	 END;
      END;
      Cleanup(expAT); Cleanup(desAT);
   END Assign;

   PROCEDURE Assignment;
      VAR 
	 lat1, lat2: Attribut;
   BEGIN 
      Designator(lat1);
      GetSymbol;
      Expression(lat2);
      Assign(lat1, lat2);
   END Assignment;

END MCP4Assign. 
