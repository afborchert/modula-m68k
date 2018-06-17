(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
IMPLEMENTATION MODULE MCP4RangeChecks; (* CHJ *) (* REV AFB 7/84 *)

   FROM MCBase IMPORT Stptr, oneword, maxint, maxcard, intptr, cardptr,
      charptr, Structform, addrptr, longintptr;
   FROM MCP4AttributSys IMPORT AtMode, Attribut;
   FROM MCP4CodeSys IMPORT EmitDCFValue, EmitExtern, Emit1, Emit2,
      EmitLabel, EmitComment, AppendComment, EmitPure, EmitPure1;
   FROM MCP4Labels IMPORT GetLabel, LabelPtr;
   FROM MCP4Global IMPORT Assert, Error;
   FROM MCP4Load IMPORT Load, LoadAndExpand;
   FROM MCMnemonics IMPORT Mnemonic;
   FROM MCP4Register IMPORT Reg;
   FROM MCP4Types IMPORT TestBaseType, SizeType, BaseType, intcarptr;
   FROM MCP4Public IMPORT sflag;
   FROM MCP4RTS IMPORT chksigned, chkunsigned, chksign, stack;
   FROM MCP4Stack IMPORT IncTop, DecTop;
   FROM Storage IMPORT DEALLOCATE;

   (*
    *	tests assignment of a constant to a typed variable;
    *	constant in an Attribut variable allows distinction
    *	of very negative integers from very big cardinals
    *)

   PROCEDURE RangeCheckForConstant(dest: Stptr; VAR fat: Attribut);
      VAR base: Stptr;
   BEGIN
      WITH fat DO
         IF mode = stringConstMod THEN
            CharCast(fat);
            (* mode = constantMod *)
         END;
	 IF mode = constantMod THEN
	    base := TestBaseType(dest);
	    WITH dest^ DO
	       IF form = subranges THEN
		  IF base = intptr THEN
		     IF (INTEGER(max) < iValue) OR
			(INTEGER(min) > iValue) THEN
			Error(222);
		     END;
		  ELSE
		     IF (max < value) OR (min > value) THEN
			Error(222);
		     END;
		  END;
	       END;
	       IF base = intptr THEN
		  IF (TestBaseType(typtr) = cardptr) AND (value > maxint) THEN
		     Error(222);
		  END;
	       ELSIF base = cardptr THEN
		  IF (TestBaseType(typtr) = intptr) AND (value > maxint) THEN
		     Error(222);
		  END;
	       ELSIF dest = charptr THEN
		  IF value > 377B THEN
		     Error(222);
		  END;
	       ELSIF form = enums THEN
		  IF value > cstnr THEN
		     Error(222);
		  END;
	       END;
	    END; (* WITH dest DO *)
	 END; (* IF mode = constantMod *)
      END; (* WITH fat DO *)
   END RangeCheckForConstant;

   (*
    *	may load fat if not constant
    *
    *	assignment of a subrange to another subrange is tested;
    *	this checks against not initialized variables
    *)

   PROCEDURE RangeCheck(dest: Stptr; VAR fat: Attribut; check: BOOLEAN);
      TYPE
	 CheckType = (signed, unsigned);
      VAR source: Stptr;

      PROCEDURE Check(ct: CheckType; low, high: CARDINAL);
	 VAR bounds, ok: LabelPtr;
      BEGIN
	 GetLabel('c', bounds); GetLabel('c', ok);
	 LoadAndExpand(fat);
	 AppendComment("range check");
	 Emit2(CMP2, "%L%l,%r", bounds^, fat.loadReg);
	 Emit1(BCC, "%l", ok^);
	 Emit2(MOVE, "%L%r,%r", fat.loadReg, d0);
	 IncTop(2*oneword); DecTop(2*oneword);
	 Emit1(PEA, "%l", bounds^); (* necessary due to as-bug *)
	 IF ct = unsigned THEN
	    EmitExtern(chkunsigned);
	    Emit1(JSR, "%l", chkunsigned);
	 ELSE
	    EmitExtern(chksigned);
	    Emit1(JSR, "%l", chksigned);
	 END;
	 EmitPure1; (* necessary due to as-bug *)
	 EmitLabel(bounds);
	 AppendComment("lower bound");
	 EmitDCFValue(INTEGER(low));
	 AppendComment("upper bound");
	 EmitDCFValue(INTEGER(high));
	 EmitPure;
	 EmitLabel(ok);
	 DISPOSE(bounds); DISPOSE(ok);
      END Check;

      PROCEDURE CheckSign;
	 VAR ok: LabelPtr;
      BEGIN
	 GetLabel('c', ok);
	 LoadAndExpand(fat);
	 EmitComment("check for sign bit");
	 Emit1(TST, "%L%r", fat.loadReg);
	 Emit1(BPL, "%l", ok^);
	 EmitExtern(chksign);
	 IncTop(oneword); DecTop(oneword);
	 Emit1(JSR, "%l", chksign);
	 EmitLabel(ok);
	 DISPOSE(ok);
      END CheckSign;

   BEGIN
      WITH fat DO
	 source := typtr;
	 IF source = dest THEN
	    (* ok *)
	 ELSIF mode = constantMod THEN
	    RangeCheckForConstant(dest, fat);
	 ELSIF check AND (SizeType(fat) <= oneword) THEN
	    IF TestBaseType(dest) = intptr THEN
	       (* includes intcar subranges of INTEGER *)
	       IF dest^.form <> subranges THEN
		  IF (source = cardptr) OR
		     (source^.form = subranges) AND
		     (BaseType(source) = cardptr) THEN
		     CheckSign;
		  END;
	       ELSE
		  (*
		   *	this test may be too hard, but detects not
		   *	initialized vars
		   *)

		  (* check for range [dest^.min .. dest^.max] *)
		  Check(signed, dest^.min, dest^.max);
		  IF (TestBaseType(source) <> intptr) AND
		     (INTEGER(dest^.min) < 0) THEN
		     CheckSign;
		  END;
	       END;
	    ELSIF BaseType(dest) = cardptr THEN
	       (*
		*	includes intcar subranges of CARDINAL but not
		*	intcar itself
		*)
	       
	       IF dest = addrptr THEN RETURN
               ELSIF dest^.form <> subranges THEN (* intptr, longintptr *)
                  IF BaseType(source) = intptr THEN
		     CheckSign;
		  END;
	       ELSE
		  (*
		   *	dest <> cardptr --> dest^.form = subranges
		   *
		   *	guarantee dest^.form = subranges; otherwise is
		   *	max meaningless
		   *)

		  (* check unsigned range [dest^.min .. dest^.max] *)
		  Check(unsigned, dest^.min, dest^.max);
		  (* intcar is only used for subranges *)
		  IF (TestBaseType(source) <> cardptr) AND
		     (TestBaseType(source) <> intcarptr) AND
		     (dest^.max > maxint) THEN
		     CheckSign;
		  END;
	       END;
	    ELSIF dest^.form = subranges THEN
	       (* but not of card, int, intcar *)
	       (* check range [dest^.min .. dest^.max] *)
	       Check(signed, dest^.min, dest^.max);
	    ELSIF (dest = charptr) AND (BaseType(source) <> charptr) THEN
	       (* check range [0 .. 377B] *)
	       Check(unsigned, 0, 377B);
	    ELSIF (dest^.form = enums) AND (BaseType(source) <> dest) THEN
	       (* check range [0 .. dest^.cstnr] *)
	       Check(unsigned, 0, dest^.cstnr);
	    (* ELSE dest = intcar or no range test possible *)
	    END; (* IF TestBaseType(dest) = intptr *)
	 END; (* IF mode = constantMod *)
      END; (* WITH fat DO *)
   END RangeCheck;

   (*
    *	check for top < limit
    *)

   PROCEDURE CheckStack;
      VAR ok: LabelPtr;
   BEGIN
      IF NOT sflag THEN
	 GetLabel('c', ok);
	 EmitComment("check for stack overflow");
	 Emit2(CMP, "%L%r,%r", top, limit);
	 Emit1(BLS, "%l", ok^);
	 EmitExtern(stack);
	 IncTop(oneword); DecTop(oneword);
	 Emit1(JSR, "%l", stack);
	 EmitLabel(ok);
	 DISPOSE(ok);
      END;
   END CheckStack;

   (*
    *	stringConstMod for character constants possible
    *)

   PROCEDURE CharCast(VAR at: Attribut);
      TYPE CharPtr = POINTER TO ARRAY[0..0] OF CHAR;
      VAR cp: CharPtr;
   BEGIN
      WITH at DO
         Assert(typtr^.ixp^.max = 0);
         cp := CharPtr(strgPtr^.valentry);
         mode := constantMod;
         value := ORD(cp^[0]);
         typtr := charptr;
      END;
   END CharCast;

END MCP4RangeChecks.
