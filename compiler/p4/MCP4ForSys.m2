(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
IMPLEMENTATION MODULE MCP4ForSys; (* AFB 9/83 *)

   FROM Storage IMPORT DEALLOCATE;
   FROM MCBase IMPORT Symbol, oneword, maxint, Stptr, charptr, maxcard,
      Structform;
   FROM MCP4AttributSys IMPORT Attribut, ArithmeticType, AtMode, TestType;
   FROM MCP4Load IMPORT Load, LoadReg, LoadAddr, LoadConstant;
   FROM MCP4Scanner IMPORT GetSymbol, sy, val;
   FROM MCP4Labels IMPORT LabelPtr, GetLabel, Label;
   FROM MCP4CodeSys IMPORT AppendComment, EmitLabel, Emit1, Emit2, Emit3;
   FROM MCP4ExpressionSys IMPORT Expression;
   FROM MCP4Designator IMPORT Designator;
   FROM MCP4StatSys IMPORT StatSequ1;
   FROM MCP4Register IMPORT Reg, GetReg, FreeReg;
   FROM MCMnemonics IMPORT Mnemonic;
   FROM MCP4Global IMPORT Error, CompilerError;
   FROM MCP4Types IMPORT Arithmetic, TestBaseType, BaseType,
      intcarptr;
   FROM MCP4Stack IMPORT GetStack, FreeStack;
   FROM MCP4Address IMPORT Cleanup;
   FROM MCP4Test IMPORT Test;

   (*
    *	FOR index := start TO end BY step DO
    *	   .
    *      .
    *   END;
    *
    *	design of for-loop:
    *
    *	(strategy = withOverflow)
    *
    *		l	r0,start
    *		b	compare
    *	begin	equ *
    *		st	r0,index
    *		.
    *		.
    *		l	r0,index
    *		ais	r0,step
    *	compare	equ *
    *		c	r0,end
    *		b<=	begin
    *
    *	(strategy = noOverflow / ABS(step) = 1)
    *
    *		l	r0,start
    *		c	r0,end
    *		b>	endfor
    *		b	assign
    *	begin	equ	*
    *		ais	r0,step
    *	assign	equ	*
    *		st	r0,index
    *		.
    *		.
    *		l	r0,index
    *		c	r0,end
    *		b<	begin
    *
    *   (strategy = noOverflow / ABS(step) > 1)
    *
    *		l	r0,start
    *		c	r0,end
    *		b>	endfor
    *		b	assign
    *	begin	equ	*
    *		ais	r0,1
    *	assign	equ	*
    *		st	r0,index
    *		.
    *		.
    *		l	r0,index
    *		ai	r0,step-1
    *		c	r0,end
    *		b<	begin
    *)

   PROCEDURE ForStatement;
      VAR
         cntat: Attribut; (* attribut of counter/index *)
	 cntind0: BOOLEAN;
         stat: Attribut;  (* attribut of start value *)
         lat: Attribut;   (* attribut of limit and step value *)
	 latreg: Reg;
         step: INTEGER;
	 compLabel : LabelPtr;
	 beginforLabel : LabelPtr;
         endforLabel: LabelPtr;
         assignLabel: LabelPtr;

         (* if zero: no allocation on stack *)
	 limitOffset: CARDINAL; (* only if not constant *)
	 addrOffset: CARDINAL;

	 limitConst: CARDINAL;
	 atype: ArithmeticType; (* signed or unsigned compare ? *)
	 rstype: Stptr;
         atleastone: BOOLEAN;
         strategy: (withOverflow, noOverflow);
         min, max: CARDINAL;

      PROCEDURE Invert(t: TestType) : TestType;
      BEGIN
         (* needed if step < 0 *)
         CASE t OF
         | lt: RETURN gt;
         | le: RETURN ge;
         | eq: RETURN eq;
         | ne: RETURN ne;
         | ge: RETURN le;
         | gt: RETURN lt;
         END;
      END Invert;

      PROCEDURE Compare(test: TestType; atype: ArithmeticType; dest: LabelPtr);
      BEGIN
	 IF cntind0 THEN
	    Emit2(CMP, "%A%a,%r", lat, d0);
	 ELSE
	    Emit2(CMP, "%A%a,%a", lat, cntat);
	 END;
         IF step < 0 THEN test := Invert(test) END;
	 Test(test, atype, dest);
      END Compare;

      PROCEDURE LoadIndex;
      BEGIN
	 IF cntind0 THEN
	    Emit2(MOVE, "%A%a,%r", cntat, d0);
	 END;
      END LoadIndex;

      PROCEDURE Increment(step: INTEGER);
      BEGIN
         (* increment/decrement counter value *)
	 IF cntind0 THEN
	    IF step > 0 THEN
	       Emit2(ADD, "%L%i,%r", step, d0);
	    ELSE
	       Emit2(SUB, "%L%i,%r", -step, d0);
	    END;
	 ELSE
	    IF step > 0 THEN
	       Emit2(ADD, "%A%i,%a", step, cntat);
	    ELSE
	       Emit2(SUB, "%A%i,%a", -step, cntat);
	    END;
	 END;
      END Increment;

      PROCEDURE SGN(step: INTEGER) : INTEGER;
      BEGIN
         IF step > 0 THEN
            RETURN 1
         ELSE
            RETURN -1
         END;
      END SGN;

      PROCEDURE MinMax(type: Stptr; VAR cmin, cmax: CARDINAL);
      BEGIN
         WITH type^ DO
            CASE form OF
            | subranges: cmin := min; cmax := max;
            | enums:     cmin := 0; cmax := cstnr;
            | bools:     cmin := 0; cmax := 1;
            | cards,
	      longcards: cmin := 0; cmax := maxcard;
            | ints,
	      longints:  cmin := CARDINAL(-maxint-1); cmax := maxint;
            | chars:     cmin := 0; cmax := 377B;
            | opens:     MinMax(openstruc, cmin, cmax);
            ELSE
               CompilerError;
            END;
         END;
      END MinMax;

      PROCEDURE InRange() : BOOLEAN;
	 VAR
	    absstep: CARDINAL;
	    maxchar: CARDINAL;
      BEGIN
         IF (rstype = intcarptr) AND (step < 0) THEN
            RETURN TRUE
         END;
         IF limitOffset <> 0 THEN RETURN FALSE END;
	 IF rstype = charptr THEN
	    (* in case of rstype=charptr byte-arithmetic will be taken! *)
	    absstep := VAL(CARDINAL, ABS(step));
	    maxchar := ORD(MAX(CHAR));
	    RETURN (step > 0) AND (limitConst <= maxchar-absstep) OR
		   (step < 0) AND (limitConst >= absstep)
	 END;
	 (* long arithmetic *)
         IF atype = signed THEN
            RETURN (step > 0) AND (INTEGER(limitConst) <= maxint-step) OR
                   (step < 0) AND (INTEGER(limitConst) >= -maxint-1-step);
         ELSE
            RETURN (step > 0) AND (limitConst <= maxcard-CARDINAL(step)) OR
                   (step < 0) AND (limitConst >= CARDINAL(ABS(step)));
         END;
      END InRange;

   BEGIN
      AppendComment("for-statement");
      GetLabel('F', beginforLabel);
      GetLabel('F', compLabel);
      GetLabel('F', assignLabel);
      GetLabel('F', endforLabel);

      Designator(cntat); (* get attribut of counter *)
      WITH cntat DO
	 IF addrReg <> illegal THEN
	    (* avoid long-time use of registers *)
	    LoadAddr(cntat);
	    GetStack(1, addrOffset);
	 ELSE
	    addrOffset := 0;
	 END;
      END;

      GetSymbol; (* comma *)
      Expression(stat);
      atype := Arithmetic(cntat, stat);
      rstype := TestBaseType(cntat.typtr);

      GetSymbol; (* tosy *)
      Expression(lat);
      IF lat.mode = constantMod THEN
	 limitConst := lat.value;
	 limitOffset := 0;
	 cntind0 := FALSE;
      ELSE
	 Load(lat); lat.typtr := rstype; (* for correct size attributes *)
	 GetStack(1, limitOffset);
	 cntind0 := TRUE;
      END;

      IF addrOffset <> 0 THEN
	 AppendComment("index address");
	 (* cntat.mode = addrLoadedMod *)
	 Emit3(MOVE, "%L%r,%r@(%o)", cntat.addrReg, base, addrOffset);
	 IF NOT cntat.readonly THEN
	    FreeReg(cntat.addrReg);
	 END;
	 WITH cntat DO
	    readonly := FALSE;
	    mode := localMod;
	    addr := addrOffset;
	    addrReg := illegal;
	    memindex := TRUE;
	    scale := 1;
	    post := TRUE;
	    od := 0;
	 END;
      END;
      IF limitOffset <> 0 THEN
	 AppendComment("limit");
	 latreg := lat.loadReg;
	 Cleanup(lat);
	 WITH lat DO
	    readonly := FALSE;
	    mode := stackMod;
	    size := oneword;
	    indirect := FALSE;
	    offset := limitOffset;
	 END;
	 Emit2(MOVE, "%A%r,%a", latreg, lat);
      END;

      IF sy = bysy THEN
	 GetSymbol;
	 GetSymbol;
         step := val;
      ELSE
	 step := 1;
      END;
      IF step = 0 THEN
	 Error(202);
      END;

      (* strategy ?? *)
      IF InRange() THEN
         (* limitConst + step is in [min..max] *)
         strategy := withOverflow;
      ELSE
         strategy := noOverflow;
      END;
      MinMax(cntat.typtr, min, max);
      atleastone := (limitOffset = 0) AND
         ((step < 0) AND (limitConst = min) OR
          (step > 0) AND (limitConst = max));

      IF cntind0 THEN
	 Emit2(MOVE, "%A%#a,%r", stat, d0);
      ELSE
	 Emit2(MOVE, "%A%#a,%a", stat, cntat);
      END;
      Cleanup(stat);

      IF strategy = withOverflow THEN
         (* branch to loop condition *)
	 Emit1(BRA, "%l", compLabel^);
      ELSE
         IF NOT atleastone THEN
            Compare(gt, atype, endforLabel);
         END;
	 Emit1(BRA, "%l", assignLabel^);
      END;

      (* top of the loop *)
      EmitLabel(beginforLabel);

      IF strategy = noOverflow THEN
         Increment(SGN(step));
      END;

      EmitLabel(assignLabel);
      IF cntind0 THEN
	 (* store actual index value (in d0) back *)
	 Emit2(MOVE, "%A%r,%a", d0, cntat);
      END;

      (* body of the loop *)
      StatSequ1(endsy);

      LoadIndex;
      IF strategy = withOverflow THEN
         Increment(step);
      ELSIF (* strategy = noOverflow AND *) ABS(step) > 1 THEN
         Increment(step - SGN(step));
      END;

      (* condition part *)
      EmitLabel(compLabel);
      IF strategy = withOverflow THEN
         Compare(le, atype, beginforLabel);
      ELSE
         IF ABS(step) = 1 THEN
            Compare(ne, atype, beginforLabel);
         ELSE
            Compare(lt, atype, beginforLabel);
         END;
      END;

      EmitLabel(endforLabel);
      AppendComment("endfor");

      (* cleanup *)
      DISPOSE(beginforLabel);
      DISPOSE(compLabel);
      DISPOSE(assignLabel);
      DISPOSE(endforLabel);
      IF addrOffset <> 0 THEN
         FreeStack(1, addrOffset);
      END;
      IF limitOffset <> 0 THEN
	 FreeStack(1, limitOffset);
      END;
      IF cntat.mode <> stackMod THEN
	 Cleanup(cntat);
      END;
      GetSymbol;
   END ForStatement;

END MCP4ForSys.
