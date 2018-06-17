(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
IMPLEMENTATION MODULE MCP4ExpressionSys; (* AFB 9/83, 5/85, 12/87 *)

   FROM SYSTEM IMPORT ADDRESS;
   FROM Storage IMPORT ALLOCATE, DEALLOCATE;
   FROM MCBase IMPORT Idptr, Stptr, realptr, boolptr, charptr, intptr,
      cardptr, wordptr, addrptr, Symbol, Structform, bitsetptr, BitsPerWord,
      maxcard, Constval, SetValuePtr, oneword, Stset;
   FROM MCMnemonics IMPORT Mnemonic;
   FROM MCP4CodeSys IMPORT EmitDCFValue, EmitExtern, Emit1, Emit2, Emit3,
      EmitLabel, AppendComment;
   FROM MCP4ConstArithmetic IMPORT ConstMul, ConstDiv, ConstMod;
   FROM MCP4Block IMPORT level, EnterEQU, offset;
   FROM MCP4Scanner IMPORT GetSymbol, sy, val, nptr, cstPtr, cString,
      arithmeticRangeCheck;
   FROM MCP4Global IMPORT Error, CompilerError, Assert;
   FROM MCP4AttributSys IMPORT AtMode, Attribut, ArithmeticType, TestType;
   FROM MCP4Types IMPORT IsSetType, ResultType, Arithmetic, SimpleType,
      ByteSize, BaseType;
   FROM MCP4Load IMPORT Load, LoadReg, LoadAddr, LoadDynHigh, LoadBigSet,
      LoadCond, LoadAndExpand, LoadConstant;
   FROM MCP4CallSys IMPORT ProcFuncCall;
   FROM MCP4Register IMPORT Reg, FloatReg, GetReg, GetFloatReg, FreeReg,
      FreeFloatReg, GetAddrReg;
   FROM MCP4Labels IMPORT LabelPtr, LabelType, GetLabel, Label;
   FROM MCP4Designator IMPORT Designator;
   FROM MCP4Address IMPORT Cleanup;
   FROM MCP4Assign IMPORT MoveNBytes;
   FROM MCBigSet IMPORT InitConstSet, ConstSetElement, TermConstSet;
   FROM MCP4Stack IMPORT GetStackAt, FreeStackAt;
   FROM MCP4Public IMPORT mc68881;
   FROM MCP4FPEmulator IMPORT FPOp, FPNeg;
   FROM MCP4RangeChecks IMPORT RangeCheck;
   FROM MCP4Test IMPORT Invert, Test;
   IMPORT MCP4Block;

   PROCEDURE BigSetOp(op: Symbol; VAR at1, at2: Attribut);
      (* generate code for fat op lat and clean up lat *)
      (* precondition: lat.typtr^.size > oneword (if op <> insy) *)
      VAR
	 helpat: Attribut;
	 at1reg, at2reg: Reg;
	 cntreg: Reg;
	 dreg: Reg;
	 size: CARDINAL;
	 loop: LabelPtr;

      PROCEDURE InOp(VAR fat, lat: Attribut);
	 VAR
	    offset: CARDINAL;
      BEGIN
	 offset := lat.typtr^.offset;
	 LoadAndExpand(fat);
	 IF offset > 0 THEN
	    Emit2(SUB, "%L%c,%r", offset, fat.loadReg);
	 END;
	 IF lat.mode = constantMod THEN Load(lat) END;
	 Emit3(BFTST, "%a{%r:%c}", lat, fat.loadReg, 1);
	 Cleanup(fat); Cleanup(lat);
	 WITH fat DO
	    readonly := FALSE;
	    typtr := boolptr;
	    mode := conditionMod;
	    atype := signed;
	    test := ne;  (* BNZ *)
	    tlabel := NIL;
	    flabel := NIL;
	 END;
      END InOp;

   BEGIN
      IF op <> insy THEN
	 Assert(at2.typtr^.size > oneword); (* don't check fat (opsy = insy) *)
      END;
      dreg := illegal;
      CASE op OF
      | eql, neq:
	    (* no loading necessary *)
      | leq, geq:
	    (* no loading necessary *)
	    GetReg(dreg);
      | plus, times, slash: (* commutative operations on sets *)
	    IF (at2.mode = stackMod) AND (at1.mode <> stackMod) THEN
	       (* exchange both attributes *)
	       helpat := at1; at1 := at2; at2 := helpat;
	    ELSE
	       LoadBigSet(at1);
	    END;
	    GetReg(dreg);
      | minus:
	    LoadBigSet(at1);
	    GetReg(dreg);
      | insy:
	    InOp(at1, at2);
	    RETURN
      END;
      size := at1.typtr^.size;
      IF at1.mode <> stackMod THEN
	 LoadAddr(at1); at1reg := at1.addrReg;
      ELSE
	 GetAddrReg(at1reg);
	 Emit2(LEA, "%a,%r", at1, at1reg);
      END;
      IF at2.mode <> stackMod THEN
	 LoadAddr(at2); at2reg := at2.addrReg;
      ELSE
	 GetAddrReg(at2reg);
	 Emit2(LEA, "%a,%r", at2, at2reg);
      END;

      (* init of loop *)
      GetReg(cntreg);
      LoadConstant(cntreg, size DIV oneword - 1);
      GetLabel('S', loop);
      EmitLabel(loop);

      (* inner loop *)
      (* (op <> insy) *)
      CASE op OF
      | plus, minus, times, slash:
	    Emit2(MOVE, "%L%r@+,%r", at2reg, dreg);
	    CASE op OF
	    | plus:  Emit2(ORop, "%L%r,%r@+", dreg, at1reg);
	    | minus: Emit2(EOR, "%L%i,%r", -1, dreg);
		     Emit2(ANDop, "%L%r,%r@+", dreg, at1reg);
	    | times: Emit2(ANDop, "%L%r,%r@+", dreg, at1reg);
	    | slash: Emit2(EOR, "%L%r,%r@+", dreg, at1reg);
	    END;
	    Emit2(DBF, "%r,%l", cntreg, loop^);
      | geq:
	    Emit2(MOVE, "%L%r@+,%r", at1reg, dreg);
	    Emit2(EOR, "%L%i,%r", -1, dreg);
	    Emit2(ANDop, "%L%r@+,%r", at2reg, dreg);
	    Emit2(DBNE, "%r,%l", cntreg, loop^);
      | leq:
	    Emit2(MOVE, "%L%r@+,%r", at2reg, dreg);
	    Emit2(EOR, "%L%i,%r", -1, dreg);
	    Emit2(ANDop, "%L%r@+,%r", at1reg, dreg);
	    Emit2(DBNE, "%r,%l", cntreg, loop^);
      | eql, neq:
	    Emit2(CMPM, "%L%r@+,%r@+", at2reg, at1reg);
	    Emit2(DBNE, "%r,%l", cntreg, loop^);
      END;
      DISPOSE(loop);
      IF dreg <> illegal THEN FreeReg(dreg) END;
      IF at1.mode = stackMod THEN
	 FreeReg(at1reg);
      END;
      IF at2.mode = stackMod THEN
	 FreeReg(at2reg);
      END;
      FreeReg(cntreg);

      IF (op = geq) OR (op = leq) OR (op = eql) OR (op = neq) THEN
	 Cleanup(at1);
	 WITH at1 DO
	    readonly := FALSE;
	    typtr := boolptr;
	    mode := conditionMod;
	    tlabel := NIL;
	    flabel := NIL;
	    atype := signed;
	    IF op = neq THEN
	       test := ne;
	    ELSE
	       test := eq;
	    END;
	 END;
      END;
      Cleanup(at2);
   END BigSetOp;

   PROCEDURE Factor(VAR fat: Attribut);   
      VAR rptr: POINTER TO (* REAL *)
                        RECORD r1, r2: CARDINAL END;
      VAR slabel: LabelPtr;

      PROCEDURE SetConstructor(VAR fat: Attribut);
         VAR at1, at2: Attribut;
             spat: BITSET;
             basetype: Stptr;
	     emptylab: LabelPtr;
      BEGIN
         WITH fat.typtr^ DO
            Assert(form = sets);
            basetype := basep;
         END;
         WITH fat DO
	    readonly := FALSE;
            mode := loadedMod;
            GetReg(loadReg);
	    Emit2(MOVE, "%L%c,%r", 0, loadReg);
         END;
         spat := { };
         GetSymbol; (* lconbr *)
         WHILE sy <> rconbr DO
            Expression(at1);
            RangeCheck(basetype, at1, arithmeticRangeCheck);
            IF at1.mode <> constantMod THEN
               LoadAndExpand(at1);
            END;
            IF sy = range THEN
               GetSymbol; (* range *)
               Expression(at2);
               RangeCheck(basetype, at2, arithmeticRangeCheck);
               IF (at1.mode = constantMod) AND (at2.mode = constantMod) THEN
                  spat := spat + {at1.value..at2.value};
               ELSE
		  GetLabel('S', emptylab);
                  IF at1.mode = constantMod THEN
		     LoadAndExpand(at2);
		     Emit2(SUB, "%L%c,%r", at1.value - 1, at2.loadReg);
		     Emit1(BLE, "%l", emptylab^);
		     Emit3(BFSET, "%r{%c:%r}", fat.loadReg, at1.value,
			at2.loadReg);
                  ELSE
                     (* at1.mode = loadedMod *)
		     IF at2.mode = constantMod THEN
			INC(at2.value);
			LoadAndExpand(at2);
			Emit2(SUB, "%L%r,%r", at1.loadReg, at2.loadReg);
			Emit1(BLE, "%l", emptylab^);
		     ELSE
			LoadAndExpand(at2);
			Emit2(SUB, "%L%r,%r", at1.loadReg, at2.loadReg);
			Emit1(BLT, "%l", emptylab^);
			Emit2(ADD, "%L%c,%r", 1, at2.loadReg);
		     END;
		     Emit3(BFSET, "%r{%r:%r}", fat.loadReg, at1.loadReg,
			at2.loadReg);
                  END;
		  EmitLabel(emptylab);
		  DISPOSE(emptylab);
		  Cleanup(at1); Cleanup(at2);
               END;
            ELSE
               IF at1.mode = constantMod THEN
                  INCL(spat, at1.value);
               ELSE
		  Emit3(BFSET, "%r{%r:%c}", fat.loadReg, at1.loadReg, 1);
		  Cleanup(at1);
               END;
            END;
         END; (* WHILE *)
         IF spat <> {} THEN
	    Emit2(ORop, "%L%i,%r", spat, fat.loadReg);
         END;
         GetSymbol; (* rconbr *)
      END SetConstructor;

      PROCEDURE BigSetConstructor(VAR fat: Attribut);
         VAR at1, at2: Attribut;
	     setconstat: Attribut;
             spat: Constval;
	     styp: Stptr;
	     loop, do, empty: LabelPtr;
             setconstlabel: LabelPtr;
	     cfat: Attribut;
      BEGIN
         GetLabel('S', setconstlabel);
	 InitConstSet(spat, fat.typtr);
         spat.setvalue^.label := setconstlabel;
	 styp := fat.typtr^.basep;
	 GetStackAt(fat);

         (* copy setconstant at setconstlabel to set on stack *)
	 cfat := fat;
	 cfat.readonly := TRUE; (* avoid stack release of fat *)
	 LoadAddr(cfat); cfat.readonly := FALSE;
	 WITH setconstat DO
	    readonly := FALSE;
	    mode := addrLoadedMod;
	    typtr := fat.typtr;
	    GetAddrReg(addrReg);
	    Emit2(LEA, "%l,%r", setconstlabel^, addrReg);
	 END;
	 MoveNBytes(cfat, setconstat, fat.typtr^.size);
	 Cleanup(setconstat); Cleanup(cfat);

         GetSymbol; (* lconbr *)
         WHILE sy <> rconbr DO
            Expression(at1);
	    RangeCheck(styp, at1, arithmeticRangeCheck);
            IF sy = range THEN
               GetSymbol; (* range *)
               Expression(at2);
	       RangeCheck(styp, at2, arithmeticRangeCheck);
               IF (at1.mode = constantMod) AND (at2.mode = constantMod) THEN
		  ConstSetElement(spat, at1.value, at2.value);
               ELSE
		  LoadAndExpand(at1); LoadAndExpand(at2);
		  GetLabel('S', loop); GetLabel('S', do); GetLabel('S', empty);
		  Emit2(SUB, "%L%r,%r", at1.loadReg, at2.loadReg);
		  Emit1(BLT, "%l", empty^);
		  Emit2(ADD, "%L%c,%r", 1, at2.loadReg);
		  IF fat.typtr^.offset > 0 THEN
		     Emit2(SUB, "%L%c,%r", fat.typtr^.offset, at1.loadReg);
		  END;
		  Emit1(BRA, "%l", do^);
		  EmitLabel(loop);
		  Emit3(BFSET, "%a{%r:%c}", fat, at1.loadReg, BitsPerWord);
		  Emit2(ADD, "%L%c,%r", BitsPerWord, at1.loadReg);
		  Emit2(SUB, "%L%c,%r", BitsPerWord, at2.loadReg);
		  EmitLabel(do);
		  Emit2(CMP, "%L%c,%r", BitsPerWord, at2.loadReg);
		  Emit1(BGT, "%l", loop^);
		  Emit3(BFSET, "%a{%r:%r}", fat, at1.loadReg, at2.loadReg);
		  EmitLabel(empty);
		  DISPOSE(loop); DISPOSE(do); DISPOSE(empty);
                  Cleanup(at1); Cleanup(at2);
               END;
            ELSE
               IF at1.mode = constantMod THEN
		  ConstSetElement(spat, at1.value, at1.value);
               ELSE
		  LoadAndExpand(at1);
		  IF fat.typtr^.offset > 0 THEN
		     Emit2(SUB, "%L%c,%r", fat.typtr^.offset, at1.loadReg);
		  END;
		  Emit3(BFSET, "%a{%r:%c}", fat, at1.loadReg, 1);
                  Cleanup(at1);
               END;
            END;
         END; (* WHILE *)
         GetSymbol; (* rconbr *)
	 TermConstSet(spat);
      END BigSetConstructor;

   BEGIN (* Factor *)  
      IF sy = lparent THEN
         GetSymbol;
	 Expression(fat);
	 GetSymbol; (* rparent *)  
      ELSIF sy = notsy THEN
	 (* fat.typtr = boolptr *)
	 GetSymbol;
	 Factor(fat);         
	 IF fat.mode <> constantMod THEN
	    LoadCond(fat);
	    WITH fat DO
	       test := Invert(test);
	       slabel := tlabel; tlabel := flabel; flabel := slabel;
	    END;
	 ELSE
	    fat.value := 1-fat.value (* NOT fat.value *)
	 END;
      ELSIF (sy = namesy) OR (sy = field) THEN
         Designator(fat);  
         IF sy = lparent THEN (* function call *)  
            GetSymbol;
	    ProcFuncCall(fat);
         ELSIF sy = lconbr THEN
            IF (fat.typtr <> NIL) AND (fat.typtr^.form = bigsets) THEN
               BigSetConstructor(fat);
            ELSE
               SetConstructor(fat);
            END;
         END
      ELSIF sy = lconbr THEN
         fat.typtr := bitsetptr;
         SetConstructor(fat);
      ELSE (* Constant *)   
         Assert(sy = anycon);  
         WITH fat DO 
            typtr := cstPtr;  
	    readonly := FALSE;
	    IF (typtr <> NIL) AND (typtr^.form = bigsets) THEN
	       mode := setConstMod;
	       setPtr := SetValuePtr(cString);
            ELSIF NOT SimpleType(fat) THEN                         
               mode := stringConstMod;
	       strgPtr := cString; (* see MCP4Scanner *)
            ELSIF BaseType(typtr) = realptr THEN 
               mode := doubleConstMod; 
               rptr := ADDRESS(cString);
               Real1 := rptr^.r1;  Real2 := rptr^.r2;
               Load(fat);
            ELSE
	       mode := constantMod;
               value := CARDINAL(val);
            END;
         END;
         GetSymbol;
      END;
   END Factor;   

   PROCEDURE Adapt(VAR l1, l2: LabelPtr);
   BEGIN
      IF l2 <> NIL THEN
	 IF l1 = NIL THEN
	    l1 := l2;
	 ELSE
	    EnterEQU(l2, l1);
	    DISPOSE(l2);
	 END;
      END;
   END Adapt;

   PROCEDURE Term(VAR fat: Attribut);   
      VAR 
         lat: Attribut;
	 opsy: Symbol;  
         AType: ArithmeticType;

      PROCEDURE Multiplication(VAR fat, lat: Attribut);
         VAR cat: Attribut;
	     opt: BOOLEAN;                     
             tmpat: Attribut; (* used for exchange of fat and lat *)
      BEGIN
	 opt := FALSE;
         GetSymbol;
         Factor(lat);
         IF (fat.mode <> loadedMod) AND (lat.mode = loadedMod) THEN
            tmpat := fat; fat := lat; lat := tmpat;
         END;
         CASE Arithmetic(fat, lat) OF
            bitwise: 
		IF (fat.typtr^.form = bigsets) AND
		   (fat.typtr^.size > oneword) THEN
		   BigSetOp(times, fat, lat);
		ELSE
		   Load(fat);
		   Emit2(ANDop, "%A%a,%a", lat, fat);
		   Cleanup(lat);
		END;

         |  floating:
		IF mc68881 THEN
		   Load(fat);
		   Emit2(FMUL, "%A%a,%a", lat, fat);
		   Cleanup(lat);
		ELSE
		   FPOp(times, fat, lat);
		END;

         |  signed, unSigned:
                IF fat.mode = constantMod THEN   
                   Load(lat);
                   ConstMul(Arithmetic(fat, lat), lat, fat.value);
		   fat.mode := loadedMod;
                   IF lat.mode <> loadedMod THEN
                      Load(lat);
                   END;
		   fat.loadReg := lat.loadReg;
                ELSIF lat.mode = constantMod THEN
                   Load(fat);
                   ConstMul(Arithmetic(fat, lat), fat, lat.value);
                ELSE
                   Mult(fat, lat, Arithmetic(fat, lat));
                END;
         END;
      END Multiplication;

   BEGIN (* Term *)  
      Factor(fat);  
      WHILE (sy >= andsy) AND (sy <= modsy) DO
	 opsy := sy;
         IF sy = times THEN
            Multiplication(fat, lat);
         ELSE
            (* special case: short circuit AND *)
            IF opsy = andsy THEN
	       LoadCond(fat);
	       WITH fat DO
		  IF flabel = NIL THEN
		     GetLabel('I', flabel);
		  END;
		  test := Invert(test); (* now branch on false condition *)
		  Test(test, atype, flabel);
		  IF tlabel <> NIL THEN
		     EmitLabel(tlabel);
		     DISPOSE(tlabel);
		  END;
	       END;
            END;
            GetSymbol;  
            Factor(lat); 
            AType := Arithmetic(fat, lat);
            CASE opsy OF
               andsy:
                  Assert(AType = logical);
		  LoadCond(lat);
		  fat.test := lat.test;
		  Adapt(fat.tlabel, lat.tlabel);
		  Adapt(fat.flabel, lat.flabel);
                  fat.atype := lat.atype;

            |  divsy, modsy:
		  IF lat.mode = constantMod THEN
		     IF opsy = divsy THEN
			ConstDiv(AType, fat, lat.value);
		     ELSE
			ConstMod(AType, fat, lat.value);
		     END;
		  ELSE 
                     DivMod(fat, lat, opsy, AType);
		  END;
          
            |  slash:
                  CASE AType OF
                        bitwise:
			   IF (fat.typtr^.form = bigsets) AND
			      (fat.typtr^.size > oneword) THEN
			      BigSetOp(slash, fat, lat);
			   ELSE
			      IF fat.mode <> constantMod THEN
				 Load(fat);
			      END;
			      Load(lat);
			      Emit2(EOR, "%A%a,%a", lat, fat);
			      Cleanup(lat);
			   END;
                     |  floating:
			   IF mc68881 THEN
			      Load(fat);
			      Emit2(FDIV, "%A%a,%a", lat, fat);
			      Cleanup(lat);
			   ELSE
			      FPOp(slash, fat, lat);
			   END;
                  END;
            END;
         END;
         fat.typtr := ResultType(fat, lat);
      END;  
   END Term;  

   PROCEDURE SimpleExpression(VAR fat: Attribut);   
      (* fat describes the first operand and the result *)
      VAR 
         opsy: Symbol;  
         lat: Attribut; (* descriptor of second operand *)
         negb: BOOLEAN; (* a negation has to take place *)  
         AType: ArithmeticType;
         tmpat: Attribut; (* used for exchange of fat and lat *)
   BEGIN
      negb := sy = minus; 
      IF negb THEN
	 GetSymbol
      END;  
      Term(fat);  
      IF negb THEN  
         IF Arithmetic(fat, fat) = floating THEN
	    IF mc68881 THEN
	       Load(fat);
	       Emit2(FNEG, "%X%f,%f", fat.floatLoadReg, fat.floatLoadReg);
	       AppendComment( "assembler bug");
	    ELSE
	       FPNeg(fat);
	    END;
         ELSIF fat.mode <> constantMod THEN
            (* negation *)
	    Load(fat);
	    Emit1(NEG, "%A%a", fat);
         ELSE 
            fat.iValue := -fat.iValue;
         END;
      END;  
      WHILE (sy >= plus) AND (sy <= orsy) DO
	 opsy := sy;
         (* special case: short circuit OR *)
         IF opsy = orsy THEN
	    LoadCond(fat);
	    WITH fat DO
	       IF tlabel = NIL THEN
		  GetLabel('I', tlabel);
	       END;
	       Test(test, atype, tlabel);
	       IF flabel <> NIL THEN
		  EmitLabel(flabel);
		  DISPOSE(flabel);
	       END;
	    END;
         END;
         GetSymbol;
         Term(lat);
         AType := Arithmetic(fat, lat);
         CASE opsy OF   
            minus:
               CASE AType OF
                     signed, unSigned:
			Load(fat);
			Emit2(SUB, "%A%#a,%a", lat, fat);
			Cleanup(lat);
                  |  bitwise:
			IF (fat.typtr^.form = bigsets) AND
			   (fat.typtr^.size > oneword) THEN
			   BigSetOp(minus, fat, lat);
			ELSE
			   Load(lat);
			   Emit2(EOR, "%A%i,%a", -1, lat);
			   Emit2(ANDop, "%A%a,%a", fat, lat);
			   Cleanup(fat); fat := lat;
			END;
                  |  floating:
			IF mc68881 THEN
			   Load(fat);
			   Emit2(FSUB, "%A%a,%a", lat, fat);
			   Cleanup(lat);
			ELSE
			   FPOp(minus, fat, lat);
			END;
               END
         |  plus:
               IF (fat.mode <> loadedMod) AND
                  (lat.mode = loadedMod) THEN
                  tmpat := fat; fat := lat; lat := tmpat;
               END;
               CASE AType OF
                     signed, unSigned:
			Load(fat);
			Emit2(ADD, "%A%#a,%a", lat, fat);
			Cleanup(lat);
                  |  bitwise:
			IF (fat.typtr^.form = bigsets) AND
			   (fat.typtr^.size > oneword) THEN
			   BigSetOp(plus, fat, lat);
			ELSE
			   Load(fat);
			   Emit2(ORop, "%A%a,%a", lat, fat);
			   Cleanup(lat);
			END;
                  |  floating:
			IF mc68881 THEN
			   Load(fat);
			   Emit2(FADD, "%A%a,%a", lat, fat);
			   Cleanup(lat);
			ELSE
			   FPOp(plus, fat, lat);
			END;
               END;
         |  orsy:        
	       LoadCond(lat);
	       fat.test := lat.test;
	       Adapt(fat.tlabel, lat.tlabel);
	       Adapt(fat.flabel, lat.flabel);
               fat.atype := lat.atype;
         END;  
         fat.typtr := ResultType(fat, lat);
      END;  
   END SimpleExpression;   

   PROCEDURE Expression(VAR fat: Attribut);          
      (* fat describes the first operand and the result *)
      VAR 
         opsy : Symbol;
         lat: Attribut; (* descriptor of second operand *)  
	 hat: Attribut;
	 AType: ArithmeticType;
	 testcond: TestType;

   BEGIN (* Expression *)
      SimpleExpression(fat);
      IF (sy >= eql) AND (sy <= insy) THEN (* relational operator ? *)
         opsy := sy;
         IF fat.mode = conditionMod THEN
            Load(fat); (* necessary for (bool_expr) op (bool_expr) *)
         END;
         GetSymbol;
         SimpleExpression(lat);
         IF (lat.typtr^.form = bigsets) AND
	    ((lat.typtr^.size > oneword) OR (opsy = insy)) THEN
	    BigSetOp(opsy, fat, lat);
         ELSIF opsy = insy THEN
            LoadAndExpand(fat);
	    IF lat.mode = constantMod THEN Load(lat) END;
	    Emit3(BFTST, "%a{%r:%c}", lat, fat.loadReg, 1);
	    Cleanup(fat); Cleanup(lat);
            WITH fat DO
               mode := conditionMod;
	       test := ne; (* not zero *)
	       tlabel := NIL;
	       flabel := NIL;
	       atype := signed;
               typtr := boolptr;
            END;
	 ELSIF NOT mc68881 AND (fat.typtr^.form = reals) THEN
	    FPOp(opsy, fat, lat);
         ELSE (* (opsy >= eql) AND (opsy <= leq) *)
            IF (fat.typtr^.form IN Stset{sets, bigsets}) AND
	       ((opsy = geq) OR (opsy = leq)) THEN
               (* set operation >=,<= *)
	       IF opsy = geq THEN
	          Load(fat);
		  Emit2(EOR, "%L%i,%r", -1, fat.loadReg); (* complement *)
		  Emit2(ANDop, "%A%a,%r", lat, fat.loadReg);
               ELSE
                  Load(lat);
		  Emit2(EOR, "%L%i,%r", -1, lat.loadReg); (* complement *)
		  Emit2(ANDop, "%A%a,%r", fat, lat.loadReg);
	       END;
	       testcond := eq; (* atype becomes *) AType := signed;
	       Cleanup(fat); Cleanup(lat);
            ELSE
	       IF fat.mode = conditionMod THEN
		  Load(fat);
	       END;
	       IF lat.mode = conditionMod THEN
		  Load(lat);
	       END;
	       AType := Arithmetic(fat, lat);
	       CASE opsy OF  
	       | neq: testcond := ne;
	       | eql: testcond := eq;
	       | leq: testcond := le;
	       | lss: testcond := lt;
	       | geq: testcond := ge;
	       | grt: testcond := gt;
	       END;
	       IF (lat.mode = loadedMod) AND (fat.mode <> loadedMod) OR
		  (fat.mode = constantMod) THEN
		  hat := lat; lat := fat; fat := hat;
		  CASE testcond OF
		  | le: testcond := ge;
		  | lt: testcond := gt;
		  | ge: testcond := le;
		  | gt: testcond := lt;
		  ELSE
		  END;
	       ELSIF (fat.mode <> loadedMod) AND (lat.mode <> constantMod) THEN
		  Load(fat);
	       END;
	       CASE AType OF
	       |  signed, unSigned, logical, bitwise:
		     Emit2(CMP, "%A%#a,%a", lat, fat);
		     Cleanup(fat); Cleanup(lat);
	       |  floating:
		     Emit2(FCMP, "%A%a,%a", lat, fat);
		     Cleanup(fat); Cleanup(lat);
	       END;
            END;
            WITH fat DO
               typtr := boolptr;
	       mode := conditionMod;
	       tlabel := NIL;
	       flabel := NIL;
	       test := testcond;
	       IF (AType = signed) OR (AType = floating) THEN
		  atype := AType;
	       ELSE
		  atype := unSigned;
	       END;
            END;
         END;
      END;
   END Expression;  

   (*
    *	restrictions for following procedures
    *
    *	they must not call any procedures from MCP4ConstArithmetic
    *	if the loadReg of fat is freed they must not reallocate this register
    *)

   (* only signed/unsigned *)

   PROCEDURE Mult(VAR fat, lat: Attribut; AType: ArithmeticType);
      VAR tmpat: Attribut;
   BEGIN
      IF (lat.mode = loadedMod) AND
         (fat.mode <> loadedMod) THEN
         tmpat := lat; lat := fat; fat := tmpat;
      END;
      Load(fat);
      IF AType = signed THEN
	 Emit2(MULS, "%A%a,%a", lat, fat);
      ELSE
	 Emit2(MULU, "%A%a,%a", lat, fat);
      END;
      Cleanup(lat);
   END Mult;

   PROCEDURE Div(VAR fat, lat: Attribut; AType: ArithmeticType);
   BEGIN
      DivMod(fat, lat, divsy, AType);
   END Div;

   PROCEDURE Mod(VAR fat, lat: Attribut; AType: ArithmeticType);
   BEGIN
      DivMod(fat, lat, modsy, AType);
   END Mod;

   PROCEDURE DivMod(VAR fat, lat: Attribut; opsy: Symbol (* divsy or modsy *);
                    AType: ArithmeticType);
      VAR destreg: Reg;
   BEGIN
      Load(fat);
      IF opsy = divsy THEN
	 IF AType = signed THEN
	    Emit2(DIVS, "%L%#a,%a", lat, fat);
	 ELSE
	    Emit2(DIVU, "%L%#a,%a", lat, fat);
	 END;
      ELSE
	 GetReg(destreg);
	 IF AType = signed THEN
	    Emit3(DIVSL, "%L%#a,%r:%r", lat, destreg, fat.loadReg);
	 ELSE
	    Emit3(DIVUL, "%L%#a,%r:%r", lat, destreg, fat.loadReg);
	 END;
	 FreeReg(fat.loadReg);
	 fat.loadReg := destreg;
      END;
      Cleanup(lat);
   END DivMod;

END MCP4ExpressionSys.  
