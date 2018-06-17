(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
IMPLEMENTATION MODULE MCP4CallSys; (* AFB 9/83 *)

   FROM Storage IMPORT ALLOCATE, DEALLOCATE;
   FROM MCBase IMPORT Idptr, Stptr, noprio, Idclass, Structform, Varkind,
      Stpures, Stfuncs, boolptr, intptr, cardptr, wordptr, byteptr,
      realptr, addrptr, charptr, mainmodp, Symbol, oneword,
      doubleword, procmarkspace, BitsPerWord, longintptr, longcardptr,
      longrealptr, onebyte;
   FROM MCTypes IMPORT IsReal;
   FROM MCP4Block IMPORT level, blockNptr;
   FROM MCP4Stack IMPORT GetStackAt, IncTop, DecTop;
   FROM MCP4ConstArithmetic IMPORT ConstDiv, ConstMul;
   FROM MCP4Scanner IMPORT nptr, GetSymbol, sy, arithmeticRangeCheck;
   FROM MCP4Global IMPORT Error, CompilerError, Assert;
   FROM MCP4Types IMPORT IsArrayType, ByteSize, IsSetType, SizeType,
      SimpleType, Arithmetic, BaseType, intcarptr;
   FROM MCP4AttributSys IMPORT ArithmeticType, AtMode, Attribut, ModeSet;
   FROM MCP4Load IMPORT Load, LoadAddr, LoadDynHigh, LoadConstant, LoadReg,
      LoadAddrReg, LoadA, LoadAndExpand;
   FROM MCP4ExpressionSys IMPORT Expression;
   FROM MCP4Designator IMPORT Designator;
   FROM MCMnemonics IMPORT Mnemonic;
   FROM MCP4RangeChecks IMPORT RangeCheck;
   FROM MCP4Register IMPORT Reg, FloatReg, GetReg, GetFloatReg, FreeReg,
      FreeFloatReg, SaveRegs, RestoreRegs, RegRequest, GetAddrReg;
   FROM MCP4Labels IMPORT Label, GetLabel, LabelPtr, LabelType, LabelLength;
   FROM MCP4CodeSys IMPORT EmitComment, AppendComment, EmitLabel,
      EmitExtern, EmitDC, EmitDCFValue, EmitProcExtern, Emit1, Emit2, Emit3,
      Emit4;
   FROM Conversions IMPORT ConvertInteger;
   FROM Sys IMPORT fork;
   FROM MCP4Address IMPORT Cleanup;
   FROM MCP4Public IMPORT mc68881;
   FROM MCP4FPEmulator IMPORT FPAbs, FPTrunc, FPFloat, FPPush, FPGetReturnVal,
      FPStore;
   FROM MCP4RTS IMPORT argcLabel, argvLabel, procprefix, halt, signal,
      newproc, transfer;
   IMPORT Storage, MCP4Stack;
   
   PROCEDURE ProcFuncCall(VAR fat: Attribut);
      VAR Index: CARDINAL;
          done: BOOLEAN;
	  added: CARDINAL; (* summary for IncTop/DecTop *)

      MODULE StackReservations;

	 FROM Storage IMPORT ALLOCATE, DEALLOCATE;
	 FROM MCP4Stack IMPORT FreeStack;

	 EXPORT EnterRes, ReleaseAll;

	 TYPE
	    ResList = POINTER TO Res;
	    Res =
	       RECORD
		  offset, size: CARDINAL;
		  link: ResList;
	       END;
	 VAR
	    reslist: ResList;

	 PROCEDURE EnterRes(offset, size: CARDINAL);
	    VAR new: ResList;
	 BEGIN
	    NEW(new);
	    new^.offset := offset; new^.size := size;
	    new^.link := reslist;
	    reslist := new;
	 END EnterRes;

	 PROCEDURE ReleaseAll;
	    VAR old: ResList;
	 BEGIN
	    WHILE reslist <> NIL DO
	       WITH reslist^ DO
		  FreeStack(size, offset);
	       END;
	       old := reslist;
	       reslist := reslist^.link;
	       DISPOSE(old);
	    END;
	 END ReleaseAll;

      BEGIN
	 reslist := NIL;
      END StackReservations;

      PROCEDURE StandFunc(VAR fat: Attribut; fCalling: Stfuncs);
         VAR
            lat1, lat2 : Attribut;
	    label: LabelPtr;
	    fReg: FloatReg;
	    lTypP: Stptr;
	    ltyp: ArithmeticType;
	    lReg: Reg;

	 PROCEDURE UnixCall(VAR fat: Attribut);
	    TYPE
	       AtList = POINTER TO At;
	       At =
		  RECORD
		     at: Attribut;
		     link: AtList;
		  END;
	    VAR
	       SVCCall: CARDINAL; (* svc call no; see module Sys *)
	       atlist: AtList;    (* list of parameters *)
	       newat, oldat: AtList;
	       d0at, d1at: Attribut;
	       parsize: CARDINAL;
	 BEGIN
	    atlist := NIL;
	    AppendComment("UNIXCALL");
	    Expression(lat1);
	    Assert(lat1.mode = constantMod);
	    SVCCall := lat1.value;
	    GetSymbol; Designator(d0at);
	    GetSymbol; Designator(d1at);
	    WHILE sy = comma DO
	       GetSymbol;
	       NEW(newat);
	       WITH newat^ DO
		  Expression(at);
		  WITH at DO
		     IF mode = conditionMod THEN
			Load(at);
		     END;
		  END;
		  link := atlist;
	       END;
	       atlist := newat;
	    END;
	    parsize := 0;
	    WHILE atlist <> NIL DO
	       WITH atlist^ DO
		  Emit2(MOVE, "%A%a,%r@-", at, top);
		  WITH at DO
		     IF typtr^.size = onebyte THEN
			INC(parsize, oneword DIV 2);
		     ELSE
			INC(parsize, typtr^.size);
		     END;
		  END;
		  Cleanup(at);
	       END;
	       oldat := atlist;
	       atlist := atlist^.link;
	       DISPOSE(oldat);
	    END;
	    Emit2(SUB, "%L%c,%r", oneword, top);
	    Emit2(MOVE, "%L%c,%r@-", SVCCall, top);
	    (*SUN: svc call no on stack. Nixdorf: svc call no in d0*)
	    Emit1(TRAP, "%c", 0);
	    WITH fat DO
	       mode := loadedMod;
	       readonly := FALSE;
	       typtr := boolptr;
	       GetReg(loadReg);
	       IF loadReg = d1 THEN
		  GetReg(loadReg);
		  FreeReg(d1);
	       END;
	       Emit1(SCC, "%r", loadReg);
	       Emit1(NEG, "%B%r", loadReg);
	    END;
	    Emit2(MOVE, "%A%r,%a", d0, d0at);
	    Emit2(MOVE, "%A%r,%a", d1, d1at);
	    Cleanup(d0at); Cleanup(d1at);
	    Emit2(ADD, "%L%c,%r", parsize + oneword, top);
	    IncTop(parsize+oneword+procmarkspace);
	    DecTop(parsize+oneword+procmarkspace);
	    WITH fat DO
	       Emit1(TST, "%B%r", loadReg); (* set condition codes *)
	    END;
	 END UnixCall;

	 PROCEDURE UnixFork(VAR fat: Attribut);
	    VAR
	       father, endif: LabelPtr;
	       saveregs: BOOLEAN;
	       ign: BOOLEAN;
	 BEGIN
	    AppendComment("UNIXFORK");
	    WITH fat DO
	       readonly := FALSE;
	       typtr := boolptr;
	       mode := loadedMod;
	       GetReg(loadReg);
	       IF loadReg = d1 THEN
		  GetReg(loadReg);
		  FreeReg(d1);
	       END;
	    END;
	    RegRequest(d1, saveregs); saveregs := NOT saveregs;
	    IF saveregs THEN
	       SaveRegs;
	       RegRequest(d1, ign); (* ign=FALSE: d1 withreg *)
	       RegRequest(fat.loadReg, ign);
	    END;
            Designator(lat1);    (* d1 not used *)
	    Emit2(MOVE, "%L%c,%r@-", fork, top);
	    Emit1(TRAP, "%c", 0);
	    WITH fat DO
	       Emit1(SCC, "%r", loadReg);
	       Emit1(NEG, "%B%r", loadReg);
	    END;
	    Emit1(TST, "%B%r", d1);
	    GetLabel('I', father); GetLabel('I', endif);
	    Emit1(BEQ, "%l", father^);
	    AppendComment("son");
	    Emit1(CLR, "%A%a", lat1);
	    Emit1(BRA, "%l", endif^);
	    EmitLabel(father);
	    AppendComment("father");
	    Emit2(MOVE, "%A%r,%a", d0, lat1);
	    EmitLabel(endif);
	    Cleanup(lat1);
	    IF saveregs THEN
	       FreeReg(fat.loadReg);
	       RestoreRegs;
	    END;
	    Emit1(TST, "%B%r", fat.loadReg);
	 END UnixFork;

	 PROCEDURE UnixSignal(VAR fat: Attribut);
	    VAR
	       i: CARDINAL;
	       at: Attribut;
	 BEGIN
	    SaveRegs;
	    i := 0;
	    LOOP
	       INC(i);
	       CASE i OF
	       | 1, 2: (* sig: CARDINAL; p: PROC *)
		     Expression(at); Load(at);
		     Emit2(MOVE, "%A%#a,%r@-", at, top);
		     Cleanup(at);
	       | 3, 4: (* VAR old: PROC; VAR error: CARDINAL *)
		     Designator(at);
		     Emit1(PEA, "%a", at);
		     IF i = 4 THEN
			EXIT;
		     END;
	       END;
	       GetSymbol; (* comma *)
	    END;

	    Emit1(JSR, "%l", signal);
	    IncTop(4 * oneword + procmarkspace);
	    DecTop(4 * oneword + procmarkspace);
	    RestoreRegs;
	    WITH fat DO
	       typtr := boolptr;
	       readonly := FALSE;
	       mode := loadedMod;
	       GetReg(loadReg);
	       Emit2(MOVE, "%L%r,%r", d0, loadReg);
	    END;
	 END UnixSignal;

      BEGIN
         CASE fCalling OF
             | argcf: (* ARGC *)
                  WITH fat DO
                     typtr := intcarptr;
                     mode := loadedMod;
                     GetReg(loadReg);
		     EmitExtern(argcLabel);
		     Emit2(MOVE, "%L%l,%r", argcLabel, loadReg);
                  END;
             | uxf: (* UNIXCALL *)
		  UnixCall(fat);
             | uxff: (* PROCEDURE UNIXFORK(VAR pid: INTEGER) : BOOLEAN; *)
		  UnixFork(fat);
             | uxsf: (* PROCEDURE UNIXSIGNAL(sig: CARDINAL; p: PROC;
                           VAR old: PROC; VAR error: CARDINAL) : BOOLEAN *)
		  UnixSignal(fat);
             | higf: (* HIGH *)
                  Designator(fat);
                  WITH fat DO
                     Assert(IsArrayType(typtr));
                     WITH typtr^ DO
                        IF dyn THEN
                           LoadDynHigh(fat);
                           typtr := cardptr;
                        ELSE
                           Assert(ixp^.form = subranges);
                           mode := constantMod;
                           value := ixp^.max;
                           typtr := ixp^.scalp;
                        END;
                     END;
                  END;

            |  sizf: (* SIZE *)
                  Designator(fat);
                  WITH fat DO
                     IF IsArrayType(typtr) AND typtr^.dyn THEN
                        LoadDynHigh(fat);
			Emit2(ADD, "%L%c,%r", 1, fat.loadReg);
                        IF NOT ByteSize(typtr^.elp) THEN
                           ConstMul(unSigned, fat, typtr^.elp^.size);
                        END;
                     ELSE
                        mode := constantMod;
                        value := SizeType(fat);
                     END;
                     typtr := intcarptr;
                  END;

            |  adrf: (* ADR *)
                  Designator(fat);
                  LoadAddr(fat);
		  WITH fat DO
		     lReg := addrReg;
                     mode := loadedMod;
                     loadReg := lReg;
                     typtr := addrptr;
		  END;

            |  oddf: (* ODD *)
                  Expression(fat);
		  Load(fat);
		  AppendComment("ODD");
		  Emit2(ANDop, "%B%c,%r", 1, fat.loadReg);
                  fat.typtr := boolptr;

            |  chrf: (* CHR *)
                  Expression(fat);
                  Load(fat);
		  RangeCheck(charptr, fat, arithmeticRangeCheck);
                  fat.typtr := charptr;

            |  ordf: (* ORD *)
                  Expression(fat);
                  Load(fat);
		  WITH fat DO
		     IF ByteSize(typtr) THEN
			Emit2(ANDop, "%L%c,%r", 0FFH, loadReg);
		     ELSE
			RangeCheck(cardptr, fat, arithmeticRangeCheck);
		     END;
		     typtr := cardptr;
		  END;

            |  valf:
                  Assert((sy = namesy) AND (nptr^.klass = types));
                  lTypP := nptr^.idtyp;
		  GetSymbol;
                  GetSymbol; (*comma*)
                  Expression(fat);
                  Load(fat);
		  WITH fat DO
		     IF ByteSize(typtr) AND NOT ByteSize(lTypP) THEN
			Emit2(ANDop, "%L%c,%r", 0FFH, loadReg);
		     END;
		     RangeCheck(lTypP, fat, arithmeticRangeCheck);
                     typtr := lTypP;
                  END;

            |  absf:  (* ABS *)
                  Expression(fat);
                  ltyp := Arithmetic(fat, fat);
                  IF ltyp = signed  THEN
		     IF fat.mode = loadedMod THEN (* set condition codes *)
			Emit1(TST, "%L%r", fat.loadReg);
		     ELSE
			Load(fat);
		     END;
		     AppendComment("signed ABS");
		     GetLabel('I', label);
		     Emit1(BPL, "%l", label^);
		     Emit1(NEG, "%L%r", fat.loadReg);
		     EmitLabel(label);
		     DISPOSE(label);
                  ELSIF ltyp = floating THEN
		     AppendComment("floating ABS");
		     IF mc68881 THEN
			WITH fat DO
			   IF mode = floatLoadedMod THEN
			      Emit2(FABS, "%X%f,%f", floatLoadReg,floatLoadReg);
			      AppendComment("assembler bug");
			   ELSE
			      GetFloatReg(fReg);
			      Emit2(FABS, "%A%a,%f", fat, fReg);
			      Cleanup(fat);
			      readonly := FALSE;
			      mode := floatLoadedMod;
			      floatLoadReg := fReg;
			   END;
			END;
		     ELSE
			FPAbs(fat);
		     END;
                  ELSE
                     Assert(ltyp=unSigned);
                  END;

            |  capf:  (* CAP *)
                  Expression(fat);
		  AppendComment("CAP");
		  Load(fat);
		  Emit2(ANDop, "%B%c,%r", 137B, fat.loadReg);

            |  fltf:  (* FLOAT *)
                  Expression(fat);
		  AppendComment("FLOAT");
                  (* fmove converts a SIGNED 32-bit value to real *)
                  (* for this reason following check is needed   *)
                  RangeCheck(intptr, fat, arithmeticRangeCheck);
		  IF mc68881 THEN
		     GetFloatReg(fReg);
		     Emit2(FMOVE, "%L%a,%f", fat, fReg);
		     Cleanup(fat);
		     WITH fat DO
			mode := floatLoadedMod;
			readonly := FALSE;
			floatLoadReg := fReg;
			typtr := realptr;
		     END;
		  ELSE
		     FPFloat(fat);
		  END;

            |  trcf:  (* TRUNC *)
		  AppendComment("TRUNC");
                  Expression(fat);
		  Load(fat);

		  IF mc68881 THEN
		     GetReg(lReg);
		     Emit2(FINTRZ, "%X%f,%f", fat.floatLoadReg, fat.floatLoadReg);
		     AppendComment("assembler bug");
		     Emit2(FMOVE, "%L%f,%r", fat.floatLoadReg, lReg);
		     WITH fat DO
			FreeFloatReg(floatLoadReg);
			mode := loadedMod;
			loadReg := lReg;
			readonly := FALSE;
		     END;
		     (* NOT standard: *)
		     (* fxdr converts floating point numbers to signed *)
		     (* 32-bit values                                  *)
		     fat.typtr := intptr;
		  ELSE
		     FPTrunc(fat);
		  END;
         END;
      END StandFunc;

      PROCEDURE StandProc(fCalling: Stpures);

	 PROCEDURE Argv;
	    VAR
	       desat, indexat: Attribut; (* arguments of ARGV *)
	       highat: Attribut;         (* HIGH(desat) *)
	       loopLabel: LabelPtr;
	       xreg: Reg;                (* -> argv[index] *)
	 BEGIN
	    AppendComment("ARGV");
	    Designator(desat);
	    GetSymbol; Expression(indexat); Load(indexat);

	    (* determine HIGH(desat) *)
	    WITH desat.typtr^ DO
	       Assert(form = arrays);
	       IF dyn THEN
		  highat := desat;
		  highat.readonly := TRUE;
		  LoadDynHigh(highat);
	       ELSE
		  WITH highat DO
		     readonly := FALSE;
		     mode := constantMod;
		     typtr := cardptr;
		  END;
		  highat.value := size-1;
		  Load(highat);
	       END;
	    END;
	    LoadAddr(desat);
	    GetAddrReg(xreg);
	    (* Emit4(MOVE, "%L%r@(%l:l)@(0:l,%r:l:4),%r", GNU *)
	    Emit4(MOVE, "%L%r@(%l:l)@(0,%r:l:4),%r",
	       illegal, argvLabel, indexat.loadReg, xreg);
	    GetLabel('L', loopLabel);
	    (* loop until desat is filled or null byte is found *)
	    EmitLabel(loopLabel);
	    Emit2(MOVE, "%B%r@+,%r@+", xreg, desat.addrReg);
	    Emit2(DBEQ, "%r,%l", highat.loadReg, loopLabel^);
	    Cleanup(desat); Cleanup(highat); FreeReg(xreg); DISPOSE(loopLabel);
	    Cleanup(indexat);
	 END Argv;

	 PROCEDURE IncDec;
	    VAR
	       desat: Attribut;
	       valat: Attribut;
	 BEGIN
	    Designator(desat);
	    IF fCalling = incp THEN
	       AppendComment("INC");
	    ELSE
	       AppendComment("DEC");
	    END;
	    IF sy = comma THEN
	       GetSymbol; Expression(valat);
	    ELSE
	       WITH valat DO
		  mode := constantMod;
		  typtr := desat.typtr;
		  readonly := FALSE;
		  value := 1;
	       END;
	    END;
	    WITH valat DO
	       IF (mode <> loadedMod) AND (mode <> constantMod) THEN
		  Load(valat);
	       END;
	       IF fCalling = incp THEN
		  AppendComment("INC");
		  Emit2(ADD, "%A%a,%a", valat, desat);
	       ELSE
		  AppendComment("DEC");
		  Emit2(SUB, "%A%a,%a", valat, desat);
	       END;
	    END;
	    Cleanup(valat); Cleanup(desat);
	 END IncDec;

	 PROCEDURE InclExcl;
	    VAR
	       lat1, lat2: Attribut;
	       inst: Mnemonic;
	 BEGIN
	    Designator(lat1);
	    GetSymbol;
	    Expression(lat2);
	    IF fCalling = inlp THEN
	       AppendComment("INCL"); inst := BFSET;
	    ELSE (*exclp*)
	       AppendComment("EXCL"); inst := BFCLR;
	    END;
	    IF lat2.mode = constantMod THEN
	       RangeCheck(lat1.typtr^.basep, lat2, arithmeticRangeCheck);
	       WITH lat1.typtr^ DO
		  IF (form = bigsets) AND (offset > 0) THEN
		     DEC(lat2.value, offset);
		  END;
	       END;
	       IF lat2.value >= BitsPerWord THEN
		  LoadAndExpand(lat2);
	       END;
	    ELSE
	       LoadAndExpand(lat2);
	       RangeCheck(lat1.typtr^.basep, lat2, arithmeticRangeCheck);
	       WITH lat1.typtr^ DO
		  IF (form = bigsets) AND (offset > 0) THEN
		     Emit2(SUB, "%L%c,%r", offset, lat2.loadReg);
		  END;
	       END;
	    END;
	    IF lat2.mode = constantMod THEN
	       Emit3(inst, "%a{%c:%c}", lat1, lat2.value, 1);
	    ELSE
	       Emit3(inst, "%a{%r:%c}", lat1, lat2.loadReg, 1);
	    END;
	    Cleanup(lat1); Cleanup(lat2);
	 END InclExcl;


	 PROCEDURE NewProcess;
	    (* PROCEDURE NEWPROCESS(P: PROC; A: ADDRESS; n: CARDINAL; *)
	    (*                      VAR new: ADDRESS);                *)
	    VAR
	       param: CARDINAL;
	       at: Attribut;
	 BEGIN
	    SaveRegs;
	    FOR param := 1 TO 4 DO
	       CASE param OF
	       | 1, 2, 3:  Expression(at); GetSymbol;
			   Emit2(MOVE, "%L%#a,%r@-", at, top);
			   Cleanup(at);
	       | 4:        Designator(at);
			   Emit1(PEA, "%a", at);
			   Cleanup(at);
	       END;
	    END;
	    Emit1(JSR, "%l", newproc);
	    RestoreRegs;
	 END NewProcess;

	 PROCEDURE Transfer;
	    (* PROCEDURE TRANSFER(VAR src, dest: ADDRESS); *)
	    VAR
	       at: Attribut;
	 BEGIN
	    SaveRegs;
	    Designator(at); Emit1(PEA, "%a", at); Cleanup(at); GetSymbol;
	    Designator(at); Emit1(PEA, "%a", at); Cleanup(at);
	    Emit1(JSR, "%l", transfer);
	    RestoreRegs;
	 END Transfer;

      BEGIN
	 CASE fCalling OF
	 | argvp:      Argv;
	 | incp, decp: IncDec;
	 | halp:       EmitExtern(halt); Emit1(JSR, "%l", halt);
	 | inlp, exlp: InclExcl;
	 | nprp:       NewProcess;
	 | trsp:       Transfer;
         END;
      END StandProc;

      (*
       *	principles of operation for nonstandard procs/funcs :
       *
       *	The parameters are stored into the activation record
       *	of the procedure to be called.
       *	In case of call by value only the address has to be be stored
       *	for arrays and records; the called procedure will
       *	copy these parameters into its activation record.
       *	Big sets must be copied in the calling sequence
       *	due to the fact that they may be temporary on stack.
       *
       *	The stack top needs not to be restored; this
       *	is done by the procedure/function itself
       *	(see MCP4Block; TerminateBlock).
       *
       *	Don't change anything without inspecting MCP4Block !
       *)

      PROCEDURE LoadParam(fsp: Stptr); (* type of procedure/function *)
         VAR
            lat: Attribut; (* attribut of one of the parameters *)
            LNP: Idptr;    (* walks through the list of parameters *)
	    loopLabel: LabelPtr;

         PROCEDURE DynParam(VAR fat: Attribut; (* parameter *)
                            IsBlock: BOOLEAN; (* ARRAY OF WORD ? *)
			    IsByteBlock: BOOLEAN; (* ARRAY OF BYTE ? *)
                            vaddr: CARDINAL); (* parameter address *)
	    VAR
	       s: CARDINAL; (* byte or word size of fat *)
	       loadreg: Reg;
	       hat: Attribut;
	       floatloadreg: FloatReg;
	 BEGIN  
	    WITH fat DO  
	       WITH typtr^ DO
		  Assert((form = arrays) OR IsBlock OR IsByteBlock);  
		  IF (form = arrays) AND dyn THEN 
		     Emit1(PEA, "%a", fat);
		     INC(added, oneword);
		     LoadDynHigh(fat);
		     IF IsBlock THEN
			IF ByteSize(elp) THEN
			   (* for fat.loadReg mod 4 = 0 should be checked *)
			   Emit2(LSR, "%L%c,%r", 2, fat.loadReg);
			ELSE
			   (* high := (high+1)*wordsize - 1 *)
			   (* calculate wordsize *)
			   s := (elp^.size + oneword - 1) DIV oneword;
			   ConstMul(unSigned, fat, s);
			   IF s > 1 THEN
			      Emit2(ADD, "%L%c,%r", s-1, fat.loadReg);
			   END;
			END;
		     ELSIF IsByteBlock THEN
			IF NOT ByteSize(elp) THEN
			   (* high := (high+1)*elp^.size - 1 *)
			   s := elp^.size;
			   ConstMul(unSigned, fat, s);
			   IF s > 1 THEN
			      Emit2(ADD, "%L%c,%r", s-1, fat.loadReg);
			   END;
			END;
		     END;
		     Emit2(MOVE, "%L%r,%r@-", fat.loadReg, top);
		     INC(added, oneword);
		     FreeReg(fat.loadReg);
		  ELSE (* (form <> arrays) OR NOT dyn *)
		     IF IsBlock OR IsByteBlock THEN
			IF (mode = loadedMod) OR (mode = constantMod) OR
			      (mode = conditionMod) THEN
			   Load(fat); hat := fat;
			   loadreg := loadReg;
			   GetStackAt(fat); EnterRes(fat.offset, fat.size);
			   fat.readonly := TRUE;
			   Emit3(MOVE, "%A%a,%r@(%o)", hat, base, fat.offset);
			   FreeReg(loadreg);
			ELSIF (mode = floatLoadedMod) OR
			      (mode = doubleConstMod) THEN
			   Load(fat);
			   IF mc68881 THEN
			      floatloadreg := floatLoadReg;
			   ELSE
			      hat := fat;
			   END;
			   GetStackAt(fat); EnterRes(fat.offset, fat.size);
			   fat.readonly := TRUE;
			   IF mc68881 THEN
			      Emit2(FMOVE, "%A%f,%a", floatloadreg, fat);
			      FreeFloatReg(floatloadreg);
			   ELSE
			      FPStore(hat, fat);
			      Cleanup(hat);
			   END;
			END;
			IF mode IN ModeSet{globalMod, localMod, addrLoadedMod,
					   externalMod, indexMod,
                                           setConstMod, stackMod,
					   absolutMod, stringConstMod} THEN 
			   IF IsBlock AND (size MOD oneword <> 0) THEN
			      Error(209);
			   END;
			   Emit1(PEA, "%a", fat);
			   IF IsBlock THEN
			      Emit2(MOVE, "%L%c,%r@-",
				 (size+oneword-1) DIV oneword - 1, top);
                           ELSIF (form = arrays) & (elp^.size = onebyte) THEN
                              WITH ixp^ DO
                                 IF max >= min THEN
                                    Emit2(MOVE, "%L%c,%r@-", max-min, top);
                                 ELSE
                                    Error(201);
                                 END;
                              END;
			   ELSE
			      Emit2(MOVE, "%L%c,%r@-", size-1, top);
			   END;
			   INC(added, oneword);
			   Cleanup(fat);
			ELSE
			   IF IsBlock THEN
			      Error(209);
			   ELSE
			      Error(214);
			   END;
			END;
		     ELSE (* NOT IsBlock AND NOT IsByteBlock *)
			Emit1(PEA, "%a", fat);
			INC(added, oneword);
			WITH ixp^ DO  
			   IF max >= min THEN
			      Emit2(MOVE, "%L%c,%r@-", max-min, top);
			      INC(added, oneword);
			   ELSE
			      Error(201);
			   END;  
			END;
			Cleanup(fat);
		     END;
		  END;
	       END;
	    END;
	 END DynParam;  

      BEGIN (* LoadParam *)
         Assert((fsp <> NIL) AND (fsp^.form = proctypes));
         LNP := fsp^.fstparam;
	 IF LNP <> NIL THEN
	    AppendComment("parameters");
	 END;
         WHILE LNP <> NIL DO
            WITH LNP^ DO
               Expression(lat); (* parameter *)
               IF (vkind = copyparam) OR (vkind = varparam) THEN
                  IF (idtyp^.form = arrays) AND idtyp^.dyn THEN
		     DynParam(lat, idtyp^.elp = wordptr,
		                   idtyp^.elp = byteptr, vaddr);
                  ELSIF (lat.mode = loadedMod) OR
			(lat.mode = floatLoadedMod) THEN
                     Error(210); (* occurs through type converters *)
                  ELSE
		     Emit1(PEA, "%a", lat);
		     INC(added, oneword);
		     Cleanup(lat);
                  END;
               ELSIF lat.typtr^.form = bigsets THEN
                  (* copy bigset into activation record *)
		  LoadAddr(lat);
		  Emit2(MOVE, "%L%c,%r", lat.typtr^.size DIV oneword - 1, d0);
		  Emit2(ADD, "%L%c,%r", lat.typtr^.size, lat.addrReg);
		  GetLabel('L', loopLabel);
		  EmitLabel(loopLabel);
		  Emit2(MOVE, "%L%r@-,%r@-", lat.addrReg, top);
		  Emit2(DBF, "%r,%l", d0, loopLabel^);
		  INC(added, lat.typtr^.size DIV oneword);
		  DISPOSE(loopLabel);
		  Cleanup(lat);
               ELSE
		  RangeCheck(idtyp, lat, arithmeticRangeCheck);
		  IF lat.mode = conditionMod THEN
		     Load(lat);
		  END;
		  IF IsReal(lat.typtr) THEN
		     IF mc68881 THEN
			Load(lat);
			Emit2(FMOVE, "%D%a,%r@-", lat, top);
			INC(added, oneword);
		     ELSE
			FPPush(lat);
			INC(added, doubleword);
		     END;
		  ELSE
		     IF lat.mode = stringConstMod THEN
			Emit2(MOVE, "%L%#a,%r@-", lat, top);
			INC(added, oneword);
		     ELSIF lat.mode = procedureMod THEN
			Emit1(PEA, "%a", lat);
			INC(added, oneword);
		     ELSE
			IF ByteSize(lat.typtr) THEN
			   Load(lat);
			   lat.typtr := cardptr;
			END;
			Emit2(MOVE, "%A%a,%r@-", lat, top);
			INC(added, oneword);
		     END;
		  END;
		  Cleanup(lat);
               END;
               LNP := vlink;
               IF LNP <> NIL THEN
                  GetSymbol; (*comma*)
               END;
            END;
         END;
         GetSymbol; (*rparent*)
      END LoadParam;

      PROCEDURE LocalProcLabel(VAR procLabel: Label; procnum: CARDINAL);
	 VAR i: CARDINAL;
             field: ARRAY[0..LabelLength-3] OF CHAR;
      BEGIN
	 ConvertInteger(procnum, 1, field);
	 procLabel := procprefix;
         FOR i := 3 TO HIGH(procLabel) DO
            procLabel[i] := field[i-3];
         END;
      END LocalProcLabel;

      PROCEDURE CalcTop;
      BEGIN
	 IncTop(added); DecTop(added); added := 0;
      END CalcTop;

      VAR
         lpsptr: Stptr;
	 procLabel: Label;
	 areg: Reg;

   BEGIN (* ProcFuncCall *)
      added := 0;
      WITH fat DO
         IF mode = procedureMod THEN
            IF procPtr^.isstandard THEN
               IF procPtr^.klass = funcs THEN
                  StandFunc(fat, procPtr^.fname)
               ELSE (* klass=pures*)
                  StandProc(procPtr^.pname)
               END;
               GetSymbol; (*rparent*)
            ELSIF procPtr^.klass = mods THEN
	       SaveRegs;
	       IF procPtr^.plev > 1 THEN
		  (* static link *)
		  INC(added, oneword); CalcTop;
		  Emit2(MOVE, "%L%r,%r@-", base, top);
	       END;
	       LocalProcLabel(procLabel, procPtr^.procnum);
	       Emit1(JSR, "%l", procLabel);
	       INC(added, procmarkspace); CalcTop;
	       RestoreRegs;
               GetSymbol; (*rparent*)
            ELSE
               lpsptr := procPtr^.idtyp;
               WITH lpsptr^ DO
		  SaveRegs;
                  LoadParam(lpsptr);
                  (* Call *)
                  WITH procPtr^ DO
                     IF (plev = level+1) THEN
                        (* local procedure *)
			(* static link *)
			INC(added, oneword);
			Emit2(MOVE, "%L%r,%r@-", base, top);
			LocalProcLabel(procLabel, procnum);
			Emit1(JSR, "%l", procLabel);
                     ELSIF plev > 1 THEN
                        (* intermediate level procedure *)
			(* static link *)
			IF level = plev THEN
			   Emit3(MOVE, "%L%r@(%C),%r@-", base, procmarkspace,
			      top);
			ELSE
			   GetAddrReg(areg);
			   Emit3(MOVE, "%L%r@(%C),%r", base, procmarkspace,
			       areg);
			   FOR Index := level-plev+1 TO 3 BY -1 DO
			      Emit3(MOVE, "%L%r@(%C),%r", areg, procmarkspace,
				 areg);
			   END;
			   Emit3(MOVE, "%L%r@(%C),%r@-", areg, procmarkspace,
			      top);
			   FreeReg(areg);
			END;
			INC(added, oneword);
			LocalProcLabel(procLabel, procnum);
			Emit1(JSR, "%l", procLabel);
                     ELSIF (plev = 1) AND (globmodp = mainmodp) THEN
                        (* global procedure *)
			(* no static link *)
			LocalProcLabel(procLabel, procnum);
			Emit1(JSR, "%l", procLabel);
                     ELSE (* imported procedure *)
			EmitProcExtern(globmodp^.identifier, procnum);
			Emit2(JSR, "_%l_P%C", globmodp^.identifier, procnum);
			(* no static link *)
                     END
                  END;
		  INC(added, procmarkspace);
		  CalcTop;
		  (* the top is already correct: see MCP4Block *)
		  RestoreRegs;
                  IF rkind = funcs THEN
                     typtr := funcp;
		     readonly := FALSE;
                     IF typtr^.size <= oneword THEN
			(* result in d0 *)
			mode := loadedMod;
			GetReg(loadReg);
			Emit2(MOVE, "%A%r,%a", d0, fat);
                     ELSE
                        Assert(IsReal(typtr));
			IF mc68881 THEN
			   (* result in fr0 *)
			   mode := floatLoadedMod;
			   GetFloatReg(floatLoadReg);
			   Emit2(FMOVE, "%X%f,%f", fr0, floatLoadReg);
			ELSE
			   FPGetReturnVal(fat);
			END;
                     END;
                  END;
               END;
            END;
         ELSE (* procedure variable *)
	    EmitComment("procedure variable call");
            (* must be loaded in the outer level *)
            LoadA(fat);
	    SaveRegs;
               (* protect register *)
               RegRequest(fat.loadReg, done);
               LoadParam(typtr);
	       (* no static link *)
	       INC(added, procmarkspace);
	       Emit1(JSR, "%r@", fat.loadReg);
               (* free inner level register *)
               IF done THEN
                  FreeReg(fat.loadReg);
               END;
	       CalcTop;
	    RestoreRegs;
            (* free outer level register *)
            FreeReg(fat.loadReg);
            IF typtr^.rkind = funcs THEN
	       typtr := typtr^.funcp;
	       IF typtr^.size <= oneword THEN
		  (* result in r0 *)
		  mode := loadedMod;
		  readonly := FALSE;
		  GetReg(loadReg);
		  Emit2(MOVE, "%A%r,%a", d0, fat);
	       ELSE
		  Assert(IsReal(typtr));
		  IF mc68881 THEN
		     (* result in fr0 *)
		     mode := floatLoadedMod;
		     GetFloatReg(floatLoadReg);
		     Emit2(FMOVE, "%X%f,%f", fr0, floatLoadReg);
		  ELSE
		     FPGetReturnVal(fat);
		  END;
	       END;
            END;
         END;
      END;
      ReleaseAll; (* stack reservations *)
      CalcTop;
   END ProcFuncCall;

END MCP4CallSys.
