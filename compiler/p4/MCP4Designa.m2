(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
IMPLEMENTATION MODULE MCP4Designator; (* AFB 9/83 *)
                                      (* REV AFB 5/84: see MCPass2.m2 *)

   FROM Storage IMPORT ALLOCATE, DEALLOCATE;
   FROM MCBase IMPORT Idptr, Stptr, Kindvar, intptr, Symbol, Structform,
      Idclass, addrptr, wordptr, oneword, doubleword, boolptr,
      procmarkspace, realptr, BitsPerWord;
   FROM MCP4Scanner IMPORT GetSymbol, val, nptr, sy, controlRangeCheck;
   FROM MCP4AttributSys IMPORT AtMode, Attribut, ArithmeticType, ModeSet;
   FROM MCMnemonics IMPORT Mnemonic;
   FROM MCP4Block IMPORT level, offset, offsetstack;
   FROM MCP4CodeSys IMPORT EmitComment, EmitLabel, EmitExtern, EmitDCFValue,
      Emit1, Emit2, Emit3;
   FROM MCP4ConstArithmetic IMPORT ConstMul, ConstMulReg;
   FROM MCP4ExpressionSys IMPORT Expression;
   FROM MCP4Global IMPORT Assert, Error, CompilerError;
   FROM MCP4Load IMPORT Load, LoadReg, LoadAddr, LoadConstant, LoadDynHigh,
      LoadA, LoadAndExpand;
   FROM MCP4RangeChecks IMPORT RangeCheckForConstant, RangeCheck;
   FROM MCP4Register IMPORT Reg, GetReg, FreeReg, GetAddrReg, RegSet, a7;
   FROM MCP4Types IMPORT TestBaseType, IsArrayType, ByteSize;
   FROM MCTypes IMPORT IsReal;
   FROM MCP4WithSys IMPORT UseWith;
   FROM MCP4Labels IMPORT LabelPtr, GetLabel, LabelType;
   FROM MCP4RTS IMPORT chkdyn;
   FROM MCP4Address IMPORT Cleanup;

   PROCEDURE Designator(VAR fat : Attribut);   
      VAR lReg: Reg;

      PROCEDURE TypFunction(ftp: Stptr; VAR fat : Attribut);   
         (* ftp is type of function *)
         VAR oldmode: AtMode;
      BEGIN
	 GetSymbol; 
         Expression(fat);  
         IF fat.mode = stringConstMod THEN
	    LoadAddr(fat);
	 END;
         WITH fat DO
            IF (typtr^.size <> ftp^.size) 
               OR ((typtr^.form = arrays) AND typtr^.dyn) THEN Error(302);
            ELSIF (ftp^.size > doubleword) OR
	       (ftp^.size = doubleword) AND NOT IsReal(typtr) THEN
	       LoadAddr(fat);
	    ELSE
               oldmode := fat.mode;
	       Load(fat);
            END;
            typtr := ftp;
         END;
      END TypFunction;  

      PROCEDURE PreDesignator(VAR fat: Attribut);
         (* The code for loading a procedure base address will already be generated 
            if necessary, but neither the value nor the address is completely
            loaded *)
         VAR i, j: CARDINAL;
             lType: Stptr;
	     lReg: Reg;
             helpat: Attribut;
      BEGIN
         WITH fat DO
	    readonly := FALSE;
            IF sy = namesy THEN
               WITH nptr^ DO
                  typtr := idtyp;
                  IF klass = vars THEN
                     addr := vaddr;
		     (* be carefully: some assumptions about mode *)
		     (* and registers are made afterwards *)
                     CASE state OF
                     |  global:
			   mode := globalMod;
			   memindex := FALSE;
                           addrReg := illegal;
                           addr2Reg := illegal;
                     |  local:
			   i := level-vlevel;
                           IF i = 0 THEN
			      mode := localMod;
                              addrReg := illegal;
			      memindex := FALSE;
                           ELSE
			      mode := indexMod; 
                              GetAddrReg(addrReg);
			      Emit3(MOVE, "%L%r@(%C),%r", base, procmarkspace,
				 addrReg);
			      FOR j := i TO 2 BY -1 DO
				 Emit3(MOVE, "%L%r@(%C),%r", addrReg, 
				   procmarkspace, addrReg);
			      END;
			      addr2Reg := illegal;
			      memindex := FALSE;
			      scale := 1;
			      addr := CARDINAL(INTEGER(offsetstack[vlevel])
			                       - INTEGER(addr));
                           END;
                           IF (typtr^.form = arrays) AND typtr^.dyn THEN
                              dynArrLevelDiff := i;
                              dynArrOffset := vaddr+oneword;
                           END;
                     |  absolute:
                           mode := absolutMod;
                           addrReg := illegal;
                           addr2Reg := illegal;
			   memindex := FALSE;
                     |  separate:
			   mode := externalMod; 
			   modPtr := globmodp;
			   addrReg := illegal;
			   addr2Reg := illegal;
			   memindex := FALSE;
                     END;
                     IF indaccess THEN
			(* assumptions: mode: global/local/index/absolut/ext *)
			(* addrReg used for index, addr2Reg always illegal! *)
			(* memindex = FALSE *)
			(* if addrReg <> illegal: scale=1 *)
			memindex := TRUE;
			post := TRUE; (* allow post-indexing *)
			od := 0;
			(* (* old code wastes address registers *)
			GetAddrReg(lReg);
                        helpat := fat;
                        helpat.typtr := intptr;
			LoadReg(helpat, lReg);
			mode := addrLoadedMod;
			addrReg := lReg;
			*)
                     END;
                  ELSIF (klass = pures) OR (klass = funcs) OR
		     (klass = mods) THEN
                     mode := procedureMod;
                     procPtr := nptr;
                  ELSE (*klass=types*)
                     Assert(klass=types);
                     GetSymbol;
                     IF sy <> lconbr THEN
                        TypFunction(idtyp, fat);
                     END;
                  END (* IF klass = .. *)  
               END (* WITH nptr^ *)
            ELSE (* sy = field *)
               Assert(sy = field); 
               UseWith(val, fat);
            END;
         END (*WITH fat*);
         IF sy <> lconbr THEN
            GetSymbol;
         END;
      END PreDesignator;

      PROCEDURE IndexVar(VAR fat: Attribut);
         (* compilation of array indexing *)
	 CONST scales = {1, 2, 4, 8}; (* possible scale values of MC68020 *)
         VAR iat: Attribut;     (* index attribute *)
	     xtyp: Stptr;       (* index type *)
	     elsize: CARDINAL;	(* element size *)
             isdyn: BOOLEAN;	(* dynamic array? *)
	     areg: Reg;

	 PROCEDURE NotReadonly(VAR fat: Attribut);
	    VAR aReg: Reg;
	 BEGIN
	    WITH fat DO
	       IF readonly THEN
		  IF addrReg <> illegal THEN
		     GetAddrReg(aReg);
		     Emit2(MOVE, "%L%r,%r", addrReg, aReg);
		     addrReg := aReg;
		     IF (mode <> addrLoadedMod) AND (mode <> localMod) AND
			(addr2Reg <> illegal) THEN
			GetAddrReg(aReg);
			Emit2(MOVE, "%L%r,%r", addrReg, aReg);
			addr2Reg := aReg;
		     END;
		  END;
		  readonly := FALSE;
	       END;
	    END;
	 END NotReadonly;

	 PROCEDURE Arithmetic(VAR xreg: Reg; VAR fat, iat: Attribut);
	    (* with cleanup of iat *)
	 BEGIN
	    WITH fat DO
	       IF elsize <> scale THEN
		  IF scale = 1 THEN
		     ConstMul(unSigned, iat, elsize);
		  ELSIF (elsize > scale) AND (elsize MOD scale = 0) THEN
		     ConstMul(unSigned, iat, elsize DIV scale);
		  ELSIF (scale > elsize) AND (scale MOD elsize = 0) THEN
		     NotReadonly(fat);
		     ConstMulReg(unSigned, xreg, scale DIV elsize);
		     scale := elsize;
		  ELSE
		     NotReadonly(fat);
		     ConstMulReg(unSigned, xreg, scale);
		     ConstMul(unSigned, iat, elsize);
		     scale := 1;
		  END;
	       END;
	       IF mode <> localMod THEN
		  NotReadonly(fat);
	       END;
	       IF (xreg IN RegSet{a0..a7}) AND
		  (iat.loadReg IN RegSet{d0..d7}) THEN
		  NotReadonly(fat);
		  Emit2(ADD, "%L%r,%r", iat.loadReg, xreg);
		  FreeReg(iat.loadReg);
	       ELSE
		  Emit2(ADD, "%L%r,%r", xreg, iat.loadReg);
		  IF (mode = localMod) AND readonly THEN
		     readonly := FALSE;
		  ELSE
		     FreeReg(xreg);
		  END;
		  xreg := iat.loadReg;
	       END;
	    END;
	 END Arithmetic;

	 PROCEDURE CheckIndex;
	    VAR label: LabelPtr; hat: Attribut;
	 BEGIN
	    IF NOT isdyn THEN
	       RangeCheck(xtyp, iat, controlRangeCheck);
	    ELSIF controlRangeCheck THEN
	       LoadAddr(fat);
	       hat := fat; hat.readonly := TRUE;
	       LoadDynHigh(hat); LoadAndExpand(hat);
	       Emit2(CMP, "%L%a,%a", hat, iat); Cleanup(hat);
	       GetLabel('I', label);
	       Emit1(BLS, "%l", label^);
	       EmitExtern(chkdyn);
	       Emit1(JSR, "%l", chkdyn);
	       EmitLabel(label);
	       DISPOSE(label);
	    END;
	 END CheckIndex;

      BEGIN
         WITH fat DO  (* attribut for array *)
            Assert(IsArrayType(typtr));
            WITH typtr^ DO
               xtyp := ixp;
               elsize := elp^.size; isdyn := dyn;
            END;
            Expression(iat); 
            IF (iat.mode = constantMod) AND NOT isdyn THEN 
	       RangeCheckForConstant(xtyp, iat);
               IF TestBaseType(xtyp) = intptr THEN 
                  SwitchAddr(fat, (CARDINAL(INTEGER(iat.value)-
				  INTEGER(xtyp^.min))*elsize));
               ELSE 
                  SwitchAddr(fat, (iat.value-xtyp^.min)*elsize);
               END;
            ELSE 
	       IF readonly THEN (* this includes stackMod *)
		  IF addrReg <> illegal THEN
		     LoadAddr(fat);
		  ELSE
		     readonly := FALSE;
		  END;
	       END;
	       (* iat.loadReg may be an address register *)
	       IF (mode = localMod) OR ByteSize(iat.typtr) THEN
		  LoadAndExpand(iat);
	       ELSIF (addrReg IN RegSet{d0..d7}) OR
		     (elsize < BitsPerWord) AND (elsize IN scales) THEN
		  LoadA(iat);
	       ELSE
		  LoadAndExpand(iat);
	       END;
	       CheckIndex;
               IF NOT isdyn AND (xtyp^.min <> 0) THEN 
		  Emit2(SUB, "%L%c,%a", xtyp^.min, iat);
	       END;
	       (* make sure that we have a free address register slot *)
	       (* or we may modify one of the address registers *)
	       (* addrLoadedMod: holds exactly one address register *)
	       (* memindex: captures at least one register; a second *)
	       (*           register is hold on localMod or NOT post *)
	       IF (mode <> addrLoadedMod) AND
		  memindex AND (NOT post OR (mode = localMod)) THEN
		  LoadAddr(fat);
	       END;
	       IF mode = addrLoadedMod THEN
		  (* convert to indexMod to have a free register slot *)
		  mode := indexMod;
		  memindex := FALSE;
		  addr := 0;
		  addr2Reg := illegal;
		  scale := 1;
	       END;
	       IF NOT memindex AND (addrReg = illegal) THEN
		  (* free slot in addrReg *)
		  IF (elsize >= BitsPerWord) OR NOT (elsize IN scales) THEN
		     ConstMul(unSigned, iat, elsize);
		     scale := 1;
		  ELSE
		     scale := elsize;
		  END;
		  readonly := FALSE;
		  addrReg := iat.loadReg;
		  IF mode <> localMod THEN
		     addr2Reg := illegal;
		  END;
	       ELSIF mode = localMod THEN
		  (* addrReg is used but may be modified *)
		  Arithmetic(addrReg, fat, iat);
	       ELSIF addr2Reg = illegal THEN
		  (* addr2Reg unused; memindex AND NOT post is excluded above *)
		  IF NOT memindex AND (scale <> 1) THEN
		     ConstMulReg(unSigned, addrReg, scale);
		  END;
		  IF (elsize >= BitsPerWord) OR NOT (elsize IN scales) THEN
		     ConstMul(unSigned, iat, elsize);
		     scale := 1;
		  ELSE
		     scale := elsize;
		  END;
		  addr2Reg := iat.loadReg;
	       ELSE (* memindex AND post OR
		     NOT memindex AND (addr2Reg <> illegal) THEN *)
		  (* addr2Reg is used and may be modified *)
		  Arithmetic(addr2Reg, fat, iat);
	       END;

	       (* the MC68020 index addressing modes allows *)
	       (* - one (optionally scaled) address register *)
	       (* - one (optionally scaled) data register *)
	       (* - one address register and an optionally scaled data reg *)
	       (* - two address registers; one of them optionally scaled *)
	       (* on memindex and post: *)
	       (* - first register must be an address register *)
	       (* - only the post-indexing register may be scaled *)

	       IF (mode <> localMod) AND (addrReg IN RegSet{d0..d7}) AND
		  ((addr2Reg IN RegSet{d0..d7}) OR (scale > 1)) THEN
		  GetAddrReg(areg);
		  Emit2(MOVE, "%L%r,%r", addrReg, areg);
		  FreeReg(addrReg);
		  addrReg := areg;
	       END;
            END; 
            typtr := typtr^.elp;
         END;
      END IndexVar;

      PROCEDURE DereferenceAt(VAR fat: Attribut);
	 VAR lReg: Reg;
      BEGIN
	 WITH fat DO
	    IF (mode IN ModeSet{globalMod, localMod, absolutMod, indexMod,
				externalMod}) AND memindex OR
	       (mode = stackMod) THEN
	       LoadA(fat);
	    END;
	    IF typtr = addrptr THEN
	       typtr := wordptr;
	    ELSE
	       Assert((typtr <> NIL) AND (typtr^.form = pointers));  
	       typtr := typtr^.elemp;
	    END;
	    IF mode = loadedMod THEN
	       lReg := loadReg;
	       mode := addrLoadedMod;
	       addrReg := lReg;
	    ELSIF mode = addrLoadedMod THEN
	       mode := indexMod;
	       addr := 0;
	       memindex := TRUE;
	       post := TRUE;
	       addr2Reg := illegal;
	       scale := 1;
	       od := 0;
	    ELSE
	       memindex := TRUE;
	       IF (addrReg <> illegal) AND (scale > 1) THEN
		  post := FALSE;
	       ELSIF mode = localMod THEN
		  post := addrReg = illegal;
	       ELSE
		  post := (addrReg IN RegSet{a0..a7, illegal}) AND
			  (addr2Reg = illegal);
	       END;
	       od := 0;
	    END;
	 END;
      END DereferenceAt;

   BEGIN (* Designator *)  
      PreDesignator(fat);  
      WHILE (sy = lbrack) OR (sy = arrow) OR (sy = period) DO  
         IF sy = lbrack THEN
	    GetSymbol;  
            IndexVar(fat);
            Assert(sy = rbrack);  
            GetSymbol (* rbrack *)  
         ELSIF sy = arrow THEN (* indirection via a pointer *)
            GetSymbol;  
	    DereferenceAt(fat);
         ELSE (* record field *)
            GetSymbol; (* period *) 
            Assert((sy = namesy) AND (nptr <> NIL) AND (nptr^.klass = fields));
            SwitchAddr(fat, nptr^.fldaddr); 
	    WITH fat DO
	       typtr := nptr^.idtyp;
	    END;
            GetSymbol;
         END  
      END  
   END Designator;  

   PROCEDURE SwitchAddr(VAR fat: Attribut; x: CARDINAL);
      VAR offs: CARDINAL; ind: BOOLEAN;
   BEGIN
      IF x > 0 THEN
	 WITH fat DO
	    IF mode = stackMod THEN
	       (* convert to localMod; this is possible only *)
	       (* if we needn't to cleanup *)
	       Assert(readonly);
	       offs := offset;
	       ind := indirect;
	       mode := localMod;
	       addr := offs;
	       addrReg := illegal;
	       scale := 1;
	       IF ind THEN
		  memindex := TRUE;
		  post := TRUE;
		  od := 0;
	       ELSE
		  memindex := FALSE;
	       END;
	    END;
	    IF mode = addrLoadedMod THEN
	       mode := indexMod;
	       addr := x;
	       memindex := FALSE;
	       scale := 1;
	       addr2Reg := illegal;
	    ELSIF memindex THEN
	       INC(od, x);
	    ELSIF mode = localMod THEN
	       DEC(addr, x);
	    ELSE
	       INC(addr, x);
	    END;
	 END;
      END;
   END SwitchAddr;

END MCP4Designator.
