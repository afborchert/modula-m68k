(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
IMPLEMENTATION MODULE MCP4Load; (* AFB 8/83 *)

   FROM Storage IMPORT DEALLOCATE;
   FROM MCBase IMPORT charptr, Stptr, realptr, Idptr, boolptr, onebyte,
      procmarkspace, doubleword, Structform, oneword, cardptr;
   FROM MCP4AttributSys IMPORT Attribut, AtMode, ModeSet, TestType,
      ArithmeticType;
   FROM MCP4Types IMPORT BaseType, ByteSize;
   FROM MCP4Register IMPORT Reg, FloatReg, GetReg, GetFloatReg, FreeReg,
      FreeFloatReg, GetAddrReg, RegSet, a7;
   FROM MCP4CodeSys IMPORT EmitComment, Emit1, Emit2, Emit3, EmitVarExtern,
      EmitProcExtern, Emit4, EmitLabel, Emit5;
   FROM MCMnemonics IMPORT Mnemonic;
   FROM MCP4Labels IMPORT LabelPtr, LabelType, GetLabel, Label;
   FROM MCP4Global IMPORT Assert, CompilerError;
   FROM Conversions IMPORT ConvertHex;
   FROM MCP4Block IMPORT offset, level, offsetstack;
   FROM MCP4Stack IMPORT GetStackAt, FreeStackAt;
   FROM MCP4Address IMPORT Cleanup;
   FROM MCP4Test IMPORT Test;
   FROM MCP4RTS IMPORT global;
   FROM MCP4Assign IMPORT MoveNBytes;
   FROM MCP4Public IMPORT mc68881;
   FROM MCP4FPEmulator IMPORT FPLoad;
   IMPORT MCP4Block;

   PROCEDURE Load(VAR fat: Attribut);
      VAR Register: Reg;
          FloatRegister: FloatReg;
   BEGIN
      WITH fat DO
         IF (mode = loadedMod) AND (loadReg IN RegSet{d0..d7}) OR
	    (mode = floatLoadedMod) THEN
            (* do nothing *)
         ELSIF BaseType(typtr) = realptr THEN
	    IF mc68881 THEN
	       GetFloatReg(FloatRegister);
	       LoadFloatReg(fat, FloatRegister);
	    ELSE
	       FPLoad(fat);
	    END;
	 ELSE
	    GetReg(Register);
	    LoadReg(fat, Register);
	 END;
      END;
   END Load;

   PROCEDURE LoadA(VAR fat: Attribut);
      (* loads fat into an address register *)
      VAR Register: Reg;
	  FloatRegister: FloatReg;
   BEGIN
      WITH fat DO
         IF (mode = loadedMod) AND (loadReg IN RegSet{a0..a7}) OR
	    (mode = floatLoadedMod) THEN
            (* do nothing *)
         ELSIF BaseType(typtr) = realptr THEN
	    GetFloatReg(FloatRegister);
	    LoadFloatReg(fat, FloatRegister);
	 ELSE
	    GetAddrReg(Register);
	    LoadReg(fat, Register);
	 END;
      END;
   END LoadA;

   PROCEDURE LoadReg(VAR fat: Attribut; Register: Reg); (* load into Reg. r *)
      VAR
	 endLabel: LabelPtr;
	 inst: Mnemonic;
	 reg: Reg;
   BEGIN
      WITH fat DO
	 IF mode = conditionMod THEN
	    IF (tlabel = NIL) AND (flabel = NIL) THEN
	       IF atype = floating THEN
		  (* exception on unordered ! *)
		  CASE test OF
		  | lt: inst := FSLT;
		  | le: inst := FSLE;
		  | eq: inst := FSEQ;
		  | ne: inst := FSNE;
		  | ge: inst := FSGE;
		  | gt: inst := FSGT;
		  END;
	       ELSIF atype = signed THEN 
		  CASE test OF 
		  | lt: inst := SLT;
		  | le: inst := SLE;
		  | eq: inst := SEQ;
		  | ne: inst := SNE;
		  | ge: inst := SGE;
		  | gt: inst := SGT;
		  END;
	       ELSE 
		  CASE test OF 
		  | lt: inst := SCS;
		  | le: inst := SLS;
		  | eq: inst := SEQ;
		  | ne: inst := SNE;
		  | ge: inst := SCC;
		  | gt: inst := SHI;
		  END;
	       END;
	       IF Register IN RegSet{d0..d7} THEN
		  reg := Register;
	       ELSE
		  (* be carefully: NEG does not allow addressing mode %an *)
		  reg := d0;
	       END;
	       (* reg IN RegSet{d0..d7} *)
	       Emit1(inst, "%r", reg);
	       Emit1(NEG, "%B%r", reg);
	       IF ~(Register IN RegSet{d0..d7}) THEN
		  Emit2(MOVE, "%L%r,%r", reg, Register);
	       END;
	    ELSE
	       GetLabel('I', endLabel);
	       IF tlabel = NIL THEN
		  GetLabel('I', tlabel);
	       END;
	       Test(test, atype, tlabel);
	       IF flabel <> NIL THEN
		  EmitLabel(flabel);
		  DISPOSE(flabel);
	       END;
	       Emit1(CLR, "%B%r", Register);
	       Emit1(JMP, "%l", endLabel^);
	       EmitLabel(tlabel);
	       DISPOSE(tlabel);
	       Emit2(MOVE, "%B%c,%r", ORD(TRUE), Register);
	       EmitLabel(endLabel);
	       DISPOSE(endLabel);
	    END;
	 ELSE
	    IF (Register IN RegSet{a0..a7}) AND ByteSize(typtr) THEN
	       Emit2(MOVE, "%A%#a,%r", fat, d0);
	       Emit2(ANDop, "%L%c,%r", 0FFH, d0);
	       Emit2(MOVE, "%L%r,%r", d0, Register);
	       typtr := cardptr;
	    ELSIF (mode = loadedMod) AND (loadReg IN RegSet{a0..a7}) THEN
	       (* avoid mov.b %ax,.. *)
	       Emit2(MOVE, "%L%#a,%r", fat, Register);
	    ELSE
	       Emit2(MOVE, "%A%#a,%r", fat, Register);
	    END;
	 END;
	 IF NOT readonly THEN
	    Cleanup(fat);
	 END;
         mode := loadedMod;
         loadReg := Register;
	 readonly := FALSE;
      END;
   END LoadReg;

   PROCEDURE LoadCond(VAR fat: Attribut);
   BEGIN
      WITH fat DO
	 IF mode <> conditionMod THEN
	    Load(fat);
	    FreeReg(loadReg);
            mode := conditionMod;
	    test := ne;
	    tlabel := NIL;
	    flabel := NIL;
	    atype := signed;
	 END;
      END;
   END LoadCond;

   PROCEDURE LoadFloatReg(VAR fat: Attribut; fr: FloatReg);
      VAR ConstLabel: LabelPtr;
   BEGIN
      WITH fat DO
	 Emit2(FMOVE, "%A%a,%f", fat, fr);
	 Cleanup(fat);
	 mode := floatLoadedMod;
	 floatLoadReg := fr;
	 readonly := FALSE;
      END;
   END LoadFloatReg;

   PROCEDURE LoadAddr(VAR fat: Attribut);
      VAR Register: Reg;
   BEGIN
      WITH fat DO
	 IF NOT readonly AND
	    (mode IN ModeSet{globalMod, localMod, absolutMod,
	                     indexMod, addrLoadedMod}) AND
	    (addrReg <> illegal) THEN
	    IF mode = addrLoadedMod THEN RETURN END;
	    IF addrReg IN RegSet{a0..a7} THEN
	       Register := addrReg;
	    ELSE
	       GetAddrReg(Register);
	    END;
         ELSE
            GetAddrReg(Register);
         END;
         LoadAddrReg(fat, Register);
	 readonly := FALSE;
      END;
   END LoadAddr;

   (* addr always zero after call of LoadAddrReg *)

   PROCEDURE LoadAddrReg(VAR fat: Attribut; Register: Reg);
   BEGIN
      WITH fat DO
	 IF mode = addrLoadedMod THEN
	    IF addrReg <> Register THEN
	       Emit2(MOVE, "%L%r,%r", addrReg, Register);
	    END;
	 ELSIF (mode <> stackMod) AND (mode <> setConstMod) AND
	       (mode <> stringConstMod) AND
	       memindex AND NOT post AND (od = 0) THEN
	    memindex := FALSE;
	    Emit2(MOVE, "%L%a,%r", fat, Register);
	 ELSE
	    Assert(Register IN RegSet{a0..a7});
	    Emit2(LEA, "%a,%r", fat, Register);
	 END;
	 IF NOT readonly THEN
	    IF (Register = addrReg) THEN
	       IF (mode <> addrLoadedMod) AND
		  (mode <> localMod) AND (addr2Reg <> illegal) THEN
		  FreeReg(addr2Reg);
	       END;
	    ELSE
	       Cleanup(fat);
	    END;
	 END;
	 mode := addrLoadedMod;
	 addrReg := Register;
	 readonly := FALSE;
      END;
   END LoadAddrReg;

   PROCEDURE LoadDynHigh(VAR fat: Attribut);
      VAR Index: CARDINAL;
          Register: Reg;
   BEGIN
      WITH fat DO
         GetReg(Register);
         Assert((mode = localMod) OR (mode = addrLoadedMod) OR
		(mode = indexMod));
         IF dynArrLevelDiff > 0 THEN
	    Emit3(MOVE, "%L%r@(%C),%r", base, procmarkspace, Register);
            FOR Index := dynArrLevelDiff TO 2 BY -1 DO
	       (* Emit4(MOVE, "%L%r@(%C:l,%r:l:1),%r", GNU *)
	       Emit4(MOVE, "%L%r@(%C,%r:l:1),%r",
		illegal,  procmarkspace, Register, Register);
            END;
	    (* Emit5(MOVE, "%L%r@(%O:l,%r:l:1),%r",illegal, dynArrLevelDiff, GNU *)
	    Emit5(MOVE, "%L%r@(%O,%r:l:1),%r",illegal, dynArrLevelDiff, 
	       dynArrOffset, Register, Register);
         ELSE
	    Emit3(MOVE, "%L%r@(%o),%r", base, dynArrOffset, Register);
         END;
	 Cleanup(fat);
	 readonly := FALSE;
         mode := loadedMod;
         loadReg := Register;
      END;
   END LoadDynHigh;

   PROCEDURE LoadConstant(Register: Reg; value: INTEGER);
   BEGIN
      IF value = 0 THEN
	 Emit1(CLR, "%L%r", Register);
      ELSE
	 Emit2(MOVE, "%L%i,%r", value, Register);
      END;
   END LoadConstant;

   PROCEDURE LoadBigSet(VAR fat: Attribut);
      VAR
	 helpat: Attribut; cfat: Attribut;
   BEGIN
      WITH fat DO
	 Assert((typtr <> NIL) AND (typtr^.form = bigsets));
	 IF mode = stackMod THEN RETURN END;
	 helpat := fat;
	 GetStackAt(fat); cfat := fat; cfat.readonly := TRUE;
	 MoveNBytes(cfat, helpat, typtr^.size);
	 Assert(cfat.mode = addrLoadedMod);
	 cfat.readonly := FALSE; Cleanup(cfat);
	 Cleanup(helpat);
      END;
   END LoadBigSet;

   PROCEDURE LoadAndExpand(VAR fat: Attribut);
      VAR reg: Reg;
   BEGIN
      WITH fat DO
	 IF mode = constantMod THEN
	    GetReg(reg);
	    Emit2(MOVE, "%L%c,%r", value, reg);
	    mode := loadedMod;
	    loadReg := reg;
	    IF ByteSize(typtr) THEN
	       typtr := cardptr;
	    END;
	 ELSE
	    Load(fat);
	    IF ByteSize(typtr) THEN
	       Emit2(ANDop, "%L%c,%r", 0FFH, loadReg);
	       typtr := cardptr;
	    END;
	 END;
      END;
   END LoadAndExpand;

END MCP4Load.
