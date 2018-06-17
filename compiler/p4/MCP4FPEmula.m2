(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
IMPLEMENTATION MODULE MCP4FPEmulator;

   FROM MCBase IMPORT Symbol, oneword, doubleword, intptr, realptr, boolptr;
   FROM MCP4Block IMPORT EnterReal;
   FROM MCP4AttributSys IMPORT Attribut, ArithmeticType, TestType, AtMode;
   FROM MCP4Designator IMPORT SwitchAddr;
   FROM MCP4Labels IMPORT Label, LabelPtr, GetLabel;
   FROM MCP4Register IMPORT RegRequest, FreeReg, Reg, GetReg, SaveRegs,
      RestoreRegs;
   FROM MCP4Load IMPORT Load, LoadReg;
   FROM MCP4CodeSys IMPORT Emit1, Emit2, EmitLabel;
   FROM MCP4Address IMPORT Cleanup;
   FROM MCP4Test IMPORT Test;
   FROM MCMnemonics IMPORT Mnemonic;
   FROM Storage IMPORT DEALLOCATE;

   CONST
      addL = "dbadd%%";
      subL = "dbsub%%";
      mulL = "dbmul%%";
      divL = "dbdiv%%";
      cmpL = "dbcmp%%";
      truncL = "dbtol%%";
      floatL = "ltodb%%";
      negL = "dbneg%%";

   PROCEDURE FPOp(op: Symbol; VAR at1, at2: Attribut);
      (* at1 op at2; result in at1; at2 is subject to Cleanup *)
      VAR
	 rtslbl: Label;
	 hat: Attribut;
	 compare: BOOLEAN;

   BEGIN

      compare := FALSE;
      CASE op OF
      | plus:  rtslbl := addL;
      | minus: rtslbl := subL;
      | times: rtslbl := mulL;
      | slash: rtslbl := divL;
      ELSE
	 rtslbl := cmpL; compare := TRUE;
      END;

      IF NOT compare THEN
	 FPLoad(at1);
      END;
      FPLoadReg(at1, d0, d1);

      SaveRegs;
      FPPush(at2);
      Emit1(JSR, "%l", rtslbl);
      Emit2(ADD, "%L%c,%r", doubleword, top);
      RestoreRegs;
      Cleanup(at2);

      WITH at1 DO
	 IF compare THEN
	    Emit1(TST, "%W%r", d0);
	    Cleanup(at1);
	    typtr := boolptr;
	    readonly := FALSE;
	    mode := conditionMod;
	    atype := signed;
	    tlabel := NIL; flabel := NIL;
	    CASE op OF  
	    | neq: test := ne;
	    | eql: test := eq;
	    | leq: test := le;
	    | lss: test := lt;
	    | geq: test := ge;
	    | grt: test := gt;
	    END;
	 ELSE
	    Emit2(MOVE, "%L%r,%r", d0, freg1);
	    Emit2(MOVE, "%L%r,%r", d1, freg2);
	 END;
      END;
   END FPOp;

   PROCEDURE FPFloat(VAR at: Attribut);
   BEGIN
      LoadReg(at, d0); Cleanup(at);
      SaveRegs;
      Emit1(JSR, "%l", floatL);
      RestoreRegs;
      FPGetReturnVal(at);
   END FPFloat;

   PROCEDURE FPTrunc(VAR at: Attribut);
   BEGIN
      FPLoadReg(at, d0, d1); Cleanup(at);
      SaveRegs;
      Emit1(JSR, "%l", truncL);
      RestoreRegs;
      WITH at DO
	 typtr := intptr;
	 readonly := FALSE;
	 mode := loadedMod;
	 GetReg(loadReg);
	 Emit2(MOVE, "%L%r,%r", d0, loadReg);
      END;
   END FPTrunc;

   PROCEDURE FPAbs(VAR at: Attribut);
      VAR
	 zero: Attribut;
	 notneg: LabelPtr;
	 saveat: Attribut;
   BEGIN
      WITH zero DO
	 typtr := realptr;
	 readonly := FALSE;
	 mode := floatLoadedMod;
	 GetReg(freg1); GetReg(freg2);
	 Emit1(CLR, "%L%r", freg1); Emit1(CLR, "%L%r", freg2);
      END;
      saveat := at; at.readonly := TRUE;
      FPOp(geq, at, zero);
      GetLabel('A', notneg);
      WITH at DO
	 Test(test, atype, notneg);
	 at := saveat; FPNeg(at);
      END;
      EmitLabel(notneg); DISPOSE(notneg);
   END FPAbs;

   PROCEDURE FPNeg(VAR at: Attribut);
   BEGIN
      FPLoad(at); FPLoadReg(at, d0, d1);
      SaveRegs;
      Emit1(JSR, "%l", negL);
      RestoreRegs;
      WITH at DO
	 Emit2(MOVE, "%L%r,%r", d0, freg1);
	 Emit2(MOVE, "%L%r,%r", d1, freg2);
      END;
   END FPNeg;

   PROCEDURE FPLoadReg(at: Attribut; r1, r2: Reg);
      VAR constLabel: LabelPtr;
   BEGIN
      WITH at DO
	 IF mode = floatLoadedMod THEN
	    IF freg1 <> r1 THEN Emit2(MOVE, "%L%r,%r", freg1, r1) END;
	    IF freg2 <> r2 THEN Emit2(MOVE, "%L%r,%r", freg2, r2) END;
	 ELSIF mode = doubleConstMod THEN
	    GetLabel('r', constLabel);
	    EnterReal(constLabel, Real);
	    Emit2(MOVE, "%L%l,%r", constLabel^, r1);
	    Emit2(MOVE, "%L%l+4,%r", constLabel^, r2);
	 ELSE
	    Emit2(MOVE, "%L%a,%r", at, r1);
	    readonly := TRUE; SwitchAddr(at, oneword);
	    Emit2(MOVE, "%L%a,%r", at, r2);
	 END;
      END;
   END FPLoadReg;

   PROCEDURE FPLoad(VAR at: Attribut);
      VAR r1, r2: Reg;
   BEGIN
      WITH at DO
	 IF mode <> floatLoadedMod THEN
	    GetReg(r1); GetReg(r2);
	    FPLoadReg(at, r1, r2);
	    Cleanup(at); readonly := FALSE;
	    mode := floatLoadedMod;
	    freg1 := r1; freg2 := r2;
	 END;
      END;
   END FPLoad;

   PROCEDURE FPStore(VAR at, des: Attribut);
      (* store at to des; no cleanup done *)
      VAR ro: BOOLEAN;
   BEGIN
      FPLoad(at);
      WITH at DO
	 Emit2(MOVE, "%L%r,%a", freg1, des);
	 ro := des.readonly; des.readonly := TRUE;
	 SwitchAddr(des, oneword); des.readonly := ro;
	 Emit2(MOVE, "%L%r,%a", freg2, des);
      END;
   END FPStore;

   PROCEDURE FPPush(VAR at: Attribut);
      (* push at onto stack; no cleanup done *)
      VAR hat: Attribut;
   BEGIN
      WITH at DO
	 IF mode = floatLoadedMod THEN
	    Emit2(MOVE, "%L%r,%r@-", freg2, top);
	    Emit2(MOVE, "%L%r,%r@-", freg1, top);
	 ELSE
	    hat := at; hat.readonly := TRUE; SwitchAddr(hat, oneword);
	    Emit2(MOVE, "%L%a,%r@-", hat, top);
	    Emit2(MOVE, "%L%a,%r@-", at, top);
	 END;
      END;
   END FPPush;

   PROCEDURE FPReturn(VAR at: Attribut);
      (* with cleanup of at *)
   BEGIN
      FPLoadReg(at, d0, d1);
      Cleanup(at);
   END FPReturn;

   PROCEDURE FPGetReturnVal(VAR at: Attribut);
   BEGIN
      WITH at DO
	 typtr := realptr;
	 readonly := FALSE;
	 mode := floatLoadedMod;
	 GetReg(freg1); GetReg(freg2);
	 Emit2(MOVE, "%L%r,%r", d0, freg1);
	 Emit2(MOVE, "%L%r,%r", d1, freg2);
      END;
   END FPGetReturnVal;

END MCP4FPEmulator.
