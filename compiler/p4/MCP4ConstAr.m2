(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
IMPLEMENTATION MODULE MCP4ConstArithmetic; (* AFB 11/83 *)

   FROM MCBase IMPORT cardptr, intptr, BitsPerWord;
   FROM MCP4AttributSys IMPORT ArithmeticType, Attribut, AtMode;
   FROM MCP4CodeSys IMPORT EmitComment, Emit1, Emit2, Emit3;
   FROM MCP4Register IMPORT Reg, GetReg, FreeReg, a7, RegSet;
   FROM MCP4Global IMPORT CompilerError, Error, Assert;
   FROM MCMnemonics IMPORT Mnemonic;
   FROM MCP4Load IMPORT Load, LoadReg;
   FROM MCP4ExpressionSys IMPORT Mult, Div, Mod;
   FROM MCP4Address IMPORT Cleanup;

   PROCEDURE Shift(shiftinstr: Mnemonic; dx: Reg; count: CARDINAL);
      CONST
	 maxshift = 8; (* maximal shift count for [AL]S[LR] &data,dx *)
      VAR
	 ax, shiftreg: Reg;
   BEGIN
      IF (count > 1) AND (dx IN RegSet{a0..a7}) THEN
	 ax := dx;
	 dx := d0;
	 Emit2(EXG, "%r,%r", dx, ax);
      ELSE
	 ax := illegal;
      END;
      IF dx IN RegSet{a0..a7} THEN
	 WHILE count > 0 DO
	    Emit1(shiftinstr, "%L#1%r", dx);
	    DEC(count);
	 END;
      ELSIF count <= maxshift THEN
	 Emit2(shiftinstr, "%L%c,%r", count, dx);
      ELSIF count <= 2*maxshift THEN
	 Emit2(shiftinstr, "%L%c,%r", maxshift, dx);
	 Emit2(shiftinstr, "%L%c,%r", count-maxshift, dx);
      ELSE
	 IF dx <> d0 THEN
	    shiftreg := d0;
	 ELSE
	    GetReg(shiftreg);
	 END;
	 Emit2(MOVE, "%L%c,%r", count, shiftreg);
	 Emit2(shiftinstr, "%L%r,%r", shiftreg, dx);
	 IF shiftreg <> d0 THEN
	    FreeReg(shiftreg);
	 END;
      END;
      IF ax <> illegal THEN
	 Emit2(EXG, "%r,%r", dx, ax);
      END;
   END Shift;

   (* only signed/unsigned arithmetic *)

   PROCEDURE ConstMul(aT: ArithmeticType; VAR fat: Attribut; val: CARDINAL);
      VAR
         power, mask: CARDINAL;
         lat: Attribut;
         done: BOOLEAN;
         rg: Reg;
   BEGIN
      WITH fat DO
	 IF val = 1 THEN
	    (* do nothing *)
	 ELSIF val = 0 THEN
            Cleanup(fat);
            fat.mode := constantMod; fat.value := 0;
         ELSE
            GetPower(power, mask, val, done);
            IF done THEN
               Load(fat); rg := fat.loadReg;
               IF aT = signed THEN
		  Shift(ASL, rg, power);
               ELSE
		  Shift(LSL, rg, power);
               END;
            ELSE
               SetConstAt(lat, val, aT);
               Mult(fat, lat, aT);
            END;
	 END;
      END;
   END ConstMul;

   PROCEDURE ConstDiv(aT: ArithmeticType; VAR fat: Attribut; val: CARDINAL);
      VAR
         power, mask: CARDINAL;
         done: BOOLEAN;
         lat: Attribut;
         rg: Reg;
   BEGIN
      WITH fat DO
	 IF val = 1 THEN
	    (* do nothing *)
	 ELSIF val = 0 THEN
            Error(221);
         ELSE
            GetPower(power, mask, val, done);
            IF done THEN
               Load(fat); rg := fat.loadReg;
               IF aT = signed THEN
		  Shift(ASR, rg, power);
               ELSE
		  Shift(LSR, rg, power);
               END;
            ELSE
               SetConstAt(lat, val, aT);
               Div(fat, lat, aT);
            END;
	 END;
      END;
   END ConstDiv;

   PROCEDURE ConstMod(aT: ArithmeticType; VAR fat: Attribut; val: CARDINAL);
      VAR
         power, mask: CARDINAL;
         done: BOOLEAN;
         lat: Attribut;
         rg: Reg;
   BEGIN
      WITH fat DO
	 IF val = 1 THEN
            Cleanup(fat);
            mode := constantMod;
            value := 0;
	 ELSIF val = 0 THEN
            Error(221);
         ELSE
            GetPower(power, mask, val, done);
            IF done THEN
               Load(fat); rg := fat.loadReg;
	       Emit2(ANDop, "%L%c,%r", mask-1, rg);
            ELSE
               SetConstAt(lat, val, aT);
               Mod(fat, lat, aT);
            END;
	 END;
      END;
   END ConstMod;

   PROCEDURE ConstMulReg(aT: ArithmeticType; rg: Reg; val: CARDINAL);
      VAR
         fat, lat: Attribut;
         done: BOOLEAN;
         power, mask: CARDINAL;
   BEGIN
      IF val = 0 THEN
	 Emit1(CLR, "%L%r", rg);
      ELSIF val = 1 THEN
         (* do nothing *)
      ELSE (* val >= 2 *)
         GetPower(power, mask, val, done);
         IF done THEN
	    IF aT = signed THEN
	       Shift(ASL, rg, power);
	    ELSE
	       Shift(LSL, rg, power);
	    END;
         ELSE
            SetAttributes(fat, lat, aT, rg, val);
            Mult(fat, lat, aT);
         END;
      END;
   END ConstMulReg;

   PROCEDURE ConstDivReg(aT: ArithmeticType; rg: Reg; val: CARDINAL);
      VAR
         fat, lat: Attribut;
         done: BOOLEAN;
         power, mask: CARDINAL;
   BEGIN
      IF val = 0 THEN
         Error(221);
      ELSIF val = 1 THEN
         (* do nothing *)
      ELSE (* val >= 2 *)
         GetPower(power, mask, val, done);
         IF done THEN
	    IF aT = signed THEN
	       Shift(ASR, rg, power);
	    ELSE
	       Shift(LSR, rg, power);
	    END;
         ELSE
            SetAttributes(fat, lat, aT, rg, val);
            Div(fat, lat, aT);
         END;
      END;
   END ConstDivReg;

   PROCEDURE ConstModReg(aT: ArithmeticType; rg: Reg; val: CARDINAL);
      VAR
         fat, lat: Attribut;
         done: BOOLEAN;
         power, mask: CARDINAL;
   BEGIN
      IF val = 0 THEN
         Error(221);
      ELSIF val = 1 THEN
	 Emit1(CLR, "%L%r", rg);
      ELSE (* val >= 2 *)
         GetPower(power, mask, val, done);
         IF done THEN
	    Emit2(ANDop, "%L%c,%r", mask-1, rg);
         ELSE
            SetAttributes(fat, lat, aT, rg, val);
            Mod(fat, lat, aT);
         END;
      END;
   END ConstModReg;

   (* local routines *)

   PROCEDURE SetAttributes(VAR fat, lat: Attribut; AType: ArithmeticType;
                           rg: Reg; val: CARDINAL);
   BEGIN
      fat.mode := loadedMod; fat.loadReg := rg;
      fat.readonly := FALSE;
      IF AType = signed THEN
         fat.typtr := intptr;
      ELSE
         fat.typtr := cardptr;
      END;
      lat.mode := constantMod; lat.value := val;
      lat.typtr := fat.typtr;
   END SetAttributes;

   PROCEDURE SetConstAt(VAR lat: Attribut; val: CARDINAL;
                        AType: ArithmeticType);
   BEGIN
      lat.mode := constantMod; lat.value := val;
      lat.readonly := FALSE;
      IF AType = signed THEN
         lat.typtr := intptr;
      ELSE
         lat.typtr := cardptr;
      END;
   END SetConstAt;

   PROCEDURE GetPower(VAR power, mask: CARDINAL;
                      val: CARDINAL; VAR done: BOOLEAN);
   BEGIN
      mask := 1;
      FOR power := 1 TO BitsPerWord-1 DO
         mask := mask * 2;
         IF mask = val THEN done := TRUE; RETURN END;
      END;
      done := FALSE;
   END GetPower;

END MCP4ConstArithmetic.
