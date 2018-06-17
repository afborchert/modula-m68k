(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
IMPLEMENTATION MODULE MCP4Register; (* AFB 8/83 *)

   FROM MCP4Global IMPORT Assert, Error;
   FROM MCP4CodeSys IMPORT Emit2;
   FROM MCP4Stack IMPORT IncTop, DecTop;
   FROM MCMnemonics IMPORT Mnemonic;
   FROM MCP4Public IMPORT mc68881;
   FROM MCBase IMPORT oneword, BitsPerWord, doubleword;

   (*
    *	principles of operation:
    *
    *	temporary use		r0, r1
    *	protected registers	base, top, limit (= UNIX stack pointer)
    *	free registers		r3, r4, r5, r6, r8, r9, ra, rb, rc, rd, rf
    *
    *	rf is used as link register, but saved by GenBlockEntry.
    *
    *	Normally register will be allocated with GetReg and
    *	freed with FreeReg. This registers are allocated at
    *	their specific level, i.e. they are lost after RestoreRegs
    *	and must not be used after SaveRegs.
    *	Exception: one register is allocated for all levels greater
    *	or equal to the actual level for each "with"-statement;
    *	this register should be declared with WithReg and freed
    *	with FreeWithReg.
    *
    *   changes for XELOS:
    *
    *   protected registers    base (= re), top (= rd), limit (= r2)
    *   free registers         r3, r4, r5, r6, r7, r8, r9, ra, rb, rc, rf
    *)

   CONST
      maxlevel = BitsPerWord;
      FreeRegisters = 11;
   TYPE
      RegType = (free, withreg, prot, locked);
      RegUse =
	 RECORD
            type: RegType;
	 END;
      FloatRegUse = (used, unused);
      Registers = [d0..top];
      FloatRegisters = [fr0..fr7];
      LevelRange = [0..maxlevel-1];
      RegisterSet = SET OF Registers;
      FloatRegSet = SET OF FloatReg;

   VAR
      level: LevelRange; (* incremented/decremented by SaveRegs/RestoreRegs *)
      leveloverflow: CARDINAL;      (* simulate level >= maxlevel *)
      RegTab : ARRAY LevelRange, Registers OF RegUse; (* general registers *)
      FloatRegTab : ARRAY LevelRange, FloatRegisters OF FloatRegUse;
         (* double precision floating point registers *)
      Index : CARDINAL;
      RegIndex : Registers;
      FloatRegIndex : FloatRegisters;

   PROCEDURE GetFloatReg(VAR r: FloatReg);
      TYPE FloatRegRange = [fr1..fr7];
   BEGIN
      FOR r := MIN(FloatRegRange) TO MAX(FloatRegRange) DO
	 IF FloatRegTab[level, r] = unused THEN
	    FloatRegTab[level, r] := used;
	    RETURN
	 END
      END;
      Error(204);
      r := fr0;
   END GetFloatReg;

   PROCEDURE Lookup(VAR r: Reg; rset: RegisterSet);
   BEGIN
      FOR r := MIN(Registers) TO MAX(Registers) DO
	 IF (r IN rset) AND (RegTab[level, r].type = free) THEN
	    RegTab[level, r].type := locked;
	    RETURN
	 END;
      END;
      Error(204);
      IF a1 IN rset THEN
	 r := limit;
      ELSE
	 r := d0;
      END;
   END Lookup;

   PROCEDURE GetReg(VAR r: Reg);
   BEGIN
      IF mc68881 THEN
	 Lookup(r, RegisterSet{d1..d7});
      ELSE
	 Lookup(r, RegisterSet{d2..d7});
      END;
   END GetReg;

   PROCEDURE RegRequest(r: Reg; VAR done: BOOLEAN);
   BEGIN
      IF RegTab[level, r].type = free THEN
	 RegTab[level, r].type := locked;
	 done := TRUE;
      ELSE
	 done := FALSE;
      END;
   END RegRequest;

   PROCEDURE GetAddrReg(VAR r: Reg);
   BEGIN
      Lookup(r, RegisterSet{a0..a4});
   END GetAddrReg;

   PROCEDURE FreeReg(r: Reg);
   BEGIN
      IF (r <> d0) AND (r <> limit) THEN
	 WITH RegTab[level, r] DO
	    Assert(type = locked);
	    type := free;
	 END;
      END;
   END FreeReg;

   PROCEDURE FreeFloatReg(r: FloatReg);
   BEGIN
      FloatRegTab[level, r] := unused;
   END FreeFloatReg;

   PROCEDURE WithReg(r: Reg);
      VAR l: LevelRange;
   BEGIN
      FOR l := level TO maxlevel-1 DO
         RegTab[l, r].type := withreg;
      END;
   END WithReg;

   PROCEDURE FreeWithReg(r: Reg);
      VAR l: LevelRange;
   BEGIN
      FOR l := level TO maxlevel-1 DO
         RegTab[l, r].type := free;
      END;
   END FreeWithReg;

   PROCEDURE IsAllocated(VAR type: RegType) : BOOLEAN;
   BEGIN
      RETURN (type = withreg) OR (type = locked);
   END IsAllocated;

   VAR
      RegAlloc, FloatRegAlloc: ARRAY LevelRange OF
         RECORD
            offset: CARDINAL; (* offset to base *)
         END;

   PROCEDURE SaveRegs;
      VAR
         space: CARDINAL; (* in words *)
	 regset: RegSet;
	 rg: Reg;
	 frg: FloatReg;
	 fregset: FloatRegSet;
	 bitset: BITSET;
	 addtop: CARDINAL;
   BEGIN
      addtop := 0;
      (* save general registers *)
      (* assume following *)
      (* 1) SIZE(RegSet) = 4 *)
      (* 2) RegSet{d0}: bit with highest significance is set *)
      regset := RegSet{};
      FOR rg := d0 TO a7 DO
	 IF IsAllocated(RegTab[level, rg].type) THEN
	    INCL(regset, rg);
	    INC(addtop, oneword);
	 END;
      END;
      IF regset <> RegSet{} THEN
	 Emit2(MOVEM, "%L%c,%r@-", CARDINAL(regset) DIV 10000H, top);
      END;

      (* save floating point registers *)
      bitset := {};
      FOR frg := MIN(FloatReg) TO MAX(FloatReg) DO
	 IF FloatRegTab[level, frg] = used THEN
	    INCL(bitset, BitsPerWord - ORD(frg) - 1);
	    INC(addtop, doubleword);
	 END;
      END;
      IF bitset <> {} THEN
	 Emit2(FMOVEM, "%X%c,%r@-", bitset, top);
      END;
      IncTop(addtop);

      IF level = maxlevel-1 THEN
         INC(leveloverflow);
      ELSE
         INC(level);
      END;
   END SaveRegs;

   PROCEDURE RestoreRegs;
      VAR
	 rg: Reg;
	 frg: FloatReg;
	 bitset: BITSET;
	 fregset: FloatRegSet;
	 dectop: CARDINAL;
   BEGIN
      (* inversion of SaveRegs *)
      IF leveloverflow > 0 THEN
         DEC(leveloverflow);
      ELSE
         DEC(level);
      END;

      dectop := 0;
      (* restore floating point registers *)
      fregset := FloatRegSet{};
      FOR frg := MIN(FloatReg) TO MAX(FloatReg) DO
	 IF FloatRegTab[level, frg] = used THEN
	    INCL(fregset, frg);
	    INC(dectop, doubleword);
	 END;
      END;
      IF fregset <> FloatRegSet{} THEN
	 Emit2(FMOVEM, "%X%r@+,%c", top, CARDINAL(fregset) DIV 1000000H);
      END;

      (* load general registers *)
      bitset := {};
      FOR rg := d0 TO a7 DO
	 IF IsAllocated(RegTab[level, rg].type) THEN
	    INCL(bitset, BitsPerWord - ORD(rg) - 1);
	    INC(dectop, oneword);
	 END;
      END;
      IF bitset <> {} THEN
	 Emit2(MOVEM, "%L%r@+,%c", top, bitset);
      END;

      DecTop(dectop);
   END RestoreRegs;

   PROCEDURE ProtectReg(r: Reg);
   BEGIN
      RegTab[level, r].type := prot;
   END ProtectReg;

BEGIN
   FOR level := 0 TO maxlevel-1 DO
      FOR RegIndex := MIN(Registers) TO MAX(Registers) DO
         RegTab[level, RegIndex].type := free;
      END;
      FOR FloatRegIndex := MIN(FloatRegisters) TO MAX(FloatRegisters) DO
         FloatRegTab[level, FloatRegIndex] := unused;
      END;
   END;
   leveloverflow := 0;
   FOR level := 0 TO maxlevel-1 DO
      ProtectReg(top);
      ProtectReg(limit);
      ProtectReg(base);
   END;
   level := 0;
END MCP4Register.
