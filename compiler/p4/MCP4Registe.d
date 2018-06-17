(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
DEFINITION MODULE MCP4Register; (* AFB 8/83 *)

   TYPE
      Reg = (d0, d1, d2, d3, d4, d5, d6, d7,
	     a0, a1, a2, a3, a4, limit, base, top,
	     illegal);
      FloatReg = (fr0, fr1, fr2, fr3, fr4, fr5, fr6, fr7);
      RegSet = SET OF Reg;
      RegRange = [MIN(Reg)..MAX(Reg)];
      FloatRegRange = [MIN(FloatReg)..MAX(FloatReg)];

   CONST
      a5 = limit;
      a6 = base;
      a7 = top;

   PROCEDURE GetReg(VAR r: Reg);
      (* post: r in d1..d7 *)

   PROCEDURE GetAddrReg(VAR r: Reg);
      (* post: r in a0..a5 *)

   PROCEDURE RegRequest(r: Reg; VAR done: BOOLEAN);

   PROCEDURE FreeReg(r: Reg);
      (* pre: r in d1..d7, a0..a5 *)

   PROCEDURE WithReg(r: Reg);

   PROCEDURE FreeWithReg(r: Reg);

   PROCEDURE GetFloatReg(VAR r: FloatReg);
      (* post: r in fr0..fr7 *)

   PROCEDURE FreeFloatReg(f: FloatReg);

   PROCEDURE SaveRegs;

   PROCEDURE RestoreRegs;

END MCP4Register.
