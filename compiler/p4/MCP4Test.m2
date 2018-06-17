(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
IMPLEMENTATION MODULE MCP4Test;         (* AFB 5/85 *)

   FROM MCMnemonics IMPORT Mnemonic;
   FROM MCP4Labels IMPORT LabelPtr;
   FROM MCP4CodeSys IMPORT Emit1;
   FROM MCP4AttributSys IMPORT ArithmeticType, TestType;
   FROM MCP4Register IMPORT Reg;
   FROM Storage IMPORT DEALLOCATE;

   PROCEDURE Invert(t: TestType) : TestType;
   BEGIN 
      CASE t OF 
      | lt: RETURN ge;
      | le: RETURN gt;
      | eq: RETURN ne;
      | ne: RETURN eq;
      | ge: RETURN lt;
      | gt: RETURN le;
      END;
   END Invert;

   PROCEDURE BranchMnem(t: TestType; atype: ArithmeticType; VAR bm: Mnemonic);
   BEGIN 
      IF atype = signed THEN 
         CASE t OF 
         | lt: bm := BLT;
         | le: bm := BLE;
         | eq: bm := BEQ;
         | ne: bm := BNE;
         | ge: bm := BGE;
         | gt: bm := BGT;
         END;
      ELSIF atype = floating THEN
	 (* exception on unordered ! *)
	 CASE t OF
         | lt: bm := FBLT;
         | le: bm := FBLE;
         | eq: bm := FBEQ;
         | ne: bm := FBNE;
         | ge: bm := FBGE;
         | gt: bm := FBGT;
	 END;
      ELSE 
         CASE t OF 
         | lt: bm := BCS;
         | le: bm := BLS;
         | eq: bm := BEQ;
         | ne: bm := BNE;
         | ge: bm := BCC;
         | gt: bm := BHI;
         END;
      END;
   END BranchMnem;

   PROCEDURE Test(test: TestType; atype: ArithmeticType; dest: LabelPtr);
      VAR 
         cmp, bm: Mnemonic;
   BEGIN 
      BranchMnem(test, atype, bm);
      Emit1(bm, "%l", dest^);
   END Test;

END MCP4Test. 
