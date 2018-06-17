(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
DEFINITION MODULE MCP4AttributSys;

   FROM MCBase IMPORT Idptr, Stptr, Stringptr, SetValuePtr;
   FROM MCP4Register IMPORT Reg, FloatReg;
   FROM MCP4Labels IMPORT LabelPtr;

   TYPE
      ArithmeticType = (signed, unSigned, bitwise, floating, logical);
      TestType = (lt, le, eq, ne, ge, gt);

      AtMode  = (globalMod, localMod, floatLoadedMod, loadedMod, addrLoadedMod,
         externalMod, indexMod, constantMod, absolutMod,
         doubleConstMod, procedureMod, stringConstMod, conditionMod,
         stackMod, setConstMod, illegalMod);

      ModeSet = SET OF AtMode;

      Attribut = RECORD
         typtr: Stptr; (* type of attribut *)
	 readonly: BOOLEAN; (* if on: don't change or release anything *)
         CASE mode: AtMode OF
         | globalMod, localMod, absolutMod, indexMod,
           addrLoadedMod, externalMod:
	       addrReg: Reg; (* may be illegal for global/local/absolut/ext *)
	       CASE : AtMode OF
	       | localMod, addrLoadedMod, indexMod:
		     (* used for dynamic array parameters *)
		     (* localMod may be converted to *)
		     (* addrLoadedMod or indexMod *)
		     CASE : BOOLEAN OF
		     | TRUE: dynArrLevelDiff, dynArrOffset: CARDINAL;
		     END;
	       END;
               CASE : AtMode OF
               | addrLoadedMod:
               ELSE
		  addr: CARDINAL; (* offset *)
		  scale: CARDINAL; (* 1, 2, 4, or 8 *)
		  CASE memindex: BOOLEAN OF
		  | TRUE: post: BOOLEAN;
			  od: CARDINAL; (* outer displacement *)
		  END;
		  CASE : AtMode OF
		  | globalMod, absolutMod, indexMod, externalMod:
			addr2Reg: Reg;
			CASE : AtMode OF (* external variables *)
			| externalMod: modPtr: Idptr;
			END;
		  END;
               END;
         | conditionMod: (* condition codes set *)
               test: TestType; (* test for TRUE *)
               atype: ArithmeticType; (* signed or unSigned *)
               tlabel: LabelPtr; (* may be NIL; branch on TRUE condition *)
               flabel: LabelPtr; (* may be NIL; branch on FALSE condition *)
         | constantMod:
               CASE : BOOLEAN OF
               | TRUE:  value: CARDINAL;
               | FALSE: iValue: INTEGER;
               END;
         | doubleConstMod:
               CASE : BOOLEAN OF
               | TRUE: Real1, Real2: CARDINAL
               | FALSE: Real: REAL
               END;
         | stringConstMod:
               strgPtr: Stringptr;
	 | setConstMod:
	       setPtr: SetValuePtr;
         | procedureMod:
              procPtr: Idptr;
         | floatLoadedMod:
	       CASE (* MCP4Public.mc68881 *) : BOOLEAN OF
	       | TRUE:  floatLoadReg: FloatReg;
	       | FALSE: freg1, freg2: Reg;
	       END;
         | loadedMod:
               loadReg: Reg;
         | stackMod:
               offset: CARDINAL; (* to base *)
               size: CARDINAL;
	       (* NOT size of type; size of stack use in fullwords *)
	       indirect: BOOLEAN;
         END;
      END (* Attribut *) ;

END MCP4AttributSys.
