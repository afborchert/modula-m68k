(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
IMPLEMENTATION MODULE MCP4WithSys;

   FROM Storage IMPORT ALLOCATE, DEALLOCATE;
   FROM MCP4StatSys IMPORT StatSequ1;
   FROM MCP4Global IMPORT Assert;
   FROM MCP4Designator IMPORT Designator;
   FROM MCP4Scanner IMPORT GetSymbol;
   FROM MCP4AttributSys IMPORT Attribut, AtMode;
   FROM MCBase IMPORT Symbol;
   FROM MCP4Register IMPORT Reg, WithReg, FreeWithReg, FreeReg;
   FROM MCP4Load IMPORT LoadAddr;
   FROM MCP4Address IMPORT Cleanup;
   FROM MCP4CodeSys IMPORT Emit3;
   FROM MCP4Stack IMPORT GetStack;
   FROM MCMnemonics IMPORT Mnemonic;

   TYPE
      WithPt = POINTER TO WithRecord;
      WithRecord = RECORD
            wat: Attribut;
	    next: WithPt
      END;

   VAR
      firstWith : WithPt;
      withCount : INTEGER;

   PROCEDURE UseWith(i: INTEGER; VAR fat: Attribut);
         (* fat gets the description of the with element.
            May produce code for loading the address of the with variable *)
      VAR
         lWith: WithPt;
         l: INTEGER;
         rg: Reg;
   BEGIN
      lWith := firstWith;
      Assert(i <= withCount);
      FOR l := 1 TO i DO
         lWith := lWith^.next;
      END;
      WITH lWith^ DO
         fat := wat;
      END;
   END UseWith;

   PROCEDURE EnterWith(VAR fat: Attribut);
         (* Store fat for further use by UseWith,
             code for entry of a with statement *)
      VAR
         lWith: WithPt;
         i: INTEGER;
	 baseoffset: CARDINAL;
   BEGIN
      INC(withCount);
      lWith := firstWith;
      FOR i := 2 TO withCount DO
         lWith := lWith^.next;
      END;
      NEW(lWith^.next);
      lWith := lWith^.next;
      WITH lWith^ DO
         wat := fat;
	 WITH wat DO
	    IF readonly THEN
	       LoadAddr(wat);
	    END;
	    IF withCount > 1 THEN (* nested WITH-statement *)
	       IF (mode = localMod) AND (addrReg <> illegal) OR
		  (mode = addrLoadedMod) OR
		  (addrReg <> illegal) OR (addr2Reg <> illegal) THEN
		  LoadAddr(wat);
		  GetStack(1, baseoffset);
		  Emit3(MOVE, "%L%r,%r@(%o)", wat.addrReg, base, baseoffset );
		  FreeReg(wat.addrReg);
		  mode := stackMod;
		  offset := baseoffset;
		  size := 1;
		  indirect := TRUE;
	       END;
	    ELSE (* withCount = 1 *)
	       IF (mode <> localMod) AND (mode <> addrLoadedMod) AND
		  (addr2Reg <> illegal) THEN
		  LoadAddr(wat);
	       END;
	    END;
	    readonly := TRUE;
	    IF (mode <> stackMod) AND (addrReg <> illegal) THEN
	       WithReg(addrReg);
	    END;
	 END;
      END;
   END EnterWith;

   PROCEDURE ExitWith;
   (* Exit the innermost with statement *)
      VAR
         lWith: WithPt;
         i:     INTEGER;
   BEGIN
      lWith := firstWith;
      FOR i := 1 TO withCount DO
         lWith := lWith^.next;
      END;
      WITH lWith^ DO
	 WITH wat DO
	    IF (mode <> stackMod) AND (addrReg <> illegal) THEN
	       FreeWithReg(addrReg);
	    ELSE
	       readonly := FALSE;
	       Cleanup(wat);
	    END;
	 END;
      END;
      DISPOSE(lWith);
      Assert(withCount > 0);
      DEC(withCount);
   END ExitWith;

   PROCEDURE WithStatement;
      VAR lat: Attribut;
   BEGIN
      Designator(lat); EnterWith(lat);
      StatSequ1(endsy);
      GetSymbol; ExitWith;
   END WithStatement;

BEGIN
   withCount := 0;
   NEW(firstWith);
END MCP4WithSys.
