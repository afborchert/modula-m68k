(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
IMPLEMENTATION MODULE MCP4Address;

   FROM Storage IMPORT DEALLOCATE;
   FROM MCP4AttributSys IMPORT Attribut, AtMode;
   FROM MCP4Register IMPORT Reg, FreeReg, FreeFloatReg;
   FROM MCP4Labels IMPORT Label, LabelPtr, GetLabel, LabelType;
   FROM MCP4Stack IMPORT FreeStackAt;
   FROM MCP4Public IMPORT mc68881;
   IMPORT MCP4Block;

   PROCEDURE Cleanup(VAR at: Attribut);
   BEGIN
      WITH at DO
	 IF readonly THEN RETURN END;
	 CASE mode OF
	 | globalMod, indexMod, absolutMod, externalMod:
	       IF addrReg <> illegal THEN
		  FreeReg(addrReg);
	       END;
	       IF addr2Reg <> illegal THEN
		  FreeReg(addr2Reg);
	       END;
	 | localMod:
	       IF addrReg <> illegal THEN
		  FreeReg(addrReg);
	       END;
	 | addrLoadedMod:
	       FreeReg(addrReg);
	 | loadedMod:
	       FreeReg(loadReg);
	 | floatLoadedMod:
	       IF mc68881 THEN
		  FreeFloatReg(floatLoadReg);
	       ELSE
		  FreeReg(freg1); FreeReg(freg2);
	       END;
	 | conditionMod:
	       IF tlabel <> NIL THEN DISPOSE(tlabel) END;
	       IF flabel <> NIL THEN DISPOSE(flabel) END;
         | stackMod:
               FreeStackAt(at);
	 ELSE (* stringConstMod, externalMod, constantMod,
		 absolutMod, doubleConstMod, procedureMod,
		 illegalMod *)
	    (* nothing *)
	 END; (* CASE *)
	 mode := illegalMod;
      END; (* WITH *)
   END Cleanup;

END MCP4Address.
