(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
IMPLEMENTATION MODULE MCP4CaseSys; (* AFB 9/83 *)

   FROM Storage IMPORT ALLOCATE, DEALLOCATE;
   FROM MCBase IMPORT Symbol, maxint, intptr;
   FROM MCP4Scanner IMPORT GetSymbol, sy, val;
   FROM MCMnemonics IMPORT Mnemonic;
   FROM MCP4AttributSys IMPORT Attribut;
   FROM MCP4CodeSys IMPORT AppendComment, EmitLabel, EmitDCFValue,
      EmitDCF, EmitExtern, EmitAlign, EmitPure, EmitImpure,
      Emit1, Emit2, Emit3;
   FROM MCP4ExpressionSys IMPORT Expression;
   FROM MCP4Global IMPORT Error;
   FROM MCP4Labels IMPORT LabelType, LabelPtr, GetLabel, LabelLength, Label;
   FROM MCP4Load IMPORT Load;
   FROM MCP4Register IMPORT Reg, FreeReg;
   FROM MCP4StatSys IMPORT StatSequ3, StatSequ1;
   FROM MCP4Types IMPORT ByteSize;
   FROM MCP4RTS IMPORT caseErr;

   PROCEDURE CaseStatement;                           
      (* the case expression is assumed to have integer type *)

      TYPE 
         caseptr = POINTER TO caserec;
         caserec = 
            RECORD
               (* [clow..chigh] *)
               clow: INTEGER;
               chigh: INTEGER;
	       clabel: LabelPtr;
               next: caseptr
            END;

	 freepointer = POINTER TO freerec;
	 freerec =
	    RECORD
	       label: LabelPtr;
	       next: freepointer;
	    END;

      VAR 
         cHeader: caseptr; (* header of circular ordered list *)
         FreeList, free: freepointer;
	 low, high: INTEGER; lhInit: BOOLEAN;
         lat: Attribut;
	 CaseComp, TableLabel, ElseCase, CaseExit : LabelPtr;
         strategy: (withTable, noTable);
         cost: INTEGER; (* memory needed for 2nd strategy *)
         diff: INTEGER;

      PROCEDURE CaseLabels;
         VAR
	    label: LabelPtr;
            rlow, rhigh: INTEGER;

         PROCEDURE EnterCaseLabel(flow, fhigh: INTEGER; flabel: LabelPtr);
            (* enter new element into circular ordered list *)
            VAR
               cp: caseptr; (* new element *)
               pred, succ: caseptr;
         BEGIN
	    IF lhInit THEN
	       lhInit := FALSE;
	       low := flow;
	       high := fhigh;
            ELSE
	       IF flow < low THEN
	          low := flow;
               END;
               IF fhigh > high THEN
	          high := fhigh;
               END;
	    END;
            NEW(cp);
            WITH cp^ DO
               clow := flow;
               chigh := fhigh;
	       clabel := flabel;
            END;

            cHeader^.chigh := fhigh; (* guarantees termination of loop *)
            pred := cHeader;
            succ := cHeader^.next;
            WHILE fhigh > succ^.chigh DO
               pred := succ;
               succ := succ^.next;
            END;
            IF (succ <> cHeader) AND (fhigh >= succ^.clow) OR
               (pred <> cHeader) AND (flow  <= pred^.chigh) THEN
               Error(223);
            END;

            (* try to join if labels are equal *)
            IF (flabel = pred^.clabel) AND (pred^.chigh+1 = flow) THEN
               pred^.chigh := fhigh;
               DISPOSE(cp);
            ELSIF (flabel = succ^.clabel) AND (fhigh+1 = succ^.clow) THEN
               succ^.clow := flow;
               DISPOSE(cp);
            ELSE (* new element in list *)
               INC(cost);
               cp^.next := succ;
               pred^.next := cp;
            END;
         END EnterCaseLabel;

      BEGIN (* CaseLabels *)
	 GetLabel('C', label);
	 EmitLabel(label);

	 (* enter label in free list *)
         NEW(free);
         free^.label := label;
         free^.next := FreeList;
         FreeList := free;

         REPEAT
            rlow := val;
	    GetSymbol;
            IF sy = range THEN
	       GetSymbol;
               rhigh := val;
               GetSymbol;
            ELSE
               rhigh := rlow;
            END;
            EnterCaseLabel(rlow, rhigh, label);
         UNTIL sy = colon;
	 GetSymbol;
      END CaseLabels;

      PROCEDURE CaseTable;
         VAR
            cp, cp2: caseptr;
            upper, i: INTEGER;
	    reg: Reg;
      BEGIN
         IF strategy = withTable THEN
	    EmitAlign;
	    EmitLabel(TableLabel);
	 ELSE
	    reg := lat.loadReg;
         END;
         cp := cHeader^.next;
         IF cp <> cHeader THEN
            WITH cp^ DO
               upper := chigh;
               IF strategy = withTable THEN
                  FOR i := clow TO chigh DO
	             EmitDCF(clabel^);
                  END;
               ELSE
                  IF clow > - maxint-1 THEN
		     Emit2(CMP, "%L%i,%r", clow-1, reg);
		     Emit1(BLE, "%l", ElseCase^);
                  END;
		  Emit2(CMP, "%L%i,%r", chigh, reg);
		  Emit1(BLE, "%l", clabel^);
               END;
               cp2 := cp;
	       cp := next;
               DISPOSE(cp2);
            END;
            WHILE cp <> cHeader DO
               WITH cp^ DO
                  IF strategy = withTable THEN
                     (* upper+1 TO clow-1 *)
                     FOR i := 2 TO INTEGER(clow - upper) DO
		        EmitDCF(ElseCase^);
                     END;
                     FOR i := clow TO chigh DO
                        EmitDCF(clabel^);
                     END;
                  ELSE
                     IF upper <> clow-1 THEN
			Emit2(CMP, "%L%i,%r", clow-1, reg);
			Emit1(BLE, "%l", ElseCase^);
                     END;
		     Emit2(CMP, "%L%i,%r", chigh, reg);
		     Emit1(BLE, "%l", clabel^);
                  END;
                  upper := chigh;
                  cp2 := cp;
		  cp := next;
		  DISPOSE(cp2);
               END;
            END; (* WHILE *)
         END; (* IF *)
         IF strategy = noTable THEN
	    Emit1(BRA, "%l", ElseCase^);
         END;
      END CaseTable;

   BEGIN (* CaseStatement *)
      AppendComment("case-statement");
      cost := 0;
      lhInit := TRUE;
      NEW(cHeader); (* header of empty cicular ordered list *)
      FreeList := NIL;
      WITH cHeader^ DO
	 next := cHeader;
         clow := 0;
         chigh := 0;
         clabel := NIL;
      END;
      GetLabel('C', TableLabel);
      GetLabel('C', ElseCase);
      GetLabel('C', CaseExit);
      GetLabel('C', CaseComp);
      Expression(lat);
      Load(lat);
      WITH lat DO
	 IF ByteSize(typtr) THEN
	    Emit2(ANDop, "%L%c,%r", 0FFH, loadReg);
	 END;
	 typtr := intptr;
      END;
      (* this must be done here; *)
      (* else the register is locked for all case parts *)
      FreeReg(lat.loadReg);
      Emit1(BRA, "%l", CaseComp^);

      AppendComment("case-parts");
      WHILE sy = ofsy DO
	 GetSymbol;           
         CaseLabels; 
         StatSequ3(ofsy, elsesy, endsy); 
	 Emit1(BRA, "%l", CaseExit^);
      END;  
      EmitLabel(ElseCase);
      IF sy = elsesy THEN
	 GetSymbol; 
         StatSequ1(endsy);
	 Emit1(BRA, "%l", CaseExit^);
      ELSE
	 EmitExtern(caseErr);
	 Emit1(JSR, "%l", caseErr);
      END;
      GetSymbol;

      (* choice strategy *)
      (* be carefully: high may be maxint and low minint !! *)
      diff := high DIV 2 - low DIV 2;
      IF diff <= cost + diff DIV 4 THEN
         strategy := withTable;
      ELSE
         strategy := noTable;
      END;

      (* branch through case table *)
      IF strategy = withTable THEN
	 AppendComment("case-table");
	 CaseTable;
	 EmitLabel(CaseComp);
	 Emit2(CMP, "%L%i,%r", low, lat.loadReg);
	 Emit1(BLT, "%l", ElseCase^);
	 Emit2(CMP, "%L%i,%r", high, lat.loadReg);
	 Emit1(BGT, "%l", ElseCase^);
         IF low < 0 THEN
	    Emit2(ADD, "%L%i,%r", -low, lat.loadReg);
         ELSIF low > 0 THEN
	    Emit2(SUB, "%L%i,%r", low, lat.loadReg);
         END;
	 (* Emit3(JMP, "%r@(%l:l,%r:l:4)@(0:l)", illegal, TableLabel^,lat.loadReg); GNU *)
	 Emit3(JMP, "%r@(%l:l,%r:l:4)@(0)", illegal, TableLabel^,lat.loadReg);
      ELSE
	 EmitLabel(CaseComp);
	 CaseTable;
      END;
      (* (* is already free'd; see above *)
      FreeReg(lat.loadReg);
      *)
      AppendComment("end-case");
      EmitLabel(CaseExit);

      (* clean up *)
      DISPOSE(cHeader);
      (* dispose all members of free list *)
      WHILE FreeList <> NIL DO
         WITH FreeList^ DO
            free := next;
            DISPOSE(label);
         END;
         DISPOSE(FreeList);
         FreeList := free;
      END;
      DISPOSE(CaseComp);
      DISPOSE(TableLabel);
      DISPOSE(ElseCase);
      DISPOSE(CaseExit);
   END CaseStatement; 

END MCP4CaseSys.
