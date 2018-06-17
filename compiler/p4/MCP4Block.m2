(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
IMPLEMENTATION MODULE MCP4Block;        (* AFB 8/83 and 10/87 *)

   FROM Storage IMPORT ALLOCATE, DEALLOCATE;
   FROM MCBase IMPORT Idptr, Idclass, Stptr, Symbol, mainmodp, globvarnext,
      root, Varkind, ismain, oneword, procmarkspace, noprio, BitsPerWord,
      procnumber;
   FROM MCP4Global IMPORT CompilerError, Error, Assert;
   FROM MCP4Scanner IMPORT GetSymbol, sy, nptr, line, pline;
   FROM MCP4StatSys IMPORT StatSequ1;
   FROM MCP4CodeSys IMPORT EmitAlign, EmitComment, AppendValue, EmitLabel,
      EmitEQU, EmitDCFValue, EmitPure, EmitImpure, star,
      AppendComment, EmitExtern, EmitBSS, EmitEntry,
      EmitEQUInt, EmitExportedProcLabel, EmitProcExtern,
      EmitDef, EmitDefPV,
      Emit, Emit1, Emit2, Emit3, Emit4, Emit5;
   FROM Conversions IMPORT ConvertInteger;
   FROM MCP4Register IMPORT Reg, GetReg, FreeReg, GetAddrReg;
   FROM MCMnemonics IMPORT Mnemonic;
   FROM MCP4Labels IMPORT LabelPtr, GetLabel, PushLabel, PopLabel,
      TopLabel, LabelType;
   FROM MCP4Types IMPORT IsArrayType, ByteSize;
   FROM MCP4ConstArithmetic IMPORT ConstMulReg;
   FROM MCP4AttributSys IMPORT ArithmeticType;
   FROM MCP4Public IMPORT profile, Kflag;
   FROM MCP4Stack IMPORT SetStartOffset, GetStackUse;
   FROM MCP4RTS IMPORT global, flag, startLabel, entprio, exprio, fret, mcount,
      procprefix;
   IMPORT MCP4CodeSys, Storage, MCMnemonics, MCP4Register, MCP4Labels,
      MCP4Public, MCP4Stack, MCBase, MCP4RTS;

   (* $R- *)

   CONST
	B20K = 20480;
	B32K = 32768;
   VAR
      StackUse: LabelPtr;
	 linkw : BOOLEAN;	(* TRUE if size of parameters < 20K *)

   PROCEDURE Block(fnptr: Idptr);
      VAR 
	 procnptr: Idptr;
	 ReturnLabel: LabelPtr;
   BEGIN 
      (* calculate base offset *)
      IF fnptr <> NIL THEN
         WITH fnptr^ DO
            IF klass = mods THEN
               offsetstack[level] := procmarkspace;
            ELSE
               offsetstack[level] := idtyp^.parlength;
            END;
         END;
      ELSE
         offsetstack[level] := 0;
      END;
      WHILE (sy = proceduresy) DO 
         procnptr := nptr;
         GetSymbol;
         INC(level);
         Block(procnptr);
         DEC(level);
      END;
      blockNptr := fnptr;
      GenBlockEntry;
      IF sy = beginsy THEN 
         GetSymbol;
         StatSequ1(endblock);
      END;
      TerminateBlock;
      Assert(sy=endblock);
      GetSymbol;                         (*endblock*)
   END Block;

   PROCEDURE GenBlockEntry;
      VAR 
	 ProfileLabel: LabelPtr;
	 label: ARRAY [0..7] OF CHAR;
         field: ARRAY [0..7-3] OF CHAR;
	 ReturnLabel: LabelPtr;
	 loopLabel: LabelPtr;
	 lpp, lvp: Idptr;
	 j: CARDINAL;
	 rlen, rx: Reg;

   BEGIN 
      IF blockNptr <> NIL THEN
         WITH blockNptr^ DO
            offset := offsetstack[level];
            (* emit procedure label *)
            GetLabel('B', ReturnLabel);
            PushLabel('B', ReturnLabel);
	    label := procprefix;
            ConvertInteger(procnum, 1, field);
            FOR j := 3 TO 7 DO
               label[j] := field[j-3];
            END;
            EmitPure;
            EmitAlign;
            EmitEQU(label,star);
	    EmitDefPV("def",line, mainmodp^.identifier, 'P', procnum);
	    (* IF plev = 1 THEN -- label must be generated in each case *)
	       (* may be exported *)
	       EmitExportedProcLabel(procnum);
	    (* END; *)
            IF ismain AND (procnum = 0) THEN
               EmitEntry(startLabel);
	       EmitEQU(startLabel, star);
            END;

            (* StackUse: symbol for allocated stack size in bytes *)
            GetLabel('I', StackUse);

            (* set up activation record *)

            IF blockNptr = mainmodp THEN
	       IF (varlength < B20K) AND ~Kflag THEN
	           Emit2(LINK, "%r,#%l", base, StackUse^);
		   linkw := TRUE;
	       ELSE
		   Emit1(LINK, "%r,#0", base);
		   Emit2( ADD, "%L#%l,%r", StackUse^, top);
		   linkw := FALSE;
	       END(*IF*);
	       Emit1(TAS, "%l", flag);
	       Emit1(BNE, "%l", ReturnLabel^);

               lpp := root^.locp^.link;
	       lvp := NIL;

               (* initialize imported modules *)
               WHILE lpp <> NIL DO
                  IF lpp <> mainmodp THEN
		     EmitProcExtern(lpp^.identifier, 0);
		     Emit1(JSR, "_%l_P0", lpp^.identifier);
                  END;
                  lpp := lpp^.link;
               END;
               lpp := NIL;
               SetStartOffset(offset);
            ELSIF klass = mods THEN
               lpp := NIL;
               lvp := NIL;
               SetStartOffset(offset);
	       IF (varlength < B20K) AND ~Kflag THEN
	           Emit2(LINK, "%r,#%l", base, StackUse^);
		   linkw := TRUE;
	       ELSE
		   Emit1(LINK, "%r,#0", base);
		   Emit2( ADD, "%L#%l,%r", StackUse^, top);
		   linkw := FALSE;
	       END(*IF*);
            ELSE
               (* proc/func entry *)
               lvp := locvarp;
               lpp := idtyp^.fstparam;

	       IF (varlength < B20K) AND ~Kflag THEN
	           Emit2(LINK, "%r,#%l", base, StackUse^);
		   linkw := TRUE;
	       ELSE
		   Emit1(LINK, "%r,#0", base);
		   Emit2( ADD, "%L#%l,%r", StackUse^, top);
		   linkw := FALSE;
	       END(*IF*);

               (* set start offset for stack allocation *)
               SetStartOffset(varlength);
            END;

            (* enter priority (if necessary) *)
            IF (plev = 1) AND (priolev <> noprio) THEN
	       Emit2(MOVE, "%L%c,%r@-", priolev, top);
               EmitExtern(entprio);
	       Emit1(JSR, "%l", entprio);
            END;

            (* allocate space for copy parameters *)
            WHILE lpp <> NIL DO
               WITH lpp^ DO
                  IF vkind = copyparam THEN

		     (* rlen: # fullwords+1 to be moved *)
		     (* copy from rx to top             *)

		     GetReg(rlen); GetAddrReg(rx);
		     (* calculate # bytes to be moved *)
                     IF IsArrayType(idtyp) AND idtyp^.dyn THEN
                        (* MCP4CallSys: vaddr(base) = copy address *)
                        (*      vaddr+oneword(base) = HIGH         *)
			Emit3(MOVE, "%L%r@(%o),%r", base, vaddr+oneword, rlen);
                        IF NOT ByteSize(idtyp^.elp) THEN
			   Emit2(ADD, "%L%c,%r", 1, rlen);
                           ConstMulReg(unSigned, rlen, idtyp^.elp^.size);
                        END;
			IF idtyp^.elp^.size MOD oneword <> 0 THEN
			   (* align rlen *)
			   IF ByteSize(idtyp^.elp) THEN
			      Emit2(ADD, "%L%c,%r", oneword, rlen);
			   ELSE
			      Emit2(ADD, "%L%c,%r", oneword-1, rlen);
			   END;
			   Emit2(ANDop, "%B%c,%r", 374B, rlen);
			END;
                     ELSE
			IF idtyp^.size MOD oneword <> 0 THEN
			   Emit3(MOVE, "%L%c,%r", idtyp^.size + oneword -
			                  idtyp^.size MOD oneword, base, rx);
			ELSE
			   Emit2(MOVE, "%L%c,%r", idtyp^.size, rlen);
			END;
                     END;
		     Emit3(MOVE, "%L%r@(%o),%r", base, vaddr, rx);
		     Emit2(ADD, "%L%r,%r", rlen, rx);
		     Emit2(LSR, "%L%c,%r", 2, rlen);
		     Emit2(SUB, "%L%c,%r", 1, rlen);
		     GetLabel('L', loopLabel);
		     EmitLabel(loopLabel);
		     Emit2(MOVE, "%L%r@-,%r@-", rx, top);
		     Emit2(DBF, "%r,%l", rlen, loopLabel^);
		     Emit3(MOVE, "%L%r,%r@(%o)", top, base, vaddr);
		     FreeReg(rlen); FreeReg(rx);
                  END;
               END;
               lpp := lpp^.vlink;
            END;

            (* allocate space for local variables *)
            WHILE lvp <> NIL DO
               WITH lvp^ DO
                  IF indaccess THEN
                     (* keep top aligned !! *)
                     IF idtyp^.size MOD oneword = 0 THEN
			Emit2(SUB, "%L%c,%r", idtyp^.size, top);
                     ELSE
			Emit2(SUB, "%L%c,%r", idtyp^.size + oneword -
                                              idtyp^.size MOD oneword, top);
                     END;
		     Emit3(MOVE, "%L%r,%r@(%o)", top, base, vaddr);
                  END;
                  lvp := vlink;
               END
            END;
            (* count procedure calls if profile is on *)
            IF profile THEN
               GetLabel('P', ProfileLabel);
               EmitBSS(ProfileLabel, 1 (* fullword *));
               EmitPure;
	       Emit2(MOVE, "%L%#l,%r@-", ProfileLabel^, top);
	       EmitExtern(mcount);
	       Emit1(JSR, "%l", mcount);
               DISPOSE(ProfileLabel);
            END;
	    (* check for stack overflow *)
	    Check;
            (* generate special label for the debugger (for breakpoints) *)
	    EmitDef("bf"); (* begin of function *)
	    pline := 1;
         END;
      END;
   END GenBlockEntry;

   PROCEDURE TerminateBlock;
      VAR label: ARRAY[0..7] OF CHAR;
          field: ARRAY[0..7-3] OF CHAR;
          j: CARDINAL;
          size: CARDINAL;
   BEGIN 
      IF blockNptr <> NIL THEN
         (* a function must return a value *)
         WITH blockNptr^ DO
            IF klass = funcs THEN
               EmitExtern(fret);
	       Emit1(JSR, "%l", fret);
            END;
         END;
         (* emit label for RETURN *)
         EmitLabel(PopLabel('B'));
	 (* generate label for stack overflow check *)
	 EmitOffsetLabel;
         (* generate label for allocated stack size in bytes *)
         GetStackUse(size); (* size in fullwords *)
	 size := size * oneword;
	 IF blockNptr^.klass <> mods THEN
	    INC(size, blockNptr^.varlength - offset);
	 END;
	 IF linkw AND ( INTEGER(size) > B32K) THEN
	    Error( 400);
	 END(*IF*);
         EmitEQUInt(StackUse^, - INTEGER(size));
         DISPOSE(StackUse);
         (* generate special label for the debugger *)
	 EmitDef("ef");
	 pline := 0;
         (* exit priority (if necessary) *)
         WITH blockNptr^ DO
            IF (plev = 1) AND (priolev <> noprio) THEN
               EmitExtern(exprio);
	       Emit1(JSR, "%l", exprio);
            END;
         END;
         (* return sequence *)
	 Emit1(UNLK, "%r", base);
	 IF offset = procmarkspace THEN
	    Emit(RTS, "");
	 ELSE
	    Emit1(RTD, "%c", offset-procmarkspace);
	 END;
	 (*EmitDefPV("def", (*line,*)mainmodp^.identifier, 'P',
						blockNptr^.procnum);*)
         FlushEQUs;
         FlushReals;
      END;
   END TerminateBlock;

   MODULE Equates;

      IMPORT EmitEQU, LabelPtr, ALLOCATE, DEALLOCATE;
      EXPORT EnterEQU, FlushEQUs;

      TYPE
         EQURel = POINTER TO Node;
         Node = RECORD
            left, right: LabelPtr;
            next: EQURel;
         END;

      VAR list: EQURel;

      PROCEDURE EnterEQU(l, r: LabelPtr);
         VAR ptr: EQURel;
      BEGIN
         NEW(ptr);
         NEW(ptr^.left);
         NEW(ptr^.right);
         ptr^.left^ := l^; ptr^.right^ := r^;
         ptr^.next := list;
         list := ptr;
      END EnterEQU;

      PROCEDURE FlushEQUs;
         VAR ptr: EQURel;
      BEGIN
         ptr := list;
         WHILE ptr <> NIL DO
            WITH ptr^ DO
               EmitEQU(left^, right^);
               DISPOSE(left); DISPOSE(right);
               ptr := next;
            END;
            DISPOSE(list);
            list := ptr;
         END;
      END FlushEQUs;

   BEGIN
      list := NIL;
   END Equates;

   MODULE RealConstants;

      FROM MCP4Labels IMPORT LabelPtr;
      FROM MCP4CodeSys IMPORT EmitLabel, EmitDCFValue, EmitPure1, EmitAlign;
      FROM Storage IMPORT ALLOCATE, DEALLOCATE;

      EXPORT EnterReal, FlushReals;

      TYPE
         RealConstPtr = POINTER TO RealNode;
         RealNode =
            RECORD
               label: LabelPtr;
               CASE : BOOLEAN OF
                 TRUE:
                   const: REAL;
               | FALSE:
                   const1, const2: INTEGER;
               END;
               link: RealConstPtr;
            END;

      VAR
         list: RealConstPtr;

      PROCEDURE EnterReal(l: LabelPtr; r: REAL);
         VAR p: RealConstPtr;
      BEGIN
         NEW(p);
         WITH p^ DO
            label := l;
            const := r;
            link := list;
         END;
         list := p;
      END EnterReal;

      PROCEDURE FlushReals;
         TYPE DoubleWord = ARRAY[0..1] OF INTEGER;
         VAR p: RealConstPtr;
             double: DoubleWord;
      BEGIN
         IF list = NIL THEN RETURN END;
         EmitPure1; (* necessary due to as-bug *)
         EmitAlign;
         p := list;
         WHILE p <> NIL DO
            WITH p^ DO
               EmitLabel(label);
               DISPOSE(label);
               double := DoubleWord(const);
               EmitDCFValue(double[0]); EmitDCFValue(double[1]);
               p := link;
            END;
            DISPOSE(list);
            list := p;
         END;
      END FlushReals;

   BEGIN (* MODULE RealConstants *)
      list := NIL;
   END RealConstants;

   MODULE Stack;

      FROM Storage IMPORT DEALLOCATE;
      FROM MCP4Stack IMPORT GetMaxIncTop;
      FROM MCP4CodeSys IMPORT EmitEQUCard, Emit1, Emit2, EmitLabel, EmitExtern;
      FROM MCP4Labels IMPORT LabelPtr, GetLabel;
      FROM MCMnemonics IMPORT Mnemonic;
      FROM MCP4Register IMPORT Reg;
      FROM MCP4Public IMPORT sflag;
      FROM MCP4RTS IMPORT stack;
      EXPORT Check, EmitOffsetLabel;

      VAR
         label: LabelPtr; (* label equ max *)

      (*
       * check for max_offset(top) < limit
       *)

      PROCEDURE Check;
         VAR okay: LabelPtr;
      BEGIN
	 IF NOT sflag THEN
	    GetLabel('I', label);
	    Emit2(MOVE, "%L%r,%r", top, d0);
	    Emit2(SUB, "%L#%l,%r", label^, d0);
	    Emit2(CMP, "%L%r,%r", limit, d0);
	    GetLabel('I', okay);
	    Emit1(BHI, "%l", okay^);
	    EmitExtern(stack);
	    Emit1(JSR, "%l", stack);
	    EmitLabel(okay);
	    DISPOSE(okay);
	 END;
      END Check;

      PROCEDURE EmitOffsetLabel;
         VAR incr: CARDINAL;
      BEGIN
	 IF NOT sflag THEN
            GetMaxIncTop(incr);
	    EmitEQUCard(label^, incr);
	    DISPOSE(label);
	 END;
      END EmitOffsetLabel;

   END Stack;

   PROCEDURE CompilationUnit;
   BEGIN
      IF profile THEN
         EmitExtern(mcount);
      END;
      level := 0;
      GetSymbol;
      Block(NIL);
   END CompilationUnit;

END MCP4Block. 
