(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
IMPLEMENTATION MODULE MCP4CodeSys; (* AFB 8/83, 9/87 *)

   (* output formatting; general principle one call/one line *)
   (* exceptions: symbol table generating pseudo-ops to as   *)
   (*             Emit?'s may append an external-declaration *)

   FROM MCP4Out IMPORT Write, WriteLn, WriteString, TermOut;
   FROM MCP4Labels IMPORT GetLabel, Label, LabelPtr;
   FROM MCP4Register IMPORT FloatReg, Reg, a7, RegSet;
   FROM MCP4Global IMPORT Assert;
   FROM Conversions IMPORT ConvertInteger, ConvertCardinal;
   FROM MCMnemonics IMPORT Mnemonic, Mnem;
   FROM MCP4Public IMPORT Sflag, mc68881;
   FROM MCP4AttributSys IMPORT Attribut, AtMode;
   FROM MCBase IMPORT Idptr, mainmodp, stringmax, charptr, oneword, doubleword,
      Idclass;
   FROM MCP4Block IMPORT EnterReal;
   FROM MCP4RTS IMPORT global, NFUN, NSLINE, NSO, NENTRY;
   FROM Storage IMPORT DEALLOCATE;
   FROM SYSTEM IMPORT BYTE, ADR, ADDRESS;
   IMPORT MCP4Block;

   CONST
      tab = 11C;
      BufSize = 40;
      maxarg = 10;

   TYPE
      Segment = (pure,(* pure1,*) impur (*, bss *) );
      ArgList = ARRAY[0..maxarg-1] OF ADDRESS;
      Text = ARRAY[0..11] OF CHAR; (* should be sufficient for maxcard *)
      RegRange = [MIN(Reg)..MAX(Reg)];
      FloatRegRange = [MIN(FloatReg)..MAX(FloatReg)];

   VAR
      i: CARDINAL;
      RegStr: ARRAY RegRange OF ARRAY [0..3] OF CHAR;
      FloatRegStr: ARRAY FloatRegRange OF ARRAY [0..3] OF CHAR;
      ActSeg: Segment;
      ComBuf: ARRAY [ 0..BufSize-1 ] OF CHAR;
      AppCom: BOOLEAN;   (* comment to be appended? *)
      emptyString: ARRAY [ 0..0 ] OF CHAR; (* = "" *)
      (* data structure for Emit?'s: *)
      args: ArgList;     (* stack of arguments *)
      arghigh: ARRAY [0..maxarg-1] OF CARDINAL; (* HIGH(args[]) *)
      argi: CARDINAL;    (* index of next arg to be pushed *)
      atsize: CARDINAL;  (* note typtr^.size of first %a-argument *)

   PROCEDURE WriteInt(i: INTEGER);
      VAR text: Text;
   BEGIN
      ConvertInteger(i, 1, text); WriteString(text);
   END WriteInt;

   PROCEDURE WriteCard(c: CARDINAL);
      VAR text: Text;
   BEGIN
      ConvertCardinal(c, 1, text); WriteString(text);
   END WriteCard;

   PROCEDURE PushArg(VAR a: ARRAY OF BYTE);
      (* push next arg of Emit? onto stack 'args' *)
      TYPE AtPtr = POINTER TO Attribut;
      VAR atptr: AtPtr; newsize: CARDINAL;
   BEGIN
      Assert(argi < maxarg);
      args[argi] := ADR(a); arghigh[argi] := HIGH(a); INC(argi);
      IF SIZE(a) = SIZE(Attribut) THEN
	 atptr := AtPtr(ADR(a));
	 IF atptr^.mode = floatLoadedMod THEN
	    newsize := 3 * oneword; (* extended format *)
	 ELSE
	    newsize := atptr^.typtr^.size;
	 END;
	 (* sizes can be different: if one of the operands *)
	 (* is a string constant of length 1 *)
	 IF (atsize = 0) OR (newsize < atsize) THEN
	    atsize := newsize;
	 END;
      END;
   END PushArg;

   PROCEDURE Print(OP: Mnemonic; VAR format: ARRAY OF CHAR);
      TYPE
	 CardPtr = POINTER TO CARDINAL;
      VAR
	 index: CARDINAL;
	 tabprinted: BOOLEAN;
	 immed: BOOLEAN; (* if on: print, if possible, immediate value *)
	 comma: BOOLEAN; (* conditional comma (%,) *)
	 emitextern: BOOLEAN; idptr: Idptr;
	 cptr: CardPtr;

      PROCEDURE PopReg;
	 TYPE RegPtr = POINTER TO Reg;
	 VAR rptr: RegPtr;
      BEGIN
	 Assert(argi > 0); DEC(argi);
	 rptr := RegPtr(args[argi]);
	 IF comma AND (rptr^ <> illegal) THEN
	    Write(',');
	 END;
	 IF rptr^ <> illegal THEN
	    WriteString(RegStr[rptr^]);	(* register illegal not to write *)
	 END;
      END PopReg;

      PROCEDURE PopFloatReg;
	 TYPE FloatRegPtr = POINTER TO FloatReg;
	 VAR frptr: FloatRegPtr;
      BEGIN
	 Assert(argi > 0); DEC(argi);
	 frptr := FloatRegPtr(args[argi]);
	 WriteString(FloatRegStr[frptr^]);
      END PopFloatReg;

      PROCEDURE PopLabel;
	 TYPE CharPtr = POINTER TO ARRAY[0..127] OF CHAR;
	 VAR cp: CharPtr; i: CARDINAL;
      BEGIN
	 Assert(argi > 0); DEC(argi);
	 cp := CharPtr(args[argi]);
	 i := 0;
	 Assert(cp^[0] <> 0C);
	 IF immed THEN Write('#') END;
	 WHILE (i <= arghigh[argi]) AND (cp^[i] <> 0C) DO
	    Write(cp^[i]);
	    INC(i);
	 END;
      END PopLabel;

      PROCEDURE PopInteger;
	 TYPE IntegerPtr = POINTER TO INTEGER;
	 VAR iptr: IntegerPtr; text: Text;
      BEGIN
	 Assert(argi > 0); DEC(argi);
	 iptr := IntegerPtr(args[argi]);
	 IF NOT comma OR (iptr^ <> 0) THEN
	    IF comma THEN
	       Write(',');
	    END;
	    WriteInt(iptr^);
	 END;
      END PopInteger;

      PROCEDURE PopOffset(offset: INTEGER);
	 TYPE IntegerPtr = POINTER TO INTEGER;
	 VAR iptr: IntegerPtr;
      BEGIN
	 Assert(argi > 0); DEC(argi);
	 iptr := IntegerPtr(args[argi]);
	 WriteInt(offset - iptr^);
      END PopOffset;

      PROCEDURE PopCardinal;
	 TYPE CardinalPtr = POINTER TO CARDINAL;
	 VAR cptr: CardinalPtr; text: Text;
      BEGIN
	 Assert(argi > 0); DEC(argi);
	 cptr := CardinalPtr(args[argi]);
	 IF NOT comma OR (cptr^ <> 0) THEN
	    IF comma THEN
	       Write(',');
	    END;
	    WriteCard(cptr^);
	 END;
      END PopCardinal;

      PROCEDURE PopAt;
	 CONST
	    scales = {1, 2, 4, 8}; (* legal scale values *)
	 TYPE
	    AtPtr = POINTER TO Attribut;
	    CharPtr = POINTER TO ARRAY[0..stringmax-1] OF CHAR;
	 VAR
	    atptr: AtPtr;
	    cp: CharPtr;
	    constLabel: LabelPtr;
	    an, ri: Reg;
	    appendlong: BOOLEAN;
      BEGIN
	 Assert(argi > 0); DEC(argi);
	 atptr := AtPtr(args[argi]);
	 CTab;
	 WITH atptr^ DO
	    CASE mode OF
	    | loadedMod:
		  WriteReg(loadReg);
	    | floatLoadedMod:
		  WriteFloatReg(floatLoadReg);
	    | addrLoadedMod:
		  WriteReg(addrReg); Write('@');
	    | constantMod:
		  Write('#'); WriteInt(iValue);
	    | doubleConstMod:
		  GetLabel('r', constLabel);
		  EnterReal(constLabel, Real);
		  WriteString(constLabel^);
	    | procedureMod:
		  emitextern := TRUE; idptr := procPtr;
		  WITH procPtr^ DO
		     IF immed THEN Write('#') END;
		     Write('_');
		     WriteString(globmodp^.identifier);
		     WriteString("_P"); WriteCard(procnum);
		  END;
	    | stringConstMod:
		  IF immed AND (typtr^.ixp^.max = 0) THEN
		     (* assignment compatible with CHAR *)
		     cp := CharPtr(strgPtr^.valentry);
		     Write('#'); WriteCard(ORD(cp^[0]));
		     IF NOT readonly THEN
			typtr := charptr;
			mode := constantMod;
			value := ORD(cp^[0]);
		     END;
		  ELSE
		     WITH strgPtr^ DO
			IF label = 0 THEN
			   GetLabel('s', constLabel);
			   label := CARDINAL(constLabel);
			ELSE
			   constLabel := LabelPtr(label);
			END;
		     END;
		     WriteString(constLabel^);
		  END;
	    | stackMod:
		  Assert(NOT immed OR (size <= doubleword DIV oneword));
		  WriteReg(base);
		  Write('@');Write('(');
		  WriteInt(INTEGER(MCP4Block.offset) - INTEGER(offset));
		  (* WriteString(":l"); GNU *)
		  Write(')');
		  IF indirect THEN
		     WriteString("@(0)");
		  END;	   
	    | setConstMod:
		  WITH setPtr^ DO
		     IF LabelPtr(label) = NIL THEN
			GetLabel('S', constLabel);
			label := constLabel;
		     ELSE
			constLabel := label;
		     END;
		     WriteString(constLabel^);
		  END;
	    | globalMod, localMod, absolutMod, indexMod, externalMod:
		  appendlong := FALSE;
		  IF mode = localMod THEN
		     an := base;
		     ri := addrReg;
		     IF ri = illegal THEN scale := 1; END;
		  ELSIF memindex AND post THEN
		     an := addrReg;
		     ri := addr2Reg;
		  ELSIF addrReg IN RegSet{a0..a7} THEN
		     an := addrReg;
		     ri := addr2Reg;
		  ELSIF (addrReg IN RegSet{d0..d7}) AND
			(addr2Reg = illegal) THEN
		     an := illegal;
		     ri := addrReg;
		  ELSIF (scale = 1) AND (addrReg IN RegSet{d0..d7}) AND
			 (addr2Reg IN RegSet{a0..a7}) THEN
		     an := addr2Reg;
		     ri := addrReg;
		  ELSE
		     an := addrReg;
		     ri := addr2Reg;
		  END;
		  IF (an <> illegal) AND (ri = illegal) AND (scale > 1) THEN
		     ri := an;
		     an := illegal;
		  END;
		  IF memindex OR (mode = localMod) OR (addrReg <> illegal) THEN
		     Assert( an >= a0);
		     IF an <> illegal THEN
		        WriteReg(an);
		     END;
		     WriteString('@');
		     Write('(');
		  END(*IF*);
		  IF mode = globalMod THEN
		     WriteString("G_AREA");
		     IF addr > 0 THEN Write('+'); WriteCard(addr) END;
		     appendlong := TRUE;
		  ELSIF mode = externalMod THEN
		     emitextern := TRUE; idptr := modPtr;
		     Write('_');
		     WriteString(modPtr^.identifier); WriteString("_V0");
		     IF addr > 0 THEN Write('+'); WriteCard(addr) END;
		     appendlong := TRUE;
		  ELSE
		     IF mode = localMod THEN
			WriteInt(INTEGER(MCP4Block.offset) - INTEGER(addr));
		     ELSE
			WriteInt(INTEGER(addr));
		     END;
		  END;
		  IF appendlong THEN
		     Write(':');
		     Write('l');
		  END;
		  IF memindex OR (an <> illegal) OR (ri <> illegal) THEN
		     IF memindex AND post THEN WriteString(")@(");
		        WriteCard(od);
			IF ri <> illegal THEN
		           Write(',');
		           WriteReg(ri);
		           WriteString(":l");
			   Write(':');
			   WriteCard(scale);
		           Assert(scale IN scales);
			END(*IF*);
		     ELSIF memindex AND NOT post THEN Write(',');
			IF ri <> illegal THEN
		           WriteReg(ri);
		           WriteString(":l");
			   Write(':');
			   WriteCard(scale);
		           Assert(scale IN scales);
			END(*IF*);
		        WriteString(")@(");
		        WriteCard(od);
		        (* WriteString(":l"); GNU *)
		     ELSE
			IF ri <> illegal THEN
		           Write(',');
		           WriteReg(ri);
		           WriteString(":l");
			   Write(':');
			   WriteCard(scale);
		           Assert(scale IN scales);
		        END(*IF*);
		     END(*IF*);
		     Write(')');
		  END(*IF*);
	       END(*CASE*);
	 END(*WITH*);
      END PopAt;

      PROCEDURE Tab;
      BEGIN
	 Assert(NOT tabprinted);
	 Write(tab);
	 tabprinted := TRUE;
      END Tab;

      PROCEDURE CTab;
      BEGIN
	 IF NOT tabprinted THEN
	    Write(tab);
	    tabprinted := TRUE;
	 END;
      END CTab;

   BEGIN
      index := 0; tabprinted := FALSE; emitextern := FALSE;
      Write(tab); WriteMnem(OP);
      WHILE ((index <= HIGH(format)) AND (format[index] <> 0C)) DO
	 IF format[index] = '%' THEN
	    INC(index);
	    IF format[index] = '#' THEN
	       immed := TRUE; INC(index);
	    ELSE
	       immed := FALSE;
	    END;
	    IF format[index] = ',' THEN
	       comma := TRUE; INC(index);
	    ELSE
	       comma := FALSE;
	    END;
	    CASE format[index] OF
	    | '%':	;
	    | 'A': Assert(atsize > 0);
		   CASE atsize OF
		   | 1:  Write('b');
		   | 2:  Write('w');
		   | 4:  Write('l');
		   | 8:  Write('d');
		   | 12: Write('x');
		   ELSE
		      Assert(TRUE);
		   END;
		   atsize := 0;
	    | 'W', 'B', 'L', 'D', 'X':
	           Write(CHR(ORD(format[index])+ORD('a')-ORD('A'))); Tab;
	    | 'a': PopAt;
	    | 'c': CTab; Write('#'); PopCardinal;
	    | 'f': CTab; PopFloatReg;
	    | 'i': CTab; Write('#'); PopInteger;
	    | 'l': CTab; PopLabel;
	    | 'o': CTab; PopOffset(MCP4Block.offset);
	    | 'O': CTab; Assert(argi > 0); DEC(argi);
	           cptr := CardPtr(args[argi]);
		   PopOffset(MCP4Block.offsetstack[MCP4Block.level-cptr^]);
	    | 'r': CTab; PopReg;
	    | 'C': CTab; PopCardinal;
	    | 'I': CTab; PopInteger;
	    ELSE
	       Assert(TRUE);
	    END;
	 ELSE
	    CTab; Write(format[index]);
	 END;
	 INC(index);
      END;
      EOL;
      IF emitextern THEN
	 WITH idptr^ DO
	    IF klass = mods THEN
	       EmitVarExtern(identifier);
	    ELSE
	       EmitProcExtern(globmodp^.identifier, procnum);
	    END;
	 END;
      END;
   END Print;

   PROCEDURE EOL;
   BEGIN
      IF Sflag AND AppCom THEN
	 Write(tab);
	 Write("|");
	 WriteString(ComBuf);
	 AppCom := FALSE;
      END;
      WriteLn;
      atsize := 0;
      Assert(argi = 0);
   END EOL;

   PROCEDURE Emit(OP: Mnemonic; format: ARRAY OF CHAR);
      (* the format string will be printed as given with following exceptions *)
      (* %B  print OPB *)
      (* %W  print OPW *)
      (* %L  print OPL *)
      (* %A  print size attribute of OP according to typtr^.size of the *)
      (*     first given attribute *)
      (* %r  take next arg as register and print its name *)
      (*     a comma (%,r) indicates that nothing will be printed if *)
      (*     the given register is illegal( on SUN always nothing is *)
      (*     printed), else it's equivalent to ,%r *)
      (* %o  take next arg as local offset (as calculated by pass 2) *)
      (* %O  take the next two args as plevel-difference and local offset *)
      (* %f  take next arg as floating point register and print its name *)
      (* %a  take next arg as attribut (see MCP4AttributSys) and print *)
      (*     according effective address *)
      (* %#a like %a, but tries to give an immediate value *)
      (*     this includes type casting from strings to single chars *)
      (* %l  take next arg as label (ARRAY OF CHAR) and print it *)
      (* %i  take next arg as integer value and print it in decimal *)
      (* %c  take next arg as cardinal value and print it in decimal *)
      (*     likewise to %,r it's possible to insert a comma (%,i and %,c) *)
      (* %%  print % *)
   BEGIN
      Print(OP, format);
   END Emit;

   PROCEDURE Emit1(OP: Mnemonic; format: ARRAY OF CHAR;
				 arg1: ARRAY OF BYTE);
   BEGIN
      PushArg(arg1);
      Print(OP, format);
   END Emit1;

   PROCEDURE Emit2(OP: Mnemonic; format: ARRAY OF CHAR;
				 arg1: ARRAY OF BYTE;
				 arg2: ARRAY OF BYTE);
   BEGIN
      PushArg(arg2);
      PushArg(arg1);
      Print(OP, format);
   END Emit2;

   PROCEDURE Emit3(OP: Mnemonic; format: ARRAY OF CHAR;
				 arg1: ARRAY OF BYTE;
				 arg2: ARRAY OF BYTE;
				 arg3: ARRAY OF BYTE);
   BEGIN
      PushArg(arg3);
      PushArg(arg2);
      PushArg(arg1);
      Print(OP, format);
   END Emit3;

   PROCEDURE Emit4(OP: Mnemonic; format: ARRAY OF CHAR;
				 arg1: ARRAY OF BYTE;
				 arg2: ARRAY OF BYTE;
				 arg3: ARRAY OF BYTE;
				 arg4: ARRAY OF BYTE);
   BEGIN
      PushArg(arg4);
      PushArg(arg3);
      PushArg(arg2);
      PushArg(arg1);
      Print(OP, format);
   END Emit4;

   PROCEDURE Emit5(OP: Mnemonic; format: ARRAY OF CHAR;
				 arg1: ARRAY OF BYTE;
				 arg2: ARRAY OF BYTE;
				 arg3: ARRAY OF BYTE;
				 arg4: ARRAY OF BYTE;
				 arg5: ARRAY OF BYTE);
   BEGIN
      PushArg(arg5);
      PushArg(arg4);
      PushArg(arg3);
      PushArg(arg2);
      PushArg(arg1);
      Print(OP, format);
   END Emit5;

   PROCEDURE Emit6(OP: Mnemonic; format: ARRAY OF CHAR;
				 arg1: ARRAY OF BYTE;
				 arg2: ARRAY OF BYTE;
				 arg3: ARRAY OF BYTE;
				 arg4: ARRAY OF BYTE;
				 arg5: ARRAY OF BYTE;
				 arg6: ARRAY OF BYTE);
   BEGIN
      PushArg(arg6);
      PushArg(arg5);
      PushArg(arg4);
      PushArg(arg3);
      PushArg(arg2);
      PushArg(arg1);
      Print(OP, format);
   END Emit6;

   PROCEDURE EmitLabel(l: LabelPtr);
   BEGIN
      WriteString(l^); Write(":"); EOL
   END EmitLabel;

   PROCEDURE EmitEQU(str1,str2: ARRAY OF CHAR);
   BEGIN
      IF (str2[0] = ".") AND (str2[1] = 0C) THEN
	 WriteString(str1); Write(":");
      ELSE
	 Write(tab);
	 WriteString(str1);Write(tab); Write("=");Write(tab); WriteString(str2);
      END;
      EOL
   END EmitEQU;

   PROCEDURE EmitEQUCard(str: ARRAY OF CHAR; c: CARDINAL);
      VAR field : ARRAY [ 0..11 ] OF CHAR;
   BEGIN
      ConvertCardinal(c,1,field);
      EmitEQU(str,field)
   END EmitEQUCard;

   PROCEDURE EmitEQUInt(str: ARRAY OF CHAR; i: INTEGER);
      VAR field : ARRAY [ 0..11 ] OF CHAR;
   BEGIN
      ConvertInteger(i, 1, field);
      EmitEQU(str, field)
   END EmitEQUInt;

   PROCEDURE EmitEntry(name: ARRAY OF CHAR);
   BEGIN
      Write(tab); WriteString(".globl"); Write(tab); WriteString(name);
      EOL;
   END EmitEntry;

   PROCEDURE EmitExtern(name: ARRAY OF CHAR);
   BEGIN
      (* (* all undefined symbols are extern *)
      Write(tab); WriteString("extrn"); Write(tab); WriteString(name);
      WriteLn
      *)
   END EmitExtern;

   PROCEDURE EmitEND;
   BEGIN
      TermOut;
   END EmitEND;

   PROCEDURE EmitComment(str: ARRAY OF CHAR);
   BEGIN
      IF NOT Sflag THEN RETURN END;
      Write('#'); Write(tab); WriteString(str); WriteLn;
   END EmitComment;

   PROCEDURE AppendComment(str: ARRAY OF CHAR);
      VAR indx : CARDINAL;
   BEGIN
      IF NOT Sflag THEN RETURN END;
      AppCom := TRUE;
      indx := 0;
      WHILE (indx < BufSize) AND (indx <= HIGH(str)) AND (str[indx] <> 0C) DO
	 ComBuf[indx] := str[indx];
	 INC(indx);
      END;
      IF (indx < BufSize) THEN
	 ComBuf[indx] := 0C;
      END;
   END AppendComment;

   PROCEDURE AppendValue(str1: ARRAY OF CHAR; val: CARDINAL;
			 str2: ARRAY OF CHAR);
      VAR field: ARRAY[1..9] OF CHAR;
	  indx, indx2 : CARDINAL;
   BEGIN
      IF NOT Sflag THEN RETURN END;
      AppCom := TRUE;
      indx := 0;
      WHILE (indx < BufSize) AND (indx <= HIGH(str1)) AND (str1[indx] <> 0C) DO
	 ComBuf[indx] := str1[indx];
	 INC(indx);
      END;
      ConvertCardinal(val,1,field); 
      indx2 := 1;
      WHILE (indx < BufSize) AND (indx2 <= 8) AND (field[indx2] <> 0C) DO
	 ComBuf[indx] := field[indx2];
	 INC(indx);
	 INC(indx2);
      END;
      indx2 := 0;
      WHILE (indx < BufSize) AND (indx2 <= HIGH(str2)) AND (str2[indx2] <> 0C) DO
	 ComBuf[indx] := str2[indx2];
	 INC(indx);
	 INC(indx2);
      END;
      IF (indx < BufSize) THEN
	 ComBuf[indx] := 0C;
      END;
   END AppendValue;

   PROCEDURE EmitBSS(l: LabelPtr; af: CARDINAL); (* af = Anzahl Fullwords *)
      VAR field : ARRAY[0..11] OF CHAR;
   BEGIN
      Write(tab); WriteString(".lcomm"); Write(tab);
      WriteString(l^); Write(",");
      ConvertCardinal(af*oneword,1,field); WriteString(field); EOL;
   END EmitBSS;
   
   PROCEDURE EmitDCF(const: ARRAY OF CHAR);
   BEGIN
      Write(tab); WriteString(".long"); Write(tab); WriteString(const); EOL
   END EmitDCF;

   PROCEDURE EmitDC(const: ARRAY OF CHAR);
   BEGIN
      Write(tab); WriteString(".word"); Write(tab); WriteString(const);
      EOL
   END EmitDC;

   PROCEDURE EmitDS(size: CARDINAL);
      VAR i : CARDINAL;
   BEGIN
      EmitImpure;
      Write(tab); WriteString(".byte"); Write(tab); Write("0");
      FOR i := 1 TO (size -1) DO
	 WriteString(",0");
      END(*FOR*);
      EOL
   END EmitDS;

   PROCEDURE EmitDCFValue(value: INTEGER);
      VAR field: ARRAY [0..11] OF CHAR;
   BEGIN
      Write(tab); WriteString(".long"); Write(tab);
      ConvertInteger(value,1,field);
      WriteString(field);
      EOL
   END EmitDCFValue;

   PROCEDURE EmitString(l: LabelPtr; str: CARDINAL);
      TYPE StringPtr = POINTER TO ARRAY [0..stringmax-1] OF CHAR;
      VAR field: ARRAY[0..2] OF CHAR;
	  ptr: StringPtr;
   BEGIN
      EmitPure;
      EmitAlign;
      EmitLabel(l);
      ptr := StringPtr(str);
      i := 0;
      REPEAT
         ConvertCardinal(ORD(ptr^[i]),1,field);
         IF i MOD 10 = 0 THEN
            IF i > 0 THEN
               EOL
            END;
            Write(tab); WriteString(".byte"); Write(tab);
         ELSE
            Write(',');
         END;
         INC(i);
         WriteString(field);
      UNTIL ptr^[i-1] = 0C;
      EOL;
   END EmitString;

   PROCEDURE EmitPure;
   BEGIN
      IF ActSeg <> pure THEN
         EmitComment("  ");
         Write(tab); WriteString(".text"); EOL;
         ActSeg := pure;
      END
   END EmitPure;

   PROCEDURE EmitPure1;
   BEGIN
      (*IF ActSeg <> pure1 THEN
         EmitComment("  ");
         Write(tab); WriteString("text 1"); EOL;
         ActSeg := pure1;
      END*)
   END EmitPure1;

   PROCEDURE EmitImpure;
   BEGIN
      IF ActSeg <> impur THEN
         EmitComment("  ");
         Write(tab); WriteString(".data"); EOL;
         ActSeg := impur;
      END
   END EmitImpure;

   PROCEDURE EmitAlign;
   BEGIN
      Write(tab); WriteString(".even");
      EOL
   END EmitAlign;

   PROCEDURE EmitFileName(filename: ARRAY OF CHAR);
      VAR
         i: CARDINAL;
	 l: LabelPtr;
	 str: ARRAY [0..3] OF CHAR;
   BEGIN
      FOR i := 0 TO HIGH(filename) DO
	 IF filename[i] = '"' THEN
	    filename[i] := " ";
	 END;
      END;
      Write(tab); WriteString(".stabs"); Write(tab);
      WriteString('"'); WriteString(filename); WriteString('"');
      Write(','); WriteString(NSO); Write(','); Write('0');
      Write(','); Write('0');
      Write(','); GetLabel( 'L', l);
      WriteString(l^);
      EOL;
      EmitLabel(l );
      (*
      (* identification for what (sccs) and ident (rcs) *)
      Write(tab);
      WriteString("db"); Write(tab);
      WriteString("c'$Source:@(#)");
      WriteString(filename);
      WriteString("$',0"); EOL;
      *)
   END EmitFileName;

   PROCEDURE EmitExportedProcLabel(num: CARDINAL);
      VAR field: ARRAY[0..11] OF CHAR;
   BEGIN
      Write('_');
      WriteString(mainmodp^.identifier); WriteString("_P");
      ConvertCardinal(num, 1, field);
      WriteString(field); Write(":"); EOL;
      Write(tab); WriteString(".globl"); Write(tab);
      Write('_'); WriteString(mainmodp^.identifier); WriteString("_P");
      WriteString(field); EOL;
   END EmitExportedProcLabel;

   PROCEDURE EmitProcExtern(modname: ARRAY OF CHAR; procnum: CARDINAL);
      VAR field: ARRAY[0..11] OF CHAR;
   BEGIN
      Write(tab); WriteString(".globl"); Write(tab);
      Write('_'); WriteString(modname);
      WriteString("_P"); ConvertCardinal(procnum, 1, field);
      WriteString(field); EOL
   END EmitProcExtern;

   PROCEDURE EmitVarEntry(modname: ARRAY OF CHAR);
   BEGIN
      Write(tab);Write('_');
      WriteString(modname); WriteString("_V0"); Write(tab);
      Write('='); Write(tab); WriteString(global); EOL;
      Write(tab); WriteString(".globl"); Write(tab);
      Write('_'); WriteString(modname); WriteString("_V0"); EOL;
   END EmitVarEntry;

   PROCEDURE EmitVarExtern(modname: ARRAY OF CHAR);
   BEGIN
      Write(tab); WriteString(".globl"); Write(tab); Write('_');
      WriteString(modname); WriteString("_V0");
      EOL
   END EmitVarExtern;

   PROCEDURE EmitLn(line: CARDINAL);
      VAR field: ARRAY[0..11] OF CHAR;
	     l : LabelPtr;
   BEGIN
      Write(tab); WriteString(".stabn"); Write(tab);
      WriteString(NSLINE);Write(",");
      Write("0"); Write(",");
      ConvertCardinal(line, 1, field);
      WriteString(field); Write(",");
      GetLabel('Z', l);
      WriteString(l^); EOL;
      EmitLabel(l);
   END EmitLn;

(*  generate breakpoint marks for mdb  *)
   PROCEDURE EmitDef(symbol: ARRAY OF CHAR);
      VAR 
	     l : LabelPtr;
   BEGIN
      Write(tab); WriteString(".stabn"); 
      Write(tab);WriteString(NENTRY);
      IF symbol[0] = 'e' THEN
      WriteString(",0,1,");
         GetLabel('e', l);	(*end procedure *)
      ELSE
      WriteString(",0,0,");
         GetLabel('b', l);	(*begin procedure *)
      END(*IF*);
      WriteString(l^); EOL;
      EmitLabel(l);
   END EmitDef;

   PROCEDURE EmitDefPV(opt: ARRAY OF CHAR; line : INTEGER;
		       modname: ARRAY OF CHAR; PV: CHAR; num: CARDINAL);
      VAR field: ARRAY[0..11] OF CHAR;
   BEGIN
      IF NOT ((opt[0] = 'd') AND (opt[1] = 'e') AND (opt[2] = 'f')) THEN
	 Write(';');
      END;
      Write(tab); WriteString(".stabs"); Write(tab);
      Write('"'); Write('_');WriteString(modname);
      Write('_'); Write(PV);
      ConvertCardinal(num, 1, field);
      WriteString(field); WriteString('",');
      WriteString(NFUN); Write(',');
      WriteString("0,");
      ConvertInteger( line, 1 ,field); 	(*linenumber for mdb *)
      WriteString(field); Write(',');
      Write('_'); WriteString(modname); Write('_');
      Write(PV);
      ConvertCardinal(num, 1, field);
      WriteString(field); EOL;
   END EmitDefPV;


   PROCEDURE WriteMnem(OP: Mnemonic);
   BEGIN
      WriteString(Mnem[OP]);
   END WriteMnem;

   PROCEDURE WriteReg(r: Reg);
   BEGIN
      WriteString(RegStr[r]);
   END WriteReg;

   PROCEDURE WriteFloatReg(r: FloatReg);
   BEGIN
      Assert(mc68881);
      WriteString(FloatRegStr[r]);
   END WriteFloatReg;

   PROCEDURE InitRegStr;
      VAR i: CARDINAL;
   BEGIN
      RegStr[d0] := "d0";
      RegStr[d1] := "d1";
      RegStr[d2] := "d2";
      RegStr[d3] := "d3";
      RegStr[d4] := "d4";
      RegStr[d5] := "d5";
      RegStr[d6] := "d6";
      RegStr[d7] := "d7";
      RegStr[a0] := "a0";
      RegStr[a1] := "a1";
      RegStr[a2] := "a2";
      RegStr[a3] := "a3";
      RegStr[a4] := "a4";
      RegStr[limit] := "a5";
      RegStr[base] := "a6";	(* Nixdorf: fp *)
      RegStr[top] := "sp";
      RegStr[illegal] := "za0"; (* doesn't exist on SUN, is never printed *)
      FloatRegStr[fr0] := "fp0";
      FloatRegStr[fr1] := "fp1";
      FloatRegStr[fr2] := "fp2";
      FloatRegStr[fr3] := "fp3";
      FloatRegStr[fr4] := "fp4";
      FloatRegStr[fr5] := "fp5";
      FloatRegStr[fr6] := "fp6";
      FloatRegStr[fr7] := "fp7";
   END InitRegStr;

BEGIN
   star[0] := "."; star[1] := 0C;
   emptyString[0] := 0C;
   ActSeg := impur;
   AppCom := FALSE;
   EmitPure;
   InitRegStr;
   argi := 0;
END MCP4CodeSys.
