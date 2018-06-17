(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
DEFINITION MODULE MCP4CodeSys; (* AFB 8/83 *)

   FROM MCP4Register IMPORT Reg, FloatReg, RegSet;
   FROM MCP4Labels IMPORT LabelPtr;
   FROM MCMnemonics IMPORT Mnemonic;
   FROM SYSTEM IMPORT BYTE;

   VAR
      star : ARRAY [0..1] OF CHAR;

   PROCEDURE Emit(OP: Mnemonic; format: ARRAY OF CHAR);
      (* the format string will be printed as given with following exceptions *)
      (* %B  print OPB *)
      (* %W  print OPW *)
      (* %L  print OPL *)
      (* %A  print size attribute of OP according to typtr^.size of the *)
      (*     first given attribute *)
      (* %r  take next arg as register and print its name *)
      (*     a comma (%,r) indicates that nothing will be printed if *)
      (*     the given register is illegal(on SUN always nothing is *)
      (*     printed ), else it's equivalent to ,%r *)
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

   PROCEDURE Emit1(OP: Mnemonic; format: ARRAY OF CHAR;
				 arg1: ARRAY OF BYTE);

   PROCEDURE Emit2(OP: Mnemonic; format: ARRAY OF CHAR;
				 arg1: ARRAY OF BYTE;
				 arg2: ARRAY OF BYTE);

   PROCEDURE Emit3(OP: Mnemonic; format: ARRAY OF CHAR;
				 arg1: ARRAY OF BYTE;
				 arg2: ARRAY OF BYTE;
				 arg3: ARRAY OF BYTE);

   PROCEDURE Emit4(OP: Mnemonic; format: ARRAY OF CHAR;
				 arg1: ARRAY OF BYTE;
				 arg2: ARRAY OF BYTE;
				 arg3: ARRAY OF BYTE;
				 arg4: ARRAY OF BYTE);

   PROCEDURE Emit5(OP: Mnemonic; format: ARRAY OF CHAR;
				 arg1: ARRAY OF BYTE;
				 arg2: ARRAY OF BYTE;
				 arg3: ARRAY OF BYTE;
				 arg4: ARRAY OF BYTE;
				 arg5: ARRAY OF BYTE);

   PROCEDURE Emit6(OP: Mnemonic; format: ARRAY OF CHAR;
				 arg1: ARRAY OF BYTE;
				 arg2: ARRAY OF BYTE;
				 arg3: ARRAY OF BYTE;
				 arg4: ARRAY OF BYTE;
				 arg5: ARRAY OF BYTE;
				 arg6: ARRAY OF BYTE);

   PROCEDURE EmitLabel(l: LabelPtr);

   PROCEDURE EmitEQU(str1,str2: ARRAY OF CHAR);

   PROCEDURE EmitEQUCard(str : ARRAY OF CHAR; c: CARDINAL);

   PROCEDURE EmitEQUInt(str: ARRAY OF CHAR; i: INTEGER);

   PROCEDURE EmitEND;

   PROCEDURE EmitComment(str: ARRAY OF CHAR);

   PROCEDURE EmitDCF(const: ARRAY OF CHAR);

   PROCEDURE EmitDC(const: ARRAY OF CHAR);

   PROCEDURE EmitDS(size: CARDINAL);

   PROCEDURE EmitDCFValue(value: INTEGER);

   PROCEDURE EmitBSS(l: LabelPtr; af: CARDINAL);

   PROCEDURE EmitPure;

   PROCEDURE EmitPure1;

   PROCEDURE EmitImpure;

   PROCEDURE EmitAlign;

   PROCEDURE EmitEntry(name: ARRAY OF CHAR);

   PROCEDURE EmitVarEntry(modname: ARRAY OF CHAR);

   PROCEDURE EmitExtern(name: ARRAY OF CHAR);

   PROCEDURE EmitString(l: LabelPtr; str: CARDINAL); (* see Stringval ! *)

   PROCEDURE AppendComment(str: ARRAY OF CHAR);

   PROCEDURE AppendValue(str1: ARRAY OF CHAR; val: CARDINAL; str2: ARRAY OF CHAR);

   (* assembler directives for the symbol table: *)

   PROCEDURE EmitFileName(filename: ARRAY OF CHAR);

   PROCEDURE EmitExportedProcLabel(num: CARDINAL);

   PROCEDURE EmitProcExtern(modname: ARRAY OF CHAR; procnum: CARDINAL);

   PROCEDURE EmitLn(line: CARDINAL);


   PROCEDURE EmitDef(symbol: ARRAY OF CHAR);

   PROCEDURE EmitVarExtern(modname: ARRAY OF CHAR);

   PROCEDURE EmitDefPV(opt: ARRAY OF CHAR; line: INTEGER;
   			modname: ARRAY OF CHAR; PV: CHAR; num: CARDINAL);

END MCP4CodeSys.
