(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
DEFINITION MODULE MCP4Block; (* AFB 8/83 *)

   FROM MCBase IMPORT
      Idptr, BitsPerWord, doubleword, procmarkspace;
   FROM MCP4Labels IMPORT LabelPtr;

   CONST
      maxlevel = BitsPerWord; (* see MCP2Ident.m2 and MCP3Ident.m2 *)
   VAR
      level : CARDINAL; (* current level *)
      blockNptr : Idptr;
      (* offset to base to get the address of the local *)
      (* activation record; use "(%o,%r)",addr,base *)
      offset: CARDINAL; (* current offset *)
      (* offsetstack[level] = offset *)
      offsetstack : ARRAY[0..maxlevel-1] OF CARDINAL;

   PROCEDURE Block(fnptr: Idptr);

   PROCEDURE EnterEQU(l, r: LabelPtr);

   PROCEDURE EnterReal(l: LabelPtr; r: REAL);

   PROCEDURE CompilationUnit;

END MCP4Block.
