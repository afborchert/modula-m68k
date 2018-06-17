(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)

DEFINITION MODULE MCP1Public; (* AFB 3/84 *)

   FROM MCBase IMPORT Spellix;

   CONST
      FileNameLength = 128;
   TYPE
      FileName = ARRAY[0..FileNameLength-1] OF CHAR;
      String14 = ARRAY[0..13] OF CHAR;
      SymNamePtr = POINTER TO SymNameNode;
      SymNameNode =
	 RECORD
	    symName: FileName;
	    moduleName: Spellix;
	    link: SymNamePtr;
	 END;
   VAR 
      ascName: FileName;                   (* identifier table file *)
      srcName: FileName;                   (* compiler input file *)
      il1Name: FileName;		    (* interpass file *)
      symNames: SymNamePtr;
      listing: BOOLEAN;

      ErrorsFound: BOOLEAN;
      SymFilesMissing: BOOLEAN;

END MCP1Public. 
