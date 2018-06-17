(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
DEFINITION MODULE MCP4Scanner;          (* AFB 8/83 *)

   FROM MCBase IMPORT Symbol, Idptr, Stptr, Stringptr;

   (*
   EXPORT QUALIFIED GetSymbol, Skip, sy, position, line, nptr,
      controlRangeCheck, arithmeticRangeCheck, cstPtr, cString, val;
   *)

   VAR 
      position             : INTEGER;
      line                 : INTEGER;
      sy                   : Symbol;
      val                  : INTEGER;
      nptr                 : Idptr;
      cstPtr               : Stptr;
      cString              : Stringptr;
      controlRangeCheck    : BOOLEAN;
      arithmeticRangeCheck : BOOLEAN;
      pline                : INTEGER; (* line since last procedure start *)

   PROCEDURE GetSymbol;

   PROCEDURE Skip(x1,x2: Symbol);

END MCP4Scanner. 
