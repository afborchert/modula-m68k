(* Modula-2 makefile generator - UNIX System V - AFB 3/87 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
DEFINITION MODULE TopSort;

   FROM SymTab IMPORT ModuleRange;

   VAR
      defindex: ARRAY ModuleRange OF ModuleRange;
      defmodules: CARDINAL;

   PROCEDURE TopSort;

   PROCEDURE PrintSortedArgs;

   PROCEDURE PrintDepsForTsort;

   PROCEDURE ProcCallsTopSort;

END TopSort.
