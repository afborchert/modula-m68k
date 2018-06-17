(* Modula-2 makefile generator - UNIX System V - AFB 3/87 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
DEFINITION MODULE Scan;

   FROM FileNames IMPORT FileName;
   FROM SymTab IMPORT ModuleName;

   TYPE
      Symbol =
	 (modulesy, definitionsy, implementationsy,
	 fromsy, importsy, systemsy, ident, sem, comma, eop, illegal);
      Identifier = ModuleName;

   VAR
      line: CARDINAL; (* current source line *)

   PROCEDURE OpenScan(file: FileName);

   PROCEDURE GetSy(VAR sy: Symbol; VAR id: Identifier);

END Scan.
