DEFINITION MODULE Scan;

   FROM SymTab IMPORT FileName, Identifier, Line;

   TYPE
      Symbol = (endsy, modulesy, procsy);

   PROCEDURE OpenScan(file: FileName);

   PROCEDURE GetSy(VAR sy: Symbol; VAR id: Identifier;
                   VAR line: Line) : BOOLEAN;
      (* return FALSE on eof *)

END Scan.
