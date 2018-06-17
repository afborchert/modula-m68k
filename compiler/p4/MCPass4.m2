(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
MODULE MCPass4; (* AFB 8/83 *)

  FROM MCP4Init IMPORT Init;
  FROM MCP4Strings IMPORT PrintStrings;
  FROM MCP4SetConst IMPORT EmitSetConst;
  FROM MCP4Public IMPORT ErrorsFound;
  FROM MCP4In IMPORT CloseIO;
  FROM MCP4CodeSys IMPORT EmitEND;
  FROM MCStop IMPORT Stop;
  FROM MCP4Block IMPORT CompilationUnit;

BEGIN  
  Init;
  CompilationUnit;
  PrintStrings;
  EmitSetConst;
  CloseIO;
  EmitEND;
  IF ErrorsFound THEN
     Stop(1);
  END;
END MCPass4.
