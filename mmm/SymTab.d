(* Modula-2 makefile generator - UNIX System V - AFB 3/87 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
DEFINITION MODULE SymTab;

   FROM FileNames IMPORT FileName;

   CONST
      maxmodule = 512;		(* maximum number of modules *)
      modnmlen  = 512;		(* maximal module name length *)

   TYPE
      ModuleName = ARRAY [0..modnmlen-1] OF CHAR;
      ModuleRange = [0..maxmodule-1];
      ModuleSet = SET OF ModuleRange;
      SourceKind = (m2impl, m2def, m2main, mrimpl, mrdef, mrmain);
      SourceKindSet = SET OF SourceKind;
      SourceRef = POINTER TO Source;
      Source =
	 RECORD
	    kind: SourceKind;
	    filename: FileName;
	    modulename: ModuleName;
	    extern: BOOLEAN;                (* if on: no source given *)
	    depset: ModuleSet;
	    link: SourceRef;
	    CASE associated: BOOLEAN OF
	    | TRUE: otherpart: ModuleRange; (* impmod of defmod or vice versa *)
	    END;
	 END;

   CONST
      defs = SourceKindSet{m2def, mrdef};

   VAR
      mtab: ARRAY ModuleRange OF SourceRef;
      modules: CARDINAL; (* count of modules *)
      defset: ModuleSet; (* set of definition modules *)
      extset: ModuleSet; (* set of modules in MODPATH and lib *)
      (* sets of non-external Modula-2 and Modula/R modules *)
      m2set, mrset: ModuleSet;
      srclist: SourceRef;

   PROCEDURE InitSymTab;

   PROCEDURE InitModules;

   PROCEDURE LookForDef(modname: ModuleName; VAR module: ModuleRange) : BOOLEAN;
      (* only Modula-2 modules will be returned *)

   PROCEDURE LookForDefR(modname: ModuleName;
                         VAR module: ModuleRange) : BOOLEAN;
      (* Modula/R or Modula-2 module possible *)

   PROCEDURE LookForMod(modname: ModuleName; VAR module: ModuleRange) : BOOLEAN;

   PROCEDURE LookForModR(modname: ModuleName;
                         VAR module: ModuleRange) : BOOLEAN;

   PROCEDURE CheckFileNames;

   PROCEDURE Closure(deps: ModuleSet; VAR closure: ModuleSet);

END SymTab.
