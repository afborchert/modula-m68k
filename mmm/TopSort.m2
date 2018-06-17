(* Modula-2 makefile generator - UNIX System V - AFB 3/87 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
IMPLEMENTATION MODULE TopSort;

   FROM SymTab IMPORT ModuleRange, ModuleSet, modules, mtab, defs,
      SourceRef, srclist;
   FROM Errors IMPORT Fatal;
   FROM FileNames IMPORT WriteFileName, FileName;
   FROM StdIO IMPORT stdout;
   FROM InOut IMPORT Write, WriteLn, WriteString;
   FROM ASCII IMPORT tab;

   (* (* exported from definition module *)
   VAR
      defindex: ARRAY ModuleRange OF ModuleRange;
      defmodules: CARDINAL;
   *)

   PROCEDURE TopSort;
      VAR
	 inindex: ModuleSet;
	 module: ModuleRange;
	 sorted: BOOLEAN;
	 cycle: BOOLEAN;
	 candidate: ModuleRange;  (* perhabs member of cycle *)
   BEGIN
      IF modules = 0 THEN RETURN END;
      defmodules := 0;
      inindex := ModuleSet{};
      REPEAT
	 sorted := TRUE; cycle := TRUE;
	 FOR module := MIN(ModuleRange) TO modules-1 DO
	    WITH mtab[module]^ DO
	       IF (kind IN defs) AND NOT (module IN inindex) THEN
		  IF depset <= inindex THEN
		     defindex[defmodules] := module;
		     INC(defmodules);
		     INCL(inindex, module);
		     cycle := FALSE;
		  ELSE
		     sorted := FALSE;
		     candidate := module;
		  END;
	       END;
	    END;
	 END;
      UNTIL sorted OR cycle;
      IF NOT sorted THEN
	 Fatal(mtab[candidate]^.modulename, "illegal reference cycle");
      END;
   END TopSort;

   PROCEDURE PrintSortedArgs;
      VAR
	 src: SourceRef;
	 module: ModuleRange;
	 first: BOOLEAN;
   BEGIN
      first := TRUE;
      IF defmodules > 0 THEN
	 FOR module := MIN(ModuleRange) TO defmodules-1 DO
	    WITH mtab[defindex[module]]^ DO
	       IF NOT extern THEN
		  IF first THEN
		     first := FALSE;
		  ELSE
		     Write(" ");
		  END;
		  WriteFileName(stdout, filename);
	       END;
	    END;
	 END;
      END;
      src := srclist;
      WHILE src <> NIL DO
	 WITH src^ DO
	    IF NOT extern AND NOT (kind IN defs) THEN
	       IF first THEN
		  first := FALSE;
	       ELSE
		  Write(" ");
	       END;
	       WriteFileName(stdout, filename);
	    END;
	 END;
	 src := src^.link;
      END;
      WriteLn;
   END PrintSortedArgs;

   PROCEDURE PrintDepsForTsort;
      VAR
	 src: SourceRef;
	 module: ModuleRange;

      PROCEDURE WriteDeps(fname: FileName; deps: ModuleSet);
	 VAR
	    module: ModuleRange;
	    depfile: FileName;
      BEGIN
	 FOR module := MIN(ModuleRange) TO modules-1 DO
	    IF module IN deps THEN
	       WriteFileName(stdout, mtab[module]^.filename); Write(" ");
	       WriteFileName(stdout, fname); WriteLn;
	    END;
	 END;
      END WriteDeps;

   BEGIN
      IF defmodules > 0 THEN
	 FOR module := MIN(ModuleRange) TO defmodules-1 DO
	    WITH mtab[defindex[module]]^ DO
	       IF NOT extern THEN
		  WriteDeps(filename, depset);
	       END;
	    END;
	 END;
      END;
      src := srclist;
      WHILE src <> NIL DO
	 WITH src^ DO
	    IF NOT extern AND NOT (kind IN defs) THEN
	       IF associated THEN
		  WriteDeps(filename, depset + ModuleSet{otherpart});
	       ELSE
		  WriteDeps(filename, depset);
	       END;
	    END;
	 END;
	 src := src^.link;
      END;
   END PrintDepsForTsort;

   PROCEDURE ProcCallsTopSort;
      VAR
	 inindex: ModuleSet;
	 module: ModuleRange;
	 done: BOOLEAN;
	 cycle: BOOLEAN;
	 firstcycle: BOOLEAN;
	 deptab: ARRAY ModuleRange OF ModuleSet;
	 candidates: ModuleSet;

      PROCEDURE InitDepTab;
	 VAR
	    module: ModuleRange;
	    import: ModuleRange;
      BEGIN
	 FOR module := MIN(ModuleRange) TO modules-1 DO
	    WITH mtab[module]^ DO
	       deptab[module] := ModuleSet{};
	       IF NOT (kind IN defs) AND NOT extern THEN
		  FOR import := MIN(ModuleRange) TO modules-1 DO
		     IF import IN depset THEN
			WITH mtab[import]^ DO
			   IF associated AND (otherpart <> module) THEN
			      INCL(deptab[module], otherpart);
			   END;
			END;
		     END;
		  END;
	       END;
	    END;
	 END;
      END InitDepTab;

      PROCEDURE FixCycle;
	 VAR
	    module: ModuleRange;

	 PROCEDURE PrintFile(module: ModuleRange);
	 BEGIN
	    WriteFileName(stdout, mtab[module]^.filename);
	 END PrintFile;

	 PROCEDURE SelectCycle;
	    VAR
	       oldcandidates: ModuleSet;
	       candidate: ModuleRange;
	       alldeps: ModuleSet;
	       oneofthem: ModuleRange;
	       importset: ModuleSet;
	       stop: BOOLEAN;
	 BEGIN
	    REPEAT
	       oldcandidates := candidates;
	       alldeps := ModuleSet{};
	       FOR candidate := MIN(ModuleRange) TO modules-1 DO
		  IF candidate IN candidates THEN
		     alldeps := alldeps + deptab[candidate];
		     oneofthem := candidate;
		  END;
	       END;
	       candidates := candidates * alldeps;
	    UNTIL oldcandidates = candidates;

	    importset := ModuleSet{oneofthem};
	    REPEAT
	       stop := TRUE;
	       FOR candidate := MIN(ModuleRange) TO modules-1 DO
		  IF (candidate IN candidates) AND
		     NOT (candidate IN importset) AND
		     (deptab[candidate] * importset <> ModuleSet{}) THEN
		     INCL(importset, candidate);
		     stop := FALSE;
		  END;
	       END;
	    UNTIL stop;
	    candidates := importset;
	 END SelectCycle;

      BEGIN
	 IF firstcycle THEN
	    firstcycle := FALSE;
	 END;
	 WriteString("reference cycle:"); WriteLn;
	 SelectCycle;
	 FOR module := MIN(ModuleRange) TO modules-1 DO
	    IF module IN candidates THEN
	       (* member of reference cycle *)
	       INCL(inindex, module);
	       Write(tab); PrintFile(module); WriteLn;
	    END;
	 END;
	 inindex := inindex + candidates;
      END FixCycle;

   BEGIN
      IF modules = 0 THEN RETURN END;
      InitDepTab; firstcycle := TRUE;
      inindex := ModuleSet{};
      REPEAT
	 REPEAT
	    done := TRUE; cycle := TRUE; candidates := ModuleSet{};
	    FOR module := MIN(ModuleRange) TO modules-1 DO
	       WITH mtab[module]^ DO
		  IF NOT (kind IN defs) AND NOT (module IN inindex) THEN
		     IF deptab[module] <= inindex THEN
			INCL(inindex, module);
			cycle := FALSE;
		     ELSE
			done := FALSE;
			INCL(candidates, module);
		     END;
		  END;
	       END;
	    END;
	 UNTIL done OR cycle;
	 IF NOT done THEN
	    FixCycle;
	 END;
      UNTIL done;
      IF firstcycle THEN
	 WriteString("no reference cycles found"); WriteLn;
      END;
   END ProcCallsTopSort;

END TopSort.
