(* Modula-2 makefile generator - UNIX System V - AFB 3/87 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
IMPLEMENTATION MODULE SymTab;

   FROM FileNames IMPORT FileName, GetFileName, ConstructFileName, filenmlen;
   FROM Errors IMPORT Fatal, Warning;
   FROM Options IMPORT versmanag, VersionsManagement;
   FROM Strings IMPORT StrCmp, StrLen;
   FROM Storage IMPORT ALLOCATE;
   FROM Suffix IMPORT SuffixToSourceKind;

   PROCEDURE InitSymTab;
      (* get source files and library modules in sorted order via *)
      (* GetFileName and enter them into srclist and mtab.        *)
      (* calculate some module sets for general use.              *)
      VAR
	 filenm: FileName;                (* source filename *)
	 newsrc: SourceRef;               (* to be appended to srclist *)
	 srctail: SourceRef;              (* tail of srclist *)
	 ext: BOOLEAN; mname: ModuleName; (* out-parms of GetFileName *)
	 modR: BOOLEAN;
   BEGIN
      srclist := NIL;
      modules := 0;
      srctail := NIL;
      defset := ModuleSet{};
      extset := ModuleSet{};
      m2set := ModuleSet{}; mrset := ModuleSet{};

      WHILE GetFileName(filenm, ext, modR, mname) DO
	 NEW(newsrc);
	 WITH newsrc^ DO
	    filename := filenm;
	    IF ext THEN
	       IF modR THEN
		  kind := mrdef;
	       ELSE
		  kind := m2def;
	       END;
	       modulename := mname;
	       extern := TRUE;
	    ELSE
	       kind := SuffixToSourceKind(filenm.suffix);
	       modulename[0] := 0C;
	       extern := FALSE;
	    END;
	    IF modules <= HIGH(mtab) THEN
	       mtab[modules] := newsrc;
	       IF extern THEN
		  INCL(extset, modules);
	       ELSIF kind IN defs THEN
		  INCL(defset, modules);
	       ELSIF kind = m2impl THEN
		  INCL(m2set, modules);
	       ELSIF kind = mrimpl THEN
		  INCL(mrset, modules);
	       END;
	       INC(modules);
	    ELSE
	       Fatal(filenm.basename, "too many modules");
	    END;
	    depset := ModuleSet{};
	    associated := FALSE;
	    link := NIL;
	 END;
	 IF srclist = NIL THEN
	    srclist := newsrc;
	 ELSE
	    srctail^.link := newsrc;
	 END;
	 srctail := newsrc;
      END;
   END InitSymTab;

   PROCEDURE InitModules;
      VAR
	 defmodule, module: ModuleRange;
   BEGIN
      IF modules = 0 THEN RETURN END;
      FOR module := MIN(ModuleRange) TO modules-1 DO
	 WITH mtab[module]^ DO
	    IF (kind <> m2def) AND (kind <> mrdef) THEN
	       IF (kind = m2impl) AND LookForDef(modulename, defmodule) OR
		  (kind = mrimpl) AND LookForDefR(modulename, defmodule) THEN
		  associated := TRUE;
		  otherpart := defmodule;
		  WITH mtab[otherpart]^ DO
		     IF associated THEN
			Warning(modulename, "multiple defined");
		     ELSE
			associated := TRUE;
			otherpart := module;
		     END;
		  END;
		  INCL(depset, otherpart);
	       END;
	    END;
	 END;
      END;
   END InitModules;

   PROCEDURE LookFor(modname: ModuleName;
		     srcset: SourceKindSet;
		     VAR module: ModuleRange) : BOOLEAN;
      (* search for module within srcset with name 'modname'       *)
      (* check if 'modname' is locally defined (explicitely given) *)
      (* if not then return external ones                          *)
      VAR
	 found: BOOLEAN;	(* module found; but external *)
	 first: ModuleRange;	(* the module we have found first *)
   BEGIN
      found := FALSE;
      module := MIN(ModuleRange);
      WHILE module < modules DO
	 WITH mtab[module]^ DO
	    IF (kind IN srcset) AND (StrCmp(modname, modulename) = 0) THEN
	       IF extern THEN
		  first := module;
		  found := TRUE;
	       ELSE
		  RETURN TRUE
	       END;
	    END;
	 END;
	 INC(module);
      END;
      IF found THEN module := first; RETURN TRUE END;
      RETURN FALSE
   END LookFor;

   PROCEDURE LookForDef(modname: ModuleName; VAR module: ModuleRange) : BOOLEAN;
   BEGIN
      RETURN LookFor(modname, SourceKindSet{m2def}, module)
   END LookForDef;

   PROCEDURE LookForDefR(modname: ModuleName;
                         VAR module: ModuleRange) : BOOLEAN;
   BEGIN
      RETURN LookFor(modname, defs, module)
   END LookForDefR;

   PROCEDURE LookForMod(modname: ModuleName; VAR module: ModuleRange) : BOOLEAN;
   BEGIN
      RETURN LookFor(modname, SourceKindSet{m2main, m2impl},
		     module)
   END LookForMod;

   PROCEDURE LookForModR(modname: ModuleName;
                         VAR module: ModuleRange) : BOOLEAN;
   BEGIN
      RETURN LookFor(modname, SourceKindSet{m2main, m2impl, mrmain, mrimpl},
		     module)
   END LookForModR;

   PROCEDURE CheckFileNames;
      (* check for some naming conventions and potential troubles *)
      (* caused by module/file names *)
      CONST
	 significant = 11;
      VAR
	 src: SourceRef;
	 file: ARRAY [0..filenmlen-1] OF CHAR;
	 savech: CHAR;
   BEGIN
      src := srclist;
      WHILE src <> NIL DO
	 WITH src^ DO
	    ConstructFileName(file, filename);
	    IF StrLen(filename.basename) > significant THEN
	       Warning(file, "base name too long");
	    ELSE
	       savech := modulename[significant];
	       modulename[significant] := 0C;
	       IF StrCmp(filename.basename, modulename) <> 0 THEN
		  Warning(file, "file name should be identical to module name");
	       END;
	       modulename[significant] := savech;
	       IF NOT extern AND (versmanag <> none) AND
		  (StrLen(modulename) > significant-2) THEN
		  Warning(file, "module name too long for rcs/sccs");
	       END;
	    END;
	    src := link;
	 END;
      END;
   END CheckFileNames;

   PROCEDURE Closure(deps: ModuleSet; VAR closure: ModuleSet);
      VAR
	 added: ModuleSet;
	 module: ModuleRange;
   BEGIN
      added := ModuleSet{};
      REPEAT
	 closure := deps;
	 FOR module := MIN(ModuleRange) TO modules-1 DO
	    IF (module IN deps) AND NOT (module IN added) THEN
	       deps := deps + mtab[module]^.depset;
	       WITH mtab[module]^ DO
		  IF associated THEN
		     deps := deps + mtab[otherpart]^.depset;
		  END;
	       END;
	       INCL(added, module);
	    END;
	 END;
      UNTIL closure = deps;
      closure := closure - extset;
   END Closure;

END SymTab.
