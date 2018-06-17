(* Modula-2 makefile generator - UNIX System V - AFB 3/87 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
IMPLEMENTATION MODULE Sources;

   FROM SymTab IMPORT SourceRef, Source, srclist, mtab,
      SourceKind, SourceKindSet;
   FROM Scan IMPORT line, Symbol, Identifier, OpenScan, GetSy;
   FROM FileNames IMPORT ConstructFileName, filenmlen;
   FROM Errors IMPORT Fatal;
   FROM InOut IMPORT WriteString, WriteLn;
   FROM Strings IMPORT StrCpy, StrCmp;
   IMPORT Errors, Strings, SymTab, Storage, Options;

   PROCEDURE ScanSources;
      VAR
	 sp: SourceRef;

      MODULE DepLists;

	 FROM Errors IMPORT Warning;
	 FROM SymTab IMPORT LookForDef, ModuleRange, ModuleSet, SourceRef,
	    ModuleName, LookForDefR, SourceKindSet, SourceKind;
	 FROM Options IMPORT scanlibs;
	 FROM Strings IMPORT StrCpy;
	 FROM Storage IMPORT ALLOCATE;
	 IMPORT sp;

	 EXPORT NewSource, DependsFrom, MakeSets;

	 TYPE
	    SourceList = POINTER TO SourceRec;
	    DepList = POINTER TO Dep;
	    SourceRec =
	       RECORD
		  src: SourceRef;
		  dep: DepList;
		  link: SourceList;
	       END;
	    Dep =
	       RECORD
		  modnm: ModuleName;
		  link: DepList;
	       END;
	 VAR
	    slhead: SourceList;

	 PROCEDURE NewSource;
	    VAR newsl: SourceList;
	 BEGIN
	    NEW(newsl);
	    WITH newsl^ DO
	       src := sp;
	       dep := NIL;
	       link := slhead;
	    END;
	    slhead := newsl;
	 END NewSource;

	 PROCEDURE DependsFrom(module: ARRAY OF CHAR);
	    VAR
	       newdep: DepList;
	 BEGIN
	    WITH slhead^ DO
	       NEW(newdep);
	       WITH newdep^ DO
		  StrCpy(modnm, module);
		  link := dep;
	       END;
	       dep := newdep;
	    END;
	 END DependsFrom;

	 PROCEDURE MakeSets;
	    VAR sl: SourceList;

	    PROCEDURE MakeSet(sl: SourceList);
	       VAR dl: DepList;

	       PROCEDURE Include(VAR mset: ModuleSet; modnm: ModuleName);
		  VAR module: ModuleRange;
	       BEGIN
		  IF (sl^.src^.kind IN SourceKindSet{mrdef, mrmain, mrimpl})
		     AND LookForDefR(modnm, module) THEN
		     INCL(mset, module);
		  ELSIF LookForDef(modnm, module) THEN
		     INCL(mset, module);
		  ELSIF scanlibs THEN
		     Warning(modnm, "not found");
		  END;
	       END Include;

	    BEGIN
	       WITH sl^ DO
		  dl := dep;
		  WHILE dl <> NIL DO
		     Include(src^.depset, dl^.modnm);
		     dl := dl^.link;
		  END;
	       END;
	    END MakeSet;

	 BEGIN
	    sl := slhead;
	    WHILE sl <> NIL DO
	       MakeSet(sl);
	       sl := sl^.link;
	    END;
	 END MakeSets;

      BEGIN
	 slhead := NIL;
      END DepLists;

      PROCEDURE Parse(VAR src: Source);
	 VAR
	    sy: Symbol;
	    id: Identifier;

	 PROCEDURE Syntax;
	    VAR fn: ARRAY [0..filenmlen-1] OF CHAR;
	 BEGIN
	    ConstructFileName(fn, src.filename);
	    Errors.Syntax(fn, line, id);
	 END Syntax;

	 PROCEDURE OtherError(text: ARRAY OF CHAR);
	    VAR fn: ARRAY [0..filenmlen-1] OF CHAR;
	 BEGIN
	    ConstructFileName(fn, src.filename);
	    Fatal(fn, text);
	 END OtherError;

	 PROCEDURE Check(checkfor: Symbol);
	 BEGIN
	    GetSy(sy, id);
	    IF sy <> checkfor THEN Syntax END;
	 END Check;

	 PROCEDURE Module;

	    PROCEDURE FromList;
	    BEGIN
	       IF sy = ident THEN
		  DependsFrom(id);
	       ELSIF sy <> systemsy THEN
		  Syntax;
	       END;
	       Check(importsy);
	       GetSy(sy, id);
	       WHILE sy = ident DO
		  GetSy(sy, id);
		  IF (sy <> comma) AND (sy <> sem) THEN Syntax END;
		  GetSy(sy, id);
	       END;
	    END FromList;

	    PROCEDURE ImportList;
	       VAR term: BOOLEAN;
	    BEGIN
	       IF (sy <> ident) AND (sy <> systemsy) THEN Syntax END;
	       REPEAT
		  IF sy <> systemsy THEN
		     DependsFrom(id);
		  END;
		  GetSy(sy, id);
		  IF (sy <> comma) AND (sy <> sem) THEN Syntax END;
		  term := sy = sem;
		  GetSy(sy, id);
	       UNTIL term;
	    END ImportList;

	 BEGIN
	    IF (sy = ident) AND (StrCmp(id, "DATABASE") = 0) THEN
	       IF src.kind <> mrdef THEN
		  OtherError("no database definition module");
	       END;
	       Check(definitionsy);
	    END;
	    CASE sy OF
	    | definitionsy:
	       IF (src.kind <> m2def) AND (src.kind <> mrdef) THEN
		  OtherError("no definition module");
	       END;
	       Check(modulesy);
	    | implementationsy:
	       IF (src.kind <> m2impl) AND (src.kind <> mrimpl) THEN
		  OtherError("no implementation module");
	       END;
	       Check(modulesy);
	    | modulesy:
	       CASE src.kind OF
	       | m2impl: src.kind := m2main;
	       | mrimpl: src.kind := mrmain;
	       ELSE
		  OtherError("no program module");
	       END;
	    ELSE
	       Syntax;
	    END;
	    Check(ident);
	    StrCpy(src.modulename, id);
	    Check(sem);
	    GetSy(sy, id);
	    WHILE (sy <> eop) AND (sy <> modulesy) DO
	       IF sy = fromsy THEN GetSy(sy, id); FromList
	       ELSIF sy = importsy THEN GetSy(sy, id); ImportList
	       ELSE
		  Syntax;
	       END;
	    END;
	    IF src.kind IN SourceKindSet{mrmain, mrimpl, mrdef} THEN
	       DependsFrom("QEM");
	       DependsFrom("RDSREM");
	    END;
	 END Module;

      BEGIN
	 OpenScan(src.filename);
	 GetSy(sy, id);
	 Module;
      END Parse;

   BEGIN
      sp := srclist;
      WHILE sp <> NIL DO
	 IF NOT sp^.extern THEN
	    NewSource;
	    Parse(sp^);
	 END;
	 sp := sp^.link;
      END;
      MakeSets;
   END ScanSources;

END Sources.
