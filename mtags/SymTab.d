DEFINITION MODULE SymTab;

   CONST
      FileNameLength = 64;
      LineLength = 128;
      IdentLen = 24;

   TYPE
      FileName = ARRAY[0..FileNameLength-1] OF CHAR;
      Line = ARRAY[0..LineLength-1] OF CHAR;
      Identifier = ARRAY[0..IdentLen-1] OF CHAR;
      SymRef = POINTER TO SymEntry;
      SymEntry =
	 RECORD
	    name: Identifier;  (* name of procedure/module *)
	    file: FileName;
	    line: Line;
	    multdecl: BOOLEAN; (* multiple declared ? *)
	    father: SymRef;
	    link: SymRef;
	 END;

   PROCEDURE EnterProc(pname: Identifier; line: Line);

   PROCEDURE EndProc;

   PROCEDURE EnterFile(fname: FileName);

   PROCEDURE EndFile;

   PROCEDURE FirstProc(VAR sym: SymRef);

   PROCEDURE NextProc(VAR sym: SymRef);

END SymTab.
