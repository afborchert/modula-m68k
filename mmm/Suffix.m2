(* Modula-2 makefile generator - UNIX System V - AFB 3/87 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
IMPLEMENTATION MODULE Suffix;

   FROM FileNames IMPORT maxsuffix;
   FROM SymTab IMPORT SourceKind;
   FROM Errors IMPORT Fatal;
   FROM Strings IMPORT StrCmp, StrCpy;

   PROCEDURE SuffixToSourceKind(suffix: ARRAY OF CHAR) : SourceKind;
      (* aborts on unknown suffixes *)
   BEGIN
      IF StrCmp(suffix, Sm2impl) = 0 THEN RETURN m2impl
      ELSIF StrCmp(suffix, Sm2def) = 0 THEN RETURN m2def
      ELSIF StrCmp(suffix, Smrimpl) = 0 THEN RETURN mrimpl
      ELSIF StrCmp(suffix, Smrdef) = 0 THEN RETURN mrdef
      ELSE Fatal(suffix, "unknown suffix")
      END;
   END SuffixToSourceKind;

   PROCEDURE SourceKindToSuffix(sourcekind: SourceKind;
				VAR suffix: ARRAY OF CHAR);
      (* NOT the inversion of SuffixToSourceKind; but the *)
      (* suffix of the output file after compiling a source *)
      (* of that kind *)
   BEGIN
      CASE sourcekind OF
      | m2impl:	StrCpy(suffix, Om2impl);
      | m2def:	StrCpy(suffix, Om2def);
      | m2main:	StrCpy(suffix, Om2main);
      | mrimpl:	StrCpy(suffix, Omrimpl);
      | mrdef:	StrCpy(suffix, Omrdef);
      | mrmain:	StrCpy(suffix, Omrmain);
      END;
   END SourceKindToSuffix;

END Suffix.
