(* Modula-2 makefile generator - UNIX System V - AFB 3/87 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
DEFINITION MODULE Suffix;

   FROM FileNames IMPORT maxsuffix;
   FROM SymTab IMPORT SourceKind;

   CONST
      (* suffixes of source files *)
      Sm2impl = "m2"; Sm2def  = "d";
      Smrimpl = "mr"; Smrdef  = "dr";

      (* suffixes of compiler output files *)
      Om2impl = "o";  Om2def  = "sy"; Om2main = "o";
      Omrimpl = "o";  Omrdef  = "sy"; Omrmain = "o";

      Archive = "a";
      Ref = "r";


   PROCEDURE SuffixToSourceKind(suffix: ARRAY OF CHAR) : SourceKind;
      (* aborts on unknown suffixes *)

   PROCEDURE SourceKindToSuffix(sourcekind: SourceKind;
				VAR suffix: ARRAY OF CHAR);
      (* NOT the inversion of SuffixToSourceKind; but the *)
      (* suffix of the output file after compiling a source *)
      (* of that kind *)

END Suffix.
