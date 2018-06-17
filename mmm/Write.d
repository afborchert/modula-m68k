(* Modula-2 makefile generator - UNIX System V - AFB 3/87 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
DEFINITION MODULE Write;

   FROM SymTab IMPORT ModuleSet;
   FROM FileNames IMPORT FileName;

   PROCEDURE WriteNameAndSuffix(fn: FileName; suffix: ARRAY OF CHAR);

   PROCEDURE WriteArchiveAndSuffix(fn: FileName;
                                   archive, suffix: ARRAY OF CHAR);

   PROCEDURE WriteModuleSet(mset: ModuleSet; suffix: ARRAY OF CHAR);

   PROCEDURE WriteModuleSetInArchive(mset: ModuleSet;
                                     archive, suffix: ARRAY OF CHAR);

   PROCEDURE WriteSortedModuleSetInArchive(mset: ModuleSet;
                                           archive, suffix: ARRAY OF CHAR);

END Write.
