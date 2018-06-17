(* Modula-2 makefile generator - UNIX System V - AFB 3/87 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
DEFINITION MODULE Out;

   (* text formatting for makefile *)

   PROCEDURE WriteString(text: ARRAY OF CHAR);

   PROCEDURE WriteLn;

   PROCEDURE Write(ch: CHAR);

   PROCEDURE WriteTab;

   PROCEDURE OpenMakefile(filename: ARRAY OF CHAR);

   PROCEDURE CloseMakefile;

END Out.
