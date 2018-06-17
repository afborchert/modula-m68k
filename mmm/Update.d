(* Modula-2 makefile generator - UNIX System V - AFB 3/87 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
DEFINITION MODULE Update;

   PROCEDURE Update(infile, outfile: ARRAY OF CHAR);

   PROCEDURE FirstMakefile(outfile: ARRAY OF CHAR);

   PROCEDURE FirstScan(infile: ARRAY OF CHAR; lookforSRC: BOOLEAN);

END Update.
