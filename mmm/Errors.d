(* Modula-2 makefile generator - UNIX System V - AFB 3/87 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
DEFINITION MODULE Errors;

   PROCEDURE Warning(text1, text2: ARRAY OF CHAR);

   PROCEDURE Error(text1, text2: ARRAY OF CHAR);

   PROCEDURE Fatal(text1, text2: ARRAY OF CHAR);

   PROCEDURE Syntax(fn: ARRAY OF CHAR; line: CARDINAL; text: ARRAY OF CHAR);

   PROCEDURE Errors() : CARDINAL;

END Errors.
