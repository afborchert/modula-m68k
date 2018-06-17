DEFINITION MODULE MCHalfword; (* AFB 3/84 *)

   FROM StdIO IMPORT FILE;

   (*
   EXPORT QUALIFIED WriteHalfword, ReadHalfword;
   *)

   PROCEDURE WriteHalfword(f: FILE; c: CARDINAL);

   PROCEDURE ReadHalfword(f: FILE; VAR c: CARDINAL);

END MCHalfword.
