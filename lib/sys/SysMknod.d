(* Modula-2 Library    -  UNIX System V  -     AFB 3/89 *)
(* (c) Universitaet Ulm, Sektion Informatik, D-7900 Ulm *)
DEFINITION MODULE SysMknod;

   PROCEDURE Mknod(path: ARRAY OF CHAR;
		   mode: BITSET;
		   dev: CARDINAL (* major and minor number *)
		   ) : BOOLEAN;

   PROCEDURE CreateNamedPipe(path: ARRAY OF CHAR; mode: BITSET) : BOOLEAN;
      (* the mode bits for a named pipe are or'ed to mode;
	 so mode consists of the protection bits only
      *)

END SysMknod.
