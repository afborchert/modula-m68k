(* Modula-2 Library    -  UNIX System V  -     AFB 3/89 *)
(* (c) Universitaet Ulm, Sektion Informatik, D-7900 Ulm *)
IMPLEMENTATION MODULE SysMknod;

   FROM SYSTEM IMPORT UNIXCALL, ADR;
   FROM Sys IMPORT mknod;
   FROM SysStat IMPORT FileType, IfFifo;
   FROM Errno IMPORT errno;
   FROM UnixString IMPORT Copy, Buffer;

   PROCEDURE Mknod(path: ARRAY OF CHAR;
		   mode: BITSET;
		   dev: CARDINAL (* major and minor number *)
		   ) : BOOLEAN;
      VAR
	 r0, r1: INTEGER;
	 pathbuf: Buffer;
   BEGIN
      Copy(pathbuf, path);
      IF UNIXCALL(mknod, r0, r1, ADR(pathbuf),
		  CARDINAL(mode) DIV 10000H, dev) THEN
	 RETURN TRUE
      ELSE
	 errno := r0;
	 RETURN FALSE
      END;
   END Mknod;

   PROCEDURE CreateNamedPipe(path: ARRAY OF CHAR; mode: BITSET) : BOOLEAN;
      (* the mode bits for a named pipe are or'ed to mode;
	 so mode consists of the protection bits only
      *)
   BEGIN
      mode := mode - FileType + IfFifo;
      RETURN Mknod(path, mode, 0)
   END CreateNamedPipe;

END SysMknod.
