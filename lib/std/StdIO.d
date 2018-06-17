(* Modula-2 Library    -  UNIX System V  -     AFB 1/84 *)
(* (c) Universitaet Ulm, Sektion Informatik, D-7900 Ulm *)
DEFINITION MODULE StdIO;

   FROM SYSTEM IMPORT ADDRESS;
   FROM SystemTypes IMPORT OFF;

   TYPE
      FILE; (* hidden *)
      MODE = (read, write, append);

   VAR
      stdin, stdout, stderr: FILE;

   (* all functions return FALSE in error case *)

   PROCEDURE Fopen(VAR f: FILE; name: ARRAY OF CHAR; mode: MODE;
		   buffered: BOOLEAN) : BOOLEAN;

   PROCEDURE Fclose(f: FILE) : BOOLEAN;

   PROCEDURE Fread(ptr: ADDRESS; size: CARDINAL; VAR nitems: CARDINAL;
		   f: FILE) : BOOLEAN;

   PROCEDURE Fwrite(ptr: ADDRESS; size: CARDINAL; VAR nitems: CARDINAL;
		    f: FILE) : BOOLEAN;

   PROCEDURE Fseek(f: FILE; offset: OFF; whence: CARDINAL) : BOOLEAN;

   PROCEDURE Ftell(f: FILE; VAR pos: OFF) : BOOLEAN;

   PROCEDURE Feof(f: FILE) : BOOLEAN;

   PROCEDURE Ferror(f: FILE) : BOOLEAN;

   PROCEDURE Fgetc(VAR ch: CHAR; f: FILE) : BOOLEAN;

   PROCEDURE Fputc(ch: CHAR; f: FILE) : BOOLEAN;

   PROCEDURE Fungetc(ch: CHAR; f: FILE) : BOOLEAN;

   PROCEDURE CloseAll() : BOOLEAN;

   PROCEDURE Fflush(f: FILE) : BOOLEAN;

   PROCEDURE Fdopen(VAR f: FILE; fd: CARDINAL; mode: MODE;
                    buffered: BOOLEAN) : BOOLEAN;

   PROCEDURE FileNo(f: FILE) : CARDINAL;

END StdIO.
