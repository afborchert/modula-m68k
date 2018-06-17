(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)

IMPLEMENTATION MODULE MCP1Public;   (* AFB 2/84 *)
                                    (* REV AFB 11/84: MODPATH (see m2c) *)

   FROM SysPerror IMPORT Perror;
   FROM MCStop IMPORT Stop;
   FROM StdIO IMPORT FILE, Fopen, Fputc, stderr;
   FROM FtdIO IMPORT FwriteLn, FwriteString;
   FROM MCBase IMPORT modrev, modrev2, ismain, Spellix, loading;
   FROM Storage IMPORT ALLOCATE;

   PROCEDURE WorkupArguments;
      VAR buf: FileName;
          index: CARDINAL;
          start: CARDINAL;
          ign: BOOLEAN;
          newsym: SymNamePtr;
          last: SymNamePtr; (* last in chain *)

      PROCEDURE Usage;
      BEGIN
         FwriteString(stderr, "Usage: ");
	 ARGV(buf, 0);
	 FwriteString(stderr, buf);
	 FwriteString(stderr,
	    " storage [ -flags ] source il1 ascii [symfiles ...]");
         FwriteLn(stderr);
         Stop(2);
      END Usage;

   BEGIN (* WorkupArguments *)
      IF ARGC() < 5 THEN Usage END;

      (* look for flags *)

      ARGV(buf, 2);
      IF buf[0] = '-' THEN
         IF ARGC() < 6 THEN Usage END;
         index := 1;
         WHILE (buf[index] <> 0C) AND (index <= HIGH(buf)) DO
            CASE buf[index] OF
            | 'r' : modrev := TRUE;
	    | '2' : modrev2 := TRUE; modrev := TRUE;
            | 'L' : listing := TRUE;
            | 'm' : ismain := TRUE;
            | 'l' : loading := TRUE; Stop(0);
	    | 'x' : (* ignore it *) (* salami *)
            ELSE
               FwriteString(stderr, "m0: Unknown flag: ");
               ign := Fputc(buf[index], stderr);
               FwriteLn(stderr);
               Usage;
            END;
            INC(index);
         END;
         start := 3;
      ELSE
         start := 2;
      END;

      (* source file and temporary files *)

      FOR index := start TO start+2 DO
         ARGV(buf, index);
         CASE index-start OF
         | 0 : (* source file *)
	       srcName := buf;
         | 1 : (* il1 *)
	       il1Name := buf;
         | 2 : (* ascii *)
	       ascName := buf;
         END;
      END;

      (* symbolfiles *)

      symNames := NIL;
      last := NIL;
      FOR index := start+3 TO ARGC()-1 DO
         ARGV(buf, index);
	 (* link the filename into the list of symbol file names *)
         NEW(newsym);
         WITH newsym^ DO
            symName := buf;
            moduleName := Spellix(0);
            link := NIL;
         END;
         IF last = NIL THEN
            symNames := newsym;
         ELSE
            last^.link := newsym;
         END;
         last := newsym;
      END;
   END WorkupArguments;

BEGIN
   ErrorsFound := FALSE;
   modrev := FALSE;
   modrev2 := FALSE;
   SymFilesMissing := FALSE;
   ismain := FALSE;
   listing := FALSE;
   loading := FALSE;
   WorkupArguments;
END MCP1Public.
