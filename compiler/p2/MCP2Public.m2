(****************************************
*                                       *
*     MODULA-2 Multi-Pass Compiler      *
*     ****************************      *
*                                       *
*     Implementation for ENDLICH        *
*                                       *
*     running on ENDLICH                *
*                                       *
*     MCP2Public:                       *
*                                       * 
*     public part of the common base    *
*     of the Modula-2 compiler          *
*     and argument handling             *
*                                       * 
****************************************)

IMPLEMENTATION MODULE MCP2Public;   (* AFB 3/84 *)

  IMPORT StdIO; (* MUST be initialized first !! *)

   FROM SysPerror IMPORT Perror;
   FROM MCStop IMPORT Stop;
   FROM StdIO IMPORT FILE, Fopen, Fputc, stderr;
   FROM FtdIO IMPORT FwriteLn, FwriteString;
   FROM MCBase IMPORT loading;

   PROCEDURE WorkupArguments;
      VAR buf: FileName;
          index: CARDINAL;
          start: CARDINAL;
          ign: BOOLEAN;

      PROCEDURE Usage;
      BEGIN
         FwriteString(stderr, "Usage: ");
	 ARGV(buf, 0);
	 FwriteString(stderr, buf);
	 FwriteString(stderr, " storage [ -flags ] il1 il2 ascii [ ref ]");
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
	    | 'l' : loading := TRUE; Stop(0);
            ELSE
               FwriteString(stderr, "m1: Unknown flag: ");
               ign := Fputc(buf[index], stderr);
               FwriteLn(stderr);
               Usage;
            END;
         END;
         start := 3;
      ELSE
         start := 2;
      END;

      (* source file, temporary file names and output files *)

      FOR index := start TO ARGC()-1 DO
         ARGV(buf, index);
         CASE index-start OF
           0 : (* il1 *)
	       il1Name := buf;
         | 1 : (* il2 *)
	       il2Name := buf;
         | 2 : (* ascii *)
	       ascName := buf;
         | 3 : (* ref file *)
	       refName := buf;
         END;
      END;
   END WorkupArguments;

BEGIN
   ErrorsFound := FALSE;
   IF loading THEN Stop(0) END;
   WorkupArguments;
END MCP2Public.
