(****************************************
*                                       *
*     MODULA-2 Multi-Pass Compiler      *
*     ****************************      *
*                                       *
*     Implementation for MC68020/UNIX   *
*                                       *
*     running on MC68020/UNIX
*                                       *
*     MCP3Public:                       *
*                                       * 
*     public part of the common base    *
*     of the Modula-2 compiler          *
*     and argument handling             *
*                                       * 
****************************************)

IMPLEMENTATION MODULE MCP3Public;   (* AFB 3/84 *)

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
	 FwriteString(stderr, " storage [ -flags ] il1 il2 ascii");
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
	    | 'D' : MDDFlag := TRUE;
	    | 'l' : loading := TRUE; Stop(0);
            ELSE
               FwriteString(stderr, "m2: Unknown flag: ");
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

      (* interpass files *)

      FOR index := start TO start+2 DO
         ARGV(buf, index);
         CASE index-start OF
           0 : (* il1 *)
	       il1Name := buf;
         | 1 : (* il2 *)
	       il2Name := buf;
         | 2 : (* ascii *)
	       ascName := buf;
         END;
      END;
   END WorkupArguments;

BEGIN
   IF loading THEN Stop(0) END;
   ErrorsFound := FALSE;
   MDDFlag := FALSE;
   WorkupArguments;
END MCP3Public.
