(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
IMPLEMENTATION MODULE MCP4Out; (* AFB 8/83 *)
   (* output routines *)

   FROM StdIO IMPORT FILE, Fopen, Fclose, Fputc, write;
   FROM MCP4Public IMPORT assName;
   FROM SysPerror IMPORT Perror;
   FROM MCStop IMPORT Stop;

   CONST
      nl = 12C;

   VAR
      ass: FILE;

   PROCEDURE WriteString(str: ARRAY OF CHAR);
      VAR i: CARDINAL;
   BEGIN
      i := 0;
      WHILE (i <= HIGH(str)) AND (str[i] <> 0C) DO
         Write(str[i]);
         INC(i);
      END
   END WriteString;

   PROCEDURE WriteLn;
   BEGIN
      Write(nl);
   END WriteLn;

   PROCEDURE Write(ch: CHAR);
   BEGIN
      IF NOT Fputc(ch, ass) THEN
         Perror(assName);
         Stop(1);
      END;
   END Write;

   PROCEDURE TermOut;
   BEGIN
      IF NOT Fclose(ass) THEN
         Perror(assName);
         Stop(1);
      END;
   END TermOut;

BEGIN
   IF NOT Fopen(ass, assName, write, (* buffered = *) TRUE) THEN
      Perror(assName);
      Stop(1);
   END;
END MCP4Out.
