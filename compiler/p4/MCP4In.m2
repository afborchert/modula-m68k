(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
IMPLEMENTATION MODULE MCP4In;           (* AFB 8/83 *)

   FROM SYSTEM IMPORT WORD;
   FROM StdIO IMPORT FILE, Fopen, Fclose, read;
   FROM FtdIO IMPORT FreadWord, Done;
   FROM MCHalfword IMPORT ReadHalfword;
   FROM MCStop IMPORT Stop;
   FROM SysPerror IMPORT Perror;
   FROM MCP4Public IMPORT il1Name;

   VAR
      il1: FILE;

   PROCEDURE ReadInputWord(VAR w: WORD);
   BEGIN 
      FreadWord(il1, w);
      IF NOT Done THEN
         Perror(il1Name);
      END;
   END ReadInputWord;

   PROCEDURE ReadInputHalfword(VAR w: WORD);
      VAR c: CARDINAL;
   BEGIN
      ReadHalfword(il1, c);
      w := WORD(c);
   END ReadInputHalfword;

   PROCEDURE CloseIO;
   BEGIN 
      IF NOT Fclose(il1) THEN
         Perror(il1Name);
         Stop(1);
      END;
   END CloseIO;

BEGIN 
   IF NOT Fopen(il1, il1Name, read, (* buffered = *) TRUE) THEN
      Perror(il1Name);
      Stop(1);
   END;
END MCP4In. 
