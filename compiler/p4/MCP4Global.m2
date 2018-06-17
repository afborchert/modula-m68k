(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
IMPLEMENTATION MODULE MCP4Global;       (* rev AFB 8/83 *)

   FROM MCP4Public IMPORT ErrorsFound, errName, errFile;
   FROM SysPerror IMPORT Perror;
   FROM StdIO IMPORT stderr, FILE, Fopen, write, CloseAll;
   FROM FtdIO IMPORT FwriteString, FwriteCard, FwriteLn;
   FROM MCP4Scanner IMPORT line, position;
   FROM MCStop IMPORT Stop;
   FROM MCHalfword IMPORT WriteHalfword;

   VAR
      fp: FILE;

   PROCEDURE Assert(expr: BOOLEAN);
   BEGIN 
      IF NOT expr THEN 
         FwriteString(stderr,  "---- Assertion failed");
         FwriteLn(stderr);
         CompilerError;
      END 
   END Assert;

   PROCEDURE Error(n: CARDINAL);
   BEGIN 
      IF errFile THEN
         WriteHalfword(fp, line);
         WriteHalfword(fp, position);
         WriteHalfword(fp, n);
      ELSE
         FwriteString(stderr, "---- error at line ");
         FwriteCard(stderr, line, 1);
         FwriteString(stderr, ", pos ");
         FwriteCard(stderr, position, 1);
         FwriteString(stderr,  ": ");
         FwriteCard(stderr, n, 1);
         FwriteLn (stderr);
      END;
      ErrorsFound := TRUE
   END Error;

   PROCEDURE CompilerError;
      VAR dummy: INTEGER;
          ign: BOOLEAN;
   BEGIN 
      FwriteString(stderr,  "---- Compiler Error");
      FwriteLn(stderr);
      errFile := FALSE;
      Error(0);
      FwriteString(stderr,  "---- STOP");
      FwriteLn(stderr);
      (* flush i/o buffers *)
      ign := CloseAll();
      (* produce a core dump *)
      dummy := 0;
      dummy := dummy DIV dummy;
   END CompilerError;

BEGIN
   IF errFile THEN
      IF NOT Fopen(fp, errName, write, (* buffered = *) FALSE) THEN
         Perror(errName);
         Stop(2);
      END;
   END;
END MCP4Global. 
