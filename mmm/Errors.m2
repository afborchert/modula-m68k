(* Modula-2 makefile generator - UNIX System V - AFB 3/87 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
IMPLEMENTATION MODULE Errors;

   FROM FtdIO IMPORT FwriteString, FwriteLn, FwriteChar, FwriteCard;
   FROM StdIO IMPORT Fflush, stderr;
   FROM SysExit IMPORT Exit;

   VAR
      errors: CARDINAL;

   PROCEDURE Warning(text1, text2: ARRAY OF CHAR);
   BEGIN
      Out("warning", text1, text2);
   END Warning;

   PROCEDURE Error(text1, text2: ARRAY OF CHAR);
   BEGIN
      Out("error", text1, text2);
      INC(errors);
   END Error;

   PROCEDURE Fatal(text1, text2: ARRAY OF CHAR);
   BEGIN
      Out("fatal", text1, text2);
      Exit(errors+1);
   END Fatal;

   PROCEDURE Syntax(fn: ARRAY OF CHAR; line: CARDINAL; text: ARRAY OF CHAR);
   BEGIN
      FwriteString(stderr, 'syntax error in "');
      FwriteString(stderr, fn);
      FwriteString(stderr, '", line ');
      FwriteCard(stderr, line, 1);
      FwriteString(stderr, ' near "');
      FwriteString(stderr, text);
      FwriteChar(stderr, '"');
      FwriteLn(stderr);
      Exit(errors+1);
   END Syntax;

   PROCEDURE Errors() : CARDINAL;
   BEGIN
      RETURN errors;
   END Errors;

   PROCEDURE Out(text1, text2, text3: ARRAY OF CHAR);
   BEGIN
      FwriteString(stderr, text1); FwriteString(stderr, ": ");
      FwriteChar(stderr, '"'); FwriteString(stderr, text2);
      FwriteChar(stderr, '"'); FwriteString(stderr, ": ");
      FwriteString(stderr, text3); FwriteLn(stderr);
      IF NOT Fflush(stderr) THEN END;
   END Out;

BEGIN
   errors := 0;
END Errors.
