IMPLEMENTATION MODULE Terminal; (* AFB 8/84 *)

  (* version using StdIO *)

   FROM SYSTEM IMPORT ADR;
   FROM StdIO IMPORT stdin, stdout, Fputc, Fgetc, Fungetc, Fwrite;

   VAR
      (* Done: BOOLEAN; *)
      oldch: CHAR;
      readAgain: BOOLEAN;

   PROCEDURE Read(VAR ch: CHAR);
   BEGIN
      IF NOT Fgetc(ch, stdin) THEN
         Done := FALSE;
         ch := 0C;
      ELSE
         Done := TRUE;
      END;
      readAgain := FALSE;
      oldch := ch;
   END Read;

   PROCEDURE ReadAgain;
   BEGIN
      IF readAgain THEN
         Done := FALSE;
      ELSE
         Done := Fungetc(oldch, stdin);
         readAgain := TRUE;
      END;
   END ReadAgain;

   PROCEDURE Write(ch: CHAR);
   BEGIN
      Done := Fputc(ch, stdout);
   END Write;

   PROCEDURE WriteLn;
      CONST nl = 12C;
   BEGIN
      Write(nl);
   END WriteLn;

   PROCEDURE WriteString(s: ARRAY OF CHAR);
      VAR i: CARDINAL;
   BEGIN
      i := 0;
      WHILE (i <= HIGH(s)) AND (s[i] <> 0C) DO
         INC(i);
      END;
      (* use Fwrite for efficiency *)
      Done := (i = 0) OR Fwrite(ADR(s), SIZE(CHAR), i, stdout);
   END WriteString;

BEGIN
   readAgain := FALSE;
   Done := TRUE;
END Terminal.
