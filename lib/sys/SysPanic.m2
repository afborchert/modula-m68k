IMPLEMENTATION MODULE SysPanic;

   (* don't use the StdIO or FtdIO (else reference cycle problems) *)

   FROM SysWrite IMPORT Write;
   FROM SysExit IMPORT Exit;
   FROM SYSTEM IMPORT ADR;

   PROCEDURE Panic(text: ARRAY OF CHAR);
      CONST
         stderr = 2;
      VAR
         index: CARDINAL;
         count: CARDINAL;
   BEGIN
      count := 1;
      index := 0;
      WHILE (count = 1) AND (index <= HIGH(text)) AND (text[index] <> 0C)
            AND Write(stderr, ADR(text[index]), count) DO
         INC(index);
      END;
      IF count = 1 THEN
         text[0] := 12C; (* nl *)
         IF Write(stderr, ADR(text), count) THEN END;
      END;
      Exit(255);
   END Panic;

END SysPanic.
