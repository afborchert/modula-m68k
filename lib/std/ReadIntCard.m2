IMPLEMENTATION MODULE ReadIntCard;

   FROM SYSTEM IMPORT WORD;

   (* (* from definition module *)
   TYPE
      Type = (int, card);
      ReadProc = PROCEDURE(VAR CHAR);

   VAR Done: BOOLEAN;
   *)

   PROCEDURE Read(VAR w: WORD; t: Type; ReadChar: ReadProc);
      CONST
         tab = 11C;
         nl = 12C;
      VAR
         minus: BOOLEAN;
         arg: CARDINAL;
         ch: CHAR;
   BEGIN
      minus := FALSE;
      REPEAT
         ReadChar(ch);
         IF (ch = '-') AND (t = int) THEN
            minus := NOT minus;
            ch := ' ';
         ELSIF ch = '+' THEN
            ch := ' ';
         END;
      UNTIL (ch <> ' ') AND (ch <> tab) AND (ch <> nl);
      IF (ch < '0') OR (ch > '9') THEN
         Done := FALSE;
         RETURN;
      END;
      arg := ORD(ch) - ORD('0');
      REPEAT
         ReadChar(ch);
         IF (ch >= '0') AND (ch <= '9') THEN
            arg := arg*10 + (CARDINAL(ORD(ch)) - CARDINAL(ORD('0')));
         END;
      UNTIL (ch < '0') OR (ch > '9');
      Done := TRUE;
      IF minus THEN
         w := WORD(- INTEGER(arg));
      ELSE
         w := WORD(arg);
      END;
   END Read;

END ReadIntCard.
