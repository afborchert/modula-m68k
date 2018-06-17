IMPLEMENTATION MODULE FtdWin;

   FROM ASCII IMPORT nl, tab, bs, del;
   FROM Conversions IMPORT ConvertInteger, ConvertCardinal;
   FROM Windows IMPORT Window, WindowRead, WindowWrite,
      GetWindowAttributes, SetWindowAttributes, WindowAtSet,
      WindowAttributes;
   FROM SYSTEM IMPORT WORD;
   IMPORT Windows;

   (* (* from definition module *)
   VAR Done: BOOLEAN;
       termCH: CHAR;
   *)
   TYPE
      Type = (int, card);
      CharSet = SET OF CHAR;
   CONST
      WhiteSpace = CharSet{nl, tab, ' '};

   PROCEDURE ReadText(win: Window; VAR text: ARRAY OF CHAR;
		      skip, delim: CharSet);
      VAR
	 index: CARDINAL;
	 ch: CHAR;
	 atset: WindowAtSet;
	 skipping: BOOLEAN;

      PROCEDURE Read(VAR ch: CHAR) : BOOLEAN;
      BEGIN
	 WindowRead(win, ch);
	 Done := Windows.Done;
	 RETURN Done
      END Read;

   BEGIN
      delim := delim - CharSet{bs, del};
      skipping := TRUE;
      GetWindowAttributes(win, atset);
      IF echo IN atset THEN
	 SetWindowAttributes(win, atset - WindowAtSet{echo});
      END;
      index := 0;
      WHILE (index <= HIGH(text)) AND Read(ch) AND
	    (skipping AND (ch IN skip) OR NOT (ch IN delim)) DO
	 CASE ch OF
	 | bs, del:
	    IF index > 0 THEN
	       DEC(index);
	       IF echo IN atset THEN
		  WindowWrite(win, bs); WindowWrite(win, ' ');
		  WindowWrite(win, bs);
	       END;
	    END;
	 ELSE
	    IF NOT (ch IN skip) THEN
	       skipping := FALSE;
	       text[index] := ch;
	       INC(index);
	    END;
	    IF echo IN atset THEN
	       WindowWrite(win, ch);
	    END;
	 END;
      END;
      IF index <= HIGH(text) THEN
	 text[index] := 0C;
      END;
      IF echo IN atset THEN
	 SetWindowAttributes(win, atset);
      END;
      termCH := ch;
   END ReadText;

   (* don't use ReadIntCard.Read because this would enforce use of *)
   (* global variables (procedure variables must be non-local).    *)
   (* To enable coroutine-usage of FtdWin text must be local.	   *)

   PROCEDURE Read(VAR w: WORD; t: Type; win: Window);
      CONST
	 AllChars = CharSet{MIN(CHAR)..MAX(CHAR)};
	 Digits = CharSet{'0'..'9'};
	 Delim = AllChars - Digits - CharSet{'+', '-'};
      VAR
         minus: BOOLEAN;
         arg: CARDINAL;
         ch: CHAR;
	 text: ARRAY[0..15] OF CHAR; index: CARDINAL;

      PROCEDURE ReadChar(VAR ch: CHAR);
      BEGIN
	 ch := text[index]; INC(index);
      END ReadChar;

   BEGIN
      ReadText(win, text, WhiteSpace, Delim); index := 0;
      minus := FALSE;
      REPEAT
         ReadChar(ch);
         IF (ch = '-') AND (t = int) THEN
            minus := NOT minus;
            ch := ' ';
         ELSIF ch = '+' THEN
            ch := ' ';
         END;
      UNTIL NOT (ch IN WhiteSpace);
      IF NOT (ch IN Digits) THEN
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

   PROCEDURE Blanks(win: Window;
                    VAR n: CARDINAL; min: CARDINAL; VAR ok: BOOLEAN);
      (* if n > min then write n-min blanks *)
   BEGIN
      ok := TRUE;
      WHILE (n > min) AND ok DO
         WindowWrite(win, " ");
         ok := Done;
         DEC(n);
      END;
   END Blanks;

   PROCEDURE WinReadInt(w: Window; VAR arg: INTEGER);
   BEGIN
      Read(arg, int, w);
   END WinReadInt;

   PROCEDURE WinWriteInt(win: Window; arg: INTEGER; w: CARDINAL);
      VAR
         field: ARRAY[0..10] OF CHAR;
         ok: BOOLEAN;
   BEGIN
      Blanks(win, w, HIGH(field)+1, ok);
      IF NOT ok THEN Done := FALSE; RETURN END;
      ConvertInteger(arg, w, field);
      WinWriteString(win, field);
   END WinWriteInt;

   PROCEDURE WinReadCard(w: Window; VAR arg: CARDINAL);
   BEGIN
      Read(arg, card, w);
   END WinReadCard;

   PROCEDURE WinWriteCard(win: Window; arg: CARDINAL; w: CARDINAL);
      VAR
         field: ARRAY[0..10] OF CHAR;
         ok: BOOLEAN;
   BEGIN
      Blanks(win, w, HIGH(field)+1, ok);
      IF NOT ok THEN Done := FALSE; RETURN END;
      ConvertCardinal(arg, w, field);
      WinWriteString(win, field);
   END WinWriteCard;

   PROCEDURE WinReadString(win: Window; VAR str: ARRAY OF CHAR);
   BEGIN
      ReadText(win, str, WhiteSpace - CharSet{nl}, WhiteSpace);
   END WinReadString;

   PROCEDURE WinReadLine(win: Window; VAR str: ARRAY OF CHAR);
   BEGIN
      ReadText(win, str, CharSet{}, CharSet{nl});
   END WinReadLine;

   PROCEDURE WinWriteString(win: Window; str: ARRAY OF CHAR);
      VAR cnt: CARDINAL;
   BEGIN
      cnt := 0;
      Done := TRUE;
      WHILE (cnt <= HIGH(str)) AND (str[cnt] <> 0C) DO
	 WindowWrite(win, str[cnt]);
         Done := Done AND Windows.Done;
         INC(cnt);
      END;
   END WinWriteString;

   PROCEDURE WinWriteLn(win: Window);
   BEGIN
      WindowWrite(win, nl);
      Done := Windows.Done;
   END WinWriteLn;

END FtdWin.
