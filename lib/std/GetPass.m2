IMPLEMENTATION MODULE GetPass;

   FROM SysIoctl IMPORT Sgttyb, Cbreak, Echo, Gtty, Stty, Isatty;
   FROM SysOpen IMPORT Open;
   FROM FtdIO IMPORT FreadChar, FwriteChar, FwriteString, FwriteLn;
   FROM ASCII IMPORT nl, nak, bs;
   FROM RandomGenerator IMPORT Random;
   FROM StdIO IMPORT FILE, stdin, stdout, Fdopen, Fclose, read, write;

   PROCEDURE GetPass(prompt: ARRAY OF CHAR; VAR passwd: ARRAY OF CHAR);
      CONST
         MaxPasswdLen = 14;
         Terminal = "/dev/tty";
         Read = 0;
         Write = 1;
      VAR
         index: CARDINAL; (* in passwd *)
         oldterm, term: Sgttyb;
         ch: CHAR; (* last character read *)
         bslen: ARRAY[0..MaxPasswdLen-1] OF CARDINAL;
         i: CARDINAL;
         low, high: CARDINAL;
         termin, termout: FILE;
         termfdin: CARDINAL; (* file descriptor of terminal input *)
         termfdout: CARDINAL;
         terminal: BOOLEAN;
   BEGIN
      terminal := Isatty(0) AND Isatty(1);
      IF terminal THEN
         termin := stdin; termout := stdout;
         termfdin := 0; termfdout := 1;
      ELSIF NOT Open(termfdin, Terminal, Read) OR
            NOT Open(termfdout, Terminal, Write) OR
            NOT Fdopen(termin, termfdin, read, (* buffered = *) FALSE) OR
            NOT Fdopen(termout, termfdout, write, (* buffered = *) FALSE) THEN
         RETURN
      END;
      FwriteString(termout, prompt);
      IF NOT Gtty(termfdin, term) THEN RETURN END;
      oldterm := term;
      term.flags := term.flags - Echo + Cbreak;
      IF NOT Stty(termfdin, term) THEN RETURN END;
      index := 0;
      REPEAT
         FreadChar(termin, ch);
         CASE ch OF
         | nl: FwriteChar(termout, ch);
         | bs:
             IF (index > 0) THEN
                DEC(index);
                FOR i := 1 TO bslen[index] DO
                   FwriteChar(termout, bs);
                END;
             END;
         | nak: (* ^U *)
             index := 0;
             FwriteLn(termout);
             FwriteString(termout, prompt);
         ELSE
             passwd[index] := ch;
             bslen[index] := Random(1, 3);
             low := ORD('!'); high := ORD('~');
             FOR i := 1 TO bslen[index] DO
                FwriteChar(termout, CHR(Random(low, high)));
             END;
             INC(index);
         END;
      UNTIL (ch = nl) OR (index > HIGH(passwd)) OR (index > MaxPasswdLen)
            OR (index > HIGH(bslen));
      IF ch <> nl THEN
         FwriteChar(termout, nl);
      END;
      IF index <= HIGH(passwd) THEN
         passwd[index] := 0C;
      END;
      IF NOT Stty(termfdin, oldterm) THEN RETURN END;
      IF NOT terminal AND Fclose(termin) AND Fclose(termout) THEN END;
   END GetPass;

END GetPass.
