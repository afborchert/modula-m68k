(****************************************
*                                       *
*     MODULA-2 Multi-Pass Compiler      *
*     ****************************      *
*                                       *
*     Implementation for MC68020/UNIX   *
*                                       *
*     MCP3IO:                           *
*                                       * 
*     input / output handling in Pass 3 *
*                                       * 
****************************************)

IMPLEMENTATION MODULE MCP3IO;           (* LG *)
                                        (* REV AFB 3/84 : UNIX I/O *)

   IMPORT SYSTEM, StdIO, MCHalfword, SysPerror, MCStop, Storage, MCBase,
      MCP3Public, FtdIO, SystemTypes;

  (* declarations in definition module
  TYPE Savepos;
  VAR sy : Symbol;
      val : CARDINAL;    (* value *)
      typeset: Stset;
      length : CARDINAL; (* string length *)
      spix : Spellix;    (* spelling index of identifier *)
      nptr : Idptr;      (* pointer to referenced name *)

  end declarations *)

   VAR 
      pos, line, lpos, lline : CARDINAL;

   MODULE OutputSystem;

      FROM SYSTEM IMPORT WORD;
      FROM Storage IMPORT ALLOCATE, DEALLOCATE;
      FROM StdIO IMPORT FILE, Fopen, Fclose, Fseek, Ftell, write;
      FROM FtdIO IMPORT FwriteWord;
      FROM MCHalfword IMPORT WriteHalfword;
      FROM MCStop IMPORT Stop;
      FROM SysPerror IMPORT Perror;
      FROM MCBase IMPORT Symbol;
      FROM MCP3Public IMPORT il1Name;
      FROM SystemTypes IMPORT OFF;
      IMPORT pos, line;

      EXPORT PutSy, PutWord, TermOutput, Savepos, InitSave, ResetSave,
         ReleaseSave;

      TYPE 
         Savepos = OFF;
         Symptr  = POINTER TO Symrec;
         Symrec  = RECORD 
            next : Symptr;
            elem : WORD;
         END;
         Remptr  = POINTER TO Remrec;
         Remrec  = RECORD 
            next : Remptr;
            sympos : Symptr;
            save : Savepos;
         END;

      VAR 
         il1              : FILE;
         symhead, symtail : Symptr;
         remhead, remtail : Remptr;
         savelevel        : CARDINAL;
         remcnt           : CARDINAL;
         saving           : BOOLEAN;
         remaining        : BOOLEAN;

      PROCEDURE SaveWord(w: WORD);
      BEGIN 
         WITH symtail^ DO 
            elem := w;
            IF next = NIL THEN 
               NEW(next);
               next^.next := NIL 
            END;
            symtail := next;
         END;
         DEC(remcnt);
         IF remcnt = 0 THEN 
            remaining := FALSE 
         END;
      END SaveWord;

      PROCEDURE PutSy(sy: Symbol);
      (* put symbol sy on output stream *)
         VAR 
            w : WORD;

      BEGIN 
         w := WORD(ORD(sy) * 400B + pos);
	 IF remaining THEN (* symbol of option *)
	    SaveWord(w);
         ELSIF saving THEN 
            CASE sy OF 
              eol, errorsy : 
                  remcnt := 2;
            | option : 
                  remcnt := 3;
            ELSE 
               remcnt := 0;
            END;
            remaining := remcnt > 0;
            IF remaining THEN 
               SetRemaining;
               SaveWord(w)
            END;
         END;
         WriteHalfword(il1, CARDINAL(w));
      END PutSy;

      PROCEDURE PutWord(w: WORD);
      (* put word w on output stream *)
      BEGIN 
         IF remaining THEN 
            SaveWord(w)
         END;
         FwriteWord(il1, w);
      END PutWord;

      PROCEDURE SetRemaining;
      BEGIN 
         WITH remtail^ DO 
            sympos := symtail;
            IF NOT Ftell(il1, save) THEN 
               Perror(il1Name);
               Stop(2);
            END;
            IF next = NIL THEN 
               NEW(next);
               next^.next := NIL 
            END;
            remtail := next;
         END;
      END SetRemaining;

      PROCEDURE ResetRemainings(s : Savepos);
         VAR 
            sym : Symptr;
            r   : Remptr;
            sy  : Symbol;

      BEGIN 
         r := remhead;
         WHILE (r <> remtail) AND (r^.save < s) DO 
            r := r^.next 
         END;
         WHILE r <> remtail DO 
            WITH r^ DO 
               sym := sympos;
               IF NOT Ftell(il1, save) THEN 
                  Perror(il1Name);
                  Stop(2);
               END;
               r := next;
            END;
            sy := VAL(Symbol,CARDINAL(sym^.elem) DIV 400B);
            WriteHalfword(il1, CARDINAL(sym^.elem));
            sym := sym^.next;
            PutWord(sym^.elem);
            IF sy = option THEN
               sym := sym^.next;
               WriteHalfword(il1, CARDINAL(sym^.elem));
            END;
         END;
      END ResetRemainings;

      PROCEDURE InitSave(VAR s: Savepos);
      BEGIN 
         IF saving THEN 
            INC(savelevel);
         ELSE 
            saving := TRUE;
            symtail := symhead;
            remtail := remhead;
            savelevel := 0;
         END;
         IF NOT Ftell(il1, s) THEN 
            Perror(il1Name);
            Stop(2);
         END;
      END InitSave;

      PROCEDURE ResetSave(s: Savepos);
      BEGIN 
         IF NOT Fseek(il1, s, 0) THEN 
            Perror(il1Name);
            Stop(2);
         END;
         ResetRemainings(s);
      END ResetSave;

      PROCEDURE ReleaseSave(s: Savepos);
      BEGIN 
         IF savelevel = 0 THEN 
            saving := FALSE;
         ELSE 
            DEC(savelevel);
         END;
      END ReleaseSave;

      PROCEDURE TermOutput;
      BEGIN 
         WHILE symhead <> NIL DO 
            symtail := symhead;
            symhead := symhead^.next;
            DISPOSE(symtail);
         END;
         WHILE remhead <> NIL DO 
            remtail := remhead;
            remhead := remhead^.next;
            DISPOSE(remtail);
         END;
         PutSy(eop);
         IF NOT Fclose(il1) THEN 
            Perror(il1Name);
            Stop(2);
         END;
      END TermOutput;

   BEGIN 
      NEW(symhead);
      symhead^.next := NIL;
      NEW(remhead);
      remhead^.next := NIL;
      saving := FALSE;
      remaining := FALSE;
      IF NOT Fopen(il1, il1Name, write, (* buffered = *) TRUE) THEN 
         Perror(il1Name);
         Stop(2);
      END;
   END OutputSystem;

   MODULE ErrorSystem;

      FROM MCBase IMPORT Symbol;
      FROM MCP3Public IMPORT ErrorsFound;
      FROM OutputSystem IMPORT PutSy, PutWord;
      IMPORT line, pos, lline, lpos;

      EXPORT Error,ErrorLS;

      CONST 
         errmax = 300;

      VAR 
         errcount : CARDINAL;

      PROCEDURE Error(n : CARDINAL);
      BEGIN 
         INC(errcount);
         ErrorsFound := TRUE;
         IF errcount < errmax THEN 
            PutSy(errorsy);
            PutWord(n);
         ELSIF errcount = errmax THEN 
            PutSy(errorsy);
            PutWord(5);                 (* too many errors *)
         END;
      END Error;

      PROCEDURE ErrorLS(n : CARDINAL);
         VAR 
            hpos : CARDINAL;
      BEGIN 
         hpos := pos;
         pos := lpos;
         IF lline <> line THEN 
            PutSy(eol);
            PutWord(lline);
            Error(n);
            PutSy(eol);
            PutWord(line);
         ELSE 
            Error(n);
         END;
         pos := hpos;
      END ErrorLS;

   BEGIN 
      errcount := 0;
      ErrorsFound := FALSE;
   END ErrorSystem;

   MODULE Scanner;

      FROM SYSTEM IMPORT WORD;
      FROM StdIO IMPORT FILE, Fopen, Fclose, read;
      FROM MCHalfword IMPORT ReadHalfword;
      FROM FtdIO IMPORT FreadWord, Done;
      FROM SysPerror IMPORT Perror;
      FROM MCStop IMPORT Stop;
      FROM MCBase IMPORT Idptr, Spellix, Symbol;
      FROM MCP3Public IMPORT il2Name;
      FROM OutputSystem IMPORT PutSy, PutWord;
      IMPORT sy, val, length, spix, nptr, pos, line, lpos, lline, typeset;

      EXPORT GetSy, PutGetSy, TermInput;

      VAR 
         card : CARDINAL;
         il2  : FILE;
         issy : BOOLEAN;

      PROCEDURE ReadWord(VAR w: WORD);
      BEGIN
         FreadWord(il2, w);
         IF NOT Done THEN
            Perror(il2Name);
            Stop(2);
         END;
      END ReadWord;

      PROCEDURE GetSy;
         VAR dummy: CARDINAL;
      BEGIN                             (* get next symbol *)
         lpos := pos;
         lline := line;
         REPEAT 
            issy := TRUE;
            ReadHalfword(il2, card);
            pos := card MOD 400B;
            sy := VAL(Symbol,card DIV 400B);
            CASE sy OF 
            | ident: 
                  ReadWord(spix);
            | namesy,proceduresy,modulesy,symbolsy,definitionsy: 
                  ReadWord( dummy);
                  nptr := Idptr(dummy);
            | intcon,cardcon,charcon,realcon: 
                  ReadWord(val);
	    | intcarcon:
		  ReadWord(typeset);
		  ReadWord(val);
            | stringcon: 
                  ReadWord(val);
                  ReadWord(length);
            | option: 
                  ReadWord(val);
                  ReadHalfword(il2, card);
                  PutSy(sy);
                  PutWord(val);
                  PutSy(Symbol(card DIV 100H));
                  issy := FALSE;
            | errorsy,eol: 
                  ReadWord(val);
                  IF sy = eol THEN 
                     line := val 
                  END;
                  PutSy(sy);
                  PutWord(val);
                  issy := FALSE;
            ELSE                        (* no activity *)
            END;                        (* CASE *)
         UNTIL issy;
      END GetSy;

      PROCEDURE PutGetSy;
      BEGIN                             (* put last Symbol, get next Symbol *)
         PutSy(sy);
         CASE sy OF 
         | namesy,proceduresy,modulesy: 
               PutWord(nptr);
         ELSE                           (* no activity *)
         END;                           (* CASE *)
         GetSy;
      END PutGetSy;

      PROCEDURE TermInput;
      BEGIN 
         IF NOT Fclose(il2) THEN 
            Perror(il2Name);
            Stop(2);
         END;
      END TermInput;

   BEGIN 
      IF NOT Fopen(il2, il2Name, read, (* buffered = *) TRUE) THEN 
         Perror(il2Name);
         Stop(2);
      END;
      line := 1;
   END Scanner;

   PROCEDURE TermInOut;
   BEGIN 
      TermInput;
      TermOutput;
   END TermInOut;

END MCP3IO. 
