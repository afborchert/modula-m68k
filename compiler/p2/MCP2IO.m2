(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
IMPLEMENTATION MODULE MCP2IO; (* LG / AFB *)

   (* $T- *)

   IMPORT SYSTEM, StdIO, FtdIO, MCHalfword, MCBase, MCP2Public,
      SysPerror, MCStop;
   FROM MCBase IMPORT Keyarr;

   VAR 
      lpos, lline: CARDINAL;

   MODULE OutputSystem;

      FROM SYSTEM IMPORT WORD;
      FROM StdIO IMPORT FILE, write, Fopen, Fclose;
      FROM FtdIO IMPORT FwriteWord;
      FROM MCHalfword IMPORT WriteHalfword;
      FROM SysPerror IMPORT Perror;
      FROM MCStop IMPORT Stop;
      FROM MCBase IMPORT Symbol;
      FROM MCP2Public IMPORT il2Name;
      IMPORT pos;

      EXPORT il2, PutSy, PutWord, StopOutput, RestartOutput, TermOutput;

      VAR 
         il2    : FILE;
         output : BOOLEAN;              (* output on il2 allowed *)

      PROCEDURE PutSy(s : Symbol);
	 (* put Symbol and pos on il2-file *)
	 (* pack symbol with pos into two bytes *)
      BEGIN 
         IF output THEN 
            WriteHalfword(il2, ORD(s) * 400B + pos)
         END 
      END PutSy;

      PROCEDURE PutWord(w : WORD);
	 (* put word on il2-file *)
      BEGIN 
         IF output THEN 
            FwriteWord(il2, w)
         END 
      END PutWord;

      PROCEDURE StopOutput;
      BEGIN 
         output := FALSE 
      END StopOutput;

      PROCEDURE RestartOutput;
      BEGIN 
         output := TRUE 
      END RestartOutput;

      PROCEDURE TermOutput;
      BEGIN 
         PutSy(eop);
         IF NOT Fclose(il2) THEN 
            Perror(il2Name);
            Stop(2);
         END;
      END TermOutput;

   BEGIN 
      IF NOT Fopen(il2, il2Name, write, (* buffered = *) TRUE) THEN 
         Perror(il2Name);
         Stop(2);
      END;
      output := TRUE;
   END OutputSystem;

   MODULE ErrorSystem;

      FROM MCP2Public IMPORT ErrorsFound;
      FROM MCHalfword IMPORT WriteHalfword;
      FROM FtdIO IMPORT FwriteWord;
      FROM MCBase IMPORT Symbol;
      IMPORT line, pos, lline, lpos, OutputSystem;

      EXPORT Error, ErrorLS;

      CONST 
         errmax = 300;

      VAR 
         errsymval, eolsymval : CARDINAL;
         errcount             : CARDINAL;

      PROCEDURE Error(n: CARDINAL);
      (* no suppression of writing on il2 file *)
      BEGIN 
         INC(errcount);
         ErrorsFound := TRUE;
         IF errcount < errmax THEN 
            WriteHalfword(il2,errsymval + pos); (* pack symbols and pos into two bytes *)
            FwriteWord(il2,n);
         ELSIF errcount = errmax THEN 
            WriteHalfword(il2,errsymval);
            FwriteWord(il2,5);       (* too many errors *)
         END;
      END Error;

      PROCEDURE ErrorLS(n: CARDINAL);
         VAR 
            hpos : CARDINAL;
      BEGIN 
         hpos := pos;
         pos := lpos;
         IF lline <> line THEN 
            WriteHalfword(il2,eolsymval);
            FwriteWord(il2,lline);
            Error(n);
            WriteHalfword(il2,eolsymval);
            FwriteWord(il2,line);
         ELSE 
            Error(n);
         END;
         pos := hpos;
      END ErrorLS;

   BEGIN 
      errcount := 0;
      ErrorsFound := FALSE;
      errsymval := ORD(errorsy) * 400B; (* value for errorsy *)
      eolsymval := ORD(eol) * 400B;     (* value for eol *)
   END ErrorSystem;

   MODULE Scanner;

      FROM SYSTEM IMPORT WORD;
      FROM StdIO IMPORT FILE, read, Fopen, Fclose;
      FROM MCHalfword IMPORT ReadHalfword, WriteHalfword;
      FROM FtdIO IMPORT FwriteWord, FreadWord, Done;
      FROM SysPerror IMPORT Perror;
      FROM MCStop IMPORT Stop;
      FROM MCBase IMPORT Symbol, Spellix;
      FROM MCP2Public IMPORT il1Name;
      IMPORT ErrorSystem, OutputSystem, sy, spix, maxspix, val, length, pos,
         line, lpos, lline, typeset;

      EXPORT GetSy, PutGetSy, TermInput;

      VAR 
         card : CARDINAL;
         il1  : FILE;
         issy : BOOLEAN;

      PROCEDURE ReadWord(VAR w: WORD);
      BEGIN
         FreadWord(il1, w);
         IF NOT Done THEN
            Perror("ReadWord");
            Stop(2);
         END;
      END ReadWord;

      PROCEDURE GetSy;
      BEGIN                             (* get next Symbol *)
         lpos := pos;
         lline := line;
         REPEAT 
            issy := TRUE;
            ReadHalfword(il1, card);
            pos := card MOD 400B;
            sy := VAL(Symbol,card DIV 400B);
            CASE sy OF 
            | ident: 
                  ReadWord(spix);
            | intcon,cardcon,charcon,realcon,bigsetcon: 
                  ReadWord(val);
	    | intcarcon: (* revision in interpass file *)
		  ReadWord(typeset);
		  ReadWord(val);
            | stringcon: 
                  ReadWord(val);
                  ReadWord(length);
            | option: 
                  ReadWord(val);
                  ReadHalfword(il1, card);
                  PutSy(option);
                  PutWord(val);
                  PutSy(Symbol(card DIV 100H));
                  issy := FALSE;
            | errorsy,eol: 
                  ReadWord(val);
                  IF sy = eol THEN 
                     line := val 
                  END;
                  WriteHalfword(il2,card); (* errorsy or eol *)
                  FwriteWord(il2,val);
		  (* no suppression *)
                  issy := FALSE;
            ELSE                        (* no activity *)
            END;                        (* case *)
         UNTIL issy;
      END GetSy;

      PROCEDURE PutGetSy;
      BEGIN                             (* put last Symbol, get next Symbol *)
         PutSy(sy);
         IF sy = ident THEN 
            PutWord(spix)
         ELSIF (sy >= intcon) AND (sy <= stringcon) THEN 
            IF (sy = intcarcon) THEN
               PutWord(typeset);
            END;
            PutWord(val);
            IF sy = stringcon THEN 
               PutWord(length)
            END 
         END;
         GetSy;
      END PutGetSy;

      PROCEDURE TermInput;
      BEGIN 
         IF NOT Fclose(il1) THEN 
            Perror(il1Name);
            Stop(2);
         END;
      END TermInput;

   BEGIN 
      IF NOT Fopen(il1, il1Name, read, (* buffered = *) TRUE) THEN 
         Perror(il1Name);
         Stop(2);
      END;
      line := 1;
      pos := 1;
   END Scanner;

   MODULE AsciiHandling;                (* $T- *)
      (* handling with the identifier-file ASCII *)

      FROM StdIO IMPORT FILE, Fopen, read, Fclose, Fseek, Fgetc;
      FROM SysPerror IMPORT Perror;
      FROM MCStop IMPORT Stop;
      FROM MCBase IMPORT Spellix;
      FROM MCP2Public IMPORT ascName;
      IMPORT maxspix;

      EXPORT AsciiSetPos, AsciiRead, TermAscii;

      VAR 
         asc : FILE;
	 returnDummy: BOOLEAN;

      PROCEDURE AsciiSetPos(spix: Spellix);
      (* set position on ASCII file *)
      BEGIN 
	 IF spix >= maxspix THEN
	    returnDummy := TRUE;
	 ELSE
	    returnDummy := FALSE;
	    IF NOT Fseek(asc, INTEGER(spix), 0) THEN 
	       Perror(ascName);
	       Stop(2);
	    END;
	 END;
      END AsciiSetPos;

      PROCEDURE AsciiRead(VAR ch: CHAR);
      (* read character from ASCII file *)
      BEGIN 
	 IF returnDummy THEN
	    ch := ' '; (* end of identifier in ascii-file *)
	    RETURN
	 END;
         IF NOT Fgetc(ch, asc) THEN 
            Perror(ascName);
            Stop(2);
         END;
      END AsciiRead;

      PROCEDURE TermAscii;
      BEGIN 
         IF NOT Fclose(asc) THEN 
            Perror(ascName);
            Stop(2);
         END;
      END TermAscii;

   BEGIN                                (* AsciiHandling *)
      IF NOT Fopen(asc, ascName, read, (* buffered = *) TRUE) THEN 
         Perror(ascName);
         Stop(2);
      END;
   END AsciiHandling;                   (* $T= *)

   MODULE SkipInSymbolModule;

      FROM MCBase IMPORT Symbol;
      FROM Scanner IMPORT GetSy;
      IMPORT sy;

      EXPORT SkipConstant, SkipType;

      PROCEDURE Skip(s: Symbol);
      BEGIN 
         WHILE sy <> s DO 
            GetSy 
         END;
         GetSy;
      END Skip;

      PROCEDURE SkipQualIdent;
      BEGIN 
         IF sy = ident THEN 
            GetSy;
            WHILE sy = period DO 
               GetSy;
               GetSy 
            END;
         END;
      END SkipQualIdent;

      PROCEDURE SkipConstant;
	 (* skip constant in a symbol module *)
      BEGIN 
         IF sy = cardcon THEN 
            GetSy;
            SkipQualIdent;
         ELSIF (sy = stringcon) OR (sy = realcon) OR (sy = intcarcon) THEN 
            GetSy;
         END;
      END SkipConstant;

      PROCEDURE SkipType;
	 (* skip type structures in a symbol module *)

         PROCEDURE SkipVariants;
	    (* skip variant structures *)
         BEGIN 
            IF sy = casesy THEN 
               Skip(colon);
               SkipQualIdent;
               WHILE sy = ofsy DO 
                  Skip(colon);
                  SkipVariants;
                  GetSy;                (* size *)
               END;
               IF sy = elsesy THEN 
                  GetSy;
                  SkipVariants;
                  GetSy;                (* size *)
               END;
               GetSy;                   (* endsy *)
            END;
         END SkipVariants;

      BEGIN                             (* SkipType *)
         CASE sy OF 
         | arraysy: 
               Skip(ofsy);
               SkipType;
         | recordsy: 
               GetSy;
               WHILE sy = ident DO 
                  Skip(colon);
                  SkipType 
               END;
               SkipVariants;
               GetSy;                   (* endsy *)
               GetSy;                   (* size *)
         | setsy,pointersy: 
               GetSy;
               SkipType;
         | proceduresy: 
               Skip(rparent);
               IF sy = colon THEN 
                  GetSy;
                  SkipType 
               END;
         | hidden: 
               GetSy;
         | lparent: 
               Skip(rparent);
         | ident: 
               SkipQualIdent;
               (* revision in interpass file *)
               IF sy = lbrack THEN Skip(rbrack) END;
         | lbrack: 
               Skip(rbrack);
         END;
      END SkipType;

   END SkipInSymbolModule;

   PROCEDURE GetModuleKey(VAR modkey: Keyarr);
      VAR 
         ix : CARDINAL;
   BEGIN (* dummy implementation *)
      FOR ix := 0 TO 2 DO 
         modkey[ix] := 0;
      END;
   END GetModuleKey;

   PROCEDURE DefModStatus;
   BEGIN 
   END DefModStatus;

   PROCEDURE TermInOut;
   BEGIN 
      TermInput;
      TermOutput;
      TermAscii;
   END TermInOut;

END MCP2IO. 
