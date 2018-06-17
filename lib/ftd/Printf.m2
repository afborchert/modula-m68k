IMPLEMENTATION MODULE Printf;

   FROM SYSTEM IMPORT ADDRESS, BYTE, ADR;
   FROM Conversions IMPORT ConvertHex, ConvertInteger, ConvertOctal,
      ConvertCardinal;
   FROM RealConv IMPORT WriteFloat, WriteFix;
   FROM Chars IMPORT CharSet, DigitS, esc, lf, bs, nul, bel, ff ,cr, tab, sp,
      UpperS, Lower, OctDigitS;
   FROM LongStrings IMPORT Long, AddChar, AddBytes, ValidLong;
   FROM Bytes IMPORT PINC;

   IMPORT SYSTEM, Bytes, Chars, Strings, Errno, SysPerror;

   CONST 
      MaxWidth = 2048;      (* limits field width (used for error detection) *)
      MaxNumLen= 64;         (* assumed maximum of digit                     *)

   MODULE FmtError;

      FROM Strings IMPORT StrCpy, StrCat;
      FROM Errno IMPORT errno;
      FROM SysPerror IMPORT GetErrorString;
      IMPORT FmtExitCode, FmtExitSet, SynError, OutError, TellSyntax,
         TellOutput, AbortSyntax, AbortOutput, WabortP, WabortA, Sabort, Eabort,
	 Pabort,Fabort,Labort;
      EXPORT GetReason, ErrorReaction;

      CONST 
         ParmError  = FmtExitSet {AllocFailed, StringTooSmall, WindowTooSmall 
            ,IllegalWindowOffset};

         WriteError = FmtExitSet {CannotWriteFile, CannotWriteStderr 
            , CannotWriteStdout};
         WindowError = FmtExitSet {CannotFlushWindow, CannotAccessWindow,
            CannotWriteWindow};

      PROCEDURE GetReason(reason : FmtExitCode; VAR text : ARRAY OF CHAR);

         VAR 
            perror : ARRAY[0..39] OF CHAR;

      BEGIN 
         IF reason IN WriteError THEN 
            GetErrorString(errno,text);
            StrCat(text,": ");
         ELSIF reason IN ParmError THEN 
            StrCpy(text,"Illegal parameter values: ");
	 ELSIF reason IN WindowError THEN
	    StrCpy(text,"Failure on window operation: ");
         ELSIF reason IN SynError THEN 
            StrCpy(text,"Bad Format: ");
         ELSE 
            StrCpy(text,"");
         END;
         CASE reason OF 
         | BslashAnd0: 
               StrCat(text,"illegal combination of '0' and '\\'.");
         | BadOctalChar : 
               StrCat(text,"bad octal character specification.");
         | Success: 
               StrCat(text,"Success.");
         | CannotWriteStderr : 
               StrCat(text,"cannot write to stderr.");
         | AllocFailed: 
               StrCat(text,"invalid long.");
         | FmtPanic: 
               StrCat(text,"internal fatal error.");
         | CannotWriteStdout : 
               StrCat(text,"cannot write to stdout.");
         | CannotWriteFile : 
               StrCat(text,"cannot write to file.");
         | StringTooSmall : 
               StrCat(text,"string too small.");
         | IllegalWidth : 
               StrCat(text,"illegal width.");
         | TooFewFormElems : 
               StrCat(text,"too few format elements.");
         | TooManyFormElems : 
               StrCat(text,"too many format elements.");
         | IllegalConvChar : 
               StrCat(text,"illegal conversion character.");
         | Undefined : 
               StrCat(text,"Undefined error code.");
         | MinHat0Comb : 
               StrCat(text,"illegal combination of '-', '^' and '0'.");
         | IllegalWindowOffset:
	       StrCat(text,"offset out of window.");
	 | WindowTooSmall:
	       StrCat(text,"window too small.");
	 | CannotFlushWindow:
	       StrCat(text,"cannot flush.");
	 | CannotWriteWindow:
	       StrCat(text,"cannot write %oC into window.");
         | CannotAccessWindow:
	       StrCat(text,"cannot access window.");
         ELSE 
            StrCat(text,"Unknown error code.");
         END;
      END GetReason;

      PROCEDURE ErrorReaction(reason : FmtExitCode; mode : BITSET; no : 
         CARDINAL; VAR exit : INTEGER; VAR fmt : ARRAY OF CHAR);

         VAR 
            tell, abort : BOOLEAN;
            text        : ARRAY[0..255] OF CHAR;
      BEGIN 
         tell := ((reason IN SynError) AND (TellSyntax IN mode)) OR ((reason 
            IN OutError) AND (TellOutput IN mode));
         abort := ((reason IN SynError) AND (AbortSyntax IN mode)) OR (
            (reason IN OutError) AND (AbortOutput IN mode));
         IF tell THEN 
            GetReason(reason,text);
            CASE exit OF 
              WabortP: 
                  StrCpy(fmt, "W.printf%u(...,%u,%u,\Q%s\Q");
            | WabortA: 
                  StrCpy(fmt, "W.append%u(...,%u,%u,\Q%s\Q");
            | Sabort: 
                  StrCpy(fmt, "S.printf%u(...,\Q%s\Q");
            | Fabort: 
                  StrCpy(fmt, "F.printf%u(...,\Q%s\Q");
            | Pabort: 
                  StrCpy(fmt, "P.rintf%u(\Q%s\Q");
            | Labort: 
                  StrCpy(fmt, "L.printf%u(...,\Q%s\Q" );
            | Eabort: 
                  StrCpy(fmt, "E.rror%u(%u,\Q%s\Q" );
            END;
            IF no > 0 THEN 
               StrCat(fmt,",...");
            END;
            StrCat(fmt,");\n        ");
            StrCat(fmt,text);
            StrCat(fmt,"\n");
	 ELSE
	    StrCpy(fmt,"");
         END;
	 IF NOT abort THEN
	    exit := 0;
	 END;
      END ErrorReaction;

   END FmtError;

   MODULE ConvByAdd;

      FROM SYSTEM IMPORT ADDRESS, BYTE, ADR;
      FROM Bytes IMPORT ByteNCopy, PINC;
      FROM Chars IMPORT nul;

      EXPORT AddToInt, AddToCard, AddToChar, AddToReal, AddToBool, AddToAdd,
	 AddToLineChar;

      PROCEDURE AddToBool(add : ADDRESS; sz : CARDINAL) : BOOLEAN;

         VAR 
            p : POINTER TO BYTE;

      BEGIN 
         WHILE sz > 0 DO 
            p := add;
            IF CHAR(p^) # nul THEN 
               RETURN TRUE;
            END;
            DEC(sz);
            INC(add);
         END;
         RETURN FALSE;
      END AddToBool;

      PROCEDURE AddToLineChar (add : ADDRESS) : CHAR;

      VAR
	 cp : POINTER TO CHAR;

      BEGIN
	 cp := add;
	 RETURN cp^;
      END AddToLineChar;

      PROCEDURE AddToInt (add : ADDRESS; sz : CARDINAL) : INTEGER;

         VAR 
            int : INTEGER;

      BEGIN 
         int := 0;
         Copy(add,ADR(int),sz,SIZE(INTEGER));
         RETURN int;
      END AddToInt;

      PROCEDURE AddToCard (add : ADDRESS; sz : CARDINAL) : CARDINAL;

         VAR 
            card : CARDINAL;

      BEGIN 
         card := 0;
         Copy(add,ADR(card),sz,SIZE(CARDINAL));
         RETURN card;
      END AddToCard;

      PROCEDURE AddToAdd (add : ADDRESS; sz : CARDINAL) : ADDRESS;

         VAR 
            addr : ADDRESS;

      BEGIN 
         addr := ADDRESS(0);
         Copy(add,ADR(addr),sz,SIZE(ADDRESS));
         RETURN addr;
      END AddToAdd;

      PROCEDURE Copy(from, to : ADDRESS; sizefrom, sizeto : CARDINAL);

      BEGIN 
         IF sizefrom = 0 THEN 
            RETURN;
         END;
         IF sizefrom = sizeto THEN      (* %c ==> 'c' *)
         ELSIF sizefrom < sizeto THEN   (* %u ==> 'c' *)
         (* shift difference right *)
            INC(to,sizeto-sizefrom);
         ELSE                           (* %c ==> 123 *)
         (* sizefrom > sizeto *)
            INC(from,sizefrom-sizeto);
            sizefrom := sizeto;
         END;
         ByteNCopy(to,from,sizefrom);
      END Copy;

      PROCEDURE AddToChar (add : ADDRESS; sz : CARDINAL) : CHAR;

         VAR 
            char : CHAR;

      BEGIN 
         char := nul;
         Copy(add,ADR(char),sz,SIZE(CHAR));
         RETURN char;
      END AddToChar;

      PROCEDURE AddToReal (add : ADDRESS; sz : CARDINAL) : REAL;

         VAR 
            real : REAL;

      BEGIN 
         real := 0.0;
         Copy(add,ADR(real),sz,SIZE(REAL));
         RETURN real;
      END AddToReal;

   BEGIN 
   END ConvByAdd;

   PROCEDURE Printf(output : Long; no: CARDINAL; VAR fmt: ARRAY OF CHAR;
      VAR i1 ,i2,i3, i4, i5,i6,i7,i8 : ARRAY OF BYTE) : FmtExitCode;

      CONST 
         NumFmt = CharSet {'f','e','o','d','x','u'};
         SigFmt = NumFmt - CharSet {'x','o'};

      VAR 
         c       : CHAR;                (* currently treated charcter of fmt *)
         cidx    : CARDINAL;            (* its index *)
         i       : ADDRESS;        (* address of currently treated parameter *)
         iidx    : CARDINAL;            (* no. of that parameter *)
         size    : CARDINAL;            (* size (in bytes) of it *)
         add     : ADDRESS;             (* address of object to be output *)
         sz      : CARDINAL;
         min     : CARDINAL;
         max     : CARDINAL;
         octal   : CARDINAL;
         k       : CARDINAL;
         fill    : CHAR;
         first   : CHAR;
         isnum   : BOOLEAN;
         left    : BOOLEAN;
         upperS  : BOOLEAN;
         between : BOOLEAN;
         limit   : BOOLEAN;
         sign    : BOOLEAN;            (* output of a signed number required *)
         string  : ARRAY[0..MaxNumLen-1] OF CHAR;
                                   (* result of numeric conversions put here *)

      PROCEDURE FindNull(limit : CARDINAL) : CARDINAL;

         VAR 
            cp : POINTER TO CHAR;
            i  : CARDINAL;

      BEGIN 
         cp := add;
	 FOR i := 0 TO limit -1 DO
	    IF cp^ = nul THEN
	       RETURN i;
	    END;
	    PINC(cp,1);
	 END;
	 RETURN limit;
      END FindNull;

      PROCEDURE NextCh;

      BEGIN 
         IF cidx > HIGH(fmt) THEN 
            c := nul;
         ELSE 
            c := fmt[cidx];
            INC(cidx);
         END;
      END NextCh;

      PROCEDURE NextIdx() : BOOLEAN;

      BEGIN 
         IF iidx >= no THEN 
            RETURN FALSE;
         ELSE 
            CASE iidx OF 
              0: 
                  i := ADR(i1);
                  size := HIGH(i1) + 1;
            | 1: 
                  i := ADR(i2);
                  size := HIGH(i2) + 1;
            | 2: 
                  i := ADR(i3);
                  size := HIGH(i3) + 1;
            | 3: 
                  i := ADR(i4);
                  size := HIGH(i4) + 1;
            | 4: 
                  i := ADR(i5);
                  size := HIGH(i5) + 1;
            | 5: 
                  i := ADR(i6);
                  size := HIGH(i6) + 1;
            | 6: 
                  i := ADR(i7);
                  size := HIGH(i7) + 1;
            | 7: 
                  i := ADR(i8);
                  size := HIGH(i8) + 1;
            END;
            INC(iidx);
            RETURN TRUE;
         END;
      END NextIdx;

      PROCEDURE GetNum(VAR num : CARDINAL) : BOOLEAN;

      BEGIN 
         num := 0;
         IF c = '*' THEN 
            IF NOT NextIdx() THEN 
               RETURN FALSE;
            ELSE 
            (* shift *)
               num := AddToCard(add,sz);
               add := i;
               sz := size;
               NextCh;
            END;
         ELSE 
            WHILE c IN DigitS DO 
               num := num * 10 + ORD(c) - ORD('0');
               NextCh;
            END;
         END;
         RETURN TRUE;
      END GetNum;

      PROCEDURE FormatString();

      (* context: add  : address of string to be formatted and output *)
      (*          sz   : size of that string                          *)
      (*          first: it may contain the sign of a number          *)
      (*          fill : character for left-hand filling              *)
      (*          min  : minimum output length                        *)
      (*          max  : maximum output length (if limit = TRUE)      *)
      (*          left : if TRUE, output is left adjusted             *)
      (*          isnum: if TRUE, output a number (must no be cut)    *)

         VAR 
            strlen : CARDINAL;
            nofill : CARDINAL;

         PROCEDURE Fill();

         BEGIN 
            WHILE nofill > 0 DO 
               AddChar(output,fill);
               DEC(nofill);
            END;
         END Fill;

      BEGIN 
         IF isnum THEN 
            limit := FALSE;
            IF first = nul THEN 
               strlen := sz;
            ELSE 
               strlen := sz + 1;
            END;
         ELSE 
            IF limit AND (sz > max) THEN 
               sz := max;
            END;
            strlen := sz;
         END;
         IF min > strlen THEN           (* must fill *)
            nofill := min - strlen;     (* nofill chars *)
         ELSE 
            nofill := 0;
         END;
         IF isnum AND (left OR between) AND (first # nul) THEN 
            AddChar(output,first);
            first := nul;
         END;
         IF NOT left OR between THEN 
            Fill;
         END;
         IF first # nul THEN 
            AddChar(output,first);
         END;
         AddBytes(output,add,sz);
         Fill;
      END FormatString;

   BEGIN 
      cidx := 0;
      iidx := 0;
      sign := FALSE;
      NextCh;
      LOOP 
         CASE c OF 
           nul : 
               EXIT;
         | '\': 
               NextCh;
               CASE c OF 
                 'n' : 
                     AddChar(output,lf);
               | 'b' : 
                     AddChar(output,bs);
               | 'Q' : 
                     AddChar(output,'"');
               | 'q' : 
                     AddChar(output,"'");
               | 't' : 
                     AddChar(output,tab);
               | 'e' : 
                     AddChar(output,esc);
               | 'f' : 
                     AddChar(output,ff);
               | 'r' : 
                     AddChar(output,cr);
               | '&' : 
                     AddChar(output,bel);
               | '0'..'7' : 
                     octal := 0;
                     WHILE c IN OctDigitS DO 
                        octal := octal * 8 + ORD(c) - ORD('0');
                        NextCh;
                     END;
                     IF (c # 'C') OR (octal > ORD(MAX(CHAR))) THEN 
                        RETURN BadOctalChar;
                     ELSE 
                        AddChar(output,VAL(CHAR,octal));
                     END;
               ELSE 
                  AddChar(output,c);
               END;
               NextCh;
         |'%': 
               NextCh;
               IF c='%' THEN 
                  AddChar(output,c);
                  NextCh;
               ELSE 
                  IF NOT NextIdx() THEN 
                     RETURN TooManyFormElems;
                  ELSE 
                     add := i;
                     sz := size;
                  END;
                  IF c = '+' THEN 
                     sign := TRUE;
                     NextCh;
                  END;
                  fill := sp;
                  left := FALSE;
                  between := FALSE;
                  IF c = '-' THEN 
                     left := TRUE;
                     NextCh;
                  ELSIF c ='^' THEN 
                     between := TRUE;
                     left := TRUE;
                     NextCh;
		  ELSIF c ='0' THEN 
                     between := TRUE;
                     fill := '0';       (* '0' implies '-' *)
                     NextCh;
                  END;
                  IF c IN CharSet{'^','-','0'} THEN 
                     RETURN MinHat0Comb;
                  ELSIF c = '\' THEN 
                     IF fill = '0' THEN 
                        RETURN BslashAnd0;
                     END;
                     NextCh;
                     fill := c;
                     NextCh;
                  END;
                  min := 0;
                  IF NOT GetNum(min) THEN 
                     RETURN TooManyFormElems;
                  ELSIF min > MaxWidth THEN 
                     RETURN IllegalWidth;
                  END;
                  limit := (c = '.');
                  IF limit THEN 
                     NextCh;
                     IF NOT GetNum(max) THEN 
                        RETURN TooManyFormElems;
                     END;
                  END;
                  upperS := FALSE;
                  IF c IN UpperS THEN 
                     add := AddToAdd(add,sz);
                     CASE c OF 
                       'D' : 
                           sz := SIZE(INTEGER);
                     | 'C','L' : 
                           sz := SIZE(CHAR);
                     | 'X','U','O' : 
                           sz := SIZE(CARDINAL);
                     | 'B' , 'J' , 'Y' : 
                           sz := SIZE(BOOLEAN);
                     | 'S' : 
                           upperS := TRUE;
                           sz := FindNull(MAX(CARDINAL));
                     | 'F', 'E': 
                           sz := SIZE(REAL);
                     ELSE 
                        RETURN IllegalConvChar;
                     END;
                     Lower(c);
                  END;
                  IF c # 's' THEN 
                     CASE c OF 
                       'd' : 
                           ConvertInteger(AddToInt(add,sz),0,string);
                     | 'f' : 
                              (* implementation of real conversions is crude *)
                           IF (max = 0) AND NOT limit THEN 
                              k := 20;
                           ELSE 
                              k := max;
                           END;
                           WriteFix(string,AddToReal(add,sz),10,k);
                     | 'e' : 
                           IF (max = 0) AND NOT limit THEN 
                              max := 30;
                           ELSE 
                              max := 7+max;
                           END;
                           WriteFloat(string,AddToReal(add,sz),10,max);
                     | 'x' : 
                           ConvertHex(AddToCard(add,sz),0,string);
                     | 'o' : 
                           ConvertOctal(AddToCard(add,sz),0,string);
                     | 'u' : 
                           ConvertCardinal(AddToCard(add,sz),0,string);
                     | 'c' : 
                           string[0] := AddToChar(add,sz);
                           string[1] := nul;
                     | 'l' : 
                           string[0] := AddToLineChar(add);
                           string[1] := nul;
                           fill := string[0];
                           c := 'c';
                     | 'b' : 
                           IF AddToBool(add,sz) THEN 
                              string := "TRUE";
                           ELSE 
                              string := "FALSE";
                           END;
                     | 'y' : 
                           IF AddToBool(add,sz) THEN 
                              string := "yes";
                           ELSE 
                              string := "no";
                           END;
                     | 'j' : 
                           IF AddToBool(add,sz) THEN 
                              string := "ja";
                           ELSE 
                              string := "nein";
                           END;
                     ELSE 
                        RETURN IllegalConvChar;
                     END;
                     add := ADR(string);
                     sz := FindNull(MaxNumLen);
                  ELSIF NOT upperS THEN 
                     sz := FindNull(sz);
                  END;
                  first := nul;
                  IF c IN SigFmt THEN 
                     IF string[0] = '-' THEN 
                        first := '-';
                        DEC(sz);
                        INC(add);
                     ELSIF (string[0] = sp) AND sign THEN 
                               (* this is crude but corrects real conversion *)
                        first := '+';
                        DEC(sz);
                        INC(add);
                     ELSIF sign (*AND (c IN SigFmt)*) THEN 
                        first := '+';
                     END;
                  END;
                  isnum := c IN NumFmt;
                  FormatString;
                  NextCh;
               END;
         ELSE 
            AddChar(output,c);
            NextCh;
         END;
      END;
      IF NextIdx() THEN 
         RETURN TooFewFormElems;
      ELSIF ValidLong(output) THEN 
         RETURN Success;
      ELSE 
         RETURN AllocFailed;
      END;
   END Printf;

BEGIN 
END Printf. 
