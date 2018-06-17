(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
IMPLEMENTATION MODULE MCP1IO;           (* LG *)
                                        (* REV AFB 3/84: UNIX I/O *)
					(* REV AFB 5/84: symfiles with *)
					(*               long constants *)
(* $T- *)

   IMPORT SYSTEM, StdIO, FtdIO, MCStop, SysPerror, Storage, MCBase,
      MCP1Public, MCSymFileDefs, MCP1Reals, MCHalfword, Archive, MCTypes,
      MCBigSet, SystemTypes;

  (* declarations from definition module 
  TYPE String14 = ARRAY [0 .. 13] OF CHAR;
  VAR sy : Symbol;
      val : Constval;
      typeset : Stset;
      length : CARDINAL;
      spix : Spellix;

  end of declarations *)

   VAR 
      ch        : CHAR;
      pos, line : CARDINAL;

   MODULE OutputSystem;

      FROM Storage IMPORT ALLOCATE, DEALLOCATE;
      FROM MCBase IMPORT Symbol, Spellix;
      FROM MCP1Public IMPORT il1Name, ErrorsFound;
      FROM StdIO IMPORT FILE, Fopen, Fclose, MODE, stderr;
      FROM FtdIO IMPORT FwriteWord, FwriteString, FwriteLn, FwriteCard;
      FROM MCHalfword IMPORT WriteHalfword;
      FROM SysPerror IMPORT Perror;
      FROM MCStop IMPORT Stop;
      IMPORT sy, val, length, spix, pos, line, typeset;

      EXPORT PutS, PutSy, PutSyVal, PutIdent, Error, InitSave, StopSave,
         RestartSave, ReleaseSys, InitOutput, TermOutput;

      CONST 
         errormax = 300;

      TYPE 
         Syptr = POINTER TO Symb;
         Symb  = RECORD 
            next: Syptr;
            halfword: BOOLEAN; (* if on: use WriteHalfword, else WriteWord *)
            card: CARDINAL 
         END;

      VAR 
         savesys    : BOOLEAN;
         first, last: Syptr;
         initline,stopline,restartline: CARDINAL;
         errorcount : CARDINAL;
         Il1        : FILE;

      PROCEDURE PutSy(s : Symbol);
         VAR 
            c: CARDINAL;

      BEGIN                             (* put symbol and pos into word *)
         c := ORD(s) * 400B;
         IF pos >= 400B THEN 
            INC(c,377B)
         ELSE 
            INC(c,pos)
         END;
         IF savesys THEN 
            WITH last^ DO 
               card := c;
               halfword := TRUE;
               IF next = NIL THEN 
                  NEW(next);
                  next^.next := NIL 
               END;
               last := next 
            END 
         ELSE 
            WriteHalfword(Il1, c);
         END 
      END PutSy;

      PROCEDURE PutI(c : CARDINAL);
      BEGIN                             (* pack c in four bytes *)
         IF savesys THEN 
            WITH last^ DO 
               card := c;
               halfword := FALSE;
               IF next = NIL THEN 
                  NEW(next);
                  next^.next := NIL 
               END;
               last := next 
            END;
         ELSE 
            FwriteWord(Il1, c);
         END 
      END PutI;

      PROCEDURE PutS;
      BEGIN 
         PutSy(sy);
         CASE sy OF 
           ident : 
               PutI(spix)
         | intcon,cardcon,charcon,realcon,bigsetcon : 
               PutI(val.value)
         | intcarcon : (* revision in interpass file *)
               PutI(CARDINAL(typeset));
               PutI(val.value);
         | stringcon : 
               PutI(val.value);
               PutI(length)
         | eol : 
               PutI(line)
         ELSE                           (* nothing *)
         END 
      END PutS;

      PROCEDURE PutSyVal(sy: Symbol; val: CARDINAL);
      BEGIN 
         PutSy(sy);
         PutI(val);
      END PutSyVal;

      PROCEDURE PutIdent(spix: Spellix);
      BEGIN 
         PutSy(ident);
         PutI(spix);
      END PutIdent;

      PROCEDURE Error(n: CARDINAL);
      BEGIN 
         INC(errorcount);
         ErrorsFound := TRUE;
         IF errorcount <= errormax THEN 
            IF errorcount = errormax THEN 
               n := 5 
            END;
            PutSyVal(errorsy, n)
         ELSIF errorcount >= 10000 THEN 
            FwriteString(stderr, "cannot recover from error at line ");
            FwriteCard(stderr, line, 1);
            FwriteString(stderr, "; pos = "); FwriteCard(stderr, pos, 1);
            FwriteString(stderr, "; error number = "); FwriteCard(stderr, n, 1);
            FwriteString(stderr, "; good bye !");
            FwriteLn(stderr);
            Stop(2);			(* LOOP *)
         END;
      END Error;

      PROCEDURE InitSave;
      BEGIN 
         savesys := TRUE;
         initline := line;
         restartline := line;
      END InitSave;

      PROCEDURE StopSave;
      BEGIN 
         savesys := FALSE;
         IF restartline <> line THEN 
            PutSyVal(eol,line)
         END;
         stopline := line;
      END StopSave;

      PROCEDURE RestartSave;
      BEGIN 
         savesys := TRUE;
         IF stopline <> line THEN 
            PutSyVal(eol,line)
         END;
         restartline := line;
      END RestartSave;

      PROCEDURE ReleaseSys;
         VAR 
            p : Syptr;

      BEGIN 
         savesys := FALSE;
         IF initline <> line THEN 
            PutSyVal(eol, initline)
         END;
         p := first;
         WHILE p <> last DO 
            WITH p^ DO 
               IF halfword THEN
                  WriteHalfword(Il1, card);
               ELSE
                  FwriteWord(Il1, card);
               END;
               p := next 
            END;
         END;
         last := first;
         IF stopline <> line THEN 
            PutSyVal(eol, line)
         END;
      END ReleaseSys;

      PROCEDURE InitOutput;
      BEGIN 
         IF NOT Fopen(Il1, il1Name, write, (* buffered = *) TRUE) THEN 
            Perror(il1Name);
            Stop(2);
         END;
         NEW(first);
         first^.next := NIL;
         initline := 0;
         stopline := 0;
         restartline := 0;
         last := first;
         savesys := FALSE;
         errorcount := 0;
      END InitOutput;

      PROCEDURE TermOutput;
      BEGIN 
         WHILE first <> NIL DO 
            last := first;
            first := first^.next;
            DISPOSE(last);
         END;
         PutSy(eop);
         IF NOT Fclose(Il1) THEN 
            Perror(il1Name);
            Stop(2);
         END;
      END TermOutput;

   END OutputSystem;

   MODULE IdentSystem;

      FROM StdIO IMPORT FILE, MODE, Fopen, Fclose, Fputc, Ftell;
      FROM SysPerror IMPORT Perror;
      FROM MCStop IMPORT Stop;
      FROM Storage IMPORT ALLOCATE, DEALLOCATE;
      FROM MCBase IMPORT Spellix, Symbol;
      FROM MCP1Public IMPORT ascName;
      FROM OutputSystem IMPORT Error;
      FROM SystemTypes IMPORT OFF;
      IMPORT sy, spix, ch, String14;

      EXPORT InIdTab, OutIdTab, TermIdTab, EnterId, GetDmId, HashIdent,
         EnterResWord;

      CONST 
         idmax        = 19999;
         idmaxplusone = idmax + 1;
         htabmax      = 1801;            (* MUST BE A PRIME NUMBER *)
         rswlgth      = 14;
         (* maxspix must be equal to constant in p2/MCP2IO.d *)
         maxspix      = MAX(INTEGER);  (* must be less than maxcard *)

      TYPE 
         Idindex = [0..idmax];
         Htindex = [0..htabmax];

      VAR 
         asc      : FILE;
         idtab    : POINTER TO ARRAY Idindex OF CHAR;
         htab     : POINTER TO ARRAY Htindex OF RECORD 
            identry : [0..idmaxplusone];
            spix : Spellix;
         END;
         idbase, idtop, inaux, outaux: CARDINAL;
         dmid     : CARDINAL;
         hsix, ix : CARDINAL;
         ispix    : OFF;

      PROCEDURE GetDmId;
      BEGIN 
         INC(dmid);
         spix := dmid;
         outaux := 0;
      END GetDmId;

      PROCEDURE InIdTab;
      BEGIN 
         idtab^[inaux] := ch;
         hsix := hsix MOD 257*ORD(ch)+1;
         IF inaux < idmax THEN 
            INC(inaux)
         END 
      END InIdTab;

      PROCEDURE OutIdTab;
      (* return character from idtab *)
      BEGIN 
         ch := idtab^[outaux];
         INC(outaux);
      END OutIdTab;

      PROCEDURE EnterId;
         VAR 
            h, i, id, b1, b2: CARDINAL;

      BEGIN 
         idtab^[inaux] := ' ';          (* separator *)
         IF inaux < idmax THEN 
            INC(inaux)
         END;
         sy := ident;
         h := hsix MOD htabmax;
         hsix := 1;
         IF inaux > idmax THEN 
            GetDmId;
            Error(7)
         ELSE 
            IF h = 0 THEN 
               h := htabmax-1 
            END;
            i := h;
            LOOP 
               id := htab^[i].identry;
               IF id > idmax THEN       (* a new identifier *)
                  IF inaux+20 < idmax THEN 
                     IF NOT Ftell(asc, ispix) THEN 
                        Perror(ascName);
                        Stop(2);
                     END;
                     spix := CARDINAL(ispix);
                     htab^[i].identry := idtop;
                     htab^[i].spix := spix;
                     outaux := idtop;
                     WHILE idtop < inaux DO 
                        IF NOT Fputc(idtab^[idtop], asc) THEN 
                           Perror(ascName);
                           Stop(2);
                        END;
                        INC(idtop);
                     END;
                  ELSE 
                     GetDmId;
                     Error(7);
                     inaux := idtop 
                  END;
                  EXIT 
               END;
               b1 := id;
               b2 := idtop;             (* compare identifiers *)
               LOOP 
                  IF (idtab^[b1]<>idtab^[b2]) OR (b2=inaux) THEN 
                     EXIT 
                  END;
                  INC(b1);
                  INC(b2)
               END;
               IF b2 = inaux THEN 
                  inaux := idtop;       (* identifier found *)
                  IF id < idbase THEN   (* reserved word *)
                     sy := VAL(Symbol,ORD(idtab^[b1]));
                  ELSE 
                     outaux := id;
                     spix := htab^[i].spix;
                  END;
                  EXIT 
               END;
               i := (i+h) MOD htabmax;
               IF i = 0 THEN 
                  GetDmId;
                  Error(8);
                  EXIT 
               END 
            END 
         END 
      END EnterId;

      PROCEDURE HashIdent(VAR str: String14);
         VAR 
            l: [0..rswlgth];

      BEGIN 
         l := 0;
         WHILE (l<rswlgth) AND (str[l]<>0C) DO 
            ch := str[l];
            InIdTab;
            INC(l);
         END;
         EnterId;
      END HashIdent;

      PROCEDURE EnterResWord(str: String14; s: Symbol);
      BEGIN 
         HashIdent(str);
         idtab^[idtop] := CHR(ORD(s));
         INC(idtop);
         inaux := idtop;
         idbase := idtop;
      END EnterResWord;

      PROCEDURE TermIdTab;
      BEGIN 
         IF NOT Fclose(asc) THEN 
            Perror(ascName);
            Stop(2);
         END;
         DISPOSE(idtab);
         DISPOSE(htab);
      END TermIdTab;

   BEGIN                                (* initialisation *)
      idbase := 0;
      idtop := 0;
      inaux := 0;
      dmid := maxspix;
      hsix := 1;
      NEW(idtab);
      NEW(htab);
      FOR ix := 0 TO htabmax DO 
         htab^[ix].identry := idmaxplusone 
      END;
      IF NOT Fopen(asc, ascName, write, (* buffered = *) TRUE) THEN 
         Perror(ascName);
         Stop(2);
      END;
   END IdentSystem;

   MODULE StringSystem;

      FROM Storage IMPORT ALLOCATE, DEALLOCATE;
      FROM MCBase IMPORT Stringptr, Constval, stringmax;
      FROM OutputSystem IMPORT Error;

      EXPORT QUALIFIED PutStrCh, InitString, TermString;

      TYPE 
         Bufptr = POINTER TO Stringbuffer;

      TYPE 
         Stringbuffer = ARRAY [1 .. stringmax] OF CHAR;

      VAR 
         string   : Bufptr;
         strcount : CARDINAL;
         ix       : CARDINAL;

      PROCEDURE PutStrCh(ch: CHAR);
      (* put character into stringbuffer *)
      BEGIN 
         INC(strcount);
         IF strcount > stringmax THEN 
            Error(6);
            strcount := stringmax;
         END;
         string^[strcount] := ch;
      END PutStrCh;

      PROCEDURE InitString;
      (* initalisation of string area *)
      BEGIN 
         strcount := 0;
         NEW(string);
      END InitString;

      PROCEDURE TermString(VAR length: CARDINAL; VAR val: Constval);
         VAR 
            buffp : Bufptr;
            ix    : CARDINAL;
            sval  : Stringptr;

      BEGIN 
         length := strcount;
         PutStrCh(0C);
         IF ODD(strcount) THEN 
            PutStrCh(0C)
         END;
         ALLOCATE(buffp,strcount);
         (* array-structure is overlayed to allocated area *)
         FOR ix := 1 TO strcount DO 
            buffp^[ix] := string^[ix]
         END;
         DISPOSE(string);
         (* string value entry *)
         NEW(sval);
         WITH sval^ DO 
            label := NIL;
            valentry := CARDINAL(buffp);
            slink := NIL;               (* initialisation *)
         END;
         val.svalue := sval;
      END TermString;

   END StringSystem;

   MODULE SymFileInput;

      FROM SYSTEM IMPORT TSIZE;
      FROM StdIO IMPORT FILE, MODE, stderr, Fopen, Fclose;
      FROM FtdIO IMPORT FwriteString, FwriteLn, FwriteChar;
      FROM MCHalfword IMPORT ReadHalfword;
      FROM Archive IMPORT AFILE, ArchiveOpen, ArchiveClose, ArchiveRead;
      FROM SysPerror IMPORT Perror;
      FROM Storage IMPORT ALLOCATE;
      FROM MCBase IMPORT Symbol, Spellix, Stset, SetValuePtr, Constval,
         maxcard, oneword;
      FROM MCStop IMPORT Stop;
      FROM MCP1Public IMPORT SymFilesMissing, listing, SymNamePtr, symNames,
         String14;
      FROM MCSymFileDefs IMPORT symFileKey, SymFileSymbols;
      FROM MCBigSet IMPORT TermConstSet;
      FROM OutputSystem IMPORT PutS, StopSave, RestartSave, Error;
      FROM IdentSystem IMPORT InIdTab, OutIdTab, EnterId;
      FROM StringSystem IMPORT PutStrCh, InitString, TermString;
      IMPORT sy, val, length, spix, ch, typeset;

      EXPORT GetSeparateModule;

      CONST 
         maxload = 32;               (* maximal number of loaded symbolfiles *)
         (* standard archives for symbol files *)
         symArchive = "SYM";
         symLibArchive = "/usr/lib/modula/SYM";

      TYPE 
         Byte = [0..377B];

      VAR 
         symstream       : FILE;
         asymFile        : AFILE;
         ReadArchive     : BOOLEAN;     (* if on: use ArchiveRead *)
         buffword        : CARDINAL;
         highbyte        : BOOLEAN;
         loadsym         : ARRAY [1 .. maxload] OF CARDINAL;
         (* list of symbolfiles already loaded *)
         maxindex, index : CARDINAL;

      PROCEDURE ReadSym(VAR b: Byte);
         VAR ch: CHAR;

         PROCEDURE ARead(VAR ch: CHAR);
         BEGIN
            IF NOT ArchiveRead(asymFile, ch) THEN
               Perror("ReadSym");
               Stop(2);
            END;
         END ARead;

      BEGIN 
         IF highbyte THEN 
            b := buffword DIV 400B;
         ELSE 
            IF ReadArchive THEN
               ARead(ch);
               buffword := ORD(ch) * 400B;
               ARead(ch);
               buffword := buffword + ORD(ch);
            ELSE
               ReadHalfword(symstream, buffword);
            END;
            b := buffword MOD 400B;
         END;
         highbyte := NOT highbyte;
      END ReadSym;

      PROCEDURE SymGetSy;
      (* get symbol from symstream *)
         CONST 
            rwordnum = 2;               (* number of words for a real number *)
	    cbytenum = 4;		(* number of bytes for a cardinal n. *)

         VAR 
            b     : Byte;
            ix    : CARDINAL;
	    ix2   : CARDINAL;
            rconv : RECORD 
               CASE : BOOLEAN OF 
                 FALSE : 
                     ra : ARRAY [1..rwordnum] OF CARDINAL;
               | TRUE : 
                     rc : REAL;
               END;
            END;
            size  : CARDINAL;
            offset: CARDINAL;

         PROCEDURE GetI(VAR value: CARDINAL);
            VAR ix: CARDINAL; b: Byte;
         BEGIN
            value := 0;
            FOR ix := 1 TO cbytenum DO
               ReadSym(b);
               value := value * 400B + b;
            END;
         END GetI;

         PROCEDURE BigSetConstr(sz, offs: CARDINAL; VAR val: Constval);
            TYPE BigSetPtr = POINTER TO
                    ARRAY [0..(maxcard - oneword) DIV oneword] OF BITSET;
            VAR index: CARDINAL; setptr: BigSetPtr; cval: CARDINAL;
         BEGIN
            NEW(val.setvalue);
            WITH val.setvalue^ DO
               size := sz;
               offset := offs;
               slink := NIL;
               label := NIL;
               ALLOCATE(valentry, size * TSIZE(BITSET));
               setptr := valentry;
               FOR index := 0 TO size-1 DO
                  GetI(cval);
                  setptr^[index] := BITSET(cval);
               END;
            END;
            TermConstSet(val);
         END BigSetConstr;

      BEGIN 
         ReadSym(b);
         (* convert symbols *)
	 CASE VAL(SymFileSymbols,b) OF 
	   endfileSS : 
	       sy := eop;
	 | unitSS : 
	       sy := symbolsy;
	 | endunitSS : 
	       sy := endblock;
	 | importSS : 
	       sy := importsy;
	 | exportSS : 
	       sy := qualifiedsy;
	 | constSS : 
	       sy := constsy;
	 | normalconstSS : 
	       sy := cardcon;
               GetI(val.value);
	 | realconstSS : 
	       sy := realcon;
	       FOR ix := 1 TO rwordnum DO 
                  GetI(rconv.ra[ix]);
	       END;
	       NEW(val.rvalue);
	       val.rvalue^ := rconv.rc;
         | bigsetconstSS :
               sy := bigsetcon;
               GetI(size);
               GetI(offset);
               BigSetConstr(size, offset, val);
	 | stringconstSS : 
	       sy := stringcon;
	       InitString;
	       ReadSym(b);
	       WHILE b <> 0 DO 
		  PutStrCh(CHR(b));
		  ReadSym(b)
	       END;
	       TermString(length,val);
         | intcarconstSS :
               sy := intcarcon;
               GetI(val.value); typeset := Stset(val.value);
               GetI(val.value);
	 | typSS : 
	       sy := typesy;
	 | arraytypSS : 
	       sy := arraysy;
	 | recordtypSS : 
	       sy := recordsy;
	 | settypSS : 
	       sy := setsy;
	 | pointertypSS : 
	       sy := pointersy;
	 | hiddentypSS : 
	       sy := hidden;
	 | varSS : 
	       sy := varsy;
	 | procSS, funcSS : 
	       sy := proceduresy;
	 | identSS : 
	       sy := ident;
	       ReadSym(b);
	       WHILE b <> 0 DO 
		  ch := CHR(b);
		  InIdTab;
		  ReadSym(b);
	       END;
	       EnterId;
	 | periodSS : 
	       sy := period;
	 | colonSS : 
	       sy := colon;
	 | rangeSS : 
	       sy := range;
	 | lparentSS : 
	       sy := lparent;
	 | rparentSS : 
	       sy := rparent;
	 | lbracketSS : 
	       sy := lbrack;
	 | rbracketSS : 
	       sy := rbrack;
	 | caseSS : 
	       sy := casesy;
	 | ofSS : 
	       sy := ofsy;
	 | elseSS : 
	       sy := elsesy;
	 | endSS : 
	       sy := endsy;
	 END;                        (* CASE *)
      END SymGetSy;

      PROCEDURE SymFileOK(): BOOLEAN;
         VAR 
            b   : Byte;
            key : CARDINAL;

      BEGIN 
         ReadSym(b);
         IF b = ORD(normalconstSS) THEN 
	    ReadSym(b);
	    IF b <> 0 THEN RETURN FALSE END;
	    ReadSym(b);
	    IF b <> 0 THEN RETURN FALSE END;
            ReadSym(b);
            key := b;
            ReadSym(b);
            key := key * 400B + b;
            RETURN key = symFileKey;
         ELSE 
            RETURN FALSE;
         END;
      END SymFileOK;

      PROCEDURE GetSeparateModule;
         CONST 
            strmax = 14;
         VAR 
            name          : ARRAY [0 .. strmax-1] OF CHAR;
                                        (* name of the symbolfile *)
            modname       : String14;
            dummyname     : ARRAY [0..1] OF CHAR;
            lastinputspix : Spellix;
            lastinputch   : CHAR;
            lastinputsy   : Symbol;
            pos           : CARDINAL;
            pos2          : CARDINAL;
            okfile        : BOOLEAN;

         PROCEDURE Print(modname, archive, symname: ARRAY OF CHAR);
         BEGIN
            IF listing THEN
               FwriteChar(stderr, " "); FwriteString(stderr, modname);
               FwriteString(stderr, ": ");
               IF archive[0] <> 0C THEN
                  FwriteString(stderr, archive); FwriteChar(stderr, "(");
               END;
               FwriteString(stderr, symname);
               IF archive[0] <> 0C THEN
                  FwriteChar(stderr, ")");
               END;
               FwriteLn(stderr);
            END;
         END Print;

         PROCEDURE Lookup(VAR name: ARRAY OF CHAR; modname: String14);
            VAR sym: SymNamePtr; modulespix: Spellix;

            PROCEDURE CheckSymFile() : BOOLEAN;
            BEGIN
               highbyte := FALSE;
               IF SymFileOK() THEN
                  SymGetSy;
                  modulespix := spix;
                  IF (sy = ident) AND (spix = lastinputspix) THEN
                     RETURN TRUE (* okay *)
                  END;
               END;
               modulespix := Spellix(0);
               IF ReadArchive THEN
                  ArchiveClose(asymFile);
               ELSE
                  IF NOT Fclose(symstream) THEN END;
               END;
               RETURN FALSE;
            END CheckSymFile;

            PROCEDURE TryArchive(archive, name: ARRAY OF CHAR) : BOOLEAN;
            BEGIN
               IF ArchiveOpen(asymFile, archive, name) THEN
                  ReadArchive := TRUE;
                  IF CheckSymFile() THEN
                     Print(modname, archive, name);
                     RETURN TRUE;
                  END;
               END;
               RETURN FALSE
            END TryArchive;

            PROCEDURE TryFile(name: ARRAY OF CHAR) : BOOLEAN;
            BEGIN
               IF Fopen(symstream, name, read, (* buffered = *) TRUE) THEN
                  ReadArchive := FALSE;
                  IF CheckSymFile() THEN
                     Print(modname, "", name);
                     RETURN TRUE
                  END;
               END;
               RETURN FALSE
            END TryFile;

         BEGIN
            okfile := TRUE;

            (* try to open name *)
            IF TryFile(name) THEN RETURN END;
            (* look for symbofile/archive in list *)
            sym := symNames;
            WHILE sym <> NIL DO
               WITH sym^ DO
                  (* already known ??? *)
                  IF moduleName = lastinputspix THEN
                     IF TryFile(symName) THEN RETURN END;
                  ELSIF moduleName <> Spellix(0) THEN
                     (* continue *)
                  ELSE
                     (* check for archive *)
                     IF TryArchive(symName, name) THEN RETURN END;
                     (* check for further symbol files *)
                     IF TryFile(symName) THEN RETURN END;
                     moduleName := modulespix;
                  END;
               END;
               sym := sym^.link;
            END;
            (* last chance *)
            IF TryArchive(symArchive, name) THEN RETURN END;
            IF TryArchive(symLibArchive, name) THEN RETURN END;
            okfile := FALSE;
         END Lookup;

      BEGIN 
         lastinputch := ch;
         lastinputspix := spix;
         lastinputsy := sy;
         (* test on already loaded symbolfile *)
         index := 1;
         WHILE (index <= maxindex) AND (loadsym[index] <> spix) DO 
            INC(index);
         END;
         IF index > maxindex THEN       (* new symbolfile *)
            IF maxindex < maxload THEN 
               INC(maxindex);
               loadsym[maxindex] := spix;
            END;

            (* get module name *)
            OutIdTab;
            pos := 0;
            WHILE (pos < strmax-3) AND (ch <> ' ') DO 
               name[pos] := ch;
               modname[pos] := ch;
               INC(pos);
               OutIdTab;                (* get next ch *)
            END;
            pos2 := pos;
            WHILE (pos2 <= HIGH(modname)) AND (ch <> ' ') DO
               modname[pos2] := ch;
               INC(pos2);
               OutIdTab;
            END;
            (* append .sy *)
            IF pos2 <= HIGH(modname) THEN modname[pos2] := 0C END;
            name[pos] := '.';
            INC(pos);
            name[pos] := 's';
            INC(pos);
            name[pos] := 'y';
            INC(pos);
            IF pos < strmax THEN 
               name[pos] := 0C 
            END;

            (* search symbol file *)
            Lookup(name, modname);
            IF okfile THEN
               (* copy file *)
               StopSave;
               SymGetSy;
               WHILE sy <> eop DO 
                  PutS;
                  SymGetSy 
               END;
               RestartSave;
               (* close file *)
               IF ReadArchive THEN
                  ArchiveClose(asymFile);
               ELSIF NOT Fclose(symstream) THEN
               END;
            ELSE 
               SymFilesMissing := TRUE;
               IF listing THEN FwriteChar(stderr, " ") END;
               FwriteString(stderr, modname);
               FwriteString(stderr, ": no symbol file found");
               FwriteLn(stderr);
            END;
         END;
         ch := lastinputch;
         sy := lastinputsy;
         spix := lastinputspix;
      END GetSeparateModule;

   BEGIN                                (* SymFileInput *)
      maxindex := 0;                    (* no symbolfile loaded *)
   END SymFileInput;

   MODULE Scanner;

      FROM StdIO IMPORT FILE, MODE, Fopen, Fclose, Fgetc;
      FROM Storage IMPORT ALLOCATE, DEALLOCATE;
      FROM SysPerror IMPORT Perror;
      FROM MCBase IMPORT Symbol, modrev, longmaxcard, longmaxint, Stptr,
         Structform, Stset;
      FROM MCStop IMPORT Stop;
      FROM MCP1Public IMPORT srcName;
      FROM MCP1Reals IMPORT InitRealConst, ConvertToFraction,
         ConvertToExponent, TermRealConst;
      FROM OutputSystem IMPORT PutS, PutSyVal, Error;
      FROM IdentSystem IMPORT InIdTab, EnterId;
      FROM StringSystem IMPORT InitString, PutStrCh, TermString;
      FROM MCTypes IMPORT ConstType;
      IMPORT sy, val, length, spix, ch, line, pos, typeset;

      EXPORT GetSy, InitInput, TermInput;

      TYPE 
         Optptr = POINTER TO Opt;
         Opt    = RECORD 
            next: Optptr;
            s: Symbol 
         END;

      CONST 
         rangech = 35C;                 (* means same as ".." *)
         eofch   = 36C;                 (* character indicating end of file *)
         eolch   = 37C;                 (* character indicating end of line *)
         zero    = 60B;                 (* ORD('0') *)
         eolc    = 12C;

      VAR 
         optroot           : ARRAY [ 'A'.. 'Z'] OF Optptr;
         cch, sch, och     : CHAR; (* CAP(ch), string del, old char *)
         dval, oval, hval  : CARDINAL;(*decimal, octal and hexadecimal value *)
         dok, ook, hok, rok: BOOLEAN;
         long              : BOOLEAN; (* if on: long real constant *)
         input             : FILE;
         mustread          : BOOLEAN;

      PROCEDURE NextCh;
      BEGIN 
         IF mustread THEN 
            IF NOT Fgetc(ch, input) THEN 
               ch := 0C;
            END;
            INC(pos);
         ELSE 
            ch := 0C;
         END;
         IF ch < 40C THEN 
            IF ch = eolc THEN 
               ch := eolch;
            ELSIF ch = 0C THEN 
               ch := eofch;
               mustread := FALSE;
            ELSE 
               ch := ' ';
            END;
         END;
      END NextCh;

      PROCEDURE Comment;
         VAR 
            clevel : CARDINAL;

         PROCEDURE Options;
            VAR 
               op : Optptr;
         BEGIN 
            LOOP 
               WHILE ch = ' ' DO 
                  NextCh 
               END;
               IF ch<> '$' THEN 
                  EXIT 
               END;
               NextCh;
               cch := CAP(ch);
               IF (cch< 'A') OR ( 'Z'<cch) THEN 
                  EXIT 
               END;
               NextCh;
               IF (ch = '+') OR (ch = '-') OR (ch = '=') THEN 
                  IF ch = '=' THEN 
                     IF optroot[cch] <> NIL THEN 
                        op := optroot[cch];
                        optroot[cch] := optroot[cch]^.next;
                        DISPOSE(op)
                     END;
                     IF optroot[cch] = NIL THEN 
                        sy := plus;
                     ELSE 
                        sy := optroot[cch]^.s;
                     END;
                  ELSE 
                     IF ch = '+' THEN 
                        sy := plus 
                     ELSE 
                        sy := minus 
                     END;
                     NEW(op);
                     WITH op^ DO 
                        next:=optroot[cch];
                        optroot[cch]:=op;
                        s:=sy;
                     END;
                  END;
                  PutSyVal(option, ORD(cch));
                  PutS;
                  NextCh;
               END;
               sy := eol;               (* dummy symbol *)
               WHILE ch = ' ' DO 
                  NextCh 
               END;
               IF ch<> ',' THEN 
                  EXIT 
               ELSE 
                  NextCh 
               END 
            END;                        (* LOOP *)
         END Options;

      BEGIN 
         clevel := 1;
         Options;
         WHILE (clevel > 0) AND (ch <> eofch) DO 
            och := ch;
            NextCh;
            IF (och= '*') AND (ch= ')') THEN 
               DEC(clevel);
               NextCh;
            ELSIF (och= '(') AND (ch= '*') THEN 
               INC(clevel);
               NextCh;
            ELSIF och=eolch THEN 
               INC(line);
               pos := 0;
               PutS;                    (*sy=eol*)
            END;
         END;
         IF clevel > 0 THEN 
            pos := 1;
            Error(3)
         END;
      END Comment;

      PROCEDURE GetSy;
         VAR 
            i: CARDINAL;
            typtr: Stptr;
      BEGIN 
         sy := eol;                     (* eol is never returned from GetSy *)
         REPEAT 
            och := ch;
            NextCh;
            CASE och OF 
              eofch: 
                  sy := eop 
            | eolch: 
                  INC(line);
                  pos := 0;
                  PutS 
            | ' ' : 
                  WHILE ch = ' ' DO 
                     NextCh 
                  END 
            | 'A'.. 'Z', 'a'.. 'z':     (* identifier or reserved word *)
                  cch := ch;
                  ch := och;
                  InIdTab;
                  ch := cch;
                  cch := CAP(ch);
                  WHILE ( 'A'<=cch) AND (cch<= 'Z') OR ( '0'<=ch) AND (ch
                     <= '9') DO 
                     InIdTab;
                     NextCh;
                     cch := CAP(ch)
                  END;
                  EnterId 
            | '0'.. '9':                (* constant *)
                  InitRealConst;
                  dval := ORD(och) - zero;
                  dok := TRUE;
                  oval := dval;
                  ook := dval < 8;
                  hval := dval;
                  hok := TRUE;
                  ConvertToFraction(och);
                  rok := TRUE;
                  och := ' ';     (* for test on octal numbers or characters *)
                  cch := CAP(ch);
                  WHILE ( '0'<=ch) AND (ch<= '9') OR ( 'A'<=cch) AND (cch
                     <= 'F') DO 
                     IF ch <= '9' THEN  (* digits *)
                        i := ORD(ch) - ORD( '0');
                        IF och <> ' ' THEN 
                           och := 'H'
                        END;
                     ELSE               (* letters 'A' to 'F' *)
                        i := ORD(cch) - ORD( 'A') + 10;
                        IF (och = ' ') AND ook THEN 
                           och := cch 
                        ELSE 
                           och := 'H'
                        END;
                     END;
                     dok := dok AND
                            ((dval < longmaxcard DIV 10) AND (i < 10) OR
                             (dval = longmaxcard DIV 10) AND (i <= 5));
                     ook := ook AND (oval <= longmaxcard DIV 10B) AND (i < 10B);
                     hok := hok AND (hval <= longmaxcard DIV 10H);
                     rok := rok AND (i < 10);
                     IF dok THEN 
                        dval := 10 * dval + i 
                     END;
                     IF ook THEN 
                        oval := 10B * oval + i 
                     END;
                     IF hok THEN 
                        hval := 10H * hval + i 
                     END;
                     IF rok THEN 
                        ConvertToFraction(ch)
                     END;
                     NextCh;
                     cch := CAP(ch);
                  END;
                  sy := intcarcon;
                  IF cch = 'H' THEN     (* hexadecimal number *)
                     NextCh;
                     dval := hval;
                     dok := hok;
                  ELSIF och = 'B' THEN  (* octal constant *)
                     dval := oval;
                     dok := TRUE;
                  ELSIF och = 'C' THEN  (* character constant *)
                     sy := charcon;
                     dval := oval;
                     dok := oval < 400B;
                  ELSIF ch = '.' THEN 
                     NextCh;
                     IF ch = '.' THEN 
                        ch := rangech;
                     ELSE               (* real number *)
                        ConvertToFraction( '.');
                        sy := realcon;
                        WHILE ( '0' <= ch) AND (ch <= '9') DO 
                           IF rok THEN 
                              ConvertToFraction(ch)
                           END;
                           NextCh;
                        END;
                        IF CAP(ch) = 'E' THEN 
                           NextCh;
                           IF (ch = '-') OR (ch = '+') THEN 
                              IF ch = '-' THEN 
                                 ConvertToExponent(ch)
                              END;
                              NextCh;
                           END;
                           IF ('0' <= ch) AND (ch <= '9') THEN 
                              REPEAT 
                                 IF rok THEN 
                                    ConvertToExponent(ch)
                                 END;
                                 NextCh;
                              UNTIL (ch < '0') OR ( '9' < ch);
                           ELSE 
                              rok := FALSE;
                           END;
                        END;
                     END;
                  END;
                  IF sy = realcon THEN 
                     IF rok THEN 
                        TermRealConst(val, long, rok);
                        (* ignore long-flag *)
                        rok := NOT rok; (* inverse error flag *)
                     ELSE 
                        val.rvalue := NIL;
                     END;
                     IF NOT rok THEN 
                        Error(2)
                     END;
                  ELSIF dok THEN 
                     val.value := dval;
                     ConstType(val, typtr);
                     CASE typtr^.form OF
                     | ints, cards, longints, longcards:
                        typeset := Stset{typtr^.form};
                     | setoftypes:
                        typeset := typtr^.typeset;
                     END;
                  ELSE 
                     val.value := 0;
                     typeset := Stset{ints, cards, longints, longcards};
                     Error(2);
                  END;
            | ':' : 
                  IF ch= '=' THEN 
                     NextCh;
                     sy := becomes 
                  ELSE 
                     sy := colon 
                  END 
            | '<' : 
                  IF ch = '=' THEN 
                     NextCh;
                     sy := leq 
                  ELSIF ch= '>' THEN 
                     NextCh;
                     sy := neq 
                  ELSE 
                     sy := lss 
                  END 
            | '>' : 
                  IF ch= '=' THEN 
                     NextCh;
                     sy := geq 
                  ELSE 
                     sy := grt 
                  END 
            | '"', "'": 
                  i := 0;
                  sy := stringcon;
                  LOOP 
                     IF ch = och THEN 
                        NextCh;
                        EXIT 
                     END;
                     IF (ch = eolch) OR (ch = eofch) THEN
                        Error(4);
                        EXIT
                     END;
                     INC(i);
                     IF i = 1 THEN 
                        IF modrev THEN
                           InitString; PutStrCh(ch);
                        ELSE
                           sch := ch 
                        END;
                     ELSE 
                        IF NOT modrev AND (i = 2) THEN 
                           InitString;
                           PutStrCh(sch)
                        END;
                        PutStrCh(ch);
                     END;
                     NextCh 
                  END;
                  IF NOT modrev AND (i = 1) THEN 
                     sy := charcon;
                     val.value := ORD(sch)
                  ELSE 
                     IF i = 0 THEN      (* empty string *)
                        InitString;
                        PutStrCh(0C);
                     END;
                     TermString(length,val);
                  END 
            | rangech : 
                  sy := range 
            | '.' : 
                  IF ch= '.' THEN 
                     NextCh;
                     sy := range 
                  ELSE 
                     sy := period 
                  END 
            | '(' : 
                  IF ch= '*' THEN 
                     NextCh;
                     Comment 
                  ELSE 
                     sy := lparent 
                  END 
            | '*' : 
                  sy := times 
            | '/' : 
                  sy := slash 
            | '+' : 
                  sy := plus 
            | '-' : 
                  sy := minus 
            | '=' : 
                  sy := eql 
            | ')' : 
                  sy := rparent 
            | ',' : 
                  sy := comma 
            | ';' : 
                  sy := semicolon 
            | '[' : 
                  sy := lbrack 
            | ']' : 
                  sy := rbrack 
            | '^' : 
                  sy := arrow 
            | '|' : 
                  sy := variant 
            | '#' : 
                  sy := neq 
            | '&' : 
                  sy := andsy 
            | '~' :
                  IF modrev THEN
                     sy := notsy
                  ELSE
                     Error(0);
                  END;
            | '{' : 
                  sy := lconbr 
            | '}' : 
                  sy := rconbr 
            ELSE 
               Error(0)
            END;
         UNTIL sy<>eol 
      END GetSy;

      PROCEDURE InitInput;
         VAR 
            ch : CHAR;

      BEGIN 
         IF NOT Fopen(input, srcName, read, (* buffered = *) TRUE) THEN 
            Perror(srcName);
            Stop(2);
         END;
         line := 1;
         pos := 0;
         mustread := TRUE;
         FOR ch := 'A' TO 'Z' DO 
            optroot[ch] := NIL 
         END;
         PutSyVal(eol,1);
         NextCh;
      END InitInput;

      PROCEDURE TermInput;
         VAR 
            ch : CHAR;
            op : Optptr;
      BEGIN 
         FOR ch := 'A' TO 'Z' DO 
            WHILE optroot[ch] <> NIL DO 
               op := optroot[ch];
               optroot[ch] := optroot[ch]^.next;
               DISPOSE(op);
            END;
         END;
         IF NOT Fclose(input) THEN 
            Perror(srcName);
            Stop(2);
         END;
      END TermInput;

   END Scanner;

   PROCEDURE InitInOut;
   BEGIN 
      InitOutput;
      InitInput;
   END InitInOut;

   PROCEDURE TermInOut;
   BEGIN 
      TermInput;
      TermOutput;
      TermIdTab;
   END TermInOut;

END MCP1IO. 
