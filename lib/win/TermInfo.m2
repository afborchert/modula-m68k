(* Modula-2 Library    -  UNIX System V  -     AFB 5/88 *)
(* (c) Universitaet Ulm, Sektion Informatik, D-7900 Ulm *)
IMPLEMENTATION MODULE TermInfo;

   FROM Clock IMPORT UnitsPerSecond, RealTime;
   FROM Environment IMPORT GetEnv;
   FROM Strings IMPORT StrCat, StrCpy;
   FROM StdIO IMPORT Fopen, Fclose, read, FILE, Fgetc, Ftell, Fseek;
   FROM SYSTEM IMPORT ADDRESS, ADR;
   IMPORT Conversions, Strings, SysPanic, SYSTEM;

   MODULE Interpreter;

      FROM Conversions IMPORT ConvertInteger;
      FROM Strings IMPORT StrCat;
      FROM SysPanic IMPORT Panic;
      FROM SYSTEM IMPORT ADDRESS, BYTE, ADR;

      EXPORT Tparm, Tparm1, Tparm2, Tparm3, Tparm4, Tparm9;

      CONST
	 maxarg = 10;
      TYPE
	 ArgList = ARRAY[0..maxarg-1] OF ADDRESS;
      VAR
	 args: ArgList;     (* stack of arguments *)
	 arghigh: ARRAY [0..maxarg-1] OF CARDINAL; (* HIGH(args[]) *)
	 argi: CARDINAL;    (* index of next arg to be pushed *)

      PROCEDURE Abort(msg: ARRAY OF CHAR);
	 VAR panicmsg: ARRAY [0..79] OF CHAR;
      BEGIN
	 panicmsg := "TermInfo.Tparm: ";
	 StrCat(panicmsg, msg);
	 Panic(panicmsg);
      END Abort;

      PROCEDURE PushArg(VAR a: ARRAY OF BYTE);
	 (* push next arg of Tparm? onto stack 'args' *)
      BEGIN
	 (* Assert(argi < maxarg); *)
	 args[argi] := ADR(a); arghigh[argi] := HIGH(a); INC(argi);
      END PushArg;

      PROCEDURE Print(VAR out: ARRAY OF CHAR; fmt: ARRAY OF CHAR);
	 TYPE
	    CharSet = SET OF CHAR;
	    VarName = ['a'..'z'];
	 CONST
	    Digits = CharSet{'0'..'9'};
	    VarNames = CharSet{MIN(VarName)..MAX(VarName)};
	    bitsperword = 32; (* size of BITSET in bits *)
	 VAR
	    index: CARDINAL;
	    outindex: CARDINAL;
	    zeropad: BOOLEAN;
	    padcnt: CARDINAL;
	    vars: ARRAY VarName OF INTEGER;
	    minus: BOOLEAN;
	    op: INTEGER; (* one operand *)
	    nestlevel: CARDINAL;

	 MODULE Stack;

	    IMPORT Abort;

	    EXPORT Push, Pop;

	    CONST
	       stacksize = 10;
	    VAR
	       stack: ARRAY [0..stacksize-1] OF INTEGER;
	       top: CARDINAL;

	    PROCEDURE Push(arg: INTEGER);
	    BEGIN
	       IF top <= HIGH(stack) THEN
		  stack[top] := arg;
		  INC(top);
	       ELSE
		  Abort("stack overflow");
	       END;
	    END Push;

	    PROCEDURE Pop() : INTEGER;
	    BEGIN
	       IF top > 0 THEN
		  DEC(top);
		  RETURN stack[top]
	       ELSE
		  Abort("stack underflow");
	       END;
	    END Pop;

	 BEGIN
	    top := 0;
	 END Stack;

	 PROCEDURE Write(ch: CHAR);
	 BEGIN
	    IF outindex <= HIGH(out) THEN
	       out[outindex] := ch;
	       INC(outindex);
	    ELSE
	       Abort("overflow of output buffer");
	    END;
	 END Write;

	 PROCEDURE WriteString(s: ARRAY OF CHAR);
	    VAR index: CARDINAL;
	 BEGIN
	    index := 0;
	    WHILE (index <= HIGH(s)) AND (s[index] <> 0C) DO
	       Write(s[index]);
	       INC(index);
	    END;
	 END WriteString;

	 PROCEDURE Next;
	 BEGIN
	    IF index < HIGH(fmt) THEN
	       INC(index);
	    ELSE
	       Abort("unexpected end in format string");
	    END;
	 END Next;

	 PROCEDURE PopInteger;
	    VAR
	       field: ARRAY [0..11] OF CHAR;
	       index: CARDINAL;
	 BEGIN
	    ConvertInteger(Pop(), padcnt, field);
	    IF zeropad THEN
	       index := 0;
	       WHILE (index <= HIGH(field)) AND (field[index] = ' ') DO
		  field[index] := '0';
		  INC(index);
	       END;
	    END;
	    WriteString(field);
	 END PopInteger;

	 PROCEDURE PopChar;
	    CONST
	       minchar = VAL(INTEGER, ORD(MIN(CHAR)));
	       maxchar = VAL(INTEGER, ORD(MAX(CHAR)));
	    VAR
	       char: INTEGER;
	 BEGIN
	    char := Pop();
	    IF (char >= minchar) AND (char <= maxchar) THEN
	       Write(CHR(char));
	    ELSE
	       Abort("popped value of %c not in [0C..377C]");
	    END;
	 END PopChar;

	 PROCEDURE PushParm(i: CARDINAL);
	    VAR
	       ip: POINTER TO INTEGER;
	 BEGIN
	    IF (i = 0) OR (i > argi) THEN
	       Abort("bad parameter number");
	    END;
	    DEC(i);
	    ip := args[i];
	    Push(ip^);
	 END PushParm;

	 PROCEDURE Increment;
	    VAR ip: POINTER TO INTEGER;
	 BEGIN
	    IF argi < 1 THEN
	       Abort("%i: not enough parameters");
	    ELSIF argi = 1 THEN
	       ip := args[0]; INC(ip^);
	    ELSE
	       ip := args[0]; INC(ip^);
	       ip := args[1]; INC(ip^);
	    END;
	 END Increment;

	 PROCEDURE Skip(cset: CharSet);
	    VAR level: CARDINAL;
	 BEGIN
	    level := 0; Next;
	    LOOP
	       IF fmt[index] = '%' THEN
		  Next;
		  IF (level = 0) AND (fmt[index] IN cset) THEN
		     RETURN
		  END;
		  CASE fmt[index] OF
		  | '?': INC(level);
		  | ';': IF level > 0 THEN
			    DEC(level);
			 ELSE
			    Abort("%; without %?");
			 END;
		  ELSE
		  END;
	       END;
	       Next;
	    END;
	 END Skip;

      BEGIN (* Print *)
	 index := 0; outindex := 0; nestlevel := 0;
	 WHILE (index <= HIGH(fmt)) AND (fmt[index] <> 0C) DO
	    IF fmt[index] = '%' THEN
	       Next;
	       IF fmt[index] = '0' THEN
		  zeropad := TRUE; Next;
	       ELSE
		  zeropad := FALSE;
	       END;
	       IF (fmt[index] = '2') OR (fmt[index] = '3') THEN
		  padcnt := ORD(fmt[index]) - ORD('0');
		  Next;
	       ELSE
		  padcnt := 0;
	       END;
	       CASE fmt[index] OF
	       | 'c': PopChar;
	       | 'd': PopInteger;
	       | 'g': Next;
		      IF fmt[index] IN VarNames THEN
			 Push(vars[fmt[index]]);
		      ELSE
			 Abort("%g without a-z");
		      END;
	       | 'i': Increment;
	       | 'p': Next;
		      IF fmt[index] IN Digits THEN
			 PushParm(ORD(fmt[index]) - ORD('0'));
		      ELSE
			 Abort("%p without digit");
		      END;
	       | 'P': Next;
		      IF fmt[index] IN VarNames THEN
			 vars[fmt[index]] := Pop();
		      ELSE
			 Abort("%P without a-z");
		      END;
	       | '{': op := 0; Next;
		      IF fmt[index] = '-' THEN
			 minus := TRUE; Next;
		      ELSE
			 IF fmt[index] = '+' THEN Next; END;
			 minus := FALSE;
		      END;
		      REPEAT
			 IF fmt[index] IN Digits THEN
			    op := op * 10 +
				  VAL(INTEGER, ORD(fmt[index]) - ORD('0'));
			 ELSE
			    Abort("illegal char inside {}");
			 END;
			 Next;
		      UNTIL fmt[index] = '}';
		      IF minus THEN op := - op END;
		      Push(op);
	       (* if-then-else *)
	       | '?': INC(nestlevel);
	       | 't': IF Pop() = 0 THEN Skip(CharSet{'e', ';'}) END;
	       | 'e': Skip(CharSet{';'});
	       | ';': IF nestlevel > 0 THEN
			 DEC(nestlevel);
		      ELSE
			 Abort("%; without %?");
		      END;
	       (* arithmetic operations *)
	       | '+': Push(Pop() + Pop());
	       | '-': op := Pop(); Push(Pop() - op);
	       | '*': Push(Pop() * Pop());
	       | '/': op := Pop(); Push(Pop() DIV op);
	       | 'm': op := Pop(); Push(Pop() MOD op);
	       (* logical operations *)
	       | '=': Push(ORD(Pop() = Pop()));
	       | '>': op := Pop(); Push(ORD(Pop() > op));
	       | '<': op := Pop(); Push(ORD(Pop() < op));
	       | '!': IF Pop() = 0 THEN
			 Push(ORD(TRUE));
		      ELSE
			 Push(ORD(FALSE));
		      END;
	       (* bit operations *)
	       | '&': Push(INTEGER(BITSET(Pop()) * BITSET(Pop())));
	       | '|': Push(INTEGER(BITSET(Pop()) + BITSET(Pop())));
	       | '^': Push(INTEGER(BITSET(Pop()) / BITSET(Pop())));
	       | '~': Push(INTEGER({0..bitsperword-1} - BITSET(Pop())));
	       | "'": Next; Push(ORD(fmt[index])); Next;
	       | '%': Write("%");
	       ELSE
		  Abort("illegal letter following %");
	       END;
	    ELSE
	       Write(fmt[index]);
	    END;
	    INC(index);
	 END;
	 IF outindex <= HIGH(out) THEN
	    out[outindex] := 0C;
	 END;
      END Print;

      PROCEDURE Tparm(VAR out: ARRAY OF CHAR; fmt: ARRAY OF CHAR);
      BEGIN
	 argi := 0;
	 Print(out, fmt);
      END Tparm;

      PROCEDURE Tparm1(VAR out: ARRAY OF CHAR; fmt: ARRAY OF CHAR;
		       arg1: ARRAY OF BYTE);
      BEGIN
	 argi := 0;
	 PushArg(arg1);
	 Print(out, fmt);
      END Tparm1;

      PROCEDURE Tparm2(VAR out: ARRAY OF CHAR; fmt: ARRAY OF CHAR;
		       arg1: ARRAY OF BYTE;
		       arg2: ARRAY OF BYTE);
      BEGIN
	 argi := 0;
	 PushArg(arg1);
	 PushArg(arg2);
	 Print(out, fmt);
      END Tparm2;

      PROCEDURE Tparm3(VAR out: ARRAY OF CHAR; fmt: ARRAY OF CHAR;
		       arg1: ARRAY OF BYTE;
		       arg2: ARRAY OF BYTE;
		       arg3: ARRAY OF BYTE);
      BEGIN
	 argi := 0;
	 PushArg(arg1);
	 PushArg(arg2);
	 PushArg(arg3);
	 Print(out, fmt);
      END Tparm3;

      PROCEDURE Tparm4(VAR out: ARRAY OF CHAR; fmt: ARRAY OF CHAR;
		       arg1: ARRAY OF BYTE;
		       arg2: ARRAY OF BYTE;
		       arg3: ARRAY OF BYTE;
		       arg4: ARRAY OF BYTE);
      BEGIN
	 argi := 0;
	 PushArg(arg1);
	 PushArg(arg2);
	 PushArg(arg3);
	 PushArg(arg4);
	 Print(out, fmt);
      END Tparm4;

      PROCEDURE Tparm9(VAR out: ARRAY OF CHAR; fmt: ARRAY OF CHAR;
		       arg1: ARRAY OF BYTE;
		       arg2: ARRAY OF BYTE;
		       arg3: ARRAY OF BYTE;
		       arg4: ARRAY OF BYTE;
		       arg5: ARRAY OF BYTE;
		       arg6: ARRAY OF BYTE;
		       arg7: ARRAY OF BYTE;
		       arg8: ARRAY OF BYTE;
		       arg9: ARRAY OF BYTE);
      BEGIN
	 argi := 0;
	 PushArg(arg1);
	 PushArg(arg2);
	 PushArg(arg3);
	 PushArg(arg4);
	 PushArg(arg5);
	 PushArg(arg6);
	 PushArg(arg7);
	 PushArg(arg8);
	 PushArg(arg9);
	 Print(out, fmt);
      END Tparm9;

   END Interpreter;

   PROCEDURE Delay(delay: CARDINAL);
      VAR
	 units: CARDINAL;
	 time: CARDINAL;
   BEGIN
      (* at least one system call *)
      time := RealTime((* reset = *) TRUE);
      units := delay * UnitsPerSecond DIV 10000;
      IF units > 0 THEN
	 (* every RealTime call causes a system call *)
	 (* so this busy-loop is fair to other processes *)
	 REPEAT
	    time := RealTime((* reset = *) FALSE);
	 UNTIL time > units;
      END;
   END Delay;

   PROCEDURE Tputs(str: ARRAY OF CHAR; affcnt: CARDINAL;
		   outc: OutProc);
   BEGIN
      TputsDelay(str, affcnt, outc, Delay);
   END Tputs;

   PROCEDURE TputsDelay(str: ARRAY OF CHAR; affcnt: CARDINAL;
			outc: OutProc; Delay: DelayProc);
      VAR
	 index: CARDINAL;

      PROCEDURE Pad;
	 TYPE
	    CharSet = SET OF CHAR;
	 CONST
	    Digits = CharSet{'0'..'9'};
	 VAR
	    delay: CARDINAL;	(* delay in tenth of milliseconds *)
	    start: CARDINAL;
      BEGIN
	 (* convert delay: "$<" {digit} ["." {digit}] ["*"] ">" *)
	 start := index;
	 delay := 0;
	 INC(index, 2);		(* skip "$<" *)
	 WHILE (index <= HIGH(str)) AND (str[index] IN Digits) DO
	    delay := 10 * delay + ORD(str[index]) - ORD('0');
	    INC(index);
	 END;
	 delay := delay * 10;
	 IF (index <= HIGH(str)) AND (str[index] = '.') THEN
	    INC(index);
	    IF (index <= HIGH(str)) AND (str[index] IN Digits) THEN
	       INC(delay, ORD(str[index]) - ORD('0'));
	       INC(index);
	       WHILE (index <= HIGH(str)) AND (str[index] IN Digits) DO
		  INC(index);
	       END;
	    END;
	 END;
	 IF (index <= HIGH(str)) AND (str[index] = '*') THEN
	    INC(index);
	    delay := delay * affcnt;
	 END;

	 IF (index > HIGH(str)) OR (str[index] <> '>') THEN
	    (* "$<" with no ">" *)
	    outc(str[start]);		(* avoid infinite loop *)
	    index := start + 1;
	    RETURN
	 ELSE
	    INC(index); (* skip '$' *)
	 END;

	 IF delay = 0 THEN
	    RETURN
	 END;

	 Delay(delay);
      END Pad;

   BEGIN
      index := 0;
      WHILE (index <= HIGH(str)) AND (str[index] <> 0C) DO
	 IF (str[index] = '$') AND (index < HIGH(str)) AND
	    (str[index+1] = '<') THEN
	    Pad;
	 ELSE
	    outc(str[index]);
	    INC(index);
	 END;
      END;
   END TputsDelay;

   PROCEDURE SetupTerm(termname: ARRAY OF CHAR; VAR tinfo: Term) : BOOLEAN;
      CONST
	 defaultdir = "/usr/lib/terminfo";
	 TImagic = 432B;		(* magic number of terminfo file *)
	 headersize = 6 * 2;		(* size of header in bytes *)
      TYPE
	 Section = (boolsec, numbersec, offsetsec, stringsec);
	 OFF = INTEGER;			(* offset type as used by StdIO.Ftell *)
	 Short = INTEGER;
	 Header =
	    RECORD
	       magic: Short;		(* to be compared with TImagic *)
	       nameSize: Short;		(* in bytes *)
	       boolcnt: Short;		(* in bytes *)
	       numcnt: Short;      	(* # of short integers *)
	       offcnt: Short;   	(* # of offsets *)
	       tabsize: Short;  	(* in bytes of the string table *)
	    END;
      VAR
	 ok: BOOLEAN;			(* result of GetEnv *)
	 tifile: ARRAY [0..79] OF CHAR;	(* compiled terminfo file *)
	 tname: ARRAY [0..31] OF CHAR;	(* terminal name *)
	 dir: ARRAY[0..2] OF CHAR;	(* first letter of terminal name *)
	 header: Header;		(* header of terminfo file *)
	 tfp: FILE;			(* terminfo file *)
	 done: BOOLEAN;		(* set to FALSE if anything goes wrong *)

      PROCEDURE ReadShort(VAR short: Short);
	 VAR byte1, byte2: CHAR;
      BEGIN
	 IF Fgetc(byte1, tfp) AND Fgetc(byte2, tfp) THEN
	    IF (byte1 = 377C) AND (byte2 = 377C) THEN
	       short := -1; (* other negative values are illegal *)
	    ELSE
	       (* reverse byte order (like PDP-11) *)
	       short := ORD(byte2) * 100H + ORD(byte1);
	    END;
	 ELSE
	    (* bad terminfo file *)
	    short := -1;
	    done := FALSE;
	 END;
      END ReadShort;

      PROCEDURE ReadHeader(VAR header: Header) : BOOLEAN;
	 VAR
	    size: Short;
      BEGIN
	 WITH header DO
	    ReadShort(magic);
	    ReadShort(nameSize);
	    ReadShort(boolcnt);
	    ReadShort(numcnt);
	    ReadShort(offcnt);
	    ReadShort(tabsize);
	    done := done AND (magic = TImagic);
	    RETURN done
	 END;
      END ReadHeader;

      PROCEDURE SeekToSection(section: Section; offset: OFF);
	 (* seek to the begin of the given section + offset *)
	 VAR origin: OFF;
      BEGIN
	 WITH header DO
	    origin := headersize + nameSize;
	    CASE section OF
	    | boolsec:   (* OK *)
	    | numbersec: INC(origin, boolcnt);
	    | offsetsec: INC(origin, boolcnt + numcnt * 2);
	    | stringsec: INC(origin, boolcnt + numcnt * 2 + offcnt * 2);
	    END;
	 END;
	 IF (section > boolsec) AND ODD(origin) THEN
	    INC(origin);
	 END;
	 done := done AND Fseek(tfp, origin + offset, 0);
      END SeekToSection;

      PROCEDURE ReadBooleanSection(count: CARDINAL; ptr: ADDRESS);
	 VAR
	    boolptr: POINTER TO BOOLEAN;
	    byte: CHAR;
	    index: CARDINAL;
      BEGIN
	 SeekToSection(boolsec, 0);
	 FOR index := 1 TO bools DO
	    boolptr := ptr;
	    IF count > 0 THEN
	       IF NOT Fgetc(byte, tfp) THEN
		  done := FALSE; RETURN
	       END;
	       boolptr^ := VAL(BOOLEAN, ORD(byte));
	       DEC(count);
	    ELSE
	       boolptr^ := FALSE;
	    END;
	    INC(ptr, SIZE(BOOLEAN));
	 END;
      END ReadBooleanSection;

      PROCEDURE ReadNumberSection(count: CARDINAL; ptr: ADDRESS);
	 VAR
	    shortptr: POINTER TO Short;
	    short: Short;
	    index: CARDINAL;
      BEGIN
	 SeekToSection(numbersec, 0);
	 FOR index := 1 TO ints DO
	    shortptr := ptr;
	    IF count > 0 THEN
	       ReadShort(short);
	       shortptr^ := short;
	       DEC(count);
	    ELSE
	       shortptr^ := -1;
	    END;
	    INC(ptr, SIZE(Short));
	 END;
      END ReadNumberSection;

      PROCEDURE ReadStringSection(count: CARDINAL; ptr: ADDRESS);
	 CONST
	    maxoffset = strings;
	 VAR
	    offtab: ARRAY [1..maxoffset] OF Short;
	    index: CARDINAL;

	 PROCEDURE ReadString(ptr: ADDRESS);
	    VAR
	       ch: CHAR;
	       cp: POINTER TO CHAR;
	       cnt: CARDINAL;
	 BEGIN
	    cnt := 0;
	    WHILE Fgetc(ch, tfp) AND (ch <> 0C) AND (cnt < SIZE(String)) DO
	       cp := ptr;
	       cp^ := ch;
	       INC(cnt);
	       INC(ptr, SIZE(CHAR));
	    END;
	    IF cnt < SIZE(String) THEN
	       cp := ptr; cp^ := 0C;
	    END;
	 END ReadString;

	 PROCEDURE NullString(ptr: ADDRESS);
	    VAR cp: POINTER TO CHAR;
	 BEGIN
	    cp := ptr; cp^ := 0C;
	 END NullString;

      BEGIN
	 SeekToSection(offsetsec, 0);
	 IF count > strings THEN
	    count := strings;
	 END;
	 FOR index := 1 TO count DO
	    ReadShort(offtab[index]);
	 END;
	 FOR index := 1 TO count DO
	    IF offtab[index] = -1 THEN
	       NullString(ptr + (index-1) * SIZE(String));
	    ELSE
	       SeekToSection(stringsec, offtab[index]);
	       ReadString(ptr + (index-1) * SIZE(String));
	    END;
	 END;
      END ReadStringSection;

   BEGIN
      done := TRUE;
      IF termname[0] = 0C THEN
	 (* terminal name given? *)
	 GetEnv("TERM", tname, ok);
	 IF NOT ok THEN RETURN FALSE END;
      ELSE
	 StrCpy(tname, termname);
      END;
      GetEnv("TERMINFO", tifile, ok);
      IF NOT ok THEN
	 tifile := defaultdir;
      END;
      dir[0] := "/"; dir[1] := tname[0]; dir[2] := "/";
      StrCat(tifile, dir);
      StrCat(tifile, tname);
      IF NOT Fopen(tfp, tifile, read, (* buffered = *) TRUE) THEN
	 RETURN FALSE
      END;

      (* read header and skip names section *)
      IF NOT ReadHeader(header) THEN
	 IF NOT Fclose(tfp) THEN END;
	 RETURN FALSE
      END;

      (* order in file: boolean-section, number section, offset-section *)
      (* order in record: String's, BOOLEAN's, INTEGER's *)

      WITH header DO
	 ReadBooleanSection(boolcnt, ADR(tinfo) + strings * SIZE(String));
	 ReadNumberSection(numcnt, ADR(tinfo) + SIZE(Term) -
						ints * SIZE(INTEGER));
	 ReadStringSection(offcnt, ADR(tinfo));
      END;
      RETURN done
   END SetupTerm;

END TermInfo.
