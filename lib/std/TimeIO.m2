(* Modula-2 Library    -  UNIX System V  -     AFB 9/88 *)
(* (c) Universitaet Ulm, Sektion Informatik, D-7900 Ulm *)
IMPLEMENTATION MODULE TimeIO;

   FROM ASCII IMPORT nl, tab;
   FROM Calendar IMPORT Time, Date, Daytime, Calendarday, CalendarInfo,
      ConvertTime, ConvertDate, ConvertCald, Weekday, Hour, GetTimezone,
      DateOK, CDate, Minute, Second, Month, CurrentTime, Day, CTime;
   FROM Conversions IMPORT ConvertCardinal;
   FROM Environment IMPORT GetEnv;
   FROM FtdIO IMPORT FwriteString;
   FROM StdIO IMPORT FILE, stdin, stdout, Fgetc;
   FROM SYSTEM IMPORT ADDRESS, ADR;
   IMPORT Strings, Storage;

   (* (* exported from definition module *)
   TYPE
      Style = (date,	(* date(1) and ctime(3) standard format *)
	       ls,	(* like the ls-command *)
	       env);	(* see for TIMEFMT in environment *)
   VAR
      Done: BOOLEAN;
      termCH: CHAR;
   *)
   TYPE
      ReadCharProc = PROCEDURE (VAR CHAR);
      CharPointer = POINTER TO CHAR;
      CharSet = SET OF CHAR;
      Components = (yearC, monthC, dayC, hourC, minuteC, secondC);
      CompSet = SET OF Components;
      TimeField = ARRAY Components OF CARDINAL;
   CONST
      DateComp = CompSet{yearC, monthC, dayC};
      TimeComp = CompSet{hourC, minuteC, secondC};
      AllComp = CompSet{MIN(Components)..MAX(Components)};
   VAR
      input: FILE;
      stringref: CharPointer;

   PROCEDURE WriteTime(format: ARRAY OF CHAR; time: Time);
      (* the output format is very close to date(1): *)
      (* each field descriptor is preceded by % and will be *)
      (* replaced in the output by its corresponding value. *)
      (* WriteTime does not append a newline automatically  *)
      (* like date(1).                                      *)
      (* output is directed to StdIO.stdout                 *)
   BEGIN
      FwriteTime(stdout, format, time);
   END WriteTime;

   PROCEDURE FwriteTime(file: FILE; format: ARRAY OF CHAR; time: Time);
      (* like WriteTime but output is directed to file      *)
      VAR
	 buf: ARRAY [0..1023] OF CHAR;
   BEGIN
      SwriteTime(buf, format, time);
      FwriteString(file, buf);
   END FwriteTime;

   PROCEDURE SwriteTime(VAR string: ARRAY OF CHAR;
			format: ARRAY OF CHAR;
			time: Time);
      (* like WriteTime but output is put into string       *)
      VAR
	 date: Date;
	 daytime: Daytime;
   BEGIN
      ConvertTime(time, date, daytime);
      WriteDateAndTime(string, format, date, daytime);
   END SwriteTime;

   PROCEDURE WriteDateAndTime(VAR string: ARRAY OF CHAR;
			      format: ARRAY OF CHAR;
			      date: Date; daytime: Daytime);
      VAR
	 index: CARDINAL;	(* index of format *)
	 out: CARDINAL;		(* index of string *)
	 calendarday: Calendarday;
	 calendarinfo: CalendarInfo;
	 hour: Hour;
	 tzname: ARRAY [0..2] OF CHAR;

      PROCEDURE Write(ch: CHAR);
      BEGIN
	 IF out <= HIGH(string) THEN
	    string[out] := ch;
	    INC(out);
	 ELSE
	    Done := FALSE;
	 END;
      END Write;

      PROCEDURE WriteCard(card: CARDINAL; len: CARDINAL);
	 VAR
	    buf: ARRAY [0..11] OF CHAR;
	    i: CARDINAL;
      BEGIN
	 ConvertCardinal(card, len, buf);
	 i := 0;
	 WHILE (i <= HIGH(buf)) AND (buf[i] <> 0C) DO
	    IF buf[i] = ' ' THEN
	       Write('0');
	    ELSE
	       Write(buf[i]);
	    END;
	    INC(i);
	 END;
      END WriteCard;

      PROCEDURE WriteLn;
      BEGIN
	 Write(nl);
      END WriteLn;

      PROCEDURE WriteString(s: ARRAY OF CHAR);
	 VAR
	    index: CARDINAL;
      BEGIN
	 index := 0;
	 WHILE (index <= HIGH(s)) AND (s[index] <> 0C) DO
	    Write(s[index]);
	    INC(index);
	 END;
      END WriteString;

      PROCEDURE WriteWeekday(weekday: Weekday);
      BEGIN
	 CASE weekday OF
	 | Mon: WriteString("Mon");
	 | Tue: WriteString("Tue");
	 | Wed: WriteString("Wed");
	 | Thu: WriteString("Thu");
	 | Fri: WriteString("Fri");
	 | Sat: WriteString("Sat");
	 | Sun: WriteString("Sun");
	 END;
      END WriteWeekday;

      PROCEDURE WriteMonth(month: Month);
      BEGIN
	 CASE month OF
	 | 1:  WriteString("Jan");
         | 2:  WriteString("Feb");
         | 3:  WriteString("Mar");
         | 4:  WriteString("Apr");
         | 5:  WriteString("May");
         | 6:  WriteString("Jun");
         | 7:  WriteString("Jul");
         | 8:  WriteString("Aug");
         | 9:  WriteString("Sep");
         | 10: WriteString("Oct");
         | 11: WriteString("Nov");
         | 12: WriteString("Dec");
	 END;
      END WriteMonth;

   BEGIN
      ConvertDate(date, calendarday);
      ConvertCald(calendarday, calendarinfo);
      index := 0; out := 0; Done := TRUE;
      WHILE (index <= HIGH(format)) AND (format[0] <> 0C) DO
	 IF format[index] = '%' THEN
	    INC(index);
	    IF index <= HIGH(format) THEN
	       CASE format[index] OF
	       | '%': Write('%');
	       | 'n': WriteLn;
	       | 't': Write(tab);
	       | 'm': WriteCard(calendarday.month, 2);
	       | 'd': WriteCard(calendarday.day, 2);
	       | 'y': WriteCard(calendarday.year MOD 100, 2);
	       | 'D': WriteCard(calendarday.month, 2); Write('/');
		      WriteCard(calendarday.day, 2); Write('/');
		      WriteCard(calendarday.year MOD 100, 2);
	       | 'H': WriteCard(daytime.hour, 2);
	       | 'M': WriteCard(daytime.minute, 2);
	       | 'S': WriteCard(daytime.second, 2);
	       | 'T': WriteCard(daytime.hour, 2); Write(':');
		      WriteCard(daytime.minute, 2); Write(':');
		      WriteCard(daytime.second, 2);
	       | 'j': WriteCard(calendarinfo.yearday, 3);
	       | 'w': WriteCard((ORD(calendarinfo.weekday) +
				 (7-ORD(Sun))) MOD 7, 1);
	       | 'a': WriteWeekday(calendarinfo.weekday);
	       | 'h': WriteMonth(calendarday.month);
	       | 'r': hour := daytime.hour MOD 12;
		      IF hour = 0 THEN hour := 12 END;
		      WriteCard(hour, 2); Write(':');
		      WriteCard(daytime.minute, 2); Write(':');
		      WriteCard(daytime.second, 2);
		      IF daytime.hour >= 12 THEN
			WriteString(" PM");
		      ELSE
			WriteString(" AM");
		      END;
	       (* additional formats (not supported by date) *)
	       | 'Y': WriteCard(calendarday.year, 4);
	       | 'Z': GetTimezone(tzname); WriteString(tzname);
	       ELSE
		  Done := FALSE;
	       END;
	    ELSE
	       Done := FALSE;
	    END;
	 ELSE
	    Write(format[index]);
	 END;
	 INC(index);
      END;
      Write(0C);
   END WriteDateAndTime;

   PROCEDURE WriteTimeLike(style: Style; time: Time);
      (* write time to StdIO.stdout according to the given  *)
      (* style.                                             *)
   BEGIN
      FwriteTimeLike(stdout, style, time);
   END WriteTimeLike;

   PROCEDURE FwriteTimeLike(file: FILE; style: Style; time: Time);
      VAR
	 buf: ARRAY [0..1023] OF CHAR;
   BEGIN
      SwriteTimeLike(buf, style, time);
      FwriteString(file, buf);
   END FwriteTimeLike;

   PROCEDURE SwriteTimeLike(VAR string: ARRAY OF CHAR;
			    style: Style; time: Time);
      CONST
	 datefmt = "%a %h %d %T %Z %Y";
	 halfyear = Time(6*30*24*60*60);
      VAR
	 format: ARRAY [0..1023] OF CHAR;
	 ok: BOOLEAN;
	 now: Time;
   BEGIN
      CASE style OF
      | ls:   now := CurrentTime();
	      IF (now < time+halfyear) AND (time < now+halfyear) THEN
		 SwriteTime(string, "%h %d %H:%M", time);
	      ELSE
		 SwriteTime(string, "%h %d  %Y", time);
	      END;
      | date: SwriteTime(string, datefmt, time);
      | env:  GetEnv("TIMEFMT", format, ok);
	      IF ok THEN
		 SwriteTime(string, format, time);
	      ELSE
		 SwriteTime(string, datefmt, time);
	      END;
      END;
   END SwriteTimeLike;

   PROCEDURE ReadFromInput(VAR ch: CHAR);
   BEGIN
      IF NOT Fgetc(ch, input) THEN
	 ch := 0C; Done := FALSE;
      END;
   END ReadFromInput;

   PROCEDURE ReadFromString(VAR ch: CHAR);
   BEGIN
      ch := stringref^;
      stringref := ADDRESS(stringref) + SIZE(CHAR);
   END ReadFromString;

   PROCEDURE ReadTime(VAR time: Time);
      (* read time from StdIO.stdin *)
   BEGIN
      input := stdin; RdTime(ReadFromInput, time);
   END ReadTime;

   PROCEDURE FreadTime(file: FILE; VAR time: Time);
   BEGIN
      input := file; RdTime(ReadFromInput, time);
   END FreadTime;

   PROCEDURE SreadTime(string: ARRAY OF CHAR; VAR time: Time);
   BEGIN
      stringref := ADR(string); RdTime(ReadFromString, time);
   END SreadTime;

   PROCEDURE RdTime(Read: ReadCharProc; VAR time: Time);

      VAR
	 times: TimeField;

      PROCEDURE Convert;
	 CONST
	    Day = Time(24*60*60);
	 VAR
	    date: Date; daytime: Daytime;
	    mindate, maxdate: Date; (* convertable date range *)
      BEGIN
	 IF DateOK(times[yearC], times[monthC], times[dayC]) THEN
	    date := CDate(times[yearC], times[monthC], times[dayC]);
	    ConvertTime(MIN(Time)+Day, mindate, daytime);
	    ConvertTime(MAX(Time)-Day, maxdate, daytime);
	    IF (date >= mindate) AND (date <= maxdate) THEN
	       daytime.hour := times[hourC];
	       daytime.minute := times[minuteC];
	       daytime.second := times[secondC];
	       time := CTime(date, daytime);
	    ELSE
	       Done := FALSE;
	    END;
	 ELSE
	    Done := FALSE;
	 END;
      END Convert;

   BEGIN
      RdComp(Read, times, AllComp);
      IF Done THEN
	 Convert;
      END;
   END RdTime;

   PROCEDURE WriteDate(format: ARRAY OF CHAR; date: Date);
      VAR
	 buf: ARRAY [0..1023] OF CHAR;
   BEGIN
      SwriteDate(buf, format, date);
      FwriteString(stdout, buf);
   END WriteDate;

   PROCEDURE FwriteDate(file: FILE; format: ARRAY OF CHAR; date: Date);
      VAR
	 buf: ARRAY [0..1023] OF CHAR;
   BEGIN
      SwriteDate(buf, format, date);
      FwriteString(file, buf);
   END FwriteDate;

   PROCEDURE SwriteDate(VAR string: ARRAY OF CHAR;
			format: ARRAY OF CHAR; date: Date);
      VAR
	 daytime: Daytime;
   BEGIN
      WITH daytime DO
	 hour := 0; minute := 0; second := 0;
      END;
      WriteDateAndTime(string, format, date, daytime);
   END SwriteDate;

   PROCEDURE ReadDate(VAR date: Date);
   BEGIN
      input := stdin; RdDate(ReadFromInput, date);
   END ReadDate;

   PROCEDURE FreadDate(file: FILE; VAR date: Date);
   BEGIN
      input := file; RdDate(ReadFromInput, date);
   END FreadDate;

   PROCEDURE SreadDate(string: ARRAY OF CHAR; VAR date: Date);
   BEGIN
      stringref := ADR(string); RdDate(ReadFromString, date);
   END SreadDate;

   PROCEDURE RdDate(Read: ReadCharProc; VAR date: Date);
      VAR ymd: TimeField;
   BEGIN
      RdComp(Read, ymd, DateComp);
      IF DateOK(ymd[yearC], ymd[monthC], ymd[dayC]) THEN
	 date := CDate(ymd[yearC], ymd[monthC], ymd[dayC]);
      ELSE
	 Done := FALSE;
      END;
   END RdDate;

   PROCEDURE RdComp(Read: ReadCharProc; VAR time: TimeField;
					compset: CompSet);

      VAR
	 century: CARDINAL; (* default century *)

      MODULE Lex;

	 IMPORT Read, nl, tab, termCH, CharSet, GetTermChars;
	 EXPORT Symbol, GetSy, sy, val, char, ident;

	 TYPE
	    Symbol = (numberSY, identSY, charSY, eofSY);
	 VAR
	    ch: CHAR;			(* ch read in advance *)
	    char: CHAR;			(* char-value if sy = charSY *)
	    sy: Symbol;			(* result of GetSy *)
	    eof: BOOLEAN;		(* 0C returned by Read or errors *)
	    val: CARDINAL;		(* value if sy = numberSY *)
	    ident: ARRAY [0..7] OF CHAR;(* text if sy = identSY *)
	    termchars: CharSet;

	 PROCEDURE NextCh;
	 BEGIN
	    IF NOT eof THEN
	       Read(ch); termCH := ch;
	       IF ch IN termchars THEN
		  ch := 0C;
		  eof := TRUE;
	       END;
	    ELSE
	       ch := 0C;
	    END;
	 END NextCh;

	 PROCEDURE GetSy;
	    CONST
	       WhiteSpace = CharSet{' ', tab};
	       Digits = CharSet{'0'..'9'};
	       Alpha = CharSet{'a'..'z', 'A'..'Z'};
	    VAR
	       index: CARDINAL;		(* index of ident *)
	 BEGIN
	    WHILE ch IN WhiteSpace DO
	       Read(ch);
	    END;
	    CASE ch OF
	    | '0'..'9':
		  val := 0;
		  WHILE ch IN Digits DO
		     val := 10 * val + ORD(ch) - ORD('0');
		     NextCh;
		  END;
		  sy := numberSY;
	    | 'a'..'z', 'A'..'Z':
		  index := 0;
		  WHILE ch IN Alpha DO
		     IF index <= HIGH(ident) THEN
			ident[index] := CAP(ch);
			INC(index);
		     END;
		     NextCh;
		  END;
		  IF index <= HIGH(ident) THEN
		     ident[index] := 0C;
		  END;
		  sy := identSY;
	    | nl, 0C: sy := eofSY; eof := TRUE;
	    ELSE
	       sy := charSY; char := ch; NextCh;
	    END;
	 END GetSy;

      BEGIN
	 eof := FALSE; GetTermChars(termchars); INCL(termchars, 0C);
	 NextCh;
      END Lex;

      PROCEDURE Defaults;
	 VAR
	    daytime: Daytime;
	    calendarday: Calendarday;
	    date: Date;
	    now: Time;
      BEGIN
	 now := CurrentTime();
	 ConvertTime(now, date, daytime);
	 ConvertDate(date, calendarday);
	 time[hourC] := daytime.hour;
	 time[minuteC] := daytime.minute;
	 time[secondC] := daytime.second;
	 time[yearC] := calendarday.year;
	 time[monthC] := calendarday.month;
	 time[dayC] := calendarday.day;
	 century := time[yearC] - time[yearC] MOD 100;
      END Defaults;

      MODULE Parse;

	 FROM Storage IMPORT ALLOCATE, DEALLOCATE;
	 FROM Strings IMPORT StrCpy;
	 IMPORT Symbol, sy, val, ident, char, GetSy, CharSet,
	    Components, CompSet, time, century, Done,
	    (* from TimePattern *) Start, Match, GetPattern;

	 EXPORT ParseTime;

	 TYPE
	    SymList = POINTER TO SymRec;
	    SymRec =
	       RECORD
		  link: SymList;
		  CASE sym: Symbol OF
		  | numberSY: value: CARDINAL;
		  | identSY:  text: ARRAY [0..2] OF CHAR;
		  END;
	       END;
	 VAR
	    head, tail: SymList;

	 PROCEDURE Add;
	    VAR
	       new: SymList;
	 BEGIN
	    NEW(new);
	    WITH new^ DO
	       link := NIL;
	       sym := sy;
	       CASE sym OF
	       | numberSY: value := val;
	       | identSY:  StrCpy(text, ident);
	       ELSE
	       END;
	    END;
	    IF tail = NIL THEN
	       head := new;
	    ELSE
	       tail^.link := new;
	    END;
	    tail := new;
	 END Add;

	 PROCEDURE Cleanup;
	    VAR
	       old: SymList;
	 BEGIN
	    WHILE head # NIL DO
	       old := head;
	       head := head^.link;
	       DISPOSE(old);
	    END;
	    tail := NIL;
	 END Cleanup;

	 PROCEDURE ParseTime(compset: CompSet);
	    VAR
	       charset: CharSet;
	       pattern: ARRAY [0..15] OF CHAR;

	    PROCEDURE CheckNumber(VAR charset: CharSet);

	       PROCEDURE Check(min, max: CARDINAL; char: CHAR);
	       BEGIN
		  IF (val >= min) AND (val <= max) THEN
		     INCL(charset, char);
		  END;
	       END Check;

	    BEGIN
	       Check(0, 23, 'H');
	       Check(0, 59, 'M');
	       Check(0, 59, 'S');
	       Check(1, 2037, 'y');
	       Check(1, 12, 'm');
	       Check(1, 31, 'd');
	    END CheckNumber;

	    PROCEDURE CheckMonth(VAR charset: CharSet);
	       VAR month: CARDINAL;
	    BEGIN
	       NameOfMonth(ident, month);
	       IF month # 0 THEN INCL(charset, 'm'); END;
	    END CheckMonth;

	    PROCEDURE NameOfMonth(ident: ARRAY OF CHAR; VAR month: CARDINAL);
	    BEGIN
	       CASE ident[0] OF
	       | 'J':   CASE ident[1] OF
			| 'A': month := 1;
			| 'U':   CASE ident[2] OF
				 | 'N': month := 6;
				 | 'L': month := 7;
				 ELSE month := 0;
				 END;
			ELSE month := 0;
			END;
	       | 'F':   month := 2;
	       | 'M':   CASE ident[2] OF
			| 'R': month := 3;
			| 'Y', 'I': month := 5;
			ELSE month := 0;
			END;
	       | 'A':   CASE ident[1] OF
			| 'P': month := 4;
			| 'U': month := 8;
			ELSE month := 0;
			END;
	       | 'S':   month := 9;
	       | 'O':   month := 10;
	       | 'N':   month := 11;
	       | 'D':   month := 12;
	       ELSE     month := 0;
	       END;
	    END NameOfMonth;

	    PROCEDURE Assign(pattern: ARRAY OF CHAR);
	       (* assign read values accordingly to pattern *)
	       CONST
		  Numbers = CharSet{'H','M','S', 'y','m','d'};
	       VAR
		  index: CARDINAL;	(* index of pattern *)
		  missing: CompSet;	(* values not set 'til now *)
		  ptr: SymList;		(* pattern[index] matches ptr^ *)
		  charset: CharSet;	(* returned by NextSy *)
		  comp: Components;	(* temporary use *)

	       PROCEDURE Comp(pattern: CHAR) : Components;
	       BEGIN
		  CASE pattern OF
		  | 'H': RETURN hourC
		  | 'M': RETURN minuteC
		  | 'S': RETURN secondC
		  | 'y': RETURN yearC
		  | 'm': RETURN monthC
		  | 'd': RETURN dayC
		  END;
	       END Comp;

	    BEGIN
	       missing := compset;
	       index := 0; ptr := head;
	       WHILE (index <= HIGH(pattern)) AND (pattern[index] # 0C) DO
		  IF pattern[index] IN Numbers THEN
		     WITH ptr^ DO
			IF pattern[index] = 'm' THEN
			   IF sym = numberSY THEN
			      time[monthC] := value;
			   ELSE
			      NameOfMonth(text, time[monthC]);
			   END;
			ELSE
			   time[Comp(pattern[index])] := value;
			END;
		     END;
		     EXCL(missing, Comp(pattern[index]));
		  END;
		  INC(index);
		  ptr := ptr^.link;
		  IF ptr = NIL THEN
		     NextSy(charset); ptr := tail;
		     IF NOT (pattern[index] IN charset) THEN
			Done := FALSE; RETURN
		     END;
		  END;
	       END;

	       (* set missing components in dependance of the   *)
	       (* order of the type Components                  *)
	       (* - components with order below the first given *)
	       (*   component are defaulted by the current time *)
	       (* - components with order higher than a given   *)
	       (*   component are defaulted by their minimal    *)
	       (*   value (0 or 1)                              *)
	       IF missing # compset THEN
		  comp := MIN(Components);
		  WHILE (comp < MAX(Components)) AND (comp IN missing) DO
		     INC(comp); (* time[comp] set by Defaults *)
		  END;
		  FOR comp := comp TO MAX(Components) DO
		     IF comp IN missing THEN
			CASE comp OF
			| monthC, dayC:            time[comp] := 1;
			| hourC, minuteC, secondC: time[comp] := 0;
			END;
		     END;
		  END;
	       END;
	       (* two-digit years are defaulted to the current century *)
	       (* Yes, it is impossible to give dates of the early     *)
	       (* christian aera - but do we need them??               *)
	       IF time[yearC] < 100 THEN
		  INC(time[yearC], century);
	       END;
	    END Assign;

	    PROCEDURE NextSy(VAR charset: CharSet);
	    BEGIN
	       GetSy; Add;
	       charset := CharSet{};
	       CASE sy OF
	       | numberSY: CheckNumber(charset);
	       | identSY:  CheckMonth(charset);
	       | charSY:   charset := CharSet{ char };
	       | eofSY:    charset := CharSet{ 0C };
	       END;
	    END NextSy;

	 BEGIN
	    Start(compset);
	    REPEAT
	       NextSy(charset);
	    UNTIL NOT Match(charset);
	    IF Done THEN
	       GetPattern(pattern);
	       Assign(pattern);
	    END;
	    Cleanup;
	 END ParseTime;

      BEGIN
	 head := NIL; tail := NIL;
      END Parse;

   BEGIN (* RdTime *)
      Done := TRUE;
      Defaults;
      ParseTime(compset);
      Done := Done AND DateOK(time[yearC], time[monthC], time[dayC]);
   END RdComp;

   MODULE TimePattern;

      FROM Storage IMPORT ALLOCATE, DEALLOCATE;
      FROM Strings IMPORT StrCpy;
      IMPORT Done, CharSet, Components, CompSet;

      EXPORT (* exported by defmodule *) Append, Insert, ReleaseList,
					 DefaultList,
	     (* exported to env only  *) Start, Match, GetPattern,
					 GetTermChars;

      CONST
	 maxpatternlen = 16;
	 max = 32;            (* maximal number of patterns *)
      TYPE
	 PatternList = POINTER TO Pattern;
	 Pattern =
	    RECORD
	       str: ARRAY [0..maxpatternlen-1] OF CHAR;
	       compset: CompSet;
	       link: PatternList;
	    END;
	 PatternSet = SET OF [0..max-1];
      VAR
	 head, tail: PatternList;
	 cnt: CARDINAL;		(* # of patterns *)
	 match: PatternSet;	(* set of matching patterns *)
	 oldset: PatternSet;
	 card: CARDINAL;	(* cardinality of match *)
	 pos: CARDINAL;
	 termchars: CharSet;	(* set of termination chars *)

      PROCEDURE GetCompSet(pattern: ARRAY OF CHAR; VAR compset: CompSet);
	 VAR
	    index: CARDINAL;
      BEGIN
	 compset := CompSet{};
	 index := 0;
	 WHILE (index <= HIGH(pattern)) AND (pattern[index] <> 0C) DO
	    CASE pattern[index] OF
	    | 'y': INCL(compset, yearC);
	    | 'm': INCL(compset, monthC);
	    | 'd': INCL(compset, dayC);
	    | 'H': INCL(compset, hourC);
	    | 'M': INCL(compset, minuteC);
	    | 'S': INCL(compset, secondC);
	    ELSE
	    END;
	    INC(index);
	 END;
      END GetCompSet;

      (* calling order: Start Match { Match } [ GetPattern ] *)
      (* Match must not be called again after having returned FALSE *)

      PROCEDURE Start(compset: CompSet);
	 VAR
	    ptr: PatternList;
	    index: CARDINAL;
      BEGIN
	 pos := 0; card := cnt;
	 ptr := head; match := PatternSet{};
	 FOR index := 0 TO cnt-1 DO
	    IF ptr^.compset <= compset THEN
	       INCL(match, index);
	    END;
	    ptr := ptr^.link;
	 END;
      END Start;

      PROCEDURE Match(charset: CharSet) : BOOLEAN;
	 (* returns TRUE if cardinality of match > 1 *)
	 (*              and 0C not in charset       *)
	 (* Done is set to TRUE if there are any remaining patterns *)
	 VAR
	    index: CARDINAL;
	    ptr: PatternList;
      BEGIN
	 IF cnt = 0 THEN Done := FALSE; RETURN FALSE END;
	 oldset := match;
	 ptr := head;
	 FOR index := 0 TO cnt-1 DO
	    IF (index IN match) AND NOT (ptr^.str[pos] IN charset) THEN
	       EXCL(match, index);
	       DEC(card);
	    END;
	    ptr := ptr^.link;
	 END;
	 INC(pos);
	 Done := card # 0;
	 RETURN (card > 1) AND NOT (0C IN charset)
      END Match;

      PROCEDURE GetPattern(VAR pattern: ARRAY OF CHAR);
	 VAR
	    index: CARDINAL;
	    ptr: PatternList;
      BEGIN
	 IF (card = 0) AND (pos = 1) THEN
	    Done := FALSE; pattern[0] := 0C;
	 ELSE
	    IF card = 0 THEN
	       match := oldset;
	    END;
	    Done := TRUE;
	    ptr := head;
	    FOR index := 0 TO cnt-1 DO
	       IF index IN match THEN
		  StrCpy(pattern, ptr^.str); RETURN
	       END;
	       ptr := ptr^.link;
	    END;
	 END;
      END GetPattern;

      PROCEDURE TermChars(pattern: ARRAY OF CHAR);
	 CONST
	    Letters = CharSet{'A'..'Z', 'a'..'z'};
	    Digits = CharSet{'0'..'9'};
	 VAR
	    index: CARDINAL;
      BEGIN
	 index := 0;
	 WHILE (index <= HIGH(pattern)) AND (pattern[index] # 0C) DO
	    CASE pattern[index] OF
	    | 'm': termchars := termchars - Letters - Digits;
	    | 'y', 'd', 'H', 'M', 'S':
		  termchars := termchars - Digits;
	    ELSE
	       EXCL(termchars, pattern[index]);
	    END;
	    INC(index);
	 END;
      END TermChars;

      PROCEDURE GetTermChars(VAR charset: CharSet);
      BEGIN
	 charset := termchars;
      END GetTermChars;

      PROCEDURE Append(pattern: ARRAY OF CHAR);
	 VAR
	    new: PatternList;
      BEGIN
	 IF max = cnt THEN
	    Done := FALSE; RETURN
	 END;
	 Done := TRUE;
	 NEW(new);
	 WITH new^ DO
	    StrCpy(str, pattern);
	    GetCompSet(str, compset);
	    TermChars(str);
	    link := NIL;
	 END;
	 IF tail = NIL THEN
	    head := new;
	 ELSE
	    tail^.link := new;
	 END;
	 tail := new;
	 INC(cnt);
      END Append;

      PROCEDURE Insert(pattern: ARRAY OF CHAR);
	 VAR
	    new: PatternList;
      BEGIN
	 IF max = cnt THEN
	    Done := FALSE; RETURN
	 END;
	 Done := TRUE;
	 NEW(new);
	 WITH new^ DO
	    StrCpy(str, pattern);
	    GetCompSet(str, compset);
	    TermChars(str);
	    link := head;
	 END;
	 head := new;
	 INC(cnt);
      END Insert;

      PROCEDURE ReleaseList;
	 VAR old: PatternList;
      BEGIN
	 WHILE head # NIL DO
	    old := head;
	    head := head^.link;
	    DISPOSE(old);
	 END;
	 tail := NIL; cnt := 0;
	 termchars := CharSet{MIN(CHAR)..MAX(CHAR)};
      END ReleaseList;

      PROCEDURE DefaultList;
      BEGIN
	 Append("m/d/yH:M:S");
	 Append("m/d/yH:M");
	 Append("m/d/yH");
	 Append("m/d/y");
	 Append("m/dH:M:S");
	 Append("m/dH:M");
	 Append("m/dH");
	 Append("m/d");
	 Append("dH:M:S");
	 Append("dH:M");
	 Append("dH");
	 Append("yH:M:S");
	 Append("yH:M");
	 Append("yH");
	 Append("d.m.yH:M:S");
	 Append("d.m.yH:M");
	 Append("d.m.yH");
	 Append("d.m.y");
	 Append("d.mH:M:S");
	 Append("d.mH:M");
	 Append("d.mH");
	 Append("d.m");
	 Append("m,y");
	 Append("md,y");
	 Append("md");
	 Append("d");
	 Append("y");
	 Append("m");
	 Append("");
      END DefaultList;

   BEGIN
      head := NIL; tail := NIL; cnt := 0;
      termchars := CharSet{MIN(CHAR)..MAX(CHAR)};
      DefaultList;
   END TimePattern;

END TimeIO.
