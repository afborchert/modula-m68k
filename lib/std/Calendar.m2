IMPLEMENTATION MODULE Calendar;
(*
 *			Martin Hasch, University of Ulm, Jan 12, 1988
 *)

   FROM Environment IMPORT GetEnv;
   FROM Strings	    IMPORT StrCpy, StrCat;
   IMPORT SysTime;

   CONST
      SecsPerDay = 24 * 60 * 60;
      TimebaseDn = 719470;			(* = DayNo(1970,1,1) *)

   VAR
      localtime:      Time;			(* seconds relative to GMT *)
      tzname:	      ARRAY [0..255] OF CHAR;	(* usually 3 characters *)
      firstofweek:    Weekday;
      maxcalendarday: Calendarday;

   (*
    *	supported format of TZ environment parameter:
    *
    *	[arbitrary_nonnumeric_string ]delta[ more_characters] .
    *
    *	The string becomes tzname if present. Delta is an integer with
    *	absolute value < 60, representing the difference between local time
    *	and GMT in hours, or a larger integer number of seconds. Examples:
    *	"MEZ-1", "MESZ-2" . If delta is not present, "GMT0" is taken instead.
    *)
   PROCEDURE PresetLocaltime;			(* look at env. par. "TZ" *)
      TYPE CharSet = SET OF CHAR;
      CONST
	 Digits = CharSet{"0".."9"};
	 NumChars = CharSet{0C,"+","-"} + Digits;
      VAR
	 aux,
	 index:	 CARDINAL;
	 negative,
	 found:	 BOOLEAN;
	 tzparm: ARRAY [0..255] OF CHAR;
   BEGIN
      GetEnv("TZ",tzparm,found);
      IF found THEN
	 index := 0;
	 WHILE (index <= HIGH(tzparm)) & ~(tzparm[index] IN NumChars) DO
	    INC(index)
	 END
      END;
      IF ~found OR (index > HIGH(tzparm)) OR (tzparm[index] = 0C) THEN
	 aux := 0;
	 StrCpy(tzname,"GMT");
	 RETURN
      END;
      StrCpy(tzname,tzparm);
      IF index <= HIGH(tzname) THEN
	 tzname[index] := 0C
      END;			(* what follows is a self-made sscanf("%d") *)
      negative := tzparm[index] = "-";
      IF negative OR (tzparm[index] = "+") THEN
	 INC(index)
      END;
      aux := 0;
      WHILE (index <= HIGH(tzparm)) & (tzparm[index] IN Digits) DO
	 aux := ORD(tzparm[index]) - ORD("0") + aux * 10;
	 INC(index)
      END;
      IF aux < 60 THEN			(* small number => amount in hours *)
	 aux := aux * (60 * 60)
      ELSIF aux > SecsPerDay THEN	(* number too big even for seconds *)
	 aux := 0
      END;
      IF negative THEN
	 localtime := -Time(aux)
      ELSE
	 localtime := Time(aux)
      END
   END PresetLocaltime;

   PROCEDURE SetFirstOfWeek(weekday: Weekday);
   BEGIN
      firstofweek := weekday
   END SetFirstOfWeek;

   PROCEDURE GetTimezone(VAR name: ARRAY OF CHAR);
   BEGIN
      StrCpy(name,tzname)
   END GetTimezone;

   PROCEDURE SetTimezone(name: ARRAY OF CHAR);
   BEGIN
      StrCpy(tzname,name)
   END SetTimezone;

   PROCEDURE GetLocaltime(VAR ltime: Time);
   BEGIN
      ltime := localtime
   END GetLocaltime;

   PROCEDURE SetLocaltime(ltime: Time);
   BEGIN
      localtime := ltime
   END SetLocaltime;

   PROCEDURE CurrentTime(): Time;
      VAR res: Time;
   BEGIN
      IF ~SysTime.Time(res) THEN
	 RETURN 0			(* this should not be expected *)
      END;
      RETURN res
   END CurrentTime;

   PROCEDURE CTime(date: Date; daytime: Daytime): Time;
   (*
    *	Huge date values cause undefined result.
    *)
   BEGIN
      WITH daytime DO
	 RETURN
	    (Time(date) - Time(TimebaseDn)) * SecsPerDay
	    + Time(hour * (60 * 60) + minute * 60 + second) + localtime
      END
   END CTime;

   (*
    *	The basic formula:
    *)
   PROCEDURE CDate(year: Year; month: Month; day: Day): Date;
   (*
    *	correct results guaranteed for correct dates only; other usages are
    *	not encouraged outside this module
    *)
   BEGIN				(*$R- (turn range checks off) *)
      IF month < 3 THEN
	 INC(month,9); DEC(year)
      ELSE
	 DEC(month,3)
      END;
      INC(day, (month * 153 + 2) DIV 5 + year * 1461 DIV 4 - 1 );
      IF day >= 578053 THEN	(* Gregor calendar reformation Oct 15, 1582 *)
	 DEC(day, (day DIV 36525 * 3 - 5) DIV 4 )
      END;
      RETURN day
   END CDate;				(*$R= (reset range checks flag) *)

   PROCEDURE CWeekday(date: Date): Weekday;
   (*
    *	Which is the first day in the enumeration of Weekday can be changed
    *	without changing this.
    *)
   BEGIN
      RETURN VAL(Weekday, (date + ORD(Mon)) MOD 7 )
   END CWeekday;

   PROCEDURE ConvertTime(time: Time; VAR date: Date; VAR daytime: Daytime);
      VAR remainder: Time;
   BEGIN
      DEC(time,localtime);
      IF time >= 0 THEN
	 remainder := time MOD SecsPerDay;
	 date := Date(time DIV SecsPerDay) + TimebaseDn
      ELSE			(* negative integers - machine dependent! *)
	 remainder := time MOD SecsPerDay;
	 IF remainder < 0 THEN
	    INC(remainder, SecsPerDay);
	 END;
	 date := Date( TimebaseDn - 1 + (time + 1 - remainder) DIV SecsPerDay )
      END;
      WITH daytime DO
	 second := remainder MOD 60;
	 minute := remainder DIV 60 MOD 60;
	 hour   := remainder DIV (60 * 60) MOD 24;
      END
   END ConvertTime;

   PROCEDURE ConvertDate(date: Date; VAR calendarday: Calendarday);
      VAR
	 aux: CARDINAL;
   BEGIN
      IF date >= 578043 THEN	(* Gregor calendar reformation Oct 15, 1582 *)
	 INC(date, ((date * 4 - 5) DIV 146097 * 3 - 5) DIV 4)
      END;
      WITH calendarday DO
	 year := (date * 4 + 3) DIV 1461;
	 DEC(date, year * 1461 DIV 4);
	 aux := (date * 5 + 2) DIV 153;
	 day := (date * 5 + 2) MOD 153 DIV 5 + 1;
	 IF aux > 9 THEN
	    month := aux - 9; INC(year)
	 ELSE
	    month := aux + 3;
	 END
      END
   END ConvertDate;

   PROCEDURE ConvertCald(calendarday: Calendarday; VAR info: CalendarInfo);
      PROCEDURE FirstWeek(newyear: Date): Date;
      BEGIN
	 RETURN
	    newyear + 3
	    - (ORD(CWeekday(newyear)) + 10 - ORD(firstofweek)) MOD 7
      END FirstWeek;

      VAR today, lastnewyear, firstweek: Date;
   BEGIN
      WITH calendarday DO
	 today := CDate(year,month,day);
	 lastnewyear := CDate(year,1,1);
	 firstweek := FirstWeek(lastnewyear);
	 IF today < firstweek THEN
	    firstweek := FirstWeek(CDate(year-1,1,1))
	 END;
	 WITH info DO
	    weekday := CWeekday(today);
	    yearday := today - lastnewyear + 1;
	    week := (today - firstweek) DIV 7 + 1;
	 END
      END
   END ConvertCald;

   PROCEDURE CUltimo(year: Year; month: Month): Date;
   BEGIN
      RETURN CDate(year,month+1,1) - 1
   END CUltimo;

   PROCEDURE DateOK(year, month, day: CARDINAL): BOOLEAN;
      CONST
	 month30 = {4, 6, 9, 11};
      VAR
	 leapyear: BOOLEAN;
   BEGIN
      IF (year = 0) OR (month = 0) OR (month > 12) OR
	 (day = 0) OR (day > 31) THEN
	 RETURN FALSE
      END;
      IF year > 1582 THEN
	 leapyear := (year MOD 4 = 0) AND
		     ((year MOD 100 # 0) OR (year MOD 400 = 0));
      ELSIF year = 1582 THEN
	 leapyear := FALSE;
	 IF (month = 10) AND (day > 4) AND (day < 15) THEN
	    RETURN FALSE
	 END;
      ELSE
	 leapyear := year MOD 4 = 0
      END;
      IF (month IN month30) AND (day = 31) THEN
	 RETURN FALSE
      ELSIF (month = 2) AND ((day > 29) OR ~leapyear & (day = 29)) THEN
	 RETURN FALSE
      END;
      IF year # maxcalendarday.year THEN
	 RETURN year < maxcalendarday.year
      END;
      IF month # maxcalendarday.month THEN
	 RETURN month < maxcalendarday.month
      END;
      RETURN day <= maxcalendarday.day
   END DateOK;

   PROCEDURE TimeToString(time: Time; VAR string: ARRAY OF CHAR);
      PROCEDURE Num2(val: CARDINAL);
      (* copies last two digits of val into string *)
	 VAR substr: ARRAY [0..1] OF CHAR;
      BEGIN
	 substr[0] := CHR( val DIV 10 MOD 10 + ORD("0") );
	 substr[1] := CHR( val MOD 10 + ORD("0") );
	 StrCat(string,substr)
      END Num2;

      VAR
	 date:  Date;
	 cday:  Calendarday;
	 dtime: Daytime;
   BEGIN
      ConvertTime(time,date,dtime);
      ConvertDate(date,cday);
      CASE CWeekday(date) OF
	 Mon:  StrCpy(string,"Mon")
      |  Tue:  StrCpy(string,"Tue")
      |  Wed:  StrCpy(string,"Wed")
      |  Thu:  StrCpy(string,"Thu")
      |  Fri:  StrCpy(string,"Fri")
      |  Sat:  StrCpy(string,"Sat")
      |  Sun:  StrCpy(string,"Sun")
      END;
      StrCat(string," ");
      WITH cday DO
	 CASE month OF
	    1: StrCat(string,"Jan")
	 |  2: StrCat(string,"Feb")
	 |  3: StrCat(string,"Mar")
	 |  4: StrCat(string,"Apr")
	 |  5: StrCat(string,"May")
	 |  6: StrCat(string,"Jun")
	 |  7: StrCat(string,"Jul")
	 |  8: StrCat(string,"Aug")
	 |  9: StrCat(string,"Sep")
	 | 10: StrCat(string,"Oct")
	 | 11: StrCat(string,"Nov")
	 | 12: StrCat(string,"Dec")
	 END;
	 StrCat(string," ");
	 Num2(day);
	 StrCat(string," ");
	 WITH dtime DO
	    Num2(hour);
	    StrCat(string,":");
	    Num2(minute);
	    StrCat(string,":");
	    Num2(second);
	 END;
	 StrCat(string," ");
	 Num2(year DIV 100);
	 Num2(year);
      END;
      StrCat(string," ");	(* up to now totally fixed length format *)
      StrCat(string,tzname)	(* length of tzname, however, can vary *)
   END TimeToString;

BEGIN
   PresetLocaltime;
   SetFirstOfWeek(Mon);
   ConvertDate(MAX(Date) DIV 5, maxcalendarday); (* DIV 5: avoid overflow *)
END Calendar.
