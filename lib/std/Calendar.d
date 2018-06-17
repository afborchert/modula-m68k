DEFINITION MODULE Calendar;

   FROM SystemTypes IMPORT TIME;

(*
 *	Date calculations with
 *	(a)	Julius Caesar's calendar since Jan 01, 0001
 *	(b)	the Gregorian calendar	 since Oct 15, 1582
 *	(c)	Xelos system time.
 *
 *	Martin Hasch, University of Ulm, Jan 1988
 *)

   TYPE
      Time         = TIME;    			(* consecutive seconds *)
      Date         = LONGCARD;			(* consecutive days *)

      Year         = CARDINAL;
      Month        = [1..12];
      Day          = [1..31];
      Hour         = [0..23];
      Minute       = [0..59];
      Second       = [0..59];
      Weekday      = (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
      Week         = [1..53];
      Yearday      = [1..366];

      Daytime      = RECORD
			hour:    Hour;
			minute:  Minute;
			second:  Second;
		     END;
      Calendarday  = RECORD
			year:    Year;
			month:   Month;
			day:     Day;
		     END;
      CalendarInfo = RECORD
			weekday: Weekday;
			week:	 Week;
			yearday: Yearday;
		     END;

   PROCEDURE CurrentTime(): Time;
   (*
    *	returns actual system time = seconds since Jan 1, 1970, 00:00:00 GMT
    *)

   PROCEDURE ConvertTime(time: Time; VAR date: Date; VAR daytime: Daytime);
   PROCEDURE ConvertDate(date: Date; VAR calendarday: Calendarday);
   PROCEDURE ConvertCald(calendarday: Calendarday; VAR info: CalendarInfo);

   PROCEDURE CTime   (date: Date; daytime: Daytime):       Time;
   PROCEDURE CDate   (year: Year; month: Month; day: Day): Date;
   PROCEDURE CUltimo (year: Year; month: Month):	   Date;
   PROCEDURE CWeekday(date: Date):			   Weekday;

   PROCEDURE DateOK(year, month, day: CARDINAL): BOOLEAN;

   PROCEDURE TimeToString(time: Time; VAR string: ARRAY OF CHAR);
   (*
    *	converts time to a string, e.g. "Sun Sep 16 01:03:52 1973 GMT"
    *)

   PROCEDURE SetFirstOfWeek(weekday: Weekday);
   (*
    *	important for week of year calculation in ConvertCald; default is Mon.
    *)

   PROCEDURE GetTimezone (VAR tzname: ARRAY OF CHAR);
   PROCEDURE SetTimezone (    tzname: ARRAY OF CHAR);
   PROCEDURE GetLocaltime(VAR delay: Time);
   PROCEDURE SetLocaltime(    delay: Time);
   (*
    *	important for CTime, ConvertTime and TimeToString.
    *)

END Calendar.
