IMPLEMENTATION MODULE Clock;

   FROM SYSTEM IMPORT ADR, UNIXCALL;
   FROM Sys IMPORT getrusage, gettimeofday;
   FROM SysTime IMPORT Time;
   FROM SystemTypes IMPORT TIME;

   TYPE
      TimesRec =
         RECORD
            utime: TIME;        (* CPU time while in user mode *)
            stime: TIME;        (* CPU time while in system mode *)
            cutime: TIME;       (* user time of all children *)
            cstime: TIME;       (* system time of all children *)
         END;
      TimeVal =
         RECORD
            tvsec: TIME;
            tvusec: TIME;
         END;

   VAR
      real: TimeVal;
      cpu: TIME;

   PROCEDURE GetProcessTimes(VAR times: TimesRec);
      CONST
         rusageSelf = 0;
         rusageChildren = -1;
      TYPE
         ResourceUsage =
            RECORD
               utime: TimeVal;  (* user time used *)
               stime: TimeVal;  (* system time used *)
               notOfInterest: ARRAY [0..13] OF INTEGER;
            END;
      VAR
         d0, d1: INTEGER;
         self, children: ResourceUsage;

      PROCEDURE TimeValToUnits(tval: TimeVal) : TIME;
      BEGIN
         RETURN tval.tvsec * UnitsPerSecond +
                tval.tvusec DIV 100 * UnitsPerSecond DIV 10000
      END TimeValToUnits;
 
   BEGIN
      IF UNIXCALL(getrusage, d0, d1, rusageSelf, ADR(self)) &
            UNIXCALL(getrusage, d0, d1, rusageChildren, ADR(children)) THEN
         times.utime := TimeValToUnits(self.utime);
         times.stime := TimeValToUnits(self.stime);
         times.cutime := TimeValToUnits(children.utime);
         times.cstime := TimeValToUnits(children.stime);
      ELSE
         times.utime := 0; times.stime := 0;
         times.cutime := 0; times.cstime := 0;
      END;
   END GetProcessTimes;

   PROCEDURE GetTimeVal(VAR timeval: TimeVal);
      VAR
         d0, d1: INTEGER;
   BEGIN
      IF ~UNIXCALL(gettimeofday, d0, d1, ADR(timeval), 0) THEN
         timeval.tvsec := 0;
         timeval.tvusec := 0;
      END;
   END GetTimeVal;

   PROCEDURE RealTime(reset: BOOLEAN) : TIME;
      (* return elapsed real time in units elapsed since the
         start of the process or since the last call with
         argument TRUE
      *)
      VAR
         result: TIME;
         buf   : TimeVal;

      PROCEDURE DiffInUnits(tval1, tval2: TimeVal) : TIME;
         VAR
            diff: TIME;
      BEGIN
         RETURN (tval2.tvsec - tval1.tvsec) * UnitsPerSecond + (tval2.tvusec
            - tval1.tvusec) DIV 100 * UnitsPerSecond DIV 10000;
      END DiffInUnits;

   BEGIN
      GetTimeVal(buf);
      result := DiffInUnits(real, buf);
      IF reset THEN
         real := buf;
      END;
      RETURN result
   END RealTime;

   PROCEDURE CPUTime (reset: BOOLEAN): TIME;
      VAR
         result: TIME;
         buf   : TimesRec;
   BEGIN
      GetProcessTimes(buf);
      result := buf.utime + buf.stime + buf.cutime + buf.cstime - cpu;
      IF reset THEN
         INC(cpu, result);
      END;
      RETURN result
   END CPUTime;

BEGIN
   GetTimeVal(real);
   cpu := 0;
END Clock.
