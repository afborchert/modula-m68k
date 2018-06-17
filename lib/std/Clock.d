DEFINITION MODULE Clock;

   FROM SystemTypes IMPORT TIME;

   CONST UnitsPerSecond = 60;

   PROCEDURE RealTime(reset: BOOLEAN): TIME;
   PROCEDURE CPUTime (reset: BOOLEAN): TIME;
   (*
    *	These functions return the time in units elapsed since the start
    *	of the current process or since the last call with argument TRUE.
    *)

END Clock.
