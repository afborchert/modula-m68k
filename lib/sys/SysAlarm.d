DEFINITION MODULE SysAlarm;

   VAR
      previous: CARDINAL; (* previous amount *)

   PROCEDURE Alarm(sec: CARDINAL) : BOOLEAN;

END SysAlarm.
