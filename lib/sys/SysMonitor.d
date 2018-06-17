DEFINITION MODULE SysMonitor;

   FROM SYSTEM IMPORT ADDRESS;

   (* MUST be first procedure *)

   PROCEDURE Monitor(lowpc, highpc, buf: ADDRESS; bufsiz, cntsiz: CARDINAL);

END SysMonitor.
