(*%k+;u+*)
(****************************************
*                                       *
*     MODULA-2 Multi-Pass Compiler      *
*     ****************************      *
*                                       *
*     Implementation for ENDLICH        *
*                                       *
*     running on ENDLICH                *
*                                       *
*     MCStop:                           *
*                                       * 
*     abnormal termination              *
*     of the Modula-2 compiler          *
*                                       * 
****************************************)

IMPLEMENTATION MODULE MCStop;

   FROM StdIO IMPORT CloseAll;
   FROM SysExit IMPORT Exit;

   PROCEDURE Exit1;
   BEGIN
      Stop(1);
   END Exit1;

   PROCEDURE Stop(exitCode: CARDINAL);
      VAR ignore: BOOLEAN;
   BEGIN
      ignore := CloseAll(); (* for flushing buffered i/o *)
      Exit(exitCode);
      (* NOTREACHED *)
   END Stop;

END MCStop.
