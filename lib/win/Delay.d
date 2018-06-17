(* Modula-2 Library    -  UNIX System V  -     AFB 6/88 *)
(* (c) Universitaet Ulm, Sektion Informatik, D-7900 Ulm *)
DEFINITION MODULE Delay;

   FROM StdIO IMPORT FILE;

   TYPE
      OutProc = PROCEDURE (CHAR);

   PROCEDURE InitDelay(baudrate: CARDINAL; padch: CHAR; outc: OutProc);

   PROCEDURE InitDelayFile(baudrate: CARDINAL; padch: CHAR; fp: FILE);

   PROCEDURE Delay(tenthofmillisecs: CARDINAL);

END Delay.
