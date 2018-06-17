(* 
 *    P - formatted output to stdout  (ws 6/88)
 *    =========================================
 *)

DEFINITION MODULE P;

   FROM Printf IMPORT FmtExitCode;
   FROM SYSTEM IMPORT BYTE;

   (* --- diagnostic --- *)

   PROCEDURE done () : BOOLEAN;

   PROCEDURE success() : FmtExitCode;

   (* --- output procedures --- *)

   PROCEDURE rintf0 (fmt : ARRAY OF CHAR);

   PROCEDURE rintf1 (fmt : ARRAY OF CHAR; i1 : ARRAY OF BYTE);

   PROCEDURE rintf2 (fmt : ARRAY OF CHAR; i1,i2 : ARRAY OF BYTE);

   PROCEDURE rintf3 (fmt : ARRAY OF CHAR; i1,i2, i3 : ARRAY OF BYTE );

   PROCEDURE rintf4 (fmt : ARRAY OF CHAR; i1, i2, i3, i4 : ARRAY OF BYTE);

   PROCEDURE rintf5 (fmt : ARRAY OF CHAR; i1, i2, i3, i4, i5 : ARRAY OF BYTE);

   PROCEDURE rintf6 (fmt : ARRAY OF CHAR; i1, i2, i3, i4, i5, i6 : ARRAY OF 
      BYTE);

   PROCEDURE rintf7 (fmt : ARRAY OF CHAR; i1, i2, i3, i4, i5, i6, i7 : 
      ARRAY OF BYTE);

   PROCEDURE rintf8 (fmt : ARRAY OF CHAR; i1, i2, i3, i4, i5, i6, i7 , i8 : 
      ARRAY OF BYTE);

   (* --- set mode for errorhandling (default is "FmtError.Default") --- *)

   PROCEDURE setmode (mode : BITSET);

   PROCEDURE getmode (VAR mode : BITSET);

END P. 
