(* 
 *    L - formatted output to longstrings  (ws 6/88)
 *    ==============================================
 *        WARNING: output is always appended, long is never cleared
 *
 *)

DEFINITION MODULE L;

   FROM SYSTEM IMPORT BYTE;
   FROM Printf IMPORT FmtExitCode;
   FROM LongStrings IMPORT Long;

   (* --- diagnostic --- *)

   PROCEDURE done () : BOOLEAN;

   PROCEDURE success() : FmtExitCode;

   (* --- output procedures --- *)

   PROCEDURE printf0 (long : Long; fmt : ARRAY OF CHAR);

   PROCEDURE printf1 (long : Long; fmt : ARRAY OF CHAR; i1 : 
      ARRAY OF BYTE);

   PROCEDURE printf2 (long : Long; fmt : ARRAY OF CHAR; i1,i2 : 
      ARRAY OF BYTE);

   PROCEDURE printf3 (long : Long; fmt : ARRAY OF CHAR; i1,i2, i3 : 
      ARRAY OF BYTE );

   PROCEDURE printf4 (long : Long; fmt : ARRAY OF CHAR; i1, i2, i3,
      i4 : ARRAY OF BYTE);

   PROCEDURE printf5 (long : Long; fmt : ARRAY OF CHAR; i1, i2, i3,
      i4, i5 : ARRAY OF BYTE);

   PROCEDURE printf6 (long : Long; fmt : ARRAY OF CHAR; i1, i2, i3,
      i4, i5, i6 : ARRAY OF BYTE);

   PROCEDURE printf7 (long : Long; fmt : ARRAY OF CHAR; i1, i2, i3,
      i4, i5, i6, i7 : ARRAY OF BYTE);

   PROCEDURE printf8 (long : Long; fmt : ARRAY OF CHAR; i1, i2, i3,
      i4, i5, i6, i7 , i8 : ARRAY OF BYTE);

   (* --- set mode for errorhandling (default is "FmtError.Default") --- *)

   PROCEDURE setmode (mode : BITSET);

   PROCEDURE getmode (VAR mode : BITSET);

END L. 
