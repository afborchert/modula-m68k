(* 
 *    S - formatted output to strings  (ws 6/88)
 *    ==========================================
 *)

DEFINITION MODULE S;

   FROM SYSTEM IMPORT BYTE;
   FROM Printf IMPORT FmtExitCode;

   (* --- diagnostic --- *)

   PROCEDURE done () : BOOLEAN;

   PROCEDURE success() : FmtExitCode;

   (* --- output procedures --- *)

   PROCEDURE printf0 (VAR s : ARRAY OF CHAR; fmt : ARRAY OF CHAR);

   PROCEDURE printf1 (VAR s : ARRAY OF CHAR; fmt : ARRAY OF CHAR; i1 : 
      ARRAY OF BYTE);

   PROCEDURE printf2 (VAR s : ARRAY OF CHAR; fmt : ARRAY OF CHAR; i1,i2 : 
      ARRAY OF BYTE);

   PROCEDURE printf3 (VAR s : ARRAY OF CHAR; fmt : ARRAY OF CHAR; i1,i2, i3 : 
      ARRAY OF BYTE );

   PROCEDURE printf4 (VAR s : ARRAY OF CHAR; fmt : ARRAY OF CHAR; i1, i2, i3,
      i4 : ARRAY OF BYTE);

   PROCEDURE printf5 (VAR s : ARRAY OF CHAR; fmt : ARRAY OF CHAR; i1, i2, i3,
      i4, i5 : ARRAY OF BYTE);

   PROCEDURE printf6 (VAR s : ARRAY OF CHAR; fmt : ARRAY OF CHAR; i1, i2, i3,
      i4, i5, i6 : ARRAY OF BYTE);

   PROCEDURE printf7 (VAR s : ARRAY OF CHAR; fmt : ARRAY OF CHAR; i1, i2, i3,
      i4, i5, i6, i7 : ARRAY OF BYTE);

   PROCEDURE printf8 (VAR s : ARRAY OF CHAR; fmt : ARRAY OF CHAR; i1, i2, i3,
      i4, i5, i6, i7 , i8 : ARRAY OF BYTE);

   (* --- set mode for errorhandling (default is "FmtError.Default") --- *)

   PROCEDURE setmode (mode : BITSET);

   PROCEDURE getmode (VAR mode : BITSET);

END S. 
