(* 
 *    F - formatted output to files  (ws 5/88)
 *    ========================================
 *)

DEFINITION MODULE F;

   FROM SYSTEM IMPORT BYTE;
   FROM StdIO IMPORT FILE;
   FROM Printf IMPORT FmtExitCode;

   (* --- diagnostic --- *)

   PROCEDURE done () : BOOLEAN;

   PROCEDURE success() : FmtExitCode;


   (* --- output procedures --- *)

   PROCEDURE printf0 (f : FILE; fmt : ARRAY OF CHAR);

   PROCEDURE printf1 (f : FILE; fmt : ARRAY OF CHAR; i1 : ARRAY OF BYTE);

   PROCEDURE printf2 (f : FILE; fmt : ARRAY OF CHAR; i1,i2 : ARRAY OF BYTE);

   PROCEDURE printf3 (f : FILE; fmt : ARRAY OF CHAR; i1,i2, i3 : ARRAY OF BYTE 
      );

   PROCEDURE printf4 (f : FILE; fmt : ARRAY OF CHAR; i1, i2, i3, i4 : ARRAY OF 
      BYTE);

   PROCEDURE printf5 (f : FILE; fmt : ARRAY OF CHAR; i1, i2, i3, i4, i5 : 
      ARRAY OF BYTE);

   PROCEDURE printf6 (f : FILE; fmt : ARRAY OF CHAR; i1, i2, i3, i4, i5, i6 : 
      ARRAY OF BYTE);

   PROCEDURE printf7 (f : FILE; fmt : ARRAY OF CHAR; i1, i2, i3, i4, i5, i6,
      i7 : ARRAY OF BYTE);

   PROCEDURE printf8 (f : FILE; fmt : ARRAY OF CHAR; i1, i2, i3, i4, i5, i6,
      i7 , i8 : ARRAY OF BYTE);


   (* --- set mode for errorhandling (default is "FmtError.Default") --- *)

   PROCEDURE setmode (mode : BITSET);

   PROCEDURE getmode (VAR mode : BITSET);
   
END F. 
