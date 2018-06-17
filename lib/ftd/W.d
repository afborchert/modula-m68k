(* 
 *    W - formatted output to windows (ws 3/89)
 *    =========================================
 *)

DEFINITION MODULE W;

   FROM SYSTEM IMPORT BYTE;
   FROM Printf IMPORT FmtExitCode;
   FROM Windows IMPORT Window;

   PROCEDURE done () : BOOLEAN;

   PROCEDURE success() : FmtExitCode;

   PROCEDURE setmode (mode : BITSET);

   PROCEDURE getmode (VAR mode : BITSET);

   PROCEDURE printf0 (w : Window; line, col : CARDINAL; fmt : ARRAY OF CHAR);

   PROCEDURE printf1 (w : Window; line, col : CARDINAL; fmt : ARRAY OF CHAR;
      i1 : ARRAY OF BYTE);

   PROCEDURE printf2 (w : Window; line, col : CARDINAL; fmt : ARRAY OF CHAR;
      i1,i2 : ARRAY OF BYTE);

   PROCEDURE printf3 (w : Window; line, col : CARDINAL; fmt : ARRAY OF CHAR;
      i1,i2, i3 : ARRAY OF BYTE );

   PROCEDURE printf4 (w : Window; line, col : CARDINAL; fmt : ARRAY OF CHAR;
      i1, i2, i3, i4 : ARRAY OF BYTE);

   PROCEDURE printf5 (w : Window; line, col : CARDINAL; fmt : ARRAY OF CHAR;
      i1, i2, i3, i4, i5 : ARRAY OF BYTE);

   PROCEDURE printf6 (w : Window; line, col : CARDINAL; fmt : ARRAY OF CHAR;
      i1, i2, i3, i4, i5, i6 : ARRAY OF BYTE);

   PROCEDURE printf7 (w : Window; line, col : CARDINAL; fmt : ARRAY OF CHAR;
      i1, i2, i3, i4, i5, i6, i7 : ARRAY OF BYTE);

   PROCEDURE printf8 (w : Window; line, col : CARDINAL; fmt : ARRAY OF CHAR;
      i1, i2, i3, i4, i5, i6, i7 , i8 : ARRAY OF BYTE);

   PROCEDURE append0 (w : Window; fmt : ARRAY OF CHAR);

   PROCEDURE append1 (w : Window; fmt : ARRAY OF CHAR; i1 : ARRAY OF BYTE);

   PROCEDURE append2 (w : Window; fmt : ARRAY OF CHAR; i1,i2 : ARRAY OF BYTE);

   PROCEDURE append3 (w : Window; fmt : ARRAY OF CHAR; i1,i2, i3 : ARRAY OF 
      BYTE );

   PROCEDURE append4 (w : Window; fmt : ARRAY OF CHAR; i1, i2, i3, i4 : 
      ARRAY OF BYTE);

   PROCEDURE append5 (w : Window; fmt : ARRAY OF CHAR; i1, i2, i3, i4, i5 : 
      ARRAY OF BYTE);

   PROCEDURE append6 (w : Window; fmt : ARRAY OF CHAR; i1, i2, i3, i4, i5, i6 
      : ARRAY OF BYTE);

   PROCEDURE append7 (w : Window; fmt : ARRAY OF CHAR; i1, i2, i3, i4, i5, i6,
      i7 : ARRAY OF BYTE);

   PROCEDURE append8 (w : Window; fmt : ARRAY OF CHAR; i1, i2, i3, i4, i5, i6,
      i7 , i8 : ARRAY OF BYTE);

END W. 
