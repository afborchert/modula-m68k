DEFINITION MODULE RealInOut; (* AFB 6/84 * rev. wsc 2/85 *)

   FROM StdIO IMPORT FILE;

   VAR
      Done: BOOLEAN;

   (*
    *	Read REAL number x according to syntax:
    *
    *	["+" | "-"] digit { digit } ["." digit { digit } ]
    *	["E" ["+" | "-"] digit [digit] ]
    *
    *	Done := "a number was read".
    *
    *	at most 16 digits are significant, leading zeroes not
    *	counting. Maximum exponent is 76. Input terminates
    *	with a blank or any control character.
    *)

   PROCEDURE ReadReal(VAR x: REAL);

   PROCEDURE FreadReal(f: FILE; VAR x: REAL);

   (*
    *	Write x using n characters. If fewer than n characters
    *	are needed, leading blanks are inserted.
    *)

   PROCEDURE WriteReal(x: REAL; n: CARDINAL);

   PROCEDURE FwriteReal(f: FILE; x: REAL; n: CARDINAL);

   (*
    *	Write x in fixed point notation using pd digits in front
    *	of decimal point and dp digits behind decial point. If
    *	fewer than pd digits are needed, leading blanks are
    *	inserted.
    *)

   PROCEDURE WriteFloat(x: REAL; pd: CARDINAL; dp: CARDINAL);

   PROCEDURE FwriteFloat(f: FILE; x: REAL; pd: CARDINAL; dp: CARDINAL);

   (*
    *	Write x in octal/hexadecimal form with exponent and mantissa
    *)

   PROCEDURE WriteRealOct(x: REAL);

   PROCEDURE FwriteRealOct(f: FILE; x: REAL);

   PROCEDURE WriteRealHex(x: REAL);

   PROCEDURE FwriteRealHex(f: FILE; x: REAL);

END RealInOut.
