IMPLEMENTATION MODULE RealInOut; (* AFB 6/84 * rev. wsc 2/85 *)

   FROM FtdIO IMPORT FwriteString, FwriteChar, FreadChar;
   IMPORT FtdIO;
   FROM StdIO IMPORT stdin, stdout, FILE;
   FROM Strings IMPORT StrLen;
   IMPORT RealConv; (* basic routines for real conversions *)
   IMPORT Terminal;

   (* (* from definition module *)

   VAR
      Done: BOOLEAN;

   *)

   VAR fp: FILE;

   PROCEDURE Read(VAR ch: CHAR);
   BEGIN
      FreadChar(fp, ch);
   END Read;

   PROCEDURE Write(ch: CHAR);
   BEGIN
      FwriteChar(fp, ch);
   END Write;

   PROCEDURE WriteString(s: ARRAY OF CHAR);
   BEGIN
      FwriteString(fp, s);
   END WriteString;

   PROCEDURE FreadReal(f: FILE; VAR x: REAL);
   BEGIN
      fp := f;
      RealConv.ReadReal(Read, x);
      Done := RealConv.Done;
   END FreadReal;

   PROCEDURE ReadReal(VAR x: REAL);
   BEGIN
      RealConv.ReadReal(Terminal.Read, x);
      Done := RealConv.Done;
   END ReadReal;

   PROCEDURE FwriteReal(f: FILE; x: REAL; n: CARDINAL);

   (*
    *	wsc 2/85
    *
    *	standard version of WriteReal:
    *		output in floating point notation using
    *		'dp' decimal places.
    *)

      CONST
         maxdignum = 16;
         expdigits = 3;
         len = maxdignum + expdigits + 3; (* "." "e" "+"|"-" *)
      VAR
         field: ARRAY[0..len] OF CHAR;
         i: CARDINAL;

   BEGIN
      fp := f;
      RealConv.WriteFloat(field, x, 10, n);
      FOR i := StrLen(field)+1 TO n DO
         Write(" ")
      END;
      WriteString(field);
      Done := RealConv.Done AND FtdIO.Done;
   END FwriteReal;

   PROCEDURE WriteReal(x: REAL; n: CARDINAL);
   BEGIN
      FwriteReal(stdout, x, n);
   END WriteReal;

   PROCEDURE FwriteFloat(f: FILE; x: REAL; pd, dp: CARDINAL);

   (*
    *	wsc 2/85
    *
    *	extended version of WriteReal:
    *		output in fixed point notation using
    *		at least 'pd' places in front of the
    *		decimal point (leading blanks are 
    *		inserted if neccessary)
    *		and 'dp' decimal places.
    *)

      CONST
         maxdignum = 16;
         len = maxdignum + 1; (* "." *)
      VAR
         field: ARRAY[0..len] OF CHAR;
         help, i: CARDINAL;
         places: CARDINAL; (* returns number of zeros behind field *)

      PROCEDURE WriteField;
         (* output of field right alligned *)

         VAR
            help : CARDINAL;

      BEGIN
         IF dp >= maxdignum THEN (* 0.0000xxxxx *)
            FOR i:=3 TO pd DO Write(" ") END;
            IF field[0] = "-" THEN
               Write("-");
               help := StrLen(field) - 1
            ELSIF pd >= 2 THEN
               Write(" ");
               help := StrLen(field)
            ELSE
               help := StrLen(field)
            END; (* if *)
            WriteString("0.");
            FOR i := help+1 TO dp DO Write("0") END;
            IF field[0] = "-" THEN
               i := 1;
               WHILE (i <= len) AND (field[i] # 0C) DO
                  Write(field[i]);
                  INC(i)
               END (* while *)
            ELSE
               WriteString(field)
            END (* if *)
         ELSE (* dp < maxdignum *) (*     xxxx.xxxx *)
            FOR i := StrLen(field) - dp + 1 TO pd DO Write(" ") END;
            IF (dp # 0) AND (StrLen(field)-dp <= pd) THEN Write(" ") END;
            WriteString(field)
         END (* if *)
      END WriteField;

   BEGIN (* FwriteFloat *)
      fp := f;
      places := dp;
      RealConv.WriteFix(field, x, 10, places);

      IF places = 0 THEN (* no following zeros *)
         WriteField
      ELSIF places <= dp THEN (* add zeros behind decimal point *)
         dp := dp - places;
         WriteField;
         IF dp=0 THEN Write(".") END;
         FOR i := 1 TO places DO Write("0") END
      ELSE (* add zeros in front of and behind decimal point *)
         IF pd>places-dp THEN pd:=pd-(places-dp) ELSE pd:=0 END;
         help := dp;
         dp:=0;
         WriteField;
         dp := help;
         FOR i := 1 TO places - dp DO Write("0") END;
         IF dp # 0 THEN
            Write(".");
            FOR i := 1 TO dp DO Write("0") END
         END (* if *)
      END; (* if *)
      Done := RealConv.Done AND FtdIO.Done
   END FwriteFloat;

   PROCEDURE WriteFloat(x: REAL; pd, dp: CARDINAL);
   BEGIN
      FwriteFloat(stdout, x, pd, dp);
   END WriteFloat;


   PROCEDURE FwriteRealOct(f: FILE; x: REAL);
      CONST
         maxdignum = 19;
         expdigits = 3;
         len = maxdignum + expdigits + 3; (* "." "e" "+"|"-" *)
      VAR
         field: ARRAY[0..len] OF CHAR;
   BEGIN
      fp := f;
      RealConv.WriteFloat(field, x, 8, len+1);
      WriteString(field);
      Done := RealConv.Done AND FtdIO.Done;
   END FwriteRealOct;

   PROCEDURE WriteRealOct(x: REAL);
   BEGIN
      FwriteRealOct(stdout, x);
   END WriteRealOct;

   PROCEDURE FwriteRealHex(f: FILE; x: REAL);
      CONST
         maxdignum = 14;
         expdigits = 3;
         len = maxdignum + expdigits + 3; (* "." "e" "+"|"-" *)
      VAR
         field: ARRAY[0..len] OF CHAR;
   BEGIN
      fp := f;
      RealConv.WriteFloat(field, x, 16, len+1);
      WriteString(field);
      Done := RealConv.Done AND FtdIO.Done;
   END FwriteRealHex;

   PROCEDURE WriteRealHex(x: REAL);
   BEGIN
      FwriteRealHex(stdout, x);
   END WriteRealHex;

END RealInOut.
