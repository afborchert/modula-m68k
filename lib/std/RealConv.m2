IMPLEMENTATION MODULE RealConv;        (* AFB 6/84 * rev. wsc 2/85 *)

   (* (* from definition module *)
   TYPE
      ReadProc = PROCEDURE(VAR CHAR);
   VAR
      Done: BOOLEAN;
      termCH: CHAR;
   *)

   (* see p1/MCP1Reals.m2 *)

   CONST 
      eof = 0C;
      nl  = 12C;
      tab = 11C;

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

   PROCEDURE ReadReal(Read: ReadProc; VAR x: REAL);
      CONST 
         (* machine dependent *)
         maxexp    = 309;
         minexp    = -307;
         maxdignum = 16;

      VAR 
         base   : REAL;
         period : BOOLEAN;
         exp    : INTEGER;
         dignum : INTEGER;
         rval   : REAL;
         eval   : INTEGER;               (* value of exponent *)
         eok    : BOOLEAN;               (* exponent okay *)
         negexp : BOOLEAN;               (* negative exponent *)
         err    : BOOLEAN;		 (* error flag *)
         minus  : BOOLEAN;
         inexp  : BOOLEAN;		 (* in exponent *)
         ch     : CHAR;
         dfound : BOOLEAN;		 (* digit found *)

      PROCEDURE ConvertToFraction(ch: CHAR);
      BEGIN 
         IF ch = '.' THEN 
            period := TRUE;
         ELSE 
            IF (dignum = 0) AND (ch = '0') THEN 
               IF period THEN 
                  DEC(exp)
               END;
            ELSIF dignum < maxdignum THEN 
               rval := rval * 10.0 + FLOAT(ORD(ch) - ORD( '0'));
               IF period THEN 
                  DEC(exp)
               END;
               INC(dignum);
            ELSE 
               base := base / 10.0;
               rval := rval + base * FLOAT(ORD(ch) - ORD( '0'));
               IF NOT period THEN 
                  INC(exp)
               END;
            END;
         END;
      END ConvertToFraction;

      PROCEDURE ConvertToExponent(ch: CHAR);
      BEGIN 
         IF ch = '-' THEN 
            negexp := TRUE;
         ELSIF ch = '+' THEN
            (* nothing *)
         ELSE 
            IF eval < 1000 THEN 
               eval := eval * 10 + VAL(INTEGER, ORD(ch) - ORD( '0'));
            ELSE 
               eok := FALSE;
            END;
         END;
      END ConvertToExponent;

      PROCEDURE TermRealConst;
      (* terminate the calculation of a constant real number *)
         VAR 
            totexp : INTEGER;

         PROCEDURE TenTo(exp: INTEGER): REAL;
            VAR 
               r, res : REAL;

         BEGIN 
            res := 1.0;
            r := 10.0;
            LOOP 
               IF ODD(exp) THEN 
                  res := res * r 
               END;
               exp := exp DIV 2;
               IF exp = 0 THEN 
                  EXIT 
               END;
               r := r * r;
            END;
            RETURN res 
         END TenTo;

      BEGIN 
         err := FALSE;
         IF eok THEN 
            IF negexp THEN 
               DEC(exp,eval)
            ELSE 
               INC(exp,eval)
            END;
            totexp := dignum + exp;
            IF totexp > maxexp THEN 
               exp := 0;
               err := TRUE;
            ELSIF totexp < minexp THEN 
               exp := 0;
               rval := 0.0;
            END;
            IF exp > 0 THEN             (* maximal value is maxexp - 1 *)
               rval := rval * TenTo(exp);
            ELSIF exp < 0 THEN        (* minimal value is minexp - maxdignum *)
               IF exp < minexp THEN 
                  rval := rval / TenTo(minexp - exp);
                  exp := minexp;
               END;
               rval := rval / TenTo(- exp);
            END;
         ELSE                           (* not eok *)
            IF dignum <> 0 THEN 
               err := TRUE 
            END;
         END;
         IF err THEN 
            rval := 0.0 
         END;
      END TermRealConst;
      
      PROCEDURE Finish;
      BEGIN
	 IF dfound THEN
	    TermRealConst;
	    IF NOT err THEN
	       x := rval;
	       IF minus THEN x := -x END;
	       Done := TRUE;
	    END;
	 END;
      END Finish;

   BEGIN 
      (* init *)
      dfound := FALSE;
      inexp := FALSE;
      rval := 0.0;
      base := 1.0;
      period := FALSE;
      exp := 0;
      dignum := 0;
      eval := 0;
      eok := TRUE;
      negexp := FALSE;
      minus := FALSE;
      (* skip white space *)
      Read(ch);
      WHILE (ch <> eof) AND ((ch = ' ') OR (ch = tab) OR (ch = nl)) DO 
         Read(ch);
      END;
      Done := FALSE;
      x := 0.0;
      IF ch = eof THEN
         RETURN 
      END;
      IF (ch = '-') OR (ch = '+') THEN 
         IF ch = '-' THEN 
            minus := TRUE;
         END;
         Read(ch);
         IF ch = eof THEN
            RETURN 
         END;
      END;
      dfound := ('0' <= ch) AND (ch <= '9');
      WHILE ch <> eof DO 
         IF (ch = 'e') OR (ch = 'E') THEN 
            inexp := TRUE;
         ELSIF (ch >= '0') AND (ch <= '9') OR (ch = '-') AND inexp
            OR (ch = '.') AND NOT period OR (ch = '+') AND inexp THEN
            IF inexp THEN 
               IF (ch = '.') THEN 
                  eok := FALSE;
               ELSIF eok THEN 
                  ConvertToExponent(ch);
               END;
            ELSIF (ch <> '+') THEN
               ConvertToFraction(ch);
            END;
         ELSE 
            termCH := ch;
	    Finish;
            RETURN
         END;
         Read(ch);
      END;                              (* WHILE *)
      termCH := 0C;
      Finish;
   END ReadReal;



   PROCEDURE WriteFloat(VAR field: ARRAY OF CHAR; x: REAL; cbase: CARDINAL;
                       dp : CARDINAL );

   (*
    *	rev. wsc 2/85
    *
    *	standard version of WriteReal:
    *		floating point notation using
    *		'dp' decimal places.
    *)

      CONST
         digits = "0123456789ABCDEF";
      VAR
         Digits: ARRAY[0..15] OF CHAR;
         maxdignum: CARDINAL;
         maxexpnum: CARDINAL;
         fi: CARDINAL; (* index of field *)
         exp: INTEGER;
         fract: REAL;
         roundoff: REAL;
         minus: BOOLEAN;
         negexp: BOOLEAN;
         i: CARDINAL;
         digit: CARDINAL;
         base: INTEGER; (* equal to cbase; but of type INTEGER *)

      PROCEDURE Write(ch: CHAR);
      BEGIN
         IF fi <= HIGH(field) THEN
            field[fi] := ch;
            INC(fi);
         ELSIF ch <> 0C THEN
            Done := FALSE;
         END;
      END Write;

   BEGIN 
      (* init *)
      Done := TRUE;
      base := cbase;
      Digits := digits;
      fi := 0;
      CASE base OF
      |  8: maxdignum := 19; maxexpnum := 3; (* octal *)
      | 10: maxdignum := 16; maxexpnum := 3; (* decimal *)
      | 16: maxdignum := 14; maxexpnum := 3; (* hexadecimal *)
      ELSE (* shouldn't happen *)
         Done := FALSE;
         RETURN
      END; (* case *)
      IF (dp<=maxexpnum+4) OR (dp>maxdignum+maxexpnum+4) THEN 
         dp:=maxdignum+maxexpnum+4
      END; (* if *)
      dp := dp-maxexpnum-4;

      (* some floating point processors (like MC68881) have infinity... *)
      IF x > MAX(REAL) THEN
	 x := MAX(REAL);
      ELSIF x < MIN(REAL) THEN
	 x := MIN(REAL);
      END;

      (* calculate exp and fract *)
      exp := 0;
      fract := x;
      minus := x < 0.0;
      IF minus THEN fract := - fract END;
      negexp := (fract < 1.0) AND (fract # 0.0);
      IF fract # 0.0 THEN
         IF negexp THEN
            WHILE fract < 1.0 DO
               INC(exp);
               fract := fract * FLOAT(base)
            END (* while *)
         ELSE
            WHILE fract >= FLOAT(base) DO
               INC(exp);
               fract := fract / FLOAT(base)
            END (* while *)
         END (* if *)
      END; (* if *)

      (*****
      (*  roundoff  *)
      roundoff:=FLOAT(base)/2.0;
      FOR i:=1 TO dp DO roundoff:=roundoff/FLOAT(base) END;
      fract:=fract+roundoff;
      (* correction *)
      IF fract >= FLOAT(base) THEN
         fract:=fract/FLOAT(base);
         IF negexp THEN
            DEC(exp);
            negexp := exp > 0
         ELSE
            INC(exp)
         END (* if *)
      END; (* if *)
      *****)

      (* write mantissa *)
      IF minus THEN Write("-") ELSE Write(" ") END;
      FOR i:=1 TO dp DO
         digit:=TRUNC(fract); (* =>  0<=digit<=(base-1) *)
         fract:=(fract-FLOAT(digit))*FLOAT(base);
         Write(Digits[digit]);
         IF i = 1 THEN Write(".") END
      END; (* for *)

      (* write exponent *)
      Write("e");
      IF negexp THEN Write("-") ELSE Write("+") END;
      IF maxexpnum = 3 THEN
         IF exp < base*base THEN
            Write("0")
         ELSE
            Write(Digits[exp DIV (base*base)]);
            exp := exp MOD (base*base)
         END
      END;
      IF exp < base THEN
         Write("0")
      ELSE
         Write(Digits[exp DIV base])
      END;
      Write(Digits[exp MOD base]);

      Write(0C);
   END WriteFloat;

   PROCEDURE WriteFix(VAR field: ARRAY OF CHAR; x: REAL; cbase: CARDINAL;
                       VAR dp : CARDINAL );

   (*
    *	wsc 2/85
    *
    *	extended version of WriteReal:
    *		fixed point notation
    *)

      CONST
         digits = "0123456789ABCDEF";
      VAR
         Digits: ARRAY[0..15] OF CHAR;
         maxdignum: CARDINAL;
         fi: CARDINAL; (* index of field *)
         exp: CARDINAL;
         fract: REAL;
         roundoff: REAL;
         minus: BOOLEAN;
         negexp: BOOLEAN;
         i: CARDINAL;
         base: INTEGER; (* equal to cbase; but of type INTEGER *)
         dignum: CARDINAL; (* number of written digits *)

      PROCEDURE Write(ch: CHAR);
      BEGIN
         IF fi <= HIGH(field) THEN
            field[fi] := ch;
            INC(fi);
         ELSIF ch <> 0C THEN
            Done := FALSE;
         END;
      END Write;

      PROCEDURE WriteDigit;
         VAR
            digit: CARDINAL;
      BEGIN
         digit := TRUNC(fract);
         fract := (fract - FLOAT(digit)) * FLOAT(base);
         Write(Digits[digit])
      END WriteDigit;

   BEGIN 

      (* init *)
      Done := TRUE;
      base := INTEGER(cbase);
      Digits := digits;
      fi := 0;
      dignum := 0;
      CASE base OF
        8: maxdignum := 19 (* octal *)
      | 10: maxdignum := 16 (* decimal *)
      | 16: maxdignum := 14 (* hexadecimal *)
      ELSE (* shouldn't happen *)
         Done := FALSE;
         RETURN
      END; (* case *)
      fract := x;
      minus := x < 0.0;
      IF minus THEN fract := -fract END;

      (* calculate exp and fract *)
      exp := 0;
      negexp := (fract < 1.0) AND (fract # 0.0);
      IF fract # 0.0 THEN
         IF negexp THEN
            WHILE fract < 1.0 DO
               INC(exp);
               fract := fract * FLOAT(base)
            END (* while *)
         ELSE
            WHILE fract >= FLOAT(base) DO
               INC(exp);
               fract := fract / FLOAT(base)
            END (* while *)
         END (* if *)
      END; (* if *)

      (* roundoff *)
      roundoff := FLOAT(base)/2.0;
      IF negexp THEN
         IF exp <= dp THEN
            FOR i := exp TO dp DO
               roundoff := roundoff/FLOAT(base)
            END (* for *)
         (* ELSE nix *)
         END (* if *)
      ELSE (* positive ep. *)
         i := 0;
         WHILE (i <= dp + exp) AND (i < maxdignum ) DO
            roundoff := roundoff/FLOAT(base);
            INC(i)
         END (* while *)
      END; (* if *)
      fract := fract + roundoff;
      (* correction *)
      IF fract >= FLOAT(base) THEN
         fract:=fract/FLOAT(base);
         IF negexp THEN
            DEC(exp);
            negexp := exp > 0
         ELSE
            INC(exp)
         END (* if *)
      END; (* if *)

      (* write number *)
      IF minus THEN Write("-") END;
      IF NOT negexp THEN
         WHILE (exp > 0) AND (dignum < maxdignum) DO
            WriteDigit;
            INC(dignum);
            DEC(exp)
         END; (* while *)
         IF dignum = maxdignum THEN (* xxxxxx0000.0000 *)
            Write(0C);
            dp := dp + exp + 1;
            RETURN
         ELSIF (dignum+1 = maxdignum) OR (dp = 0) THEN (* xxxxxxx.0000 *)
            WriteDigit;
            Write(0C);
            RETURN
         ELSE (*    xxxx.xxxx *)
            WriteDigit;
            INC(dignum);
            Write(".");
            WHILE (dp#0) AND (dignum < maxdignum) DO
               WriteDigit;
               INC(dignum);
               DEC(dp)
            END; (* while *)
            Write(0C);
            RETURN
         END (* if *)
      ELSIF (* negexp AND *) dp = 0 THEN (* 0 *)
         Write("0");
         Write(0C);
         RETURN
      ELSIF (* negexp AND *) dp < maxdignum THEN (* 0.xxxxxx *)
         Write("0"); Write(".");
         FOR i:=1 TO dp DO
            IF exp <= i THEN WriteDigit ELSE Write("0") END
         END; (* for *)
         dp := 0;
         Write(0C);
         RETURN
      ELSIF (* negexp AND dp>=maxdignum AND *) exp <= dp THEN (* 0.00xxxx000 *)
         dp := dp - exp +1;
         WHILE (dp # 0) AND (dignum < maxdignum) DO
            WriteDigit;
            INC(dignum);
            DEC(dp)
         END; (* while *)
         Write(0C);
         RETURN
      ELSE (* exp > dp *)
         Write("0");
         Write(0C);
         RETURN
      END (* if *)
   END WriteFix;

END RealConv. 
