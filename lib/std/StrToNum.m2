IMPLEMENTATION MODULE StrToNum; (* mh 5/85; rev afb 4/86: StrToOct/Hex *)

   FROM ASCII IMPORT tab, nl;

   PROCEDURE CharToDigit(c: CHAR; base: CARDINAL; VAR d: CARDINAL): BOOLEAN;

   BEGIN
      IF (base = 16) AND ('A' <= CAP(c)) AND (CAP(c) <= 'F') THEN
	 d := ORD(CAP(c)) - ORD('A') + 10;
      ELSIF ('0' <= c) AND (c < CHR(base+ORD('0'))) THEN
	 d := ORD(c) - ORD('0');
	 RETURN TRUE
      ELSE
	 RETURN FALSE
      END
   END CharToDigit;

   PROCEDURE SearchBegin(string: ARRAY OF CHAR; VAR pos: CARDINAL);

      PROCEDURE ToIgnore(char: CHAR): BOOLEAN;

      BEGIN
	 RETURN (char = ' ') OR (char = tab) OR (char = nl)
      END ToIgnore;

   BEGIN
      pos := 0;
      WHILE (pos <= HIGH(string)) AND ToIgnore(string[pos]) DO
	 INC(pos)
      END
   END SearchBegin;

   PROCEDURE StrToCardinal(str: ARRAY OF CHAR; base: CARDINAL;
		       VAR card: CARDINAL): BOOLEAN;

      VAR
	 Counter:  CARDINAL;
	 HelpCard: CARDINAL;

   BEGIN
      card := 0;
      SearchBegin(str,Counter);
      IF str[Counter] = '+' THEN
	 INC(Counter)
      ELSIF str[Counter] = 0C THEN
	 RETURN FALSE
      END;

      WHILE (Counter <= HIGH(str)) AND (str[Counter] > " ") DO
	 IF CharToDigit(str[Counter], base, HelpCard) THEN
	    HelpCard := HelpCard + base * card;
	    IF HelpCard < card THEN
	       RETURN FALSE			(* overflow *)
	    ELSE
	       card := HelpCard
	    END
	 ELSE
	    RETURN FALSE			(* syntax-error *)
	 END;
	 INC(Counter)
      END; (* WHILE *)
      RETURN TRUE
   END StrToCardinal;

   PROCEDURE StrToInt(str: ARRAY OF CHAR; VAR integ: INTEGER): BOOLEAN;

      VAR
	 Counter,Digit: CARDINAL;
	 HelpInt, Sign:	INTEGER;

   BEGIN
      integ := 0; Sign := 1;
      SearchBegin(str,Counter);
      IF str[Counter] = '-' THEN
	 INC(Counter); Sign := -Sign
      ELSIF str[Counter] = '+' THEN
	 INC(Counter)
      ELSIF str[Counter] = 0C THEN
	 RETURN FALSE
      END;

      WHILE (Counter <= HIGH(str)) AND (str[Counter] > " ") DO
	 IF CharToDigit(str[Counter], 10, Digit) THEN
	    HelpInt := Sign * INTEGER(Digit) + 10 * integ;
	    IF (Sign < 0) AND (HelpInt > integ) OR
		  (Sign > 0) AND (HelpInt < integ) THEN
	       RETURN FALSE			(* overflow *)
	    ELSE
	       integ := HelpInt
	    END
	 ELSE
	    RETURN FALSE			(* syntax-error *)
	 END;
	 INC(Counter)
      END; (* WHILE *)
      RETURN TRUE
   END StrToInt;

   PROCEDURE StrToCard(str: ARRAY OF CHAR; VAR card: CARDINAL): BOOLEAN;
   BEGIN
      RETURN StrToCardinal(str, 10, card);
   END StrToCard;

   PROCEDURE StrToOct(str: ARRAY OF CHAR; VAR card: CARDINAL) : BOOLEAN;
   BEGIN
      RETURN StrToCardinal(str, 10B, card);
   END StrToOct;

   PROCEDURE StrToHex(str: ARRAY OF CHAR; VAR card: CARDINAL) : BOOLEAN;
   BEGIN
      RETURN StrToCardinal(str, 10H, card);
   END StrToHex;

END StrToNum.
