IMPLEMENTATION MODULE Chars;

   PROCEDURE Lower (VAR ch : CHAR);

   BEGIN
      IF ch IN UpperS THEN
	 ch := CHR(ORD(ch) + ORD('a') - ORD('A'));
      END;
   END Lower;

   PROCEDURE Upper (VAR ch : CHAR);

   BEGIN
      IF ch IN LowerS THEN
	 ch := CHR(ORD(ch) + ORD('A') - ORD('a'));
      END;
   END Upper;

   PROCEDURE Class(ch : CHAR) : CharClass;

   BEGIN 
      CASE ch OF 
      | 'a'..'z','A'..'Z': 
            RETURN letter;
      | '0' ..'9' : 
            RETURN digit;
      | nul : 
            RETURN nullc;
      ELSE 
         IF ch IN PunctS THEN 
            RETURN punct 
         ELSIF ch IN SpaceS THEN 
            RETURN space;
	 ELSIF ch IN ControlS THEN
	    RETURN control
         ELSE 
            RETURN nonascii 
         END;
      END;

   END Class;

END Chars. 
