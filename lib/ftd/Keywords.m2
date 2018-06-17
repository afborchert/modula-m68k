IMPLEMENTATION MODULE Keywords;

   CONST 
      HTMAX   = 359;
      AVWORDSZ= 15;             (* assumed average word size of stored names *)
      IDMAX   = (HTMAX+1) * (AVWORDSZ + 1);

   TYPE 
      HashEntry = 
         RECORD 
            spix : INTEGER;     (* spelling indes; negative meeans free slot *)
            info : INTEGER;
         END;

   CONST 
      FREE = -1;                        (* to mark free slots in hashtab *)

   VAR 
      hashtab : ARRAY[0..HTMAX-1] OF HashEntry;
      names   : ARRAY[0..IDMAX-1] OF CHAR;
      nix     : CARDINAL;               (* index into names *)
      i       : CARDINAL;               (* auxilary         *)

   PROCEDURE DefineKey (name : ARRAY OF CHAR; key: INTEGER);

      VAR 
         hval : CARDINAL;

   BEGIN 
      hval := Hash(name);
      IF hval >= HTMAX THEN
	 ok := FALSE;
	 RETURN;
      END;
      WITH hashtab[hval] DO 
         info := key;
         IF spix = FREE THEN            (*newentry*)
            spix := Enter(name);
	    IF spix >= IDMAX THEN
	       ok := FALSE;
	       spix := FREE;
	    END;
         END;
      END;
   END DefineKey;

   PROCEDURE IsKey (string: ARRAY OF CHAR; VAR key: INTEGER) : BOOLEAN;

      VAR 
         hval : CARDINAL;

   BEGIN 
      hval := Hash(string);
      IF hval >= HTMAX THEN
	 RETURN FALSE;
      END;
      WITH hashtab[hval] DO 
         IF spix = FREE THEN 
            RETURN FALSE 
         ELSE 
            key := info;
            RETURN TRUE;
         END;
      END;
   END IsKey;

   PROCEDURE Enter(name : ARRAY OF CHAR) : CARDINAL ;

      VAR 
         val, i : CARDINAL;

   BEGIN 
      IF nix >= IDMAX THEN
	 RETURN IDMAX;
      END;
      i := 0;
      val := nix;
      WHILE (i<=HIGH(name)) AND (name[i] # 0C) AND (nix + i < IDMAX-1) DO 
         names[nix+i] := name[i];
         INC(i);
      END;
      names[nix+i] := 0C;
      nix := nix+i+1;
      RETURN val ;
   END Enter;

   PROCEDURE Equal(spix : CARDINAL; name : ARRAY OF CHAR) : BOOLEAN;

      VAR 
         i : CARDINAL;

   BEGIN 
      i := 0;
      LOOP 
         IF (i > HIGH(name)) THEN 
            IF (names[spix+i] # 0C) THEN 
               RETURN FALSE;
            ELSE 
               RETURN TRUE;
            END;
         END;
         IF name[i] # names[spix+i] THEN 
            RETURN FALSE;
         END;
         IF (name[i] = 0C) AND (names[spix+i] = 0C) THEN 
            RETURN TRUE 
         END;
         INC(i);
      END;
   END Equal;

   PROCEDURE Hash(name : ARRAY OF CHAR) : CARDINAL;

      VAR 
         val : CARDINAL;
         i   : CARDINAL;
         base: CARDINAL;

   BEGIN 
      i := 0;
      val := 0;
      WHILE (i<= HIGH(name)) AND (name[i] # 0C) DO 
         val := (val * 24 + ORD(name[i]) - ORD('A')) MOD HTMAX;
         INC(i);
      END;
      base := 1;
      WHILE (hashtab[val].spix # FREE) AND (NOT Equal(hashtab[val].spix,name))
         DO 
	 IF base > HTMAX THEN (* overflow check *)
	    RETURN HTMAX;
	 END;
         val :=( val + base*base) MOD HTMAX;
         INC(base);
      END;
      RETURN val;
   END Hash;

BEGIN 
   nix := 0;
   ok := TRUE;
   FOR i := 0 TO HTMAX-1 DO 
      hashtab[i].spix := FREE;
   END;
END Keywords. 
