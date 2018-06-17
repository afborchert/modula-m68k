IMPLEMENTATION MODULE Strings; (* AFB 7/84 *)

   PROCEDURE StrLen(s: ARRAY OF CHAR) : CARDINAL;
      VAR len: CARDINAL;
   BEGIN
      len := 0;
      WHILE (len <= HIGH(s)) AND (s[len] <> 0C) DO
         INC(len);
      END;
      RETURN len;
   END StrLen;

   PROCEDURE StrCat(VAR s: ARRAY OF CHAR; s1: ARRAY OF CHAR);
      VAR index1, index2: CARDINAL;
   BEGIN
      index1 := StrLen(s);
      index2 := 0;
      WHILE (index1 <= HIGH(s)) AND (index2 <= HIGH(s1)) AND
            (s1[index2] <> 0C) DO
         s[index1] := s1[index2];
         INC(index1);
         INC(index2);
      END;
      IF index1 <= HIGH(s) THEN
         s[index1] := 0C;
      END;
   END StrCat;

   PROCEDURE StrCmp(a, b: ARRAY OF CHAR) : INTEGER;
      VAR index: CARDINAL;
          min: CARDINAL;
   BEGIN
      IF HIGH(a) < HIGH(b) THEN min := HIGH(a) ELSE min := HIGH(b) END;
      FOR index := 0 TO min DO
         IF a[index] <> b[index] THEN
            RETURN ORD(a[index]) - ORD(b[index]);
         ELSIF a[index] = 0C THEN
            RETURN 0;
         END;
      END;
      IF HIGH(a) = HIGH(b) THEN
         RETURN 0
      ELSIF (HIGH(a) > min) AND (a[min+1] <> 0C) THEN
         RETURN 1;
      ELSIF (HIGH(b) > min) AND (b[min+1] <> 0C) THEN
         RETURN -1;
      ELSE
         RETURN 0;
      END;
   END StrCmp;

   PROCEDURE StrCpy(VAR s: ARRAY OF CHAR; s1: ARRAY OF CHAR);
      VAR index: CARDINAL;
          min: CARDINAL;
   BEGIN
      index := 0;
      IF HIGH(s) < HIGH(s1) THEN min := HIGH(s) ELSE min := HIGH(s1) END;
      FOR index := 0 TO min DO
         s[index] := s1[index];
         IF s[index] = 0C THEN
            RETURN;
         END;
      END;
      index := min+1;
      IF index <= HIGH(s) THEN
         s[index] := 0C;
      END;
   END StrCpy;

END Strings.
