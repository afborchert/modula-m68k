IMPLEMENTATION MODULE MCTypes; (* AFB 1/85 *)

   (* handling of type sets *)

   FROM MCBase IMPORT Stset, Structform, Stptr, maxcard, maxint,
      longmaxint, longmaxcard, realsize, longrealsize, oneword, longword,
      Constval;
   FROM Storage IMPORT ALLOCATE;

   TYPE
      TypeSetList = POINTER TO TypeSetNode;
      TypeSetNode =
         RECORD
            typtr: Stptr;
            link: TypeSetList;
         END;
   VAR
      typesetlist: TypeSetList;

   PROCEDURE IsReal(sp: Stptr) : BOOLEAN;
   BEGIN
      IF sp = NIL THEN RETURN FALSE END;
      WITH sp^ DO
         RETURN (form = reals) OR (form = longreals) OR
                (form = setoftypes) AND
                (Stset{reals, longreals} * typeset <> Stset{});
      END;
   END IsReal;

   PROCEDURE IsCard(sp: Stptr) : BOOLEAN;
   BEGIN
      IF sp = NIL THEN RETURN FALSE END;
      WITH sp^ DO
         RETURN (form = cards) OR (form = longcards) OR
                (form = setoftypes) AND
                (Stset{cards, longcards} * typeset <> Stset{});
      END;
   END IsCard;

   PROCEDURE IsInt(sp: Stptr) : BOOLEAN;
   BEGIN
      IF sp = NIL THEN RETURN FALSE END;
      WITH sp^ DO
         RETURN (form = ints) OR (form = longints) OR
                (form = setoftypes) AND
                (Stset{ints, longints} * typeset <> Stset{});
      END;
   END IsInt;

   PROCEDURE IsLong(sp: Stptr) : BOOLEAN;
      CONST
         Longs = Stset{longints, longcards, longreals};
   BEGIN
      IF sp = NIL THEN RETURN FALSE END;
      WITH sp^ DO
         RETURN (form IN Longs) OR
                (form = setoftypes) AND
                (typeset <= Longs);
      END;
   END IsLong;

   PROCEDURE ConstType(const: Constval; VAR type: Stptr);
      VAR set: Stset;
   BEGIN
      set := Stset{longcards};
      WITH const DO
         IF value <= longmaxint THEN
            INCL(set, longints);
         END;
         IF value <= maxcard THEN
            INCL(set, cards);
            IF value <= maxint THEN
               INCL(set, ints);
            END;
         END;
      END;
      type := GetType(set);
   END ConstType;

   PROCEDURE GetType(set: Stset) : Stptr;
      VAR ptr: TypeSetList;
   BEGIN
      ptr := typesetlist;
      WHILE ptr <> NIL DO
         WITH ptr^ DO
            IF typtr^.typeset = set THEN
               RETURN typtr;
            END;
            ptr := link;
         END;
      END;
      NEW(ptr);
      WITH ptr^ DO
         NEW(typtr);
         WITH typtr^ DO
            IF reals IN set THEN
               size := realsize;
            ELSIF longreals IN set THEN
               size := longrealsize;
            ELSIF Stset{cards, ints} * set <> Stset{} THEN
               size := oneword;
            ELSE
               size := longword;
            END;
            stidp := NIL;
            inlist := TRUE;
            form := setoftypes;
            typeset := set;
         END;
         link := typesetlist;
      END;
      typesetlist := ptr;
      RETURN typesetlist^.typtr;
   END GetType;

   PROCEDURE TypeSetCompatible(sp1, sp2: Stptr) : BOOLEAN;
   BEGIN
      RETURN TypeSetResult(sp1, sp2) <> NIL;
   END TypeSetCompatible;

   PROCEDURE TypeSetResult(sp1, sp2: Stptr) : Stptr;
      VAR set1, set2: Stset; result: Stset;

      PROCEDURE TypeSet(sp: Stptr) : Stset;
      BEGIN
         IF sp = NIL THEN RETURN Stset{} END;
         WITH sp^ DO
            IF form = setoftypes THEN
               RETURN typeset
            ELSE
               RETURN Stset{form}
            END;
         END;
      END TypeSet;

   BEGIN
      set1 := TypeSet(sp1); set2 := TypeSet(sp2);
      result := set1 * set2;
      IF result = Stset{} THEN
         RETURN NIL
      ELSE
         RETURN GetType(result);
      END;
   END TypeSetResult;

   PROCEDURE ResultOfNegation(sp: Stptr) : Stptr;
      CONST NotCard = Stset{ints, longints};
   BEGIN
      IF NOT IsReal(sp) THEN
         RETURN TypeSetResult(GetType(NotCard), sp)
      ELSE
         RETURN sp
      END;
   END ResultOfNegation;

BEGIN
   typesetlist := NIL;
END MCTypes.
