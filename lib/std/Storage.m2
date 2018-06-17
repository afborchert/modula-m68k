IMPLEMENTATION MODULE Storage;

   FROM SYSTEM IMPORT WORD, ADDRESS, ADR, TSIZE;
   FROM SysBreak IMPORT Sbreak;
   FROM SysPanic IMPORT Panic;

   (* see "The C Programming Language", Page 173 *)

   TYPE
      FreePtr = POINTER TO FreeNode;
      FreeNode =
         RECORD
            size: CARDINAL; (* in units *)
	    CASE : BOOLEAN OF
              TRUE:
		 next: FreePtr;
	    | FALSE:
		 cnext: CARDINAL;
            END;
         END;
   VAR
      FreeList: FreePtr; (* circular ordered list *)
      base: FreeNode;
      ign: BOOLEAN;
      Mode: (returnNIL, abort);

   (* ask system for memory *)

   PROCEDURE MoreCore(nu: CARDINAL) : FreePtr;
      CONST Nalloc = 128; (* #units to allocate at once *)
      VAR rnu: CARDINAL; (* rounded number of units *)
	  adr: ADDRESS;
	  fp: FreePtr;
   BEGIN
      rnu := Nalloc * ((nu+Nalloc-1) DIV Nalloc);
      adr := Sbreak(stob(rnu));
      IF adr = NIL THEN
	 adr := Sbreak(stob(nu));
	 IF adr = NIL THEN
	    RETURN NIL;
	 ELSE
	    rnu := nu;
	 END;
      END;
      fp := FreePtr(adr);
      DEALLOCATE(fp, (* size in bytes = *) rnu * TSIZE(FreeNode));
      RETURN FreeList;
   END MoreCore;

   (* bytes to units *)

   PROCEDURE btou(nb: CARDINAL) : CARDINAL;
   BEGIN
      nb := TSIZE(FreeNode) * ((nb+TSIZE(FreeNode)-1) DIV TSIZE(FreeNode));
      RETURN nb DIV TSIZE(FreeNode);
   END btou;

   (* size to bytecount *)

   PROCEDURE stob(size: CARDINAL) : CARDINAL;
   BEGIN
      RETURN size * TSIZE(FreeNode);
   END stob;

   PROCEDURE DEALLOCATE(VAR ptr: ADDRESS; size: CARDINAL);
      (* size in words *)
      VAR free: FreePtr;
	  cfree: CARDINAL;
	  cptr: CARDINAL;
	  fptr: FreePtr;
   BEGIN
      size := btou(size); (* now size in units *)

      cptr := CARDINAL(ptr);
      free := FreeList;
      LOOP
	 cfree := CARDINAL(free);
	 IF (cptr > cfree) AND (cptr < free^.cnext) THEN
	    EXIT
	 END;
	 IF (cfree >= free^.cnext) AND
	    ((cptr > cfree) OR (cptr < free^.cnext)) THEN
	    EXIT (* at one end or other *)
	 END;
	 free := free^.next;
      END; (* LOOP *)

      fptr := FreePtr(ptr);
      fptr^.size := size;
      IF cptr + stob(fptr^.size) = free^.cnext THEN (* join to upper nbr *)
	 fptr^.size := size + free^.next^.size;
	 fptr^.next := free^.next^.next;
      ELSE
	 fptr^.next := free^.next;
      END;
      IF cfree + stob(free^.size) = cptr THEN (* join to lower nbr *)
	 free^.size := free^.size + fptr^.size;
	 free^.next := fptr^.next;
      ELSE
	 free^.next := fptr;
      END;
      FreeList := free;

      ptr := NIL;
   END DEALLOCATE;

   PROCEDURE ALLOCATE(VAR ptr: ADDRESS; size: CARDINAL);
      VAR free: FreePtr;
          dummy: ADDRESS;
          old: FreePtr;
   BEGIN
      size := btou(size); (* now size in units *)
      old := FreeList;
      free := FreeList^.next;
      LOOP
         IF free^.size >= size THEN

	    (* free block found *)

	    ptr := free;
            IF free^.size > size THEN
	       free^.size := free^.size - size;
	       INC(ptr, stob(free^.size));
	    ELSE
               IF old^.next = FreeList THEN
                  FreeList := old;
               END;
     	       old^.next := free^.next;
	    END;
            RETURN;
         END;
         old := free;
	 free := free^.next;
	 IF free = FreeList^.next THEN
	    free := MoreCore(size);
	    IF free = NIL THEN
	       EXIT
	    END;
	 END;
      END; (* LOOP *)

      IF Mode = returnNIL THEN
         ptr := NIL;
      ELSE
         Panic("No space available.");
      END;
   END ALLOCATE;

   PROCEDURE Setmode(m: CARDINAL);
   BEGIN
      CASE m OF
        1: Mode := abort;
      | 2: Mode := returnNIL;
      ELSE
         (* nothing *)
      END;
   END Setmode;

BEGIN
   FreeList := ADR(base);
   base.next := FreeList;
   base.size := 0;
   Mode := abort;
END Storage.
