(***************************************
*                                      *
*    Compiler specific storage         *
*    management:                       *
*                                      *
*    save dynamic storage contents     *
*    into a file and read this         *
*    structures at the beginning of    *
*    the next pass.                    *
*                                      *
***************************************)
IMPLEMENTATION MODULE Storage; (* AFB 3/84 *)

   (* DONT'T USE THE StdIO !!! *)

   FROM SYSTEM IMPORT WORD, ADDRESS, ADR, TSIZE;
   FROM SysBreak IMPORT Sbreak, Break;
   FROM SysOpen IMPORT Open;
   FROM SysClose IMPORT Close;
   FROM SysWrite IMPORT Write;
   FROM SysRead IMPORT Read;
   FROM SysPanic IMPORT Panic;

   (* save the global variables of MCBase *)
   FROM MCBase IMPORT boolptr (* first variable *), ismain (* last *);

   TYPE
      Header =
         RECORD
            base: ADDRESS; (* base of the expansion area *)
            top: ADDRESS;
         END;
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
      header: Header;
      StorageFile: ARRAY[0..31] OF CHAR;
      FreeList: FreePtr; (* circular ordered list *)
      base: FreeNode;
      ign: BOOLEAN;

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
      (* size in bytes *)
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
	 IF free = FreeList THEN
	    free := MoreCore(size);
	    IF free = NIL THEN
	       EXIT
	    END;
	 END;
         old := free;
	 free := free^.next;
      END; (* LOOP *)

      Panic("Storage: out of memory");
   END ALLOCATE;

   CONST adc = 4; (* address constant *)

   PROCEDURE InitStorage;
      TYPE
	 CardPtr = POINTER TO CARDINAL;
      VAR
         fd: CARDINAL; (* file descriptor *)
         bytecount: CARDINAL;
         GlobalSize: CARDINAL;
         oldBaseAdr: FreePtr;
         bytecount3: CARDINAL;
	 ptr: FreePtr;
   BEGIN
      IF ARGC() < 2 THEN
         Panic("Storage: first argument must be storage file");
      END;
      ARGV(StorageFile, 1);
      bytecount := TSIZE(Header);
      IF NOT Open(fd, StorageFile, (* read = *) 0) OR
         NOT Read(fd, ADR(header), bytecount) THEN
         Panic("Storage: cannot open/read storage file");
      END;
      IF CARDINAL(Sbreak(0)) > header.base THEN
         Panic("Storage: illegal address in storage file");
      END;
      IF header.top <> header.base THEN (* in m1, m2 and m3 *)
         GlobalSize := CARDINAL(ADR(ismain))-CARDINAL(ADR(boolptr))+adc;
         bytecount3 := TSIZE(FreePtr);
         IF NOT Read(fd, ADR(base), bytecount) OR
            NOT Read(fd, ADR(oldBaseAdr), bytecount3) OR
            NOT Read(fd, ADR(boolptr), GlobalSize) THEN
            Panic("Storage: read error on storage file");
         END;
         IF NOT Break(header.top) THEN
            Panic("Storage: cannot allocate contents of storage file");
         END;
         bytecount := CARDINAL(header.top)-CARDINAL(header.base);
         IF NOT Read(fd, header.base, bytecount) THEN
            Panic("Storage: read error on storage file");
         END;

         (* search the reference to the old base in the circular *)
         (* linked list *)

         ptr := base.next;
         WHILE ptr^.next <> oldBaseAdr DO
            ptr := ptr^.next;
         END;
         ptr^.next := ADR(base);
      ELSE
	 IF NOT Break(header.base) THEN
            Panic("Storage: illegal address in storage file");
	 END;
         base.next := FreeList;
         base.size := 0;
      END;
      IF NOT Close(fd) THEN
         Panic("Storage: cannot close storage file");
      END;
   END InitStorage;

   PROCEDURE EndStorage;
      VAR fd: CARDINAL; (* file descriptor *)
          bytecount: CARDINAL;
          bytecount2: CARDINAL;
          bytecount3: CARDINAL;
          GlobalSize: CARDINAL;
          bytecount5: CARDINAL;
          baseadr: ADDRESS;
   BEGIN
      header.top := Sbreak(0);
      bytecount := TSIZE(Header);
      bytecount2 := TSIZE(FreeNode);
      bytecount3 := CARDINAL(header.top) - CARDINAL(header.base);
      GlobalSize := CARDINAL(ADR(ismain))-CARDINAL(ADR(boolptr))+adc;
      bytecount5 := TSIZE(ADDRESS);
      baseadr := ADR(base);
      IF NOT Open(fd, StorageFile, (* write = *) 1) OR
         NOT Write(fd, ADR(header), bytecount) OR
         NOT Write(fd, ADR(base), bytecount2) OR
         NOT Write(fd, ADR(baseadr), bytecount5) OR
         NOT Write(fd, ADR(boolptr), GlobalSize) OR
         NOT Write(fd, header.base, bytecount3) THEN
         Panic("Storage: open/write error on storage file");
      END;
      IF NOT Close(fd) THEN
         Panic("Storage: cannot close storage file");
      END;
   END EndStorage;

   PROCEDURE Setmode(m: CARDINAL);
   BEGIN
   END Setmode;

BEGIN
   FreeList := ADR(base);
   base.next := FreeList;
   base.size := 0;
   InitStorage;
END Storage.
