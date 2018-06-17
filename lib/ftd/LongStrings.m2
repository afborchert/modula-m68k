(*
 *    LongStrings- Allocate and handle strings of variable length (ws 6/88)
 *    ======================================================================
 *
 *)

IMPLEMENTATION MODULE LongStrings;

   FROM SYSTEM IMPORT ADDRESS, ADR;
   FROM StdIO IMPORT FILE, Fwrite;
   FROM Storage IMPORT ALLOCATE, DEALLOCATE;
   FROM Chars IMPORT nul;
   FROM Bytes IMPORT ByteNCopy, PINC, PDEC;
   FROM Strings IMPORT StrLen;
   FROM Stack IMPORT Stack, Push, Pop, Create, Elems;

   CONST 
      BlockSize = 512;

   TYPE 
      Long            = POINTER TO LongDescription;
      LongDescription = 
         RECORD 
            address : ADDRESS;
            next : ADDRESS;
            avail : CARDINAL;
            used : CARDINAL;
            valid : BOOLEAN;
         END;

      EchoDesc        = 
         RECORD 
            used : BOOLEAN;
            origin : FILE;
            echo : FILE;
         END;

   CONST 
      MaxEcho   = 10;
      StackSize = 30;
      MaxBytes  = 65536;

   VAR 
      AllEcho : ARRAY[0..MaxEcho-1] OF EchoDesc;
      stored  : CARDINAL;
      stack   : Stack;
      noused  : CARDINAL;
      xxx     : Long;

   PROCEDURE Alloc(VAR l : Long);

   BEGIN 
      IF NOT Pop(stack,l) OR NOT ValidLong(l) THEN
         create(l);
      ELSE
	 DEC(stored,l^.avail);
      END;
      ClearLong(l);
   END Alloc;

   PROCEDURE Free(VAR l : Long);

   BEGIN 
      IF NOT ValidLong(l) THEN
	 RETURN;
      END;
      IF ((l^.avail+stored) > MaxBytes) OR NOT Push(stack,l) THEN 
         Dispose(l);
      ELSE 
	 INC(stored,l^.avail);
	 l := NIL;
      END;
   END Free;

   PROCEDURE ValidLong(long : Long) : BOOLEAN;

   BEGIN 
      RETURN (long # NIL) AND (long^.valid);
   END ValidLong;

   PROCEDURE create (VAR str : Long);

      VAR 
         cp : POINTER TO CHAR;

   BEGIN 
      NEW(str);
      IF str = NIL THEN 
         RETURN 
      ELSE 
         WITH str^ DO 
            ALLOCATE(address,BlockSize);
            IF address = NIL THEN 
               DISPOSE(str);
               str := NIL;
               RETURN;
            END;
            valid := TRUE;
            next := address;
            avail := BlockSize;
            used := 0;
            cp := next;
            cp^ := nul;
         END;
      END;
   END create;

   PROCEDURE ClearLong (str : Long);

      VAR 
         cp : POINTER TO CHAR;
   BEGIN 
      IF NOT ValidLong(str) THEN 
         RETURN 
      END;
      WITH str^ DO 
         IF avail > 0 THEN 
            used := 0;
            next := address;
            cp := next;
            cp^ := nul;
         ELSE 
            valid := FALSE;
         END;
      END;
   END ClearLong;

   PROCEDURE GetChar (long : Long; index : INTEGER) : CHAR;

      VAR 
         idx : CARDINAL;
         cp  : POINTER TO CHAR;

   BEGIN 
      IF NOT ValidLong(long) OR (index < LastChar) THEN 
         RETURN nul;
      ELSIF index = LastChar THEN 
         idx := long^.used-1;
      ELSE 
         idx := VAL(CARDINAL,index);
      END;
      IF idx > long^.used-1 THEN 
         RETURN nul;
      ELSE 
         cp := long^.address;
         PINC(cp,idx);
         RETURN cp^;
      END;
   END GetChar;

   PROCEDURE CountChar(long : Long; char : CHAR) : CARDINAL;

      VAR 
         cp : POINTER TO CHAR;
         res: CARDINAL;
         add: ADDRESS;

   BEGIN 
      IF NOT ValidLong(long) OR (long^.used = 0) THEN 
         RETURN 0;
      ELSE 
         WITH long^ DO 
            res := 0;
            FOR add := address TO address+used-1 DO 
               cp := add;
               IF cp^ = char THEN 
                  INC(res);
               END;
            END;
         END;
         RETURN res;
      END;
   END CountChar;

   PROCEDURE Dispose (VAR str : Long);

   BEGIN 
      IF str = NIL THEN 
         RETURN 
      END;
      IF str^.avail > 0 THEN 
         DEALLOCATE(str^.address, str^.avail);
      END;
      DISPOSE(str);
      str := NIL;
   END Dispose;

   PROCEDURE Align(VAR need : CARDINAL);

   BEGIN 
      IF need MOD BlockSize # 0 THEN 
         need := (need DIV BlockSize + 1) * BlockSize;
      END;
   END Align;

   PROCEDURE Operate(str : Long; more : CARDINAL; add : ADDRESS);

      VAR 
         need : CARDINAL;
         new  : ADDRESS;
         cp   : POINTER TO CHAR;

   BEGIN 
      IF NOT ValidLong(str) OR (more = 0) THEN 
         RETURN 
      END;
      WITH str^ DO 
         need := used + more + 1;
         IF need > avail THEN 
            Align(need);
            ALLOCATE(new,need);
            IF new = NIL THEN 
               valid := FALSE;
               RETURN 
            END;
            IF used > 0 THEN 
               ByteNCopy(new,address,used);
            END;
            IF avail > 0 THEN 
               DEALLOCATE(address,avail);
            END;
            avail := need;
            address := new;
            next := address;
            PINC(next,used);
         END;
         ByteNCopy(next,add,more);
         PINC(next,more);
         INC(used,more);
         cp := next;
         cp^ := nul;
      END;
   END Operate;

   PROCEDURE AddString(str : Long; text : ARRAY OF CHAR);

   BEGIN 
      Operate(str,StrLen(text),ADR(text));
   END AddString;

   PROCEDURE AddChar(str : Long; char : CHAR);

   BEGIN 
      Operate(str,SIZE(CHAR),ADR(char));
   END AddChar;

   PROCEDURE AddBytes(str : Long; add : ADDRESS; n : CARDINAL);

   BEGIN 
      Operate(str,n,add);
   END AddBytes;

   PROCEDURE CutLong(str : Long; n : CARDINAL);

   BEGIN 
      IF ValidLong(str) AND (n < str^.used) THEN 
         WITH str^ DO 
            used := n;
            next := address;
            PINC(next,n);
         END;
         AddChar(str,nul);
      END;
   END CutLong;

   PROCEDURE StrAdr(str : Long) : ADDRESS;

   BEGIN 
      IF ValidLong(str) THEN 
         RETURN str^.address;
      ELSE 
         RETURN NIL;
      END;
   END StrAdr;

   PROCEDURE StrSize(str : Long) : CARDINAL;

   BEGIN 
      IF NOT ValidLong(str) THEN 
         RETURN 0 ;
      ELSE 
         RETURN str^.used;
      END;
   END StrSize;

   PROCEDURE Write(address : ADDRESS; bytes : CARDINAL; file : FILE) : BOOLEAN
      ;

      VAR 
         res   : BOOLEAN;
         i     : CARDINAL;
         nitem : CARDINAL;

   BEGIN 
      nitem := 1;
      res := Fwrite(address,bytes,nitem,file) AND (nitem=1);
      IF res AND (noused > 0) THEN 
         i := 0;
         WHILE res AND (i <= MaxEcho-1) DO 
            WITH AllEcho[i] DO 
               IF used AND (origin=file) THEN 
                  res := res AND Fwrite(address,bytes,nitem,echo) AND (nitem 
                     = 1);
               END;
            END;
            INC(i);
         END;
      END;
      RETURN res;
   END Write;

   PROCEDURE Lwrite(str : Long; file : FILE) : BOOLEAN;

   BEGIN 
      RETURN ValidLong(str) AND (str^.used > 0) AND Write(str^.address,str^.
         used,file);
   END Lwrite;

   PROCEDURE FindChar(long : Long; char : CHAR; offset, count : INTEGER)
      : INTEGER;

      VAR 
         cp      : POINTER TO CHAR;
         inc     : PROCEDURE (VAR ADDRESS, CARDINAL);
         add     : INTEGER;
         maxloop : INTEGER;

   BEGIN 
      IF NOT ValidLong(long) OR (count = 0) OR (offset < LastChar) OR (offset 
         >= VAL (INTEGER,long^.used)) THEN 
         RETURN NotFound;
      ELSIF count < 0 THEN 
         inc := PDEC;
         add := -1;
         count := -count;
      ELSE 
         inc := PINC;
         add := 1;
      END;
      maxloop := long^.used;
      IF offset = LastChar THEN 
         offset := VAL(INTEGER,long^.used) - 1;
      ELSE 
         DEC(maxloop,VAL(CARDINAL,offset));
      END;
      cp := long^.address;
      PINC(cp,VAL(CARDINAL,offset));
      LOOP 
         DEC(maxloop);
         IF cp^ = char THEN 
            DEC(count);
         END;
         IF (maxloop = 0) OR (count = 0) THEN 
            EXIT;
         ELSE 
            inc(cp,1);
            offset := offset + add;
         END;
      END;
      IF (count = 0) THEN 
         RETURN offset;
      ELSE 
         RETURN NotFound;
      END;
   END FindChar;

   PROCEDURE LwritePart(long : Long; from, to : INTEGER; file : FILE)
      : BOOLEAN;

      VAR 
         f,t : CARDINAL;

   BEGIN 
      IF NOT ValidLong(long) OR (from < LastChar) OR (to < LastChar) THEN 
         RETURN FALSE;
      END;
      IF from = LastChar THEN 
         f := long^.used-1;
      ELSE 
         f := VAL(CARDINAL,from);
      END;
      IF to = LastChar THEN 
         t := long^.used-1;
      ELSE 
         t := VAL(CARDINAL,to);
      END;
      RETURN (f <= t) AND (t < long^.used) AND Write (long^.address+f,t-f+1,
         file)
   END LwritePart;

   PROCEDURE Echo (from,to : FILE) : BOOLEAN;

      VAR 
         i : CARDINAL;
   BEGIN 
      IF noused>=MaxEcho THEN 
         RETURN FALSE;
      ELSE 
         i := 0;
         LOOP 
            WITH AllEcho[i] DO 
               IF used THEN 
                  INC(i);
               ELSE 
                  origin := from;
                  echo := to;
                  used := TRUE;
                  INC(noused);
                  RETURN TRUE;
               END;
            END;
         END;
      END;
   END Echo;

   PROCEDURE NoEcho(from,to : FILE) : BOOLEAN;

      VAR 
         old,i : CARDINAL;
   BEGIN 
      old := noused;
      IF noused = 0 THEN 
         RETURN FALSE;
      ELSE 
         FOR i := 0 TO MaxEcho-1 DO 
            WITH AllEcho[i] DO 
               IF used AND (from = origin) AND (to = echo) THEN 
                  used := FALSE;
                  DEC(noused);
               END;
            END;
         END;
      END;
      RETURN old # noused;
   END NoEcho;

BEGIN 
   FOR noused := 0 TO MaxEcho-1 DO 
      AllEcho[noused].used := FALSE;
   END;
   noused := 0;
   stored := 0;
   Create(stack,StackSize,xxx);
END LongStrings. 
