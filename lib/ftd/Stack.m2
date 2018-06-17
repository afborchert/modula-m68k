IMPLEMENTATION MODULE Stack;

   FROM SYSTEM IMPORT BYTE, ADDRESS, ADR, WORD;
   FROM Storage IMPORT ALLOCATE, DEALLOCATE;
   FROM Bytes IMPORT ByteNCopy;
   
   TYPE 
      StackType = RECORD
	 maxno : CARDINAL; 
	 top   : CARDINAL;
	 elemsz: CARDINAL;
	 align : CARDINAL;
	 add   : ADDRESS;
      END;
      Stack = POINTER TO StackType;
	 
   PROCEDURE Create(VAR s : Stack; noelem : CARDINAL; elem : ARRAY OF BYTE);

   BEGIN
      NEW(s);
      WITH s^ DO
	 top := 0;
	 maxno := noelem;
	 elemsz := HIGH(elem)+1;
	 align := elemsz DIV SIZE(WORD) * SIZE(WORD);
	 IF align # elemsz THEN
	    INC(align,SIZE(WORD));
	 END;
	 ALLOCATE(add,align*maxno);
      END;
   END Create;

   PROCEDURE Dispose (VAR s : Stack);

   BEGIN
      IF s # NIL THEN
	 WITH s^ DO
	    DEALLOCATE(add,align*maxno);
	 END;
	 DISPOSE(s);
	 s := NIL;
      END;
   END Dispose;

   PROCEDURE Clear(s : Stack);

   BEGIN
      IF s # NIL THEN
	 s^.top := 0;
      END;
   END Clear;

   PROCEDURE Elems(s : Stack) : CARDINAL;

   BEGIN
      IF s # NIL THEN
	 RETURN s^.top;
      ELSE
	 RETURN 0;
      END;
   END Elems;

   PROCEDURE Push(s : Stack; elem : ARRAY OF BYTE) : BOOLEAN;

   BEGIN
      IF (s = NIL) OR (HIGH(elem)+1 # s^.elemsz) OR (s^.top = s^.maxno) THEN
	 RETURN FALSE;
      ELSE
	 WITH s^ DO
	    ByteNCopy(add+top*align,ADR(elem),elemsz);
	    INC(top);
	 END;
	 RETURN TRUE;
      END;
   END Push;

   PROCEDURE Pop(s : Stack; VAR elem : ARRAY OF BYTE) : BOOLEAN;

   BEGIN
      IF (s # NIL) AND (HIGH(elem)+1 = s^.elemsz) AND Top(s,0,elem) THEN
	 DEC(s^.top);
	 RETURN TRUE;
      ELSE
	 RETURN FALSE;
      END;
   END Pop;

   PROCEDURE Adr(s : Stack; pos : CARDINAL; VAR a : ADDRESS) : BOOLEAN;

   BEGIN
      IF (s = NIL) OR (s^.top <= pos) THEN
	 a := NIL;
	 RETURN FALSE;
      ELSE
	 WITH s^ DO
	    a := add +(top-1-pos)*align;
	 END;
	 RETURN TRUE;
      END;
   END Adr;

   PROCEDURE Top(s : Stack; pos : CARDINAL; VAR elem : ARRAY OF BYTE) : BOOLEAN;

   VAR
      a : ADDRESS;

   BEGIN
      IF (s # NIL) AND (s^.elemsz = HIGH(elem)+1) AND Adr(s,pos,a) THEN
	 ByteNCopy(ADR(elem),a,s^.elemsz);
	 RETURN TRUE;
      ELSE
         RETURN FALSE;
      END;
   END Top;

END Stack.
