IMPLEMENTATION MODULE MCBigSet; (* AFB 1/85 *)

   FROM MCBase IMPORT Constval, SetValuePtr, Stptr, maxcard, oneword,
      BitsPerWord, Symbol, bigsetroot, boolptr;
   FROM Storage IMPORT ALLOCATE, DEALLOCATE;
   IMPORT MCBase;

   TYPE BigSetPtr = POINTER TO ARRAY [0..(maxcard-oneword) DIV oneword] OF BITSET;

   PROCEDURE InitConstSet(VAR c: Constval; sp: Stptr);
      (* create new SetValue field *)
      VAR new: SetValuePtr; index: CARDINAL; setptr: BigSetPtr;
   BEGIN
      NEW(new);
      WITH new^ DO
         label := NIL;
         ALLOCATE(valentry, sp^.size);
         size := sp^.size DIV oneword;
         offset := sp^.offset;
         setptr := valentry;
         FOR index := 0 TO size-1 DO
            setptr^[index] := {};
         END;
         slink := NIL;
      END;
      c.setvalue := new;
   END InitConstSet;

   PROCEDURE ConstSetElement(c: Constval; from, to: CARDINAL);
      (* add constant set element range to c *)
      VAR index: CARDINAL; setptr: BigSetPtr; offs: CARDINAL;
   BEGIN
      WITH c.setvalue^ DO
         setptr := valentry;
         offs := offset;
      END;
      FOR index := from-offs TO to-offs DO
         INCL(setptr^[index DIV BitsPerWord], index MOD BitsPerWord);
      END;
   END ConstSetElement;

   PROCEDURE TermConstSet(VAR c: Constval);
      (* link SetValue into chain of set constants *)
   BEGIN
      (* uniqe check possible; but if c.setvalue^.label <> NIL *)
      (* c MUST be linked into chain                           *)
      WITH c DO
         setvalue^.slink := bigsetroot;
         bigsetroot := setvalue;
      END;
   END TermConstSet;

   PROCEDURE BigSetOp(c1, c2: Constval; VAR res: Constval; op: Symbol;
                      VAR tp: Stptr; VAR err: BOOLEAN);
      VAR index: CARDINAL; result, left, right: BigSetPtr;
   BEGIN
      err := FALSE;
      IF op = insy THEN
         tp := boolptr;
         WITH c2.setvalue^ DO
            right := valentry;
            index := c1.value - offset;
            res.value := ORD((index MOD BitsPerWord) IN
               right^[index DIV BitsPerWord]);
         END;
      ELSE
         IF (op = eql) OR (op = neq) OR (op = leq) OR (op = geq) THEN
            tp := boolptr;
         ELSE
            InitConstSet(res, tp);
            result := res.setvalue^.valentry;
         END;
         left := c1.setvalue^.valentry;
         right := c2.setvalue^.valentry;
         FOR index := 0 TO c1.setvalue^.size-1 DO
            CASE op OF
            | plus  : result^[index] := left^[index] + right^[index];
            | minus : result^[index] := left^[index] - right^[index];
            | times : result^[index] := left^[index] * right^[index];
            | slash : result^[index] := left^[index] / right^[index];
            | eql, neq :
                  IF left^[index] <> right^[index] THEN
                     res.value := ORD(op = neq);
                     RETURN
                  END;
            | leq :
                  IF NOT (left^[index] <= right^[index]) THEN
                     res.value := ORD(FALSE);
                     RETURN
                  END;
            | geq :
                  IF NOT (left^[index] >= right^[index]) THEN
                     res.value := ORD(FALSE);
                     RETURN
                  END;
            ELSE
               err := TRUE; RETURN
            END;
         END;
         CASE op OF
         | eql, neq, leq, geq:
               res.value := ORD(op <> neq);
         ELSE
            TermConstSet(res);
         END;
      END;
   END BigSetOp;

END MCBigSet.
