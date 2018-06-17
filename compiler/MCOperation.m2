(****************************************
*                                       *
*     MODULA-2 Multi-Pass Compiler      *
*     ****************************      *
*                                       *
*     Implementation for ENDLICH        *
*                                       *
*     MCOperations:                     *
*                                       * 
*     Arithmetic operations for         *
*     constant expression evaluation    *
*                                       * 
****************************************)

IMPLEMENTATION MODULE MCOperations;     (* LG *)
(* $T- *)

(* operations on constant expressions in compiler *)

   FROM MCBase IMPORT Stptr, Symbol, Structform, Constval,
      realptr, boolptr, longmaxcard, longmaxint, Stset;
   FROM MCTypes IMPORT IsReal, TypeSetCompatible, GetType,
      ResultOfNegation, IsCard, IsInt, IsLong, ConstType;
   FROM MCBigSet IMPORT BigSetOp;

   PROCEDURE TypeOfConstant(value: CARDINAL; VAR type: Stptr);
      VAR const: Constval;
   BEGIN
      const.value := value; ConstType(const, type);
   END TypeOfConstant;

   PROCEDURE TypeOfIntConstant(value: CARDINAL; VAR type: Stptr);
      VAR int: INTEGER;
   BEGIN
      int := INTEGER(value);
      type := ResultOfNegation(type);
   END TypeOfIntConstant;

   PROCEDURE BaseType(tp: Stptr) : Stptr;
   BEGIN
      WHILE (tp <> NIL) AND (tp^.form = subranges) DO
         tp := tp^.scalp;
      END;
      RETURN tp;
   END BaseType;

   PROCEDURE RelOp(c1,c2: Constval; VAR res: Constval; op: Symbol; tp: Stptr;
      VAR err: BOOLEAN);
      (* evaluation of a relational operation on constant values *)
      (* reals not implemented *)
      VAR 
         bool   : BOOLEAN;
         v1, v2 : CARDINAL;
   BEGIN 
      err := IsReal(tp);
      v1 := c1.value;
      v2 := c2.value;
      tp := BaseType(tp);
      IF tp = NIL THEN err := TRUE; RETURN END;
      IF tp^.form = bigsets THEN
         BigSetOp(c1, c2, res, op, tp, err); RETURN
      END;
      CASE op OF 
      | eql: 
            bool := v1 = v2;
      | neq: 
            bool := v1 <> v2;
      | leq: 
            IF tp^.form = sets THEN 
               bool := BITSET(v1) <= BITSET(v2);
            ELSIF IsInt(tp) THEN 
               bool := INTEGER(v1) <= INTEGER(v2);
            ELSE 
               bool := v1 <= v2;
            END;
      | geq: 
            IF tp^.form = sets THEN 
               bool := BITSET(v1) >= BITSET(v2);
            ELSIF IsInt(tp) THEN 
               bool := INTEGER(v1) >= INTEGER(v2);
            ELSE 
               bool := v1 >= v2;
            END;
      | lss: 
            IF IsInt(tp) THEN 
               bool := INTEGER(v1) < INTEGER(v2);
            ELSE 
               bool := v1 < v2 
            END;
      | grt: 
            IF IsInt(tp) THEN 
               bool := INTEGER(v1) > INTEGER(v2);
            ELSE 
               bool := v1 > v2;
            END;
      | insy: 
            (* tp type of v2 *)
            IF tp^.form = bigsets THEN
               BigSetOp(c1, c2, res, insy, tp, err);
               bool := VAL(BOOLEAN, res.value);
            ELSE
               bool := v1 IN BITSET(v2);
            END;
      END;
      IF err THEN 
         bool := FALSE 
      END;
      res.value := ORD(bool);
   END RelOp;

   PROCEDURE AddOp(c1,c2: Constval; VAR res: Constval; op: Symbol; VAR tp: 
      Stptr; VAR err: BOOLEAN);
      (* evaluation of additional operations on constant values *)
      (* return the result value by res and the result type by tp *)
      (* err indicates an overflow error *)

      (* reals not implemented *)

      VAR 
         v1, v2, v3 : CARDINAL;
         save       : CARDINAL;         (* for INTEGER subtraction *)
   BEGIN 
      v1 := c1.value;
      v2 := c2.value;
      err := FALSE;
      tp := BaseType(tp);
      IF tp = NIL THEN
         err := TRUE;
      ELSIF IsCard(tp) THEN
	 (* simulation of long cardinal arithmetic *)
         IF op = plus THEN 
            IF longmaxcard - v2 >= v1 THEN 
               v3 := v1 + v2;
	       TypeOfConstant(v3, tp);
            ELSE 
               err := TRUE;
            END;
         ELSE                           (* op = minus *)
            IF v2 <= v1 THEN 
               v3 := v1 - v2;
            ELSIF IsInt(tp) THEN        (* integer-or-cardinal type *)
               v3 := CARDINAL(INTEGER(v1) - INTEGER(v2));
	       tp := ResultOfNegation(tp);
            ELSE 
               err := TRUE;
            END;
         END;
      ELSIF IsInt(tp) THEN
	 (* simulation of integer arithmetic *)
         save := 0;
         IF op = minus THEN             (* invert operation *)
            IF v2 = longmaxint + 1 THEN (* maximal negative number *)
               v2 := longmaxint;
               save := 1;
            ELSIF v2 <> 0 THEN 
               v2 := longmaxcard - v2 + 1;
            END;
         END;
         IF (v1 <= longmaxint) AND (v2 <= longmaxint) THEN 
            v3 := v1 + v2 + save;
            err := v3 > longmaxint;
         ELSIF (v1 > longmaxint) AND (v2 > longmaxint) THEN 
            v2 := longmaxcard - v2 + 1;
            v3 := v1 - v2;              (* save is always zero *)
            err := v3 <= longmaxint;
         ELSE 
            v3 := CARDINAL(INTEGER(v1) + INTEGER(v2) + INTEGER(save));
         END;
	 TypeOfIntConstant(v3, tp);
      ELSIF IsReal(tp) THEN
         IF (op = minus) AND (c1.rvalue = NIL) AND (c2.rvalue <> NIL) THEN 
	    (* sign inversion *)
            (*
	    IF IsLong(tp) THEN
	       c2.lrvalue^ := - c2.lrvalue^;
	    ELSE
            *)
	       c2.rvalue^ := - c2.rvalue^;
            (*
	    END;
            *)
         ELSE 
            err := TRUE;                (* not implemented *)
         END;
	 (* bug fix afb 9/91: result must be valid in each case *)
	 v3 := v2;                   (* pointer value *)
      ELSIF tp = boolptr THEN 
         IF op = orsy THEN 
            v3 := ORD(VAL(BOOLEAN,v1) OR VAL(BOOLEAN,v2));
         END;
      ELSIF tp^.form = sets THEN 
         IF op = plus THEN 
            v3 := CARDINAL(BITSET(v1) + BITSET(v2));
         ELSE 
            v3 := CARDINAL(BITSET(v1) - BITSET(v2));
         END;
      ELSIF tp^.form = bigsets THEN
         BigSetOp(c1, c2, res, op, tp, err);
         RETURN
      END;
      IF err AND NOT IsReal(tp) THEN (* bug fix afb 9/91: see above *)
         v3 := 0 
      END;
      res.value := v3;
   END AddOp;

   PROCEDURE MulOp(c1,c2: Constval; VAR res: Constval; op: Symbol; VAR tp: 
      Stptr; VAR err: BOOLEAN);
      (* evaluation of multiplicational operations on constant values *)
      (* return the result value by res and the result type by tp *)
      (* err indicates an overflow or zero-division error *)

      (* reals not implemented *)

      VAR 
         v1, v2, v3 : CARDINAL;
         pos        : BOOLEAN;

      PROCEDURE Mul;
	 (* multiplication of positive values v1 and v2; result to v3 *)
	 (* use CARDINAL arithmetic only *)
         VAR 
            prod, d, h: CARDINAL;
            flag      : BOOLEAN;        (* is set if d becomes too big *)

      BEGIN 
         flag := FALSE;
         prod := 0;
         IF v1 < v2 THEN 
            h := v1;
            d := v2 
         ELSE 
            h := v2;
            d := v1 
         END;
         LOOP 
            IF h = 0 THEN 
               EXIT 
            END;
            IF ODD(h) THEN 
               IF flag OR (prod > longmaxcard - d) THEN 
		  (* overflow occured *)
                  prod := 0;
                  err := TRUE;
                  EXIT;
               ELSE 
                  prod := prod + d;
               END;
            END;
            h := h DIV 2;
            IF d > longmaxint THEN 
               flag := TRUE 
            ELSE 
               d := 2 * d 
            END;
         END;                           (* LOOP *)
         v3 := prod;
      END Mul;

      PROCEDURE DivMod;
	 (* DIV or MOD of values v1 and v2 simulated with *)
	 (* CARDINAL arithmetic. Result is assigned to v3 *)
         VAR 
            r, q, d: CARDINAL;

      BEGIN 
         IF v2 = 0 THEN 
            err := TRUE;
            v3 := 0 
         ELSE 
            r := v1;
            q := 0;
            d := v2;
            WHILE (d < r) AND (d <= longmaxint) DO 
               d := d * 2 
            END;
            LOOP 
               IF r >= d THEN 
                  r := r - d;
                  q := q + 1;
               END;
               IF d = v2 THEN 
                  EXIT 
               END;
               q := q * 2;
               d := d DIV 2;
            END;
            IF op = divsy THEN 
               v3 := q 
            ELSE 
               v3 := r 
            END;
         END;
      END DivMod;

   BEGIN 
      v1 := c1.value;
      v2 := c2.value;
      err := FALSE;
      tp := BaseType(tp);
      IF tp = NIL THEN
         err := TRUE;
      ELSIF IsCard(tp) THEN 
         IF op = times THEN 
            Mul;
         ELSIF (op = divsy) OR (op = modsy) THEN 
            DivMod;
         END;
	 TypeOfConstant(v3, tp);
      ELSIF IsInt(tp) THEN
         pos := TRUE;
         IF v1 > longmaxint THEN 
            pos := NOT pos;
            v1 := longmaxcard - v1 + 1 
         END;
         IF v2 > longmaxint THEN 
            pos := NOT pos;
            v2 := longmaxcard - v2 + 1 
         END;
         IF op = times THEN 
            Mul;
            IF NOT err THEN 
               IF pos THEN 
                  err := v3 > longmaxint;
               ELSE 
                  err := v3 > longmaxint + 1;
                  v3 := longmaxcard - v3 + 1;
               END;
            END;
         ELSIF (op = divsy) OR (op = modsy) THEN 
            DivMod;
            IF NOT err THEN 
               IF pos THEN 
                  err := v3 > longmaxint;
               ELSE 
                  v3 := longmaxcard - v3 + 1;
               END;
            END;
         END;
	 TypeOfIntConstant(v3, tp);
      ELSIF IsReal(tp) THEN 
         err := TRUE;                   (* not implemented *)
      ELSIF tp = boolptr THEN 
         IF op = andsy THEN 
            v3 := ORD(VAL(BOOLEAN,v1) AND VAL(BOOLEAN,v2));
         END;
      ELSIF tp^.form = sets THEN 
         IF op = times THEN 
            v3 := CARDINAL(BITSET(v1) * BITSET(v2));
         ELSIF op = slash THEN 
            v3 := CARDINAL(BITSET(v1) / BITSET(v2));
         END;
      ELSIF tp^.form = bigsets THEN
         BigSetOp(c1, c2, res, op, tp, err);
         RETURN
      END;
      IF err THEN 
	 IF IsReal(tp) THEN
	    (* bug fix afb 9/91: real value must be still valid *)
	    v3 := v2; (* pointer assignment *)
	 ELSE
	    v3 := 0 
	 END;
      END;
      res.value := v3;
   END MulOp;

   PROCEDURE NotOp(c1: Constval; VAR res: Constval);
      (* evaluation of NOT operation on a constant value *)
   BEGIN 
      res.value := ORD(NOT VAL(BOOLEAN,c1.value));
   END NotOp;

END MCOperations. 
