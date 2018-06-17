IMPLEMENTATION MODULE Functions; (* AFB 12/88 *)

   FROM Storage IMPORT ALLOCATE, DEALLOCATE;
   FROM Strings IMPORT StrCmp, StrCpy;
   IMPORT Strings, Storage;

   IMPORT ASCII;

   (* (* exported from definition module *)
   TYPE
      Function;
      Real = REAL;
      StdFunc1 = PROCEDURE (Real) : Real;
      StdFunc2 = PROCEDURE (Real, Real) : Real;

   VAR
      errpos: CARDINAL; (* error position in expr of ParseFunction *)
   *)

   CONST
      symlength = 8; (* number of significant letters in identifiers *)

   TYPE
      Symbol = (plus, minus, times, slash,
		lt, le, eq, ne, ge, gt,
		andSY, orSY, notSY,
		thenSY, elseSY,
		lparen, rparen, comma,
		identSY, constSY, eofSY, errorSY);
      SymbolSet = SET OF Symbol;

   CONST
      RelOp = SymbolSet{lt..gt};
      AddOp = SymbolSet{plus, minus, orSY};
      MulOp = SymbolSet{times, slash, andSY};

   TYPE
      SymName = ARRAY [0..symlength-1] OF CHAR;

      (* tree for standard functions and constants *)
      StdType = (func1, func2, stdconst);
      StdTree = POINTER TO StdRec;
      StdRec =
	 RECORD
	    stdname: SymName;
	    left, right: StdTree;
	    CASE st: StdType OF
	    | func1:    stdfunc1: StdFunc1;
	    | func2:    stdfunc2: StdFunc2;
	    | stdconst: constval: Real;
	    END;
	 END;

      (* tree for parameters -- one per function *)
      SymTree = POINTER TO SymRec;
      SymRec =
	 RECORD
	    symname: SymName;
	    value: Real;
	    next: SymTree;
	    left, right: SymTree;
	 END;

      (* expression tree -- one per function *)
      NodeType = (const, var, op, std);
      ExprTree = POINTER TO ExprRec;
      ExprRec =
	 RECORD
	    CASE nt: NodeType OF
	    | const: constval: Real;
	    | var:   symref: SymTree;
	    | op:    opsy: Symbol;
		     left, right: ExprTree;
	    | std:   stdref: StdTree;		(* functions only *)
		     arg1, arg2: ExprTree;
	    END;
	 END;

      (* implementation of abstract data type *)
      Function = POINTER TO FunctionRec;
      FunctionRec =
	 RECORD
	    param: SymTree;     (* binary, sorted *)
	    nextparam: SymTree; (* linear, sorted *)
	    actparam: SymTree;  (* actual parameter *)
	    expr: ExprTree;     (* binary *)
	 END;

   VAR
      stdroot: StdTree;		(* tree of standard names *)

   (* routines for management of standard names *)

   PROCEDURE StdSearch(name: ARRAY OF CHAR; VAR ptr: StdTree) : BOOLEAN;
      VAR
	 father: StdTree;
   BEGIN
      ptr := stdroot; father := NIL;
      WHILE (ptr # NIL) & (StrCmp(name, ptr^.stdname) # 0) DO
	 father := ptr;
	 IF StrCmp(name, ptr^.stdname) < 0 THEN
	    ptr := ptr^.left;
	 ELSE
	    ptr := ptr^.right;
	 END;
      END;
      IF ptr = NIL THEN
	 ptr := father;
	 RETURN FALSE
      END;
      RETURN TRUE
   END StdSearch;

   PROCEDURE StdInsert(name: ARRAY OF CHAR; VAR ptr: StdTree);
      VAR
	 new: StdTree;
   BEGIN
      IF NOT StdSearch(name, ptr) THEN
	 NEW(new);
	 WITH new^ DO
	    left := NIL; right := NIL;
	    StrCpy(stdname, name);
	 END;
	 IF ptr = NIL THEN
	    stdroot := new;
	 ELSIF StrCmp(name, ptr^.stdname) < 0 THEN
	    ptr^.left := new;
	 ELSE
	    ptr^.right := new;
	 END;
	 ptr := new;
      END;
   END StdInsert;

   PROCEDURE InstallStdFunc1(funcname: ARRAY OF CHAR; stdfunc: StdFunc1);
      VAR ptr: StdTree;
   BEGIN
      StdInsert(funcname, ptr);
      WITH ptr^ DO
	 st := func1;
	 stdfunc1 := stdfunc;
      END;
   END InstallStdFunc1;

   PROCEDURE InstallStdFunc2(funcname: ARRAY OF CHAR; stdfunc: StdFunc2);
      VAR ptr: StdTree;
   BEGIN
      StdInsert(funcname, ptr);
      WITH ptr^ DO
	 st := func2;
	 stdfunc2 := stdfunc;
      END;
   END InstallStdFunc2;

   PROCEDURE InstallStdConst(constname: ARRAY OF CHAR; val: Real);
      VAR ptr: StdTree;
   BEGIN
      StdInsert(constname, ptr);
      WITH ptr^ DO
	 st := stdconst;
	 constval := val;
      END;
   END InstallStdConst;

   PROCEDURE ParseFunction(expr: ARRAY OF CHAR; VAR func: Function) : BOOLEAN;

      VAR
	 error: BOOLEAN;  (* any errors occured? *)

      PROCEDURE Error;
      BEGIN
	 error := TRUE; SetErrPos;
      END Error;

      MODULE Lex;

	 FROM ASCII IMPORT tab, nl; (* tab and new-line *)
	 IMPORT Symbol, expr, Real, SymName, error, errpos, Error;
	 EXPORT GetSy, sy, val, ident, SetErrPos;

	 VAR
	    index: CARDINAL;	(* position inside of `expr' *)
	    ch: CHAR;		(* character read by NextCh *)
	    sy: Symbol;		(* symbol returned by GetSy *)
	    val: Real;		(* set by GetSy if sy=constSY *)
	    ident: SymName;	(* set by GetSy if sy=identSY *)

	 PROCEDURE SetErrPos;
	    (* correct error position (necessary due to lookahead) *)
	 BEGIN
	    IF index > 1 THEN
	       errpos := index-2;
	    ELSE
	       errpos := 0;
	    END;
	 END SetErrPos;

	 PROCEDURE NextCh;
	    (* return next character of `expr' *)
	 BEGIN
	    IF index <= HIGH(expr) THEN
	       ch := expr[index];
	       IF ch # 0C THEN
		  INC(index);
	       END;
	    ELSE
	       ch := 0C;
	    END;
	 END NextCh;

	 PROCEDURE GetSy;
	    VAR
	       index: CARDINAL;		(* index of `ident' *)

	    PROCEDURE ReadReal(VAR x: REAL);
	       (* first character is read and already in `ch' *)
	       CONST 
		  (* machine dependent *)
		  maxexp    = 76;	(* maximal exponent *)
		  minexp    = -80;	(* minimal exponent *)
		  maxdignum = 16;	(* # of significant digits *)

		  eof = 0C;

	       VAR 
		  base   : REAL;
		  period : BOOLEAN;
		  exp    : INTEGER;
		  dignum : INTEGER;
		  rval   : REAL;
		  eval   : INTEGER;               (* value of exponent *)
		  eok    : BOOLEAN;               (* exponent okay *)
		  negexp : BOOLEAN;               (* negative exponent *)
		  err    : BOOLEAN;		 (* error flag *)
		  inexp  : BOOLEAN;		 (* in exponent *)

	       PROCEDURE ConvertToFraction(ch: CHAR);
	       BEGIN 
		  IF ch = '.' THEN 
		     period := TRUE;
		  ELSE 
		     IF (dignum = 0) AND (ch = '0') THEN 
			IF period THEN 
			   DEC(exp)
			END;
		     ELSIF dignum < maxdignum THEN 
			rval := rval * 10.0 + FLOAT(ORD(ch) - ORD( '0'));
			IF period THEN 
			   DEC(exp)
			END;
			INC(dignum);
		     ELSE 
			base := base / 10.0;
			rval := rval + base * FLOAT(ORD(ch) - ORD( '0'));
			IF NOT period THEN 
			   INC(exp)
			END;
		     END;
		  END;
	       END ConvertToFraction;

	       PROCEDURE ConvertToExponent(ch: CHAR);
	       BEGIN 
		  IF ch = '-' THEN 
		     negexp := TRUE;
		  ELSIF ch = '+' THEN
		     (* nothing *)
		  ELSE 
		     IF eval < 1000 THEN 
			eval := eval * 10 + VAL(INTEGER, ORD(ch) - ORD( '0'));
		     ELSE 
			eok := FALSE;
		     END;
		  END;
	       END ConvertToExponent;

	       PROCEDURE TermRealConst;
	       (* terminate the calculation of a constant real number *)
		  VAR 
		     totexp : INTEGER;

		  PROCEDURE TenTo(exp: INTEGER): REAL;
		     VAR 
			r, res : REAL;

		  BEGIN 
		     res := 1.0;
		     r := 10.0;
		     LOOP 
			IF ODD(exp) THEN 
			   res := res * r 
			END;
			exp := exp DIV 2;
			IF exp = 0 THEN 
			   EXIT 
			END;
			r := r * r;
		     END;
		     RETURN res 
		  END TenTo;

	       BEGIN 
		  err := FALSE;
		  IF eok THEN 
		     IF negexp THEN 
			DEC(exp,eval)
		     ELSE 
			INC(exp,eval)
		     END;
		     totexp := dignum + exp;
		     IF totexp > maxexp THEN 
			exp := 0;
			err := TRUE;
		     ELSIF totexp < minexp THEN 
			exp := 0;
			rval := 0.0;
		     END;
		     IF exp > 0 THEN             (* maximal value is maxexp - 1 *)
			rval := rval * TenTo(exp);
		     ELSIF exp < 0 THEN (* minimal value is minexp-maxdignum *)
			IF exp < minexp THEN 
			   rval := rval / TenTo(minexp - exp);
			   exp := minexp;
			END;
			rval := rval / TenTo(- exp);
		     END;
		  ELSE                           (* not eok *)
		     IF dignum <> 0 THEN 
			err := TRUE 
		     END;
		  END;
		  IF err THEN 
		     rval := 0.0 
		  END;
	       END TermRealConst;

	       PROCEDURE Finish;
	       BEGIN
		  TermRealConst;
		  IF err THEN
		     Error;
		  ELSE
		     x := rval;
		  END;
	       END Finish;

	    BEGIN 
	       (* init *)
	       inexp := FALSE;
	       rval := 0.0;
	       base := 1.0;
	       period := FALSE;
	       exp := 0;
	       dignum := 0;
	       eval := 0;
	       eok := TRUE;
	       negexp := FALSE;
	       x := 0.0;
	       IF ch = eof THEN
		  Error; RETURN 
	       END;
	       WHILE ch <> eof DO 
		  IF ((ch = 'e') OR (ch = 'E')) & ~inexp THEN 
		     inexp := TRUE;
		  ELSIF (ch >= '0') AND (ch <= '9') OR (ch = '-') AND inexp
		     OR (ch = '.') AND NOT period OR (ch = '+') AND inexp THEN
		     IF inexp THEN 
			IF (ch = '.') THEN 
			   eok := FALSE;
			ELSIF eok THEN 
			   ConvertToExponent(ch);
			END;
		     ELSIF (ch <> '+') THEN
			ConvertToFraction(ch);
		     END;
		  ELSE 
		     Finish; RETURN
		  END;
		  NextCh;
	       END;                              (* WHILE *)
	       Finish;
	    END ReadReal;

	 BEGIN
	    WHILE (ch = ' ') OR (ch = tab) OR (ch = nl) DO
	       NextCh;
	    END;
	    CASE ch OF
	    | '0'..'9':
		  sy := constSY;
		  ReadReal(val);
	    | 'a'..'z', 'A'..'Z':
		  sy := identSY;
		  index := 0;
		  REPEAT
		     IF index <= HIGH(ident) THEN
			ident[index] := ch;
		     END;
		     INC(index);
		     NextCh;
		  UNTIL ((ch < '0') OR (ch > '9')) &
			((ch < 'a') OR (ch > 'z')) &
			((ch < 'A') OR (ch > 'Z'));
		  IF index <= HIGH(ident) THEN
		     ident[index] := 0C;
		  END;
	    | '+': sy := plus; NextCh;
	    | '-': sy := minus; NextCh;
	    | '*': sy := times; NextCh;
	    | '/': sy := slash; NextCh;
	    | '(': sy := lparen; NextCh;
	    | ')': sy := rparen; NextCh;
	    | '&': sy := andSY; NextCh;
	    | '|': sy := orSY; NextCh;
	    | '~': sy := notSY; NextCh;
	    | ',': sy := comma; NextCh;
	    | '?': sy := thenSY; NextCh;
	    | ':': sy := elseSY; NextCh;
	    | '#': sy := ne; NextCh;
	    | '=': sy := eq; NextCh;
	    | '<': NextCh;
		   IF ch = '=' THEN
		      sy := le; NextCh;
		   ELSIF ch = '>' THEN
		      sy := ne; NextCh;
		   ELSE
		      sy := lt;
		   END;
	    | '>': NextCh;
		   IF ch = '=' THEN
		      sy := ge; NextCh;
		   ELSE
		      sy := gt;
		   END;
	    | 0C:  sy := eofSY;
	    ELSE
	       sy := errorSY;
	    END;
	 END GetSy;

      BEGIN
	 index := 0; NextCh;
      END Lex;

      (* expression tree constructors *)

      PROCEDURE Node(opsym: Symbol; l, r: ExprTree) : ExprTree;
	 VAR
	    new: ExprTree;
      BEGIN
	 NEW(new);
	 WITH new^ DO
	    nt := op;
	    opsy := opsym;
	    left := l;
	    right := r;
	 END;
	 RETURN new
      END Node;

      PROCEDURE LeafVar(sym: SymTree) : ExprTree;
	 VAR
	    new: ExprTree;
      BEGIN
	 NEW(new);
	 WITH new^ DO
	    nt := var;
	    symref := sym;
	 END;
	 RETURN new
      END LeafVar;

      PROCEDURE LeafConst(val: Real) : ExprTree;
	 VAR
	    new: ExprTree;
      BEGIN
	 NEW(new);
	 WITH new^ DO
	    nt := const;
	    constval := val;
	 END;
	 RETURN new;
      END LeafConst;

      PROCEDURE LeafStd(stdptr: StdTree; expr1, expr2: ExprTree) : ExprTree;
	 VAR
	    new: ExprTree;
      BEGIN
	 NEW(new);
	 WITH new^ DO
	    nt := std;
	    stdref := stdptr;
	    arg1 := expr1;
	    arg2 := expr2;
	 END;
	 RETURN new;
      END LeafStd;

      PROCEDURE EnterSym() : SymTree;
	 (* management of symbol table *)
	 (* take ident from Lex *)
	 VAR
	    father, ptr: SymTree;
	    cmp: INTEGER;

	 PROCEDURE Compare() : INTEGER;
	 BEGIN
	    cmp := StrCmp(ident, ptr^.symname);
	    RETURN cmp
	 END Compare;

      BEGIN
	 ptr := func^.param; father := NIL;
	 WHILE (ptr # NIL) & (Compare() # 0) DO
	    father := ptr;
	    IF cmp < 0 THEN
	       ptr := ptr^.left;
	    ELSE
	       ptr := ptr^.right;
	    END;
	 END;
	 IF ptr = NIL THEN
	    NEW(ptr);
	    WITH ptr^ DO
	       left := NIL;
	       right := NIL;
	       next := NIL;
	       symname := ident;
	       value := 0.0;
	    END;
	    IF father = NIL THEN
	       func^.param := ptr;
	    ELSE
	       IF cmp < 0 THEN
		  father^.left := ptr;
	       ELSE
		  father^.right := ptr;
	       END;
	    END;
	 END;
	 RETURN ptr
      END EnterSym;

      (* nonterminal procedures *)

      PROCEDURE CondExpression(VAR expr: ExprTree);
	 (* right-associative cond? then-expr: else-expr *)
	 VAR
	    then, else: ExprTree;
      BEGIN
	 Expression(expr);
	 IF sy = thenSY THEN
	    GetSy;
	    CondExpression(then);
	    IF sy = elseSY THEN
	       GetSy;
	       CondExpression(else);
	    ELSE
	       else := NIL;
	       Error;
	    END;
	    then := Node(elseSY, then, else);
	    expr := Node(thenSY, expr, then);
	 END;
      END CondExpression;

      PROCEDURE Expression(VAR expr: ExprTree);

	 VAR
	    rexpr: ExprTree;
	    opsy: Symbol;

	 PROCEDURE SimpleExpression(VAR expr: ExprTree);

	    VAR
	       rexpr: ExprTree;
	       opsy: Symbol;
	       neg: BOOLEAN;     (* unary minus? *)

	    PROCEDURE Term(VAR expr: ExprTree);

	       VAR
		  rexpr: ExprTree;
		  opsy: Symbol;

	       PROCEDURE Factor(VAR expr: ExprTree);
		  VAR
		     stdref: StdTree;
		     arg1, arg2: ExprTree;
	       BEGIN
		  CASE sy OF
		  | constSY: expr := LeafConst(val); GetSy;
		  | identSY: IF StdSearch(ident, stdref) THEN
				WITH stdref^ DO
				   IF st = stdconst THEN
				      expr := LeafConst(constval); GetSy;
				   ELSE (* st = func1 or func2 *)
				      GetSy;
				      IF sy = lparen THEN
					 GetSy;
					 CondExpression(arg1);
					 IF st = func2 THEN
					    IF sy = comma THEN
					       GetSy;
					       CondExpression(arg2);
					    ELSE
					       Error;
					    END;
					 ELSE (* st = func1 *)
					    arg2 := NIL;
					 END;
					 expr := LeafStd(stdref, arg1, arg2);
					 IF sy = rparen THEN
					    GetSy;
					 ELSE
					    Error;
					 END;
				      ELSE
					 Error;
				      END;
				   END;
				END;
			     ELSE
				expr := LeafVar(EnterSym()); GetSy;
			     END;
		  | lparen:  GetSy; CondExpression(expr);
			     IF sy = rparen THEN GetSy ELSE Error END;
		  | notSY:   GetSy; Factor(expr);
			     expr := Node(notSY, expr, NIL);
		  ELSE
		     expr := NIL; Error;
		  END;
	       END Factor;

	    BEGIN (* Term *)
	       Factor(expr);
	       WHILE sy IN MulOp DO
		  opsy := sy; GetSy;
		  Factor(rexpr);
		  expr := Node(opsy, expr, rexpr);
	       END;
	    END Term;

	 BEGIN (* SimpleExpression *)
	    neg := sy = minus;
	    IF (sy = plus) OR (sy = minus) THEN
	       GetSy;
	    END;
	    Term(expr);
	    WHILE sy IN AddOp DO
	       opsy := sy; GetSy;
	       Term(rexpr);
	       expr := Node(opsy, expr, rexpr);
	    END;
	    IF neg THEN
	       expr := Node(minus, expr, NIL);
	    END;
	 END SimpleExpression;

      BEGIN (* Expression *)
	 SimpleExpression(expr);
	 IF sy IN RelOp THEN (* relational operators are non-associative *)
	    opsy := sy; GetSy;
	    SimpleExpression(rexpr);
	    expr := Node(opsy, expr, rexpr);
	 END;
      END Expression;

      PROCEDURE MakeChain;

	 (* link parameters in alphabetical order *)

	 PROCEDURE Inorder(sym: SymTree);
	    (* reverse inorder *)

	    PROCEDURE Insert(sym: SymTree);
	    BEGIN
	       sym^.next := func^.nextparam;
	       func^.nextparam := sym;
	    END Insert;

	 BEGIN
	    IF sym # NIL THEN
	       WITH sym^ DO
		  Inorder(right);
		  Insert(sym);
		  Inorder(left);
	       END;
	    END;
	 END Inorder;

      BEGIN
	 WITH func^ DO
	    nextparam := NIL;
	    Inorder(param);
	    actparam := nextparam;
	 END;
      END MakeChain;

   BEGIN
      NEW(func); func^.param := NIL; func^.expr := NIL;
      error := FALSE;
      GetSy;
      CondExpression(func^.expr);
      IF sy # eofSY THEN Error END;
      MakeChain;
      IF error THEN
	 DisposeFunction(func);
      END;
      RETURN NOT error
   END ParseFunction;

   PROCEDURE FirstParam(func: Function);
   BEGIN
      WITH func^ DO
	 actparam := nextparam;
      END;
   END FirstParam;

   PROCEDURE NextParam(func: Function; VAR symname: ARRAY OF CHAR) : BOOLEAN;
   BEGIN
      WITH func^ DO
	 IF actparam = NIL THEN
	    actparam := nextparam;
	    RETURN FALSE
	 ELSE
	    StrCpy(symname, actparam^.symname);
	    actparam := actparam^.next;
	    RETURN TRUE
	 END;
      END;
   END NextParam;

   PROCEDURE SetFuncParam(func: Function; parname: ARRAY OF CHAR;
			  value: Real);
      VAR
	 father, ptr: SymTree;
	 cmp: INTEGER;

      PROCEDURE Compare() : INTEGER;
      BEGIN
	 cmp := StrCmp(parname, ptr^.symname);
	 RETURN cmp
      END Compare;

   BEGIN
      ptr := func^.param; father := NIL;
      WHILE (ptr # NIL) & (Compare() # 0) DO
	 father := ptr;
	 IF cmp < 0 THEN
	    ptr := ptr^.left;
	 ELSE
	    ptr := ptr^.right;
	 END;
      END;
      IF ptr = NIL THEN
	 (* parname unknown *)
	 RETURN
      END;
      ptr^.value := value;
   END SetFuncParam;

   PROCEDURE EvalFunction(func: Function) : Real;

      PROCEDURE Eval(expr: ExprTree) : Real;

	 CONST
	    RealTrue = 1.0;
	    RealFalse = 0.0;

	 PROCEDURE btor(bool: BOOLEAN) : Real;
	 BEGIN
	    IF bool THEN
	       RETURN RealTrue
	    ELSE
	       RETURN RealFalse
	    END;
	 END btor;

	 PROCEDURE rtob(r: Real) : BOOLEAN;
	 BEGIN
	    IF r = RealFalse THEN
	       RETURN FALSE
	    ELSE
	       RETURN TRUE
	    END;
	 END rtob;

      BEGIN
	 WITH expr^ DO
	    CASE nt OF
	    | const: RETURN constval
	    | var:   RETURN symref^.value
	    | std:   WITH stdref^ DO
			CASE st OF
			| func1: RETURN stdfunc1(Eval(arg1))
			| func2: RETURN stdfunc2(Eval(arg1), Eval(arg2))
			END;
		     END;
	    | op:    CASE opsy OF
		     | plus:  RETURN Eval(left) + Eval(right)
		     | minus: IF right = NIL THEN
				 RETURN - Eval(left)
			      ELSE
				 RETURN Eval(left) - Eval(right)
			      END;
		     | times: RETURN Eval(left) * Eval(right)
		     | slash: RETURN Eval(left) / Eval(right)
		     | lt:    RETURN btor(Eval(left) < Eval(right))
		     | le:    RETURN btor(Eval(left) <= Eval(right))
		     | eq:    RETURN btor(Eval(left) = Eval(right))
		     | ne:    RETURN btor(Eval(left) # Eval(right))
		     | ge:    RETURN btor(Eval(left) >= Eval(right))
		     | gt:    RETURN btor(Eval(left) > Eval(right))
		     | andSY: RETURN btor(rtob(Eval(left)) &
					  rtob(Eval(right)))
		     | orSY:  RETURN btor(rtob(Eval(left)) OR
					  rtob(Eval(right)))
		     | notSY: RETURN btor(NOT rtob(Eval(left)))
		     | thenSY:IF rtob(Eval(left)) THEN
				 RETURN Eval(right^.left)
			      ELSE
				 RETURN Eval(right^.right)
			      END;
		     END;
	    END;
	 END;
      END Eval;

   BEGIN
      RETURN Eval(func^.expr)
   END EvalFunction;

   PROCEDURE DisposeFunction(VAR func: Function);

      PROCEDURE DisposeParams(param: SymTree);
	 VAR
	    old: SymTree;
      BEGIN
	 WHILE param # NIL DO
	    old := param;
	    param := param^.next;
	    DISPOSE(old);
	 END;
      END DisposeParams;

      PROCEDURE DisposeExpr(expr: ExprTree);
      BEGIN
	 IF expr # NIL THEN
	    WITH expr^ DO
	       CASE nt OF
	       | op:  DisposeExpr(left); DisposeExpr(right);
	       | std: DisposeExpr(arg1); DisposeExpr(arg2);
	       ELSE
	       END;
	    END;
	    DISPOSE(expr);
	 END;
      END DisposeExpr;

   BEGIN
      WITH func^ DO
	 DisposeParams(nextparam);
	 DisposeExpr(expr);
      END;
      DISPOSE(func);
   END DisposeFunction;

BEGIN
   stdroot := NIL;
END Functions.
