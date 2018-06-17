(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
IMPLEMENTATION MODULE MCP1Reals;  (* LG / AFB *)

  FROM Storage IMPORT ALLOCATE;
  FROM MCBase IMPORT Constval; 
 
  (*
   *	Motorola M68881
   *
   *	double precision binary real format:
   *
   *	 0 1       11 12                                          63
   *	+-+----------+------------------------------- - - - --------+
   *	|S|    X     |		F                                   |
   *	+-+----------+------------------------------- - - - --------+
   *
   *	where
   *
   *	S:	sign bit
   *	X:	biased exponent (11 bits)
   *	F:	fraction (52 bits)
   *		
   *	most negative:	ffef ffff ffff ffff'	-1.797693134862314e+308
   *	least negative: 8010 0000 0000 0000'	-2.225073858507201e-308
   *	least positive:	0010 0000 0000 0000'	 2.225073858507201e-308
   *	most positive:	7fef ffff ffff ffff'	 1.797693134862314e+308
   *)

  CONST maxexp = 309;
        minexp = -307;
        maxdignum = 16;

	(* last digit of maxfractdigits 4 (instead of 5) due to *)
	(* rounding errors *)
        maxfractdigits = "1797693134862314";
	minfractdigits = "2225073858507201";
 
  TYPE DigitString = ARRAY[0..maxdignum-1] OF CHAR;
 
  VAR minfract, maxfract : REAL;
      r0, r1, r10 : REAL; 
      rval : REAL;
      base : REAL;
      baseexp : CARDINAL;
      period : BOOLEAN;
      exp : INTEGER;
      dignum : INTEGER;
      eval : INTEGER;
      fok, eok : BOOLEAN;
      negexp : BOOLEAN;
      LeastPositive: ARRAY[0..1] OF CARDINAL;
 
  PROCEDURE InitRealConst;
    (* initialise the calculation of a constant real number *)
  BEGIN
    rval := r0;
    base := r1;
    baseexp := 0;
    period := FALSE;
    exp := 0;
    dignum := 0;
    eval := 0;
    eok := TRUE;
    fok := TRUE;
    negexp := FALSE;
  END InitRealConst;
 
  PROCEDURE ConvertToFraction(ch: CHAR);
    (* convert a character to the fraction of a constant real number *)
  BEGIN
    IF ch = '.' THEN period := TRUE;
    ELSE
      IF (dignum = 0) AND (ch = '0') THEN
        IF period THEN DEC(exp) END;
      ELSIF dignum < maxdignum THEN 
        rval := rval * r10 + FLOAT(ORD(ch) - ORD('0')); 
        IF period THEN DEC(exp) END; 
        INC(dignum); 
      ELSE
	INC(baseexp);
	IF baseexp >= ORD(ABS(minexp)) THEN
	  fok := FALSE;
	ELSE
	  base := base / r10;
	  rval := rval + base * FLOAT(ORD(ch) - ORD('0'));
	END;
        IF NOT period THEN INC(exp) END;
      END;
    END;
  END ConvertToFraction;
 
  PROCEDURE ConvertToExponent(ch: CHAR);
    (* convert a character to the exponent of a constant real number *)
  BEGIN
    IF ch = '-' THEN negexp := TRUE;
    ELSE
      IF eval < 1000 THEN
        eval := eval * 10 + VAL(INTEGER,ORD(ch) - ORD('0'));
      ELSE
        eok := FALSE;
      END;
    END;
  END ConvertToExponent;
 
  PROCEDURE TermRealConst(VAR cval: Constval; VAR long, err: BOOLEAN);
    (* terminate the calculation of a constant real number *)
    VAR totexp : INTEGER;

    PROCEDURE TenTo(exp: INTEGER): REAL;
      VAR r, res : REAL;
    BEGIN
      res := r1;
      r := r10;
      LOOP
        IF ODD(exp) THEN res := res * r END;
        exp := exp DIV 2;
        IF exp = 0 THEN EXIT END;
        r := r * r;
      END;
      RETURN res
    END TenTo;
 
  BEGIN
    err := FALSE;
    long := FALSE;
    IF fok AND eok THEN
      IF negexp THEN DEC(exp,eval) ELSE INC(exp,eval) END;
      totexp := dignum + exp;
      IF totexp > maxexp THEN exp := 0; err := TRUE;
      ELSIF (totexp = maxexp) OR (totexp = minexp) THEN
        (* compare with maxfract/minfract *);
        WHILE dignum < maxdignum DO
          rval := rval * r10;
          INC(dignum);
          DEC(exp);
        END;
	IF (totexp = maxexp) AND (rval > maxfract) OR
	  (totexp = minexp) AND (rval < minfract) THEN
	  exp := 0; rval := r0; err := totexp = maxexp; 
	ELSIF (totexp = minexp) AND (rval = minfract) THEN
	  (* avoid underflow *)
	  rval := REAL(LeastPositive);
	  exp := 0;
	END;
      ELSIF totexp < minexp THEN exp := 0; rval := r0;
      END;
      IF exp > 0 THEN (* maximal value is maxexp - 1 *)
        rval := rval * TenTo(exp);
      ELSIF exp < 0 THEN (* minimal value is minexp - maxdignum *)
        IF exp < -(maxexp-1) + maxdignum THEN
          rval := rval / TenTo(-(maxexp-1) + maxdignum - exp);
          exp := -(maxexp-1) + maxdignum;
        END;
        rval := rval / TenTo(- exp);
      END;
    ELSE (* not (eok or fok) *)
      IF dignum <> 0 THEN err := TRUE END;
    END;
    IF err THEN rval := r0 END;
    NEW(cval.rvalue);
    cval.rvalue^ := rval;
  END TermRealConst;

  PROCEDURE InitFraction(str: DigitString; VAR fract: REAL);
    VAR ix : CARDINAL;
  BEGIN
    InitRealConst;
    ix := 0;
    WHILE (ix < maxdignum) AND (str[ix] <> 0C) DO
      ConvertToFraction(str[ix]);
      INC(ix);
    END;
    fract := rval;
  END InitFraction;

BEGIN (* MCP1Reals *)
  r0 := FLOAT(0);
  r1 := FLOAT(1);
  r10 := FLOAT(10);
  InitFraction(maxfractdigits, maxfract);
  InitFraction(minfractdigits, minfract);
  LeastPositive[0] := 00100000H;
  LeastPositive[1] := 0H;
END MCP1Reals.
