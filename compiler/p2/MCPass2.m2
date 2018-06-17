(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)

MODULE MCPass2; (* AFB 83-87 *)

  (* $T- *)

  IMPORT SYSTEM, MCP2Public, Storage, MCBase, MCP2IO, MCP2Ident, MCP2Reference,
     MCOperations, MCStop, MCTypes, MCBigSet;
  FROM Storage IMPORT ALLOCATE, DEALLOCATE, EndStorage;
  FROM MCStop IMPORT Stop;
  FROM MCP2Public IMPORT ErrorsFound;
  FROM MCBase IMPORT Idptr, Stptr, Structform, Stset, Idclass, Idset,
    Varkind, Kindvar, Constval, Keyarr, root, mainmodp, globvarnext, sysmodp,
    procnumber, modnamlength, maxprio, noprio, Symbol, onebyte,
    oneword, doubleword, procmarkspace, BitsPerWord, modrev, maxcard,
    longword, stackdir, Direction;
  FROM MCP2Reference IMPORT Reference, EndReference, InitRef, TermRef;
  FROM MCP2Ident IMPORT Locate, NewImpList, TermImpList, EnterImpList,
    MarkScope, ReleaseScope, MsEntry, EnterList, EnterId, SearchInBlock,
    SearchId, ExportSearch, SymModSearch, GlobalKnown, EnterExportId;
  FROM MCTypes IMPORT IsCard;
  FROM MCP2IO IMPORT TermInOut, Error, ErrorLS, GetSy, sy,
     spix, PutSy, PutGetSy, SkipConstant, SkipType, val, PutWord,
     AsciiSetPos, AsciiRead, GetModuleKey, DefModStatus, StopOutput,
     RestartOutput;

  MODULE ModulInitialisation; 
    (* $T- *)

    FROM MCP2IO IMPORT PutSy, PutWord;
    FROM MCBase IMPORT Symbol, Idptr;

    EXPORT MarkInitBlock, ReleaseInitBlock, EnterInitModule, ToInitModule,
      InitModules, MustInit, Initrange, ResetModuleInit;

    CONST initmax = 20;
          blevelmax = 20; 

    TYPE Initrange = [0..initmax];

    VAR inittab : ARRAY Initrange OF 
                    RECORD toinit : BOOLEAN;
                      mptr : Idptr; 
                    END;
        blockdispl : ARRAY [1..blevelmax] OF Initrange; 
        blevel : CARDINAL; 
        inittop : CARDINAL;

    PROCEDURE MarkInitBlock;
    BEGIN
      INC(blevel);
      IF blevel > blevelmax THEN HALT END;
      blockdispl[blevel] := inittop;
    END MarkInitBlock;

    PROCEDURE ReleaseInitBlock;
    BEGIN 
      inittop := blockdispl[blevel];
      DEC(blevel); 
    END ReleaseInitBlock;

    PROCEDURE EnterInitModule(ip : Idptr; VAR initix : Initrange); 
    BEGIN INC(inittop); 
      IF inittop > initmax THEN HALT END;
      initix := inittop; 
      WITH inittab[inittop] DO toinit := FALSE; mptr := ip END;
    END EnterInitModule;

    PROCEDURE ToInitModule(initix : Initrange); 
    BEGIN 
      WITH inittab[initix] DO
        toinit := TRUE;
        PutSy(proceduresy); PutWord(mptr);
      END 
    END ToInitModule; 

    PROCEDURE InitModules;
      VAR i : CARDINAL;
    BEGIN i := blockdispl[blevel] + 1;
      WHILE i <= inittop DO 
        WITH inittab[i] DO
          IF toinit THEN PutSy(call); PutSy(namesy); PutWord(mptr); 
            PutSy(lparent); PutSy(rparent); 
          END;
        END; INC(i);
      END 
    END InitModules;

    PROCEDURE MustInit() : BOOLEAN; 
      VAR i : CARDINAL;
    BEGIN i := blockdispl[blevel] + 1;
      WHILE i <= inittop DO
        IF inittab[i].toinit THEN RETURN TRUE END;
        INC(i);
      END;
      RETURN blevel = 1; (* global modules must always be initialised *)
    END MustInit; 

    PROCEDURE ResetModuleInit;
    BEGIN
      inittop := 0;
      blevel := 0;
    END ResetModuleInit;

    (* $T= *)
  END ModulInitialisation;

  PROCEDURE FAmong(sp: Stptr; forms: Stset): BOOLEAN;
  BEGIN
    IF sp = NIL THEN RETURN FALSE END;
    WITH sp^ DO
      RETURN (form IN forms) OR
	     (form = setoftypes) AND (typeset * forms <> Stset{}) OR (* REV *)
             (form = subranges) AND FAmong(scalp,forms);
    END;
  END FAmong;

  PROCEDURE QualIdent(klset: Idset; errnum: CARDINAL; VAR ip: Idptr);
  BEGIN (* sy = ident *)
    SearchId(ip); 
    LOOP
      GetSy;
      IF sy <> period THEN EXIT END;
      IF (ip <> NIL) AND (ip^.klass = mods) THEN
        GetSy;
        ExportSearch(ip^.expp,ip); (* new value for ip *)
      ELSE ErrorLS(105); GetSy;
      END;
    END;
    IF ip = NIL THEN
      ErrorLS(errnum);
    ELSIF NOT (ip^.klass IN klset) THEN
      ErrorLS(103); (* identifier not of expected klass *)
      ip := NIL;
    END;
  END QualIdent;

  PROCEDURE InitId(ip: Idptr; cl: Idclass);
    (* initialisation of identifier record *)
  BEGIN
    WITH ip^ DO
      name := spix; (* from MCP2IO *)
      globmodp := mainmodp;
      idtyp := NIL; (* = nxtidp *)
      klass := cl; link := NIL;
    END;
  END InitId;

   PROCEDURE Align(VAR addr: CARDINAL);
      VAR incr: CARDINAL;
   BEGIN
      IF addr MOD oneword <> 0 THEN
         incr := oneword - addr MOD oneword;
         IF MAX(CARDINAL) - addr >= incr THEN
            INC(addr, incr);
         ELSE
            ErrorLS(100);
         END;
      END
   END Align;

   CONST byteSize = Stset{bools, chars, bytes};

   PROCEDURE ToBeAligned(sp: Stptr) : BOOLEAN;
   BEGIN
      IF sp = NIL THEN RETURN FALSE END;
      WITH sp^ DO
         RETURN NOT (form IN byteSize) OR
                (form = subranges) AND ToBeAligned(scalp);
      END;
   END ToBeAligned;

  CONST scalars = Stset{enums,bools,chars,ints,cards,subranges,
			longints, longcards};
	intcars = Stset{ints, cards, longints, longcards};
	typesettypes = Stset{ints, cards, longints, longcards,
			     reals, longreals, setoftypes};
        charmax = 255; (* maximal value for a character *)

  VAR nestlevel : CARDINAL; (* nesting level *)
      symmod : BOOLEAN; (* current module is symbol module *)
      defmod : BOOLEAN; (* module is definition module *)
      impl: BOOLEAN; (* implementation expected *)
      oldlist: Idptr; (* list of identifiers to implement *)
      proccount : CARDINAL; (* counter of procedures *)

  MODULE ConstDefinition; 

    FROM SYSTEM IMPORT TSIZE;
    FROM Storage IMPORT ALLOCATE;
    FROM MCP2IO IMPORT sy, val, length, Error, ErrorLS, GetSy, PutGetSy,
      typeset;
    FROM MCBase IMPORT Idptr, Stptr, Idclass, Idset, Structform, Stset,
      Constval, Symbol, intptr, cardptr, realptr, charptr, boolptr,
      bitsetptr, strptrs, modrev, modrev2, maxcard, maxint, Stfuncs;
    FROM MCOperations IMPORT RelOp, AddOp, MulOp, NotOp;
    FROM TypeDefinition IMPORT ArrayStruct;
    FROM MCTypes IMPORT TypeSetCompatible, GetType, ResultOfNegation,
      TypeSetResult, IsInt, IsReal, IsCard, ConstType;
    FROM MCBigSet IMPORT InitConstSet, ConstSetElement, TermConstSet;
    IMPORT FAmong, QualIdent, symmod, intcars, scalars, typesettypes,
      BitsPerWord;

    EXPORT ConstantVal, ConstantRange, Compatible; 

    TYPE Constform = RECORD
                       ctp: Stptr;
                       cval: Constval;
                     END; 

    VAR forms : Stset;
        err : BOOLEAN;

    PROCEDURE ResultType(sp1, sp2: Stptr) : Stptr;
    BEGIN
      IF (sp1 = NIL) OR (sp2 = NIL) THEN
        RETURN NIL;
      END;
      IF (sp1^.form IN typesettypes) AND (sp2^.form IN typesettypes) THEN
	RETURN TypeSetResult(sp1, sp2)
      ELSE
	RETURN sp1
      END;
    END ResultType;

    PROCEDURE SetConstructor(VAR setptr: Stptr; VAR setval: Constval); 
      VAR styp,ctyp : Stptr;
          c1,c2 : CARDINAL;
          smin,smax : CARDINAL; 
          setpat : BITSET; (* set pattern *)
    BEGIN 
      smin := 0; smax := BitsPerWord-1; setpat := {};
      IF FAmong(setptr,Stset{sets}) THEN 
        styp := setptr^.basep;
      ELSE ErrorLS(99); 
        styp := NIL; setptr := NIL; 
      END;
      IF styp <> NIL THEN 
        WITH styp^ DO 
          CASE form OF
          | subranges:
             IF min > smin THEN smin := min END; 
             IF max < smax THEN smax := max END; 
	     styp := scalp; 
          | enums: IF cstnr < smax THEN smax := cstnr END;
          | bools: smax := 1;
          ELSE styp := NIL; 
          END; (* CASE *)
        END; (* WITH *) 
      END;
      GetSy; (* lconbr *) 
      WHILE sy <> rconbr DO 
        ConstantRange(ctyp,c1,c2); 
        IF (styp = NIL) AND FAmong(ctyp,Stset{enums,bools,cards}) THEN
          styp := ctyp
        END;
        IF (styp <> NIL) AND Compatible(styp, ctyp) THEN (* REV AFB 5/88 *)
          IF (c1 < smin) OR (c2 > smax) THEN
	    ErrorLS(98); 
          ELSE
	    setpat := setpat + {c1..c2};
          END;
        ELSE ErrorLS(97); 
        END;
      END;
      GetSy; (* rconbr *) 
      setval.value := CARDINAL(setpat);
    END SetConstructor; 

    PROCEDURE BigSetConstructor(VAR settype: Stptr; VAR setval: Constval);
       VAR styp, ctyp: Stptr; c1, c2: CARDINAL; smin, smax: CARDINAL;
    BEGIN
      (* settype^.form = bigsets *)
      WITH settype^ DO
         smin := low;
         smax := high;
         styp := basep;
      END;
      (* get base type of set for compatibility checks *)
      WHILE (styp <> NIL) AND (styp^.form = subranges) DO
         styp := styp^.scalp;
      END;
      InitConstSet(setval, settype);
      GetSy; (* lconbr *) 
      WHILE sy <> rconbr DO 
        ConstantRange(ctyp, c1, c2); 
        IF styp = NIL THEN
           (* error message already printed *)
        ELSIF (ctyp <> NIL) AND Compatible(styp, ctyp) THEN (* REV AFB 5/88 *)
          IF (c1 < smin) OR (c2 > smax) THEN
	    ErrorLS(98); 
          ELSE
            ConstSetElement(setval, c1, c2);
          END;
        ELSE ErrorLS(97); 
        END;
      END;
      GetSy; (* rconbr *) 
      TermConstSet(setval);
    END BigSetConstructor;

    PROCEDURE IsString(s: Stptr): BOOLEAN;
       VAR 
          str: BOOLEAN;
    BEGIN                          (* string structure ecpected *)
       str := FALSE;
       IF s <> NIL THEN 
          WITH s^ DO 
             IF (form = arrays) AND NOT dyn THEN 
                str := (elp=charptr) AND (ixp^.scalp=cardptr) AND (ixp^.
                   min=0)
             END 
          END                      (* WITH s^ *)
       END;
       RETURN str;
    END IsString;

    PROCEDURE IsCharComp(s: Stptr): BOOLEAN;
    BEGIN
       RETURN IsChar(s) OR IsString(s) AND (s^.ixp^.max = 0);
    END IsCharComp;

    PROCEDURE Compatible(tp1,tp2: Stptr): BOOLEAN;          
      (* compare on type compatibility in constant expressions *) 
    BEGIN 
      WHILE (tp1 <> NIL) AND (tp1^.form = subranges) DO
	tp1 := tp1^.scalp;
      END;
      WHILE (tp2 <> NIL) AND (tp2^.form = subranges) DO
	tp2 := tp2^.scalp;
      END;
      RETURN
        (tp1=tp2) OR (tp1=NIL) OR (tp2=NIL) OR 
	FAmong(tp1, typesettypes) AND TypeSetCompatible(tp1, tp2);
    END Compatible;   

    PROCEDURE IsChar(s: Stptr) : BOOLEAN;
    BEGIN
      IF s = NIL THEN RETURN FALSE END;
      IF s^.form = subranges THEN
        RETURN IsChar(s^.scalp)
      ELSE
        RETURN s^.form = chars
      END;
    END IsChar;
 
    PROCEDURE CharCast(VAR cf: Constform);
       TYPE
          CharPtr = POINTER TO ARRAY[0..0] OF CHAR;
       VAR
          cp: CharPtr;
    BEGIN
       WITH cf DO
          IF NOT IsChar(ctp) THEN (* string *)
             WITH cval DO
                cp := CharPtr(svalue^.valentry);
                value := ORD(cp^[0]);
             END;
             ctp := charptr;
          END;
       END;
    END CharCast;

    PROCEDURE ConstExpression(VAR cf: Constform);
      (* expression in constant definitions *)
      VAR cf1: Constform;
          op : Symbol;
          res : Constval;
          tp, tp1 : Stptr;

      PROCEDURE ConstSimpleExpression(VAR cf: Constform);
        (* simple expression in constant definitions *)
        VAR cf1 : Constform;
            op : Symbol;
            sign : BOOLEAN;

        PROCEDURE ConstTerm(VAR cf: Constform); 
          (* term in constant definitions *)  
          VAR cf1 : Constform; 
              op : Symbol; 

          PROCEDURE ConstFactor(VAR cf: Constform);  
            (* factor in constant definitions *)
            VAR ip : Idptr;

	    PROCEDURE StFunc(ip: Idptr; VAR type: Stptr; VAR val: Constval);
	      (* evaluate some standard functions in constant definitions *)
	      VAR
		cf: Constform;
		argip: Idptr;  (* symbol table ref of argument *)
		argtype: Stptr; (* type of argument *)
		minc, maxc: Constval; (* for MIN, MAX std function *)

	      PROCEDURE VariantAnalyse(tagref: Stptr; VAR sz: CARDINAL);
		 (* analyse of procedure parameters for record variants *)
		 (* in standard procedures SIZE, TSIZE *)
		 VAR 
		    equal    : BOOLEAN; (* if true: tag label found *)
		    vxv,vtrf : Stptr;  (* vtrf scans though the tagrefs *)
		    ttyp     : Stptr;  (* type of tag field *)
		    tval     : Constval; (* tag value *)
		    cf       : Constform;
	      BEGIN 
		 vtrf := tagref;
		 WHILE vtrf <> NIL DO 
		    IF sy = rparent THEN 
		       vtrf := NIL;
		    ELSE 
		       GetSy;
		       ConstExpression(cf);
		       ttyp := cf.ctp; tval := cf.cval;
		       WITH vtrf^ DO 
			  IF Compatible(ttyp, tagtyp) THEN
			     vxv := fstvarp;
			     equal := FALSE;
			     WHILE (vxv <> NIL) AND NOT equal DO 
				WITH vxv^ DO 
				   IF varval = tval.value THEN 
				      equal := TRUE 
				   ELSE 
				      vxv := nxtvarp 
				   END 
				END;            (* WITH vxv^ *)
			     END;
			     IF NOT equal AND (elsevarp <> NIL) THEN 
						(* ELSE variant *)
				vxv := elsevarp;
				equal := TRUE;
			     END;
			     IF equal THEN 
				sz := vxv^.size;
				vtrf := vxv^.subtagp;
				IF (vtrf = NIL) AND (sy <> rparent) THEN 
				   Error(132)
				END 
			     ELSE 
				Error(148);
				vtrf := NIL;
			     END;
			  ELSE 
			     Error(92);
			     vtrf := NIL;
			  END;
		       END;
		    END;
		 END;
		 WHILE sy <> rparent DO 
		    GetSy; (* skip comma *)
		    ConstExpression(cf);
		 END;
	      END VariantAnalyse;

	      PROCEDURE NextParameter(VAR cf: Constform);
	      BEGIN
		WITH cf DO
		   ctp := cardptr;
		   cval.value := 1;
		END;
		IF sy = rparent THEN
		  Error(127);
		ELSE
		  ConstExpression(cf);
		END;
	      END NextParameter;

	      PROCEDURE FetchType(idset: Idset; VAR type: Stptr);
		VAR argip: Idptr;
	      BEGIN
		IF sy = ident THEN
		  QualIdent(idset, 73, argip);
		  IF argip <> NIL THEN type := argip^.idtyp;
		  ELSE type := NIL;
		  END;
		ELSE
		  type := NIL;
		  ErrorLS(20);
		END;
	      END FetchType;

	    BEGIN
	      GetSy; (* skip lparent *)
	      CASE ip^.fname OF
	      | higf: (* HIGH *)
		  type := cardptr; val.value := 1;
		  FetchType(Idset{vars}, argtype);
		  IF argtype <> NIL THEN
		    WITH argtype^ DO
		      IF (form = arrays) AND NOT dyn THEN
			type := ixp;
			val.value := ixp^.max;
		      ELSE
			Error(136);
		      END;
		    END;
		  ELSE
		    Error(122);
		  END;
	      | sizf, tszf: (* SIZE, TSIZE *)
		  type := cardptr; val.value := 1;
		  IF ip^.fname = sizf THEN
		    FetchType(Idset{vars, types}, argtype);
		  ELSE
		    FetchType(Idset{types}, argtype);
		  END;
		  IF argtype <> NIL THEN
		    WITH argtype^ DO
		      IF NOT ((form = arrays) AND dyn) THEN
			val.value := size;
			IF form = records THEN
			  VariantAnalyse(tagp, val.value);
			END;
			ConstType(val, type);
		      ELSE
			Error(136);
		      END;
		    END;
		  END;
	      | oddf: (* ODD *)
		  type := boolptr;
		  NextParameter(cf);
		  val.value := ORD(ODD(cf.cval.value));
	      | absf: (* ABS *)
		  NextParameter(cf);
		  type := cf.ctp;
		  IF IsInt(type) THEN
		    val.value := ABS(INTEGER(cf.cval.value));
		  ELSIF IsReal(type) THEN
		    val.rvalue := cf.cval.rvalue;
		    val.rvalue^ := ABS(val.rvalue^);
		  ELSIF IsCard(type) THEN
		    val.value := cf.cval.value;
		  ELSE
		    val.value := 1;
		    ErrorLS(121);
		  END;
	      | capf: (* CAP *)
		  type := charptr;
		  NextParameter(cf);
		  IF IsCharComp(cf.ctp) THEN
		    CharCast(cf);
		    (* $R- *)
		    val.value := ORD(CAP(CHR(cf.cval.value)));
		    (* $R= *)
		  ELSE
		    ErrorLS(121);
		  END;
	      | ordf: (* ORD *)
		  type := GetType(Stset{cards, longcards});
		  NextParameter(cf);
		  IF IsCharComp(cf.ctp) THEN
		    CharCast(cf);
		  END;
		  IF FAmong(cf.ctp, scalars) THEN
		    val := cf.cval;
		  ELSE
		    ErrorLS(106);
		  END;
	      | chrf: (* CHR *)
		  type := charptr;
		  NextParameter(cf);
		  IF FAmong(cf.ctp, Stset{cards, longcards}) THEN
		    val := cf.cval;
		  ELSE
		    ErrorLS(128);
		  END;
	      | minf, maxf: (* MIN, MAX *)
		  FetchType(Idset{types}, argtype);
		  type := argtype;
		  IF argtype <> NIL THEN
		    WITH argtype^ DO
		      CASE form OF
		      | bools:
			  minc.value := ORD(MIN(BOOLEAN));
			  maxc.value := ORD(MAX(BOOLEAN));
		      | chars:
			  minc.value := ORD(MIN(CHAR));
			  maxc.value := ORD(MAX(CHAR));
		      | ints:
			  minc.value := CARDINAL(MIN(INTEGER));
			  maxc.value := CARDINAL(MAX(INTEGER));
		      | cards:
			  minc.value := MIN(CARDINAL);
			  maxc.value := MAX(CARDINAL);
		      | reals, longreals:
			  NEW(val.rvalue);
			  IF ip^.fname = minf THEN
			    val.rvalue^ := MIN(REAL);
			  ELSE
			    val.rvalue^ := MAX(REAL);
			  END;
		      | longints:
			  minc.value := LONGCARD(MIN(LONGINT));
			  maxc.value := LONGCARD(MAX(LONGINT));
		      | longcards:
			  minc.value := MIN(LONGCARD);
			  maxc.value := MAX(LONGCARD);
		      | enums:
			  minc.value := 0;
			  maxc.value := cstnr;
		      | subranges:
			  minc.value := min;
			  maxc.value := max;
		      ELSE
			Error(109);
			minc.value := 0;
			maxc.value := 0;
		      END; (* CASE form OF *)
		      IF NOT (form IN Stset{reals, longreals}) THEN
			IF ip^.fname = minf THEN
			  val := minc;
			ELSE
			  val := maxc;
			END;
		      END;
		    END; (* WITH argtype^ DO *)
		  ELSE (* argtype = NIL *)
		    (* error message already printed *)
		  END;
	      | valf: (* VAL *)
		  type := NIL;
		  FetchType(Idset{types}, argtype);
		  IF FAmong(argtype, scalars) THEN
		    type := argtype;
		  ELSE
		    ErrorLS(121);
		  END;
		  IF sy = comma THEN
		    GetSy;
		    NextParameter(cf);
		    IF FAmong(cf.ctp, intcars) THEN
		      val := cf.cval;
		    ELSE
		      ErrorLS(127);
		      val.value := 0;
		    END;
		  END;
	      ELSE (* CASE ip^.fname OF *)
		ErrorLS(136); (* no compiletime computable std func used *)
	      END;
	      IF sy <> rparent THEN
		ErrorLS(127); (* too many parameters *)
	      END;
	      (* avoid trouble in cases like TSIZE(INTEGER*4) *)
	      WHILE (sy <> rparent) AND (sy <> comma) AND (sy <> lparent) DO
		GetSy;
	      END;
	      (* skip superfluous parameters *)
	      WHILE sy <> rparent DO
		IF sy = comma THEN
		  GetSy;
		END;
		ConstExpression(cf);
	      END;
	      GetSy; (* skip rparent *)
	    END StFunc;

	    PROCEDURE TypFunction(VAR cf: Constform);
	       VAR 
		  argcf: Constform;
	    BEGIN 
	       GetSy; (* skip lparent *)
	       IF sy = rparent THEN 
		  Error(137);
	       ELSE 
		  ConstExpression(argcf);
		  IF IsChar(cf.ctp) AND IsCharComp(argcf.ctp) THEN
		     CharCast(argcf);
		  END;
		  IF NOT FAmong(argcf.ctp, scalars +
					   Stset{sets, reals, longreals}) OR
		     NOT FAmong(cf.ctp, scalars +
					Stset{sets, reals, longreals}) THEN
		     ErrorLS(139);
		     cf.ctp := NIL;
		     cf.cval.value := 0;
		  ELSE
		    IF (cf.ctp <> NIL) AND (argcf.ctp <> NIL) THEN 
		       IF cf.ctp^.size = argcf.ctp^.size THEN 
			  cf.cval := argcf.cval;
		       ELSE 
			  ErrorLS(120);
		       END;
		    END;
		  END;
		  IF sy <> rparent THEN
		    ErrorLS(127); (* skip superfluous parameters *)
		    WHILE sy <> rparent DO
		      IF sy = comma THEN
			GetSy;
		      END;
		      ConstExpression(argcf);
		    END;
		  END;
	       END;
	       GetSy; (* skip rparent *)
	    END TypFunction;

          BEGIN (* ConstFact *)
            WITH cf DO ctp := NIL; cval.value := 0 END;
            IF (sy >= intcon) AND (sy <= stringcon) THEN
              WITH cf DO
                WITH cval DO
                  value := val;
                  CASE sy OF
                  | intcon: ctp := intptr;
                  | intcarcon: ctp := GetType(typeset);
                  | cardcon: ctp := cardptr;
                  | realcon: ctp := GetType(Stset{reals, longreals});
                  | charcon: ctp := charptr;
                  | stringcon:
                      IF (length > 20) OR (strptrs[length] = NIL) THEN
                        ctp := ArrayStruct(0,length-1,cardptr,charptr);
                        IF length <= 20 THEN strptrs[length] := ctp END;
                      ELSE
                        ctp := strptrs[length];
                      END;
                  END;
                  IF symmod AND (sy = cardcon) THEN (* get type identifier *)
                    GetSy;
                    QualIdent(Idset{types},73,ip);
                    ctp := ip^.idtyp;
                  ELSE GetSy;
                  END;
                END; (* WITH *)
              END; (* WITH *)
            ELSIF sy = ident THEN
              (* constant or set constructor with type identifier *)
	      IF modrev2 THEN
		 (* some standard functions are allowed *)
		 QualIdent(Idset{consts,types,funcs},73,ip);
	      ELSE
		 QualIdent(Idset{consts,types},73,ip);
	      END;
              IF sy = lconbr THEN (* set constructor *)
                IF (ip <> NIL) AND (ip^.klass = types) THEN
                  cf.ctp := ip^.idtyp;
                ELSIF ip <> NIL THEN ErrorLS(103);
                END;
                WITH cf DO
                   IF (ctp <> NIL) AND (ctp^.form = bigsets) THEN
                      BigSetConstructor(ctp, cval);
                   ELSE
                      SetConstructor(ctp, cval);
                   END;
                END;
              ELSIF sy = bigsetcon THEN (* big set constant from symbol file *)
                cf.ctp := ip^.idtyp;
                cf.cval.value := val;
                GetSy;
	      ELSIF modrev2 AND (sy = lparent) THEN
		IF (ip <> NIL) AND (ip^.klass = funcs) AND ip^.isstandard THEN
		  StFunc(ip, cf.ctp, cf.cval);
		ELSIF (ip <> NIL) AND (ip^.klass = types) THEN
		  cf.ctp := ip^.idtyp;
		  TypFunction(cf);
		ELSE
		  ErrorLS(136);
		  GetSy;
		  WHILE sy <> rparent DO (* skip parameters *)
		    IF sy = comma THEN
		      GetSy;
		    END;
		    ConstExpression(cf);
		  END;
		  GetSy;
		END;
              ELSE
                IF (ip <> NIL) AND (ip^.klass = consts) THEN
                  WITH ip^ DO
                    cf.ctp := idtyp;
                    IF idtyp = realptr THEN (* make a copy of the real value *)
                      NEW(cf.cval.rvalue);
                      cf.cval.rvalue^ := cvalue.rvalue^;
                    ELSIF idtyp = NIL THEN ErrorLS(73);
                    ELSE cf.cval := cvalue;
                    END;
                  END;
                ELSIF ip <> NIL THEN ErrorLS(103); 
                END;
              END;
            ELSIF sy = lconbr THEN (* bitset *)
              cf.ctp := bitsetptr;
              SetConstructor(cf.ctp,cf.cval);
            ELSIF sy = lparent THEN
              GetSy;
              ConstExpression(cf);
              GetSy;
            ELSIF sy = notsy THEN
              GetSy;
              ConstFactor(cf);
              IF FAmong(cf.ctp,Stset{bools}) THEN
                NotOp(cf.cval,cf.cval);
              ELSE ErrorLS(140);
              END;
            END;
          END ConstFactor;

        BEGIN       
          ConstFactor(cf); 
          WHILE (sy >= andsy) AND (sy <= modsy) DO
            op := sy; 
            GetSy;
            ConstFactor(cf1); 
            IF cf.ctp = NIL THEN cf := cf1;
            ELSE
              WITH cf DO 
                IF Compatible(ctp,cf1.ctp) THEN
                  CASE op OF
                  | andsy: forms := Stset{bools};
                  | times: forms := Stset{ints,cards,longints,longcards,
					  sets,bigsets,reals};
                  | slash: forms := Stset{sets,bigsets,reals};
                  | divsy,modsy: forms := Stset{ints,cards,longints,longcards};
                  END;
                  IF FAmong(ctp,forms) THEN
		    ctp := ResultType(ctp, cf1.ctp);
                    MulOp(cval,cf1.cval,cval,op,ctp,err);
                    IF err THEN ErrorLS(94) END;
                  ELSE ErrorLS(140); 
                  END; 
                ELSE ErrorLS(143); 
                END; 
              END; (* WITH *) 
            END; 
          END; (* WHILE *) 
        END ConstTerm; 

      BEGIN 
        sign := (sy = minus) OR (sy = plus);
        IF sign THEN op := sy; GetSy; END;
        ConstTerm(cf);
        IF sign THEN
          WITH cf DO
	    IF IsInt(ctp) OR IsReal(ctp) THEN
              IF op = minus THEN
                IF ctp = realptr THEN cf1.cval.rvalue := NIL;
                ELSE cf1.cval.value := 0;
                END;
		ctp := ResultOfNegation(ctp);
                AddOp(cf1.cval,cval,cval,minus,ctp,err);
                IF err THEN ErrorLS(94) END;
              END;
            ELSIF IsCard(ctp) THEN
              IF op = minus THEN ErrorLS(121) END;
            ELSE ErrorLS(121);
            END; 
          END; 
        END;
        WHILE (sy >= plus) AND (sy <= orsy) DO 
          op := sy;
          GetSy;
          ConstTerm(cf1);
          IF cf.ctp = NIL THEN cf := cf1;
          ELSE
            WITH cf DO 
              IF Compatible(ctp,cf1.ctp) THEN
                CASE op OF
                | orsy: forms := Stset{bools};
                | plus,minus: forms := Stset{ints,cards,longints,longcards,
					     sets,bigsets,reals};
                END;
                IF FAmong(ctp,forms) THEN
		  ctp := ResultType(ctp, cf1.ctp);
                  AddOp(cval,cf1.cval,cval,op,ctp,err);
                  IF err THEN ErrorLS(94) END; 
                ELSE ErrorLS(140); 
                END; 
              ELSE ErrorLS(143); 
              END; 
            END; (* WITH *) 
          END; 
        END; (* WHILE *) 
      END ConstSimpleExpression;   

    BEGIN
      ConstSimpleExpression(cf);
      IF (sy >= eql) AND (sy <= insy) THEN
        res.value := ORD(FALSE); (* initial value *)
        op := sy;
        GetSy;
        ConstSimpleExpression(cf1);
        IF modrev THEN
           IF IsCharComp(cf.ctp) THEN
              CharCast(cf);
           END;
           IF IsCharComp(cf1.ctp) THEN
              CharCast(cf1);
           END;
        END;
        tp := cf.ctp;
        tp1 := cf1.ctp;    
        IF op = insy THEN
          IF FAmong(tp1,Stset{sets, bigsets}) THEN
            IF Compatible(tp,tp1^.basep) THEN
              (* RelOp must distinguish between sets and bigsets *)
              RelOp(cf.cval,cf1.cval,res,insy,tp1,err); 
              (* copy result of RelOp back to tp *)
              tp := tp1;
              IF err THEN ErrorLS(94) END;
            ELSE ErrorLS(142); 
            END;
          ELSE ErrorLS(149);
          END;
        ELSIF Compatible(tp,tp1) THEN
          CASE op OF
          | eql,neq: forms := Stset{bigsets,sets,pointers,reals} + scalars;
          | geq,leq: forms := Stset{bigsets,sets,reals} + scalars;
          | grt,lss: forms := Stset{reals} + scalars;
          END;
          IF FAmong(tp,forms) THEN 
            RelOp(cf.cval,cf1.cval,res,op,tp,err);
            IF err THEN ErrorLS(94) END;
          ELSE ErrorLS(140);
          END;
        ELSE ErrorLS(143) 
        END; 
        cf.ctp := boolptr; 
        cf.cval := res; 
      END; 
    END ConstExpression; 

    PROCEDURE Constant(VAR c: Constform);
    BEGIN
      ConstExpression(c);
    END Constant;

    PROCEDURE ConstantRange(VAR ctyp: Stptr; VAR cmin,cmax: CARDINAL);
      VAR c1,c2: CARDINAL; 
          tp : Stptr;
          c : Constform;
    BEGIN (* ConstantRange *)
      Constant(c);
      IF modrev AND IsCharComp(c.ctp) THEN
         CharCast(c);
      END;
      tp := c.ctp;
      c1 := c.cval.value; c2 := c1;
      IF sy = range THEN
        GetSy;
        IF FAmong(tp,scalars) THEN
          Constant(c);
          IF modrev AND IsCharComp(c.ctp) THEN
             CharCast(c);
          END;
          WITH c DO
            IF Compatible(tp,ctp) THEN
	      tp := ResultType(tp, ctp);
              c2 := cval.value;
              IF IsInt(tp) THEN
                IF INTEGER(c2) < INTEGER(c1) THEN c2 := c1; ErrorLS(95) END;
              ELSE
                IF c2 < c1 THEN c2 := c1; ErrorLS(95) END;
              END;
            ELSE ErrorLS(95)
            END;
          END;
        ELSE ErrorLS(96); 
          Constant(c);
        END;
      END;
      ctyp := tp;
      cmin := c1; cmax := c2; 
    END ConstantRange; 

    PROCEDURE ConstantVal(VAR fsp: Stptr; VAR fval: Constval); 
      VAR c: Constform; 
    BEGIN Constant(c);
      WITH c DO fsp := ctp; fval := cval END;
    END ConstantVal;

  END ConstDefinition;

  MODULE TypeDefinition;

    FROM Storage IMPORT ALLOCATE, DEALLOCATE;
    FROM MCBase IMPORT Idptr, Stptr, Idclass, Idset, Structform, Stset,
      Symbol, Varkind, Kindvar, Recpart, intptr, cardptr,
      maxcard, maxint, realsize, longrealsize, longintptr, longcardptr,
      longrealptr, realptr, longmaxint, longmaxcard, longword,
      stackdir, Direction;
    FROM MCP2IO IMPORT sy, spix, val, Error, ErrorLS, GetSy, PutGetSy;
    FROM MCP2Ident IMPORT Locate, EnterList, EnterId, EnterForward,
      SearchId, SearchInBlock, MsEntry;
    FROM MCP2Reference IMPORT Reference;
    FROM ConstDefinition IMPORT ConstantRange, Compatible;
    FROM MCTypes IMPORT IsCard, IsLong, IsInt;
    IMPORT symmod, nestlevel, oneword, doubleword, procmarkspace,
      scalars, charmax, BitsPerWord, FAmong, QualIdent, InitId,
      byteSize, onebyte, ToBeAligned, Align;

    EXPORT ActualTyp, ArrayStruct, ParamList;

    PROCEDURE CheckAndGetBounds(fsp: Stptr; VAR fmin,fmax: CARDINAL);
      VAR lmin,lmax: CARDINAL;
    BEGIN lmin := 0; lmax := 0;
      IF fsp <> NIL THEN
        WITH fsp^ DO
          CASE form OF
          | enums: lmax := cstnr;
          | bools: lmax := 1;
          | chars: lmax := charmax;
          | ints,cards,longints,longcards: ErrorLS(108);
          | subranges: lmin := min; lmax := max;
          ELSE ErrorLS(109);
          END; (* CASE *)
        END; (* WITH *) 
      END; (* IF *) 
      fmin := lmin; fmax := lmax;
    END CheckAndGetBounds;

    PROCEDURE BaseType(sp: Stptr) : Stptr;
    BEGIN
      IF sp = NIL THEN RETURN NIL END;
      IF sp^.form <> setoftypes THEN RETURN sp END;
      IF IsCard(sp) THEN
	IF IsLong(sp) THEN
	  RETURN longcardptr;
	ELSE
	  RETURN cardptr;
	END;
      ELSIF IsInt(sp) THEN
	IF IsLong(sp) THEN
	  RETURN longintptr;
	ELSE
	  RETURN intptr;
	END;
      ELSE (* IF IsReal(sp) THEN *)
	IF IsLong(sp) THEN
	  RETURN longrealptr;
	ELSE
	  RETURN realptr;
	END;
      END;
    END BaseType;

    PROCEDURE Subrange(cp: Stptr; c1,c2: CARDINAL): Stptr; 
      VAR sp : Stptr; 
    BEGIN 
      cp := BaseType(cp);
      NEW(sp,subranges);
      WITH sp^ DO 
        IF cp <> NIL THEN
          size := cp^.size;
        ELSE
          size := oneword;
        END;
        stidp := NIL; inlist := FALSE;
        form := subranges; scalp := cp; 
        min := c1; max := c2; 
      END;
      RETURN sp 
    END Subrange; 

    PROCEDURE ArrayStruct(imin,imax: CARDINAL; ind,el: Stptr): Stptr;
      VAR sp : Stptr;
          idiff : CARDINAL; (* index difference *)
          mc, mv : CARDINAL; (* help for multiplication *)
    BEGIN
      NEW(sp,arrays);
      WITH sp^ DO
        IF el <> NIL THEN
          (* assume that 'imin <= imax' in relation belonging to type *)
          IF FAmong(ind,Stset{ints}) THEN
            IF (INTEGER(imin) < 0) AND (INTEGER(imax) >= 0) THEN
              idiff := maxcard - imin + 1 + imax;
            ELSE (* both bounds with same sign *)
              idiff := imax - imin;
            END;
          ELSE
            idiff := imax - imin;
          END;
          (* number of elements is 'idiff + 1' *)
          IF FAmong(el, byteSize) THEN
            size := (idiff + 1) * onebyte;
          ELSE (* multiply with element size *)
            size := 0;
            IF idiff < maxcard THEN
              mc := idiff + 1;
              IF mc < el^.size THEN
		 mv := mc;
		 mc := el^.size
	      ELSE
		 mv := el^.size
	      END;
              WHILE (mv > 0) AND (size <= maxcard - mc) DO
		 DEC(mv); INC(size,mc)
	      END;
              IF mv > 0 THEN
		 ErrorLS(100)
	      END;
            ELSE ErrorLS(100);
            END;
          END;
          Align(size);
        ELSE size := 0;
        END;
        stidp := NIL; inlist := FALSE;
        form := arrays; dyn := FALSE; elp := el;
        IF FAmong(ind,Stset{subranges}) THEN ixp := ind
        ELSE ixp := Subrange(ind,imin,imax);
        END;
      END;
      RETURN sp;
    END ArrayStruct;

    PROCEDURE ParamList(withid: BOOLEAN; VAR paraddr: CARDINAL;
                        VAR procp: Stptr);
      VAR parh,part,parn : Idptr;
          ftp,sp : Stptr;
          rk : Idclass;
          vk : Varkind;
          cpar : BOOLEAN; (* parameters are copy parameters *)
          indac : BOOLEAN; (* indirect access to parameter value *)
          space : CARDINAL; (* space used for entry on stack *)

      PROCEDURE ParamId;
        VAR ip : Idptr;
      BEGIN
        NEW(ip,vars);
        InitId(ip,vars);
        WITH ip^ DO
          indaccess := FALSE;
          vkind := vk; vlevel := nestlevel;
          state := local; vlink := NIL;
        END;
        IF parh = NIL THEN parh := ip ELSE part^.vlink := ip END;
        part := ip;
        IF withid THEN EnterId(ip) END;
      END ParamId;

    BEGIN
      paraddr := 0; (* REV AFB 9/84 *)
      parh := NIL; part := NIL;
      rk := pures;
      IF sy = lparent THEN GetSy;
        WHILE sy <> rparent DO
          IF sy = varsy THEN GetSy; vk := varparam ELSE vk := valparam END;
          parn := part; (* mark new list of parameters *)
          IF withid THEN 
            WHILE sy <> colon DO ParamId; GetSy END;
            GetSy; (* colon *)
          ELSE ParamId;
          END;
          FormalTyp(ftp);
          IF vk = varparam THEN
             space := oneword;
          ELSIF FAmong(ftp, byteSize) THEN
	     (*
             space := onebyte;
	     *)
	     space := oneword;
          ELSE
             space := oneword;
          END;
          cpar := FALSE;
          IF FAmong(ftp,Stset{longreals,reals,records,arrays,bigsets}) THEN
            indac := TRUE;
            IF (ftp^.form = arrays) AND ftp^.dyn THEN
              space := doubleword;
              IF vk = valparam THEN
                cpar := TRUE;
              END;
            ELSIF vk = valparam THEN
              IF FAmong(ftp,Stset{longreals,reals}) THEN
                indac := FALSE;
		IF ftp^.form = reals THEN
		  space := realsize;
		ELSE
		  space := longrealsize;
		END;
              ELSIF FAmong(ftp, Stset{bigsets}) THEN
                indac := FALSE;
                space := ftp^.size;
              ELSE (* arrays, records *)
                cpar := TRUE;
              END;
            END;
          ELSE
            indac := vk <> valparam;
          END;
          (* allocation and type entry *)
          IF parn = NIL THEN parn := parh ELSE parn := parn^.vlink END;
          WHILE parn <> NIL DO
            WITH parn^ DO
              idtyp := ftp;
              IF cpar THEN vkind := copyparam END;
              indaccess := indac;
	      (*
              IF ToBeAligned(ftp) OR (space >= oneword) THEN
                 Align(paraddr);
              END;
	      (* on MC68020 a7 is decremented by 2 even on pushing *)
	      (* byte sized objects *)
	      IF (space = onebyte) AND ODD(paraddr) THEN
		 INC(paraddr);
	      END;
	      *)
	      IF stackdir = forward THEN
		vaddr := paraddr;
		INC(paraddr,space);
	      ELSE
		INC(paraddr,space);
		IF (ftp # NIL) AND (ftp^.form = arrays) AND ftp^.dyn THEN
		  vaddr := paraddr - oneword;
		ELSIF NOT indaccess AND FAmong(ftp, byteSize) THEN
		  vaddr := paraddr - oneword + onebyte;
		ELSE
		  vaddr := paraddr;
		END;
	      END;
              parn := vlink;
            END;
          END;
        END; (* WHILE *)
        GetSy; (* rparent*)
        IF sy = colon THEN GetSy;
          rk := funcs;
          SimpleTyp(ftp);
          IF NOT FAmong(ftp,Stset{enums,bools,chars,ints,cards,words,reals,
				  longints,longcards,longreals,bytes,
                                  subranges,pointers,sets,hides}) THEN
            ErrorLS(88) 
          END;
        END;
      END;
      IF rk = pures THEN NEW(sp,proctypes,pures)
      ELSE NEW(sp,proctypes,funcs)
      END;
      WITH sp^ DO
        size := oneword; stidp := NIL; inlist := FALSE;
        form := proctypes; fstparam := parh;
        Align(paraddr);
        INC(paraddr, procmarkspace);  (* REV AFB 9/84 *)
	IF nestlevel > 1 THEN
	  (* local procedure with static link which counts as parameter *)
	  INC(paraddr, oneword);
	END;
        parlength := paraddr;
        rkind := rk;
        IF rk = funcs THEN funcp := ftp END;
      END;
      procp := sp;
    END ParamList;
 
    PROCEDURE SimpleTyp(VAR trf: Stptr);
      VAR lsp: Stptr;
          opttype: Stptr;
          lip, tref: Idptr;
          cp: Stptr; 
          c1, c2 : CARDINAL;
          error: BOOLEAN; (* error message printed ??? *)
    BEGIN 
      error := FALSE;
      IF sy = lparent THEN GetSy; c1 := 0;
        NEW(lsp,enums); 
        WITH lsp^ DO form := enums; stidp := NIL; inlist := FALSE END;
        tref := NIL;
        WHILE sy <> rparent DO
          NEW(lip,consts); 
          InitId(lip,consts);
          WITH lip^ DO
            idtyp := lsp;
            IF symmod THEN GetSy; cvalue.value := val;
            ELSE cvalue.value := c1;
            END;
            (* (* only if TSIZE(enum type) = onebyte *)
            IF cvalue.value > 377B THEN
               IF NOT error THEN
                  Error(208);
                  error := TRUE;
               END;
               cvalue.value := 0;
            END;
            *)
          END;
          EnterList(tref,lip); 
          SearchInBlock(lip); (* new value for lip *) 
          IF (lip <> NIL) AND (lip^.klass <> unknown) THEN Error(72) END; 
          GetSy;
          INC(c1);
        END;
        WITH lsp^ DO
          size := oneword;
          fcstp := tref; cstnr := c1 - 1; (* c1 is number of elements *)
        END;
        MsEntry(tref);
        GetSy;
      ELSIF (sy = ident) OR (sy = lbrack) THEN
        IF sy = ident THEN
          QualIdent(Idset{types},73,lip); 
          IF lip = NIL THEN lsp := NIL;
          ELSE
            lsp := lip^.idtyp;
            IF lsp = NIL THEN ErrorLS(74) END;
          END;
          opttype := lsp;
        ELSE
          opttype := NIL;
        END;
        IF sy = lbrack THEN
          GetSy;
          ConstantRange(cp,c1,c2);
          IF opttype <> NIL THEN
             IF NOT Compatible(opttype, cp) THEN
                ErrorLS(128);
             ELSE
                cp := opttype;
             END;
          END;
	  (* (* will be done in Subrange *)
          IF IsCard(cp) THEN
             cp := cardptr; (* change to base type CARDINAL *)
          END;
	  *)
          lsp := Subrange(cp,c1,c2);
          GetSy; (* rbrack *)
        END;
      END;
      trf := lsp; 
    END SimpleTyp;

    PROCEDURE PointerTyp(VAR trf: Stptr); 
      VAR lip: Idptr;
          lsp,t1rf: Stptr;
    BEGIN
      NEW(lsp,pointers); trf := lsp;
      WITH lsp^ DO
        size := oneword; stidp := NIL; inlist := FALSE; 
        form := pointers; elemp := NIL; 
      END;
      IF sy = ident THEN
        (* search for module name first *)
        SearchId(lip);
        IF (lip <> NIL) AND (lip^.klass = mods) THEN
          QualIdent(Idset{types},73,lip);
          IF lip <> NIL THEN
            lsp^.elemp := lip^.idtyp;
            IF lip^.idtyp = NIL THEN ErrorLS(74) END;
          END;
        ELSE
          (* search for a local declared name *)
          SearchInBlock(lip);
          IF (lip = NIL) OR (lip^.klass = unknown) THEN
            EnterForward(lsp);
          ELSE
            IF lip^.klass = types THEN
              lsp^.elemp := lip^.idtyp;
              IF lip^.idtyp = NIL THEN Error(74) END;
            ELSE Error(73);
            END;
          END;
          GetSy;
          IF sy = period THEN (* overread incorrect qualident *)
            ErrorLS(105);
            WHILE sy = period DO GetSy; GetSy; END;
          END;
        END;
      ELSE ActualTyp(t1rf); lsp^.elemp := t1rf; END;
    END PointerTyp; 

    PROCEDURE RecordTyp(VAR trf: Stptr);
      VAR vrf: Stptr;   (* variant part *)
          frf: Idptr;   (* field list *)
          offset: CARDINAL;
          lsp: Stptr;


      PROCEDURE FieldList(VAR offs: CARDINAL; VAR vtabref: Stptr); 
        VAR offset, offse, maxoffset: CARDINAL;
            ttp: Stptr;
            tagref, svtrf, cxv: Stptr;
	    oldsubtag: Stptr;
            x, xh, xt: Idptr;
            ctp : Stptr;           (* type of new current case labels *)
            c1, c2: CARDINAL;      (* bound values of current case labels *)
            ltp : Stptr;           (* expected type of case labels *)
            lmin, lmax : CARDINAL; (* minimal and maximal value of case labels *)

        PROCEDURE EnterVariant(VAR vrf: Stptr; val: CARDINAL);
          VAR varref: Stptr;
        BEGIN NEW(varref,records,variantpart);
          WITH varref^ DO
            stidp := NIL; inlist := FALSE;
            form := records; rpart := variantpart; nxtvarp := NIL;
            subtagp := cxv; (* temporary link *)
            varval := val;
          END;
          cxv := varref; vrf := varref; 
        END EnterVariant; 

        PROCEDURE VariantField(val: CARDINAL); 
          VAR lsp: Stptr; goon: BOOLEAN;
        BEGIN goon := TRUE; 
          WITH tagref^ DO 
            IF fstvarp = NIL THEN EnterVariant(fstvarp,val); goon := FALSE; 
            ELSE lsp := fstvarp; END; 
          END;
          WHILE goon DO 
            WITH lsp^ DO
              IF varval = val THEN ErrorLS(93); goon := FALSE;
              ELSIF nxtvarp = NIL THEN EnterVariant(nxtvarp,val); goon := FALSE;
              ELSE lsp := nxtvarp END;
            END;
          END;
        END VariantField; 

        PROCEDURE IdentComplete(ip: Idptr); 
          VAR sz: CARDINAL; nextip: Idptr;
        BEGIN (* offset, ttp from FieldList *)
          IF ttp <> NIL THEN sz := ttp^.size ELSE sz := 0 END;
          WHILE ip <> NIL DO
            WITH ip^ DO
              nextip := nxtidp; idtyp := ttp;
              IF ToBeAligned(idtyp) THEN
                 Align(offset);
              END;
              fldaddr := offset;
              IF maxcard - sz >= offset THEN INC(offset, sz);
              ELSE (* record structure too large *)
                ErrorLS(100);
              END;
            END;
	    ip := nextip; (* bug fix - afb 2/88 *)
          END;
        END IdentComplete;

        PROCEDURE DisposeCase(fsp: Stptr);
          VAR lsp,lsp1: Stptr;
        BEGIN 
          IF fsp <> NIL THEN
            lsp := fsp^.fstvarp;
            WHILE lsp <> NIL DO 
              IF lsp^.nxtvarp = NIL THEN DisposeCase(lsp^.subtagp); 
              ELSIF lsp^.nxtvarp^.subtagp <> lsp^.subtagp THEN
                DisposeCase(lsp^.subtagp);
              END;
              lsp1 := lsp; lsp := lsp^.nxtvarp; 
              DISPOSE(lsp1,records,variantpart);
            END;
            IF fsp^.elsevarp <> NIL THEN DisposeCase(fsp^.elsevarp^.subtagp); 
              DISPOSE(fsp^.elsevarp,records,variantpart); 
            END;
            lsp := fsp; DISPOSE(lsp,records,tagfield);
          END;
        END DisposeCase;

        PROCEDURE LabelTypeAndBounds(sp: Stptr);
	  (* set the variables ltp, lmin, and lmax *)
        BEGIN (* LabelTypeAndBounds *)
          ltp := BaseType(sp);
          lmin := 0; lmax := 0;
          IF ltp <> NIL THEN
            WITH ltp^ DO
              CASE form OF
              |  enums     : lmax := cstnr;
              |  bools     : lmax := 1;
              |  chars     : lmax := charmax;
              |  ints      : lmin := CARDINAL(-maxint - 1);
                             lmax := maxint;
	      |  longints  : lmin := LONGCARD(-longmaxint - 1);
			     lmax := longmaxint;
              |  cards     : lmax := maxcard;
	      |  longcards : lmin := longmaxcard;
              |  subranges : lmin := min; lmax := max; ltp := scalp;
              ELSE (* no scalar type *)
                ErrorLS(109);
                ltp := NIL;
              END; (* CASE *)
            END; (* WITH *)
          END;
        END LabelTypeAndBounds;

      BEGIN (*FieldList*) 
        offset := offs; tagref := NIL;
        DisposeCase(vtabref); 
        IF sy = casesy THEN GetSy;
          xh := NIL; 
          IF sy = ident THEN (* explicit tagfield *)
            NEW(xh,fields);
            InitId(xh,fields);
            EnterList(frf,xh); GetSy; 
          END;
          (* sy = colon ;  inserted by pass1 *) 
          GetSy;
          QualIdent(Idset{types},91,x); 
          IF x = NIL THEN ttp := NIL;
          ELSE ttp := x^.idtyp;
            IF ttp = NIL THEN ErrorLS(74) END;
          END;
          IF xh <> NIL THEN IdentComplete(xh) END;
          NEW(tagref,records,tagfield); 
          WITH tagref^ DO 
            stidp := NIL; inlist := FALSE;
            form := records;
            rpart := tagfield; fstvarp := NIL;
            tagtyp := ttp; elsevarp := NIL;
          END;
          LabelTypeAndBounds(ttp);
          Align(offset);
          maxoffset := offset; cxv := NIL;
          (* Modula-2 Revision: # case variants may be zero *)
          WHILE sy = ofsy DO GetSy; 
            REPEAT (*process variant label:*) 
              IF symmod THEN VariantField(val); GetSy;
              ELSE
                ConstantRange(ctp,c1,c2);
                IF ltp = NIL THEN LabelTypeAndBounds(ctp) END; 
		IF Compatible(ltp, ctp) THEN
		  IF FAmong(ltp, Stset{ints, longints}) THEN
		    IF (INTEGER(c1) < INTEGER(lmin)) OR
		       (INTEGER(c2) > INTEGER(lmax)) THEN
		      ErrorLS(110);
		    END;
		    VariantField(c1);
		    WHILE INTEGER(c1) < INTEGER(c2) DO
		      IF c1 = maxcard THEN c1 := 0 ELSE INC(c1) END;
		      VariantField(c1);
		    END;
		  ELSE
		    IF (c1 < lmin) OR (c2 > lmax) THEN ErrorLS(110) END;
		    VariantField(c1);
		    WHILE c1 < c2 DO
		      INC(c1);
		      VariantField(c1);
		    END;
		  END;
                ELSE ErrorLS(92); 
                END;
              END;
            UNTIL sy = colon; 
            (*process fields of variant:*)
            GetSy; offse := offset; svtrf := NIL; 
            WHILE (sy = ident) OR (sy = casesy) DO
              FieldList(offse,svtrf); 
            END;
            IF symmod THEN offse := val; GetSy END;
            Align(offse);
            WHILE cxv <> NIL DO (* enter size of variant in variantlabels *) 
                                (* cxv is set by procedure EnterVariant *)
                                (* field subtagp links variantlabels *)
              WITH cxv^ DO
                size := offse;
		oldsubtag := subtagp;
		subtagp := svtrf;
		cxv := oldsubtag; (* bug fix afb 2/88 *)
              END;
            END;
            IF offse > maxoffset THEN maxoffset := offse END; 
          END (*while sy = ofsy*);
          IF sy = elsesy THEN (*else variant*)
            GetSy; offse := offset; svtrf := NIL; 
            EnterVariant(tagref^.elsevarp,0);
            WHILE (sy=ident) OR (sy=casesy) DO FieldList(offse,svtrf) END;
            IF symmod THEN offse := val; GetSy END;
            Align(offse);
            WITH cxv^ DO size := offse; subtagp := svtrf END; 
            IF offse > maxoffset THEN maxoffset := offse END; 
          END;
          tagref^.size := maxoffset; offs := maxoffset; 
          GetSy;
        ELSIF (sy <> endsy) AND (sy <> elsesy) THEN
          (* revised Modula-2: FieldList may be empty *)
          xh := NIL; 
          WHILE sy <> colon DO
            NEW(x,fields); 
            InitId(x,fields);
            IF symmod THEN GetSy; x^.fldaddr := val;
            ELSE
              IF xh = NIL THEN xh := x ELSE xt^.nxtidp := x END;
              xt := x;
            END;
            EnterList(frf,x); GetSy; 
          END;
          GetSy; ActualTyp(ttp);
          IF symmod THEN
            WITH x^ DO
              idtyp := ttp;
            END;
          ELSE IdentComplete(xh)
          END;
          offs := offset;
        END;
        vtabref := tagref;
      END FieldList;

    BEGIN (*RecordTyp*) 
      offset := 0;
      frf := NIL; vrf := NIL;
      WHILE sy <> endsy DO FieldList(offset,vrf) END; 
      GetSy;
      IF symmod THEN offset := val; GetSy
      END; 
      NEW(lsp,records,fixedpart); 
      lsp^.size := offset; (* must be done here; name conflict ! *)
      WITH lsp^ DO
	Align(size); (* ARRAY [...] OF RECORD ... *)
        stidp := NIL; inlist := FALSE;
        form := records; rpart := fixedpart;
        fieldp := frf; tagp := vrf; 
      END;
      trf := lsp; 
    END RecordTyp;

    PROCEDURE ArrayTyp(VAR trf: Stptr); 
      VAR lsp,lsp1,t1rf: Stptr; lmin,lmax: CARDINAL; 
    BEGIN SimpleTyp(lsp); 
      IF sy = comma THEN GetSy END; (* revision in interpass file *)
      CheckAndGetBounds(lsp,lmin,lmax); 
      IF sy <> ofsy THEN ArrayTyp(t1rf) ELSE GetSy; ActualTyp(t1rf) END;
      trf := ArrayStruct(lmin,lmax,lsp,t1rf);
    END ArrayTyp; 

    PROCEDURE SetTyp(VAR trf: Stptr); 
      VAR lsp,lsp1: Stptr; lmin,lmax: CARDINAL;
    BEGIN SimpleTyp(lsp1);
      CheckAndGetBounds(lsp1,lmin,lmax);
      (* set range must be in [0..maxcard-1] *)
      IF (lmin > lmax) OR (lmax >= maxint) THEN
        ErrorLS(107);
      END;
      IF lmax < BitsPerWord THEN
        NEW(lsp,sets,sets);
        WITH lsp^ DO
          size := oneword; stidp := NIL; inlist := FALSE;
          form := sets; basep := lsp1; 
        END;
      ELSE
        NEW(lsp, bigsets, bigsets);
        WITH lsp^ DO
          stidp := NIL;
          inlist := FALSE;
          form := bigsets;
          basep := lsp1;
          low := lmin;
          high := lmax;
          IF low >= BitsPerWord THEN
            offset := low;
          ELSE
            offset := 0;
          END;
          size := (high - offset + BitsPerWord) DIV BitsPerWord * oneword;
        END;
      END;
      trf := lsp; 
    END SetTyp; 

    PROCEDURE ProcedureTyp(VAR trf: Stptr); 
      VAR dummysize : CARDINAL;
    BEGIN
      ParamList(FALSE,dummysize,trf);
    END ProcedureTyp; 

    PROCEDURE HiddenTyp(VAR trf: Stptr);
    BEGIN
      NEW(trf,hides);
      WITH trf^ DO
        size := oneword; form := hides;
        stidp := NIL; inlist := FALSE;
      END;
    END HiddenTyp;

    PROCEDURE ActualTyp(VAR trf: Stptr);
    BEGIN 
      IF sy = arraysy THEN GetSy; ArrayTyp(trf);
      ELSIF sy = recordsy THEN GetSy; RecordTyp(trf); 
      ELSIF sy = setsy THEN GetSy; SetTyp(trf); 
      ELSIF sy = pointersy THEN GetSy; PointerTyp(trf); 
      ELSIF sy = proceduresy THEN GetSy; ProcedureTyp(trf); 
      ELSIF sy = hidden THEN  GetSy; HiddenTyp(trf);
      ELSE SimpleTyp(trf) END;
    END ActualTyp;

    PROCEDURE FormalTyp(VAR trf: Stptr); 
      VAR lsp,elementp: Stptr; 
    BEGIN 
      IF sy = arraysy THEN GetSy; 
        SimpleTyp(elementp);
        NEW(lsp,arrays); 
        WITH lsp^ DO
          size := doubleword; stidp := NIL; inlist := FALSE;
          form := arrays; dyn := TRUE; 
          ixp := cardptr; elp := elementp; 
        END;
        trf := lsp; 
      ELSE
        SimpleTyp(trf); (* expect type identifier *)
      END;
    END FormalTyp;

  END TypeDefinition; 

  PROCEDURE Module(mp: Idptr; priority: CARDINAL;
                   VAR alladdr: CARDINAL; VAR varp: Idptr);
    VAR initindex : Initrange;
        priotp : Stptr;
        prioval : Constval;

    PROCEDURE ExportList; 
      VAR rf,x: Idptr;
          qualif : BOOLEAN;
    BEGIN
      (* qualif := sy = qualifiedsy; *)
      qualif := sy <> exportsy;
      rf := mp^.expp;
      IF (sy = qualifiedsy) OR (sy = exportsy) THEN GetSy;
        WHILE sy = ident DO 
          IF symmod THEN Locate(rf,x);
          ELSE x := NIL;
          END;
          IF x = NIL THEN
            NEW(x,unknown); 
            InitId(x,unknown);
            EnterList(rf,x);
            IF NOT qualif THEN
              (* check whether this identifier is *)
              (* already known in the environment *)
              SearchInBlock(x); (* new value for x *)
              IF (x <> NIL) AND (x^.klass <> unknown) THEN Error(75) END; 
            END;
          END;
          GetSy;
        END 
      END;
      mp^.expp := rf; mp^.qualexp := qualif; 
      (* generate inverse link for unknown elements in export-list *) 
      x := mp;
      WHILE rf <> NIL DO
        IF rf^.klass = unknown THEN rf^.nxtidp := x END;
        x := rf;
        rf := x^.link;
      END;
    END ExportList; 
  
    PROCEDURE TestExport; 
      VAR ip : Idptr; 
    BEGIN 
      ip := mp^.expp; 
      WHILE ip <> NIL DO
        IF ip^.klass = unknown THEN ErrorLS(101) END; 
        ip := ip^.link
      END 
    END TestExport; 

    PROCEDURE EnterExport(ip: Idptr); 
      VAR lip : Idptr;
    BEGIN (* enter exportlist of module in mslist of environment *) 
      IF NOT ip^.qualexp THEN
        ip := ip^.expp;
        MsEntry(ip);
        WHILE ip <> NIL DO 
          lip := ip;
          IF lip^.klass = indrct THEN lip := lip^.nxtidp END;
          WITH lip^ DO
            IF klass = mods THEN EnterExport(lip);
            ELSIF (klass = types) AND (idtyp <> NIL) THEN 
              WITH idtyp^ DO
                IF form = enums THEN MsEntry(fcstp) END;
              END;
            END;
          END;
          ip := ip^.link;
        END;
      END;
    END EnterExport; 

    PROCEDURE ImportList;
      (* analyse import list of a module *)
      VAR ip,ep : Idptr;  
          frommod : BOOLEAN; 
          oldp: Idptr; (* rev afb *)
    BEGIN 
      NewImpList(mp^.impp); 
      WHILE (sy = importsy) OR (sy = fromsy) DO
        frommod := sy = fromsy; 
        IF frommod THEN 
          GetSy; 
          SearchId(ip); 
          IF (ip = NIL) OR (ip^.klass <> mods) THEN (* skip this list *) 
            PutSy(fromsy); 
            WHILE sy = ident DO PutGetSy END; 
          ELSE 
            ep := ip^.expp;
            GetSy; 
          END; 
        ELSE PutGetSy; (* importsy *)
        END; 
        WHILE sy = ident DO (* identifier skipped if module not found *) 
          IF frommod THEN
	     ExportSearch(ep,ip)
	  ELSE
	     IF impl THEN
	        Locate(oldlist, oldp);
	     ELSE
		oldp := NIL;
	     END;
	     IF oldp = NIL THEN
		SearchId(ip);
	     ELSE
		(* ip from oldlist will be disposed in ProcFuncDecl !! -afb *)
		ip := NIL;
	     END;
	  END; 
          IF ip = NIL THEN 
            IF frommod THEN Error(71); GetSy ELSE PutGetSy END;  
          ELSE 
            EnterImpList(ip); 
            GetSy; 
          END; 
        END; (* while *)  
      END; (* while *) 
      TermImpList(mp^.impp); 
    END ImportList; 

    PROCEDURE Block(VAR alladdr: CARDINAL; VAR varp: Idptr;
                    moduleblock: BOOLEAN);

      PROCEDURE DeleteOld(VAR ip: Idptr);
        (* delete old entry of implemented identifier *)
        VAR lip: Idptr; 
            pp1,pp2 : Idptr; 
      BEGIN (* assume ip <> NIL *)
        IF ip = oldlist THEN oldlist := ip^.link;   
        ELSE  
          lip := oldlist; 
          WHILE lip^.link <> ip DO lip := lip^.link END;
          lip^.link := ip^.link; 
        END; 
        IF oldlist = NIL THEN impl := FALSE END;
        CASE ip^.klass OF 
        | types: DISPOSE(ip,types); 
        | pures,funcs: (* delete also parameter and structure entry *) 
            WITH ip^ DO 
              pp1 := idtyp^.fstparam; 
              WHILE pp1 <> NIL DO  
                pp2 := pp1; 
                pp1 := pp2^.vlink;
                DISPOSE(pp2,vars); 
              END;
              IF klass = pures THEN DISPOSE(idtyp,proctypes,pures) 
              ELSE DISPOSE(idtyp,proctypes,funcs) 
              END; 
            END; 
            DISPOSE(ip,pures,FALSE,pures); 
        END; (* case *)   
      END DeleteOld;

      PROCEDURE ConstDeclaration; 
        VAR lip: Idptr;
      BEGIN 
        WHILE sy = ident DO 
          IF symmod THEN SymModSearch(lip) ELSE lip := NIL END;
          IF lip = NIL THEN
            NEW(lip,consts);
            InitId(lip,consts);
            IF defmod AND modrev THEN (* implicit export *)
              EnterExportId(lip);
            ELSE
              EnterId(lip); 
            END;
            GetSy;
            WITH lip^ DO ConstantVal(idtyp,cvalue) END;
          ELSE GetSy; SkipConstant;
          END;
        END;
      END ConstDeclaration; 

      PROCEDURE TypDeclaration; 
        VAR lip: Idptr;
            trf: Stptr; 
            oldp : Idptr;
      BEGIN 
        WHILE sy = ident DO 
          IF symmod THEN SymModSearch(lip) ELSE lip := NIL END;
          IF lip = NIL THEN
            oldp := NIL;
            IF impl AND (nestlevel = 0) AND GlobalKnown(spix) THEN
              (* implementation possible *)
              Locate(oldlist,oldp);
            END;
            NEW(lip,types); 
            InitId(lip,types);
            IF defmod AND modrev THEN
              EnterExportId(lip);
            ELSE
              EnterId(lip); 
            END;
            GetSy; ActualTyp(trf);
            IF (trf <> NIL) AND (trf^.stidp = NIL) THEN
              trf^.stidp := lip;
            END;
            lip^.idtyp := trf; 
            IF (oldp <> NIL) AND (oldp^.klass = types) THEN
              (* implementation of hidden type *)
              WITH oldp^.idtyp^ DO (* replace hidden structure *)
                form := opens;
                openstruc := trf;
              END;
	      IF oneword = longword THEN
		IF NOT FAmong(trf,Stset{ints,cards,words,pointers,
					longints,longcards,
					sets,hides}) THEN
		  ErrorLS(82)
		END;
	      ELSE
		IF NOT FAmong(trf,Stset{ints,cards,words,pointers,
					sets,hides}) THEN
		  ErrorLS(82)
		END;
	      END;
              DeleteOld(oldp);
            END;
            Reference(lip);
          ELSE GetSy; SkipType;
          END;
        END;
      END TypDeclaration; 

      PROCEDURE VarDeclaration(VAR vhead: Idptr);
        VAR v, vn, vt : Idptr;
            trf : Stptr;
            space : CARDINAL; (* space for allocation *)
            decl : BOOLEAN; (* identifier is new declared *)
            indac : BOOLEAN; (* indirect access to variable *)
            absval : Constval;
      BEGIN      
        IF vhead = NIL THEN vt := NIL
        ELSE (* search last entry *)
          vn := vhead;
          WHILE vn <> NIL DO vt := vn; vn := vn^.vlink END;
        END;
        WHILE sy = ident DO
          vn := vt; (* mark for new declared list of variables *)
          WHILE sy <> colon DO
            IF symmod THEN SymModSearch(v) ELSE v := NIL END;
            IF v = NIL THEN
              decl := TRUE;
              NEW(v,vars);
              InitId(v,vars);
              WITH v^ DO
                indaccess := FALSE;
                vkind := noparam;
                vaddr := maxcard; vlevel := nestlevel;
                IF vlevel = 0 THEN state := global;
                ELSE state := local;
                END;
                vlink := NIL;
              END;
              IF vhead = NIL THEN vhead := v ELSE vt^.vlink := v END;
              vt := v;
              IF defmod AND modrev THEN
                EnterExportId(v);
              ELSE
                EnterId(v);
              END;
            ELSE decl := FALSE
            END;
            GetSy;
            IF symmod THEN
              IF sy = lbrack THEN
                GetSy; (* lbrack *)
                IF decl THEN
                  WITH v^ DO
                    vaddr := val; state := absolute; vlevel := 0;
                  END;
                END;
                GetSy; (* cardcon *)
                GetSy; (* rbrack *)
              ELSE
                IF decl THEN
                  WITH v^ DO
                    vaddr := val; state := separate; vlevel := 0;
                  END;
                END;
                GetSy; (* cardcon *)
              END;
            ELSIF sy = lbrack THEN
              GetSy; (* lbrack *)
              ConstantVal(trf,absval);
              IF decl THEN
                WITH v^ DO
                  IF FAmong(trf,Stset{cards}) THEN
                    vaddr := absval.value;
                  ELSE ErrorLS(78); vaddr := 0;
                  END;
                  state := absolute;
                  vlevel := 0;
                END;
              END;
              GetSy; (* rbrack *)
            END;
          END; (* WHILE *)
          GetSy; (* colon *)
          IF decl THEN
            ActualTyp(trf);
            space := oneword; indac := FALSE;
            IF trf <> NIL THEN
              space := trf^.size; (* especially for reals *)
            END;
            (* allocation and type entry *)
            IF vn = NIL THEN vn := vhead ELSE vn := vn^.vlink END;
            WHILE vn <> NIL DO
              WITH vn^ DO
                idtyp := trf;
                IF (state = local) OR (state = global) THEN
                  IF ToBeAligned(idtyp) THEN
                     Align(alladdr);
                  END;
		  IF (stackdir = forward) OR (state = global) THEN
		    vaddr := alladdr; INC(alladdr,space);
		  ELSE
		    INC(alladdr,space); vaddr := alladdr;
		  END;
                END;
                IF state <> absolute THEN indaccess := indac END;
                Reference(vn);
                vn := vlink;
              END;
            END; 
          ELSE SkipType;
          END;
        END;
      END VarDeclaration;

      PROCEDURE ProcFuncDecl;
        VAR localaddr : CARDINAL;
            localvar : Idptr; (* list of local variables *)
            xb,oldp : Idptr;
            ip: Idptr;

        PROCEDURE CompProc(oproc,nproc: Stptr);
          (* compare old procedure from definition module with *)
          (* new declared procedure in implementation module   *)
          VAR op,np : Idptr; (* parameters *) 
              os,ns : Stptr; (* structures *) 
              comp : BOOLEAN; 
   
          PROCEDURE Equivalent(os,ns: Stptr): BOOLEAN;  
          BEGIN 
            RETURN
              (os = ns) OR
              (os<>NIL) AND (os^.form=opens) AND (os^.openstruc=ns);
          END Equivalent; 
  
          PROCEDURE DynArr(sp: Stptr): BOOLEAN; 
          BEGIN 
            RETURN (sp <> NIL) AND (sp^.form = arrays) AND sp^.dyn;
          END DynArr; 

        BEGIN (* CompProc *)
          comp := oproc^.rkind = nproc^.rkind;
          op := oproc^.fstparam; np := nproc^.fstparam;
          WHILE comp AND (op <> np) DO
            IF (op=NIL) OR (np=NIL) OR (op^.vkind<>np^.vkind) THEN
              comp := FALSE 
            ELSE 
              os := op^.idtyp; ns := np^.idtyp; 
              comp := Equivalent(os,ns) OR
                      DynArr(os) AND DynArr(ns) AND
                      Equivalent(os^.elp,ns^.elp); 
              op := op^.vlink; 
              np := np^.vlink; 
            END; 
          END; 
          IF comp AND (oproc^.rkind = funcs) THEN 
            comp := Equivalent(oproc^.funcp,nproc^.funcp) 
          END; 
          IF NOT comp THEN ErrorLS(83) END;
        END CompProc; 

      BEGIN (* ProcFuncDecl *)
        IF symmod THEN SymModSearch(xb) ELSE xb := NIL END;
        IF xb = NIL THEN
          oldp := NIL;
          IF impl AND (nestlevel = 0) AND GlobalKnown(spix) THEN
            (* implementation possible *)
            Locate(oldlist,oldp);
          END;
          localaddr := 0; localvar := NIL;
          NEW(xb,pures,FALSE,pures); (* = NEW(xb,funcs,FALSE,funcs) *)
          InitId(xb,pures);
          IF defmod AND modrev THEN
            EnterExportId(xb);
          ELSE
            EnterId(xb);
          END;
          GetSy;
          INC(nestlevel);
          WITH xb^ DO
            locp := NIL;
            msp := NIL;
            plev := nestlevel;
            isstandard := FALSE; (* initialisation *)
            IF symmod THEN procnum := val; GetSy; GetSy; (* symbolic *)
            ELSIF oldp <> NIL THEN procnum := oldp^.procnum; (* impl *)
            ELSE procnum := proccount; INC(proccount);
            END;
            priolev := priority;
            externalaccess := (oldp <> NIL) AND oldp^.externalaccess;
          END;
          MarkScope(xb);
          ParamList(NOT symmod,localaddr,xb^.idtyp);
          (* Reference uses xb^.idtyp^.parlength *)
          Reference(xb);
          IF NOT symmod THEN
             ip := xb^.idtyp^.fstparam;
             WHILE ip <> NIL DO
                Reference(ip);
                ip := ip^.vlink;
             END;
          END;
          WITH xb^ DO
            idtyp^.stidp := xb; (* enter identifier reference *)
            klass := idtyp^.rkind;
          END;
          IF (oldp <> NIL) AND (oldp^.klass IN Idset{pures,funcs}) THEN
            (* implementation of procedure from definition module *)
            CompProc(oldp^.idtyp,xb^.idtyp);
            DeleteOld(oldp);
          END;
          IF NOT (symmod OR defmod) THEN (* block expected *)
            PutSy(proceduresy); PutWord(xb);
            Block(localaddr,localvar,FALSE); 
          END;
          ReleaseScope(xb);
          WITH xb^ DO
            varlength := localaddr;
            Align(varlength);
            locvarp := localvar;
          END;
          EndReference(xb);
          DEC(nestlevel);
        ELSE
          GetSy; (* ident *)
          GetSy; (* cardcon = Procedure number *)
          SkipType;
        END;
      END ProcFuncDecl;

    BEGIN (* Block *) 
      MarkInitBlock;
      REPEAT
        IF sy = varsy THEN GetSy; VarDeclaration(varp); 
        ELSIF sy = proceduresy THEN GetSy; ProcFuncDecl;
        ELSIF sy = modulesy THEN
          GetSy; ModuleDeclaration(priority,alladdr,varp);
        ELSIF sy = typesy THEN GetSy; TypDeclaration; 
        ELSIF sy = constsy THEN GetSy; ConstDeclaration; 
        END 
      UNTIL (sy = beginsy) OR (sy = endblock);
      IF (sy = beginsy) OR MustInit() THEN
        IF moduleblock THEN
          ToInitModule(initindex);
        END; 
        IF sy = beginsy THEN PutGetSy ELSE PutSy(beginsy) END;
        InitModules;
        (* skip statements *)
        WHILE sy <> endblock DO PutGetSy END; 
        IF moduleblock THEN PutSy(endblock) END;
      END;
      PutGetSy; (* endblock *)
      ReleaseInitBlock;
    END Block;

  BEGIN (* Module *)   
    PutSy(modulesy); PutWord(mp); 
    IF sy = lbrack THEN (* priority specified *)   
      GetSy; (* lbrack *)
      ConstantVal(priotp,prioval);
      IF IsCard(priotp) AND (prioval.value <= maxprio) AND
         ((priority = noprio) OR (priority <= prioval.value))
      THEN
        priority := prioval.value;
      ELSE
        ErrorLS(80);
      END;
      GetSy; (* rbrack *)
    END;
    EnterInitModule(mp,initindex);
    mp^.priolev := priority;
    ImportList;   
    ExportList; 
    MarkScope(mp);
    Block(alladdr,varp,TRUE); 
    TestExport; 
    ReleaseScope(mp);
    EnterExport(mp); 
  END Module; 

  PROCEDURE EnterMods(VAR ip: Idptr);
    (* initialisation and entry of a module *)
  BEGIN
    InitId(ip,mods);
    WITH ip^ DO
      isstandard := FALSE;
      procnum := proccount; INC(proccount);
      plev := nestlevel + 1;
      (* no parameters *)
      varlength := procmarkspace; (* for module initialisation *)
      priolev := noprio;
      externalaccess := FALSE;
      locp := NIL; msp := NIL; impp := NIL; expp := NIL;
      qualexp := FALSE; globalmodule := FALSE;
    END; 
    EnterId(ip);
  END EnterMods;
 
  PROCEDURE ModuleDeclaration(oldprio : CARDINAL;
                              VAR alladdr: CARDINAL; VAR varp: Idptr);
    (* declaration of local modules *)
    VAR ip : Idptr;
  BEGIN  
    NEW(ip,mods,FALSE,mods,FALSE);
    EnterMods(ip);
    GetSy; (* identifier *)
    Reference(ip);
    Module(ip,oldprio,alladdr,varp);
    EndReference(ip);
  END ModuleDeclaration;

  PROCEDURE StartDecl; 

    VAR globaladdr : CARDINAL;
        ip : Idptr;
        modcount : CARDINAL;
        modkey : Keyarr;
        ix : CARDINAL;

    PROCEDURE InitImplementation(VAR listp: Idptr; exp: BOOLEAN);
      (* initialisation of an implementation module *)   
      VAR ip1, ip2 : Idptr;
          ndp : Idptr; (* identifier to be new declared *)
          newdecl : BOOLEAN; 

    BEGIN   
      ip1 := listp; ip2 := NIL; 
      WHILE ip1 <> NIL DO 
        newdecl := FALSE; 
        WITH ip1^ DO 
          CASE klass OF 
          | types: (* hidden declared types must be implemented *)
              newdecl := (idtyp^.form = hides) AND (idtyp^.stidp = ip1);
              Reference(ip1);
          | vars: (* search for maximal used allocation address *)  
              Reference(ip1);
              IF state <> absolute THEN
                state := global;
                IF vaddr >= globaladdr THEN
                  globaladdr := vaddr;
                  IF ToBeAligned(idtyp) THEN
                     Align(globaladdr);
                  END;
                  IF indaccess THEN INC(globaladdr,oneword);
                  ELSE INC(globaladdr,idtyp^.size);
                  END;
                END;  
              END;  
          | pures,funcs: (* implementation; maximal procedure number *)
              newdecl := TRUE; 
              IF procnum >= proccount THEN proccount := procnum + 1 END; 
              externalaccess := exp;
          ELSE (* nothing for consts *)
          END; (* case *)     
        END; (* with *)   
        IF newdecl THEN 
          ndp := ip1;  
          IF exp THEN (* replace by unknown identifier in exportlist *) 
            NEW(ip1,unknown); 
            WITH ip1^ DO 
              name := ndp^.name; klass := unknown;
              link := ndp^.link; (* nxtidp is set in procedure ExportList *)
              globmodp := mainmodp;
            END; (* with *) 
            IF ip2 = NIL THEN listp := ip1 ELSE ip2^.link := ip1 END; 
          ELSE (* delete in local list *)  
            IF ip2=NIL THEN listp := ip1^.link;
            ELSE ip2^.link := ip1^.link;
            END;  
            ip1 := ip2; 
          END; 
          (* enter identifier for implementation in separate list *)
          EnterList(oldlist,ndp);                                          
        END;
        ip2 := ip1;
        IF ip1 = NIL THEN ip1 := listp ELSE ip1 := ip1^.link END;
      END; (* while *)   
    END InitImplementation;

    PROCEDURE EnterGlobMods(VAR ip: Idptr);
      (* complete global module entry *)
      VAR ch : CHAR;
          pos : CARDINAL;
	  length: CARDINAL;
    BEGIN
      INC(modcount);
      WITH ip^ DO
        globalmodule := TRUE;
        externalaccess := TRUE; (* call always from environment *)
        modulekey := modkey;
        globvarp := NIL;
        modnum := modcount;
        (* copy identifier *)
        AsciiSetPos(name);
        pos := 0;
        AsciiRead(ch);
	length := modnamlength;
        WHILE (ch <> ' ') AND (pos < length) DO
	  identifier[pos] := ch;
          INC(pos);
          AsciiRead(ch);
        END;
        (* fill with 0C *)
        WHILE pos < length DO
	  identifier[pos] := 0C;
          INC(pos);
        END;
      END; 
    END EnterGlobMods;
 
  BEGIN (* StartDecl *)
    nestlevel := 0;
    modcount := 0; (* initialisation *)
    root^.locp := sysmodp; (* enter link to system module *)
    spix := sysmodp^.name;
    EnterId(sysmodp); (* module SYSTEM *)
    GetSy;
    WHILE sy <> eop DO
      ip := NIL;
      impl := FALSE;
      globaladdr := 0;
      proccount := 0; (* 0 for initialisation part of global module *)
      symmod := sy = symbolsy;
      defmod := sy = definitionsy;
      impl := sy = implementationsy;
      IF NOT (defmod OR symmod) THEN InitRef END;
      GetSy;
      IF impl THEN (* implementation module *)
        SymModSearch(ip);
        oldlist := NIL;
        IF ip = NIL THEN
          Error(81);
          FOR ix := 0 TO 2 DO modkey[ix] := 0 END;
        ELSE
          mainmodp := ip;
          Reference(ip);
          proccount := 1; (* at least module procedure is entered *)
          InitImplementation(ip^.expp,TRUE);
          InitImplementation(ip^.locp,FALSE);
        END;
        impl := oldlist <> NIL; (* objects to implement *)
      ELSIF symmod THEN (* symbolic module *)
        (* key to compilation version *)
        FOR ix := 0 TO 2 DO modkey[ix] := val; GetSy END;
        SymModSearch(ip);
        IF ip <> NIL THEN
          mainmodp := ip;
        END;   
      ELSE (* defmod or module *)
        GetModuleKey(modkey);
        IF defmod THEN DefModStatus END;
      END;
      IF ip = NIL THEN (* generate new entry *)
        NEW(ip,mods,FALSE,mods,TRUE);
        mainmodp := ip;
        EnterMods(ip);
        EnterGlobMods(ip);
        Reference(ip); (* no effect for defmod or symmod *)
      END;
      GetSy; (* ident *)
      IF defmod OR symmod THEN StopOutput END;
      ResetModuleInit;
      Module(ip,noprio,globaladdr,ip^.globvarp);
      IF defmod OR symmod THEN
        RestartOutput;
      ELSE
        IF impl THEN ErrorLS(84) END; (* some implementations missing *)
        EndReference(ip);
        TermRef;
      END;
    END;
    globvarnext := globaladdr;
    procnumber := proccount;
  END StartDecl; 

BEGIN (* MCPass2 *)
  StartDecl; 
  TermInOut;
  EndStorage;
  IF ErrorsFound THEN
     Stop(1);
  END;
END MCPass2.
