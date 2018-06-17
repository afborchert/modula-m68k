(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)

MODULE MCPass3;     (* LG + AFB *)
(* $T+ *)
   IMPORT Storage, MCBase, MCP3IO, MCP3Ident, MCOperations, MCStop, MCTypes,
      MCBigSet;
   FROM MCBase IMPORT Idptr, Stptr, Idclass, Idset, Structform, Stset,
      Constval, Levrange, Symbol, intptr, noprio,
      BitsPerWord, oneword, doubleword, onebyte, procmarkspace, modrev,
      maxcard, maxint;
   FROM MCStop IMPORT Stop;
   FROM MCP3IO IMPORT sy, nptr, PutSy, PutWord, Error, ErrorLS, GetSy,
      PutGetSy, TermInOut;
   FROM MCP3Ident IMPORT FAmong, NewImpList, TermImpList, EnterImpList,
      DisposeImpList, SearchId, ExportSearch, MarkModScope, ReleaseModScope,
      MarkProcScope, ReleaseProcScope, MarkWithScope, ReleaseWithScope,
      FieldIndex, BodyMark, BodyScopes;
   FROM SYSTEM IMPORT TSIZE;
   FROM Storage IMPORT ALLOCATE, DEALLOCATE, EndStorage;
   FROM MCP3Public IMPORT ErrorsFound, MDDFlag;
   FROM MCTypes IMPORT IsInt;

   PROCEDURE QualIdent(klset: Idset; errnum: CARDINAL; VAR ip: Idptr);
   (* Search a possibly qualified identifier of indicated *)
   (* class. If the search fails then write the error     *)
   (* message ernum. The name spix is taken from MCP3IO.  *)
   BEGIN 
      SearchId(ip);
      GetSy;                            (* next symbol already read *)
      IF ip = NIL THEN 
         ErrorLS(errnum);
      ELSE 
         LOOP 
            IF ip^.klass <> mods THEN 
               IF NOT (ip^.klass IN klset) THEN 
                  ErrorLS(103);         (* identifier not of expected klass *)
                  ip := NIL;
               END;
               EXIT;
            ELSIF sy <> period THEN 
               Error(53);
               ip := NIL;
               EXIT;
            END;
            GetSy;
            ExportSearch(ip^.expp,ip);  (* new value for ip *)
            GetSy;                      (* next symbol already read *)
            IF ip = NIL THEN 
               ErrorLS(158);
               EXIT 
            END;
         END;                           (* LOOP *)
      END;
      IF ip = NIL THEN                  (* read all qualifications *)
         WHILE sy = period DO 
            GetSy;
            GetSy 
         END;
      END;
   END QualIdent;

   CONST 
      intcars      = Stset{ints,cards,longints,longcards};
      scalars      = Stset{enums,bools,chars,subranges} + intcars;
      typesettypes = Stset{reals,longreals,setoftypes} + intcars;
      charmax      = 255;                 (* maximal value for a character *)

   VAR 
      nestlevel : Levrange;             (* nesting level *)

   MODULE ExpressionSystem;

      FROM Storage IMPORT ALLOCATE, DEALLOCATE;
      FROM MCBase IMPORT noprio, Idptr, Stptr, Idclass, Idset, Structform,
         Stset, Varkind, Kindvar, charptr, boolptr, intptr, realptr, byteptr,
         cardptr, wordptr, bitsetptr, addrptr, processptr, procptr, strptrs,
         substptr, stringroot, Stringptr, Stpures, Stfuncs,
         Symbol, Constval, modrev, maxint, maxcard, onebyte, oneword,
         longmaxint, longmaxcard, modrev2;
      FROM MCBigSet IMPORT InitConstSet, ConstSetElement, TermConstSet;
      FROM MCP3IO IMPORT sy, val, length, spix, PutSy, PutWord, Savepos,
         InitSave, ResetSave, ReleaseSave, Error, ErrorLS, GetSy, PutGetSy,
	 typeset;
      FROM MCP3Ident IMPORT Locate, FAmong, SearchId, FieldIndex;
      FROM MCOperations IMPORT RelOp, AddOp, MulOp, NotOp;
      FROM MCTypes IMPORT GetType, ConstType, IsReal, IsInt, IsCard,
	 ResultOfNegation;
      IMPORT scalars, doubleword, nestlevel, QualIdent, MCBase, TSIZE,
         BitsPerWord, MCTypes, charmax, typesettypes, intcars;

      EXPORT Expression, TypeExpression, ExprSequence, ParamCheck, StProcCheck
         , ExprComp, AssignComp, AddressComp, Selector, PreSelector, Attribut,
         Attributmode, ConstantRange, ConstantVal, PutConst, InitAt, IsCharComp,
         CharCast;

      TYPE 
         Attributmode = (constm, varm, exprm);
         Attribut     = RECORD 
            mode : Attributmode;
            atp : Stptr;
            aval : Constval;
         END;

      VAR 
         stringtail : Stringptr;

      MODULE TypeHandling;

         FROM MCBase IMPORT Idptr, Stptr, Idclass, Structform, Stset, Varkind,
            charptr, intptr, cardptr, addrptr, modrev, oneword,
	    onebyte, wordptr, byteptr;
	 FROM MCTypes IMPORT TypeSetCompatible, TypeSetResult, IsReal;

         IMPORT Attribut, Attributmode, FAmong, BitsPerWord, typesettypes;

         EXPORT ExprComp, AssignComp, AddressComp, WordComp, OpenTest,
            IsString, CharCast, IsCharComp, IsChar, Compatible, ResultType,
            ByteComp;

	 PROCEDURE ResultType(sp1, sp2: Stptr) : Stptr;
	 BEGIN
	    IF (sp1 = NIL) OR (sp2 = NIL) THEN
	       RETURN NIL
	    ELSIF (sp1^.form IN typesettypes) AND
		  (sp2^.form IN typesettypes) THEN
	       RETURN TypeSetResult(sp1, sp2)
	    ELSE
	       RETURN sp1
	    END;
	 END ResultType;

         PROCEDURE OpenTest(VAR sp: Stptr);
	    (* test on open structure and change to actual structure *)
         BEGIN 
            WHILE (sp <> NIL) AND (sp^.form = opens) DO 
               sp := sp^.openstruc;
            END 
         END OpenTest;

         PROCEDURE IsString(s: Stptr): BOOLEAN;
            VAR 
               str: BOOLEAN;
         BEGIN                          (* string structure ecpected *)
            str := FALSE;
            IF s <> NIL THEN 
               WITH s^ DO 
                  IF (form=arrays) AND NOT dyn THEN 
                     str := (elp=charptr) AND (ixp^.scalp=cardptr) AND (ixp^.
                        min=0)
                  END 
               END                      (* WITH s^ *)
            END;
            RETURN str;
         END IsString;

	 PROCEDURE DynArr(sp: Stptr): BOOLEAN;
	 BEGIN 
	    RETURN (sp <> NIL) AND (sp^.form = arrays) AND sp^.dyn 
	 END DynArr;

         PROCEDURE ProcedureCheck(sp1,sp2: Stptr): BOOLEAN;
	    (* check type equality between two procedure structures *)
            VAR 
               p1,p2 : Idptr;
               s1,s2 : Stptr;
         BEGIN 
            IF sp1^.rkind <> sp2^.rkind THEN 
               RETURN FALSE 
            END;
            p1 := sp1^.fstparam;
            p2 := sp2^.fstparam;
            LOOP 
               IF p1 = p2 THEN 
                  EXIT 
               END;                     (* paramlist checked *)
               IF (p1 = NIL) OR (p2 = NIL) OR (p1^.vkind <> p2^.vkind) THEN 
                  RETURN FALSE 
               ELSE 
                  s1 := p1^.idtyp;
                  s2 := p2^.idtyp;
                  OpenTest(s1);
                  OpenTest(s2);
                  IF s1 <> s2 THEN 
                     IF DynArr(s1) AND DynArr(s2) THEN 
                        IF s1^.elp <> s2^.elp THEN 
                           RETURN FALSE 
                        END;
                     ELSE 
                        RETURN FALSE 
                     END;
                  END;
               END;
               p1 := p1^.vlink;
               p2 := p2^.vlink;
            END;                        (* LOOP *)
            (* exit loop only with equality *)
            IF sp1^.rkind = funcs THEN 
               RETURN sp1^.funcp = sp2^.funcp 
            ELSE 
               RETURN TRUE 
            END;
         END ProcedureCheck;

	 PROCEDURE Compatible(t1p, t2p: Stptr) : BOOLEAN;
	 BEGIN
	    IF DynArr(t1p) THEN (* afb 4/90 *)
	       RETURN FALSE
	    END;
            IF (t1p = t2p) THEN
               RETURN TRUE 
            END;
            WHILE (t1p <> NIL) AND (t1p^.form = subranges) DO
               t1p := t1p^.scalp 
            END;
            WHILE (t2p <> NIL) AND (t2p^.form = subranges) DO
               t2p := t2p^.scalp 
            END;
            IF (t1p = t2p) OR (t1p = NIL) OR (t2p = NIL) THEN 
               RETURN TRUE 
            END;
	    IF (t1p^.form IN typesettypes) AND (t2p^.form IN typesettypes) THEN
	       RETURN TypeSetCompatible(t1p, t2p);
	    END;
            RETURN FALSE;
	 END Compatible;

         PROCEDURE ExprComp(at1,at2 : Attribut) : BOOLEAN;
            VAR t1p, t2p: Stptr;
         BEGIN 
            t1p := at1.atp; t2p := at2.atp;
            IF Compatible(t1p, t2p) THEN RETURN TRUE END;
            IF (t1p^.form = pointers) AND (t2p^.form = pointers) AND
               ((at1.mode = constm) OR (at2.mode = constm)) THEN
               (* NIL compatible to each pointer *)
               RETURN TRUE
            END;
            (* following check isn't perfect; it allows        *)
            (*                                                 *)
            (*        TYPE t1 = PROCEDURE(INTEGER);            *)
            (*        TYPE t2 = PROCEDURE(INTEGER);            *)
            (*        VAR p1: t1; p2: t2;                      *)
            (*        ...                                      *)
            (*        p1 := (p2);  (* (p2) in exprm-Mode *)    *)
            IF ((at1.mode = varm) <> (at2.mode = varm)) AND
               (t1p^.form=proctypes) AND (t2p^.form=proctypes) THEN 
               RETURN ProcedureCheck(t1p,t2p)
            END;
            IF ((at1.mode = constm) OR (at2.mode = constm)) AND
               IsString(t1p) AND IsString(t2p) THEN 
                                        (* string compatibility *)
               RETURN ((at1.mode=varm) <> (t1p^.ixp^.max<t2p^.ixp^.max))
                     OR (t1p^.ixp^.max = t2p^.ixp^.max) OR (t1p^.size = 0)
                     OR (t2p^.size = 0);
            END;
            RETURN FALSE
         END ExprComp;

         PROCEDURE IsCharComp(at: Attribut) : BOOLEAN;
         BEGIN
            WITH at DO
               IF atp = NIL THEN RETURN FALSE END;
               RETURN IsChar(at) OR (mode = constm) AND IsString(atp) AND
                      (atp^.ixp^.max = 0);
            END;
         END IsCharComp;

         PROCEDURE IsChar(at: Attribut) : BOOLEAN;
         BEGIN
            WITH at DO
               IF atp = NIL THEN RETURN FALSE END;
               IF atp^.form = subranges THEN
                  RETURN atp^.scalp^.form = chars
               ELSE
                  RETURN atp^.form = chars
               END;
            END;
         END IsChar;

         (* change constant string with len 1 to character constant *)

         PROCEDURE CharCast(VAR at: Attribut);
            TYPE
               CharPtr = POINTER TO ARRAY[0..0] OF CHAR;
            VAR
               cp: CharPtr;
         BEGIN
            WITH at DO
               (* mode = constm *)
               IF NOT IsChar(at) THEN (* string *)
                  WITH aval DO
                     cp := CharPtr(svalue^.valentry);
                     value := ORD(cp^[0]);
                  END;
                  atp := charptr;
               END;
            END;
         END CharCast;

         PROCEDURE AssignComp(at1, at2: Attribut): BOOLEAN;
	    (* assignment compatibility *)
            CONST 
               incar = Stset{ints,cards,longints,longcards};
         BEGIN 
            RETURN ExprComp(at1,at2) OR
		   FAmong(at1.atp,incar) AND FAmong(at2.atp,incar) OR
		   modrev AND IsCharComp(at1) AND IsCharComp(at2) OR
		   modrev AND IsReal(at1.atp) AND IsReal(at2.atp)
         END AssignComp;

         PROCEDURE AddressComp(at1,at2: Attribut): BOOLEAN;
	    (* compatibility to type ADDRESS *)
            VAR 
               sp : Stptr;
         BEGIN 
            IF at1.atp = addrptr THEN 
               sp := at2.atp 
            ELSIF at2.atp = addrptr THEN 
               sp := at1.atp 
            ELSE 
               sp := NIL 
            END;
            RETURN FAmong(sp,Stset{pointers})
         END AddressComp;

         PROCEDURE WordComp(sp: Stptr): BOOLEAN;
	    (* compatibility to formal WORD parameter *)
         BEGIN 
	    (*
            RETURN NOT FAmong(sp,Stset{arrays,records,longreals,reals});
	    *)
	    RETURN sp^.size = wordptr^.size;
         END WordComp;

	 PROCEDURE ByteComp(sp: Stptr): BOOLEAN;
	    (* compatibility to formal BYTE parameter *)
	 BEGIN
	    RETURN sp^.size = byteptr^.size;
	 END ByteComp;

      END TypeHandling;

      PROCEDURE InitAt(VAR at : Attribut);
      BEGIN 
         WITH at DO 
            mode := varm;
            atp := NIL;
            aval.value := 0;
         END;
      END InitAt;

      PROCEDURE StringStruct(length: CARDINAL): Stptr;
	 (* get string structure entry *)
         VAR 
            sp : Stptr;
      BEGIN 
         IF (length > 20) OR (strptrs[length] = NIL) THEN 
         (* generate new entry *)
            NEW(sp,arrays);
            WITH sp^ DO 
	       (* align size *)
               size := length;
	       IF size MOD oneword <> 0 THEN
		  INC(size, oneword - size MOD oneword);
	       END;
               stidp := NIL;
               form := arrays;
               dyn := FALSE;
               elp := charptr;
               NEW(ixp,subranges);
               WITH ixp^ DO 
                  size := oneword;
                  stidp := NIL;
                  form := subranges;
                  scalp := cardptr;
                  min := 0;
                  max := length - 1;
               END;
            END;
            IF length <= 20 THEN 
               strptrs[length] := sp 
            END;
         ELSE 
            sp := strptrs[length];
         END;
         RETURN sp;
      END StringStruct;

      PROCEDURE AllocString(strval: Stringptr; length: CARDINAL);
      BEGIN 
         IF strval^.label = NIL THEN
            strval^.label := 0;
            (* enter link *)
            IF stringroot = NIL THEN 
               stringroot := strval;
            ELSE 
               stringtail^.slink := strval;
            END;
            stringtail := strval;
            stringtail^.slink := NIL;
         END;
      END AllocString;

      PROCEDURE PutConst(sp: Stptr; cval: Constval);
      BEGIN 
         PutSy(anycon);
         PutWord(sp);
         PutWord(cval.value);
         IF IsString(sp) THEN 
            AllocString(cval.svalue,sp^.ixp^.max + 1);
         END;
      END PutConst;

      PROCEDURE TypeExpression(VAR at : Attribut; forms: Stset);
	 (* expression with type in forms expected *)
      BEGIN 
         Expression(at);
         IF NOT FAmong(at.atp,forms) THEN 
            IF modrev AND IsCharComp(at) THEN
               CharCast(at);
               IF NOT FAmong(at.atp, forms) THEN
                  ErrorLS(121);
               END;
            ELSE
               ErrorLS(121)
            END;
         END;
      END TypeExpression;

      PROCEDURE VariableStandard(VAR at: Attribut; forms: Stset);
	 (* variable with type in forms expected *)
      BEGIN 
         Expression(at);
         IF at.mode = varm THEN 
            IF NOT FAmong(at.atp,forms) THEN 
               ErrorLS(121)
            END 
         ELSE 
            ErrorLS(122)
         END;
      END VariableStandard;

      PROCEDURE VariantAnalyse(tagref: Stptr; VAR sz: Constval);
	 (* analyse of procedure parameters for record variants *)
	 (* in standard procedures NEW, DISPOSE, TSIZE *)
         VAR 
            equal    : BOOLEAN;
            vxv,vtrf : Stptr;
            ttyp     : Stptr;
            tval     : Constval;
      BEGIN 
         vtrf := tagref;
         WHILE vtrf <> NIL DO 
            IF sy = rparent THEN 
               vtrf := NIL;
            ELSE 
               GetSy;                (* tags are not written in the il1 file *)
               ConstantVal(ttyp,tval);
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
                        sz.value := vxv^.size;
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
            GetSy;
            ConstantVal(ttyp,tval)
         END;
      END VariantAnalyse;

      PROCEDURE StProcCheck(ip: Idptr);
         VAR 
            ok     : BOOLEAN;
            i      : CARDINAL;
            at,at1 : Attribut;
            ltp    : Stptr;
            lval   : Constval;
            nam    : Stpures;
            subp   : Idptr;
      BEGIN 
         nam := ip^.pname;
         (* test on procedures to be sustituted *)
         subp := substptr;
         WHILE (subp <> NIL) AND (subp^.pname <> nam) DO 
            subp := subp^.link;
         END;
         IF subp <> NIL THEN            (* procedure must be substituted *)
            spix := subp^.name;         (* ATTENTION : spix is changed *)
            SearchId(ip);
            IF (ip <> NIL) AND (ip^.klass IN Idset{pures,vars}) THEN 
	       (* procedure found for substitution *)
	       (* initialisation of attributes for compatibility check *)
               at.atp := subp^.idtyp;
               at.mode := exprm;
               at1.atp := ip^.idtyp;
               at1.mode := varm;
               IF NOT ExprComp(at,at1) THEN 
                  ErrorLS(125)
               END;
            ELSE 
               ErrorLS(124);
            END;
         END;
         PutSy(namesy);
         PutWord(ip);
         PutGetSy;                      (* lparent *)
         i := 0;
         CASE nam OF 
           argvp:                       (* ARGV *)
               LOOP 
                  IF sy = comma THEN 
                     PutGetSy 
                  END;
                  IF sy = rparent THEN 
                     Error(127);
                     EXIT 
                  END;
                  INC(i);
                  CASE i OF 
                    1 : 
                        Expression(at);
                        WITH at DO 
                           IF mode <> varm THEN 
                              ErrorLS(122);
                           ELSE 
                              WITH atp^ DO 
                                 IF (atp = NIL) OR (form <> arrays) OR (elp <> charptr) THEN 
                                    ErrorLS(121);
                                 END;
                              END;
                           END;
                        END;
                  | 2 : 
                        TypeExpression(at1, intcars);
                        EXIT;
                  END;
               END;
         | incp,decp:                   (*INC,DEC*)
               IF sy = rparent THEN 
                  Error(127);
               ELSE 
                  VariableStandard(at,scalars);
                  IF sy <> rparent THEN 
                     PutGetSy;
                     TypeExpression(at1,intcars);
                  END;
               END;
         | disp,newp:                   (* NEW,DISPOSE *)
               IF sy = rparent THEN 
                  Error(127);
                  ltp := NIL;
               ELSE 
                  VariableStandard(at,Stset{pointers});
                  ltp := at.atp;
               END;
               IF FAmong(ltp,Stset{pointers}) THEN 
                  ltp := ltp^.elemp 
               END;
               IF ltp = NIL THEN 
                  lval.value := 0 
               ELSE 
                  WITH ltp^ DO 
                     lval.value := size;
                     IF form = records THEN 
                        VariantAnalyse(tagp,lval);
                     END;
                  END;                  (* WITH ltp^ *)
               END;
               PutSy(comma);
               PutConst(cardptr,lval);
               (* substituted procedures now with two        *)
               (* parameters on the il1-file: (pointer,size) *)
         | inlp,exlp:                   (* INCL,EXCL *)
               IF sy = rparent THEN 
                  Error(127);
               ELSE 
                  VariableStandard(at,Stset{sets,bigsets});
                  IF sy = rparent THEN 
                     Error(127);
                  ELSE 
                     PutGetSy;
                     TypeExpression(at1,scalars);
                     IF FAmong(at.atp,Stset{sets,bigsets}) THEN 
                        at.atp := at.atp^.basep;
                        IF NOT AssignComp(at,at1) THEN 
                           ErrorLS(128)
                        END;
                     END;
                  END;
               END;
         | nprp:                        (* NEWPROCESS *)
               LOOP 
                  IF sy = comma THEN 
                     PutGetSy 
                  END;
                  IF sy = rparent THEN 
                     Error(127);
                     EXIT 
                  END;
                  INC(i);
                  CASE i OF 
                    1: 
                        Expression(at);
			at1.atp := procptr;
                        at1.mode := varm;
                        IF NOT ExprComp(at,at1) THEN 
                           ErrorLS(127)
                        END;
                  | 2: 
			(* REV AFB 6/92: allow pointers, not only ADDRESS *)
                        TypeExpression(at,Stset{cards, longcards, pointers});
		  | 3:
                        TypeExpression(at,Stset{cards, longcards});
                  | 4: 
                        Expression(at);
                        WITH at DO
                           IF NOT ((mode = varm) AND
			      (NOT modrev2 AND (atp = processptr) OR
			       (atp = addrptr))) THEN 
                              ErrorLS(127);
                           END 
                        END;
                        EXIT;
                  END                   (* CASE *)
               END;
         | trsp:                        (* TRANSFER *)
               LOOP 
                  IF sy = comma THEN 
                     PutGetSy 
                  END;
                  IF sy = rparent THEN 
                     Error(127);
                     EXIT 
                  END;
                  INC(i);
		  Expression(at);
		  WITH at DO 
		     IF NOT ((mode = varm) AND
		        (NOT modrev2 AND (atp = processptr) OR
		        (atp = addrptr))) THEN 
			ErrorLS(127)
		     END 
		  END;
		  IF i = 2 THEN 
		     EXIT 
		  END;
               END; (* LOOP *)
         | halp:                        (* NO ACTIVITY *)
	       (* HALT *)
	 END;                (* CASE *)
         IF sy = rparent THEN 
            PutGetSy;
         ELSE 
            ErrorLS(127);
            ExprSequence;
         END;
      END StProcCheck;

      PROCEDURE TypFunction(VAR at : Attribut);
         CONST 
            forms = Stset{sets,bigsets,pointers} + scalars;
         VAR 
            lat : Attribut;
      BEGIN 
         PutGetSy;                      (* lparent *)
         IF sy = rparent THEN 
            Error(137);
         ELSE 
            Expression(lat);
            IF modrev AND IsChar(at) AND IsString(lat.atp) AND IsCharComp(lat) THEN
               CharCast(lat);
            END;
            IF (at.atp <> NIL) AND (lat.atp <> NIL) THEN 
               IF (at.atp^.size = lat.atp^.size) THEN 
                  IF (lat.mode = constm) AND FAmong(at.atp,forms) AND FAmong
                     (lat.atp,forms) THEN 
                     at.mode := constm;
                     at.aval := lat.aval;
                  ELSIF (lat.atp^.form = arrays) AND (lat.atp^.dyn) THEN 
                     ErrorLS(120);
                  END;
               ELSE 
                  at.atp := NIL;
                  ErrorLS(120);
               END;
            END;
         END;
         IF sy = rparent THEN 
            PutGetSy;
         ELSE 
            ErrorLS(127);
            ExprSequence;
         END;
      END TypFunction;

      PROCEDURE PreSelector(x: Idptr; VAR at: Attribut);
      BEGIN 
         WITH x^ DO 
            OpenTest(idtyp);
            WITH at DO 
               IF klass IN Idset{vars,fields} THEN 
                  mode := varm;
               ELSE 
                  mode := exprm;
               END;
               atp := idtyp;
               IF NOT (klass IN Idset{consts,types}) THEN 
                  IF klass = fields THEN 
                     PutSy(field);
                     PutWord(FieldIndex());
                     PutSy(period);
                  END;
                  PutSy(namesy);
                  PutWord(x);
               END;
            END                         (* WITH at *)
         END                            (* WITH x^ *)
      END PreSelector;

      PROCEDURE Selector(VAR at : Attribut);
         VAR 
            lat,indat : Attribut;
            x         : Idptr;
      BEGIN 
         REPEAT 
            IF sy = lbrack THEN 
               PutGetSy;
               Expression(lat);
               WITH at DO 
                  IF atp <> NIL THEN 
                     WITH atp^ DO 
                        IF form = arrays THEN 
                           OpenTest(ixp);
                           OpenTest(elp);
                           indat.atp := ixp;
                           indat.mode := varm;
                           IF NOT AssignComp(indat,lat) THEN 
                              ErrorLS(128)
                           END;
                           atp := elp 
                        ELSE 
                           ErrorLS(130)
                        END;
                     END;               (* WITH atp^ *)
                  END;
               END;                     (* WITH at *)
               PutGetSy;                (* rbrack *)
            ELSIF sy = period THEN 
               PutGetSy;
               WITH at DO 
                  IF atp <> NIL THEN 
                     WITH atp^ DO 
                        IF form <> records THEN 
                           Error(131)
                        ELSE 
                           Locate(fieldp,x);
                                      (* entries with klass = fields assumed *)
                           IF x <> NIL THEN 
                              PutSy(namesy);
                              PutWord(x);
                              OpenTest(x^.idtyp);
                              atp := x^.idtyp;
                           ELSE 
                              Error(73)
                           END;
                        END 
                     END;               (* WITH atp^ *)
                  END;
               END;                     (* WITH at *)
               GetSy;
            ELSIF sy = arrow THEN 
               WITH at DO 
                  IF atp = addrptr THEN 
                     atp := wordptr 
                  ELSIF atp <> NIL THEN 
                     WITH atp^ DO 
                        IF form <> pointers THEN 
                           Error(146)
                        ELSE 
                           OpenTest(elemp);
                           atp := elemp 
                        END 
                     END;               (* WITH typptr^ *)
                  END;
               END;                     (* WITH at *)
               PutGetSy;
            END;
         UNTIL (sy <> lbrack) AND (sy <> period) AND (sy <> arrow);
      END Selector;

      PROCEDURE ParamCheck(fstp : Idptr);
         VAR 
            at1,at2 : Attribut;
            ls      :Idptr;
            comp    : BOOLEAN;

         PROCEDURE DynArrCheck(dynarelp,aptr: Stptr);
	    (* compare element types *)
         BEGIN 
            IF (dynarelp <> wordptr) AND (dynarelp <> byteptr) THEN 
               IF aptr^.form = arrays THEN 
                  IF aptr^.elp <> dynarelp THEN 
                     ErrorLS(135)
                  END;
               ELSE 
                  ErrorLS(128);
               END;
            END;
         END DynArrCheck;

      BEGIN                             (* ParamCheck *)
         ls := fstp;
         WHILE sy <> rparent DO 
            Expression(at1);
            IF ls = NIL THEN 
               Error(132)
            ELSE 
               WITH at1 DO 
                  IF atp <> NIL THEN 
                     WITH ls^ DO 
                        IF (vkind = varparam) AND (mode <> varm) THEN 
                           ErrorLS(134);
                        END;
                        OpenTest(idtyp);
                        IF (idtyp<>NIL) AND (idtyp^.form=arrays) AND idtyp^.
                           dyn THEN 
                           DynArrCheck(idtyp^.elp,atp);
                        ELSIF idtyp = wordptr THEN 
                           IF NOT WordComp(atp) THEN 
                              ErrorLS(150);
                           END;
			ELSIF idtyp = byteptr THEN
			   IF NOT ByteComp(atp) THEN
			      ErrorLS(162);
			   END;
                        ELSE 
                           at2.atp := idtyp;
                           at2.mode := varm;
                           IF vkind=varparam THEN 
                              IF modrev THEN
                                 comp := idtyp = atp;
                              ELSE
                                 comp := ExprComp(at2,at1)
                              END;
                           ELSE 
                              comp := AssignComp(at2,at1)
                           END;
                           IF NOT (comp OR AddressComp(at1,at2)) THEN 
                              IF modrev AND (vkind = varparam) AND
                                 ExprComp(at2, at1) THEN
                                 ErrorLS(304);
                              ELSE
                                 ErrorLS(128);
                              END;
                           END 
                        END;
                     END;               (* WITH ls^ *)
                  END;
                  ls := ls^.vlink;
               END;                     (* WITH at1 *)
            END;
            IF sy = comma THEN 
               PutGetSy 
            END;
         END;                           (* WHILE *)
         PutGetSy;
         IF ls <> NIL THEN 
            ErrorLS(137)
         END;
      END ParamCheck;

      PROCEDURE ExprSequence;
	 (* Terminate a sequence of Expressions *)
         VAR 
            at: Attribut;
      BEGIN 
         WHILE sy <> rparent DO 
            IF sy = comma THEN 
               PutGetSy 
            END;
            Expression(at);
         END;
         PutGetSy;                      (* rparent *)
      END ExprSequence;

      PROCEDURE Expression(VAR at : Attribut);
         VAR 
            lat        : Attribut;
            tpat,tplat : Stptr;
            op         : Symbol;
            forms      : Stset;
            err        : BOOLEAN;
            save       : Savepos;

         PROCEDURE SimpleExpression(VAR at : Attribut);
            VAR 
               lat  : Attribut;
               op   : Symbol;
               sign : BOOLEAN;
               save : Savepos;

            PROCEDURE Term(VAR at : Attribut);
               VAR 
                  lat  : Attribut;
                  op   : Symbol;
                  save : Savepos;

               PROCEDURE Factor(VAR at : Attribut);
                  VAR 
                     x    : Idptr;
                     lat  : Attribut;
                     save : Savepos;

                  PROCEDURE ConstSetConstructor(VAR at: Attribut);
                     VAR 
                        styp, ctyp         : Stptr;
                        smin, smax, v1, v2 : CARDINAL;
                        c1, c2             : Constval;
                        spat               : BITSET;(* set pattern *)

                  BEGIN 
                     smin := 0;
                     smax := BitsPerWord;
                     spat := {};
                     IF FAmong(at.atp,Stset{sets}) THEN 
                        styp := at.atp^.basep;
                     ELSE 
                        ErrorLS(99);
                        styp := NIL;
                     END;
                     IF FAmong(styp,Stset{enums,bools,cards,longcards,
					  ints,longints}) THEN 
                        WITH styp^ DO 
                           CASE form OF 
                           | subranges: 
                                 styp := scalp;
				 (* must not be INTEGER *)
                                 IF min > smin THEN 
                                    smin := min 
                                 END;
                                 IF max < smax THEN 
                                    smax := max 
                                 END;
                           | enums: 
                                 IF cstnr < smax THEN 
                                    smax := cstnr 
                                 END 
                           | bools: 
                                 smax := 1 
                           ELSE 
                              styp := NIL;
                           END;
                        END;            (* WITH *)
                     ELSE 
                        styp := NIL 
                     END;
                     GetSy;             (* lconbr *)
                     WHILE sy <> rconbr DO 
                        ConstantRange(ctyp,c1,c2);
                        IF (styp = NIL) AND FAmong(ctyp,Stset{enums,bools,
                           cards,longcards,ints,longints}) THEN 
                           styp := ctyp 
                        END;
			IF (styp <> NIL) AND Compatible(styp, ctyp) THEN
                           v1 := c1.value;
                           v2 := c2.value;
                           IF (v1 < smin) OR (v2 > smax) THEN 
                              ErrorLS(98);
                           ELSE 
                              WHILE v1 <= v2 DO 
                                 INCL(spat,v1);
                                 INC(v1)
                              END;
                           END;
                        ELSE 
                           ErrorLS(97);
                        END;
                     END;               (* WHILE *)
                     GetSy;
                     at.mode := constm;
                     at.aval.value := CARDINAL(spat);
                  END ConstSetConstructor;

		  PROCEDURE ExprRange(VAR tp: Stptr; VAR at1, at2: Attribut);
		     VAR lval, fval: CARDINAL;
		  BEGIN 
		     Expression(at1);
                     IF (at1.mode = constm) AND IsCharComp(at1) THEN
                        CharCast(at1);
                     END;
		     tp := at1.atp;
		     IF sy = range THEN 
			IF FAmong(tp,scalars) THEN 
			   PutGetSy;                   (* range *)
			   Expression(at2);
                           IF (at2.mode = constm) AND IsCharComp(at2) THEN
                              CharCast(at2);
                           END;
			   WITH at2 DO 
			      IF Compatible(tp, atp) THEN
				 IF (at1.mode = constm) AND
				    (at2.mode = constm) THEN
				    fval := at1.aval.value;
				    lval := at2.aval.value;
				    IF FAmong(tp, Stset{ints,longints}) &
				       FAmong(atp, Stset{ints, longints}) THEN 
				       IF INTEGER(lval) < INTEGER(fval) THEN 
					  ErrorLS(95);
					  lval := fval;
				       END;
				    ELSIF lval < fval THEN 
				       ErrorLS(95);
				       lval := fval;
				    END;
				 END;
			      ELSE 
				 ErrorLS(95);
			      END;
			   END;                     (* WITH *)
			ELSE 
			   Error(96);
			   GetSy;
			END;
		     ELSE
			at2 := at1;
		     END;
		  END ExprRange;

		  PROCEDURE ConstBigSetConstructor(VAR at: Attribut);
		  BEGIN
		     BigSetConstructor(at);
		     IF at.mode <> constm THEN
			at.mode := constm;
			InitConstSet(at.aval, at.atp);
			TermConstSet(at.aval);
			ErrorLS(136);
		     END;
		  END ConstBigSetConstructor;

                  PROCEDURE BigSetConstructor(VAR at: Attribut);
                     VAR 
			settype            : Stptr;
                        styp, ctyp         : Stptr;
                        smin, smax, v1, v2 : CARDINAL;
                        c1, c2             : Constval;
			spat               : Constval;
                        at1, at2           : Attribut; (* expr range *)
                        isconstant         : BOOLEAN;

                  BEGIN  (* BigSetConstructor *)
		     (* at.atp^.form = bigsets *)
                     isconstant := TRUE;
		     settype := at.atp;
		     InitConstSet(spat, settype);
                     WITH at.atp^ DO
		        smin := low;
		        smax := high;
                     END;
		     styp := settype^.basep;
                     PutGetSy;             (* lconbr *)
                     WHILE sy <> rconbr DO 
                        ExprRange(ctyp, at1, at2);
			IF (styp <> NIL) AND Compatible(styp, ctyp) THEN
                           IF (at1.mode <> constm) OR (at2.mode <> constm) THEN
                              isconstant := FALSE;
                           END;
                           IF at1.mode = constm THEN
                              v1 := at1.aval.value;
                           ELSE
                              v1 := smin;
                           END;
                           IF at2.mode = constm THEN
                              v2 := at2.aval.value;
                           ELSE
                              v2 := smax;
                           END;
                           IF (v1 < smin) OR (v2 > smax) THEN 
                              ErrorLS(98);
                           ELSIF isconstant THEN 
			      ConstSetElement(spat, v1, v2);
                           END;
                        ELSE 
                           ErrorLS(97);
                        END;
                     END;               (* WHILE *)
                     PutGetSy; (* rconbr *)
                     IF isconstant THEN
                        at.mode := constm;
			TermConstSet(spat);
			at.aval := spat;
                     ELSE
                        at.mode := exprm;
                        (* at.atp still set *)
                     END;
                  END BigSetConstructor;

                  PROCEDURE SetConstructor(VAR at: Attribut);
                     VAR 
                        styp, ctyp         : Stptr;
                        smin, smax, v1, v2 : CARDINAL;
                        c1, c2             : Constval;
                        spat               : BITSET;(* set pattern *)
                        at1, at2           : Attribut; (* expr range *)
                        isconstant         : BOOLEAN;

                  BEGIN  (* SetConstructor *)
                     isconstant := TRUE;
                     smin := 0;
                     smax := BitsPerWord-1;
                     spat := {};
                     IF FAmong(at.atp,Stset{sets}) THEN 
                        styp := at.atp^.basep;
                     ELSE 
                        ErrorLS(99);
                        styp := NIL;
                     END;
                     IF FAmong(styp,Stset{enums,bools,cards,longcards,
					  ints,longints}) THEN 
                        WITH styp^ DO 
                           CASE form OF 
                           | subranges: 
				 (* must not be INTEGER *)
                                 IF min > smin THEN 
                                    smin := min;
                                 END;
                                 IF max < smax THEN 
                                    smax := max;
                                 END;
                                 styp := scalp;
                           | enums: 
                                 IF cstnr < smax THEN 
                                    smax := cstnr 
                                 END 
                           | bools: 
                                 smax := 1 
                           ELSE 
                              styp := NIL;
                           END;
                        END;            (* WITH *)
                     ELSE 
                        styp := NIL 
                     END;
                     PutGetSy;             (* lconbr *)
                     WHILE sy <> rconbr DO 
                        ExprRange(ctyp,at1,at2);
                        IF (styp = NIL) AND FAmong(ctyp,Stset{enums,bools,
                           cards,longcards,ints,longints}) THEN 
                           styp := ctyp 
                        END;
			IF (styp <> NIL) AND Compatible(styp, ctyp) THEN
                           IF (at1.mode <> constm) OR (at2.mode <> constm) THEN
                              isconstant := FALSE;
                           END;
                           IF at1.mode = constm THEN
                              v1 := at1.aval.value;
                           ELSE
                              v1 := smin;
                           END;
                           IF at2.mode = constm THEN
                              v2 := at2.aval.value;
                           ELSE
                              v2 := smax;
                           END;
                           IF (v1 < smin) OR (v2 > smax) THEN 
                              ErrorLS(98);
                           ELSIF isconstant THEN 
                              WHILE v1 <= v2 DO 
                                 INCL(spat,v1);
                                 INC(v1)
                              END;
                           END;
                        ELSE 
                           ErrorLS(97);
                        END;
                     END;               (* WHILE *)
                     PutGetSy; (* rconbr *)
                     IF isconstant THEN
                        at.mode := constm;
                        at.aval.value := CARDINAL(spat);
                     ELSE
                        at.mode := exprm;
                        (* at.atp still set *)
                     END;
                  END SetConstructor;

		  PROCEDURE IsBigSet(at: Attribut) : BOOLEAN;
		  BEGIN
		     RETURN (at.atp <> NIL) AND (at.atp^.form = bigsets);
		  END IsBigSet;

                  PROCEDURE SkipFactor;
		     (* skip semantical incorrect elements of factor *)
                     VAR 
                        at : Attribut;
                  BEGIN 
                     InitAt(at);
                     IF sy = lconbr THEN 
                        IF modrev THEN
                           SetConstructor(at)
                        ELSE
                           ConstSetConstructor(at);
                        END;
                     ELSE 
                        Selector(at);
                        IF sy = lparent THEN 
                           GetSy;
                           ExprSequence 
                        END;
                     END;
                  END SkipFactor;

                  PROCEDURE StFuncCheck(ip: Idptr; VAR at : Attribut);
                     CONST 
                        all    = Stset{enums,bools,chars,ints,cards,bytes,
				       words,subranges,reals,pointers,
				       sets,proctypes,arrays,records,
				       bigsets,longints,longcards,longreals,
				       setoftypes};
                     TYPE
                        Kludge =
                           RECORD
                              CASE : BOOLEAN OF
                                TRUE: r1, r2: CARDINAL;
                              | FALSE: r: REAL;
                              END;
                           END;
                     VAR 
                        at1  : Attribut;
                        lval : Constval;
                        ltp  : Stptr;
                        sp   : Stptr;
                        nam  : Stfuncs;
                        i    : CARDINAL;
                        mreal: Kludge;

                     PROCEDURE NextParameter(VAR at: Attribut; forms: Stset);
                     BEGIN 
                        IF sy = rparent THEN 
                           Error(127);
                        ELSE 
                           TypeExpression(at,forms);
                        END;
                     END NextParameter;

                     (* constant folding for ORD/CHR/VAL *)

                     PROCEDURE ConstantCheck(VAR at: Attribut; at1: Attribut);
                     BEGIN
                        WITH at DO
                           IF at1.mode = constm THEN
                              mode := constm;
                              aval := at1.aval;
                           END;
                        END;
                     END ConstantCheck;

                  BEGIN 
                     InitAt(at);
                     at.mode := exprm;
                     nam := ip^.fname;
                     PutSy(namesy);
                     PutWord(ip);
                     PutGetSy;          (* lparent *)
                     CASE nam OF 
                       argcf:           (* ARGC *)
                           at.atp := GetType(intcars);
                           (* no arguments *)
                     | uxf:             (* UNIXCALL *)
			   at.atp := boolptr;
                           i := 0;
                           LOOP 
                              IF sy = comma THEN 
                                 PutGetSy 
                              END;
                              IF sy = rparent THEN 
                                 IF i < 3 THEN 
                                    ErrorLS(127);
                                 END;
                                 EXIT;
                              END;
                              INC(i);
                              CASE i OF 
                                1 : 
                                    ConstantVal(ltp, lval);
                                    PutConst(ltp, lval);
				    IF NOT FAmong(ltp, intcars) THEN
                                       ErrorLS(156);
                                    END;
                              | 2, 3 : 
                                    InitAt(at1);
                                    TypeExpression(at1, intcars);
                                    IF at1.mode <> varm THEN
                                       ErrorLS(127);
                                    END;
                              ELSE 
                                 InitAt(at1);
                                 Expression(at1);
                              END;
                           END;         (* LOOP *)
                     | uxff:             (* UNIXFORK *)
			   at.atp := boolptr;
                           IF sy = rparent THEN
                              ErrorLS(127);
                           ELSE
                              InitAt(at1);
                              TypeExpression(at1, intcars);
                              IF at1.mode <> varm THEN
                                 ErrorLS(127);
                              END;
                           END;
                     | uxsf:             (* UNIXSIGNAL *)
                           i := 0;
                           LOOP 
                              IF sy = comma THEN 
                                 PutGetSy 
                              END;
                              IF sy = rparent THEN 
                                 IF i < 4 THEN 
                                    ErrorLS(127);
                                 END;
                                 EXIT;
                              END;
                              INC(i);
                              CASE i OF 
                                1 : 
                                    TypeExpression(at,Stset{longcards,cards});
                              | 2 :
                                    Expression(at);
                                    at1.atp := procptr;
                                    at1.mode := varm;
                                    IF NOT ExprComp(at,at1) THEN 
                                       ErrorLS(127)
                                    END;
                              | 3 :
                                    Expression(at);
                                    at1.atp := procptr;
                                    at1.mode := varm;
                                    WITH at DO 
                                       IF NOT (mode = varm) OR
                                          NOT ExprComp(at, at1) THEN
                                          ErrorLS(127)
                                       END 
                                    END;
                              | 4 : 
                                    InitAt(at1);
                                    TypeExpression(at1, intcars);
                                    IF NOT (at1.mode = varm) THEN
                                       ErrorLS(127);
                                    END;
                                    EXIT;
                              END;
                           END;         (* LOOP *)
                           InitAt(at);
                           at.mode := exprm;
                           at.atp := boolptr;
                     | higf:            (* HIGH *)
                           at.atp := cardptr;
                           IF sy = rparent THEN 
                              Error(127);
                           ELSE 
                              VariableStandard(at1,Stset{arrays});
                              WITH at1 DO 
                                 IF (atp<>NIL) AND (atp^.form=arrays) THEN 
                                    IF NOT atp^.dyn THEN 
                                       at.mode := constm;
                                       at.aval.value := atp^.ixp^.max;
				       ConstType(at.aval, at.atp);
                                    END;
                                 END;
                              END;
                           END;
		     (* (* old fashion *)
                     | sizf:            (* SIZE *)
                           at.atp := cardptr;
                           IF sy = rparent THEN 
                              Error(127);
                           ELSE 
                              VariableStandard(at1,all);
                              WITH at1 DO 
                                 IF atp <> NIL THEN 
                                    IF NOT ((atp^.form=arrays) AND atp^.dyn)
                                       THEN 
                                       at.mode := constm;
                                       at.aval.value := atp^.size;
				       ConstType(at.aval, at.atp);
                                    END;
                                 END;
                              END;
                           END;
		     *)
                     | sizf, tszf:            (* SIZE, TSIZE *)
                           at.atp := cardptr;
                           IF sy = ident THEN 
			      (* new value for ip *)
			      IF modrev2 AND (nam = sizf) THEN
				 QualIdent(Idset{vars, types},129,ip);
			      ELSIF nam = sizf THEN
				 QualIdent(Idset{vars},129,ip);
			      ELSE (* nam = tszf *)
				 QualIdent(Idset{types},129,ip);
			      END;
                              IF ip <> NIL THEN 
				 sp := ip^.idtyp;
				 IF ip^.klass = vars THEN
				    PreSelector(ip, at);
				    Selector(at);
				    sp := at.atp;
				    at.mode := exprm;
				    at.atp := cardptr;
				 END;
                                 IF sp <> NIL THEN 
				    IF NOT ((sp^.form = arrays) AND sp^.dyn)
				       THEN
				       WITH sp^ DO 
					  lval.value := size;
					  IF form = records THEN 
					     VariantAnalyse(tagp,lval)
					  END;
				       END;
				       at.mode := constm;
				       at.aval := lval;
				       ConstType(at.aval, at.atp);
				    END;
                                 END;
                              END;
                           ELSE 
                              Error(135);
                           END;
                           CASE sy OF 
                             lparent, lconbr, lbrack, period, arrow: 
                                 Error(135);
                                 SkipFactor;
                           ELSE         (* nothing *)
                           END;         (* CASE *)
                     | adrf:            (* ADR *)
                           at.atp := addrptr;
                           IF sy = rparent THEN 
                              Error(127);
                           ELSE 
                              VariableStandard(at1,all);
                           END;
                     | oddf:            (* ODD *)
                           at.atp := boolptr;
                           NextParameter(at1,intcars);
                           IF at1.mode = constm THEN
                              at.mode := constm;
                              at.aval.value := ORD(ODD(at1.aval.value));
                           END;
                     | absf:            (* ABS *)
                           InitAt(at1);
                           NextParameter(at1,typesettypes);
                           IF (at1.mode = constm) AND
                              FAmong(at1.atp, Stset{ints, cards,
                                                    longints, longcards}) THEN
                              at.mode := constm;
                              IF FAmong(at1.atp, Stset{ints, longints}) THEN
                                 at.aval.value := ABS(INTEGER(at1.aval.value));
                              ELSE
                                 at.aval.value := at1.aval.value;
                              END;
                           END;
                           at.atp := at1.atp;
                     | capf:            (* CAP *)
                           at.atp := charptr;
                           NextParameter(at1,Stset{chars});
                           IF at1.mode = constm THEN
                              at.mode := constm;
                              (* $R- *)
                              at.aval.value := ORD(CAP(CHR(at1.aval.value)));
                              (* $R= *)
                           END;
                     | fltf:            (* FLOAT *)
                           at.atp := realptr;
                           NextParameter(at1,intcars);
                     | trcf:            (* TRUNC *)
                           (* use intptr instead of cardptr (standard) *)
                           (* see pass 4 for reasons                   *)
                           at.atp := GetType(Stset{ints, longints});
                           NextParameter(at1,Stset{reals,longreals});
                     | ordf:            (* ORD *)
                           at.atp := GetType(Stset{cards, longcards});
                           NextParameter(at1,scalars);
                           ConstantCheck(at, at1);
                     | chrf:            (* CHR *)
                           at.atp := charptr;
                           NextParameter(at1,intcars);
                           ConstantCheck(at, at1);
			   IF (at.mode = constm) AND
				 (at.aval.value > ORD(MAX(CHAR))) THEN
			      Error(2);
			   END;
                     | minf, maxf:        (* MIN, MAX *)
                           IF sy = ident THEN
                              QualIdent(Idset{types}, 129, ip);
                              (* new value for ip *)
                              IF ip <> NIL THEN
                                 sp := ip^.idtyp;
				 OpenTest(sp);
                                 at.atp := sp;
                                 IF sp <> NIL THEN
                                    WITH sp^ DO
                                       WITH lval DO
                                          CASE form OF
                                          | bools:
                                              IF nam = minf THEN
                                                 value := ORD(FALSE);
                                              ELSE
                                                 value := ORD(TRUE);
                                              END;
                                          | chars:
                                              IF nam = minf THEN
                                                 value := 0B;
                                              ELSE
                                                 value := charmax;
                                              END;
                                          | ints:
                                              IF nam = minf THEN
                                                 value := CARDINAL(-maxint-1);
                                              ELSE
                                                 value := maxint;
                                              END;
					  | longints:
					      IF nam = minf THEN
						 value := LONGCARD(-longmaxint-1);
					      ELSE
						 value := longmaxint;
					      END;
                                          | cards:
                                              IF nam = minf THEN
                                                 value := 0;
                                              ELSE
                                                 value := maxcard;
                                              END;
					  | longcards:
					      IF nam = minf THEN
						 value := 0;
					      ELSE
						 value := longmaxcard;
					      END;
                                          | subranges:
                                              IF nam = minf THEN
                                                 value := min;
                                              ELSE
                                                 value := max;
                                              END;
                                          | enums:
                                              IF nam = minf THEN
                                                 value := 0;
                                              ELSE
                                                 value := cstnr;
                                              END;
                                          | reals, longreals:
                                              (* kludgy, kludgy *)
                                              NEW(rvalue);
                                              mreal.r2 := maxcard;
                                              IF nam = minf THEN
                                                 mreal.r1 := 0FFEFFFFFH;
                                              ELSE
                                                 mreal.r1 := 07FEFFFFFH;
                                              END;
                                              rvalue^ := mreal.r;
                                          ELSE
                                             Error(109);
                                          END;
                                       END; (* WITH lval *)
                                    END; (* WITH sp^ *)
                                    at.mode := constm;
                                    at.aval := lval;
                                 END;
                              END;
                           ELSE
                              Error(135);
                           END;
                           CASE sy OF
                             lparent, lconbr, lbrack, period, arrow:
                                  Error(135);
                                  SkipFactor;
                           ELSE (* nothing *)
                           END;
                     | valf:            (* VAL *)
                           at.atp := NIL;
                           IF sy = ident THEN 
                              QualIdent(Idset{types},129,ip);
                                        (* new value for ip *)
                              IF ip <> NIL THEN 
                                 IF FAmong(ip^.idtyp,scalars) THEN 
                                    at.atp := ip^.idtyp;
                                 ELSE 
                                    ErrorLS(121);
                                 END;
                              END;
                              PutSy(namesy);
                              PutWord(ip);
                              IF (sy <> comma) AND (sy <> rparent) THEN
                                 Error(135);
                                 REPEAT
                                    GetSy;
                                 UNTIL (sy = comma) OR (sy = rparent);
                              END;
                           ELSE 
                              Error(135);
                           END;
                           IF sy = comma THEN 
                              PutGetSy 
                           END;
                           NextParameter(at1,intcars);
                           ConstantCheck(at, at1);
                     END;               (* CASE nam *)
		     IF sy <> rparent THEN
			(* avoid trouble in cases like 'TSIZE(CARDINAL*4)' *)
			ErrorLS(127);
			WHILE (sy <> rparent) AND (sy <> comma) AND
			   (sy <> lparent) DO
			   PutGetSy;
			END;
		     END;
                     IF sy = rparent THEN 
                        PutGetSy;
                     ELSE 
                        ExprSequence;
                     END;
                  END StFuncCheck;

               BEGIN                    (* Factor *)
                  InitSave(save);
                  InitAt(at);
                  IF sy = ident THEN 
                     QualIdent(Idset{consts..funcs},73,x);
                     IF x <> NIL THEN   (* ident IN {consts..funcs} *)
                        IF (x^.klass IN Idset{pures,funcs}) AND x^.isstandard 
                           THEN 
                           IF sy = lparent THEN 
                              IF x^.klass = funcs THEN 
                                 StFuncCheck(x,at);
                              ELSE 
                                 Error(145);
                                 PutGetSy; (* lparent *)
                                 ExprSequence;
                              END;
                           ELSE 
                              CASE sy OF 
                                lconbr, lbrack, period, arrow: 
                                    Error(144);
                                    SkipFactor;
                              ELSE 
                                 ErrorLS(147);
                              END;      (* CASE *)
                           END;
                        ELSE 
                           PreSelector(x,at);
                           WITH x^ DO 
                              CASE klass OF 
                                consts : 
                                    CASE sy OF 
                                      lparent, lconbr, lbrack, period, arrow: 
                                          Error(123);
                                          InitAt(at);
                                          SkipFactor;
                                    ELSE 
                                       at.mode := constm;
				       IF IsReal(idtyp) THEN
                                        (* make a copy of the real value *)
                                          NEW(at.aval.rvalue);
                                          at.aval.rvalue^ := cvalue.rvalue^;
                                       ELSE 
                                          at.aval := cvalue;
                                       END;
                                    END;(* CASE *)
                              | types : 
                                    CASE sy OF 
                                      lconbr: 
                                          IF modrev THEN
                                             PutSy(namesy);
                                             PutWord(x);
					     IF IsBigSet(at) THEN
						BigSetConstructor(at);
					     ELSE
						SetConstructor(at);
					     END;
                                          ELSIF IsBigSet(at) THEN
					     ConstBigSetConstructor(at);
					  ELSE
                                             ConstSetConstructor(at);
                                          END;
                                    | lparent: 
                                          PutSy(namesy);
                                          PutWord(x);
                                          TypFunction(at)
                                    | lbrack, period, arrow: 
                                          Error(144);
                                          InitAt(at);
                                          SkipFactor;
                                    ELSE 
                                       ErrorLS(137);
                                       InitAt(at);
                                    END;(* CASE *)
                              | vars,fields,pures,funcs: 
                                    IF at.mode = varm THEN 
                                       Selector(at)
                                    END;
                                    CASE sy OF 
                                      lparent: 
                                          at.mode := exprm;
                                          IF (klass=funcs)AND(priolev<>noprio)
                                             THEN 
                                             externalaccess := TRUE;
                                          END;
                                          PutGetSy;
                                          IF at.atp <> NIL THEN 
                                             WITH at.atp^ DO 
                                                IF (form=proctypes) AND (rkind
                                                   =funcs) THEN 
                                                   ParamCheck(fstparam);
                                                   at.atp := funcp;
                                                ELSE 
                                                   ErrorLS(145);
                                                   at.atp := NIL;
                                                   ExprSequence;
                                                END;
                                             END;
                                          ELSE 
                                             ExprSequence;
                                          END;
                                    | lconbr, lbrack, period, arrow: 
                                          Error(144);
                                          InitAt(at);
                                          SkipFactor;
                                    ELSE 
                                       IF (klass = pures) OR (klass = funcs)
                                          THEN 
                                          externalaccess := TRUE;
                                          IF plev <> 1 THEN 
                                             ErrorLS(141)
                                          END;
                                          at.mode := exprm;
                                       END;
                                    END;(* CASE *)
                              END;      (* CASE *)
                           END;         (* WITH x^ *)
                        END;
                     ELSE               (* x = NIL *)
                        SkipFactor;
                     END;
                  ELSIF sy = lconbr THEN 
                                        (* BITSET *)
                     at.atp := bitsetptr;
                     IF modrev THEN
                        SetConstructor(at);
                     ELSE
                        ConstSetConstructor(at);
                     END;
                  ELSIF sy = notsy THEN 
                     PutGetSy;
                     Factor(at);
                     IF FAmong(at.atp,Stset{bools}) THEN 
                        IF at.mode = constm THEN 
                           NotOp(at.aval,at.aval);
                        ELSE 
                           at.mode := exprm;
                        END;
                     ELSE 
                        at.mode := exprm;
                        ErrorLS(138);
                     END;
                  ELSIF sy = lparent THEN 
                     PutGetSy;
                     Expression(at);
                     IF at.mode = varm THEN 
                        at.mode := exprm 
                     END;
                     PutGetSy 
                  ELSE                  (* constants *)
                     WITH at DO 
                        mode := constm;
                        aval.value := val;
                        CASE sy OF 
                          intcon : 
                              atp := intptr;
                        | intcarcon : 
			      atp := GetType(typeset);
                        | cardcon : 
                              atp := cardptr;
                        | charcon : 
                              atp := charptr;
                        | realcon : 
                              (* atp := realptr; *)
			      atp := GetType(Stset{reals, longreals});
                        | stringcon : 
                              atp := StringStruct(length);
                        END;            (* CASE *)
                        GetSy;
                     END;
                  END;
                  IF at.mode = constm THEN 
                     ResetSave(save);
                     PutConst(at.atp,at.aval);
                  END;
                  ReleaseSave(save);
               END Factor;

            BEGIN                       (* Term *)
               InitSave(save);
               Factor(at);
               WHILE (sy >= andsy) AND (sy <= modsy) DO 
                  op := sy;
                  PutGetSy;
                  Factor(lat);
                  IF at.atp = NIL THEN 
                     at.atp := lat.atp;
                  ELSE 
                     IF ExprComp(at,lat) THEN 
                        CASE op OF 
                          andsy: 
                              forms := Stset{bools}
                        | times: 
                              forms := intcars +
				       Stset{sets,bigsets,reals,longreals}
                        | slash: 
                              forms := Stset{sets,bigsets,reals,longreals}
                        | divsy,modsy: 
                              forms := intcars
                        END;            (* CASE *)
                        IF FAmong(at.atp,forms) THEN 
                           IF (at.mode = constm) AND (lat.mode = constm) THEN 
			      at.atp := ResultType(at.atp, lat.atp);
                              MulOp(at.aval,lat.aval,at.aval,op,at.atp,err);
                              IF err THEN 
                                 at.mode := exprm;
                                 IF NOT FAmong(at.atp,Stset{reals,longreals})
				    THEN 
                                    ErrorLS(94);
                                 END;
                              ELSE 
                                 ResetSave(save);
                                 PutConst(at.atp,at.aval);
                              END;
                           ELSE 
                              at.mode := exprm;
                           END;
                        ELSE 
                           at.mode := exprm;
                           ErrorLS(140);
                        END;
                     ELSE 
                        at.mode := exprm;
                        ErrorLS(143);
                     END;
                  END;
               END;                     (* WHILE *)
               ReleaseSave(save);
            END Term;

         BEGIN                          (* SimpleExpression *)
            InitSave(save);
            sign := (sy = minus) OR (sy = plus);
            IF sign THEN 
               op := sy;
               IF op = minus THEN 
                  PutGetSy 
               ELSE 
                  GetSy 
               END;
            END;
            Term(at);
            IF sign THEN 
               WITH at DO 
                  IF mode = varm THEN 
                     mode := exprm 
                  END;
                  IF FAmong(atp, Stset{ints, longints, longreals, reals}) THEN
                     IF op = minus THEN 
                        IF mode = constm THEN 
                           IF IsReal(atp) THEN 
                              lat.aval.rvalue := NIL;
                           ELSE 
                              lat.aval.value := 0;
                           END;
			   atp := ResultOfNegation(atp);
                           AddOp(lat.aval,aval,aval,op,atp,err);
                           IF err THEN 
                              mode := exprm;
                              ErrorLS(94);
                           ELSE 
                              ResetSave(save);
                              PutConst(atp,aval);
                           END;
                        END;
                     END;
                  ELSIF FAmong(atp, Stset{cards, longcards}) THEN
                     IF op = minus THEN 
                        mode := exprm;
                        ErrorLS(121)
                     END;
                  ELSE 
                     mode := exprm;
                     ErrorLS(121);
                  END;
               END;                     (* WITH *)
            END;
            WHILE (sy >= plus) AND (sy <= orsy) DO 
               op := sy;
               PutGetSy;
               Term(lat);
               IF at.atp = NIL THEN 
                  at.atp := lat.atp;
               ELSE 
                  IF ExprComp(at,lat) THEN 
                     CASE op OF 
                     | orsy: 
                           forms := Stset{bools}
                     | plus,minus: 
                           forms := intcars + Stset{sets,bigsets,reals,longreals}
                     END;               (* CASE *)
                     IF FAmong(at.atp,forms) THEN 
                        IF (at.mode = constm) AND (lat.mode = constm) THEN 
			   at.atp := ResultType(at.atp, lat.atp);
                           AddOp(at.aval,lat.aval,at.aval,op,at.atp,err);
                           IF err THEN 
                              at.mode := exprm;
                              IF NOT FAmong(at.atp,Stset{reals,longreals}) THEN 
                                 ErrorLS(94);
                              END;
                           ELSE 
                              ResetSave(save);
                              PutConst(at.atp,at.aval);
                           END;
                        ELSE 
                           at.mode := exprm;
                        END;
                     ELSE 
                        at.mode := exprm;
                        ErrorLS(140);
                     END;
                  ELSE 
                     at.mode := exprm;
                     ErrorLS(143);
                  END;
               END;
            END;                        (* WHILE *)
            ReleaseSave(save);
         END SimpleExpression;

      BEGIN                             (* Expression *)
         InitSave(save);
         SimpleExpression(at);
         IF (sy >= eql) AND (sy <= insy) THEN 
            op := sy;
            PutGetSy;
            SimpleExpression(lat);
            (* cast one character strings to type CHAR *)
            IF modrev THEN
               IF (at.mode = constm) AND IsCharComp(at) THEN
                  CharCast(at);
               END;
               IF (lat.mode = constm) AND IsCharComp(lat) THEN
                  CharCast(lat);
               END;
            END;
            tpat := at.atp;
            tplat := lat.atp;
            IF op = insy THEN 
               IF FAmong(tplat,Stset{sets,bigsets}) THEN 
                  lat.atp := tplat^.basep;
                  IF ExprComp(at,lat) THEN 
                     IF (at.mode=constm) AND (lat.mode=constm) THEN 
                        RelOp(at.aval,lat.aval,at.aval,insy,tpat,err);
                        IF err THEN 
                           at.mode := exprm;
                           ErrorLS(94);
                        ELSE 
                           ResetSave(save);
                           PutConst(boolptr,at.aval);
                        END;
                     ELSE 
                        at.mode := exprm;
                     END;
                  ELSE 
                     at.mode := exprm;
                     ErrorLS(142);
                  END;
               ELSE 
                  at.mode := exprm;
                  ErrorLS(149);
               END 
            ELSIF ExprComp(at,lat) OR AddressComp(at,lat) THEN 
               IF (tpat <> NIL) AND (tpat^.form = setoftypes) THEN
                  tpat := tplat 
               END;
               CASE op OF 
                 eql,neq : 
                     forms := Stset{sets,bigsets,pointers,reals,longreals} +
			      scalars;
                     IF modrev THEN
                        INCL(forms, hides);
                     END;
               | geq,leq : 
                     forms := Stset{sets,bigsets,reals,longreals} + scalars 
               | grt,lss : 
                     forms := Stset{reals,longreals} + scalars 
               END;
               IF FAmong(tpat,forms) THEN 
                  IF (at.mode=constm) AND (lat.mode=constm) THEN 
                     RelOp(at.aval,lat.aval,at.aval,op,tpat,err);
                     IF err THEN 
                        at.mode := exprm;
                        IF NOT FAmong(tpat,Stset{reals,longreals}) THEN 
                           ErrorLS(94);
                        END;
                     ELSE 
                        ResetSave(save);
                        PutConst(boolptr,at.aval);
                     END;
                  ELSE 
                     at.mode := exprm;
                  END;
               ELSE 
                  at.mode := exprm;
                  ErrorLS(140);
               END;
            ELSE 
               at.mode := exprm;
               ErrorLS(143);
            END;
            at.atp := boolptr;
         END;
         ReleaseSave(save);
      END Expression;

      PROCEDURE Constant(VAR at: Attribut);
         VAR 
            save : Savepos;
      BEGIN 
         InitSave(save);
         Expression(at);
         WITH at DO 
            IF mode = constm THEN 
               ResetSave(save);
            ELSE 
               ErrorLS(136);
            END;
         END;
         ReleaseSave(save);
      END Constant;

      PROCEDURE ConstantRange(VAR tp: Stptr; VAR c1, c2: Constval);
         VAR 
            fval,lval : CARDINAL;
            at        : Attribut;
      BEGIN 
         Constant(at);
         IF modrev AND IsCharComp(at) THEN
            CharCast(at);
         END;
         tp := at.atp;
         fval := at.aval.value;
         lval := fval;
         IF sy = range THEN 
            IF FAmong(tp,scalars) THEN 
               GetSy;                   (* range *)
               Constant(at);
               IF modrev AND IsCharComp(at) THEN
                  CharCast(at);
               END;
               WITH at DO 
		  IF Compatible(tp, atp) THEN
                     lval := aval.value;
                     IF IsInt(tp) THEN
                        IF INTEGER(lval) < INTEGER(fval) THEN 
                           ErrorLS(95);
                           lval := fval;
                        END;
                     ELSIF lval < fval THEN 
                        ErrorLS(95);
                        lval := fval;
                     END;
                  ELSE 
                     ErrorLS(95);
                  END;
               END;                     (* WITH *)
            ELSE 
               Error(96);
               GetSy;
            END;
         END;
         c1.value := fval;
         c2.value := lval;
      END ConstantRange;

      PROCEDURE ConstantVal(VAR tp: Stptr; VAR c: Constval);
         VAR 
            at : Attribut;
      BEGIN 
         Constant(at);
         tp := at.atp;
         c := at.aval;
      END ConstantVal;

   BEGIN 
      stringroot := NIL;
   END ExpressionSystem;

   PROCEDURE ModulDeclaration;
      VAR 
         mptr : Idptr;

      PROCEDURE ImportList;
      (* analyse import list of a module *)
         VAR 
            ip, ep  : Idptr;
            frommod : BOOLEAN;

      BEGIN 
         NewImpList(mptr^.impp);
         WHILE (sy = importsy) OR (sy = fromsy) DO 
            frommod := sy = fromsy;
            GetSy;
            IF frommod THEN 
               SearchId(ip);
               IF (ip = NIL) OR (ip^.klass <> mods) THEN 
                                        (* skip this list *)
                  Error(105);
                  WHILE sy = ident DO 
                     GetSy 
                  END;
               ELSE 
                  GetSy;
                  ep := ip^.expp;
               END;
            END;
            WHILE sy = ident DO    (* identifier skipped if module not found *)
               IF frommod THEN 
                  ExportSearch(ep,ip);
                  IF ip = NIL THEN 
                     Error(71)
                  END;
               ELSE 
                  SearchId(ip);
                  IF ip = NIL THEN 
                     Error(73)
                  END;
               END;
               IF ip <> NIL THEN 
                  EnterImpList(ip)
               END;
               GetSy;
            END;                        (* while *)
         END;                           (* while *)
         TermImpList(mptr^.impp);
      END ImportList;

   BEGIN 
      mptr := nptr;
      GetSy;
      ImportList;
      MarkModScope(mptr);
      Block(mptr);
      ReleaseModScope;
      IF NOT MDDFlag THEN
	 DisposeImpList(mptr^.impp);
      END;
      GetSy;                            (* endblock *)
   END ModulDeclaration;

   PROCEDURE Block(bptr : Idptr);
      VAR 
         inloop   : BOOLEAN;
         priority : CARDINAL;

      PROCEDURE BlockDeclaration;
         VAR 
            lnptr: Idptr;

      BEGIN 
         lnptr := nptr;
         PutGetSy;
         MarkProcScope(lnptr);
         INC(nestlevel);
         Block(lnptr);
         ReleaseProcScope;
         DEC(nestlevel);
         PutGetSy;                      (* endblock *)
      END BlockDeclaration;

      PROCEDURE Statement;

         PROCEDURE StatSeq1(s1 : Symbol);
         BEGIN 
            WHILE sy <> s1 DO 
               Statement 
            END 
         END StatSeq1;

         PROCEDURE StatSeq2(s1,s2 : Symbol);
         BEGIN 
            WHILE (sy <> s1) AND (sy <> s2) DO 
               Statement 
            END 
         END StatSeq2;

         PROCEDURE StatSeq3(s1,s2,s3 : Symbol);
         BEGIN 
            WHILE (sy <> s1) AND (sy <> s2) AND (sy <> s3) DO 
               Statement 
            END 
         END StatSeq3;

         PROCEDURE Assignment;
            VAR 
               at1,at2 : Attribut;
               x       : Idptr;

         BEGIN 
            InitAt(at1);
            QualIdent(Idset{vars,fields},122,x);
            IF x <> NIL THEN 
               PreSelector(x,at1);
               Selector(at1);
               PutGetSy;                (* comma *)
               Expression(at2);
               IF NOT (AssignComp(at1,at2) OR AddressComp(at1,at2)) THEN 
                  ErrorLS(128)
               END;
            ELSE 
               Selector(at1);
               PutGetSy;                (* comma *)
               Expression(at2);
            END;
         END Assignment;

         PROCEDURE CallStatement;
            VAR 
               x  : Idptr;
               ok : BOOLEAN;
               at : Attribut;
               fp : Idptr;

            PROCEDURE SkipCall;
            (* skip semantical incorrect parts of call *)
               VAR 
                  at: Attribut;

            BEGIN 
               InitAt(at);
               Selector(at);
               PutGetSy;                (* lparent *)
               ExprSequence;
            END SkipCall;

         BEGIN 
            IF sy = namesy THEN 
               x := nptr;
               GetSy;                   (* module bodies *)
            ELSE 
               QualIdent(Idset{vars,fields,pures,funcs},73,x);
            END;
            IF x <> NIL THEN 
               IF (x^.klass = pures) AND x^.isstandard THEN 
                  IF sy = lparent THEN 
                     StProcCheck(x);
                  ELSE 
                     Error(144);
                     SkipCall;
                  END;
               ELSIF (x^.klass = funcs) AND x^.isstandard THEN 
                  IF sy = lparent THEN 
                     ErrorLS(157)
                  ELSE 
                     Error(144)
                  END;
                  SkipCall;
               ELSE 
                  PreSelector(x,at);
                  IF at.mode = varm THEN 
                     Selector(at);
                  ELSE                  (* pures, funcs, mods *)
                     WITH x^ DO 
                        IF (priolev <> priority) AND (priolev <> noprio) THEN 
                           IF (priority = noprio) OR (priority < priolev)
                              THEN 
                              externalaccess := TRUE;
                           ELSE 
                              ErrorLS(161);
                           END;
                        END;
                     END;
                  END;
                  ok := TRUE;
                  IF at.atp = NIL THEN 
                     fp := NIL;         (* may be a module call *)
                  ELSE 
                     WITH at.atp^ DO 
                        IF (form = proctypes) AND (rkind <> funcs) THEN 
                           fp := fstparam;
                        ELSE 
                           ErrorLS(157);
                           ok := FALSE;
                        END;
                     END;               (* WITH *)
                  END;
                  ok := ok AND (sy = lparent);
                  IF ok THEN 
                     PutGetSy;          (* lparent *)
                     ParamCheck(fp);
                  ELSE 
                     IF sy <> lparent THEN 
                        Error(144)
                     END;
                     SkipCall;
                  END;
               END;
            ELSE                        (* x = NIL *)
               SkipCall;
            END;
         END CallStatement;

         PROCEDURE IfStatement;
            VAR 
               at : Attribut;

         BEGIN 
            LOOP 
               TypeExpression(at,Stset{bools});
               StatSeq3(endsy,elsesy,elsifsy);
               IF sy <> elsifsy THEN 
                  EXIT 
               END;
               PutGetSy;
            END;
            IF sy = elsesy THEN 
               PutGetSy;
               StatSeq1(endsy)
            END;
            PutGetSy;                   (* endsy *)
         END IfStatement;

         PROCEDURE WithStatement;
            VAR 
               x        : Idptr;
               ltp      : Stptr;
               at       : Attribut;
               isrecord : BOOLEAN;

         BEGIN 
            QualIdent(Idset{vars,fields},122,x);
            IF x <> NIL THEN 
               PreSelector(x,at);
               Selector(at);
            ELSE 
               InitAt(at);
               Selector(at);
            END;
            ltp := at.atp;
            isrecord := FAmong(ltp,Stset{records});
            IF isrecord THEN 
               MarkWithScope(ltp^.fieldp);
            ELSE 
               ErrorLS(121);
            END;
            StatSeq1(endsy);
            IF isrecord THEN 
               ReleaseWithScope 
            END;
            PutGetSy;
         END WithStatement;

         PROCEDURE CaseStatement;
            VAR 
               at1,at2 : Attribut;
               c1, c2  : Constval;
         BEGIN 
            TypeExpression(at1,scalars);
            WHILE sy = ofsy DO 
               PutGetSy;
               REPEAT 
                  WITH at2 DO 
                     mode := constm;
                     ConstantRange(atp,c1,c2);
                     IF ExprComp(at1,at2) THEN 
                        PutConst(atp,c1);
                        IF c1.value <> c2.value THEN
                           (* revision of interpass file *)
                           PutSy(range);
                           (* pass4 expects integer ranges *)
                           IF NOT IsInt(atp) AND (c1.value <= maxint)
                              AND (c2.value > maxint) THEN
                              c1.value := maxint;
                              PutConst(atp, c1);
                              c1.value := maxint+1;
                              PutConst(atp, c1);
                              PutSy(range);
                           END;
                           PutConst(atp, c2);
                        END;
                     ELSE 
                        ErrorLS(128);
                     END;
                  END;
               UNTIL sy = colon;
               PutGetSy;
               StatSeq3(ofsy,elsesy,endsy);
            END;
            IF sy = elsesy THEN 
               PutGetSy;
               StatSeq1(endsy)
            END;
            PutGetSy;                   (* endsy *)
         END CaseStatement;

         PROCEDURE LoopStatement;
            VAR 
               oldinloop : BOOLEAN;
         BEGIN 
            oldinloop := inloop;
            inloop := TRUE;
            StatSeq1(endsy);
            PutGetSy;
            inloop := oldinloop;
         END LoopStatement;

         PROCEDURE ExitStatement;
         BEGIN 
            IF NOT inloop THEN 
               ErrorLS(151)
            END;
         END ExitStatement;

         PROCEDURE ReturnStatement;
            VAR 
               at1,at2 : Attribut;
         BEGIN                          (* expression in parenthesis *)
            CASE bptr^.klass OF 
              funcs :                   (* function block *)
                  IF sy <> lparent THEN 
                     Error(153)
                  ELSE 
                     PutGetSy;          (* lparent *)
                     Expression(at1);
                     at2.atp := bptr^.idtyp^.funcp;
                     at2.mode := varm;
                     IF NOT (AssignComp(at2,at1) OR AddressComp(at1,at2))
                        THEN 
                        ErrorLS(155)
                     END;
                     PutGetSy;          (* rparent *)
                  END;
            | pures, mods :             (* procedure or module block *)
                  IF sy = lparent THEN 
                     Error(154);
                     Expression(at1)
                  END;
            END;
         END ReturnStatement;

         PROCEDURE ForStatement;
            VAR 
               at1,at2: Attribut;
               ip     : Idptr;
               sp     : Stptr;
               lval   : Constval;
         BEGIN 
            QualIdent(Idset{vars},122,ip);
                                        (* single identifier expected *)
            IF ip <> NIL THEN 
               PreSelector(ip,at1);
               Selector(at1);
               IF NOT FAmong(at1.atp,scalars) THEN 
                  ErrorLS(139)
               END;
            ELSE 
               InitAt(at1);
               Selector(at1);
            END;
            PutGetSy;                   (* comma *)
            LOOP 
               TypeExpression(at2,scalars);
               IF NOT AssignComp(at1,at2) THEN 
                  ErrorLS(128);
               ELSIF modrev AND NOT ExprComp(at1, at2) THEN
                  ErrorLS(305);
               END;
               IF sy = tosy THEN 
                  PutGetSy 
               ELSE 
                  EXIT 
               END;
            END;                        (* LOOP *)
            IF sy = bysy THEN 
               PutGetSy;
               ConstantVal(sp,lval);
               PutConst(sp,lval);
               IF NOT FAmong(sp,intcars) THEN 
                  ErrorLS(156)
               END;
            END;
            StatSeq1(endsy);
            PutGetSy;
         END ForStatement;

         PROCEDURE RepeatStatement;
            VAR 
               at : Attribut;
         BEGIN 
            StatSeq1(untilsy);
            PutGetSy;
            TypeExpression(at,Stset{bools});
         END RepeatStatement;

         PROCEDURE WhileStatement;
            VAR 
               at : Attribut;
         BEGIN 
            TypeExpression(at,Stset{bools});
            StatSeq1(endsy);
            PutGetSy;
         END WhileStatement;

         VAR 
            lsy : Symbol;               (* leading symbol in statement *)

      BEGIN                             (* Statement *)
         lsy := sy;
         PutGetSy;
         CASE lsy OF 
         | becomes: 
               Assignment;
         | call: 
               CallStatement;
         | ifsy: 
               IfStatement;
         | withsy: 
               WithStatement 
         | casesy: 
               CaseStatement 
         | loopsy: 
               LoopStatement 
         | whilesy: 
               WhileStatement 
         | repeatsy: 
               RepeatStatement 
         | forsy: 
               ForStatement 
         | returnsy: 
               ReturnStatement 
         | exitsy: 
               ExitStatement 
         ELSE                           (* nothing *)
         END;
      END Statement;

   BEGIN                                (* Block *)
      REPEAT 
         IF sy = proceduresy THEN 
            BlockDeclaration;
         ELSIF sy = modulesy THEN 
            ModulDeclaration 
         END;
      UNTIL (sy = beginsy) OR (sy = endblock);
      inloop := FALSE;
      priority := bptr^.priolev;
      BodyMark;
      IF sy = beginsy THEN 
         PutGetSy;
         WHILE sy <> endblock DO 
            Statement 
         END;
      END;
      (* update space used by procedure on stack *)
      (* (* not used: pass4 allocates register for with-addresses *)
      INC(bptr^.varlength,BodyScopes()*oneword);
      *)
   END Block;

   PROCEDURE StartBodyAnalysis;
   BEGIN 
      GetSy;
      nestlevel := 0;
      IF sy = modulesy THEN 
         ModulDeclaration;
      END;
      PutSy(endblock);                  (* temporary *)
   END StartBodyAnalysis;

BEGIN                                   (* MCPass3 *)
   StartBodyAnalysis;
   TermInOut;
   IF ErrorsFound THEN
      Stop(2);
   END;
   EndStorage; (* save dynamic storage *)
END MCPass3. 
