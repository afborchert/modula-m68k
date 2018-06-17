(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)

DEFINITION MODULE MCBase; (* LG / CHJ / AFB *)

   FROM SYSTEM IMPORT ADDRESS;

   TYPE
      Direction = (forward, backward); (* stack direction *)

   CONST

      (* machine dependent constants *)
      (* MC68020 *)
      BitsPerWord = 32;
      BitsPerLongWord = 32;
      onebyte = 1;                     (* storage size of one byte *)
      oneword = 4 * onebyte;           (* storage size of one word *)
      longword = oneword;              (* storage size for LONGCARD + LONGINT *)
      doubleword = 2 * oneword;        (* storage size of one doubleword *)
      realsize = doubleword;           (* storage size of REAL *)
      longrealsize = realsize;         (* storage size of LONGREAL *)
      procmarkspace = 2 * oneword;     (* space used for procedure mark *)
				       (* static link counts as parameter *)
      maxcard = 37777777777B;          (* 2^BitsPerWord - 1 *)
      maxint = maxcard DIV 2;
      longmaxcard = maxcard;           (* 2^BitsPerLongWord - 1 *)
      longmaxint = longmaxcard DIV 2;
      stringmax = 256;                 (* maximal string length *)

      levmax = BitsPerWord-1;          (* maximal nesting level *)
      modnamlength = 24;               (* maximal length for module identifiers *)
      maxprio = 7;                     (* maximal priority allowed *)
      noprio = maxprio + 1;            (* priority value for no priority *)

      stackdir = backward;             (* stack direction *)

   TYPE
      Idptr = POINTER TO Identrec;
      Stptr = POINTER TO Structrec; 

      Structform = (enums, bools, chars, ints, cards, words, subranges, reals,
	 pointers, sets, proctypes, arrays, records, hides, opens, bigsets,
         longints, longcards, longreals, setoftypes, bytes);
      Stset = SET OF Structform; 
      Idclass = (consts, types, vars, fields, pures, funcs, mods, unknown,
	 indrct); 
      Idset = SET OF Idclass;
      Recpart = (fixedpart, tagfield, variantpart);

      Structrec =
	 RECORD
	    size: CARDINAL;	     (* byte size *)
            stidp: Idptr;            (* identifier defining this structure *)
            inlist: BOOLEAN;         (* structure entered into a list *)
	    CASE form: Structform OF 
	    | bools,chars,ints,cards,words,reals,
              longints,longcards,longreals,bytes: (* no field *) 
            | setoftypes:
                  typeset: Stset; (* for constants only *)
	    | enums:
		  fcstp: Idptr;      (* list of identifiers *)
		  cstnr: CARDINAL;   (* maximal ordinal value *)
	    | subranges:
		  scalp: Stptr;      (* base type *)
		  min, max: CARDINAL;
	    | pointers:
		  elemp: Stptr;
	    | sets, bigsets:
		  basep: Stptr;
                  CASE : Structform OF
                  | bigsets:
                       offset, low, high: CARDINAL;
                  | sets: (* no further fields *)
                  END;
	    | arrays: 
		  elp, ixp: Stptr;   (* element and index type *)
		  dyn: BOOLEAN;      (* open array parameter? *)
	    | records: 
		  CASE rpart: Recpart OF 
		  | fixedpart:
			fieldp: Idptr;
			tagp: Stptr;
                  | tagfield:
			fstvarp, elsevarp: Stptr;
			tagtyp: Stptr;
                  | variantpart:
			nxtvarp, subtagp: Stptr;
			varval: CARDINAL;
		  END; (* CASE Recpart *) 
            | proctypes:
		  fstparam: Idptr;   (* pointer to parameter list *)
                  parlength: CARDINAL; (* in bytes *)
		  CASE rkind: Idclass OF 
                  | funcs:
			funcp: Stptr; (* pointer to function type *)
                  | pures: (* no further fields *) 
                  END; 
            | hides, opens: (* conversion from hides to opens *)
                  openstruc: Stptr; (* used for opens *)
            END; (* case Structform *)
         END; (* RECORD *)

      Varkind = (noparam, valparam, varparam, copyparam);
      Kindvar = (global, local, absolute, separate); 
      Spellix = CARDINAL; 
      Listptr = POINTER TO Listrec;
      Listrec =
	 RECORD
	    element: Idptr;
	    next: Listptr;
	 END;
      Stringptr = POINTER TO Stringval;
      Stringval =
	 RECORD
            label: ADDRESS; (* see Pass4 *)
            valentry: CARDINAL; (* POINTER TO ARRAY OF CHAR *)
	    slink: Stringptr;
         END;
      SetValuePtr = POINTER TO SetValue;
      SetValue =
         RECORD
            label: ADDRESS; (* see Pass4 *)
            size: CARDINAL; (* in words (= TSIZE(BITSET)) *)
            offset: CARDINAL;
            valentry: ADDRESS; (* POINTER TO ARRAY OF BITSET *)
            slink: SetValuePtr;
         END;
      Constval =
	 RECORD
	    CASE : Structform OF
              arrays: (* for string constants only *)
		  svalue: Stringptr;
            | reals:
                  rvalue: POINTER TO REAL;
            | longreals:
                  lrvalue: POINTER TO LONGREAL;
            | bigsets:
                  setvalue: SetValuePtr;
            ELSE (* oneword constants *)
                  value : CARDINAL;
	    END;
         END;
      Keyarr = ARRAY [0..2] OF CARDINAL;
      Modnamarr = ARRAY [0..modnamlength-1] OF CHAR;
      Levrange = [0..levmax]; 

      Stpures = (argvp, decp, disp, exlp, halp, incp, inlp, newp, nprp, trsp);
      Stfuncs = (absf, adrf, argcf, capf, chrf, fltf, higf, minf, maxf, oddf,
         ordf, sizf, trcf, tszf, uxf, uxff, uxsf, valf);

      Identrec =
	 RECORD 
           name: Spellix;
           link: Idptr; 
           CASE : BOOLEAN OF
           | FALSE: nxtidp: Idptr;
           | TRUE:  idtyp: Stptr;
	   END;
           globmodp: Idptr; (* pointer to global module *)
           CASE klass: Idclass OF 
           | types: (* no further fields *)
           | consts, unknown: (* unknown may convert to consts *)
		 cvalue: Constval; 
	   | vars: 
		 indaccess: BOOLEAN; (* indirect access to value *)
		 state: Kindvar;
		 vkind: Varkind;
		 vlevel: Levrange;
		 vaddr: CARDINAL; (* offset *) 
		 vlink: Idptr;     (* variables or parameters *)
           | fields:
		 fldaddr: CARDINAL;
           | pures, funcs, mods: 
		 CASE isstandard: BOOLEAN OF
		 | TRUE:
		       CASE : Idclass OF
		       | pures: pname: Stpures;
		       | funcs: fname: Stfuncs;
                       END; (* CASE Idclass *) 
                 | FALSE: 
                       procnum: CARDINAL;
                       locp: Idptr;
                       msp: Listptr; 
                       plev: Levrange;
                       varlength: CARDINAL;
                       priolev: CARDINAL;
                       externalaccess: BOOLEAN;
                       CASE : Idclass OF
                       | pures, funcs:
			     locvarp: Idptr; (* local variables, no parameters *)
		       | mods:
                             impp: Listptr;
                             expp: Idptr;
                             qualexp: BOOLEAN;
                             CASE globalmodule: BOOLEAN OF
                             | FALSE: (* no further field *)
                             | TRUE:
				   globvarp: Idptr; (* global variables *)
                                   modnum: CARDINAL;
                                   modulekey: Keyarr;
				   identifier: Modnamarr;
                             END (* CASE globalmodule *)
                       END (* CASE Idclass *)
		 END (* CASE isstandard *);
            | indrct: (* no further fields *) 
           END (* CASE Idclass *) 
	 END; (* RECORD *)

   (* DON'T CHANGE any things in the variable list without *)
   (* inspecting the compiler specific storage module      *)
   (* which saves the global variables into a interpass    *)
   (* file !!!                                             *)

   VAR 
      boolptr    : Stptr;   (* structure of type BOOLEAN *) 
      charptr    : Stptr;   (* structure of type CHAR    *) 
      intptr     : Stptr;   (* structure of type INTEGER *) 
      cardptr    : Stptr;   (* structure of type CARDINAL *)
      realptr    : Stptr;   (* structure of type REAL *)
      procptr    : Stptr;   (* structure of type PROC *)  
      bitsetptr  : Stptr;   (* structure of type BITSET  *) 
      longcardptr: Stptr;   (* structure of type LONGCARD *)
      longintptr : Stptr;   (* structure of type LONGINT *)
      longrealptr: Stptr;   (* structure of type LONGREAL *)
      wordptr    : Stptr;   (* structure of type WORD    *) 
      byteptr    : Stptr;   (* structure of type BYTE    *)
      addrptr    : Stptr;   (* structure of type ADDRESS *) 
      processptr : Stptr;   (* structure of type PROCESS *) 
      strptrs    : ARRAY [0 .. 20] OF Stptr;
                            (* table to string-structure entries *)
      substptr   : Idptr;   (* list of procedures to be substituted *)
      root       : Idptr;   (* root of standardname entries  *) 
      mainmodp   : Idptr;   (* pointer to main module *)  
      sysmodp    : Idptr;   (* pointer to module SYSTEM *)
      globvarnext: CARDINAL;(* next address for global variables *)
      procnumber : CARDINAL;(* number of procedures in program *)
      stringroot : Stringptr;(* chain of strings to be loaded *)
      bigsetroot : SetValuePtr; (* chain of big set constants *)
  
      modrev     : BOOLEAN; (* if on: revised Modula-2 version *)
      modrev2    : BOOLEAN; (* if on: 2nd revision = 3rd ed. of Wirth's book *)
      loading    : BOOLEAN; (* if on: load compiler into swap area only *)
      ismain     : BOOLEAN; (* true: this is the global main module *)

   TYPE
      Symbol =
	  (eop,                                                      (*   0B *)
  (* p1 *) andsy,divsy,times,slash,modsy,notsy,plus,minus,orsy,      (*  11B *)
	   eql,neq,grt,geq,lss,leq,insy,                             (*  20B *)
           lparent,rparent,lbrack,rbrack,lconbr,rconbr,              (*  26B *)
           comma,semicolon,period,colon,range,                       (*  33B *)
           constsy,typesy,varsy,arraysy,recordsy,variant,setsy,      (*  42B *)
           pointersy,tosy,arrow,hidden,                              (*  46B *)
           importsy,exportsy,fromsy,qualifiedsy,                     (*  52B *)
           codesy,beginsy,                                           (*  54B *)
           casesy,ofsy,ifsy,thensy,elsifsy,elsesy,loopsy,            (*  63B *)
           exitsy,repeatsy,untilsy,whilesy,dosy,withsy,              (*  71B *)
           forsy,bysy,returnsy,becomes,endsy,                        (*  76B *)
           call,endblock,                                            (* 100B *)
           definitionsy,implementationsy,proceduresy,modulesy,       (* 104B *)
           symbolsy,                                                 (* 105B *)
           ident,intcon,cardcon,intcarcon,realcon,charcon,bigsetcon, (* 114B *)
           stringcon,                                                (* 115B *)
           option,errorsy,eol,                                       (* 120B *)
  (* p2 *) namesy,                                                   (* 121B *)
  (* p3 *) field,anycon);                                            (* 123B *)

END MCBase.
