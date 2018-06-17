(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
IMPLEMENTATION MODULE MCP4Types; (* AFB 9/83 *)
                                 (* REV AFB 5/84: PROCEDURE ByteSize *)

  FROM MCBase IMPORT Stptr, Stset, charptr, boolptr, intptr, cardptr,
     realptr, bitsetptr, addrptr, Structform, maxint,
     onebyte, oneword, doubleword, longintptr, longcardptr, longrealptr;
  FROM MCP4AttributSys IMPORT Attribut, ArithmeticType;
  FROM MCP4Global IMPORT Assert;
  FROM Storage IMPORT ALLOCATE;

  PROCEDURE ByteSize(fsp: Stptr): BOOLEAN;   
     CONST byteSize = Stset{bools, bytes, chars};
  BEGIN   
     WITH fsp^ DO
        RETURN (form IN byteSize) OR
               (form = subranges) AND ByteSize(scalp);
     END;
  END ByteSize;  

  PROCEDURE IsSetType(fsp:Stptr): BOOLEAN;   
  BEGIN   
    RETURN fsp^.form IN Stset{sets, bigsets};
  END IsSetType;  

  PROCEDURE IsArrayType(fsp:Stptr): BOOLEAN;   
  BEGIN   
    RETURN fsp^.form=arrays  
  END IsArrayType;  

  PROCEDURE SizeType(VAR fat: Attribut): INTEGER;   
    (* Return the size of the entity given by fat *)
  BEGIN   
    WITH fat DO  
      RETURN typtr^.size  
    END;
  END SizeType;  

  PROCEDURE BaseType(ft: Stptr): Stptr;
     VAR int, car, real: BOOLEAN;
  BEGIN
    WITH ft^ DO
       CASE ft^.form OF
       | subranges: RETURN BaseType(scalp);
       | longreals: RETURN realptr;
       | longints:  RETURN intptr;
       | longcards: RETURN cardptr;
       | setoftypes:
            int := Stset{longints, ints} * typeset # Stset{};
            car := Stset{longcards, cards} * typeset # Stset{};
            real := Stset{longreals, reals} * typeset # Stset{};
            IF real THEN RETURN realptr END;
            IF int AND car THEN RETURN intcarptr END;
            IF int THEN RETURN intptr END;
	    IF car THEN RETURN cardptr END;
	    Assert(FALSE);
       ELSE
         RETURN ft;
       END;
    END;
  END BaseType;

  PROCEDURE TestBaseType(ft: Stptr): Stptr;
  BEGIN
    RETURN BaseType(ft)
  END TestBaseType;

  PROCEDURE ResultType(VAR fat1, fat2: Attribut): Stptr;
    VAR ft1, ft2: Stptr;
  BEGIN
    ft1 := BaseType(fat1.typtr);
    ft2 := BaseType(fat2.typtr);
    IF (ft1=intptr) OR (ft2=intptr) THEN RETURN intptr END;
    IF (ft1=cardptr) OR (ft2=cardptr) THEN RETURN cardptr END;
    IF ft1=boolptr THEN RETURN boolptr END;
    IF ft1^.form=enums THEN RETURN intptr END;
    IF (ft1=addrptr) OR (ft2=addrptr) THEN RETURN cardptr END;
    IF ft1^.form = bigsets THEN RETURN ft1 END;
    IF IsSetType(ft1) THEN RETURN bitsetptr END;
    (*IF ft1=charptr THEN RETURN charptr END;*)(* both arithmetic 
                                                  are correct *)
    IF (ft1=realptr) OR (ft2=realptr) THEN RETURN realptr END;
    (* IF ft1^.form = hides THEN RETURN intptr END; *)
    RETURN intcarptr
  END ResultType;

  PROCEDURE Arithmetic(VAR fat1, fat2: Attribut): ArithmeticType;
    VAR tptr: Stptr;
  BEGIN
    tptr := ResultType(fat1, fat2);
    IF tptr=cardptr THEN RETURN unSigned END;
    IF tptr=intptr THEN RETURN signed END;
    IF tptr=intcarptr THEN RETURN unSigned END;
    (*IF tptr=charptr THEN RETURN unSigned END;*)
    IF tptr=boolptr THEN RETURN logical END;
    IF tptr=bitsetptr THEN RETURN bitwise END;
    IF tptr^.form = bigsets THEN RETURN bitwise END;
    IF tptr=realptr THEN RETURN floating END;
    RETURN unSigned
  END Arithmetic;

  PROCEDURE SimpleType(VAR fat: Attribut): BOOLEAN;   
    (* Returns fat describes an simple typed attribut *)
  BEGIN  
    WITH fat.typtr^ DO  
      RETURN (size <= doubleword) AND (form<>arrays) AND (form<>records)  
    END  
  END SimpleType;  

BEGIN
   NEW(intcarptr);
   intcarptr^ := cardptr^;
END MCP4Types.
