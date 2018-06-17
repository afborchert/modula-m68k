(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
IMPLEMENTATION MODULE MCP4Scanner;      (* rev AFB 8/83 *)

   FROM MCBase IMPORT Symbol, Idptr, Stptr, Stringptr;
   FROM MCP4In IMPORT ReadInputWord, ReadInputHalfword;
   FROM MCP4CodeSys IMPORT EmitEQU, star, EmitComment, EmitLn;
   FROM MCP4Public IMPORT Rflag, lflag, Sflag;
   FROM Conversions IMPORT ConvertCardinal;

   (* (* from definition module *)
   VAR 
      position             : INTEGER;
      line                 : INTEGER;
      sy                   : Symbol;
      val                  : INTEGER;
      nptr                 : Idptr;
      cstPtr               : Stptr;
      cString              : Stringptr;
      controlRangeCheck    : BOOLEAN;
      arithmeticRangeCheck : BOOLEAN;
      pline                : INTEGER;
   *)

   PROCEDURE OptionCode;
   (* Treat a compiler directive *)
   BEGIN 
      IF CHR(val) = 'T' THEN 
         GetSymbol;
         controlRangeCheck := sy = plus 
      ELSIF CHR(val) = 'R' THEN 
         GetSymbol;
         arithmeticRangeCheck := sy = plus 
      ELSE 
         GetSymbol;
      END;
      GetSymbol 
   END OptionCode;

   VAR lastline: CARDINAL;

   PROCEDURE GenLineNumber(line: CARDINAL);
      VAR field: ARRAY[0..8] OF CHAR;
          index: CARDINAL;
   BEGIN
      IF line <= lastline THEN
         RETURN
      END;
      lastline := line;
      ConvertCardinal(line, 1, field);
      FOR index := HIGH(field) TO 2 BY -1 DO
         field[index] := field[index-2];
      END;
      IF Sflag THEN
         field[0] := "@";
         field[1] := " ";
         EmitComment(field);
      END;
      IF lflag THEN
	 IF pline <> 0 THEN
	    EmitLn(line);
	 END;
      END;
   END GenLineNumber;

   VAR
      gen: BOOLEAN; (* line number has to be generated *)

   PROCEDURE GetSymbol;
    (* Read one Symbol (skipping the eol) and set exported variables *)
      VAR 
         i: INTEGER;
   BEGIN 
      IF gen THEN
	 GenLineNumber(line);
	 gen := FALSE;
      END;
      REPEAT 
         ReadInputHalfword(i);
         position := i MOD 400B;
         sy := Symbol(i DIV 400B);    (* no sign extension *)
         IF (sy = namesy) OR (sy = modulesy) OR (sy = proceduresy) THEN 
            ReadInputWord(nptr)
         ELSIF sy = eol THEN 
            ReadInputWord(line);
	    IF pline <> 0 THEN
	       INC(pline);
	    END;
            IF Sflag OR lflag THEN
	       gen := TRUE;
            END;
         ELSIF sy = field THEN 
            ReadInputWord(val)
         ELSIF sy = option THEN 
            ReadInputWord(val);
            OptionCode 
         ELSIF sy = anycon THEN 
            ReadInputWord(cstPtr);
            ReadInputWord(cString);
            val := INTEGER(cString)
         END;
      UNTIL sy <> eol 
   END GetSymbol;

   PROCEDURE Skip(x1, x2: Symbol);
   BEGIN 
      WHILE (sy <> endsy) AND (sy <> x1) AND (sy <> x2) DO 
         IF (sy = casesy) OR (sy = ifsy) OR (sy = withsy) OR (sy = loopsy) OR
	    (sy = forsy) OR (sy = whilesy) THEN 
            GetSymbol;
            Skip(endsy, endsy)
         END;
         GetSymbol 
      END 
   END Skip;

BEGIN                                   (* init scanner *)
   IF Rflag THEN
      controlRangeCheck := FALSE;
      arithmeticRangeCheck := FALSE;
   ELSE
      controlRangeCheck := TRUE;
      arithmeticRangeCheck := TRUE;
   END;
   lastline := 0;
   line := 1; pline := 0;
   gen := TRUE;
END MCP4Scanner. 
