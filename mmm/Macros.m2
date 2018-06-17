(* Modula-2 makefile generator - UNIX System V - AFB 3/87 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
IMPLEMENTATION MODULE Macros;

   FROM Environment IMPORT GetEnv;
   FROM Strings IMPORT StrCpy, StrCmp;
   FROM Out IMPORT WriteString, WriteLn, WriteTab;
   FROM Options IMPORT lookforenv;
   FROM Storage IMPORT ALLOCATE;

   TYPE
      Text = ARRAY [0..63] OF CHAR;
      MacroRef = POINTER TO Macro;
      Macro =
	 RECORD
	    name, val: Text;
	    link: MacroRef;
	 END;
   VAR
      macros: MacroRef;

   PROCEDURE EnterMacro(mname, mval: ARRAY OF CHAR);
      VAR new: MacroRef;
   BEGIN
      IF mval[0] <> 0C THEN
	 NEW(new);
	 WITH new^ DO
	    link := macros;
	    StrCpy(name, mname);
	    StrCpy(val, mval);
	 END;
	 macros := new;
      END;
   END EnterMacro;

   PROCEDURE WriteMacro(mname, default: ARRAY OF CHAR);
      VAR
	 ok: BOOLEAN;
	 text: Text;
	 mp: MacroRef;
   BEGIN
      WriteString(mname); WriteString(" ="); WriteTab;
      mp := macros;
      WHILE mp <> NIL DO
	 WITH mp^ DO
	    IF StrCmp(name, mname) = 0 THEN
	       WriteString(val); WriteLn;
	       RETURN
	    END;
	    mp := link;
	 END;
      END;
      IF lookforenv THEN
	 GetEnv(mname, text, ok);
      ELSE
	 ok := FALSE;
      END;
      IF ok THEN
	 WriteString(text);
      ELSE
	 WriteString(default);
      END;
      WriteLn;
   END WriteMacro;

BEGIN
   macros := NIL;
END Macros.
