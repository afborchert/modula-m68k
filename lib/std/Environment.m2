IMPLEMENTATION MODULE Environment; (* AFB 4/85 *)

   FROM SysLocations IMPORT Environment;
   FROM SYSTEM IMPORT ADDRESS, WORD;

   (* $T- *)
   (* $R- *)

   CONST
      MaxEnv = 1;
      MaxText = 1;
   TYPE
      ArgList = POINTER TO ARRAY[0..MaxEnv-1] OF
			POINTER TO ARRAY[0..MaxText-1] OF CHAR;
   VAR
      Env: ArgList;
      MaxEnvIndex: CARDINAL;
      NULL: ADDRESS;

   PROCEDURE GetEnv(name: ARRAY OF CHAR; (* parameter name to be looked for *)
		    VAR text: ARRAY OF CHAR; (* parameter contents *)
		    VAR ok: BOOLEAN);
      VAR index, i, j, namelen: CARDINAL;
   BEGIN
      ok := TRUE;
      namelen := 0;
      WHILE (namelen <= HIGH(name)) AND (name[namelen] <> 0C) DO
	 INC(namelen);
      END;
      FOR index := 0 TO MaxEnvIndex-1 DO
	 i := 0;
	 (* compare parameter names *)
	 WHILE (i < namelen) AND (Env^[index]^[i] = name[i]) DO
	    INC(i);
	 END;
	 IF (i = namelen) AND (Env^[index]^[i] = '=') THEN (* found ? *)
	    (* copy parameter contents *)
	    FOR j := 0 TO HIGH(text) DO
	       INC(i);
	       text[j] := Env^[index]^[i];
	       IF text[j] = 0C THEN
		  RETURN
	       END;
	    END;
	    RETURN
	 END;
      END;
      ok := FALSE;
   END GetEnv;

   PROCEDURE EnvPar(index: CARDINAL; (* ranging [0.. MaxEnvIndex-1] *)
		    VAR text: ARRAY OF CHAR; (* "name=contents" *)
		    VAR ok: BOOLEAN);
      VAR i: CARDINAL;

   BEGIN
      IF index >= MaxEnvIndex THEN
	 ok := FALSE;
      ELSE
	 ok := TRUE;
	 FOR i := 0 TO HIGH(text) DO
	    text[i] := Env^[index]^[i];
	    IF text[i] = 0C THEN
	       RETURN
	    END;
	 END;
      END;
   END EnvPar;

BEGIN
   Env := Environment;
   MaxEnvIndex := 0;
   NULL := 0;
   WHILE Env^[MaxEnvIndex] <> NULL DO
      INC(MaxEnvIndex);
   END;
END Environment.
