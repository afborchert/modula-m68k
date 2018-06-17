DEFINITION MODULE Environment;

   PROCEDURE GetEnv(name: ARRAY OF CHAR; (* parameter name to be looked for *)
		    VAR text: ARRAY OF CHAR; (* parameter contents *)
		    VAR ok: BOOLEAN);

   PROCEDURE EnvPar(index: CARDINAL; (* ranging [0.. #parameters-1] *)
		    VAR text: ARRAY OF CHAR; (* "name=contents" *)
		    VAR ok: BOOLEAN);

END Environment.
