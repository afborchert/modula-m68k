DEFINITION MODULE SysOpen;

   FROM SYSTEM IMPORT WORD;

   (* oflag: see SystemTypes *)

   PROCEDURE Open(VAR fd: CARDINAL; filename: ARRAY OF CHAR;
                  oflag: WORD) : BOOLEAN;

   PROCEDURE OpenCreat(VAR fd: CARDINAL; filename: ARRAY OF CHAR;
		       oflag: WORD; mode: CARDINAL) : BOOLEAN;

END SysOpen.
