(* Modula-2 Library    -  UNIX System V  -     AFB 5/88 *)
(* (c) Universitaet Ulm, Sektion Informatik, D-7900 Ulm *)
(*%p+*)
DEFINITION MODULE TermInfo;

   FROM SYSTEM IMPORT BYTE;

   CONST
      stringlen = 64;
   TYPE
      String = ARRAY [0..stringlen-1] OF CHAR;
      OutProc = PROCEDURE (CHAR);
      DelayProc = PROCEDURE (CARDINAL);
      Term =
	 RECORD
	 END;

   PROCEDURE SetupTerm(tname: ARRAY OF CHAR; VAR tinfo: Term) : BOOLEAN;

   PROCEDURE Tparm(VAR out: ARRAY OF CHAR; fmt: ARRAY OF CHAR);

   PROCEDURE Tparm1(VAR out: ARRAY OF CHAR; fmt: ARRAY OF CHAR;
		    arg1: ARRAY OF BYTE);

   PROCEDURE Tparm2(VAR out: ARRAY OF CHAR; fmt: ARRAY OF CHAR;
		    arg1: ARRAY OF BYTE;
		    arg2: ARRAY OF BYTE);

   PROCEDURE Tparm3(VAR out: ARRAY OF CHAR; fmt: ARRAY OF CHAR;
		    arg1: ARRAY OF BYTE;
		    arg2: ARRAY OF BYTE;
		    arg3: ARRAY OF BYTE);

   PROCEDURE Tparm4(VAR out: ARRAY OF CHAR; fmt: ARRAY OF CHAR;
		    arg1: ARRAY OF BYTE;
		    arg2: ARRAY OF BYTE;
		    arg3: ARRAY OF BYTE;
		    arg4: ARRAY OF BYTE);

   PROCEDURE Tparm9(VAR out: ARRAY OF CHAR; fmt: ARRAY OF CHAR;
		    arg1: ARRAY OF BYTE;
		    arg2: ARRAY OF BYTE;
		    arg3: ARRAY OF BYTE;
		    arg4: ARRAY OF BYTE;
		    arg5: ARRAY OF BYTE;
		    arg6: ARRAY OF BYTE;
		    arg7: ARRAY OF BYTE;
		    arg8: ARRAY OF BYTE;
		    arg9: ARRAY OF BYTE);

   PROCEDURE Tputs(str: ARRAY OF CHAR; affcnt: CARDINAL;
		   outc: OutProc);

   PROCEDURE TputsDelay(str: ARRAY OF CHAR; affcnt: CARDINAL;
			outc: OutProc; delay: DelayProc);

END TermInfo.
