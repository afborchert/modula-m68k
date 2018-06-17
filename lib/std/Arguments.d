DEFINITION MODULE Arguments;	(* mh 5/85 *)
				(* rev mh 6/88 *)
(*
 *	This module reads options and other arguments from the command
 *	line. An argument "-" or "--" stops option reading. "-", how-
 *	ever, will be delivered as argument then, whereas "--" will not.
 *)

(*
 *	Example:
 *
 *	xflag := FALSE;
 *	string := defaultstring;
 *	number := 1;
 *	InitArgs("[-x] [-s string] [-nnn] [file]...");
 *	WHILE GetFlag(flag) DO
 *	   CASE flag OF
 *	      "x":  xflag := TRUE;
 *	   |  "s":  FetchString(string);
 *	   |  "0".."9":
 *	            UngetOpt;
 *	            FetchCard(number);
 *	      ELSE  Usage
 *	   END;
 *	END; (*WHILE GetFlag*)
 *	WHILE GetArg(filename) DO
 *	   IF StrCmp(filename,"-") = 0 THEN
 *	      (* process stdin *)
 *	   ELSE
 *	      (* process filename *)
 *	   END;
 *	END; (*WHILE GetArg*)
 *)

   PROCEDURE InitArgs(infostring: ARRAY OF CHAR);
	(* specifies infostring and (re)starts the reading cyclus *)

   PROCEDURE Usage;
	(* prints 'Usage: command infostring' on stderr and aborts
	 * program execution. FetchString, FetchCard and FetchInt call
	 * this procedure automatically in case of errors.
	 *)

   PROCEDURE GetFlag(VAR flag: CHAR): BOOLEAN;
	(* tries to read one flag, i.e. a character within a string containing
	 * a leading '-',from the argument list and returns TRUE if successful.
	 *)

   PROCEDURE GetOpt( VAR flag: CHAR; VAR plus: BOOLEAN): BOOLEAN;
	(* reads one character within a string starting in '+' or '-'.
	 *)

   PROCEDURE FetchString(VAR string: ARRAY OF CHAR);
	(* The procedures FetchXXX try to read data of type XXX from
	 * the argument list.
	 *)

   PROCEDURE FetchCard(  VAR number: CARDINAL);
	(* syntax of cardinal arguments:  [+]{digit}  *)

   PROCEDURE FetchInt(   VAR number: INTEGER);
	(* syntax of integer arguments:  [+|-]{digit}  *)

   PROCEDURE FetchOct(   VAR number: CARDINAL);
	(* syntax of octal arguments:  [+]{octdigit}  *)

   PROCEDURE FetchHex(   VAR number: CARDINAL);
	(* syntax of hexadecimal arguments:  [+]{hexdigit}  *)

   PROCEDURE GetArg(VAR argument: ARRAY OF CHAR): BOOLEAN;
	(* reads one argument or returns FALSE if all are read. *)

   PROCEDURE UngetArg;
	(* pushes the argument that has been read just before
	 * back to the argument list.
	 *)

   PROCEDURE UngetOpt;
	(* pushes the flag or option that has been read just before
	 * back to the argument list.
	 *)

   PROCEDURE AllArgs;
	(* calls 'Usage' if any arguments are not yet read. *)

END Arguments. 
