DEFINITION MODULE StrToNum; (* mh 5/85; rev afb 4/86: StrToOct/StrToHex *)

   PROCEDURE StrToCard(str: ARRAY OF CHAR; VAR card: CARDINAL): BOOLEAN;
	(* converts str to the CARDINAL card. Leading spaces, tabs and new-
	 * lines are ignored. Returns FALSE if str is not of the syntax:
	 *   [+] {digit} , or if the resulting number exceeds CARDINAL range.
	 *)

   PROCEDURE StrToInt(str: ARRAY OF CHAR; VAR integ: INTEGER): BOOLEAN;
	(* converts str to the INTEGER integ in analogue manner.
	 * Required syntax of str here:  [+|-] {digit} .
	 *)

   PROCEDURE StrToOct(str: ARRAY OF CHAR; VAR card: CARDINAL) : BOOLEAN;

   PROCEDURE StrToHex(str: ARRAY OF CHAR; VAR card: CARDINAL) : BOOLEAN;

END StrToNum.
