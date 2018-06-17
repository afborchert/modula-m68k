DEFINITION MODULE StrSpec;		(* gsk 1/85 *)

   PROCEDURE StrPartCpy ( VAR target           : ARRAY OF CHAR;
			      source           : ARRAY OF CHAR;
			      position, number : CARDINAL );

   PROCEDURE StrDel ( VAR target           : ARRAY OF CHAR ;
			  position, number : CARDINAL );

   PROCEDURE StrIns ( VAR target      : ARRAY OF CHAR;
			  insertion   : ARRAY OF CHAR;
			  position    : CARDINAL );

   PROCEDURE StrPos ( source, search : ARRAY OF CHAR ) : CARDINAL;

END StrSpec.
