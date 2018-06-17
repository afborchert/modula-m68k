DEFINITION MODULE MCDump; (* only for debugging *)

   FROM MCBase IMPORT Stptr, Stset, Idptr;

   PROCEDURE DumpType(mess: ARRAY OF CHAR; sp: Stptr);

   PROCEDURE DumpStset(mess: ARRAY OF CHAR; st: Stset);

   PROCEDURE DumpIdent(mess: ARRAY OF CHAR; ip: Idptr);

END MCDump.
