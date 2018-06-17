DEFINITION MODULE OutLines;

(*
 * OutLines - devide long into lines and output them (ws 7/88)
 * ===========================================================
 *
 *)

   FROM StdIO IMPORT FILE;
   FROM LongStrings IMPORT Long;

   VAR
      MaxLines : INTEGER;

   PROCEDURE SetLong(long : Long);

   PROCEDURE SetFile(file : FILE);

   PROCEDURE DefineLine(relative : BOOLEAN; line : INTEGER) : BOOLEAN;

   PROCEDURE OutAll() : BOOLEAN;

   PROCEDURE OutNext() : BOOLEAN;

   PROCEDURE OutPrev() : BOOLEAN;

   PROCEDURE OutRange(begin,end : INTEGER) : BOOLEAN;

END OutLines. 
