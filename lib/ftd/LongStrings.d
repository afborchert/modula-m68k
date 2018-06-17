(* 
 *    LongStrings- Allocate and handle strings of variable length (ws 6/88)
 *    ======================================================================
 *
 *
 *)

DEFINITION MODULE LongStrings;

FROM SYSTEM IMPORT ADDRESS;
FROM StdIO IMPORT FILE;

CONST
   LastChar = -1;
   NotFound = -2;

TYPE 
   Long;

   PROCEDURE Alloc (VAR long : Long);

   PROCEDURE Dispose (VAR long : Long);

   PROCEDURE Free (VAR long : Long);

   PROCEDURE CutLong(long : Long; newsize : CARDINAL);

   PROCEDURE ClearLong (long : Long);

   PROCEDURE ValidLong(long : Long) : BOOLEAN;

   PROCEDURE StrAdr(long : Long) : ADDRESS;

   PROCEDURE StrSize(long : Long) : CARDINAL;
   

   PROCEDURE AddString(long : Long; text : ARRAY OF CHAR);

   PROCEDURE AddChar(long : Long; char : CHAR);

   PROCEDURE AddBytes(long : Long; add : ADDRESS; n : CARDINAL);
   (* AddBytes must not be used for appending long to itself!!!*)

   PROCEDURE GetChar (long : Long; index : INTEGER) : CHAR;

   PROCEDURE CountChar (long : Long; char : CHAR) : CARDINAL;

   PROCEDURE FindChar(long : Long; char : CHAR; offset, count : INTEGER) 
      : INTEGER;
 
   PROCEDURE Lwrite(long : Long; file : FILE) : BOOLEAN;

   PROCEDURE LwritePart(long : Long; from, to : INTEGER; file : FILE) 
      : BOOLEAN;

   (* Echo all output via Lwrite or LwritePart to echofile. *)

   PROCEDURE Echo(origin,echo : FILE) : BOOLEAN;

   PROCEDURE NoEcho(origin,echo : FILE) : BOOLEAN;

END LongStrings.
