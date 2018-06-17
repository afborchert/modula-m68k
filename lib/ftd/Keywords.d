DEFINITION MODULE Keywords;


   VAR
      ok : BOOLEAN;

   PROCEDURE DefineKey (string: ARRAY OF CHAR; key: INTEGER);
   (* Define string to be a keyword with value key *)


   PROCEDURE IsKey (string: ARRAY OF CHAR; VAR key: INTEGER) : BOOLEAN;
   (* Returns TRUE if string is a keyword; key holds the value *)


END Keywords. 
