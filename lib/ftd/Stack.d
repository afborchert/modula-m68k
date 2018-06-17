DEFINITION MODULE Stack;

   FROM SYSTEM IMPORT BYTE, ADDRESS;
   
   TYPE 
      Stack;

   PROCEDURE Create(VAR s : Stack; noelem : CARDINAL; elem : ARRAY OF BYTE);

   PROCEDURE Dispose (VAR s : Stack);

   PROCEDURE Clear(s : Stack);

   PROCEDURE Elems(s : Stack) : CARDINAL;

   PROCEDURE Push(s : Stack; elem : ARRAY OF BYTE) : BOOLEAN;

   PROCEDURE Pop(s : Stack; VAR elem : ARRAY OF BYTE) : BOOLEAN;

   PROCEDURE Top(s : Stack; pos : CARDINAL; VAR elem : ARRAY OF BYTE) : BOOLEAN;

   PROCEDURE Adr(s : Stack; pos : CARDINAL; VAR add : ADDRESS) : BOOLEAN;


END Stack.
