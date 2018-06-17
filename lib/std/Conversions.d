DEFINITION MODULE Conversions;                  (* LG *)
   
  PROCEDURE ConvertOctal(num, len: CARDINAL; VAR str: ARRAY OF CHAR);
    (* conversion of an octal number to a string *) 

  PROCEDURE ConvertHex(num, len: CARDINAL; VAR str: ARRAY OF CHAR);
    (* conversion of a hexadecimal number to a string *) 

  PROCEDURE ConvertCardinal(num, len: CARDINAL; VAR str: ARRAY OF CHAR);   
    (* conversion of a cardinal decimal number to a string *) 

  PROCEDURE ConvertInteger(num: INTEGER; len: CARDINAL;   
                           VAR str: ARRAY OF CHAR); 
    (* conversion of an integer decimal number to a string *) 
     
END Conversions.
