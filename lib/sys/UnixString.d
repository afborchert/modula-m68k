DEFINITION MODULE UnixString;

   CONST
      BufSiz = 512;
   TYPE
      Buffer = ARRAY[0..BufSiz-1] OF CHAR;

   (* copy str into buf and append null byte *)

   PROCEDURE Copy(VAR buf: Buffer; str: ARRAY OF CHAR);

END UnixString.
