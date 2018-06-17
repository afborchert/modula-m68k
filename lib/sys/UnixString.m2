IMPLEMENTATION MODULE UnixString;

   PROCEDURE Copy(VAR Buf: Buffer; str: ARRAY OF CHAR);
      VAR index: CARDINAL;
   BEGIN
      (* hope for HIGH(str) < BufSiz !!! *)
      FOR index := 0 TO HIGH(str) DO
         Buf[index] := str[index];
	 IF Buf[index] = 0C THEN RETURN END;
      END;
      Buf[HIGH(str)+1] := 0C;
   END Copy;

END UnixString.
