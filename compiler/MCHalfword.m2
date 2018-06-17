IMPLEMENTATION MODULE MCHalfword; (* AFB 3/84 *)

   FROM StdIO IMPORT FILE, Fputc, Fgetc;
   FROM SysPerror IMPORT Perror;
   FROM MCStop IMPORT Stop;

   PROCEDURE WriteHalfword(f: FILE; c: CARDINAL);
   BEGIN
      IF NOT Fputc(CHR(c DIV 400B), f) OR NOT Fputc(CHR(c MOD 400B), f) THEN
	 Perror("WriteHalfword");
	 Stop(1);
      END;
   END WriteHalfword;

   PROCEDURE ReadHalfword(f: FILE; VAR c: CARDINAL);
      VAR c1, c2: CHAR;
   BEGIN
      IF NOT Fgetc(c1, f) OR NOT Fgetc(c2, f) THEN
	 Perror("ReadHalfword");
	 Stop(1);
      END;
      c := ORD(c1)*400B + ORD(c2);
   END ReadHalfword;

END MCHalfword.
