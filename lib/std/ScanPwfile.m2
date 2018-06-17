IMPLEMENTATION MODULE ScanPwfile;

   FROM StdIO	IMPORT FILE, Fseek, Fgetc;

   PROCEDURE GetText(pwfile: FILE;
		     VAR text: ARRAY OF CHAR; sepchar: CHAR): BOOLEAN;
   (* EXPORTED *)
      VAR
	 ch: CHAR;
	 pos: CARDINAL;
   BEGIN
      pos := 0;
      LOOP
	 IF ~Fgetc(ch,pwfile) THEN
	    RETURN FALSE
	 END;
	 IF ch = sepchar THEN EXIT END;
	 IF pos <= HIGH(text) THEN
	    text[pos] := ch;
	    INC(pos);
	 END;
      END; (*LOOP*)
      IF pos <= HIGH(text) THEN
	 text[pos] := 0C;
      END;
      RETURN TRUE;
   END GetText;

   PROCEDURE GetNumber(pwfile: FILE;
			VAR number: CARDINAL; sepchar: CHAR): BOOLEAN;
   (* EXPORTED *)

      VAR
	 ch: CHAR;
	 numerical: BOOLEAN;
   BEGIN
      number := 0;
      numerical := TRUE;
      LOOP
	 IF ~Fgetc(ch,pwfile) THEN
	    RETURN FALSE
	 END;
	 IF ch = sepchar THEN
	    RETURN TRUE
	 END;
	 IF numerical THEN
	    IF (ch >= "0") & (ch <= "9") THEN
	       number := number * 10 + (ORD(ch) - ORD("0"));
	    ELSE
	       numerical := FALSE;
	    END;
	 END;
      END; (*LOOP*)
   END GetNumber;

   PROCEDURE ReRead(pwfile: FILE): BOOLEAN;		(* EXPORTED *)
   BEGIN
      RETURN Fseek(pwfile, (*offset*)0, (*whence=beginning*)0 )
   END ReRead;

END ScanPwfile.
