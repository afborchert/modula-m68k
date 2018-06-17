IMPLEMENTATION MODULE F;

   FROM SYSTEM IMPORT ADDRESS, ADR, BYTE;
   FROM StdIO IMPORT FILE;
   FROM Printf IMPORT Printf, FmtExitCode, Default, Fabort, ErrorReaction;
   FROM LongStrings IMPORT Long, ClearLong, Alloc, Lwrite;
   IMPORT E;

   VAR 
      ErrorMode : BITSET;
      Done      : FmtExitCode;
      OutputText: Long;
      x         : CHAR;

   PROCEDURE setmode (mode : BITSET);

   BEGIN 
      ErrorMode := mode;
   END setmode;

   PROCEDURE getmode (VAR mode : BITSET);

   BEGIN
      mode := ErrorMode;
   END getmode;
 
   PROCEDURE success() : FmtExitCode;

   BEGIN
      RETURN Done;
   END success;

   PROCEDURE done() : BOOLEAN;

   BEGIN 
      RETURN Done = Success;
   END done;

   PROCEDURE printf0(f : FILE; fmt : ARRAY OF CHAR);

   BEGIN 
      Done := Printf(OutputText,0,fmt,x,x,x,x,x,x,x,x);
      Finish(0,f,fmt);
   END printf0;

   PROCEDURE printf1(f: FILE; fmt : ARRAY OF CHAR; i1 : ARRAY OF BYTE);

   BEGIN 
      Done := Printf(OutputText,1,fmt,i1,x,x,x,x,x,x,x);
      Finish(1,f,fmt);
   END printf1;

   PROCEDURE printf2(f: FILE; fmt : ARRAY OF CHAR; i1,i2 : ARRAY OF BYTE);

   BEGIN 
      Done := Printf(OutputText,2,fmt,i1,i2,x,x,x,x,x,x);
      Finish(2,f,fmt);
   END printf2;

   PROCEDURE printf3(f: FILE; fmt : ARRAY OF CHAR; i1,i2,i3 : ARRAY OF BYTE);

   BEGIN 
      Done := Printf(OutputText,3,fmt,i1,i2,i3,x,x,x,x,x);
      Finish(3,f,fmt);
   END printf3;

   PROCEDURE printf4(f: FILE; fmt : ARRAY OF CHAR; i1,i2,i3,i4 : ARRAY OF 
      BYTE );

   BEGIN 
      Done := Printf(OutputText,4,fmt,i1,i2,i3,i4,x,x,x,x);
      Finish(4,f,fmt);
   END printf4;

   PROCEDURE printf5(f: FILE; fmt : ARRAY OF CHAR; i1,i2,i3,i4,i5 : ARRAY OF 
      BYTE);

   BEGIN 
      Done := Printf(OutputText,5,fmt,i1,i2,i3,i4,i5,x,x,x);
      Finish(5,f,fmt);
   END printf5;

   PROCEDURE printf6(f: FILE; fmt : ARRAY OF CHAR; i1,i2,i3,i4,i5,i6 : 
      ARRAY OF BYTE);

   BEGIN 
      Done := Printf(OutputText,6,fmt,i1,i2,i3,i4,i5,i6,x,x);
      Finish(6,f,fmt);
   END printf6;

   PROCEDURE printf7(f: FILE; fmt : ARRAY OF CHAR; i1,i2,i3,i4,i5,i6,i7 : 
      ARRAY OF BYTE);

   BEGIN 
      Done := Printf(OutputText,7,fmt,i1,i2,i3,i4,i5,i6,i7,x);
      Finish(7,f,fmt);
   END printf7;

   PROCEDURE printf8(f: FILE; fmt : ARRAY OF CHAR; i1,i2,i3,i4,i5,i6,i7,i8 : 
      ARRAY OF BYTE);

   BEGIN 
      Done := Printf(OutputText,8,fmt,i1,i2,i3,i4,i5,i6,i7,i8);
      Finish(8,f,fmt);
   END printf8;

   PROCEDURE Finish(no : CARDINAL; f : FILE; VAR fmt : ARRAY OF CHAR);

   VAR
      abort,tell : BOOLEAN;
      fmt3 : ARRAY[0..255] OF CHAR;
      factor : INTEGER;

   BEGIN
      IF (Done = Success) AND NOT Lwrite(OutputText,f) THEN
	 Done := CannotWriteFile;
      END;
      IF Done # Success THEN
	 factor := Fabort;
	 ErrorReaction(Done,ErrorMode,no,factor,fmt3);
	 E.rror2(factor,fmt3,no,fmt);
      END;
      ClearLong(OutputText);
   END Finish;

BEGIN 
   Done := Undefined;
   ErrorMode := Default;
   Alloc(OutputText);
END F. 
