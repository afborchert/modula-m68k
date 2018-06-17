IMPLEMENTATION MODULE P;

   FROM SYSTEM IMPORT ADDRESS, ADR, BYTE;
   FROM StdIO IMPORT stdout;
   FROM Printf IMPORT Printf, FmtExitCode, Default, Pabort, ErrorReaction;
   FROM LongStrings IMPORT Long, Alloc, StrAdr, StrSize, ClearLong,
      Lwrite;
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

   PROCEDURE rintf0(fmt : ARRAY OF CHAR);

   BEGIN 
      Done := Printf(OutputText,0,fmt,x,x,x,x,x,x,x,x);
      Finish(0,fmt);
   END rintf0;

   PROCEDURE rintf1(fmt : ARRAY OF CHAR; i1 : ARRAY OF BYTE);

   BEGIN 
      Done := Printf(OutputText,1,fmt,i1,x,x,x,x,x,x,x);
      Finish(1,fmt);
   END rintf1;

   PROCEDURE rintf2(fmt : ARRAY OF CHAR; i1,i2 : ARRAY OF BYTE);

   BEGIN 
      Done := Printf(OutputText,2,fmt,i1,i2,x,x,x,x,x,x);
      Finish(2,fmt);
   END rintf2;

   PROCEDURE rintf3(fmt : ARRAY OF CHAR; i1,i2,i3 : ARRAY OF BYTE);

   BEGIN 
      Done := Printf(OutputText,3,fmt,i1,i2,i3,x,x,x,x,x);
      Finish(3,fmt);
   END rintf3;

   PROCEDURE rintf4(fmt : ARRAY OF CHAR; i1,i2,i3,i4 : ARRAY OF 
      BYTE );

   BEGIN 
      Done := Printf(OutputText,4,fmt,i1,i2,i3,i4,x,x,x,x);
      Finish(4,fmt);
   END rintf4;

   PROCEDURE rintf5(fmt : ARRAY OF CHAR; i1,i2,i3,i4,i5 : ARRAY OF 
      BYTE);

   BEGIN 
      Done := Printf(OutputText,5,fmt,i1,i2,i3,i4,i5,x,x,x);
      Finish(5,fmt);
   END rintf5;

   PROCEDURE rintf6(fmt : ARRAY OF CHAR; i1,i2,i3,i4,i5,i6 : 
      ARRAY OF BYTE);

   BEGIN 
      Done := Printf(OutputText,6,fmt,i1,i2,i3,i4,i5,i6,x,x);
      Finish(6,fmt);
   END rintf6;

   PROCEDURE rintf7(fmt : ARRAY OF CHAR; i1,i2,i3,i4,i5,i6,i7 : 
      ARRAY OF BYTE);

   BEGIN 
      Done := Printf(OutputText,7,fmt,i1,i2,i3,i4,i5,i6,i7,x);
      Finish(7,fmt);
   END rintf7;

   PROCEDURE rintf8(fmt : ARRAY OF CHAR; i1,i2,i3,i4,i5,i6,i7,i8 : 
      ARRAY OF BYTE);

   BEGIN 
      Done := Printf(OutputText,8,fmt,i1,i2,i3,i4,i5,i6,i7,i8);
      Finish(8,fmt);
   END rintf8;

   PROCEDURE Finish(no : CARDINAL; VAR fmt : ARRAY OF CHAR);

   VAR
      fmt3 : ARRAY[0..255] OF CHAR;
      factor : INTEGER;
      
   BEGIN
      IF (Done = Success) AND NOT Lwrite(OutputText,stdout) THEN
	 Done := CannotWriteStdout;
      END;
      IF Done # Success THEN
	 factor := Pabort;
	 ErrorReaction(Done,ErrorMode,no,factor,fmt3);
	 E.rror2(factor,fmt3,no,fmt);
      END;
      ClearLong(OutputText);
   END Finish;

BEGIN 
   Done := Undefined;
   ErrorMode := Default;
   Alloc(OutputText);
END P. 
