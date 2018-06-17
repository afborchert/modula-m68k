IMPLEMENTATION MODULE W;

   FROM SYSTEM IMPORT ADDRESS, ADR, BYTE;
   FROM Bytes IMPORT PINC,PDEC;
   FROM Printf IMPORT Printf, FmtExitCode, Default, WabortA, WabortP,
      ErrorReaction;
   FROM LongStrings IMPORT Long, Alloc, StrAdr, StrSize, ClearLong;
   FROM Windows IMPORT Done, WindowWrite, FlushWindow, WindowAttributes,
      GetWindowAttributes, SetWindowAttributes, WindowAtSet, Window,
      SetWindowPos, GetWindowSize;
   FROM Chars IMPORT lf, cr, CharSet;

   IMPORT Windows, E;

   VAR 
      ErrorMode : BITSET;
      DONE      : FmtExitCode;
      OutputText: Long;
      x         : CHAR;

   PROCEDURE setmode (mode : BITSET);

   BEGIN 
      ErrorMode := mode;
      DONE := Success;
   END setmode;

   PROCEDURE getmode (VAR mode : BITSET);

   BEGIN 
      mode := ErrorMode;
      DONE := Success;
   END getmode;

   PROCEDURE success() : FmtExitCode;

   BEGIN 
      RETURN DONE;
   END success;

   PROCEDURE done() : BOOLEAN;

   BEGIN 
      RETURN DONE = Success;
   END done;

   PROCEDURE append0(win : Window; fmt : ARRAY OF CHAR);

   BEGIN 
      DONE := Printf(OutputText,0,fmt,x,x,x,x,x,x,x,x);
      Finish(TRUE,0,win,0,0,fmt);
   END append0;

   PROCEDURE append1(win : Window; fmt : ARRAY OF CHAR; i1 : ARRAY OF BYTE);

   BEGIN 
      DONE := Printf(OutputText,1,fmt,i1,x,x,x,x,x,x,x);
      Finish(TRUE,1,win,0,0,fmt);
   END append1;

   PROCEDURE append2(win : Window; fmt : ARRAY OF CHAR; i1,i2 : ARRAY OF BYTE)
      ;

   BEGIN 
      DONE := Printf(OutputText,2,fmt,i1,i2,x,x,x,x,x,x);
      Finish(TRUE,2,win,0,0,fmt);
   END append2;

   PROCEDURE append3(win : Window; fmt : ARRAY OF CHAR; i1,i2, i3 : ARRAY OF 
      BYTE);

   BEGIN 
      DONE := Printf(OutputText,3,fmt,i1,i2,i3,x,x,x,x,x);
      Finish(TRUE,3,win,0,0,fmt);
   END append3;

   PROCEDURE append4(win : Window; fmt : ARRAY OF CHAR; i1,i2, i3,i4 : 
      ARRAY OF BYTE );

   BEGIN 
      DONE := Printf(OutputText,4,fmt,i1,i2,i3,i4,x,x,x,x);
      Finish(TRUE,4,win,0,0,fmt);
   END append4;

   PROCEDURE append5(win : Window; fmt : ARRAY OF CHAR; i1,i2, i3,i4, i5 : 
      ARRAY OF BYTE);

   BEGIN 
      DONE := Printf(OutputText,5,fmt,i1,i2,i3,i4,i5,x,x,x);
      Finish(TRUE,5,win,0,0,fmt);
   END append5;

   PROCEDURE append6(win : Window; fmt : ARRAY OF CHAR; i1,i2, i3,i4, i5,i6 : 
      ARRAY OF BYTE);

   BEGIN 
      DONE := Printf(OutputText,6,fmt,i1,i2,i3,i4,i5,i6,x,x);
      Finish(TRUE,6,win,0,0,fmt);
   END append6;

   PROCEDURE append7(win : Window; fmt : ARRAY OF CHAR; i1,i2, i3,i4, i5,i6,
      i7 : ARRAY OF BYTE);

   BEGIN 
      DONE := Printf(OutputText,7,fmt,i1,i2,i3,i4,i5,i6,i7,x);
      Finish(TRUE,7,win,0,0,fmt);
   END append7;

   PROCEDURE append8(win : Window; fmt : ARRAY OF CHAR; i1,i2, i3,i4, i5,i6,
      i7 ,i8 : ARRAY OF BYTE);

   BEGIN 
      DONE := Printf(OutputText,8,fmt,i1,i2,i3,i4,i5,i6,i7,i8);
      Finish(TRUE,8,win,0,0,fmt);
   END append8;

   PROCEDURE printf0(win : Window; l,c : CARDINAL; fmt : ARRAY OF CHAR);

   BEGIN 
      DONE := Printf(OutputText,0,fmt,x,x,x,x,x,x,x,x);
      Finish(FALSE,0,win,l,c,fmt);
   END printf0;

   PROCEDURE printf1(win : Window; l,c : CARDINAL; fmt : ARRAY OF CHAR; i1 : 
      ARRAY OF BYTE);

   BEGIN 
      DONE := Printf(OutputText,1,fmt,i1,x,x,x,x,x,x,x);
      Finish(FALSE,1,win,l,c,fmt);
   END printf1;

   PROCEDURE printf2(win : Window; l,c : CARDINAL; fmt : ARRAY OF CHAR; i1,i2 
      : ARRAY OF BYTE);

   BEGIN 
      DONE := Printf(OutputText,2,fmt,i1,i2,x,x,x,x,x,x);
      Finish(FALSE,2,win,l,c,fmt);
   END printf2;

   PROCEDURE printf3(win : Window; l,c : CARDINAL; fmt : ARRAY OF CHAR; i1,i2,
      i3 : ARRAY OF BYTE);

   BEGIN 
      DONE := Printf(OutputText,3,fmt,i1,i2,i3,x,x,x,x,x);
      Finish(FALSE,3,win,l,c,fmt);
   END printf3;

   PROCEDURE printf4(win : Window; l,c : CARDINAL; fmt : ARRAY OF CHAR; i1,i2,
      i3,i4 : ARRAY OF BYTE );

   BEGIN 
      DONE := Printf(OutputText,4,fmt,i1,i2,i3,i4,x,x,x,x);
      Finish(FALSE,4,win,l,c,fmt);
   END printf4;

   PROCEDURE printf5(win : Window; l,c : CARDINAL; fmt : ARRAY OF CHAR; i1,i2,
      i3,i4, i5 : ARRAY OF BYTE);

   BEGIN 
      DONE := Printf(OutputText,5,fmt,i1,i2,i3,i4,i5,x,x,x);
      Finish(FALSE,5,win,l,c,fmt);
   END printf5;

   PROCEDURE printf6(win : Window; l,c : CARDINAL; fmt : ARRAY OF CHAR; i1,i2,
      i3,i4, i5,i6 : ARRAY OF BYTE);

   BEGIN 
      DONE := Printf(OutputText,6,fmt,i1,i2,i3,i4,i5,i6,x,x);
      Finish(FALSE,6,win,l,c,fmt);
   END printf6;

   PROCEDURE printf7(win : Window; l,c : CARDINAL; fmt : ARRAY OF CHAR; i1,i2,
      i3,i4, i5,i6,i7 : ARRAY OF BYTE);

   BEGIN 
      DONE := Printf(OutputText,7,fmt,i1,i2,i3,i4,i5,i6,i7,x);
      Finish(FALSE,7,win,l,c,fmt);
   END printf7;

   PROCEDURE printf8(win : Window; l,c : CARDINAL; fmt : ARRAY OF CHAR; i1,i2,
      i3,i4, i5,i6,i7,i8 : ARRAY OF BYTE);

   BEGIN 
      DONE := Printf(OutputText,8,fmt,i1,i2,i3,i4,i5,i6,i7,i8);
      Finish(FALSE,8,win,l,c,fmt);
   END printf8;

   PROCEDURE Finish(append : BOOLEAN; no : CARDINAL; win : Window; l,c : 
      CARDINAL; VAR fmt : ARRAY OF CHAR);

      VAR 
         add         : ADDRESS;
         size        : CARDINAL;
         fmt3        : ARRAY[0..255] OF CHAR;
         factor      : INTEGER;
         i,j         : CARDINAL;
         cp          : POINTER TO CHAR;
         s           : WindowAtSet;
         set         : BOOLEAN;
         lines, cols : CARDINAL;
         nol         : CARDINAL;
         maxw        : CARDINAL;

   BEGIN 
      IF DONE = Success THEN 
         GetWindowAttributes(win,s);
         IF Done AND NOT append THEN 
            GetWindowSize(win,lines,cols);
         END;
         IF Done AND NOT append AND ((lines <= l) OR (cols <= c)) THEN 
	    DONE := IllegalWindowOffset;
	    Done := FALSE;
	 END;
         IF Done THEN 
            IF NOT append THEN 
               SetWindowAttributes(win,s- WindowAtSet{flushalways});
            ELSE 
               SetWindowAttributes(win,s- WindowAtSet{flushalways}
                  + WindowAtSet{scroll});
            END;
            set := Done;
         END;
         IF Done AND NOT append THEN 
            SetWindowPos(win,l,c);
         END;
         IF Done THEN 
            cp := StrAdr(OutputText);
            size := StrSize(OutputText);
            i := 1;
            j := c;
            nol := l;
            IF NOT append AND NOT (scroll IN s ) THEN 
               LOOP 
                  IF i > size THEN 
                     EXIT;
                  END;
                  IF cp^ = lf THEN 
                     INC(nol);
                     j := 0;
		  ELSIF cp ^= cr THEN
		     j := 0;
                  END;
                  IF (j >= cols) OR (nol >= lines) THEN 
                     Done := FALSE;
                     EXIT;
                  ELSE 
                     INC(j);
                  END;
                  PINC(cp,1);
                  INC(i);
               END;
            END;
            IF NOT Done THEN 
               DONE := WindowTooSmall;
            ELSE 
               cp := StrAdr(OutputText);
               i := 1;
               WHILE Done & (i<= size) DO 
                  WindowWrite(win,cp^);
                  PINC(cp,1);
                  INC(i);
               END;
               IF NOT Done THEN 
                  PDEC(cp,1);
                  DONE := CannotWriteWindow;
               END;
            END;
         END;
         IF Done THEN 
            IF flushalways IN s THEN 
               FlushWindow(win);
               IF NOT Done THEN 
                  DONE := CannotFlushWindow;
               END;
            END;
         END;
         IF set THEN 
            set := Done;
            SetWindowAttributes(win,s);
            Done := set AND Done;
         END;
         IF NOT Done AND (DONE = Success) THEN 
            DONE := CannotAccessWindow;
         END;
      END;
      IF DONE # Success THEN 
	 IF append THEN
            factor := WabortA;
	 ELSE
	    factor := WabortP;
	 END;
         ErrorReaction(DONE,ErrorMode,no,factor,fmt3);
         IF DONE = CannotWriteWindow THEN 
            E.rror5(factor,fmt3,no,l,c,fmt,cp^);
         ELSE 
            E.rror4(factor,fmt3,no,l,c,fmt);
         END;
      END;
      ClearLong(OutputText);
   END Finish;

BEGIN 
   DONE := Undefined;
   ErrorMode := Default;
   Alloc(OutputText);
END W. 
