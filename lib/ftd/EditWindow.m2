IMPLEMENTATION MODULE EditWindow;

   FROM SYSTEM IMPORT ADDRESS;
   FROM Windows IMPORT Window, Done, WindowClear, FlushWindow, WindowWrite,
      SetWindowDisplayMode, SetWindowAttributes, WindowAttributes,
      WindowAtSet , WindowRead, GetWindowSize, GetWindowPos, SetWindowPos,
      WindowChar, IsFunctionKey, ToFunctionKey, GetWindowAttributes,
      SubWindow , CloseWindow;
   FROM Chars IMPORT NonControlS, CtrlD, sp, esc, cr, lf, nul, bs, CharSet, del,
      PunctS, Upper, Lower, UpperS, LowerS,AlphaNumS , DigitS, bel;
   FROM StdIO IMPORT stderr, Fflush;
   FROM FunctionKeys IMPORT FunctionKeySet;
   IMPORT FunctionKeys;
   IMPORT F, E;

   TYPE 
      EditMode = (escape,insert,replace);
      FunctionKey = FunctionKeys.FunctionKey;

   CONST 
      modes = WindowAtSet {flushoninput,movecursor,timeout,funckeys,readcr};
      UP    = TRUE;
      DOWN  = FALSE;
      LEFT  = TRUE;
      RIGHT = FALSE;

      nokey = FunctionKeys.nokey;
      left = FunctionKeys.left;
      right = FunctionKeys.right;
      up = FunctionKeys.up;
      down = FunctionKeys.down;
      home = FunctionKeys.home;
      backspace = FunctionKeys.backspace;

   VAR 
      local   : FunctionKeySet;
      charmap : ARRAY [0C..177C] OF FunctionKey;
      keymap  : ARRAY [MIN(FunctionKey)..MAX(FunctionKey)] OF 
         FunctionKeyReaction;
      once    : BOOLEAN;
      Once    : PROC;
      Modes   : WindowAtSet;

   PROCEDURE EnableSingleCharFK (set : BOOLEAN);

   BEGIN
      IF set THEN
	 INCL(Modes,mapkeys);
      ELSE
	 EXCL(Modes,mapkeys);
      END;
   END EnableSingleCharFK;

   PROCEDURE CallOnce(proc : PROC);

   BEGIN 
      once := TRUE;
      Once := proc;
   END CallOnce;

   PROCEDURE GetWindowLine(win : Window; line : CARDINAL; VAR text : ARRAY OF 
      CHAR; left,right : BOOLEAN);

      VAR 
         lines,cols   :CARDINAL;
         s            : CARDINAL;
         last, i, idx : CARDINAL;

   BEGIN 
      GetWindowSize(win,lines,cols);
      IF NOT Done OR (line >= lines)THEN 
         text[0] := nul;
         RETURN;
      END;
      s := HIGH(text);
      idx := 0;
      IF left THEN 
         WHILE (idx < cols) & (WindowChar(win,line,idx) = sp) DO 
            INC(idx);
         END;
         IF idx = cols THEN 
            text[0] := nul;
            RETURN;
         END;
      END;
      last := cols-1;
      IF right THEN 
         WHILE (last >= idx) AND (WindowChar(win,line,last) = sp) DO 
            DEC(last);
         END;
         IF last < idx THEN 
            text[0] := nul;
            RETURN;
         END;
      END;
      FOR i := 0 TO (last-idx) DO 
         IF i > s THEN 
            text[0] := nul;
            RETURN;
         END;
         text[i] := WindowChar(win,line,idx+i);
      END;
      IF (last-idx) < s THEN 
         text[last-idx+1] := nul;
      END;
   END GetWindowLine;

   PROCEDURE Wait(win : Window);

      VAR 
         set : WindowAtSet;
         ch  : CHAR;

   BEGIN 
      GetWindowAttributes(win,set);
      ClearInput(win);
      SetWindowAttributes(win,WindowAtSet{funckeys,timeout});
      WindowRead(win,ch);
      SetWindowAttributes(win,set);
   END Wait;

   PROCEDURE ReadOneChar(win : Window; ech : BOOLEAN) : CHAR;

      VAR 
         set       : WindowAtSet;
         lns, cols : CARDINAL;
         old,ch    : CHAR;

   BEGIN 
      GetWindowPos(win,lns,cols);
      GetWindowAttributes(win,set);
      ClearInput(win);
      IF ech THEN
	 SetWindowAttributes(win,WindowAtSet{funckeys,timeout,
	    flushoninput,movecursor,echo});
      ELSE
	 SetWindowAttributes(win,WindowAtSet{funckeys,timeout,
	    flushoninput,movecursor});
      END;
      WindowRead(win,ch);
      IF NOT Done OR IsFunctionKey(ch) THEN 
         ch := nul;
      END;
      SetWindowPos(win,lns,cols);
      SetWindowAttributes(win,set);
      RETURN ch;
   END ReadOneChar;

   PROCEDURE EnableLocalMoves(set : FunctionKeySet);

   BEGIN 
      local := LocalMoves * set;
   END EnableLocalMoves;

   PROCEDURE DefineReaction (key : FunctionKey; proc : FunctionKeyReaction);

   BEGIN 
      keymap[key] := proc;
   END DefineReaction;

   PROCEDURE MapChar (char : CHAR; key : FunctionKey);

   BEGIN 
      IF char <= 177C THEN 
         charmap[char] := key;
      END;
   END MapChar;

   CONST
      NoOfBeeps = 5;
   PROCEDURE Beep();

   BEGIN 
      F.printf2(stderr,"%*l",NoOfBeeps,bel);
   END Beep;

   PROCEDURE EditWindow(W : Window; def : BOOLEAN; VAR exitkey : FunctionKey);

      VAR 
         mode    : EditMode;
         lines   : CARDINAL;
         cols    : CARDINAL;
         mline   : CARDINAL;
         mcol    : CARDINAL;
         inits   : WindowAtSet;
         initl   : CARDINAL;
         initc   : CARDINAL;
         insfrom : CARDINAL;
         insto   : CARDINAL;

      PROCEDURE Mark();

      BEGIN 
         GetWindowPos(W,mline,mcol);
      END Mark;

      PROCEDURE Return();

      BEGIN 
         SetWindowPos(W,mline,mcol);
      END Return;

      PROCEDURE Edit();

         VAR 
            ch     : CHAR;
            dir    : BOOLEAN;
            end    : BOOLEAN;
            eow    : BOOLEAN;
            eol    : BOOLEAN;
            return : BOOLEAN;
            isupper: BOOLEAN;
            fx,fy  : CARDINAL;

         PROCEDURE MoveFirst(always : BOOLEAN);

            VAR 
               l,c,x : CARDINAL;

         BEGIN 
            GetWindowPos(W,l,c);
            x := 0;
            WHILE IsSpace(l,x) AND (x < cols) DO 
               INC(x)
            END;
            IF x <= (cols-1) THEN 
               SetWindowPos(W,l,x);
	    ELSIF always THEN
	       SetWindowPos(W,l,0);
            END;
         END MoveFirst;

         PROCEDURE MoveLast(always : BOOLEAN);

            VAR 
               l,c,x : CARDINAL;

         BEGIN 
            GetWindowPos(W,l,c);
            x := cols-1;
            WHILE IsSpace(l,x) AND (x > 0) DO 
               DEC(x)
            END;
            IF NOT IsSpace(l,x) THEN 
               SetWindowPos(W,l,x);
	    ELSIF always THEN
	       SetWindowPos(W,l,0);
            END;
         END MoveLast;

         PROCEDURE Vmove(up : BOOLEAN) : BOOLEAN;

            VAR 
               cline : CARDINAL;
               ccol  : CARDINAL;

         BEGIN 
            GetWindowPos(W,cline,ccol);
            insfrom := ccol;
            insto := ccol;
            IF up & (cline > 0) THEN 
               DEC(cline);
               eow := FALSE;
            ELSIF ~up & (cline < (lines-1)) THEN 
               INC(cline);
            ELSE 
               RETURN FALSE;
            END;
            SetWindowPos(W,cline,ccol);
            RETURN TRUE;
         END Vmove;

         PROCEDURE Hmove(left : BOOLEAN) : BOOLEAN;

            VAR 
               cline : CARDINAL;
               ccol  : CARDINAL;

         BEGIN 
            GetWindowPos(W,cline,ccol);
            IF left & (ccol > 0) THEN 
               DEC(ccol);
               eow := FALSE;
            ELSIF ~left & (ccol < (cols-1)) THEN 
               INC(ccol);
            ELSE 
               RETURN FALSE;
            END;
            SetWindowPos(W,cline,ccol);
            RETURN TRUE;
         END Hmove;

         PROCEDURE ShiftL();

            VAR 
               cline, ccol : CARDINAL;
               col         : CARDINAL;

         BEGIN 
            Mark();
            FOR col := mcol+1 TO cols-1 DO 
               WindowWrite(W,WindowChar(W,mline,col));
            END;
            WindowWrite(W,sp);
            Return();
         END ShiftL;

         PROCEDURE ShiftR();

            VAR 
               cline : CARDINAL;
               ccol  : CARDINAL;
               line  : CARDINAL;
               col   : CARDINAL;
               c1,c2 : CHAR;

            PROCEDURE Shift() : CHAR;

               VAR 
                  ch   : CHAR;
                  l1,l2: CHAR;
                  cl   : CARDINAL;

            BEGIN 
               l1 := WindowChar(W,line,cols-1);
               l2 := WindowChar(W,line,cols-2);
               FOR cl := cols-1 TO col+1 BY -1 DO 
                  ch := WindowChar(W,line,cl-1);
                  SetWindowPos(W,line,cl);
                  WindowWrite(W,ch);
               END;
               IF (l1 = sp) & (l2 = sp) THEN 
                  RETURN nul;
               ELSE 
                  RETURN l1;
               END;
            END Shift;

         BEGIN 
            GetWindowPos(W,cline,ccol);
            line := cline;
            col := ccol;
            c1 := sp;
            REPEAT 
               c2 := Shift();
               SetWindowPos(W,line,col);
               WindowWrite(W,c1);
               c1 := c2;
               INC(line);
               col := 0;
            UNTIL (c1 = 0C) OR (line >= lines);
            SetWindowPos(W,cline,ccol);
         END ShiftR;

         PROCEDURE Eol(left : BOOLEAN);

            VAR 
               cline : CARDINAL;
               ccol  : CARDINAL;
         BEGIN 
            GetWindowPos(W,cline,ccol);
            IF left THEN 
               ccol := 0;
            ELSE 
               ccol := cols-1;
            END;
            SetWindowPos(W,cline,ccol);
         END Eol;

         PROCEDURE LastLine() : BOOLEAN;

            VAR 
               l,c,o : CARDINAL;

         BEGIN 
            GetWindowPos(W,o,c);
            FOR l := o TO lines -1 DO 
               SetWindowPos(W,l,0);
               IF NOT LastCh() THEN 
                  SetWindowPos(W,o,c);
                  RETURN FALSE;
               END;
            END;
            SetWindowPos(W,o,c);
            RETURN TRUE;
         END LastLine;

         PROCEDURE LastCh() : BOOLEAN;

            VAR 
               c : CARDINAL;
         BEGIN 
            Mark();
            FOR c := mcol TO cols-1 DO 
               IF WindowChar(W,mline,c) # sp THEN 
                  RETURN FALSE;
               END;
            END;
            RETURN TRUE;
         END LastCh;

         PROCEDURE Last(left : BOOLEAN; VAR set : CharSet; VAR fx, fy : 
            CARDINAL);

         BEGIN 
            WHILE (WindowChar(W,fx,fy) IN set) DO 
               IF (left & (fy = 0)) OR (~left & (fy = (cols-1))) THEN 
                  RETURN;
               ELSIF left THEN 
                  DEC(fy);
               ELSE 
                  INC(fy);
               END;
            END;
            IF left THEN 
               INC(fy);
            ELSE 
               DEC(fy);
            END;
         END Last;

         PROCEDURE Eow() : BOOLEAN;

            VAR 
               ln, col : CARDINAL;

         BEGIN 
            GetWindowPos(W,ln,col);
            RETURN (ln = (lines-1)) & (col = (cols-1));
         END Eow;

         PROCEDURE OpenLine();

            VAR 
               l, c : CARDINAL;
               ch   : CHAR;

         BEGIN 
            Mark();
            FOR l := lines-1 TO mline BY -1 DO 
               FOR c := 0 TO cols-1 DO 
                  IF l = mline THEN 
                     ch := sp;
                  ELSE 
                     ch := WindowChar(W,l-1,c);
                  END;
                  SetWindowPos(W,l,c);
                  WindowWrite(W,ch);
               END;
            END;
            SetWindowPos(W,mline,0);
         END OpenLine;

         PROCEDURE Join();

            VAR 
               c,c1       : CARDINAL;
               mline,mcol : CARDINAL;
	       fx, fy     : CARDINAL;
         BEGIN 
            GetWindowPos(W,fx,fy);
            IF fx = lines-1 THEN 
               Beep();
               RETURN;
            END;
	    MoveLast(TRUE);
	    GetWindowPos(W,mline,mcol);
	    IF NOT IsSpace(mline,mcol) THEN
	       INC(mcol,2);
	    END;
	    IF mcol >= cols-1 THEN
	       Beep();
            ELSE 
	       SetWindowPos(W,mline,mcol);
               c1 := 0;
	       WHILE (c1 < cols) & IsSpace(mline+1,c1) DO
		  INC(c1);
	       END;
	       LOOP
               FOR c := mcol TO cols-1 DO 
                  WindowWrite(W,WindowChar(W,mline+1,c1));
                  INC(c1);
		  IF c1 = cols THEN EXIT END;
               END;
	       END;
               Del(mline+1,0,c1-1);
               IF LastCh() THEN 
                  DelLine();
               END;
            END;
            SetWindowPos(W,fx,fy);

         END Join;

         PROCEDURE Del(l,f,t : CARDINAL);

            VAR 
               ch : CHAR;
               del: CARDINAL;
               c  : CARDINAL;

         BEGIN 
            IF (f > t) OR (t >= cols) OR (t >= cols) THEN 
               Beep();
               RETURN;
            END;
            SetWindowPos(W,l,f);
            del := t-f+1;
            FOR c := f TO cols-1 DO 
               IF c + del > cols-1 THEN 
                  ch := sp;
               ELSE 
                  ch := WindowChar(W,l,c+del);
               END;
               WindowWrite(W,ch);
            END;
            SetWindowPos(W,l,f);
         END Del;

         PROCEDURE DelLine();

            VAR 
               l, c : CARDINAL;
               ch   : CHAR;
         BEGIN 
            Mark();
            FOR l := mline TO lines-1 DO 
               FOR c := 0 TO cols-1 DO 
                  IF l = lines-1 THEN 
                     ch := sp;
                  ELSE 
                     ch := WindowChar(W,l+1,c);
                  END;
                  SetWindowPos(W,l,c);
                  WindowWrite(W,ch);
               END;
            END;
            SetWindowPos(W,mline,0);
         END DelLine;

         PROCEDURE DefPos(dir,end,punct,put : BOOLEAN; VAR mline,mcol : 
            CARDINAL) : BOOLEAN;

            VAR 
               set1, set2, set3  : CharSet;
               ch                : CHAR;
	       x,y               : CARDINAL;

         BEGIN 
            GetWindowPos(W,mline,mcol);
            ch := WindowChar(W,mline,mcol);
            IF ch = sp THEN 
               set1 := CharSet{sp};
	       set2 := AlphaNumS+PunctS;
            ELSIF punct THEN 
	       set2 := PunctS + AlphaNumS;
               IF (ch IN PunctS) THEN 
                  set1 := PunctS;
               ELSE 
                  set1 := AlphaNumS;
               END;
            ELSE                        (* no punct *)
               set1 := AlphaNumS + PunctS;
               set2 := AlphaNumS + PunctS;
            END;
            IF NOT end THEN 
               Last(dir,set1,mline,mcol);
            END;
            IF ~Move(dir,mline,mcol) OR ~Next(dir,set2,mline,mcol) THEN 
               RETURN FALSE;
            ELSIF end THEN 
	       IF ~punct THEN
	       ELSIF WindowChar(W,mline,mcol) IN PunctS THEN
		  set2 := PunctS;
	       ELSE
		  set2 := AlphaNumS;
	       END;
               Last(dir,set2,mline,mcol);
            END;
            IF put THEN 
               SetWindowPos(W,mline,mcol);
            END;
            RETURN TRUE;
         END DefPos;

         PROCEDURE IsSpace(l,c : CARDINAL):BOOLEAN;

         BEGIN 
            RETURN (*(l<lines) & (c<cols) &*) (WindowChar(W,l,c) = sp);
         END IsSpace;

         PROCEDURE Next(left : BOOLEAN; VAR set : CharSet; VAR fx,fy : 
            CARDINAL) : BOOLEAN;

            VAR 
               ox,oy : CARDINAL;
         BEGIN 
            ox := fx;
            oy := fy;
            LOOP 
               IF (WindowChar(W,fx,fy) IN set) THEN 
                  RETURN TRUE;
               ELSIF NOT Move(left,fx,fy) THEN 
                  fx := ox;
                  fy := oy;
                  RETURN FALSE;
               END;
            END;
         END Next;

         PROCEDURE Break(l,c : CARDINAL);

	 VAR
	    i : CARDINAL;

	 BEGIN
	    SetWindowPos(W,l+1,0);
	    FOR i := c TO cols-1 DO
	       WindowWrite(W,WindowChar(W,l,i))
	    END;
	    Del(l,c,cols-1);
	 END Break;

         PROCEDURE Move(left : BOOLEAN; VAR l,c : CARDINAL) : BOOLEAN;

         BEGIN 
            IF (left & (c = 0)) OR (~left & (c = (cols-1))) THEN 
               IF (left & (l = 0)) OR (~left & (l = (lines-1))) THEN 
                  RETURN FALSE;
               ELSIF left THEN 
                  DEC(l);
                  c := cols-1;
               ELSE 
                  INC(l);
                  c := 0;
               END;
            ELSIF left THEN 
               DEC(c);
            ELSE 
               INC(c);
            END;
            RETURN TRUE;
         END Move;

         VAR 
            skip : BOOLEAN;
      BEGIN 
         eow := FALSE;
         LOOP 
            WindowRead(W,ch);
            IF once THEN 
               Once();
               once := FALSE;
            END;
            IF IsFunctionKey(ch) THEN 
               exitkey := ToFunctionKey(ch);
            ELSE 
               exitkey := charmap[ch];
            END;
            IF NOT (exitkey IN local) THEN 
	       skip := FALSE;
	    ELSE
               skip := TRUE;
               ch := nul;
               CASE exitkey OF 
                 up: 
                     IF NOT Vmove(UP) THEN 
                        Beep();
                     END;
               | home: 
                     SetWindowPos(W,0,0);
               | down: 
                     IF NOT Vmove(DOWN) THEN 
                        Beep();
                     END;
               | right: 
		     IF mode = insert THEN
		        ch := ' ';
			skip := FALSE;
		     ELSIF NOT Hmove(RIGHT) THEN 
                        Beep();
                     END;
               | left, backspace: 
                     ch := bs;
		     skip := FALSE;
               END;
               exitkey := nokey;
            END;
            IF exitkey # nokey THEN 
               keymap[exitkey](W,exitkey,return);
               SetWindowAttributes(W,Modes);
	       GetWindowPos(W,fx,fy);
	       insfrom := fy;
	       insto := fy;
               skip := TRUE;
               IF return THEN 
                  EXIT;
               END;
            END;
            IF NOT skip THEN 
               CASE mode OF 
                 escape: 
                     eow := FALSE;
                     CASE ch OF 
                       'R': 
                           mode := replace;
                     | 'G' : 
                           SetWindowPos(W,lines-1,0);
                     | '1'..'9': 
                           fx := 0;
                           WHILE ch IN DigitS DO 
                              fx := fx*10 + ORD(ch) - ORD('0');
                              WindowRead(W,ch);
                           END;
			   DEC(fx);
                           IF fx > lines-1 THEN 
                              fx := lines-1;
                           END;
                           IF ch # 'G' THEN 
                              Beep();
                           ELSE 
                              SetWindowPos(W,fx,0);
                           END;
                     | 'i' ,'I': 
                           GetWindowPos(W,fx,fy);
                           IF ch = 'I' THEN 
                              MoveFirst(FALSE);
                           END;
                           mode := insert;
                           GetWindowPos(W,fx,insfrom);
                           insto := insfrom;
                     | 'a','A': 
                           IF ch = 'A' THEN 
                              MoveLast(FALSE);
                           END;
                           IF ~Hmove(RIGHT) THEN 
                              IF ~Vmove(DOWN) THEN 
                                 Beep();
                              ELSE 
                                 Eol(LEFT);
                                 mode := insert;
                                 GetWindowPos(W,fx,insfrom);
                                 insto := insfrom;
                              END;
                           ELSE 
                              mode := insert;
                              GetWindowPos(W,fx,insfrom);
                              insto := insfrom;
                           END;
                     | '+','j': 
                           IF ~Vmove(DOWN) THEN 
                              Beep();
                           END;
                     | '-','k': 
                           IF ~Vmove(UP) THEN 
                              Beep();
                           END;
                     | sp,'l': 
                           IF ~Hmove(RIGHT) THEN 
                              Beep();
                           END;
                     | del, bs, 'h': 
                           IF ~Hmove(LEFT) THEN 
                              Beep();
                           END;
                     | lf, cr: 
                           IF Vmove(DOWN) THEN 
                              Eol(LEFT);
                           ELSE 
                              exitkey := nokey;
                              EXIT;
                           END;
                     | '0': 
                           Eol(LEFT);
                     | '$': 
			   MoveLast(TRUE);
		     | '^':
			   MoveFirst(TRUE);
                     | 'x': 
                           IF LastCh() & (mcol = 0) THEN 
                              Beep();
                           ELSE 
                              ShiftL();
                              IF LastCh() & Hmove(LEFT) THEN 
                              END;
                           END;
                     | 'r': 
                           Mark();
                           WindowRead(W,ch);
                           IF ch IN NonControlS THEN 
                              WindowWrite(W,ch);
                           ELSE 
                              Beep();
                           END;
                           Return();
                     | 'J':
			   Join();
		     | 'Z':
			   WindowRead(W,ch);
			   IF ch = 'Z' THEN
			      exitkey := nokey;
			      EXIT;
			   ELSE
			      Beep();
			   END;
                     | 'o','O': 
                           IF (ch = 'o') & ~Vmove(DOWN) THEN 
                              Beep();
                           ELSE 
                              OpenLine();
                              mode := insert;
                              insfrom := 0;
                              insto := 0;
                           END;
                     | 'W','E','B', 'w','e','b': 
                           isupper := ch IN UpperS;
                           Upper(ch);
                           IF NOT DefPos((ch ='B'), (ch='B') OR ( ch= 'E'),
                              isupper, TRUE,fx,fy) THEN 
                              Beep();
                           END;
                     | 'd':
                           WindowRead(W,ch);
                           Mark();
                           CASE ch OF 
                           | 'W','E','B', 'w','e','b': 
                                 isupper := ch IN UpperS;
                                 Upper(ch);
                                 IF NOT DefPos((ch ='B'), (ch='B') OR ( ch 
                                    = 'E'), isupper, FALSE,fx,fy) OR (mline # fx) THEN 
                                    fx := mline;
                                    CASE ch OF 
                                      'E': 
                                          fy := cols-1;
				    | 'W' :
					  fy := cols;
                                    | 'B' : 
                                          fy := 0;
                                    END;
                                 END;
				 (*
                                 IF mline # fx THEN 
                                    IF ch = 'B' THEN 
                                       fy := 0;
                                    ELSE 
                                       fy := cols-1;
                                    END;
                                 END;
				 *)
                                 CASE ch OF 
                                   'E' : 
                                       IF (mcol > fy) OR ((mcol = 0) & LastCh
                                          ()) THEN 
                                          Beep();
                                       ELSE 
                                          Del(mline,mcol,fy);
                                          IF LastCh() AND Hmove(LEFT) THEN 
                                          END;
                                       END;
                                 | 'W' : 
                                       IF (mcol > (fy-1)) OR ((mcol = 0)
                                          & LastCh()) THEN 
                                          Beep();
                                       ELSE 
                                          Del(mline,mcol,fy-1);
                                          IF LastCh() AND Hmove(LEFT) THEN 
                                          END;

                                       END;
                                 | 'B' : 
                                       IF fy > (mcol-1) THEN 
                                          Beep;
                                       ELSE 
                                          Del(mline,fy,mcol-1);
                                       END;
                                 END;
                           | 'd' : 
                                 IF LastLine() THEN 
                                    IF NOT Vmove(UP) THEN 
                                       Beep();
                                    END;
                                 ELSE 
                                    DelLine();
                                    IF LastLine() AND Vmove(UP) THEN 
                                    END;
                                 END;
                           | '$' : 
                                 IF LastCh() & (mcol = 0) THEN 
                                    Beep()
                                 ELSE 
                                    Del(mline,mcol,cols-1);
                                    IF LastCh() & Hmove(LEFT) THEN 
                                    END;
                                 END;
                           | '0' : 
                                 Mark();
                                 IF mcol > 0 THEN 
                                    Del(mline,0,mcol-1)
                                 ELSE 
                                    Beep();
                                 END;
			   | '^':
				 GetWindowPos(W,fx,fy);
				 MoveFirst(FALSE);
				 GetWindowPos(W,mline,mcol);
				 IF mcol = fy THEN
				    Beep();
				 ELSIF mcol > fy THEN
				    Del(mline,fy,mcol-1);
				 ELSE (* mcol < fy *)
				    Del(mline,mcol,fy-1);
				 END;
                           ELSE 
                              Beep();
                           END;
                     ELSE 
                        Beep();
                     END;
               | insert: 
                     IF ch IN NonControlS THEN 
                        IF eow THEN 
                           SetWindowPos(W,lines-1,cols-1);
			   Beep();
                        ELSIF ~Eow() THEN 
                           eow := FALSE;
                           ShiftR();
                           GetWindowPos(W,fx,fy);
                           WindowWrite(W,ch);
                           IF fy = cols-1 THEN 
                              insfrom := 0;
                              insto := 0;
                           ELSE 
                              INC(insto);
                           END;
                        ELSE 
                           WindowWrite(W,ch);
                           eow := TRUE;
                           INC(insto);
                           SetWindowPos(W,lines-1,cols-1);
                        END;
                     ELSE 
                        CASE ch OF 
                          esc: 
                              mode := escape;
                        | del, bs,'h': 
			      IF eow AND (insto >= cols-1) THEN
				 WindowWrite(W,sp);
				 SetWindowPos(W,lines-1,cols-1);
				 eow := FALSE;
			      ELSE
                              GetWindowPos(W,fx,fy);
                              IF (fy <= insfrom) OR (fy > insto) OR ~Hmove
                                 (LEFT) THEN 
                                 Beep();
                              ELSE 
                                 ShiftL();
                              END;
			      END;
                        | lf, cr: 
                              GetWindowPos(W,fx,fy);
                              IF fx < (lines-1)THEN 
				 SetWindowPos(W,fx+1,0);
				 OpenLine();
				 Break(fx,fy);
				 SetWindowPos(W,fx+1,0);
				 insfrom := 0;
				 insto := 0;
			      ELSE
				 (*Del(fx,fy,cols-1);*)
                                 exitkey := nokey;
                                 EXIT;
			      END;
                        ELSE 
                           Beep();
                        END;
                     END;
               | replace: 
                     IF ch IN NonControlS THEN 
                        IF ~eow THEN 
                           eow := Eow();
                           WindowWrite(W,ch);
                           IF eow THEN 
                              SetWindowPos(W,lines-1,cols-1);
                           END;
                        ELSE 
                           Beep();
                        END;
                     ELSE 
                        CASE ch OF 
                          esc: 
                              mode := escape;
                        | bs, del: 
                              IF ~Hmove(LEFT) THEN 
                                 Beep();
                              END;
                        | lf, cr: 
                              IF Vmove(DOWN) THEN 
                                 Eol(LEFT);
                              ELSE 
                                 exitkey := nokey;
                                 EXIT;
                              END;
                        ELSE 
                           Beep();
                        END;
                     END;
               END;
            END;
         END;
      END Edit;

      VAR 
	ch : CHAR;
   BEGIN 
      GetWindowPos(W,initl,initc);
      GetWindowAttributes(W,inits);
      IF def THEN 
         mode := escape;
      ELSE 
         WindowClear(W);
         mode := insert;
         insfrom := 0;
         insto := 0;
      END;
      ClearInput(W);
      SetWindowAttributes(W,Modes);
      GetWindowSize(W,lines,cols);
      SetWindowPos(W,0,0);
      Edit();
      SetWindowPos(W,initl,initc);
      SetWindowAttributes(W,inits);
   END EditWindow;

   PROCEDURE ClearInput(W:Window);

   VAR
     ch : CHAR;
   BEGIN
      SetWindowAttributes(W,WindowAtSet{nodelay});
      REPEAT
	 WindowRead(W,ch);
      UNTIL NOT Done;
   END ClearInput;
   PROCEDURE Ignore(w : Window; VAR exitcode : FunctionKey; VAR return : BOOLEAN);

   BEGIN 
      Beep();
      return := FALSE;
   END Ignore;

   PROCEDURE Return(w : Window; VAR exitcode : FunctionKey; VAR return : 
      BOOLEAN);

   BEGIN 
      exitcode := nokey;
      return := TRUE;
   END Return;
   PROCEDURE Default(w : Window; VAR exitcode : FunctionKey; VAR return : 
      BOOLEAN);

   BEGIN 
      return := TRUE;
   END Default;

   PROCEDURE Init();

      VAR 
         c : CHAR;
         k : FunctionKey;
   BEGIN 
      once := FALSE;
      local := FunctionKeySet{};
      FOR c := 0C TO 177C DO 
         charmap[c] := nokey;
      END;
      FOR k := MIN(FunctionKey) TO MAX(FunctionKey) DO 
         keymap[k] := Default;
      END;
      Modes := modes;
   END Init;

BEGIN 
   Init();
END EditWindow. 
