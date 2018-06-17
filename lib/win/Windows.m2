(* Modula-2 Library    -  UNIX System V  -  AFB,HP 5/88 *)
(* (c) Universitaet Ulm, Sektion Informatik, D-7900 Ulm *)
IMPLEMENTATION MODULE Windows;

   FROM StdIO IMPORT FILE, Fputc, Fopen, read, write, Fclose, Fgetc, Fflush,
      Ferror, FileNo;
   FROM FtdIO IMPORT FwriteChar, FwriteString, FwriteCard;
   FROM Storage IMPORT ALLOCATE, DEALLOCATE;
   FROM SYSTEM IMPORT ADR, ADDRESS, BYTE, NEWPROCESS, TRANSFER;
   FROM ASCII IMPORT nl, cr, bs, del, esc;
   FROM SysTermIO IMPORT SetTermIO, GetTermIO, TermIO, vmin, vtime,
      icanon, tab1, tab2, opost, parmrk, ignpar, inpck, istrip, ixon, icrnl,
      Baudrate;
   FROM SysIoctl IMPORT Winsize, GetWinsize;
   FROM SysFcntl IMPORT FcntlRequest, Fcntl;
   FROM SystemTypes IMPORT ndelay;
   FROM Environment IMPORT GetEnv;
   FROM Attributes IMPORT Attribute, AttributeSet, AttrInfo, OpenAttrInfo,
      CloseAttrInfo, AvailableAttributes, SetAttributes,
      ResetAttributes;
   FROM FunctionKeys IMPORT FunctionKey, OpenFKInfo, CloseFKInfo,
      EnableFunctionKeys, DisableFunctionKeys, Read, FKInfo;
   FROM Screen IMPORT OpenScreenTI, CloseScreen, ClearScreen,
      SetCursor, MoveCursor, InitScreen, Scroll, CursorVisibility,
      ResetScrollRegions;
   FROM TermInfo IMPORT Term, SetupTerm;
   FROM StrToNum IMPORT StrToCard;
   IMPORT SysTermIO, Screen;

   (* convention: 'x': line number, 'y': column number *)
   (*             both starting from 0                 *)

   (* (* from definition module *)
   TYPE
      TermType = ARRAY[0..31] OF CHAR; (* terminfo name *)
      Device;
      Terminfo;
      Window;
      WindowAttributes = (flushalways, flushoninput, nodelay, echo, scroll,
			  movecursor, funckeys, timeout, mapkeys, readcr);
      WindowAtSet = SET OF WindowAttributes;
      WindowStatbuf =			(* structure returned by `WindowStat' *)
	 RECORD
	    dev: Device;		(* associated device *)
	    in, out: FILE;		(* associated file pointers *)
	    fkinfo: FKInfo;		(* see FunctionKeys module *)
	    atinfo: AttrInfo;   	(* see Attributes module *)
	    scr: Screen;		(* see Screen module *)
	    (* system dependent components *)
	    terminfo: POINTER TO Term;	(* see TermInfo module *)
	 END;
   VAR
      Done: BOOLEAN;
   *)

   CONST
      MaxLines = 10000;		(* large enough to avoid ...		*)
      MaxColumns = 10000;	(* ... range check faults		*)
      StackSize = 32768;	(* of a process				*)
      fkbase = 200C;		(* code function keys as NON-ASCII's	*)

   TYPE
      ScreenChar =
	 RECORD
	    ch: CHAR;
	    atset: AttributeSet;
	 END;
      ScreenCont = POINTER TO ARRAY[0..MaxLines-1] OF
	           POINTER TO ARRAY[0..MaxColumns-1] OF ScreenChar;
      Device = POINTER TO DeviceEntry;
      Terminfo = POINTER TO TerminfoEntry;
      DeviceEntry =
	 RECORD
	    in, out: FILE;	(* should be buffered *)
	    tinfo: Terminfo;	(* associated terminfo *)
	    fkinfo: FKInfo;	(* see FunctionKeys module *)
	    atinfo: AttrInfo;   (* see Attributes module *)
	    useats: BOOLEAN;	(* display attributes supported? *)
	    atset: AttributeSet;(* attributes currently set *)
	    suspended: BOOLEAN; (* SuspendDevice called? *)
	    lines,		(* # of lines and columns *)
	    columns: CARDINAL;	(* duplicated from terminfo or *)
				(* retrieved by calling GetWindowsize *)
	    screen: ScreenCont;	(* current screen contents *)
	    scr: Screen.Screen;	(* see Screen module *)
	    termio: TermIO;	(* old terminal setting from SysTermIO *)
	    currio: TermIO;     (* Windows terminal setting *)
	    ndelay: BOOLEAN;	(* current ndelay-setting *)
	    cursorvis: CursorVisibility;
	    fkeysenabled: BOOLEAN;
	    x, y: CARDINAL;	(* current cursor position *)
	 END;
      TerminfoEntry =
	 RECORD
	    termtype: TermType;		(* terminal name (e.g. "vt100") *)
	    lines, columns: CARDINAL;	(* # of lines and columns, positive *)
	    term: Term;			(* terminfo-entries from TermInfo *)
	 END;
      Window = POINTER TO WindowEntry;
      Process = POINTER TO ProcessEntry;
      WindowEntry =
	 RECORD
	    lines, columns: CARDINAL;	(* window size *)
	    startx, starty: CARDINAL;	(* offset to device or win *)
	    x, y: CARDINAL;		(* current position *)
	    scrollcnt: INTEGER;         (* negative: down *)
	    winats: WindowAtSet;
	    modes: AttributeSet;	(* modes currently set *)
	    readproc: Process;		(* suspended reading process *)
            unget: BOOLEAN; (* HP *)
            ungetchar: CHAR; (* HP *)
	    CASE subwin: BOOLEAN OF
	    | FALSE: screen: ScreenCont;
	             device: Device;
		     reading: BOOLEAN;	(* WindowRead active? *)
	    | TRUE:  win: Window;	(* super-window *)
	    END;
	 END;
      StackPtr = POINTER TO ARRAY [0..StackSize-1] OF BYTE;
      ProcessEntry =
	 RECORD
	    cr: ADDRESS;		(* coroutine reference *)
	    prev: Process;		(* ring of processes *)
	    link: Process;
	    win: Window;		(* associated window *)
	    stack: StackPtr;		(* stack belonging to the coroutine *)
	 END;
      StackList = POINTER TO StackEntry;
      StackEntry =
	 RECORD
	    stack: StackPtr;
	    link: StackList;
	 END;

   VAR
      current: Process;
      main: ADDRESS;			(* main coroutine *)
      running: BOOLEAN;			(* processes are running *)
      stacklist: StackList;

   PROCEDURE Error;
      (* realized as procedure for two reasons: *)
      (* - possible breakpoint in mdb		*)
      (* - handle to do output			*)
   BEGIN
      Done := FALSE;
   END Error;

   PROCEDURE OpenTerminfo(VAR tinfo: Terminfo; ttype: TermType);
   BEGIN
      Done := TRUE;
      NEW(tinfo);
      WITH tinfo^ DO
	 IF NOT SetupTerm(ttype, term) THEN
	    Error; DISPOSE(tinfo); RETURN
	 END;
	 termtype := ttype;
	 (* Lines and Columns are -1 if not defined in terminfo *)
	 (* This is true for lineprinters or similar devices.	*)
	 WITH term DO
	    lines := ABS(Lines); columns := ABS(Columns);
	 END;
      END;
   END OpenTerminfo;

   PROCEDURE AllocateScreen(VAR screen: ScreenCont;
			    lines, columns: CARDINAL);
      (* allocate a screen of the given size and initialize it *)
      VAR line, column: CARDINAL;
   BEGIN
      ALLOCATE(screen, lines * SIZE(ADDRESS));
      FOR line := 0 TO lines-1 DO
	 ALLOCATE(screen^[line], columns * SIZE(ScreenChar));
	 FOR column := 0 TO columns-1 DO
	    WITH screen^[line]^[column] DO
	       ch := " ";
	       atset := AttributeSet{};
	    END;
	 END;
      END;
   END AllocateScreen;

   PROCEDURE DeallocateScreen(VAR screen: ScreenCont; lines, cols: CARDINAL);
      (* reverse of AllocateScreen *)
      VAR line: CARDINAL;
   BEGIN
      FOR line := 0 TO lines-1 DO
	 DEALLOCATE(screen^[line], cols * SIZE(ScreenChar));
      END;
      DEALLOCATE(screen, lines * SIZE(ADDRESS));
   END DeallocateScreen;

   PROCEDURE OpenDevice(VAR dev: Device; devin, devout: FILE;
                        terminfo: Terminfo);
      VAR
	 termbuf: TermIO;
	 availatset: AttributeSet;
	 baudrate: CARDINAL;
	 winsize: Winsize;
	 envok: BOOLEAN;
	 envparam: ARRAY [0..11] OF CHAR;
	 envval: CARDINAL;
   BEGIN
      Done := TRUE;
      NEW(dev);
      WITH dev^ DO
	 in := devin;
	 out := devout;
	 suspended := FALSE;
	 IF NOT GetTermIO(FileNo(devin), termio) THEN
	    Error;
	 ELSE
	    termbuf := termio;
	    WITH termbuf DO
	       cc[vmin] := 1C; cc[vtime] := 1C;
	       linemodes := linemodes - SysTermIO.echo - icanon;
	       inputmodes := inputmodes - parmrk + ignpar - inpck + istrip
			     (* switch off icrnl: function key codes
				may contain cr's
			     *)
			     - icrnl;
	       outputmodes := outputmodes - tab1 - tab2 - opost;
	    END;
	    currio := termbuf;
	    IF NOT SetTermIO(FileNo(devin), termbuf) THEN
	       Error;
	    END;
	 END;
	 tinfo := terminfo;
	 IF ixon <= termio.inputmodes THEN
	    baudrate := 0; (* no padding necessary *)
	 ELSE
	    baudrate := Baudrate(termio);
	 END;

	 IF GetWinsize(FileNo(out), winsize) AND
	    (winsize.rows # 0) AND (winsize.cols # 0) THEN
	    (* SUN version: dynamic window sizing *)
	    lines := winsize.rows;
	    columns := winsize.cols;
	 ELSE
	    lines := tinfo^.lines;
	    columns := tinfo^.columns;
	 END;
	 GetEnv("LINES", envparam, envok);
	 IF envok & StrToCard(envparam, envval) THEN
	    lines := envval;
	 END;
	 GetEnv("COLUMNS", envparam, envok);
	 IF envok & StrToCard(envparam, envval) THEN
	    columns := envval;
	 END;

	 OpenScreenTI(scr, out, baudrate, lines, columns, tinfo^.term);
	 ResetScrollRegions(scr);
	 ClearScreen(scr);
	 OpenFKInfo(fkinfo, tinfo^.term, baudrate, devin, devout);
	 IF tinfo^.term.MagicCookieGlitch > 0 THEN
	    useats := FALSE;
	 ELSE
	    OpenAttrInfo(atinfo, tinfo^.term, baudrate, devout);
	    AvailableAttributes(atinfo, availatset);
	    useats := availatset <> AttributeSet{};
	    atset := AttributeSet{};
	 END;
	 AllocateScreen(screen, lines, columns);
	 ndelay := FALSE;
	 cursorvis := normal;
	 fkeysenabled := FALSE;
	 x := 0; y := 0;
      END;
      IF NOT Done THEN DISPOSE(dev) END;
   END OpenDevice;

   PROCEDURE OpenDeviceFile(VAR dev: Device; devname: ARRAY OF CHAR;
			    tinfo: Terminfo);
      VAR
	 devin, devout: FILE;
   BEGIN
      Done := TRUE;
      IF Fopen(devin, devname, read, FALSE) AND
	 Fopen(devout, devname, write, TRUE) THEN
	 OpenDevice(dev, devin, devout, tinfo);
      ELSE
	 Error;
      END;
   END OpenDeviceFile;

   PROCEDURE ResetAtSet(dev: Device);
   BEGIN
      WITH dev^ DO
	 IF useats AND (atset <> AttributeSet{}) THEN
	    ResetAttributes(atinfo);
	    atset := AttributeSet{};
	 END;
      END;
   END ResetAtSet;

   PROCEDURE SetAtSet(dev: Device; ats: AttributeSet);
   BEGIN
      WITH dev^ DO
	 IF useats AND (atset <> ats) THEN
	    IF ats = AttributeSet{} THEN
	       ResetAttributes(atinfo);
	    ELSE
	       SetAttributes(atinfo, ats);
	    END;
	    atset := ats;
	 END;
      END;
   END SetAtSet;

   PROCEDURE Redraw(dev: Device);
      VAR
	 oldx, oldy: CARDINAL;
	 lastx, lasty: CARDINAL;
	 l, c: CARDINAL;
   BEGIN
      Done := TRUE;
      WITH dev^ DO
	 IF suspended THEN Error; RETURN END;
	 oldx := x; oldy := y;
	 InitScreen(scr, lines, columns);
	 ResetScrollRegions(scr); ClearScreen(scr);
	 FOR l := 0 TO lines-1 DO
	    SetCursor(scr, l, 0); lastx := l; lasty := 0;
	    FOR c := 0 TO columns-1 DO
	       IF (l <> lines-1) OR (c <> columns-1) THEN
		  WITH screen^[l]^[c] DO
		     IF (ch <> ' ') OR (atset <> AttributeSet{}) THEN
			IF (lastx <> l) OR (lasty <> c) THEN
			   ResetAtSet(dev);
			   MoveCursor(scr, (* from *) lastx, lasty,
					   (* to *)   l, c);
			END;
			SetAtSet(dev, atset);
			IF NOT Fputc(ch, out) THEN
			   Error; RETURN
			END;
			lasty := c+1;
		     END;
		  END;
	       END;
	    END;
	 END;
	 ResetAtSet(dev);
	 SetCursor(scr, oldx, oldy);
	 Screen.SetCursorVisibility(scr, cursorvis);
	 IF NOT Fflush(out) THEN Error END;
      END;
   END Redraw;

   PROCEDURE DeviceChar(dev: Device; line, column: CARDINAL) : CHAR;
   BEGIN
      WITH dev^ DO
	 IF (line >= lines) OR (column >= columns) THEN
	    Error; RETURN 0C
	 ELSE
	    RETURN screen^[line]^[column].ch
	 END;
      END;
   END DeviceChar;

   PROCEDURE CloseDevice(VAR dev: Device);
   BEGIN
      Done := TRUE;
      WITH dev^ DO
	 SetCursorVisibility(dev, normal);
	 IF ndelay THEN
	    SetNdelay(dev, FALSE);
	 END;
	 IF useats THEN
	    ResetAttributes(atinfo);
	 END;
	 CloseAttrInfo(atinfo);
	 ResetScrollRegions(scr);
	 SetCursor(scr, x, y);
	 CloseScreen(scr);
	 CloseFKInfo(fkinfo);
	 IF NOT SetTermIO(FileNo(in), termio) THEN
	    Error;
	 END;
	 IF NOT Fclose(in) OR NOT Fclose(out) THEN
	    Error;
	 END;
      END;
      DISPOSE(dev);
   END CloseDevice;

   PROCEDURE SuspendDevice(dev: Device);
      VAR
	 ndelayset: BOOLEAN;
   BEGIN
      WITH dev^ DO
	 IF suspended THEN Error; RETURN END;
	 suspended := TRUE;
	 IF useats THEN
	    ResetAttributes(atinfo);
	 END;
	 MoveCursor(scr, (* from *) x, y, (* to *) lines-1, 0);
	 IF cursorvis # normal THEN
	    Screen.SetCursorVisibility(scr, normal);
	 END;
	 IF NOT Fflush(out) THEN Error END;
	 IF NOT SetTermIO(FileNo(in), termio) THEN
	    Error;
	 END;
	 ndelayset := ndelay; SetNdelay(dev, FALSE); ndelay := ndelayset;
      END;
   END SuspendDevice;

   PROCEDURE RestoreDevice(dev: Device);
   BEGIN
      WITH dev^ DO
	 IF NOT suspended THEN Error; RETURN END;
	 IF NOT SetTermIO(FileNo(in), currio) THEN
	    Error;
	 END;
	 InitScreen(scr, lines, columns);
	 SetNdelay(dev, ndelay);
	 suspended := FALSE;
	 Redraw(dev);
      END;
   END RestoreDevice;

   PROCEDURE InitWindow(VAR win: Window; dev: Device; lns, cols: CARDINAL);
      (* common part of CreateWindow and NewWindow *)
   BEGIN
      NEW(win);
      WITH win^ DO
	 device := dev;
	 lines := lns;
	 columns := cols;
	 startx := 0;
	 starty := 0;
	 modes := AttributeSet{};
	 winats := WindowAtSet{};
	 x := 0; y := 0;
	 scrollcnt := 0;
	 AllocateScreen(screen, lines, columns);
	 readproc := NIL;
	 subwin := FALSE;
	 reading := FALSE;
	 unget := FALSE;
      END;
   END InitWindow;

   PROCEDURE CreateWindow(VAR win: Window; dev: Device);
   BEGIN
      Done := TRUE;
      InitWindow(win, dev, dev^.lines, dev^.columns);
   END CreateWindow;

   PROCEDURE GetTopWin(VAR win: Window; VAR sx, sy: CARDINAL);
   BEGIN
      sx := 0; sy := 0;
      WHILE win^.subwin DO
	 INC(sx, win^.startx); INC(sy, win^.starty);
	 win := win^.win;
      END;
   END GetTopWin;

   PROCEDURE NewWindow(VAR win: Window; newwinof: Window;
		       sx, sy, lns, cols: CARDINAL);
      VAR
	 offx, offy: CARDINAL;
	 topwin: Window;
   BEGIN
      Done := TRUE;
      IF (sx + lns > newwinof^.lines) OR (sy + cols > newwinof^.columns) THEN
	 Error; RETURN
      END;
      topwin := newwinof;
      GetTopWin(topwin, offx, offy);
      InitWindow(win, topwin^.device, lns, cols);
      WITH win^ DO
	 startx := sx + offx + topwin^.startx;
	 starty := sy + offy + topwin^.starty;
	 winats := newwinof^.winats;
	 modes := AttributeSet{};
      END;
   END NewWindow;

   PROCEDURE SubWindow(VAR win: Window; subwinof: Window;
		       sx, sy, lns, cols: CARDINAL);
   BEGIN
      Done := TRUE;
      IF (sx + lns > subwinof^.lines) OR (sy + cols > subwinof^.columns) THEN
	 Error; RETURN
      END;
      NEW(win);
      WITH win^ DO
	 subwin := TRUE; win := subwinof; readproc := NIL;
	 lines := lns; columns := cols;
	 startx := sx; starty := sy;
	 winats := subwinof^.winats;
	 modes := AttributeSet{};
	 x := 0; y := 0;
	 unget := FALSE;
      END;
   END SubWindow;

   PROCEDURE GetWindowSize(win: Window; VAR lines, columns: CARDINAL);
   BEGIN
      Done := TRUE;
      lines := win^.lines;
      columns := win^.columns;
   END GetWindowSize;

   PROCEDURE Touch(win: Window);
   BEGIN
      IF flushalways IN win^.winats THEN
	 FlushWindow(win);
      END;
   END Touch;

(* BEGIN  HP *)

   PROCEDURE GetWindowStart(win: Window; VAR line, column: CARDINAL);
   BEGIN
      Done := TRUE;
      line := win^.startx;
      column := win^.starty;
   END GetWindowStart;

   PROCEDURE GetSupWin(win: Window; VAR supwin: Window);
   BEGIN
      IF win^.subwin THEN
         supwin := win^.win;
         Done := TRUE;
      ELSE
         Done := FALSE;
      END;
   END GetSupWin;

   PROCEDURE IsSubWin(win: Window): BOOLEAN;
   BEGIN
      Done := TRUE;
      RETURN win^.subwin;
   END IsSubWin;

   PROCEDURE GetWindowPos(win: Window; VAR line, column: CARDINAL);
   BEGIN
      Done := TRUE;
      line := win^.x; column := win^.y;
   END GetWindowPos;
(* END HP *)

   PROCEDURE SetWindowPos(window: Window; line, column: CARDINAL);
      VAR
	 dx, dy: CARDINAL;
   BEGIN
      Done := TRUE;
      WITH window^ DO
	 x := line; y := column;
	 CheckPos(x, y, lines, columns);
      END;
      Touch(window);
   END SetWindowPos;

   PROCEDURE CheckPos(VAR x, y: CARDINAL; lines, cols: CARDINAL);
   BEGIN
      IF y >= cols THEN y := 0; INC(x); END;
      IF x >= lines THEN x := lines-1 END;
   END CheckPos;

   PROCEDURE WindowWrite(window: Window; ch: CHAR);
      VAR
	 oldmodes: AttributeSet;
   BEGIN
      Done := TRUE;
      WITH window^ DO
	 IF x >= lines THEN Error; RETURN END;
	 IF y >= columns THEN Error; RETURN END;
	 CASE ch OF
	 | 0C: (* do nothing *)
	 | nl: IF x < lines-1 THEN
		  INC(x); y := 0;
	       ELSIF scroll IN winats THEN
		  ScrollWindow(window, 1); y := 0;
	       ELSE
		  Error;
	       END;
	 | cr: y := 0;
	 | bs, del: IF y > 0 THEN DEC(y) ELSE Error END;
	 ELSE
	    IF (ch < ' ') OR (ch > del) THEN Error; RETURN END;
	    IF subwin THEN
	       CheckPos(x, y, lines, columns);
	       win^.x := x + startx; win^.y := y + starty;
	       oldmodes := win^.modes; win^.modes := modes;
	       WindowWrite(win, ch);
	       win^.modes := oldmodes;
	    ELSE
	       CheckPos(x, y, lines, columns);
	       screen^[x]^[y].ch := ch;
	       screen^[x]^[y].atset := modes;
	    END;
	    INC(y);
	    IF y >= columns THEN
	       y := 0; INC(x);
	       IF (x >= lines) & (scroll IN winats) THEN
		  ScrollWindow(window, 1);
		  x := lines-1;
	       END;
	    END;
	 END;
      END;
      IF Done THEN
	 Touch(window);
      END;
   END WindowWrite;

   PROCEDURE SetWindowDisplayMode(win: Window; atset: AttributeSet);
      VAR
	 availset: AttributeSet;
	 topwin: Window;
	 sx, sy: CARDINAL;
   BEGIN
      topwin := win; GetTopWin(topwin, sx, sy);
      WITH win^ DO
	 Done := TRUE;
	 IF topwin^.device^.useats THEN
	    AvailableAttributes(topwin^.device^.atinfo, availset);
	    modes := atset * availset;
	 ELSE
	    Error;
	 END;
      END;
   END SetWindowDisplayMode;

   PROCEDURE WindowClear(window: Window);
      VAR
	 l, c: CARDINAL;
   BEGIN
      WITH window^ DO
	 FOR l := 1 TO lines DO
	    SetWindowPos(window, l-1, 0);
	    FOR c := 1 TO columns DO
	       WindowWrite(window, " ");
	    END;
	 END;
	 SetWindowPos(window, 0, 0);
	 scrollcnt := 0;
      END;
   END WindowClear;

   PROCEDURE SetNdelay(device: Device; setndelay: BOOLEAN);
      VAR flagset: BITSET;
   BEGIN
      IF NOT Fcntl(FileNo(device^.in), getfl, flagset) THEN
	 Error;
      ELSE
	 IF setndelay THEN
	    flagset := flagset + ndelay; device^.ndelay := TRUE;
	 ELSE
	    flagset := flagset - ndelay; device^.ndelay := FALSE;
	 END;
	 IF NOT Fcntl(FileNo(device^.in), setfl, flagset) THEN
	    Error;
	 END;
      END;
   END SetNdelay;

   PROCEDURE SetCursorVisibility(device: Device; vis: CursorVisibility);
   BEGIN
      WITH device^ DO
	 IF vis # cursorvis THEN
	    Screen.SetCursorVisibility(scr, vis);
	    IF Screen.Done THEN
	       cursorvis := vis;
	    END;
	 END;
      END;
   END SetCursorVisibility;

   PROCEDURE WindowRead(window: Window; VAR ch: CHAR);

      VAR
	 keepx, keepy: CARDINAL; (* keep position due to coroutines *)
	 atset: WindowAtSet;
	 topwinDone: BOOLEAN;

      PROCEDURE GetChar(VAR ch: CHAR) : BOOLEAN;
	 VAR
	    fkey: FunctionKey;

	 PROCEDURE MapCh(VAR ch: CHAR);
	    (* cr to nl mapping -- switched off in termio *)
	    (* necessary for reading of function key codes *)
	 BEGIN
	    IF (ch = cr) & ~(readcr IN window^.winats) THEN
	       ch := nl;
	    END;
	 END MapCh;

      BEGIN
	 WITH window^ DO
	    WITH device^ DO
	       IF (funckeys IN winats) <> fkeysenabled THEN
		  IF funckeys IN winats THEN
		     EnableFunctionKeys(fkinfo);
		  ELSE
		     DisableFunctionKeys(fkinfo);
		  END;
		  fkeysenabled := funckeys IN winats;
	       END;
	       IF funckeys IN winats THEN
		  IF Read(fkinfo, timeout IN winats, fkey, ch) THEN
		     IF (fkey <> nokey) AND
			((ch = 0C) OR (mapkeys IN winats)) THEN
			IF ORD(fkbase) + ORD(fkey) <= ORD(MAX(CHAR)) THEN
			   ch := CHR(ORD(fkbase) + ORD(fkey));
			ELSE
			   (* cannot represent key *)
			   ch := fkbase;
			END;
		     END;
		     MapCh(ch);
		  ELSE
		     RETURN FALSE
		  END;
	       ELSE
		  IF Fgetc(ch, in) THEN
		     MapCh(ch); RETURN TRUE
		  ELSE
		     RETURN FALSE
		  END;
	       END;
	    END;
	 END;
	 RETURN TRUE
      END GetChar;

   BEGIN
      Done := TRUE;
      WITH window^ DO
         IF unget THEN (* HP *)
            ch := ungetchar;
            unget := FALSE;
	    RETURN
	 END;
	 IF subwin THEN
	    (* let the superwindow do the work; 		  *)
	    (* but: (1) our attributes must be set 		  *)
	    (*      (2) echo must be switched off; else 	  *)
	    (*          the user would be able to type outside of *)
	    (*		our window                                *)
	    CheckPos(x, y, lines, columns);
	    SetWindowPos(win, startx+x, starty+y);
	    GetWindowAttributes(win, atset);
	    SetWindowAttributes(win, winats - WindowAtSet{echo});
	    WindowRead(win, ch); topwinDone := Done;
	    SetWindowAttributes(win, atset);
	    x := win^.x - startx; y := win^.y - starty;
	    IF echo IN winats THEN
	       WindowWrite(window, ch);
	    END;
	    Done := topwinDone;
	    RETURN
	 END;

	 (* no subwindow and unget = FALSE *)
	 IF flushoninput IN winats THEN
	    reading := TRUE;
	    FlushWindow(window);
	    reading := FALSE;
	 END;
	 IF (nodelay IN winats) <> device^.ndelay THEN
	    SetNdelay(device, nodelay IN winats);
	 END;

	 IF running THEN (* coroutines running? *)
	    keepx := x; keepy := y;
	    IF (current = current^.link) AND (nodelay IN winats) THEN
	       (* ndelay leads to unnecessary loop *)
	       SetNdelay(device, FALSE);
	    END;
	    WHILE NOT GetChar(ch) DO
	       IF current <> current^.link THEN
		  current := current^.link;
		  TRANSFER(current^.prev^.cr, current^.cr);
		  (* the other coroutine may modify the position! *)
		  IF (x <> keepx) OR (y <> keepy) THEN
		     x := keepx; y := keepy;
		     SetWindowPos(window, x, y);
		     IF flushoninput IN winats THEN
			IF NOT Fflush(device^.out) THEN Error END;
		     END;
		  END;
	       END;
	    END;
	 ELSIF NOT GetChar(ch) THEN (* usual case *)
	    Error;
	 END;
	 IF echo IN winats THEN
	    IF ch < fkbase THEN
	       WindowWrite(window, ch);
	    END;
	 END;
      END;
   END WindowRead;

   PROCEDURE IsFunctionKey(ch: CHAR) : BOOLEAN;
   BEGIN
      RETURN ch >= fkbase
   END IsFunctionKey;

   PROCEDURE ToFunctionKey(ch: CHAR) : FunctionKey;
   BEGIN
      IF IsFunctionKey(ch) THEN
	 RETURN VAL(FunctionKey, ORD(ch) - ORD(fkbase))
      ELSE
	 RETURN nokey
      END;
   END ToFunctionKey;

(* BEGIN  HP *)

   PROCEDURE WindowUnget(win: Window; ch: CHAR);
   BEGIN
      WITH win^ DO
	 IF unget THEN
	    Done := FALSE;
	 ELSE
	    Done := TRUE;
	    unget := TRUE;
	    ungetchar := ch;
	 END;
      END;
   END WindowUnget;

(* END HP *)

   PROCEDURE WindowChar(win: Window; line, column: CARDINAL) : CHAR;
   BEGIN
      WITH win^ DO
	 IF (line >= lines) OR (column >= columns) THEN
	    Error; RETURN 0C
	 ELSIF subwin THEN
	    RETURN WindowChar(win, startx+line, starty+column)
	 ELSE
	    RETURN screen^[line]^[column].ch
	 END;
      END;
   END WindowChar;

   PROCEDURE ScrollWindow(window: Window; count: INTEGER);
      (* scroll `window'
	 ABS(count) = # of lines to be scrolled
	 count > 0: scroll forward (up)
	 count < 0: scroll backward (down)
      *)
      VAR
	 sx, sy: CARDINAL;
	 topwin: Window;
   BEGIN
      Done := TRUE;
      IF count # 0 THEN
	 topwin := window; GetTopWin(topwin, sx, sy);
	 WITH topwin^ DO
	    ScrollScreenCont(screen, count, sx, sy,
			     window^.lines, window^.columns);
	 END;
	 WITH window^ DO
	    INC(scrollcnt, count);
	    IF ORD(ABS(scrollcnt)) >= lines THEN
	       scrollcnt := 0;
	    END;
	 END;
	 Touch(window);
      END;
   END ScrollWindow;

   PROCEDURE ScrollScreenCont(s: ScreenCont; count: INTEGER;
			      sx, sy, lns, cols: CARDINAL);
      (* scroll contents of `s':
	 count > 0: scroll forward
	 count < 0: scroll backward (down)
	 scroll region:
	 sx, sy: upper left corner
	 lns, cols: # of lines and columns
	 scroll region must fit into `s' !!
      *)
      VAR
	 x: CARDINAL;
	 cnt: CARDINAL; (* ABS(count) *)

      PROCEDURE ClearLines(from, to: CARDINAL);
	 VAR
	    white: ScreenChar;
	    x, y: CARDINAL;
      BEGIN
	 WITH white DO
	    ch := " ";
	    atset := AttributeSet{};
	 END;
	 FOR x := from TO to DO
	    FOR y := sy TO sy+cols-1 DO
	       s^[x]^[y] := white;
	    END;
	 END;
      END ClearLines;

      PROCEDURE CopyLine(dest, source: CARDINAL);
	 VAR
	    y: CARDINAL;
      BEGIN
	 FOR y := sy TO sy+cols-1 DO
	    s^[dest]^[y] := s^[source]^[y];
	 END;
      END CopyLine;

   BEGIN
      cnt := ABS(count);
      IF count < 0 THEN
	 FOR x := sx+lns-1 TO sx+cnt BY -1 DO
	    CopyLine(x, x-cnt);
	 END;
	 ClearLines(sx, sx+cnt-1);
      ELSE
	 IF lns > cnt THEN
	    FOR x := sx TO sx+lns-cnt-1 DO
	       CopyLine(x, x+cnt);
	    END;
	 END;
	 ClearLines(sx+lns-cnt, sx+lns-1);
      END;
   END ScrollScreenCont;

   PROCEDURE FlushWindow(window: Window);
      VAR
	 dev: Device;
	 topwin: Window;
	 sx, sy: CARDINAL;
	 l, c: CARDINAL;

      PROCEDURE SetPos(l, c: CARDINAL);
	 VAR
	    dx, dy: CARDINAL;
      BEGIN
	 WITH topwin^ DO
	    dx := startx+l; dy := starty+c;
	    WITH device^ DO
	       IF (dx <> x) OR (dy <> y) THEN
		  ResetAtSet(device);
		  MoveCursor(scr, (* from *) x, y, (* to *) dx, dy);
		  x := dx; y := dy;
	       END;
	    END;
	 END;
      END SetPos;

      PROCEDURE Write(ch: CHAR; modes: AttributeSet);
      BEGIN
	 WITH topwin^.device^ DO
	    screen^[x]^[y].ch := ch;
	    screen^[x]^[y].atset := modes;
	    IF (x <> lines-1) OR (y <> columns-1) THEN
	       SetAtSet(topwin^.device, modes);
	       IF NOT Fputc(ch, out) THEN
		  Error; RETURN
	       END;
	       INC(y);
	       IF y >= columns THEN
		  y := 0; INC(x);
		  WITH tinfo^.term DO
		     ResetAtSet(topwin^.device);
		     IF NOT AutoRightMargin OR EatNewlineGlitch THEN
			SetCursor(scr, x, y);
		     END;
		  END;
	       END;
	    END;
	 END;
      END Write;

      PROCEDURE ScrollDevice(dev: Device; count: INTEGER;
			     sx, sy: CARDINAL;
			     lns, cols: CARDINAL) : BOOLEAN;
	 (* returns TRUE if scrolling is supported by hardware
	    else no scrolling is done
	 *)
	 VAR
	    down: BOOLEAN;
	    cnt: CARDINAL;
      BEGIN
	 down := count < 0;
	 cnt := ABS(count);
	 WHILE cnt > 0 DO
	    Scroll(dev^.scr, down, sx, sy, lns, cols);
	    IF NOT Screen.Done THEN RETURN FALSE END;
	    DEC(cnt);
	 END;
	 WITH dev^ DO
	    SetCursor(scr, x, y);
	 END;
	 (* update device *)
	 ScrollScreenCont(dev^.screen, count, sx, sy, lns, cols);
	 RETURN TRUE
      END ScrollDevice;

   BEGIN
      Done := TRUE;
      topwin := window; GetTopWin(topwin, sx, sy); dev := topwin^.device;
      IF dev^.suspended THEN Error; RETURN END;
      IF window^.scrollcnt # 0 THEN
	 IF NOT ScrollDevice(dev, window^.scrollcnt,
			     window^.startx+sx, window^.starty+sy,
			     window^.lines, window^.columns) THEN
	    (* nothing to be done *)
	 END;
	 window^.scrollcnt := 0;
      END;
      FOR l := 0 TO window^.lines-1 DO
	 FOR c := 0 TO window^.columns-1 DO
	    WITH topwin^ DO
	       WITH screen^[sx+l]^[sy+c] DO
		  IF (ch <> device^.screen^[sx+startx+l]^[sy+starty+c].ch) OR
		     (atset <> device^.screen^[sx+startx+l]^[sy+starty+c].atset)
		     THEN
		     SetPos(sx+l, sy+c);
		     Write(ch, atset);
		  END;
	       END;
	    END;
	 END;
      END;
      WITH window^ DO
	 IF movecursor IN winats THEN
	    CheckPos(x, y, lines, columns);
	    SetPos(sx+x, sy+y);
	    IF reading THEN
	       SetCursorVisibility(dev, morevisible);
	    END;
	    IF NOT reading OR NOT Screen.Done THEN
	       SetCursorVisibility(dev, normal);
	    END;
	 ELSE
	    SetCursorVisibility(dev, invisible);
	 END;
      END;
      IF NOT Fflush(dev^.out) THEN Error END;
   END FlushWindow;

   PROCEDURE StartWinProcess(win: Window; cproc: PROC);
      VAR
	 process: Process;
   BEGIN
      Done := TRUE;
      (*
      IF win^.readproc <> NIL THEN
	 TerminateWinProcess(win);
      END;
      *)
      IF win^.subwin THEN Error; RETURN END;
      NEW(process); process^.win := win;
      WITH process^ DO
	 IF current = NIL THEN
	    current := process;
	    link := process;
	    prev := process;
	 ELSE
	    link := current;
	    prev := current^.prev;
	    prev^.link := process;
	    current^.prev := process;
	 END;
	 NEW(stack);
	 NEWPROCESS(cproc, ADDRESS(stack)+20, StackSize*SIZE(BYTE)-20, cr);
      END;
      WITH win^ DO
	 readproc := process;
      END;
   END StartWinProcess;

   PROCEDURE TerminateWinProcess(win: Window);
      VAR dead: ADDRESS; stackref: ADDRESS;
   BEGIN
      Done := TRUE;
      IF win^.readproc = NIL THEN Error; RETURN END;
      WITH win^.readproc^ DO
	 stackref := stack;
	 IF prev = win^.readproc THEN
	    current := NIL;
	    IF running THEN
	       running := FALSE;
	       DISPOSE(win^.readproc); win^.readproc := NIL;
	       DEALLOCATE(stackref, StackSize);
	       TRANSFER(dead, main);
	    END;
	 ELSE
	    link^.prev := prev;
	    prev^.link := link;
	    IF current = win^.readproc THEN
	       current := link;
	       DISPOSE(win^.readproc); win^.readproc := NIL;
	       DEALLOCATE(stackref, StackSize);
	       TRANSFER(dead, current^.cr);
	    END;
	 END;
      END;
      DISPOSE(win^.readproc); win^.readproc := NIL;
   END TerminateWinProcess;

   PROCEDURE RunProcesses;
      VAR
	 sl: StackList;
   BEGIN
      Done := TRUE;
      IF running OR (current = NIL) THEN Error; RETURN END;
      running := TRUE;
      stacklist := NIL;
      TRANSFER(main, current^.cr);
      WHILE stacklist <> NIL DO
	 sl := stacklist;
	 stacklist := stacklist^.link;
	 DISPOSE(sl^.stack);
	 DISPOSE(sl);
      END;
      current := NIL;
      running := FALSE;
   END RunProcesses;

   PROCEDURE Terminate;
      VAR
	 sl: StackList;
	 old: Process;
	 dead: ADDRESS;
   BEGIN
      IF running THEN
	 WITH current^ DO
	    link^.prev := prev;
	    prev^.link := link;
	    NEW(sl);
	    sl^.link := stacklist;
	    stacklist := sl;
	    sl^.stack := stack;
	 END;
	 old := current; current := current^.link;
	 IF current = old THEN
	    DISPOSE(old);
	    TRANSFER(dead, main);
	 ELSE
	    DISPOSE(old);
	    TRANSFER(dead, current^.cr);
	 END;
      END;
      Error;
   END Terminate;

   PROCEDURE GetWindowAttributes(win: Window; VAR atset: WindowAtSet);
   BEGIN
      Done := TRUE;
      atset := win^.winats;
   END GetWindowAttributes;

   PROCEDURE SetWindowAttributes(win: Window; atset: WindowAtSet);
   BEGIN
      Done := TRUE;
      win^.winats := atset;
   END SetWindowAttributes;

   PROCEDURE CloseWindow(VAR win: Window);
   BEGIN
      Done := TRUE;
      WITH win^ DO
	 IF NOT subwin THEN
	    DeallocateScreen(screen, lines, columns);
	 END;
      END;
      DISPOSE(win);
   END CloseWindow;

   PROCEDURE WindowStat(window: Window; VAR statbuf: WindowStatbuf);
      VAR
	 topwin: Window;
	 sx, sy: CARDINAL; (* not of further interest *)
	 device: Device;
   BEGIN
      topwin := window; GetTopWin(topwin, sx, sy); device := topwin^.device;
      WITH statbuf DO
	 dev := device;
	 in := dev^.in;
	 out := dev^.out;
	 fkinfo := dev^.fkinfo;
	 atinfo := dev^.atinfo;
	 scr := dev^.scr;
	 terminfo := TermPtr(ADR(dev^.tinfo^.term));
      END;
   END WindowStat;

BEGIN
   running := FALSE;
   current := NIL;
END Windows.
