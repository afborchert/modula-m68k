(* Modula-2 Library    -  UNIX System V  -     AFB 6/88 *)
(* (c) Universitaet Ulm, Sektion Informatik, D-7900 Ulm *)
IMPLEMENTATION MODULE Menus;

   FROM Windows IMPORT Window, GetWindowSize, SetWindowPos, NewWindow,
      SubWindow, WindowWrite, WindowRead, GetWindowPos, SetWindowAttributes,
      WindowAtSet, WindowAttributes, SetWindowDisplayMode, FlushWindow,
      WindowClear, CloseWindow, IsFunctionKey, ToFunctionKey,
      WindowStatbuf, WindowStat;
   FROM FunctionKeys IMPORT FunctionKey;
   FROM FtdWin IMPORT WinWriteString, WinWriteLn;
   FROM Attributes IMPORT Attribute, AttributeSet, AvailableAttributes;
   FROM Strings IMPORT StrCpy, StrLen;
   FROM Storage IMPORT ALLOCATE, DEALLOCATE;
   FROM ASCII IMPORT nl;
   IMPORT Windows;

   (* (* exported from definition module *)
   TYPE
      Menu;
   VAR
      Done: BOOLEAN;
   *)
   TYPE
      Text = ARRAY [0..79] OF CHAR;
      Menu = POINTER TO MenuEntry;
      CommandList = POINTER TO CommandEntry;
      CommandType = (number, procedure, submenu);
      CommandEntry =
	 RECORD
	    name: Text;
	    line: CARDINAL;
	    CASE cmdtype: CommandType OF
	    | number:    cmdno: CARDINAL;
	    | procedure: proc: PROC;
	    | submenu:   sbmenu: Menu;
	    END;
	    prev, link: CommandList;
	 END;
      MenuEntry =
	 RECORD
	    lines, cols: CARDINAL; (* with box *)
	    header: Text;
	    head, tail: CommandList;
	 END;

      MenuList = POINTER TO MenuListEntry;
      MenuListEntry =
	 RECORD
	    mwin: Window;
	    link: MenuList;
	 END;

   PROCEDURE CreateMenu(VAR menu: Menu; title: ARRAY OF CHAR);
   BEGIN
      NEW(menu);
      WITH menu^ DO
	 StrCpy(header, title);
	 IF header[0] <> 0C THEN
	    lines := 1;
	    cols := StrLen(header);
	 ELSE
	    lines := 0;
	    cols := 0;
	 END;
	 INC(lines, 2); (* box *)
	 INC(cols, 2);
	 head := NIL;
	 tail := NIL;
      END;
      Done := TRUE;
   END CreateMenu;

   PROCEDURE AddCommand(menu: Menu;
			cmd: ARRAY OF CHAR;
			no: CARDINAL);
      VAR
	 new: CommandList;
   BEGIN
      NEW(new);
      WITH new^ DO
	 StrCpy(name, cmd);
	 cmdtype := number;
	 cmdno := no;
      END;
      AppendCommand(menu, new);
   END AddCommand;

   PROCEDURE AddProcedure(menu: Menu;
			  cmd: ARRAY OF CHAR;
			  cmdproc: PROC);
      VAR
	 new: CommandList;
   BEGIN
      NEW(new);
      WITH new^ DO
	 StrCpy(name, cmd);
	 cmdtype := procedure;
	 proc := cmdproc;
      END;
      AppendCommand(menu, new);
   END AddProcedure;

   PROCEDURE AddSubMenu(menu: Menu;
			cmd: ARRAY OF CHAR;
			smenu: Menu);
      VAR
	 new: CommandList;
   BEGIN
      NEW(new);
      WITH new^ DO
	 StrCpy(name, cmd);
	 cmdtype := submenu;
	 sbmenu := smenu;
      END;
      AppendCommand(menu, new);
   END AddSubMenu;

   PROCEDURE ExecMenu(menu: Menu; background: Window;
		      VAR selected: CARDINAL);
   BEGIN
      NestedExecMenu(menu, background, selected, NIL);
   END ExecMenu;

   PROCEDURE NestedExecMenu(menu: Menu; background: Window;
		            VAR selected: CARDINAL;
		            prevmenues: MenuList);
      CONST             (* <ctrl> ... *)
	 ScrollBackward = CHR(ORD('b')-ORD('a')+1);
	 ScrollForward =  CHR(ORD('f')-ORD('a')+1);
	 ScrollDown =     CHR(ORD('d')-ORD('a')+1);
	 ScrollUp =       CHR(ORD('u')-ORD('a')+1);
      VAR
	 x, y: CARDINAL; (* current position in background *)
	 maxlines, maxcols: CARDINAL;
	 back, box, menwin, headwin, cmdwin: Window;
	 lastcmd, cmd: CommandList;
	 again: BOOLEAN;
	 ch: CHAR;
	 newmenu: MenuList;
	 startx: CARDINAL;
	 firstline: CARDINAL;	(* first line to be showed *)
	 scroll: BOOLEAN;	(* scroll bar? *)
	 scrolled: BOOLEAN;
	 wcols: CARDINAL;	(* # cols of the menu window *)
	 wlines: CARDINAL;	(* # lines of the menu window *)
	 bar: Window;
	 standoutOK: BOOLEAN;
	 invisOK: BOOLEAN;

      PROCEDURE Update(menu: MenuList);
      BEGIN
	 IF menu <> NIL THEN
	    WITH menu^ DO
	       Update(link);
	       FlushWindow(mwin);
	    END;
	 END;
      END Update;

      PROCEDURE Box;
	 VAR line, column: CARDINAL;
      BEGIN
	 WITH menu^ DO
	    FOR column := 1 TO wcols-2 DO
	       SetWindowPos(box, 0, column); WindowWrite(box, "-");
	       SetWindowPos(box, wlines-1, column); WindowWrite(box, "-");
	    END;
	    FOR line := 1 TO wlines-2 DO
	       SetWindowPos(box, line, 0); WindowWrite(box, "|");
	       SetWindowPos(box, line, wcols-1); WindowWrite(box, "|");
	       IF scroll THEN
		  SetWindowPos(box, line, 2); WindowWrite(box, "|");
	       END;
	    END;
	    SetWindowPos(box, 0, 0); WindowWrite(box, "+");
	    SetWindowPos(box, 0, wcols-1); WindowWrite(box, "+");
	    SetWindowPos(box, wlines-1, 0); WindowWrite(box, "+");
	    SetWindowPos(box, wlines-1, wcols-1); WindowWrite(box, "+");
	 END;
      END Box;

      PROCEDURE ShowMenu;
	 VAR
	    cmd: CommandList;
      BEGIN
	 WITH menu^ DO
	    WindowClear(menwin);
	    SetWindowPos(menwin, 0, 0);
	    cmd := head;
	    WHILE (cmd <> NIL) AND (cmd^.line < firstline+wlines-2) DO
	       IF cmd^.line >= firstline THEN
		  SetWindowPos(menwin, cmd^.line-firstline, 0);
		  WinWriteString(menwin, cmd^.name);
	       END;
	       cmd := cmd^.link;
	    END;
	    FlushWindow(menwin);
	 END;
      END ShowMenu;

      PROCEDURE ShowBar;
	 
	 VAR
	    index, line: CARDINAL;
	    ch: CHAR;

	 PROCEDURE Map(line: CARDINAL) : CARDINAL;
	 BEGIN
	    RETURN line * (wlines-2) DIV (menu^.lines-2)
	 END Map;

      BEGIN
	 IF scroll THEN
	    IF standoutOK THEN
	       ch := " ";
	    ELSE
	       ch := "X";
	    END;
	    SetWindowDisplayMode(bar, AttributeSet{});
	    WindowClear(bar);
	    SetWindowDisplayMode(bar, AttributeSet{standout});
	    line := Map(firstline);
	    SetWindowPos(bar, line, 0); WindowWrite(bar, ch);
	    FOR index := 2 TO Map(wlines-2) DO
	       SetWindowPos(bar, line+index-1, 0);
	       WindowWrite(bar, ch);
	    END;
	    IF firstline = menu^.lines-(startx+1)-(wlines-2) THEN
	       FOR index := line+Map(wlines-2) TO wlines-2-1 DO
		  SetWindowPos(bar, index, 0);
		  WindowWrite(bar, ch);
	       END;
	    END;
	    FlushWindow(bar);
	 END;
      END ShowBar;

      PROCEDURE ScrollCmds;
	 VAR
	    move: CARDINAL;
	    forward: BOOLEAN;
	    index: CARDINAL;

	 PROCEDURE Forward;
	 BEGIN
	    FOR index := 1 TO move DO
	       cmd := cmd^.link;
	       IF cmd = NIL THEN
		  cmd := menu^.tail;
		  RETURN
	       END;
	    END;
	 END Forward;

	 PROCEDURE Backward;
	 BEGIN
	    FOR index := 1 TO move DO
	       cmd := cmd^.prev;
	       IF cmd = NIL THEN
		  cmd := menu^.head;
		  RETURN
	       END;
	    END;
	 END Backward;

      BEGIN
	 CASE ch OF
	 | ScrollBackward: move := wlines-2; forward := FALSE;
	 | ScrollForward:  move := wlines-2; forward := TRUE;
	 | ScrollDown:     move := (wlines-2) DIV 2; forward := TRUE;
	 | ScrollUp:       move := (wlines-2) DIV 2; forward := FALSE;
	 ELSE
	    RETURN (* no scroll command *)
	 END;
	 IF forward THEN
	    IF firstline + wlines-2 + move > menu^.lines-(startx+1) THEN
	       firstline := menu^.lines-(startx+1)-(wlines-2);
	    ELSE
	       INC(firstline, move);
	    END;
	    Forward;
	 ELSE
	    IF move < firstline THEN
	       DEC(firstline, move);
	    ELSE
	       firstline := 0;
	    END;
	    Backward;
	 END;
	 ShowBar; ShowMenu; scrolled := TRUE;
      END ScrollCmds;

      PROCEDURE StandoutOK(win: Window; VAR ok: BOOLEAN);
	 VAR
	    statbuf: WindowStatbuf;
	    atset: AttributeSet;
      BEGIN
	 WindowStat(win, statbuf);
	 AvailableAttributes(statbuf.atinfo, atset);
	 ok := standout IN atset;
      END StandoutOK;

      PROCEDURE InvisOK(win: Window; VAR ok: BOOLEAN);
	 VAR
	    statbuf: WindowStatbuf;
      BEGIN
	 WindowStat(win, statbuf);
	 ok := statbuf.terminfo^.CursorInvisible[0] # 0C;
      END InvisOK;

   BEGIN
      WITH menu^ DO
	 Done := FALSE;
	 IF head = NIL THEN RETURN END;
	 GetWindowSize(background, maxlines, maxcols);
	 StandoutOK(background, standoutOK);
	 InvisOK(background, invisOK);
	 scroll := lines > maxlines;
	 IF scroll THEN
	    wlines := maxlines;
	    wcols := cols+2;
	 ELSE
	    wlines := lines;
	    wcols := cols;
	 END;
	 IF (wcols > maxcols) OR (maxlines < 4) THEN RETURN END;
	 GetWindowPos(background, x, y);
	 IF (x+wlines > maxlines) OR (y+wcols > maxcols) THEN
	    IF x + wlines > maxlines THEN
	       x := maxlines - wlines;
	    END;
	    IF y + wcols > maxcols THEN
	       y := maxcols - wcols;
	    END;
	    SetWindowPos(background, x, y);
	 END;
	 SubWindow(back, background, x, y, wlines, wcols);
	 NewWindow(box, background, x, y, wlines, wcols);
	 IF NOT Windows.Done THEN RETURN END;
	 SetWindowAttributes(box, WindowAtSet{});
	 Box; FlushWindow(box);

	 IF header[0] <> 0C THEN
	    startx := 2; DEC(wlines);
	 ELSE
	    startx := 1;
	 END;
	 IF scroll THEN
	    SubWindow(menwin, box, startx, 3, wlines-2, cols-2);
	    SubWindow(bar, box, startx, 1, wlines-2, 1);
	 ELSE
	    SubWindow(menwin, box, startx, 1, wlines-2, cols-2);
	 END;
	 IF NOT Windows.Done THEN RETURN END;
	 SetWindowAttributes(menwin, WindowAtSet{funckeys, timeout});

	 (* display menu fields *)
	 IF header[0] <> 0C THEN
	    SubWindow(headwin, box, 1, 1, 1, wcols-2);
	    IF standoutOK AND NOT invisOK THEN
	       SetWindowAttributes(headwin, WindowAtSet{movecursor});
	    END;
	    SetWindowDisplayMode(headwin, AttributeSet{underline});
	    WindowClear(headwin);
	    WinWriteString(headwin, header);
	    FlushWindow(headwin);
	 ELSE
	    SubWindow(headwin, box, 0, 0, 1, cols-2);
	    IF NOT invisOK THEN
	       SetWindowAttributes(headwin, WindowAtSet{movecursor});
	    END;
	 END;
	 firstline := 0;
	 ShowBar; ShowMenu;

	 cmd := head;
	 REPEAT (* for every selection until the menu will be closed *)
	    LOOP (* until a command has been selected *)
	       lastcmd := cmd;
	       FlushWindow(menwin);
	       WITH cmd^ DO
		  IF scroll THEN
		     IF line < firstline THEN
			firstline := line; ShowBar; ShowMenu;
		     ELSIF line >= firstline+wlines-2 THEN
			firstline := line-(wlines-2)+1;
			ShowBar; ShowMenu;
		     END;
		  END;
		  SubWindow(cmdwin, menwin, line-firstline, 0, 1, cols-2);
		  SetWindowDisplayMode(cmdwin, AttributeSet{standout});
		  IF NOT standoutOK THEN
		     SetWindowAttributes(cmdwin, WindowAtSet{movecursor});
		  END;
		  WindowClear(cmdwin);
		  WinWriteString(cmdwin, name);
		  IF NOT standoutOK THEN
		     SetWindowPos(cmdwin, 0, 0);
		  END;
		  FlushWindow(cmdwin);
	       END;
	       IF standoutOK THEN
		  SetWindowPos(headwin, 0, 0); FlushWindow(headwin);
	       END;
	       WindowRead(cmdwin, ch);
	       IF IsFunctionKey(ch) THEN
		  CASE ToFunctionKey(ch) OF
		  | up:    ch := 'k';
		  | down:  ch := 'j';
		  | left:  ch := 'h';
		  | right: ch := 'l';
		  | sf:    ch := ScrollForward;
		  | sr:    ch := ScrollBackward;
		  ELSE
		  END;
	       END;
	       scrolled := FALSE;
	       IF scroll THEN
		  ScrollCmds;
	       END;
	       IF NOT scrolled THEN
		  CASE ch OF
		  | 'h': cmd := NIL; EXIT (* exit menu *)
		  | 'j': cmd := cmd^.link; IF cmd = NIL THEN cmd := head END;
		  | 'k': cmd := cmd^.prev; IF cmd = NIL THEN cmd := tail END;
		  | nl, 'l': EXIT (* command selected *)
		  ELSE
		  END;
	       END;

	       IF NOT scrolled THEN
		  SetWindowDisplayMode(cmdwin, AttributeSet{});
		  WindowClear(cmdwin);
		  WinWriteString(cmdwin, lastcmd^.name);
	       END;
	       CloseWindow(cmdwin);
	    END;

	    (* command execution *)
	    again := FALSE;
	    IF cmd <> NIL THEN
	       WITH cmd^ DO
		  CASE cmdtype OF
		  | number:    selected := cmdno;
		  | procedure: proc; again := TRUE;
		  | submenu:   SetWindowPos(background, x+line-firstline,
							y+wcols);
			       NEW(newmenu);
			       WITH newmenu^ DO
				 mwin := box;
				 link := prevmenues;
			       END;
			       NestedExecMenu(sbmenu, background, selected,
					      newmenu);
			       IF selected = 0 THEN
				  again := TRUE;
			       END;
		  END;
	       END;
	       IF again THEN
		  Update(prevmenues);
		  FlushWindow(box);
	       END;
	    ELSE
	       selected := 0; (* no command selected *)
	    END;
	 UNTIL NOT again;

	 SetWindowDisplayMode(cmdwin, AttributeSet{});
	 WindowClear(cmdwin);
	 WinWriteString(cmdwin, lastcmd^.name);
	 CloseWindow(cmdwin);

	 CloseWindow(headwin);
	 CloseWindow(menwin);
	 IF scroll THEN
	    CloseWindow(bar);
	 END;
	 CloseWindow(box);
	 FlushWindow(back);
	 CloseWindow(back);
      END;
      Done := TRUE;
   END NestedExecMenu;

   PROCEDURE DisposeMenu(VAR menu: Menu);
      VAR dispose, cmd: CommandList;
   BEGIN
      IF menu = NIL THEN
	 Done := FALSE; RETURN
      END;
      Done := TRUE;
      cmd := menu^.head;
      WHILE cmd <> NIL DO
	 dispose := cmd;
	 cmd := cmd^.link;
	 DISPOSE(dispose);
      END;
      DISPOSE(menu);
   END DisposeMenu;

   (* local procedures *)

   PROCEDURE AppendCommand(menu: Menu; cmd: CommandList);
   BEGIN
      WITH menu^ DO
	 IF header[0] <> 0C THEN
	    cmd^.line := lines-3;
	 ELSE
	    cmd^.line := lines-2;
	 END;
	 INC(lines);
	 IF StrLen(cmd^.name)+2 > cols THEN
	    cols := StrLen(cmd^.name) + 2;
	 END;
	 cmd^.link := NIL;
	 IF head = NIL THEN
	    head := cmd;
	    cmd^.prev := NIL;
	 ELSE
	    tail^.link := cmd;
	    cmd^.prev := tail;
	 END;
	 tail := cmd;
      END;
      Done := TRUE;
   END AppendCommand;

END Menus.
