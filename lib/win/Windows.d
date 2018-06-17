DEFINITION MODULE Windows;

   FROM StdIO IMPORT FILE;
   FROM Attributes IMPORT Attribute, AttributeSet, AttrInfo;
   FROM FunctionKeys IMPORT FunctionKey, FKInfo;
   FROM Screen IMPORT Screen;
   (* system dependent imports *)
   FROM TermInfo IMPORT Term;

   TYPE
      TermType = ARRAY[0..31] OF CHAR; (* terminfo name *)
      Device;
      Terminfo;
      Window;
      WindowAttributes = (flushalways, flushoninput, nodelay, echo, scroll,
			  movecursor, funckeys, timeout, mapkeys, readcr);
      WindowAtSet = SET OF WindowAttributes;
      TermPtr = POINTER TO Term;
      WindowStatbuf =			(* structure returned by `WindowStat' *)
	 RECORD
	    dev: Device;		(* associated device *)
	    in, out: FILE;		(* associated file pointers *)
	    fkinfo: FKInfo;		(* see FunctionKeys module *)
	    atinfo: AttrInfo;   	(* see Attributes module *)
	    scr: Screen;		(* see Screen module *)
	    (* system dependent components *)
	    terminfo: TermPtr;		(* see TermInfo module *)
	 END;

   VAR
      Done: BOOLEAN;

   PROCEDURE OpenTerminfo(VAR tinfo: Terminfo; termtype: TermType);

   PROCEDURE OpenDevice(VAR dev: Device; devin, devout: FILE; tinfo: Terminfo);

   PROCEDURE OpenDeviceFile(VAR dev: Device; devname: ARRAY OF CHAR;
			    tinfo: Terminfo);

   PROCEDURE Redraw(dev: Device);

   PROCEDURE DeviceChar(dev: Device; line, column: CARDINAL) : CHAR;

   PROCEDURE CloseDevice(VAR dev: Device);

   PROCEDURE CreateWindow(VAR win: Window; dev: Device);

   PROCEDURE NewWindow(VAR win: Window; newwinof: Window;
		       sx, sy, lns, cols: CARDINAL);

   PROCEDURE SubWindow(VAR win: Window; subwinof: Window;
		       sx, sy, lns, cols: CARDINAL);

   PROCEDURE GetWindowSize(win: Window; VAR lines, columns: CARDINAL);

   PROCEDURE GetWindowStart(win: Window; VAR line, column: CARDINAL);

   PROCEDURE GetSupWin(win: Window; VAR supwin: Window);

   PROCEDURE IsSubWin(win: Window): BOOLEAN;

   PROCEDURE GetWindowPos(win: Window; VAR line, column: CARDINAL);

   PROCEDURE WindowUnget(win: Window; ch: CHAR);

   PROCEDURE SetWindowPos(win: Window; line, column: CARDINAL);

   PROCEDURE WindowWrite(win: Window; ch: CHAR);

   PROCEDURE SetWindowDisplayMode(win: Window; atset: AttributeSet);

   PROCEDURE WindowClear(win: Window);

   PROCEDURE ScrollWindow(window: Window; count: INTEGER);
      (* scroll `window'
	 ABS(count) = # of lines to be scrolled
	 count > 0: scroll forward (up)
	 count < 0: scroll backward (down)
      *)

   PROCEDURE WindowRead(win: Window; VAR ch: CHAR);

   PROCEDURE IsFunctionKey(ch: CHAR) : BOOLEAN;

   PROCEDURE ToFunctionKey(ch: CHAR) : FunctionKey;

   PROCEDURE WindowChar(win: Window; line, column: CARDINAL) : CHAR;

   PROCEDURE FlushWindow(win: Window);

   PROCEDURE StartWinProcess(win: Window; process: PROC);

   PROCEDURE TerminateWinProcess(win: Window);

   PROCEDURE RunProcesses;

   PROCEDURE Terminate;

   PROCEDURE GetWindowAttributes(win: Window; VAR atset: WindowAtSet);

   PROCEDURE SetWindowAttributes(win: Window; atset: WindowAtSet);

   PROCEDURE CloseWindow(VAR win: Window);

   PROCEDURE WindowStat(win: Window; VAR statbuf: WindowStatbuf);

   PROCEDURE SuspendDevice(dev: Device);

   PROCEDURE RestoreDevice(dev: Device);

END Windows.
