(* Modula-2 Library    -  UNIX System V  -     AFB 5/88 *)
(* (c) Universitaet Ulm, Sektion Informatik, D-7900 Ulm *)
DEFINITION MODULE Screen;

   FROM TermInfo IMPORT Term;
   FROM StdIO IMPORT FILE;

   TYPE
      Screen;
      CursorVisibility = (normal, invisible, morevisible);
   VAR
      Done: BOOLEAN;

   PROCEDURE OpenScreen(VAR scr: Screen;
			outfp: FILE;
			baudrate: CARDINAL;
			lines, columns: CARDINAL;
			terminal: ARRAY OF CHAR) : BOOLEAN;

   PROCEDURE OpenScreenTI(VAR scr: Screen;
			  outfp: FILE;
			  baudrate: CARDINAL;
			  lines, columns: CARDINAL;
			  tinfo: Term);

   PROCEDURE CloseScreen(VAR scr: Screen);

   PROCEDURE Lines(scr: Screen) : CARDINAL;

   PROCEDURE Columns(scr: Screen) : CARDINAL;

   PROCEDURE ClearScreen(scr: Screen);

   PROCEDURE SetCursor(scr: Screen; line, column: CARDINAL);
      (* line in [0..Lines(scr)-1] and column in [0..Columns(scr)-1] *)

   PROCEDURE MoveCursor(scr: Screen;
	     (* from *) line1, column1,
	     (* to *)   line2, column2: CARDINAL);

   PROCEDURE InitScreen(scr: Screen; lines, columns: CARDINAL);

   PROCEDURE Scroll(scr: Screen; down: BOOLEAN;
		    line, column: CARDINAL; (* upper left corner *)
		    lines, columns: CARDINAL);
      (* scroll part of screen (downward or upward);
	 down: TRUE (downwards) or FALSE (upwards)
	 cursor position is undefined (if Done is set to TRUE)
	 Done set to FALSE if not supported by terminal/terminfo
      *)

   PROCEDURE ResetScrollRegions(scr: Screen);

   PROCEDURE SetCursorVisibility(scr: Screen; visibility: CursorVisibility);

END Screen.
