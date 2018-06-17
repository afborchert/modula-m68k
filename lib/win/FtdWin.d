DEFINITION MODULE FtdWin;

   FROM Windows IMPORT Window;

   VAR Done: BOOLEAN;
       termCH: CHAR;

   PROCEDURE WinReadInt(win: Window; VAR int: INTEGER);

   PROCEDURE WinWriteInt(win: Window; int: INTEGER; w: CARDINAL);

   PROCEDURE WinReadCard(win: Window; VAR card: CARDINAL);

   PROCEDURE WinWriteCard(win: Window; card: CARDINAL; w: CARDINAL);

   PROCEDURE WinReadString(win: Window; VAR str: ARRAY OF CHAR);

   PROCEDURE WinReadLine(win: Window; VAR str: ARRAY OF CHAR);

   PROCEDURE WinWriteString(win: Window; str: ARRAY OF CHAR);

   PROCEDURE WinWriteLn(win: Window);

END FtdWin.
