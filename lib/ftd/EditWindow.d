DEFINITION MODULE EditWindow;

   FROM Windows IMPORT Window;
   FROM FunctionKeys IMPORT FunctionKeySet, FunctionKey;

   TYPE 
      FunctionKeyReaction = PROCEDURE (Window,VAR FunctionKey,VAR BOOLEAN);

   CONST 
      LocalMoves = FunctionKeySet {up,down,left,right,home,backspace};

   PROCEDURE Ignore (win : Window; VAR exitkey : FunctionKey; VAR return : 
      BOOLEAN);

   PROCEDURE Return (win : Window; VAR exitkey : FunctionKey; VAR return : 
      BOOLEAN);

   PROCEDURE Default (win : Window; VAR exitkey : FunctionKey; VAR return : 
      BOOLEAN);

   PROCEDURE GetWindowLine(win : Window; line : CARDINAL; VAR text :
      ARRAY OF CHAR; ignoreleadingblanks, ignorefinalblanks : BOOLEAN);
    
   PROCEDURE EditWindow(win : Window; predefined : BOOLEAN; VAR exitkey : 
      FunctionKey );

   PROCEDURE EnableLocalMoves (directions : FunctionKeySet);

   PROCEDURE MapChar(char : CHAR; key : FunctionKey);

   PROCEDURE DefineReaction(key : FunctionKey; proc : FunctionKeyReaction);

   PROCEDURE CallOnce(proc : PROC);

   PROCEDURE ReadOneChar(win : Window; echo : BOOLEAN) : CHAR;

   PROCEDURE Wait(w : Window);

   PROCEDURE Beep();

   PROCEDURE EnableSingleCharFK (recognize : BOOLEAN);

END EditWindow. 
