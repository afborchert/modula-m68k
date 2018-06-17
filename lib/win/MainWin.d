DEFINITION MODULE MainWin;

   FROM Windows IMPORT Window;

   VAR
      mainwin: Window; (* window affected by following procedures *)
      lines, columns: CARDINAL; (* size of mainwin *)
      Done: BOOLEAN;

   PROCEDURE SetPos(line, column: CARDINAL);

   PROCEDURE GetPos(VAR line, column: CARDINAL);

   PROCEDURE Clear;

   PROCEDURE Flush;

   PROCEDURE WriteString(s: ARRAY OF CHAR);

   PROCEDURE Write(ch: CHAR);

   PROCEDURE WriteLn;

   PROCEDURE Read(VAR ch: CHAR);

   PROCEDURE ReadString(VAR s: ARRAY OF CHAR);

   PROCEDURE WriteInt(i: INTEGER; w: CARDINAL);

   PROCEDURE ReadInt(VAR i: INTEGER);

   PROCEDURE WriteCard(c: CARDINAL; w: CARDINAL);

   PROCEDURE ReadCard(VAR c: CARDINAL);

END MainWin.
