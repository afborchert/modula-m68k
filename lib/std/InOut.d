DEFINITION MODULE InOut; (* stripped version: AFB 4/84 *)

   CONST
      EOL = 12C;
   VAR
      Done: BOOLEAN; (* on eof true *)
      termCH: CHAR; (* set in ReadString and numeric input procs *)

   PROCEDURE Read(VAR ch: CHAR);

   PROCEDURE ReadString(VAR s: ARRAY OF CHAR);

   PROCEDURE ReadCard(VAR c: CARDINAL);

   PROCEDURE ReadInt(VAR i: INTEGER);

   PROCEDURE Write(ch: CHAR);

   PROCEDURE WriteLn;

   PROCEDURE WriteString(s: ARRAY OF CHAR);

   (* n: minimum field width *)

   PROCEDURE WriteInt(x: INTEGER; n: CARDINAL);

   PROCEDURE WriteCard(x: CARDINAL; n: CARDINAL);

   PROCEDURE WriteOct(x: CARDINAL; n: CARDINAL);

   PROCEDURE WriteHex(x: CARDINAL; n: CARDINAL);

END InOut.
