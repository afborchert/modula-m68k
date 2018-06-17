DEFINITION MODULE Terminal; (* A. Borchert *)

   (* read and write from/to standard input/output channel *)

   VAR Done: BOOLEAN;

   PROCEDURE Read(VAR ch: CHAR);

   PROCEDURE ReadAgain;

   PROCEDURE Write(ch: CHAR);

   PROCEDURE WriteLn;

   PROCEDURE WriteString(s: ARRAY OF CHAR);

END Terminal.
