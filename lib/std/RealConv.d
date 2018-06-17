DEFINITION MODULE RealConv; (* AFB 6/84 * rev. wsc 2/85 *)

   TYPE
      ReadProc = PROCEDURE(VAR CHAR);
   VAR
      Done: BOOLEAN;
      termCH: CHAR;

   PROCEDURE ReadReal(Read: ReadProc; VAR x: REAL);
   (* convention: Read returns 0C on eof or error *)

   PROCEDURE WriteFloat(VAR f: ARRAY OF CHAR; x: REAL; base: CARDINAL;
                        dp: CARDINAL);
   PROCEDURE WriteFix(VAR f: ARRAY OF CHAR; x: REAL; base: CARDINAL;
                      VAR dp: CARDINAL);

END RealConv.
