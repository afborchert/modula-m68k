DEFINITION MODULE ReadIntCard;

   FROM SYSTEM IMPORT WORD;

   TYPE
      Type = (int, card);
      ReadProc = PROCEDURE(VAR CHAR);

   VAR Done: BOOLEAN;

   (* convention: ReadChar returns 0C on eof or error *)

   PROCEDURE Read(VAR arg: WORD; t: Type; ReadChar: ReadProc);

END ReadIntCard.
