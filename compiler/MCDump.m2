IMPLEMENTATION MODULE MCDump; (* only for debugging *)

   FROM MCBase IMPORT Stptr, Stset, Idptr, Structform;
   FROM MCTypes IMPORT IsCard, IsInt;
   FROM InOut IMPORT WriteString, WriteCard, WriteLn, Write, WriteInt;
   FROM ASCII IMPORT tab;

   PROCEDURE DumpType(mess: ARRAY OF CHAR; sp: Stptr);
   BEGIN
      Header(mess);
      Type(sp);
      WriteLn;
   END DumpType;

   PROCEDURE Type(sp: Stptr);
   BEGIN
      IF sp = NIL THEN WriteString("NIL"); RETURN END;
      WITH sp^ DO
         CASE form OF
         | ints: WriteString("INTEGER");
         | cards: WriteString("CARDINAL");
         | longints: WriteString("LONGINT");
         | longcards: WriteString("LONGCARD");
         | reals: WriteString("REAL");
         | longreals: WriteString("LONGREAL");
         | sets: WriteString("SET OF "); Type(basep);
         | subranges: Type(scalp); Write("["); Const(scalp, min); WriteString(", ");
              Const(scalp, max); Write("]");
         | setoftypes: WriteString("TYPESET: "); Set(typeset);
         ELSE
            WriteString("-- too complicated type --");
         END;
      END;
   END Type;

   PROCEDURE Const(sp: Stptr; val: CARDINAL);
   BEGIN
      IF IsCard(sp) THEN
         WriteCard(val, 1);
      ELSIF IsInt(sp) THEN
         WriteInt(INTEGER(val), 1);
      ELSE
         WriteString("???");
      END;
   END Const;

   PROCEDURE DumpStset(mess: ARRAY OF CHAR; st: Stset);
   BEGIN
      Header(mess);
      Set(st);
      WriteLn;
   END DumpStset;

   PROCEDURE DumpIdent(mess: ARRAY OF CHAR; ip: Idptr);
   BEGIN
      Header(mess);
      WriteString("DumpIdent not yet implemented");
      WriteLn;
   END DumpIdent;

   PROCEDURE Header(mess: ARRAY OF CHAR);
   BEGIN
      WriteString(mess); Write(":");
      Write(tab);
   END Header;

   PROCEDURE Set(st: Stset);
      VAR s: Structform; prev: BOOLEAN;
   BEGIN
      prev := FALSE;
      WriteString("Stset{");
      FOR s := MIN(Structform) TO MAX(Structform) DO
         IF s IN st THEN
            IF prev THEN WriteString(", "); END;
            prev := TRUE;
            CASE s OF
            | ints :		WriteString("ints");
            | enums :		WriteString("enums");
            | bools :		WriteString("bools");
            | chars :		WriteString("chars");
            | cards :		WriteString("cards");
            | words :		WriteString("words");
	    | bytes :		WriteString("bytes");
            | subranges :	WriteString("subranges");
            | reals :		WriteString("reals");
            | pointers :	WriteString("pointers");
            | sets :		WriteString("sets");
            | proctypes :	WriteString("proctypes");
            | arrays :		WriteString("arrays");
            | records :		WriteString("records");
            | hides :		WriteString("hides");
            | opens :		WriteString("opens");
            | bigsets :		WriteString("bigsets");
            | longints :	WriteString("longints");
            | longcards :	WriteString("longcards");
            | longreals :	WriteString("longreals");
            | setoftypes :	WriteString("setoftypes");
            ELSE
               WriteString("???");
            END;
         END; (* IF *)
      END; (* FOR *)
      Write("}");
   END Set;

END MCDump.
