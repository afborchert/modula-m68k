DEFINITION MODULE FtdIO;

   FROM SYSTEM IMPORT WORD;
   FROM StdIO IMPORT FILE;

   VAR Done: BOOLEAN;
       termCH: CHAR;

   PROCEDURE FreadInt(f: FILE; VAR int: INTEGER);

   PROCEDURE FwriteInt(f: FILE; int: INTEGER; w: CARDINAL);

   PROCEDURE FreadCard(f: FILE; VAR card: CARDINAL);

   PROCEDURE FwriteCard(f: FILE; card: CARDINAL; w: CARDINAL);

   PROCEDURE FreadString(f: FILE; VAR str: ARRAY OF CHAR);

   PROCEDURE FwriteString(f: FILE; str: ARRAY OF CHAR);

   PROCEDURE FwriteLn(f: FILE);

   PROCEDURE Fread(f: FILE; VAR arr: ARRAY OF WORD);

   PROCEDURE Fwrite(f: FILE; arr: ARRAY OF WORD);

   PROCEDURE FreadWord(f: FILE; VAR w: WORD);

   PROCEDURE FwriteWord(f: FILE; w: WORD);

   PROCEDURE FreadChar(f: FILE; VAR ch: CHAR);

   PROCEDURE FwriteChar(f: FILE; ch: CHAR);

END FtdIO.
