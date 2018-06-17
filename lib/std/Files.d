DEFINITION MODULE Files;

   FROM SystemTypes IMPORT OFF;

   (* high level module for file handling *)

   IMPORT StdIO;

   TYPE
      FILE = StdIO.FILE;

   VAR Done: BOOLEAN;

   PROCEDURE OpenRead(VAR f: FILE; filename: ARRAY OF CHAR);

   PROCEDURE OpenWrite(VAR f: FILE; filename: ARRAY OF CHAR);

   PROCEDURE Close(f: FILE);

   PROCEDURE SetPos(f: FILE; pos: OFF);

   PROCEDURE GetPos(f: FILE; VAR pos: OFF);

   PROCEDURE Reset(f: FILE);

   PROCEDURE Delete(filename: ARRAY OF CHAR);

   PROCEDURE Rename(oldname, newname: ARRAY OF CHAR);

END Files.
