IMPLEMENTATION MODULE Files;

   (* high level module for file handling *)

   FROM StdIO IMPORT read, write, Fopen, Fclose, Fputc, Fgetc,
      Ftell, Fseek;
   FROM SysLink IMPORT Link;
   FROM SysUnlink IMPORT Unlink;
   FROM SystemTypes IMPORT OFF;

   (* (* from definition module *)

   IMPORT StdIO;

   TYPE FILE = StdIO.FILE;

   VAR Done: BOOLEAN;

   *)

   PROCEDURE OpenRead(VAR f: FILE; filename: ARRAY OF CHAR);
   BEGIN
      Done := Fopen(f, filename, read, (* buffered = *) TRUE);
   END OpenRead;

   PROCEDURE OpenWrite(VAR f: FILE; filename: ARRAY OF CHAR);
   BEGIN
      Done := Fopen(f, filename, write, (* buffered = *) TRUE);
   END OpenWrite;

   PROCEDURE Close(f: FILE);
   BEGIN
      Done := Fclose(f);
   END Close;

   PROCEDURE SetPos(f: FILE; pos: OFF);
   BEGIN
      Done := Fseek(f, pos, 0);
   END SetPos;

   PROCEDURE GetPos(f: FILE; VAR pos: OFF);
   BEGIN
      Done := Ftell(f, pos);
   END GetPos;

   PROCEDURE Reset(f: FILE);
   BEGIN
      Done := Fseek(f, 0, 0);
   END Reset;

   PROCEDURE Delete(filename: ARRAY OF CHAR);
   BEGIN
      Done := Unlink(filename);
   END Delete;

   PROCEDURE Rename(oldname, newname: ARRAY OF CHAR);
   BEGIN
      IF Unlink(newname) THEN (* ignore result *) END;
      (* short circuit evaluation !!! *)
      Done := Link(oldname, newname) AND Unlink(oldname);
   END Rename;

END Files.
