(* Modula-2 Library    -  UNIX System V  -     AFB 2/89 *)
(* (c) Universitaet Ulm, Sektion Informatik, D-7900 Ulm *)
DEFINITION MODULE Directories;

   FROM SystemTypes IMPORT DirSize, OFF;

   TYPE 
      DIR;

      FileName = ARRAY [0..DirSize-1] OF CHAR;
      Direct = 
         RECORD 
            ino: CARDINAL;
            name: FileName;
         END;

   PROCEDURE OpenDir(VAR dirp: DIR; filename: ARRAY OF CHAR) : BOOLEAN;

   PROCEDURE ReadDir(dirp: DIR; VAR direct: Direct) : BOOLEAN;

   PROCEDURE TellDir(dirp: DIR; VAR offset: OFF) : BOOLEAN;

   PROCEDURE SeekDir(dirp: DIR; offset: OFF) : BOOLEAN;

   PROCEDURE RewindDir(dirp: DIR) : BOOLEAN;

   PROCEDURE CloseDir(VAR dirp: DIR);

END Directories.
