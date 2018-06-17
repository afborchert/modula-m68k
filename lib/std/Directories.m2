(* Modula-2 Library    -  SunOS 3 and 4  -     AFB 2/89 *)
(* (c) Universitaet Ulm, Sektion Informatik, D-7900 Ulm *)

IMPLEMENTATION MODULE Directories;

   (* this version bases on getdirentries(2) and
      works for SunOS 3 and 4
      but possibly not for future releases
   *)

   FROM Sys IMPORT getdirentries;
   FROM SysClose IMPORT Close;
   FROM SysLseek IMPORT Lseek;
   FROM SysOpen IMPORT Open;
   FROM SysStat IMPORT StatBuf, Fstat, IfDir;
   FROM SystemTypes IMPORT DirSize, OFF, rdonly;
   FROM Storage IMPORT ALLOCATE, DEALLOCATE;
   FROM Strings IMPORT StrCpy;
   FROM SYSTEM IMPORT UNIXCALL, ADR, ADDRESS;

   (* (* exported from definition module *)
   TYPE 
      FileName = ARRAY [0..DirSize-1] OF CHAR;
      Direct = 
         RECORD 
            ino: CARDINAL;
            name: FileName;
         END;
   *)

   TYPE
      Entry = POINTER TO EntryRec;
      EntryRec =
	 RECORD
	    fileno: CARDINAL;
	    reclen1, reclen2: CHAR;	(* 2-byte record length *)
	    namlen1, namlen2: CHAR;	(* 2-byte name length *)
	    name: FileName;		(* 0C-terminated *)
	 END;
      DIR = POINTER TO DirRec;
      DirRec =
	 RECORD
	    fd: CARDINAL;
	    blocksize: OFF;	(* blocksize *)
	    nbytes: OFF;	(* number of bytes returned *)
	    offset: OFF;	(* current offset in block *)
	    block: ADDRESS;	(* current block of directory *)
	    entry: Entry;	(* current entry *)
	    basep: OFF;
	 END;

   PROCEDURE OpenDir(VAR dirp: DIR; filename: ARRAY OF CHAR) : BOOLEAN;
      VAR
	 filedesc: CARDINAL;
	 statbuf: StatBuf;
   BEGIN
      IF Open(filedesc, filename, rdonly) AND Fstat(filedesc, statbuf) AND
	 (statbuf.mode * IfDir = IfDir) THEN
	 NEW(dirp);
	 WITH dirp^ DO
	    fd := filedesc;
	    blocksize := statbuf.blksize;
	    ALLOCATE(block, blocksize);
	    entry := NIL;
	    basep := 0;
	 END;
	 RETURN TRUE
      ELSE
	 RETURN FALSE
      END;
   END OpenDir;

   PROCEDURE ReadDir(dirp: DIR; VAR direct: Direct) : BOOLEAN;
      VAR
	 d0, d1: CARDINAL;
	 reclen: CARDINAL;
   BEGIN
      WITH dirp^ DO
	 REPEAT
	    IF entry = NIL THEN
	       IF UNIXCALL(getdirentries, d0, d1,
			   fd, block, blocksize, ADR(basep)) THEN
		  nbytes := d0;
		  IF nbytes = 0 THEN
		     RETURN FALSE
		  END;
		  entry := block;
	       ELSE
		  RETURN FALSE
	       END;
	    END;
	    WITH entry^ DO
	       direct.ino := fileno;
	       StrCpy(direct.name, name);
	       reclen := ORD(reclen1) * 100H + ORD(reclen2);
	    END;
	    INC(offset, reclen);
	    IF offset < nbytes THEN
	       entry := block + ORD(offset);
	    ELSE
	       entry := NIL;
	    END;
	 UNTIL direct.ino # 0;
	 RETURN TRUE
      END;
   END ReadDir;

   PROCEDURE TellDir(dirp: DIR; VAR offset: OFF) : BOOLEAN;
   BEGIN
      WITH dirp^ DO
	 offset := basep + offset
      END;
      RETURN TRUE
   END TellDir;

   PROCEDURE SeekDir(dirp: DIR; pos: OFF) : BOOLEAN;
   BEGIN
      WITH dirp^ DO
	 IF (pos >= basep) & (pos <= basep + nbytes) THEN
	    offset := pos - basep;
	 ELSIF Lseek(fd, pos, 0) THEN
	    basep := pos;
	    entry := NIL;
	 ELSE
	    RETURN FALSE
	 END;
      END;
      RETURN TRUE
   END SeekDir;

   PROCEDURE RewindDir(dirp: DIR) : BOOLEAN;
   BEGIN
      RETURN SeekDir(dirp, 0)
   END RewindDir;

   PROCEDURE CloseDir(VAR dirp: DIR);
   BEGIN
      WITH dirp^ DO
	 IF NOT Close(fd) THEN END;
	 DEALLOCATE(block, blocksize);
      END;
      DISPOSE(dirp);
   END CloseDir;

END Directories.
