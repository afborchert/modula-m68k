DEFINITION MODULE SysStat;

   FROM SystemTypes IMPORT TIME, OFF;

   TYPE
      StatBuf =
         RECORD
            dev: CARDINAL;
            ino: CARDINAL;
            mode: BITSET;
            nlink: CARDINAL;
            uid: CARDINAL;
            gid: CARDINAL;
            rdev: CARDINAL;
            size: OFF;
            atime: TIME;
	    spare1 : CARDINAL;
            mtime: TIME;
	    spare2 : CARDINAL;
            ctime: TIME;
	    spare3 : CARDINAL;
	    blksize : CARDINAL;
	    blocks : CARDINAL;
	    spare4 : ARRAY[0..1] OF CARDINAL;
         END;
   CONST
      (* bit masks for mode; bits 0..15 used *)
      FileType = { 0..3 };
      (* IF Ifxxx = mode * FileType *)
      IfDir = { 1 };      (* directory *)
      IfChr = { 2 };      (* character special *)
      IfBlk = { 1..2 };   (* block special *)
      IfReg = { 0 };      (* regular *)
      IfLnk = { 0,2 };	  (* symbolic link *)
      IfSock = { 0..1 };  (* socket *)
      IfFifo = { 3 };     (* fifo *)
      (* IF Isxxx <= mode THEN *)
      IsUid =  { 4 };     (* set user id on execution *)
      IsGid =  { 5 };     (* set group id on execution *)
      IsVtx =  { 6 };     (* save swapped text even after use *)
      (* permissions on file: IF ... <= mode *)
      OwnerRead = { 7 };  (* read permission, owner *)
      OwnerWrite = { 8 }; (* write permission, owner *)
      OwnerExec = { 9 };  (* execute/search permission, owner *)
      GroupRead = { 10 };
      GroupWrite = { 11 };
      GroupExec = { 12 };
      WorldRead = { 13 };
      WorldWrite = { 14 };
      WorldExec = { 15 };

   PROCEDURE Stat(file: ARRAY OF CHAR; VAR buf: StatBuf) : BOOLEAN;

   PROCEDURE Fstat(fd: CARDINAL; VAR buf: StatBuf) : BOOLEAN;

END SysStat.
