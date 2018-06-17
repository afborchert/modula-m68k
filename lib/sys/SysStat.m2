IMPLEMENTATION MODULE SysStat; (* MC68020/Unix V.2 *)

   (* implementation depends on storage allocation of C *)
   (* this version doen't assume any form of alignment, i.e. *)
   (* struct stat is packed. *)
   (* This assumption is true for Nixdorf Targon/31 *)

   FROM SystemTypes IMPORT TIME, OFF;
   FROM UnixString IMPORT Copy, Buffer;
   FROM Sys IMPORT stat, fstat;
   FROM Errno IMPORT errno;
   FROM SYSTEM IMPORT UNIXCALL, ADR, ADDRESS, WORD;

   (* (* from definition module *)
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
      (* IF mask * FileType = ... *)
      IfDir = { 1 };      (* directory *)
      IfChr = { 2 };      (* character special *)
      IfBlk = { 1..2 };   (* block special *)
      IfReg = { 0 };      (* regular *)
(* on Nixdorf
      IfMpc = { 2..3 };   (* multiplexed char special *)
      IfMpb = { 1..3 };   (* multiplexed block special *)
    *)
(* on SUN *)
      IfLnk = { 0,2 };  	  (* symbolic link *)
      IfSock = { 0..1 };     (* socket *)
      IfFifo = { 3 };      (* fifo *)
(* end SUN*)
      (* IF ... <= mask THEN *)
      IsUid =  { 4 };     (* set user id on execution *)
      IsGid =  { 5 };     (* set group id on execution *)
      IsVtx =  { 6 };     (* save swapped text even after use *)
      (* permissions on file *)
      OwnerRead = { 7 };  (* read permission, owner *)
      OwnerWrite = { 8 }; (* write permission, owner *)
      OwnerExec = { 9 };  (* execute/search permission, owner *)
      GroupRead = { 10 };
      GroupWrite = { 11 };
      GroupExec = { 12 };
      WorldRead = { 13 };
      WorldWrite = { 14 };
      WorldExec = { 15 };
   *)

   TYPE
      StructStat = (* C-compatible structure *)
         RECORD
            dev1, dev2: CHAR;
            ino1, ino2, ino3, ino4: CHAR;
            mode1, mode2: CHAR;
            nlink1, nlink2: CHAR;
            uid1, uid2: CHAR;
            gid1, gid2: CHAR;
            rdev1, rdev2: CHAR;
	    size1, size2, size3, size4: CHAR;
	    atime1, atime2, atime3, atime4: CHAR;
	    spare11, spare21, spare31, spare41: CHAR;
	    mtime1, mtime2, mtime3, mtime4: CHAR;
	    spare12, spare22, spare32, spare42: CHAR;
	    ctime1, ctime2, ctime3, ctime4: CHAR;
	    spare13, spare23, spare33, spare43: CHAR;
	    blksize1, blksize2, blksize3, blksize4 : CHAR;
	    blocks1, blocks2, blocks3, blocks4: CHAR;
	    spare4 : ARRAY[0..1] OF CARDINAL;
	    (*
            size: OFF;
            atime, mtime, ctime: TIME;
	    *)
         END;

   PROCEDURE Expand(VAR from: StructStat; VAR to: StatBuf);

      PROCEDURE Convert(ch1, ch2: CHAR; VAR value: CARDINAL);
      BEGIN
         value := ORD(ch1) * 100H + ORD(ch2);
      END Convert;

      PROCEDURE Convert4(addr: ADDRESS; VAR value: WORD);
      BEGIN
	 value := addr^;
      END Convert4;

   BEGIN
      WITH from DO
         WITH to DO
            Convert(dev1, dev2, dev);
            Convert4(ADR(ino1), ino);
            mode := BITSET(ORD(mode1) * 1000000H + ORD(mode2) * 10000H);
            Convert(nlink1, nlink2, nlink);
            Convert(uid1, uid2, uid);
            Convert(gid1, gid2, gid);
            Convert(rdev1, rdev2, rdev);

	    Convert4(ADR(size1), size);
	    Convert4(ADR(atime1), atime);
	    Convert4(ADR(spare11), spare1);
	    Convert4(ADR(mtime1), mtime);
	    Convert4(ADR(spare12), spare2);
	    Convert4(ADR(ctime1), ctime);
	    Convert4(ADR(spare13), spare3);
	    Convert4(ADR(blksize1), blksize);
	    Convert4(ADR(blocks1), blocks);
         END;
	 to.spare4[0] := spare4[0];
	 to.spare4[1] := spare4[1];
	 (*
         to.size := size;
         to.atime := atime;
         to.mtime := mtime;
         to.ctime := ctime;
	 *)
      END;
   END Expand;

   PROCEDURE Stat(file: ARRAY OF CHAR; VAR buf: StatBuf) : BOOLEAN;
      VAR sb : StructStat; r0, r1: CARDINAL; Buf: Buffer;
   BEGIN
      Copy(Buf, file);
      IF UNIXCALL(stat, r0, r1, ADR(Buf), ADR(sb)) THEN
	 Expand(sb, buf);
         RETURN TRUE;
      ELSE
         errno := r0;
         RETURN FALSE;
      END;
   END Stat;

   PROCEDURE Fstat(fd: CARDINAL; VAR buf: StatBuf) : BOOLEAN;
      VAR  sb : StructStat; r0, r1: CARDINAL;
   BEGIN
      IF UNIXCALL(fstat, r0, r1, fd, ADR(sb)) THEN
	 Expand(sb, buf);
         RETURN TRUE;
      ELSE
         errno := r0;
         RETURN FALSE;
      END;
   END Fstat;

END SysStat.
