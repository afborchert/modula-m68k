IMPLEMENTATION MODULE SysSema;

   FROM Errno IMPORT errno;
   FROM Sys IMPORT semsys;
   FROM Storage IMPORT ALLOCATE, DEALLOCATE;
   FROM SYSTEM IMPORT UNIXCALL, WORD, ADDRESS, BYTE;

   (* see semop(2), semctl(2), and semget(2) *)
   (*     /usr/include/sys/sem.h and /usr/include/sys/ipc.h *)

   (* (* exported from definition module *)
   CONST
      ipcalloc =  { 16 };		(* entry currently allocated *)
      ipccreat =  { 22 };		(* create entry if key doesn't exist *)
      ipcexcl  =  { 21 };		(* fail if key exists *)
      ipcnowait = { 20 };		(* error if request must wait *)
      semundo =   { 19 };		(* set up adjust on exit entry *)

   TYPE
      Key = LONGINT;
      SemCommand = (	ipcrmid,	(* remove identifier *)
			ipcset,		(* set options *)
			ipcstat,	(* get options *)
			getncnt,	(* get semncnt *)
			getpid,		(* get sempid *)
			getval,		(* get semval *)
			getall,		(* get all semval's *)
			getzcnt,	(* get semzcnt *)
			setval,		(* set semval *)
			setall);	(* set all semval's *)
      SemVal = INTEGER;
      SemopList = POINTER TO Semop;
      Semop =
	 RECORD
	    num: CARDINAL;	(* semaphore number *)
	    op: SemVal;  	(* semaphore operation *)
	    flg: BITSET;	(* operation flags *)
	    next: SemopList;
	 END;
   *)

   TYPE
      SemCall = (semctl, semget, semop);

   PROCEDURE SemCtl(semid: CARDINAL; semnum: CARDINAL;
		    cmd: SemCommand; arg: WORD) : INTEGER;
      (* returns -1 on failure *)
      VAR
	 r0, r1: INTEGER;
   BEGIN
      IF UNIXCALL(semsys, r0, r1, semctl, semid, semnum, cmd, arg) THEN
	 RETURN r0
      ELSE
	 errno := r0;
	 RETURN -1
      END;
   END SemCtl;

   PROCEDURE SemGet(VAR semid: CARDINAL;
		    key: Key; nsems: CARDINAL; semflg: BITSET) : BOOLEAN;
      VAR
	 r0, r1: INTEGER;
   BEGIN
      IF UNIXCALL(semsys, r0, r1, semget, key, nsems, semflg) THEN
	 semid := r0;
	 RETURN TRUE
      ELSE
	 errno := r0;
	 RETURN FALSE
      END;
   END SemGet;

   PROCEDURE SemOp(semid: CARDINAL; semoplist: SemopList) : SemVal;
      CONST
	 shortpw = SIZE(CARDINAL) DIV (2*SIZE(BYTE));
      VAR
	 nsops: CARDINAL;
	 sl: SemopList;
	 slp, slpstart: ADDRESS;
	 r0, r1: INTEGER;
	 ok: BOOLEAN;

      PROCEDURE AddShort(VAR ptr: ADDRESS; short: WORD);
	 CONST
	    bytespw = SIZE(WORD);
	 TYPE
	    Bytes = ARRAY [0..bytespw-1] OF BYTE;
	 VAR
	    pb: POINTER TO BYTE;
	    bytes: Bytes;
      BEGIN
	 bytes := Bytes(short);
	 pb := ptr; pb^ := bytes[bytespw-2]; INC(ptr, SIZE(BYTE));
	 pb := ptr; pb^ := bytes[bytespw-1]; INC(ptr, SIZE(BYTE));
      END AddShort;

   BEGIN
      nsops := 0;
      sl := semoplist;
      WHILE sl <> NIL DO
	 INC(nsops);
	 sl := sl^.next;
      END;
      ALLOCATE(slpstart, SIZE(Semop) * nsops DIV shortpw);
      slp := slpstart;
      sl := semoplist;
      WHILE sl <> NIL DO
	 WITH sl^ DO
	    AddShort(slp, num);
	    AddShort(slp, op);
	    AddShort(slp, flg);
	 END;
	 sl := sl^.next;
      END;
      ok := UNIXCALL(semsys, r0, r1, semop, semid, slpstart, nsops);
      DEALLOCATE(slpstart, SIZE(Semop) * nsops DIV shortpw);
      IF ok THEN
	 RETURN r0
      ELSE
	 errno := r0;
	 RETURN -1
      END;
   END SemOp;

END SysSema.
