DEFINITION MODULE SysSema;

   FROM SYSTEM IMPORT WORD;

   (* see semop(2), semctl(2), and semget(2) *)
   (*     /usr/include/sys/sem.h and /usr/include/sys/ipc.h *)

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

   PROCEDURE SemCtl(semid: CARDINAL; semnum: CARDINAL;
		    cmd: SemCommand; arg: WORD) : INTEGER;
      (* returns -1 on failure *)

   PROCEDURE SemGet(VAR semid: CARDINAL;
		    key: Key; nsems: CARDINAL; semflg: BITSET) : BOOLEAN;

   PROCEDURE SemOp(semid: CARDINAL; semoplist: SemopList) : SemVal;

END SysSema.
