IMPLEMENTATION MODULE SysLockf;

   FROM StdIO IMPORT FILE;
   FROM SystemTypes IMPORT OFF;
   FROM Sys IMPORT fcntl;
   FROM SysFcntl IMPORT Fcntl,FcntlRequest;
   FROM SYSTEM IMPORT ADR, WORD;

   TYPE
      Flock = POINTER TO FLOCK;
      FLOCK = 
	 RECORD
	    type1, type2 : CHAR;	(*rdlck,wrlck,unlck*)
	    whence1, whence2 : CHAR; (*choose starting offset*)
				  (*0 : start,1 : current pos*)
				  (* 2 :end of file*)
	    start : OFF;	(*relative offset in bytes*)
	    len : CARDINAL;	(*length in bytes; 0 means lock to EOF*)
	    pid1, pid2 : CHAR;	(*returned with getlk*)
	    x1, x2 : CHAR;	(*unused*)
	  END;
     LockTypes = ( unused, rdlck, wrlck, unlck);

   PROCEDURE Convert(char1, char2 : CHAR;value: CARDINAL);
   BEGIN
      char1 := CHR( value DIV 100H );
      char2 := CHR( value MOD 100H );
   END Convert;

   PROCEDURE Lockf(fd: CARDINAL; function: LockFunction; size: OFF) : BOOLEAN;
      VAR arg : Flock;
   BEGIN
   WITH arg^ DO
     start :=0;
     len := size;
     Convert(x1,x2,0);
     Convert(pid1,pid2,0);
     CASE function OF
      | unlock :
		Convert(type1,type2,CARDINAL(unlck));
		Convert(whence1,whence2,1);
		IF Fcntl(fd,setlk,arg) THEN
       		   RETURN TRUE
		ELSE
		   RETURN FALSE
		END(*IF*);
      | lock : 	
		Convert(type1,type2,CARDINAL(wrlck));
		Convert(whence1,whence2,1);
		IF Fcntl(fd,setlkw,arg) THEN
       		   RETURN TRUE
		ELSE
		   RETURN FALSE
		END(*IF*);
      | testandlock : 
			Convert(type1,type2,CARDINAL(wrlck));
			Convert(whence1,whence2,1);
			IF Fcntl(fd,setlk,arg) THEN
       		        RETURN TRUE
		  	ELSE
		        RETURN FALSE
			END(*IF*);
      | test : 	
		Convert(type1,type2,CARDINAL(wrlck));
		Convert(whence1,whence2,1);
		IF Fcntl(fd,getlk,arg) THEN
       		   RETURN TRUE
		ELSE
		   RETURN FALSE
		END(*IF*);
     END(*CASE*);
   END(*WITH*);
   END Lockf;

END SysLockf.
