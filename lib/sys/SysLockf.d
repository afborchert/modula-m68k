DEFINITION MODULE SysLockf;

   FROM SystemTypes IMPORT OFF;
   FROM StdIO IMPORT FILE;

   TYPE
      LockFunction = (unlock, lock, testandlock, test);


   PROCEDURE Lockf(fd: CARDINAL; function: LockFunction;
		   size: OFF) : BOOLEAN;

END SysLockf.
