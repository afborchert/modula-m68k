MODULE transfer;

   FROM SYSTEM IMPORT ADDRESS;

   TYPE
      PROCESS = POINTER TO ProcessDesc;
      ProcessDesc =
	 RECORD
	    base, top, limit: ADDRESS;
	    p: PROC;
	    started: BOOLEAN;
	 END;

   PROCEDURE NEWPROCESS(P: PROC; A: ADDRESS; n: CARDINAL; VAR new: PROCESS);
   BEGIN
   END NEWPROCESS;

   PROCEDURE TRANSFER(VAR src, dest: PROCESS);
   BEGIN
   END TRANSFER;

END transfer.
