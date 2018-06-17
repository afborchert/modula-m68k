DEFINITION MODULE SysWrite;

   FROM SYSTEM IMPORT ADDRESS;

   PROCEDURE Write(fd: CARDINAL; ptr: ADDRESS;
                   VAR bytecount: CARDINAL) : BOOLEAN;

END SysWrite.
