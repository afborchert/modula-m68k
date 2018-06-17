DEFINITION MODULE SysLseek;

   FROM SystemTypes IMPORT OFF;

   PROCEDURE Lseek(fd: CARDINAL; offset: OFF;
                   whence: CARDINAL) : BOOLEAN;

   PROCEDURE Tell(fd: CARDINAL; VAR offset: OFF) : BOOLEAN;

END SysLseek.
