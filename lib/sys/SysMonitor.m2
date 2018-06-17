IMPLEMENTATION MODULE SysMonitor;

   (* ref to libc/gen/mon.c *)

   FROM SYSTEM IMPORT ADDRESS, TSIZE;
   FROM SysProfil IMPORT Profil;
   FROM SysCreat IMPORT Creat;
   FROM SysWrite IMPORT Write;
   FROM SysClose IMPORT Close;
   FROM SysExit IMPORT EnterCleanup;

   VAR
      sbuf: ADDRESS; (* keep address of buffer and bufsiz *)
      ssiz: CARDINAL;
      cleanup: BOOLEAN; (* cleanup is necessary *)

   PROCEDURE Cleanup;
   BEGIN
      IF cleanup THEN
	 Monitor(0, 0, 0, 0, 0);
      END;
   END Cleanup;

   PROCEDURE Monitor(lowpc, highpc, buf: ADDRESS; bufsiz, cntsiz: CARDINAL);
      CONST
	 map1to1 = 0FFFFH; (* scale value for 1-1 mapping *)
      TYPE
         HeaderPtr = POINTER TO ARRAY[0..2] OF CARDINAL;
      VAR
         scale: CARDINAL;
         header: HeaderPtr;
         ign: BOOLEAN;
	 fd: CARDINAL;
   BEGIN
      IF (lowpc = 0) & (highpc = 0) THEN
         ign := Profil(0, 0, 0, 0);
         IF Creat(fd, "mon.out", 666B) AND
            Write(fd, sbuf, ssiz) AND
            Close(fd) THEN
            (* ignore results *)
         END;
	 cleanup := FALSE;
         RETURN
      END;
      ssiz := bufsiz; sbuf := buf;

      header := HeaderPtr(buf);
      header^[0] := lowpc;
      header^[1] := highpc;
      header^[2] := cntsiz;

      INC(buf, (2*cntsiz + 3) * TSIZE(CARDINAL));
      DEC(bufsiz, (2*cntsiz + 3) * TSIZE(CARDINAL));
      IF bufsiz <= 0 THEN
         RETURN
      END;

      scale := highpc - lowpc;
      IF bufsiz < scale THEN
         scale := map1to1 DIV ((scale + bufsiz - 1) DIV bufsiz);
      ELSE
         scale := map1to1;
      END;
      ign := Profil(buf, bufsiz, lowpc, scale);
      cleanup := TRUE; EnterCleanup(Cleanup);
   END Monitor;

BEGIN
   cleanup := FALSE;
END SysMonitor.
