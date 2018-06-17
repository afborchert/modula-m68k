IMPLEMENTATION MODULE SysIoctl;

   FROM SYSTEM IMPORT UNIXCALL, ADR, BYTE;
   FROM Errno IMPORT errno;
   FROM Sys IMPORT ioctl;

   (* (* from definition module *)
   CONST
      shift = 0;
      Tandem = { shift + 15 };
      Cbreak = { shift + 14 };
      Lcase  = { shift + 13 };
      Echo   = { shift + 12 };
      Crmod  = { shift + 11 };
      Raw    = { shift + 10 };
      Oddp   = { shift + 9 };
      Evenp  = { shift + 8 };
      Anyp   = Oddp + Evenp;
      Nldelay = { shift + 6 , shift + 7 };
      Tbdelay = { shift + 4 , shift + 5 };
      Xtabs  = { shift + 4 , shift + 5 };
      Crdelay = { shift + 2 , 3 };
      Vtdelay = { shift + 1 };
      Bsdelay = { shift + 0 };
      Alldelay = Bsdelay + Vtdelay + Crdelay + Xtabs + Tbdelay + Nldelay;

      IocVoid = { 2 };
      IocOut = { 1 };
      IocIn = { 0 };
      IocInOut = IocIn + IocOut;

      getd = BITSET(0) + IocOut;
      setd = BITSET(1) + IocIn;
      hpcl = BITSET(2) + IocVoid;
      modg = BITSET(3) + IocOut;
      mods = BITSET(4) + IocIn;
      getp = BITSET(8) + IocOut;
      setp = BITSET(9) + IocIn;
      setn = BITSET(10) + IocIn;
      excl = BITSET(13) + IocVoid;
      nxcl = BITSET(14) + IocVoid;
      flush = BITSET(16) + IocIn;
      setc = BITSET(17) + IocIn;
      getc = BITSET(18) + IocOut;
      (* BSD or SUN specific ioctl-calls *)
      lbis = BITSET(127) + IocIn;
      lbic = BITSET(126) + IocIn;
      lset = BITSET(125) + IocIn;
      lget = BITSET(124) + IocOut;
      sbrk = BITSET(123) + IocVoid;
      cbrk = BITSET(122) + IocVoid;
      cdtr = BITSET(120) + IocVoid;
      gprgp = BITSET(119) + IocOut;
      sprgp = BITSET(118) + IocIn;
      sltc = BITSET(117) + IocIn;
      gltc = BITSET(116) + IocOut;
      outq = BITSET(115) + IocOut;
      sti = BITSET(114) + IocIn;
      notty = BITSET(113) + IocVoid;
      pkt = BITSET(112) + IocIn;
      stop = BITSET(111) + IocVoid;
      start = BITSET(110) + IocVoid;
      mset = BITSET(109) + IocIn;
      mbis = BITSET(108) + IocIn;
      mbic = BITSET(107) + IocIn;
      mget = BITSET(106) + IocOut;
      remote = BITSET(105) + IocIn;
      gwinsz = BITSET(104) + IocOut;
      swinsz = BITSET(103) + IocIn;
      ucntl = BITSET(102) + IocIn;

      SizeOfSgttyb = 6;	(* size of corresponding C-structures *)
      SizeOfTchars = 6;
      SizeOfWinsize = 8;

   TYPE
      Sgttyb =
         RECORD
            ispeed: CHAR;
            ospeed: CHAR;
            erase: CHAR;
            kill: CHAR;
            flags: BITSET;
         END;

      Tchars =
         RECORD
            intrc: CHAR;
            quitc: CHAR;
            startc: CHAR;
            stopc: CHAR;
            eofc: CHAR;
            brkc: CHAR;
         END;

      Winsize =
	 RECORD
	    rows, cols: CARDINAL;
	    xpixels, ypixels: CARDINAL; (* not used *)
	 END;

   *)

   PROCEDURE Ioctl(fd: CARDINAL; request: BITSET;
                   VAR argp: ARRAY OF BYTE;
		   argpsize: CARDINAL) : BOOLEAN;
      CONST
         rch = 't';
      VAR r0, r1: CARDINAL;
          requ: BITSET;
   BEGIN
      (* read comments in /usr/include/sys/ioctl.h *)
      IF NOT (IocVoid <= request) THEN
	 request := BITSET(argpsize * 10000H) + request;
      END;
      requ := BITSET(ORD(rch) * 400B) + request;
      IF NOT UNIXCALL(ioctl, r0, r1, fd, requ, ADR(argp)) THEN
         errno := r0;
         RETURN FALSE;
      ELSE
         RETURN TRUE;
      END;
   END Ioctl;

   PROCEDURE Stty(fd: CARDINAL; argp: Sgttyb) : BOOLEAN;
   BEGIN
      RETURN Ioctl(fd, setp, argp, SizeOfSgttyb);
   END Stty;

   PROCEDURE Gtty(fd: CARDINAL; VAR argp: Sgttyb) : BOOLEAN;
   BEGIN
      RETURN Ioctl(fd, getp, argp, SizeOfSgttyb);
   END Gtty;

   PROCEDURE Isatty(fd: CARDINAL) : BOOLEAN;
      VAR ttyb: Sgttyb;
   BEGIN
      RETURN Gtty(fd, ttyb);
   END Isatty;

   PROCEDURE GetWinsize(fd: CARDINAL; VAR winbuf: Winsize) : BOOLEAN;
      VAR
	 winrec: ARRAY [0..SizeOfWinsize-1] OF CHAR;

      PROCEDURE Convert(i1, i2: CARDINAL) : CARDINAL;
      BEGIN
	 RETURN ORD(winrec[i1]) * 100H + ORD(winrec[i2])
      END Convert;

   BEGIN
      IF Ioctl(fd, gwinsz, winrec, SizeOfWinsize) THEN
	 WITH winbuf DO
	    rows := Convert(0, 1);
	    cols := Convert(2, 3);
	    xpixels := Convert(4, 5);
	    ypixels := Convert(6, 7);
	 END;
	 RETURN TRUE
      ELSE
	 RETURN FALSE
      END;
   END GetWinsize;

   PROCEDURE Baudrate(speed: CHAR) : CARDINAL;
      VAR
	 baudrate: CARDINAL;
   BEGIN
      baudrate := ORD(speed);
      IF    baudrate = b0    THEN RETURN     0
      ELSIF baudrate = b50   THEN RETURN    50
      ELSIF baudrate = b75   THEN RETURN    75
      ELSIF baudrate = b110  THEN RETURN   110
      ELSIF baudrate = b134  THEN RETURN   134
      ELSIF baudrate = b150  THEN RETURN   150
      ELSIF baudrate = b200  THEN RETURN   200
      ELSIF baudrate = b300  THEN RETURN   300
      ELSIF baudrate = b600  THEN RETURN   600
      ELSIF baudrate = b1200 THEN RETURN  1200
      ELSIF baudrate = b1800 THEN RETURN  1800
      ELSIF baudrate = b2400 THEN RETURN  2400
      ELSIF baudrate = b4800 THEN RETURN  4800
      ELSIF baudrate = b9600 THEN RETURN  9600
      ELSIF baudrate = exta  THEN RETURN 19200
      ELSE (* extb *)
	 RETURN 0
      END;
   END Baudrate;

END SysIoctl.
