IMPLEMENTATION MODULE SysTermIO;

   FROM SYSTEM IMPORT UNIXCALL, ADR, BYTE;
   FROM Errno IMPORT errno;
   FROM Sys IMPORT ioctl;

   (* (* exported from definition module *)
   TYPE
      ControlChars = (vintr, vquit, verase, vkill, veof, veol, veol2, vswtch);
      ControlCharsRange = [MIN(ControlChars)..MAX(ControlChars)];
      InputModes = BITSET;
      OutputModes = BITSET;
      ControlModes = BITSET;
      LineModes = BITSET;
      TermIO =
	 RECORD
	    inputmodes: InputModes;
	    outputmodes: OutputModes;
	    controlmodes: ControlModes;
	    linemodes: LineModes;
	    linedisc: CHAR;
	    cc: ARRAY ControlCharsRange OF CHAR;
	 END;
   *)

   TYPE
      CTermIO =
	 RECORD
	    iflag1, iflag2: CHAR;
	    oflag1, oflag2: CHAR;
	    cflag1, cflag2: CHAR;
	    lflag1, lflag2: CHAR;
	    line: CHAR;
	    c1, c2, c3, c4, c5, c6, c7, c8: CHAR;
	 END;

   TYPE
      RequestType = CARDINAL;
   CONST
      tcgeta  = 40125401H;
      tcseta  = 80125402H;
      tcsetaw = 80125403H;
      tcsbrk  = 20005405H;
      tcxonc  = 20005406H;
      tcflsh  = 20005407H;

   PROCEDURE Ioctl(fd: CARDINAL; request: RequestType;
                   VAR argp: ARRAY OF BYTE) : BOOLEAN;
      VAR r0, r1: CARDINAL;
   BEGIN
      IF NOT UNIXCALL(ioctl, r0, r1, fd, request, ADR(argp)) THEN
         errno := r0;
         RETURN FALSE;
      ELSE
         RETURN TRUE;
      END;
   END Ioctl;

   PROCEDURE SetTermIO(fd: CARDINAL; termio: TermIO) : BOOLEAN;
      VAR
	 ctermio: CTermIO;

      PROCEDURE Convert(VAR flag1, flag2: CHAR; bs: BITSET);
      BEGIN
	 flag1 := CHR(CARDINAL(bs) DIV 100H);
	 flag2 := CHR(CARDINAL(bs) MOD 100H);
      END Convert;

   BEGIN
      WITH termio DO
	 WITH ctermio DO
	    Convert(iflag1, iflag2, inputmodes);
	    Convert(oflag1, oflag2, outputmodes);
	    Convert(cflag1, cflag2, controlmodes);
	    Convert(lflag1, lflag2, linemodes);
	    line := linedisc;
	    c1 := cc[vintr];
	    c2 := cc[vquit];
	    c3 := cc[verase];
	    c4 := cc[vkill];
	    c5 := cc[veof];
	    c6 := cc[veol];
	    c7 := cc[veol2];
	    c8 := cc[vswtch];
	 END;
      END;
      RETURN Ioctl(fd, tcseta, ctermio);
   END SetTermIO;

   PROCEDURE GetTermIO(fd: CARDINAL; VAR termio: TermIO) : BOOLEAN;
      VAR
	 ctermio: CTermIO;

      PROCEDURE Convert(flag1, flag2: CHAR; VAR bs: BITSET);
      BEGIN
	 bs := BITSET(ORD(flag1)*100H + ORD(flag2));
      END Convert;

   BEGIN
      IF NOT Ioctl(fd, tcgeta, ctermio) THEN RETURN FALSE END;
      WITH termio DO
	 WITH ctermio DO
	    Convert(iflag1, iflag2, inputmodes);
	    Convert(oflag1, oflag2, outputmodes);
	    Convert(cflag1, cflag2, controlmodes);
	    Convert(lflag1, lflag2, linemodes);
	    linedisc := line;
	    cc[vintr] := c1;
	    cc[vquit] := c2;
	    cc[verase] := c3;
	    cc[vkill] := c4;
	    cc[veof] := c5;
	    cc[veol] := c6;
	    cc[veol2] := c7;
	    cc[vswtch] := c8;
	 END;
      END;
      RETURN TRUE
   END GetTermIO;

   PROCEDURE Baudrate(termio: TermIO) : CARDINAL;
      VAR
	 baudrate: ControlModes;
   BEGIN
      WITH termio DO
	 baudrate := controlmodes * cbaud;
	 IF    baudrate = b0    THEN RETURN    0
	 ELSIF baudrate = b50   THEN RETURN   50
	 ELSIF baudrate = b75   THEN RETURN   75
	 ELSIF baudrate = b110  THEN RETURN  110
	 ELSIF baudrate = b134  THEN RETURN  134
	 ELSIF baudrate = b150  THEN RETURN  150
	 ELSIF baudrate = b200  THEN RETURN  200
	 ELSIF baudrate = b300  THEN RETURN  300
	 ELSIF baudrate = b600  THEN RETURN  600
	 ELSIF baudrate = b1200 THEN RETURN 1200
	 ELSIF baudrate = b1800 THEN RETURN 1800
	 ELSIF baudrate = b2400 THEN RETURN 2400
	 ELSIF baudrate = b4800 THEN RETURN 4800
	 ELSIF baudrate = b9600 THEN RETURN 9600
	 ELSE (* exta or extb *)
	    RETURN 0
	 END;
      END;
   END Baudrate;

END SysTermIO.
