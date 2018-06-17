DEFINITION MODULE SysTermIO;

   (* see termio(7) for explanations *)

   CONST
      (* input modes *)
      ignbrk = { 31 };
      brkint = { 30 };
      ignpar = { 29 };
      parmrk = { 28 };
      inpck = { 27 };
      istrip = { 26 };
      inlcr = { 25 };
      igncr = { 24 };
      icrnl = { 23 };
      iuclc = { 22 };
      ixon = { 21 };
      ixany = { 20 };
      ixoff = { 19 };
      (* output modes *)
      opost = { 31 };
      olcuc = { 30 };
      onlcr = { 29 };
      ocrnl = { 28 };
      onocr = { 27 };
      onlret = { 26 };
      ofill = { 25 };
      ofdel = { 24 };
      (* delays for newline *)
      nldly = { 23 };		(* mask *)
      nl0 = { };
      nl1 = { 23 };
      (* delays for carriage return *)
      crdly = { 21, 22 };	(* mask *)
      cr0 = { };
      cr1 = { 22 };
      cr2 = { 21 };
      cr3 = { 21, 22 };
      (* delays for tabs *)
      tabdly = { 19, 20 };	(* mask *)
      tab1 = { 20 };
      tab2 = { 19 };
      tab3 = { 19, 20 };	(* expand tabs to spaces *)
      (* delays for backspaces *)
      bsdly = { 18 };		(* mask *)
      bs0 = { };
      bs1 = { 18 };
      (* delays for vertical tabs *)
      vtdly = { 17 };		(* mask *)
      vt0 = { };
      vt1 = { 17 };
      (* delays for form feeds *)
      ffdly = { 16 };
      ff0 = { };
      ff1 = { 16 };

      (* control modes *)
      cbaud = { 28..31 };	(* mask *)
      b0 = {};
      b50 = { 31 };
      b75 = { 30 };
      b110 = { 29, 30 };
      b134 = { 29 };
      b150 = { 29, 31 };
      b200 = { 29, 30 };
      b300 = { 29..31 };
      b600 = { 28 };
      b1200 = { 28, 31 };
      b1800 = { 28, 30 };
      b2400 = { 28, 30, 31 };
      b4800 = { 28, 29 };
      b9600 = { 28, 29, 31 };
      exta = { 28..30 };
      extb = { 28..31 };
      csize = { 26, 27 };	(* mask *)
      cs5 = {};
      cs6 = { 27 };
      cs7 = { 26 };
      cs8 = { 26, 27 };
      cstopb = { 25 };
      cread = { 24 };
      parenb = { 23 };
      parodd = { 22 };
      hupcl = { 21 };
      clocal = { 20 };
      
      (* line modes *)
      isig = { 31 };
      icanon = { 30 };
      xcase = { 29 };
      echo = { 28 };
      echoe = { 27 };
      echok = { 26 };
      echonl = { 25 };
      noflsh = { 24 };
   TYPE
      ControlChars = (vintr, vquit, verase, vkill, veof, veol, veol2, vswtch);
   CONST
      vmin = veof; vtime = veol;

   TYPE
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

      PROCEDURE SetTermIO(fd: CARDINAL; termio: TermIO) : BOOLEAN;

      PROCEDURE GetTermIO(fd: CARDINAL; VAR termio: TermIO) : BOOLEAN;

      PROCEDURE Baudrate(termio: TermIO) : CARDINAL;

END SysTermIO.
