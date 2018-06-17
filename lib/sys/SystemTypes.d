(* Modula-2 Library    -  UNIX System V  -     AFB 9/88 *)
(* (c) Universitaet Ulm, Sektion Informatik, D-7900 Ulm *)
DEFINITION MODULE SystemTypes; (* and constants *)

   (* see...
	/usr/include/fcntl.h
	/usr/include/signal.h
	/usr/include/sys/dir.h
	/usr/include/sys/param.h
	/usr/include/sys/types.h
   *)

   CONST
      DirSize = 255;
      MaxOpenFiles = 30;
      (* file control options; arguments of fcntl(2) and open(2) *)
      rdonly = {};
      wronly = { 31 };
      rdwr = { 30 };
      ndelay = { 29 };
      append = { 28 };
      async = { 25 };
      creat = { 22 };
      trunc = { 21 };
      excl = { 20 };
      nbio = { 19 };
      sync = { 18 };
   TYPE
      Sig = (SIGRTI, SIGHUP, SIGINT, SIGQUIT, SIGILL,
             SIGTRAP, SIGIOT, SIGEMT, SIGFPE, SIGKILL,
	     SIGBUS, SIGSEGV, SIGSYS, SIGPIPE,
             SIGALRM, SIGTERM, SIGURG, SIGSTOP, SIGTSTP,
	     SIGCONT, SIGCHLD, SIGTTIN, SIGTTOU, SIGIO,
	     SIGXCPU, SIGXFSZ, SIGVTALARM, SIGPROF,
	     SIGWINCH, SIGLOST, SIGUSR1, SIGUSR2
	     (*SIGCLD, SIGPWR SYSV *));

      ProcessId = INTEGER;	(* ProcessId may be -1 for kill *)
      TIME = LONGINT;
      OFF = LONGINT;		(* offset/size of files *)

END SystemTypes.
