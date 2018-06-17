IMPLEMENTATION MODULE PrioTab;

   FROM SystemTypes IMPORT Sig;

BEGIN
   PrioTab[SIGHUP] := 2;
   PrioTab[SIGINT] := 1;
   PrioTab[SIGQUIT] := 2;
   PrioTab[SIGILL] := 3;
   PrioTab[SIGTRAP] := 3;
   PrioTab[SIGIOT] := 3;
   PrioTab[SIGEMT] := 3;
   PrioTab[SIGFPE] := 3;
   PrioTab[SIGBUS] := 3;
   PrioTab[SIGSEGV] := 3;
   PrioTab[SIGSYS] := 3;
   PrioTab[SIGPIPE] := 2;
   PrioTab[SIGALRM] := 1;
   PrioTab[SIGTERM] := 2;
   PrioTab[SIGURG] := 1;
   PrioTab[SIGSTOP] := 1;
   PrioTab[SIGTSTP] := 1;
   PrioTab[SIGCONT] := 1;
   PrioTab[SIGCHLD] := 1;
   PrioTab[SIGTTIN] := 1;
   PrioTab[SIGTTOU] := 1;
   PrioTab[SIGIO] := 1;
   PrioTab[SIGXCPU] := 1;
   PrioTab[SIGXFSZ] := 1;
   PrioTab[SIGVTALARM] := 1;
   PrioTab[SIGPROF] := 1;
   PrioTab[SIGWINCH] := 1;
   PrioTab[SIGLOST] := 1;
   PrioTab[SIGUSR1] := 1;
   PrioTab[SIGUSR2] := 1;
END PrioTab.
