(* 
 *    E - formatted error messages on stdout  (ws 6/88)
 *      - common error handling for fomatting modules
 *    =================================================
 *)

DEFINITION MODULE E;

   FROM SYSTEM IMPORT BYTE;
   FROM Printf IMPORT FmtExitCode;
   FROM SystemTypes IMPORT Sig;

   (* ---- program exit codes ----  *)

   TYPE 
      ExitCode = [-1..255];

      (*      -1 : issue a message, abort with core dump *)
      (*       0 : issue a message, do   n o t   terminate program *)
      (*      >0 : issue a message, terminate program with exitcode *)
      (*    >200 : used by F,S, ... *)

   (* --- diagnostic --- *)

   PROCEDURE done () : BOOLEAN;

   PROCEDURE success() : FmtExitCode;

   (* --- set mode for errorhandling (default is "FmtError.Default") --- *)

   PROCEDURE setmode (mode : BITSET);

   PROCEDURE getmode (VAR mode : BITSET);

   (* --- output procedures --- *)
   (*     if fmt is empty, no message is issued *)

   PROCEDURE rror0 (exit : ExitCode; fmt : ARRAY OF CHAR);

   PROCEDURE rror1 (exit : ExitCode; fmt : ARRAY OF CHAR; i1 : ARRAY OF BYTE);

   PROCEDURE rror2 (exit : ExitCode; fmt : ARRAY OF CHAR; i1,i2 : ARRAY OF 
      BYTE);

   PROCEDURE rror3 (exit : ExitCode; fmt : ARRAY OF CHAR; i1,i2, i3 : 
      ARRAY OF BYTE );

   PROCEDURE rror4 (exit : ExitCode; fmt : ARRAY OF CHAR; i1, i2, i3, i4 : 
      ARRAY OF BYTE);

   PROCEDURE rror5 (exit : ExitCode; fmt : ARRAY OF CHAR; i1, i2, i3, i4, i5 
      : ARRAY OF BYTE);

   PROCEDURE rror6 (exit : ExitCode; fmt : ARRAY OF CHAR; i1, i2, i3, i4, i5,
      i6 : ARRAY OF BYTE);

   PROCEDURE rror7 (exit : ExitCode; fmt : ARRAY OF CHAR; i1, i2, i3, i4, i5,
      i6, i7 : ARRAY OF BYTE);

   PROCEDURE rror8 (exit : ExitCode; fmt : ARRAY OF CHAR; i1, i2, i3, i4, i5,
      i6, i7 , i8 : ARRAY OF BYTE);

   PROCEDURE EnterInitProc(proc : PROC);

   PROCEDURE EnterExitProc(proc : PROC);

   PROCEDURE ClearInitProc();

   PROCEDURE ClearExitProc();

   (* ---- define a text which is appended to fatal error messages ---- *)
   (*      empty strings clears, others are appended                    *)

   PROCEDURE AddFatalLine(text : ARRAY OF CHAR);

   (* ---- Abort program with core dump. If impossible exit with exicode 255. *)

   (* ---- Support standard signal handling *)

   TYPE
      SigSet = SET OF Sig;

   CONST
      UserSignal = SigSet {SIGHUP, SIGINT, SIGQUIT,SIGTERM};
      FatalSignal = SigSet {SIGILL..SIGUSR2} - SigSet{SIGSTOP, SIGKILL};
      
   PROCEDURE Signals(set : SigSet; proc : PROC);

END E. 
