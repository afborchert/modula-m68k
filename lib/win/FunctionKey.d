(* Modula-2 Library    -  UNIX System V  -     AFB 5/88 *)
(* (c) Universitaet Ulm, Sektion Informatik, D-7900 Ulm *)
DEFINITION MODULE FunctionKeys;

   FROM StdIO IMPORT FILE;
   FROM TermInfo IMPORT Term;

   TYPE
      FunctionKey =
	 ((* Unix System V.R2 *)
	  nokey,	(* no function key *)
	  backspace,
	  catab,	(* clear-all-tabs *)
	  clear,	(* clear screen or erase *)
	  ctab,		(* clear tab *)
	  dc,		(* delete character *)
	  dl,		(* delete line *)
	  down,		(* down arrow key *)
	  eic,		(* sent by rmir or smir in insert mode *)
	  eol,		(* clear-to-end-of-line *)
	  eos,		(* clear-to-end-of-screen *)
	  f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10,
	  home,
	  ic,		(* ins char/enter ins mode key *)
	  il,		(* insert line *)
	  left,		(* left arrow *)
	  ll,		(* home-down *)
	  npage,	(* next page *)
	  right,	(* right arrow *)
	  sf,		(* scroll-forward *)
	  sr,		(* scroll-backward *)
	  stab,		(* set-tab *)
	  up,		(* up arrow *)
	  (* Unix System V.R3 *)
          a1, a3, b2, c1, c3,	(* keypad *)
          btab,
          beg,
          cancel,
          close,
          command,
          copy,
          create,
          end,
          enter,
          exit,
          find,
          help,
          mark,
          message,
          move,
          next,
          open,
          options,
          previous,
          print,
          redo,
          reference,
          refresh,
          replace,
          restart,
          resume,
          save,
          suspend,
          Undo,
          sbeg,
          scancel,
          scommand,
          scopy,
          screate,
          sdc,
          sdl,
          select,
          send,
          seol,
          sexit,
          sfind,
          shelp,
          shome,
          sic,
          sleft,
          smessage,
          smove,
          snext,
          soptions,
          sprevious,
          sprint,
          sredo,
          sreplace,
          sright,
          srsume,
          ssave,
          ssuspend,
          sundo,
          f11,
          f12,
          f13,
          f14,
          f15,
          f16,
          f17,
          f18,
          f19,
          f20,
          f21,
          f22,
          f23,
          f24,
          f25,
          f26,
          f27,
          f28,
          f29,
          f30,
          f31,
          f32,
          f33,
          f34,
          f35,
          f36,
          f37,
          f38,
          f39,
          f40,
          f41,
          f42,
          f43,
          f44,
          f45,
          f46,
          f47,
          f48,
          f49,
          f50,
          f51,
          f52,
          f53,
          f54,
          f55,
          f56,
          f57,
          f58,
          f59,
          f60,
          f61,
          f62,
          f63);
      FunctionKeySet = SET OF FunctionKey;
      CharSet = SET OF CHAR;
      FKInfo;

   PROCEDURE OpenFKInfo(VAR fki: FKInfo; VAR t: Term;
			baudrate: CARDINAL;
			in, out: FILE);

   PROCEDURE CloseFKInfo(VAR fki: FKInfo);

   PROCEDURE Available(fki: FKInfo; VAR fkeys: FunctionKeySet);

   PROCEDURE StartSet(fki: FKInfo; VAR startset: CharSet);

   PROCEDURE EnableFunctionKeys(fki: FKInfo);

   PROCEDURE DisableFunctionKeys(fki: FKInfo);

   PROCEDURE Read(fki: FKInfo; timeout: BOOLEAN;
		  VAR fkey: FunctionKey; VAR ch: CHAR) : BOOLEAN;

END FunctionKeys.
