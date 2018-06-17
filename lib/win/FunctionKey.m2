(* Modula-2 Library    -  UNIX System V  -     AFB 5/88 *)
(* (c) Universitaet Ulm, Sektion Informatik, D-7900 Ulm *)
IMPLEMENTATION MODULE FunctionKeys;

   FROM StdIO IMPORT FILE, Fputc, Fgetc;
   FROM Strings IMPORT StrCpy, StrCmp, StrLen;
   FROM TermInfo IMPORT Term, String, TputsDelay;
   FROM Storage IMPORT ALLOCATE, DEALLOCATE;
   FROM SysSignal IMPORT Signal, default, old;
   FROM SystemTypes IMPORT SIGALRM;
   FROM SysAlarm IMPORT Alarm;
   FROM Delay IMPORT InitDelay, Delay;
   FROM SYSTEM IMPORT ADR;
   IMPORT SysAlarm;

   (* (* exported from definition module *)
   TYPE
      FunctionKey =
	 (nokey,	(* no function key *)
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
	  up);		(* up arrow *)
      FunctionKeySet = SET OF FunctionKey;
      CharSet = SET OF CHAR;
      FKInfo;
   *)
   CONST
      maxkeylen = 8;
   TYPE
      FKInfo = POINTER TO FKInfoRec;
      TermRef = POINTER TO Term;
      FunctionKeyRange = [MIN(FunctionKey)..MAX(FunctionKey)];
      KeyField = ARRAY [0..maxkeylen-1] OF CHAR;
      FKInfoRec =
	 RECORD
	    term: TermRef;
	    baudrate: CARDINAL;
	    devin, devout: FILE;
	    (* read previously and to be returned in Read *)
	    buf: ARRAY [0..maxkeylen-1] OF CHAR;
	    pos: CARDINAL;
	    cnt: CARDINAL;
	    fkset: FunctionKeySet;
	    fkey: ARRAY FunctionKeyRange OF KeyField;
	    lenset: BITSET;
	    lensetcard: CARDINAL; (* cardinality of lenset *)
	    (* set of function key characters *)
	    cset: ARRAY [0..maxkeylen-1] OF CharSet;
	    (* cardinality of cset[] *)
	    card: ARRAY [0..maxkeylen-1] OF CARDINAL;
	    (* single char function keys *)
	    mapset: CharSet;
	    map: BOOLEAN; (* true if cardinality of mapset > 0 *)
	    maptab: ARRAY CHAR OF FunctionKey;
	 END;
   VAR
      outfp: FILE;
      TimeoutCalled: BOOLEAN;

   PROCEDURE OpenFKInfo(VAR fki: FKInfo; VAR t: Term;
			bdrate: CARDINAL;
			in, out: FILE);
      VAR
	 index: CARDINAL;

      PROCEDURE FK(cap: String; index: FunctionKeyRange);
	 VAR
	    i: CARDINAL;
	    len: CARDINAL;
      BEGIN
	 WITH fki^ DO
	    IF cap[0] <> 0C THEN
	       INCL(fkset, index);
	       IF cap[1] <> 0C THEN
		  StrCpy(fkey[index], cap);
		  len := StrLen(cap);
		  IF NOT (len IN lenset) THEN
		     INCL(lenset, len);
		     INC(lensetcard);
		  END;
		  i := 0;
		  WHILE (i <= HIGH(fkey[index])) AND (fkey[index, i] <> 0C) DO
		     IF NOT (fkey[index, i] IN cset[i]) THEN
			INC(card[i]);
			INCL(cset[i], fkey[index, i]);
		     END;
		     INC(i);
		  END;
	       ELSE
		  INCL(mapset, cap[0]); map := TRUE;
		  maptab[cap[0]] := index;
	       END;
	    ELSE
	       fkey[index,0] := 0C;
	    END;
	 END;
      END FK;

   BEGIN
      NEW(fki);
      WITH fki^ DO
	 term := ADR(t);
	 baudrate := bdrate;
	 devin := in;
	 devout := out;
	 pos := 0;
	 cnt := 0;
	 fkset := FunctionKeySet{};
	 mapset := CharSet{}; map := FALSE;
	 lenset := {};
	 lensetcard := 0;
	 FOR index := 0 TO maxkeylen-1 DO
	    cset[index] := CharSet{};
	    card[index] := 0;
	 END;
	 WITH term^ DO
	    FK(KeyBackspace, backspace);
	    FK(KeyCatab, catab);
	    FK(KeyClear, clear);
	    FK(KeyCtab, ctab);
	    FK(KeyDc, dc);
	    FK(KeyDl, dl);
	    FK(KeyDown, down);
	    FK(KeyEic, eic);
	    FK(KeyEol, eol);
	    FK(KeyEos, eos);
	    FK(KeyF0, f0);
	    FK(KeyF1, f1);
	    FK(KeyF2, f2);
	    FK(KeyF3, f3);
	    FK(KeyF4, f4);
	    FK(KeyF5, f5);
	    FK(KeyF6, f6);
	    FK(KeyF7, f7);
	    FK(KeyF8, f8);
	    FK(KeyF9, f9);
	    FK(KeyF10, f10);
	    FK(KeyHome, home);
	    FK(KeyIc, ic);
	    FK(KeyIl, il);
	    FK(KeyLeft, left);
	    FK(KeyLl, ll);
	    FK(KeyNpage, npage);
	    FK(KeyRight, right);
	    FK(KeySf, sf);
	    FK(KeySr, sr);
	    FK(KeyStab, stab);
	    FK(KeyUp, up);
	    (* System V.R3 *)
            FK(KeyA1, a1);
            FK(KeyA3, a3);
            FK(KeyB2, b2);
            FK(KeyC1, c1);
            FK(KeyC3, c3);
            FK(KeyBtab, btab);
            FK(KeyBeg, beg);
            FK(KeyCancel, cancel);
            FK(KeyClose, close);
            FK(KeyCommand, command);
            FK(KeyCopy, copy);
            FK(KeyCreate, create);
            FK(KeyEnd, end);
            FK(KeyEnter, enter);
            FK(KeyExit, exit);
            FK(KeyFind, find);
            FK(KeyHelp, help);
            FK(KeyMark, mark);
            FK(KeyMessage, message);
            FK(KeyMove, move);
            FK(KeyNext, next);
            FK(KeyOpen, open);
            FK(KeyOptions, options);
            FK(KeyPrevious, previous);
            FK(KeyPrint, print);
            FK(KeyRedo, redo);
            FK(KeyReference, reference);
            FK(KeyRefresh, refresh);
            FK(KeyReplace, replace);
            FK(KeyRestart, restart);
            FK(KeyResume, resume);
            FK(KeySave, save);
            FK(KeySuspend, suspend);
            FK(KeyUndo, Undo);
            FK(KeySbeg, sbeg);
            FK(KeyScancel, scancel);
            FK(KeyScommand, scommand);
            FK(KeyScopy, scopy);
            FK(KeyScreate, screate);
            FK(KeySdc, sdc);
            FK(KeySdl, sdl);
            FK(KeySelect, select);
            FK(KeySend, send);
            FK(KeySeol, seol);
            FK(KeySexit, sexit);
            FK(KeySfind, sfind);
            FK(KeyShelp, shelp);
            FK(KeyShome, shome);
            FK(KeySic, sic);
            FK(KeySleft, sleft);
            FK(KeySmessage, smessage);
            FK(KeySmove, smove);
            FK(KeySnext, snext);
            FK(KeySoptions, soptions);
            FK(KeySprevious, sprevious);
            FK(KeySprint, sprint);
            FK(KeySredo, sredo);
            FK(KeySreplace, sreplace);
            FK(KeySright, sright);
            FK(KeySrsume, srsume);
            FK(KeySsave, ssave);
            FK(KeySsuspend, ssuspend);
            FK(KeySundo, sundo);
            FK(KeyF11, f11);
            FK(KeyF12, f12);
            FK(KeyF13, f13);
            FK(KeyF14, f14);
            FK(KeyF15, f15);
            FK(KeyF16, f16);
            FK(KeyF17, f17);
            FK(KeyF18, f18);
            FK(KeyF19, f19);
            FK(KeyF20, f20);
            FK(KeyF21, f21);
            FK(KeyF22, f22);
            FK(KeyF23, f23);
            FK(KeyF24, f24);
            FK(KeyF25, f25);
            FK(KeyF26, f26);
            FK(KeyF27, f27);
            FK(KeyF28, f28);
            FK(KeyF29, f29);
            FK(KeyF30, f30);
            FK(KeyF31, f31);
            FK(KeyF32, f32);
            FK(KeyF33, f33);
            FK(KeyF34, f34);
            FK(KeyF35, f35);
            FK(KeyF36, f36);
            FK(KeyF37, f37);
            FK(KeyF38, f38);
            FK(KeyF39, f39);
            FK(KeyF40, f40);
            FK(KeyF41, f41);
            FK(KeyF42, f42);
            FK(KeyF43, f43);
            FK(KeyF44, f44);
            FK(KeyF45, f45);
            FK(KeyF46, f46);
            FK(KeyF47, f47);
            FK(KeyF48, f48);
            FK(KeyF49, f49);
            FK(KeyF50, f50);
            FK(KeyF51, f51);
            FK(KeyF52, f52);
            FK(KeyF53, f53);
            FK(KeyF54, f54);
            FK(KeyF55, f55);
            FK(KeyF56, f56);
            FK(KeyF57, f57);
            FK(KeyF58, f58);
            FK(KeyF59, f59);
            FK(KeyF60, f60);
            FK(KeyF61, f61);
            FK(KeyF62, f62);
            FK(KeyF63, f63);
	 END;
      END;
   END OpenFKInfo;

   PROCEDURE CloseFKInfo(VAR fki: FKInfo);
   BEGIN
      DISPOSE(fki);
   END CloseFKInfo;

   PROCEDURE Available(fki: FKInfo; VAR fkeys: FunctionKeySet);
   BEGIN
      fkeys := fki^.fkset;
   END Available;

   PROCEDURE StartSet(fki: FKInfo; VAR startset: CharSet);
   BEGIN
      startset := fki^.cset[0];
   END StartSet;

   PROCEDURE EnableFunctionKeys(fki: FKInfo);
   BEGIN
      WITH fki^ DO
	 WITH term^ DO
	    IF OK(KeypadXmit) THEN
	       InitOut(fki);
	       WITH term^ DO
		  Out(KeypadXmit);
	       END;
	    END;
	 END;
      END;
   END EnableFunctionKeys;

   PROCEDURE DisableFunctionKeys(fki: FKInfo);
   BEGIN
      WITH fki^ DO
	 WITH term^ DO
	    IF OK(KeypadLocal) THEN
	       InitOut(fki);
	       WITH term^ DO
		  Out(KeypadLocal);
	       END;
	    END;
	 END;
      END;
   END DisableFunctionKeys;

   PROCEDURE Timeout;
   BEGIN
      TimeoutCalled := TRUE;
   END Timeout;

   PROCEDURE Read(fki: FKInfo; timeout: BOOLEAN;
		  VAR funckey: FunctionKey; VAR ch: CHAR) : BOOLEAN;
      VAR
	 c: CHAR;
	 key: FunctionKeyRange;
	 secs: CARDINAL; (* SysAlarm.previous after Alarm(1) *)
	 sigalrm: PROC;  (* SysSignal.old after Signal *)

      PROCEDURE Search(VAR funckey: FunctionKey) : BOOLEAN;
	 VAR key: FunctionKeyRange;
      BEGIN
	 WITH fki^ DO
	    IF pos <= HIGH(buf) THEN
	       buf[pos] := 0C;
	    END;
	    FOR key := MIN(FunctionKeyRange) TO MAX(FunctionKeyRange) DO
	       IF StrCmp(fkey[key], buf) = 0 THEN
		  funckey := key;
		  RETURN TRUE
	       END;
	    END;
	    RETURN FALSE
	 END;
      END Search;

   BEGIN
      WITH fki^ DO
	 funckey := nokey; ch := 0C;
	 IF cnt > 0 THEN
	    ch := buf[pos];
	    INC(pos); DEC(cnt);
	 ELSIF (card[0] = 0) & NOT map THEN (* no function keys at all *)
	    IF NOT Fgetc(ch, devin) THEN ch := 0C END;
	 ELSIF Fgetc(c, devin) THEN
	    IF c IN cset[0] THEN
	       TimeoutCalled := FALSE; secs := 0; sigalrm := default;
	       IF timeout AND Signal(SIGALRM, Timeout) AND Alarm(1) THEN
		  sigalrm := old;
		  secs := SysAlarm.previous;
		  IF secs > 0 THEN
		     DEC(secs);
		  END;
	       END;
	       buf[0] := c; pos := 1; cnt := 1;
	       IF lensetcard > 1 THEN
		  WHILE (NOT (pos IN lenset) OR NOT Search(funckey)) AND
			(pos <= HIGH(buf)) AND (card[pos] > 0) AND
			(c IN cset[pos-1]) AND Fgetc(c, devin) AND
			NOT TimeoutCalled DO
		     buf[pos] := c; INC(pos); INC(cnt);
		  END;
	       ELSE
		  WHILE (pos <= HIGH(buf)) AND (card[pos] > 0) AND
			(c IN cset[pos-1]) AND Fgetc(c, devin) AND
			NOT TimeoutCalled DO
		     buf[pos] := c; INC(pos); INC(cnt);
		  END;
		  IF NOT Search(funckey) THEN END;
	       END;
	       IF timeout THEN
		  IF NOT Alarm(0) THEN END;
		  IF NOT Signal(SIGALRM, sigalrm) THEN END;
		  IF NOT Alarm(secs) THEN END;
	       END;
	       IF funckey = nokey THEN
		  ch := buf[0]; pos := 1; DEC(cnt);
	       ELSE
		  cnt := 0;
	       END;
	    ELSIF c IN mapset THEN
	       funckey := maptab[c];
	       ch := c;
	    ELSE
	       ch := c;
	    END;
	 ELSE
	    RETURN FALSE
	 END;
      END;
      RETURN TRUE
   END Read;

   (* local procedures *)

   PROCEDURE InitOut(fki: FKInfo);
   BEGIN
      WITH fki^ DO
	 outfp := devout;
	 WITH term^ DO
	    InitDelay(baudrate, PadChar[0], Write);
	 END;
      END;
   END InitOut;

   PROCEDURE Write(ch: CHAR);
   BEGIN
      IF NOT Fputc(ch, outfp) THEN END;
   END Write;

   PROCEDURE Out(VAR s: ARRAY OF CHAR);
   BEGIN
      TputsDelay(s, 1, Write, Delay);
   END Out;

   PROCEDURE OK(VAR s: ARRAY OF CHAR) : BOOLEAN;
   BEGIN
      RETURN s[0] <> 0C
   END OK;

END FunctionKeys.
