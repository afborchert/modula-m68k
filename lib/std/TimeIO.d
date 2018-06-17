(* Modula-2 Library    -  UNIX System V  -     AFB 9/88 *)
(* (c) Universitaet Ulm, Sektion Informatik, D-7900 Ulm *)
DEFINITION MODULE TimeIO;

   FROM Calendar IMPORT Time, Date;
   FROM StdIO IMPORT FILE;

   TYPE
      Style = (date,	(* date(1) and ctime(3) standard format *)
	       ls,	(* like the ls-command *)
	       env);	(* see for TIMEFMT in environment *)
   VAR
      Done: BOOLEAN;
      termCH: CHAR;

   PROCEDURE WriteTime(format: ARRAY OF CHAR; time: Time);
      (* the output format is very close to date(1): *)
      (* each field descriptor is preceded by % and will be *)
      (* replaced in the output by its corresponding value. *)
      (* WriteTime does not append a newline automatically  *)
      (* like date(1).                                      *)
      (* output is directed to StdIO.stdout                 *)

   PROCEDURE FwriteTime(file: FILE; format: ARRAY OF CHAR; time: Time);
      (* like WriteTime but output is directed to file      *)

   PROCEDURE SwriteTime(VAR string: ARRAY OF CHAR;
			format: ARRAY OF CHAR;
			time: Time);
      (* like WriteTime but output is put into string       *)

   PROCEDURE WriteTimeLike(style: Style; time: Time);
      (* write time to StdIO.stdout according to the given  *)
      (* style.                                             *)

   PROCEDURE FwriteTimeLike(file: FILE; style: Style; time: Time);

   PROCEDURE SwriteTimeLike(VAR string: ARRAY OF CHAR;
			    style: Style; time: Time);

   PROCEDURE ReadTime(VAR time: Time);
      (* read time from StdIO.stdin *)

   PROCEDURE FreadTime(file: FILE; VAR time: Time);

   PROCEDURE SreadTime(string: ARRAY OF CHAR; VAR time: Time);

   PROCEDURE WriteDate(format: ARRAY OF CHAR; date: Date);

   PROCEDURE FwriteDate(file: FILE; format: ARRAY OF CHAR; date: Date);

   PROCEDURE SwriteDate(VAR string: ARRAY OF CHAR;
			format: ARRAY OF CHAR; date: Date);

   PROCEDURE ReadDate(VAR date: Date);

   PROCEDURE FreadDate(file: FILE; VAR date: Date);

   PROCEDURE SreadDate(string: ARRAY OF CHAR; VAR date: Date);

   (* Reading  depends on a set of pattern describing valid      *)
   (* input formats. This formats are stored in an ordered list. *)
   (* If more than one pattern matches the input the first will  *)
   (* be chosen.                                                 *)
   (* Pattern consists of a sequence of letters and some special *)
   (* chars which must match the input. Whitespace (except nl)   *)
   (* is skipped by ReadTime and must not be given inside a      *)
   (* pattern.                                                   *)
   (* Legal Letters:                                             *)
   (*   'y': year, 'm': month,  'd': day                         *)
   (*   'H': hour, 'M': minute, 'S': second                      *)
   (* Examples:                                                  *)
   (*   m/d/yH:M:S        us-date, matches 10/22/86 13:12:14     *)
   (*   d.m.yH:M:S        german date, matches 22.10.86 13:12:14 *)
   (*   md,y              matches Oct 22, 1986                   *)

   PROCEDURE Append(pattern: ARRAY OF CHAR);
      (* appends a new pattern to the end of the list *)

   PROCEDURE Insert(pattern: ARRAY OF CHAR);
      (* inserts a pattern before the beginning of the list *)

   PROCEDURE ReleaseList;
      (* causes the list to be emptied *)

   PROCEDURE DefaultList;
      (* appends a list of standard patterns to the list *)
      (* this procedure is called during initialization of TimeIO *)

END TimeIO.
