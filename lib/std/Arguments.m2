IMPLEMENTATION MODULE Arguments;	(* mh 5/85 *)
					(* rev mh 6/88 *)

   FROM ASCII	 IMPORT nl;
   FROM StdIO	 IMPORT Fputc, stderr;
   FROM StrToNum IMPORT StrToInt, StrToCard, StrToOct, StrToHex;
   FROM Strings	 IMPORT StrCpy, StrLen;
   FROM StrSpec	 IMPORT StrPartCpy;
   FROM SysExit	 IMPORT Exit;

   CONST
      maxstrlen = 1024;
      defaultinfostring = "";

   TYPE
      String = ARRAY [0..maxstrlen-1] OF CHAR;

   VAR
      argindex,					(* ARGV index of argument *)
      lastarg,					(* ARGV index of last arg.*)
      arglen,					(* = StrLen(argument) *)
      charindex:  CARDINAL;
      argument,		(* argument[charindex] = next character to be read *)
      infostring: String;


   PROCEDURE ErrWrite(ch: CHAR);
   BEGIN
      IF ~Fputc(ch,stderr) THEN			(* cannot write to stderr *)
	 Exit(1)
      END;
   END ErrWrite;

   PROCEDURE ErrWriteString(str: ARRAY OF CHAR);
      VAR index: CARDINAL;
   BEGIN
      index := 0;
      WHILE (index <= HIGH(str)) & (str[index] > 0C) DO
	 ErrWrite(str[index]);
	 INC(index);
      END;
   END ErrWriteString;

   PROCEDURE Usage;					(* EXPORTED *)
      VAR cmdname: String;
   BEGIN
      ARGV(cmdname,0);
      ErrWriteString("Usage: ");
      ErrWriteString(cmdname); ErrWrite(" ");
      ErrWriteString(infostring); ErrWrite(nl);
      Exit(1)						(* NO RETURN *)
   END Usage;

   PROCEDURE NextArg;
   BEGIN
      IF argindex > lastarg THEN
	 RETURN
      END;
      INC(argindex);
      IF argindex <= lastarg THEN
	 ARGV(argument,argindex);
	 arglen := StrLen(argument);
      END;
      charindex := 0;
   END NextArg;

   PROCEDURE InitArgs(is: ARRAY OF CHAR);		(* EXPORTED *)
   BEGIN
      argindex := 0;
      NextArg;
      StrCpy(infostring,is);
   END InitArgs;

   PROCEDURE AllArgs;					(* EXPORTED *)
   BEGIN
      IF argindex <= lastarg THEN
	 Usage
      END;
   END AllArgs;

   PROCEDURE GetFlag(VAR flag: CHAR): BOOLEAN;		(* EXPORTED *)
   BEGIN
      IF (argindex > lastarg) OR (argument[0] # '-') OR
	    (argument[1] = 0C) THEN
	 RETURN FALSE
      END;
      IF charindex = 0 THEN
	 IF (argument[1] = "-") & (argument[2] = 0C) THEN
	    NextArg;
	    RETURN FALSE
	 END;
	 INC(charindex);
      END;
      flag := argument[charindex];
      INC(charindex);
      IF charindex >= arglen THEN
	 NextArg;
      END;
      RETURN TRUE
   END GetFlag;

   PROCEDURE GetOpt( VAR flag: CHAR; VAR plus: BOOLEAN): BOOLEAN;
   (* EXPORTED *)
   BEGIN
      IF (argindex > lastarg) OR
	    (argument[0] # "-") & (argument[0] # '+') OR
	    (argument[1] = 0C) THEN
	 RETURN FALSE
      END;
      plus := argument[0] = "+";
      IF charindex = 0 THEN
	 IF ~plus & (argument[1] = "-") & (argument[2] = 0C) THEN
	    NextArg;
	    RETURN FALSE
	 END;
	 INC(charindex);
      END;
      flag := argument[charindex];
      INC(charindex);
      IF charindex >= arglen THEN
	 NextArg;
      END;
      RETURN TRUE
   END GetOpt;

   PROCEDURE GetArg(VAR arg: ARRAY OF CHAR): BOOLEAN;	(* EXPORTED *)
   BEGIN
      IF argindex > lastarg THEN
	 RETURN FALSE
      END;
      StrCpy(arg, argument);
      NextArg;
      RETURN TRUE
   END GetArg;

   PROCEDURE FetchString(VAR string: ARRAY OF CHAR);	(* EXPORTED *)
   BEGIN
      IF argindex > lastarg THEN
	 Usage
      END;
      StrPartCpy(string, argument, charindex, maxstrlen);
      NextArg;
   END FetchString;

   PROCEDURE FetchCard(  VAR number: CARDINAL);		(* EXPORTED *)
      VAR buffer: String;
   BEGIN
      FetchString(buffer);
      IF ~StrToCard(buffer, number) THEN
	 Usage
      END;
   END FetchCard;

   PROCEDURE FetchInt(   VAR number: INTEGER);		(* EXPORTED *)
      VAR buffer: String;
   BEGIN
      FetchString(buffer);
      IF ~StrToInt(buffer, number) THEN
	 Usage
      END;
   END FetchInt;

   PROCEDURE FetchOct(   VAR number: CARDINAL);		(* EXPORTED *)
      VAR buffer: String;
   BEGIN
      FetchString(buffer);
      IF ~StrToOct(buffer, number) THEN
	 Usage
      END;
   END FetchOct;

   PROCEDURE FetchHex(   VAR number: CARDINAL);		(* EXPORTED *)
      VAR buffer: String;
   BEGIN
      FetchString(buffer);
      IF ~StrToHex(buffer, number) THEN
	 Usage
      END;
   END FetchHex;

   PROCEDURE UngetArg;					(* EXPORTED *)
   BEGIN
      IF charindex > 0 THEN
	 charindex := 0;
      ELSE
	 IF argindex > 1 THEN
	    DEC(argindex,2);
	    NextArg;
	 END;
      END;
   END UngetArg;

   PROCEDURE UngetOpt;					(* EXPORTED *)
   BEGIN
      IF charindex > 1 THEN
	 DEC(charindex);
      ELSE
	 charindex := 0;
	 UngetArg;
	 IF arglen > 0 THEN
	    charindex := arglen-1;
	 END;
      END;
   END UngetOpt;

BEGIN
   lastarg := ARGC()-1;
   argindex := 0;
   NextArg;
   infostring := defaultinfostring;
END Arguments.
