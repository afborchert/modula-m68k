(*
 *   Printf - formatted output  (ws 6/88)
 *   =====================================
 *
 *)

DEFINITION MODULE Printf;

   FROM SYSTEM IMPORT BYTE;
   FROM LongStrings IMPORT Long;

   (* ---- program exit codes ----  *)

   CONST 
      Pabort     = 201;
      Fabort     = 202;      (* standard exit codes of the indicated modules *)
      Sabort     = 203;
      Eabort     = 204;
      Labort     = 205;
      WabortP    = 206;
      WabortA    = 207;
      PanicAbort = 255;

      (* --- error handling modes (for procedures "setmode") --- *)

   CONST 
      TellSyntax  = 1;
      TellOutput  = 2;
      AbortSyntax = 3;
      AbortOutput = 4;
      Default     = {TellSyntax, TellOutput, AbortSyntax, AbortOutput};

      (* --- values returned by procedures "success" --- *)

   TYPE 
      FmtExitCode = ( Success, Undefined, FmtPanic, IllegalWidth,
         TooFewFormElems, TooManyFormElems, TooManyPercentOrStar,
         IllegalConvChar, MinHat0Comb, BslashAnd0, BadOctalChar, AllocFailed,
         StringTooSmall, CannotWriteFile, CannotWriteStderr, CannotWriteStdout
         , IllegalWindowOffset, WindowTooSmall, CannotFlushWindow,
         CannotAccessWindow, CannotWriteWindow);

      FmtExitSet  = SET OF FmtExitCode;

   CONST 
      SynError = FmtExitSet {IllegalWidth .. BadOctalChar};
      OutError = FmtExitSet {AllocFailed .. CannotWriteWindow};

   PROCEDURE ErrorReaction(reason : FmtExitCode; mode : BITSET; no : CARDINAL;
      VAR exit : INTEGER; VAR fmt : ARRAY OF CHAR);

   PROCEDURE Printf (output : Long; no : CARDINAL; VAR fmt: ARRAY OF CHAR;
      VAR i1 ,i2, i3, i4, i5, i6, i7,i8 : ARRAY OF BYTE) : FmtExitCode;

END Printf. 
