DEFINITION MODULE Chars;

   CONST 
      (* control characters as by ASCII(3MOD) *)
      nul   =  0C;
      soh   =  1C;
      stx   =  2C;
      etx   =  3C;
      eot   =  4C;
      enq   =  5C;
      ack   =  6C;
      bel   =  7C;
      bs    = 10C;
      ht    = 11C;
      lf    = 12C;
      vt    = 13C;
      ff    = 14C;
      cr    = 15C;
      so    = 16C;
      si    = 17C;
      dle   = 20C;
      dc1   = 21C;
      dc2   = 22C;
      dc3   = 23C;
      dc4   = 24C;
      nak   = 25C;
      syn   = 26C;
      etb   = 27C;
      can   = 30C;
      em    = 31C;
      sub   = 32C;
      esc   = 33C;
      fs    = 34C;
      gs    = 35C;
      us    = 37C;
      rs    = 36C;
      sp    = 40C;

      null  = nul;
      bell  = bel;
      tab   = ht;
      nl    = lf;
      np    = ff;
      del   = 177C;

      CtrlA = 1C;
      CtrlB = 2C;
      CtrlC = 3C;
      CtrlD = 4C;
      CtrlE = 5C;
      CtrlF = 6C;
      CtrlG = 7C;
      CtrlH = 10C;
      CtrlI = 11C;
      CtrlJ = 12C;
      CtrlK = 13C;
      CtrlL = 14C;
      CtrlM = 15C;
      CtrlN = 16C;
      CtrlO = 17C;
      CtrlP = 20C;
      CtrlQ = 21C;
      CtrlR = 22C;
      CtrlS = 23C;
      CtrlT = 24C;
      CtrlU = 25C;
      CtrlV = 26C;
      CtrlW = 27C;
      CtrlX = 30C;
      CtrlY = 31C;
      CtrlZ = 32C;

   TYPE 
      CharSet   = SET OF CHAR;
      CharClass = (nullc, letter, digit, punct, space, control, nonascii);

   CONST 
      UpperS      = CharSet {'A'..'Z'};
      LowerS      = CharSet {'a'..'z'};
      LetterS     = UpperS + LowerS;
      OctDigitS   = CharSet {'0'..'7'};
      DigitS      = OctDigitS + CharSet {'8','9'};
      HexDigitS   = DigitS + CharSet {'a'..'f','A'..'F'};
      AlphaNumS   = LetterS + DigitS;
      SpaceS      = CharSet {sp,ht,nl};
      WhiteS      = CharSet {sp,ht};

      AsciiS      = CharSet {0C..177C};

      ControlS    = CharSet {1C..37C, del};
      NonControlS = CharSet {40C..176C};

      PrintS      = NonControlS + SpaceS;
      NonPrintS   = AsciiS - PrintS;

      PunctS      = NonControlS - CharSet {sp} - AlphaNumS;

   PROCEDURE Class(ch : CHAR) : CharClass;
      (* nul     	   -> nullc
         LetterS           -> letter
	 DigitS            -> digit
	 SpaceS  	   -> space   
	 PunctS		   -> punct;
	 ControlS - SpaceS -> control *)

   PROCEDURE Lower(VAR ch : CHAR);

   PROCEDURE Upper(VAR ch : CHAR);

END Chars. 
