DEFINITION MODULE ASCII;

   CONST

      (* control characters *)

      nul = 0C;   ack = 6C;   ff  = 14C;  dc2 = 22C;  can = 30C;  rs  = 36C;  
      soh = 1C;   bel = 7C;   cr  = 15C;  dc3 = 23C;  em  = 31C;  us  = 37C;  
      stx = 2C;   bs  = 10C;  so  = 16C;  dc4 = 24C;  sub = 32C;  sp  = 40C;
      etx = 3C;   ht  = 11C;  si  = 17C;  nak = 25C;  esc = 33C;  
      eot = 4C;   lf  = 12C;  dle = 20C;  syn = 26C;  fs  = 34C;  
      enq = 5C;   vt  = 13C;  dc1 = 21C;  etb = 27C;  gs  = 35C;

      (* other usual names *)

      null = nul;
      bell = bel;
      nl   = lf; (* new line *)
      tab  = ht;
      np   = ff; (* new page *)

      del  = 177C;

END ASCII.
