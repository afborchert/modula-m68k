
IMPLEMENTATION MODULE Bytes;

   FROM SYSTEM IMPORT ADDRESS, BYTE, WORD;

   PROCEDURE PINC(VAR add : ADDRESS; inc : CARDINAL);

   BEGIN
      INC(add,inc);
   END PINC;

   PROCEDURE PDEC(VAR add : ADDRESS; dec : CARDINAL);

   BEGIN
      DEC(add,dec);
   END PDEC;

   PROCEDURE ByteNCopy(to,from : ADDRESS; no : CARDINAL);

      CONST 
         WS = SIZE(WORD);
	 CS = SIZE(LONGREAL);

      TYPE 
         BP0   = POINTER TO BYTE;
         BP1   = POINTER TO ARRAY[0..WS-1] OF BYTE;
         BP2   = POINTER TO ARRAY[0..2*WS-1] OF BYTE;
         BP4   = POINTER TO ARRAY[0..4*WS-1] OF BYTE;
         BP8   = POINTER TO ARRAY[0..8*WS-1] OF BYTE;
         BP16  = POINTER TO ARRAY[0..16*WS-1] OF BYTE;
         BP32  = POINTER TO ARRAY[0..32*WS-1] OF BYTE;
         BP64  = POINTER TO ARRAY[0..64*WS-1] OF BYTE;
         BP128 = POINTER TO ARRAY[0..128*WS-1] OF BYTE;
         BP256 = POINTER TO ARRAY[0..256*WS-1] OF BYTE;
         BP512 = POINTER TO ARRAY[0..512*WS-1] OF BYTE;


      VAR 
         t0, f0     : BP0;
         t1, f1     : BP1;
         t2, f2     : BP2;
         t4, f4     : BP4;
         t8, f8     : BP8;
         t16, f16   : BP16;
         t32, f32   : BP32;
         t64, f64   : BP64;
         t128, f128 : BP128;
         t256, f256 : BP256;
         t512, f512 : BP512;

         align      : CARDINAL;
	 i          : CARDINAL;
(*
    xxx|xxxx|xxxx|x      cannot solve this situation
      x|xxxx|xxxx|xxx
 *)

   BEGIN 
      IF (no < WS) THEN 
         align := no;
      ELSE 
         align := to MOD WS;            (* distance to word edge *)
         IF align # (from MOD WS) THEN  (* same distance ? *)
            align := no;                (* cannot assign wordwise *)
         ELSIF align # 0 THEN     
            align := WS - align;        (* can skip to next edge *)
         END;
      END;
      FOR i:= 1 TO align DO             (* skip to word edge *)
         t0 := BP0(to);
         f0 := BP0(from);
         t0^ := f0^;
         INC(to);
         INC(from);
      END;
      DEC(no,align);
      LOOP 
         CASE no OF 
           0 : 
               EXIT;
         | 1..WS-1 : 
               t0 := BP0(to);
               f0 := BP0(from);
               t0^ := f0^;
               INC(to);
               INC(from);
               DEC(no);
         | WS..2*WS-1 : 
               t1 := BP1(to);
               f1 := BP1(from);
               t1^ := f1^;
               INC(to,WS);
	       INC(from,WS);
               DEC(no,WS);
         | 2*WS..4*WS-1 : 
               t2 := BP2(to);
               f2 := BP2(from);
               t2^ := f2^;
               INC(to,2*WS);
               INC(from,2*WS);
               DEC(no,2*WS);
         | 4*WS..8*WS-1 :
               t4 := BP4(to);
               f4 := BP4(from);
               t4^ := f4^;
               INC(to,4*WS);
               INC(from,4*WS);
               DEC(no,4*WS);
         | 8*WS..16*WS-1 :
               t8 := BP8(to);
               f8 := BP8(from);
               t8^ := f8^;
               INC(to,8*WS);
               INC(from,8*WS);
               DEC(no,8*WS);
         | 16*WS..32*WS-1 :
               t16 := BP16(to);
               f16 := BP16(from);
               t16^ := f16^;
               INC(to,16*WS);
               INC(from,16*WS);
               DEC(no,16*WS);
         | 32*WS..64*WS-1 : 
               t32 := BP32(to);
               f32 := BP32(from);
               t32^ := f32^;
               INC(to,32*WS);
               INC(from,32*WS);
               DEC(no,32*WS);
         | 64*WS..128*WS-1 :
               t64 := BP64(to);
               f64 := BP64(from);
               t64^ := f64^;
               INC(to,64*WS);
               INC(from,64*WS);
               DEC(no,64*WS);
         | 128*WS..256*WS-1 :
               t128 := BP128(to);
               f128 := BP128(from);
               t128^ := f128^;
               INC(to,128*WS);
               INC(from,128*WS);
               DEC(no,128*WS);
	 | 256*WS..512*WS-1 :
               t256 := BP256(to);
               f256 := BP256(from);
               t256^ := f256^;
               INC(to,256*WS);
               INC(from,256*WS);
               DEC(no,256*WS);
         ELSE 
            t512 := BP512(to);
            f512 := BP512(from);
            t512^ := f512^;
            INC(to,512*WS);
            INC(from,512*WS);
            DEC(no,512*WS);
         END;
      END;
   END ByteNCopy;

END Bytes.
