(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
DEFINITION MODULE MCP4Public;             (* LG *) (* REV AFB 3/84 *)

   (*
   EXPORT QUALIFIED
      assName, il1Name, ErrorsFound, Rflag, sflag, sourcefilename,
      FileNameLength, FileName, profile, Sflag, lflag, errName, errFile;
    *)

   CONST
      FileNameLength = 64;
   TYPE
      FileName = ARRAY[0..FileNameLength-1] OF CHAR;
   VAR 
      il1Name : FileName;		    (* interpass file (from m3) *)
      assName : FileName;		    (* assembler output file    *)
      errName : FileName;                   (* file with error messages *)
      sourcefilename : FileName;            (* XELOS: for .file pseudo op *)

      ErrorsFound : BOOLEAN;
      profile : BOOLEAN;                    (* if on: count proc calls  *)
      Sflag : BOOLEAN;                      (* produce nice output      *)
      lflag : BOOLEAN;                      (* produce line number labels *)
      errFile : BOOLEAN;                    (* if on: messages on errName *)
      Rflag : BOOLEAN;			    (* if on: no range checks   *)
      sflag : BOOLEAN;			    (* if on: no stack checks   *)
      Kflag : BOOLEAN;			    (* if on: linkl construct instead*)
					    (* of linkw*)

      mc68881: BOOLEAN;                     (* code for MC68881? *)

END MCP4Public. 
