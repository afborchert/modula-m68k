DEFINITION MODULE ScanPwfile;
(*
 *	utility functions for modules Passwd and EtcGroup
 *
 *	Martin Hasch, University of Ulm, Dec-06-1988
 *)

   FROM StdIO IMPORT FILE;
   FROM ASCII IMPORT nl;

   CONST
      fieldsep = ":";
      linesep = nl;

   PROCEDURE ReRead(f: FILE): BOOLEAN;

   PROCEDURE GetText(f: FILE; VAR text: ARRAY OF CHAR; sepchar: CHAR): BOOLEAN;

   PROCEDURE GetNumber(f: FILE; VAR number: CARDINAL; sepchar: CHAR): BOOLEAN;

END ScanPwfile.
