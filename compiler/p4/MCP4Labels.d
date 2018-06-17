(* Modula-2 Multipass-Compiler               UNIX/MC68020 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
DEFINITION MODULE MCP4Labels;

   CONST
      LabelLength = 8;

   TYPE
      Label = ARRAY [ 0..LabelLength-1 ] OF CHAR;
      LabelPtr = POINTER TO Label;
      LabelType = CHAR; (* first letter of label *)

   PROCEDURE GetLabel(lt: LabelType; VAR l: LabelPtr);

   PROCEDURE PushLabel(lt: LabelType; l: LabelPtr);

   PROCEDURE PopLabel(lt: LabelType) : LabelPtr;

   PROCEDURE TopLabel(lt: LabelType) : LabelPtr;

END MCP4Labels.
