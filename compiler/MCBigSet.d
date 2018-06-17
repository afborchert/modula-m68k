DEFINITION MODULE MCBigSet;

   FROM MCBase IMPORT Constval, SetValuePtr, Stptr, Symbol;

   PROCEDURE InitConstSet(VAR c: Constval; sp: Stptr);
   (* create new SetValue field *)

   PROCEDURE ConstSetElement(c: Constval; from, to: CARDINAL);
   (* add constant set element range to c *)

   PROCEDURE TermConstSet(VAR c: Constval);
   (* link SetValue into chain of set constants *)

   PROCEDURE BigSetOp(c1, c2: Constval; VAR res: Constval; op: Symbol;
                      VAR tp: Stptr; VAR err: BOOLEAN);
   (* constant operations on big sets *)

END MCBigSet.
