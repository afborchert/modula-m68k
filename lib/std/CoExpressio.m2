IMPLEMENTATION MODULE CoExpressions;

   FROM SYSTEM IMPORT ADDRESS, NEWPROCESS, TRANSFER, WORD;
   FROM Storage IMPORT ALLOCATE, DEALLOCATE;

   (* (* from definition module *)
   TYPE
      CoRoutine;
   VAR
      main, source: CoRoutine;
   *)

   CONST
      StackSize = 2048;

   TYPE
      ResultType = WORD;
      CoRoutine = POINTER TO Process;

      Process =
         RECORD
            pc: ADDRESS; (* coroutine *)
            stack: ADDRESS; (* pointer to stack of pc *)
            caller: CoRoutine; (* return to caller on failure *)
         END;
   VAR
      cp: CoRoutine;
      failed: BOOLEAN;
      SuspendValue: ResultType;

   PROCEDURE Send(cr: CoRoutine; value: ResultType);
   BEGIN
      IF cr = NIL THEN RETURN END;
      source := cp;
      SuspendValue := value;
      failed := FALSE;
      cp := cr;
      TRANSFER(source^.pc, cp^.pc);
   END Send;

   PROCEDURE SendChar(cr: CoRoutine; ch: CHAR);
   BEGIN
      Send(cr, ORD(ch));
   END SendChar;

   PROCEDURE Fail;
      VAR src: CoRoutine;
   BEGIN
      source := cp;
      cp := cp^.caller;
      failed := TRUE;
      TRANSFER(source^.pc, cp^.pc);
   END Fail;

   PROCEDURE Create(VAR cr: CoRoutine; proc: PROC);
   BEGIN
      NEW(cr);
      WITH cr^ DO
         ALLOCATE(stack, StackSize);
         NEWPROCESS(proc, stack, StackSize, pc);
      END;
   END Create;

   PROCEDURE Receive(cr: CoRoutine; VAR value: ResultType) : BOOLEAN;
   BEGIN
      WITH cr^ DO
         cr^.caller := cp;
         source := cp;
         cp := cr;
         TRANSFER(source^.pc, cp^.pc);
         IF failed THEN
            DEALLOCATE(stack, StackSize);
            DISPOSE(cr);
            source := NIL;
            RETURN FALSE;
         ELSE
            value := SuspendValue;
            RETURN TRUE;
         END;
      END;
   END Receive;

   PROCEDURE ReceiveChar(cr: CoRoutine; VAR ch: CHAR) : BOOLEAN;
      VAR value: ResultType; returnValue: BOOLEAN;
   BEGIN
      returnValue := Receive(cr, value);
      IF returnValue THEN
         ch := CHR(CARDINAL(value));
      END;
      RETURN returnValue;
   END ReceiveChar;

   PROCEDURE Call(VAR cr: CoRoutine);
      VAR dummy: ResultType;
   BEGIN
      IF NOT Receive(cr, dummy) THEN
         cr := NIL;
      END;
   END Call;

BEGIN
   source := NIL;
   NEW(cp);
   main := cp;
END CoExpressions.
