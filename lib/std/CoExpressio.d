DEFINITION MODULE CoExpressions;

   FROM SYSTEM IMPORT WORD;

   TYPE
      CoRoutine;

   VAR
      main, source: CoRoutine;

   PROCEDURE Create(VAR cr: CoRoutine; p: PROC);

   PROCEDURE Send(cr: CoRoutine; value: WORD);

   PROCEDURE Receive(cr: CoRoutine; VAR value: WORD) : BOOLEAN;

   PROCEDURE SendChar(cr: CoRoutine; ch: CHAR);

   PROCEDURE ReceiveChar(cr: CoRoutine; VAR ch: CHAR) : BOOLEAN;

   PROCEDURE Fail;

   PROCEDURE Call(VAR cr: CoRoutine);

END CoExpressions.
