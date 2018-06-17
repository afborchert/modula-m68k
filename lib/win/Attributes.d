(* Modula-2 Library    -  UNIX System V  -     AFB 5/88 *)
(* (c) Universitaet Ulm, Sektion Informatik, D-7900 Ulm *)
DEFINITION MODULE Attributes;

   FROM StdIO IMPORT FILE;
   FROM TermInfo IMPORT Term;

   TYPE
      Attribute = (standout, underline, reverse, blink, dim, bold, blank,
		   protect, alternate);
      AttributeSet = SET OF Attribute;
      AttrInfo;

   VAR
      Done: BOOLEAN;

   PROCEDURE OpenAttrInfo(VAR atinfo: AttrInfo;
			  VAR tinfo: Term;
			  baudrate: CARDINAL;
			  out: FILE);

   PROCEDURE CloseAttrInfo(VAR atinfo: AttrInfo);

   PROCEDURE AvailableAttributes(atinfo: AttrInfo; VAR atset: AttributeSet);

   PROCEDURE SetAttribute(atinfo: AttrInfo; at: Attribute);

   PROCEDURE SetAttributes(atinfo: AttrInfo; atset: AttributeSet);

   PROCEDURE ResetAttribute(atinfo: AttrInfo; at: Attribute);

   PROCEDURE ResetAttributes(atinfo: AttrInfo);

END Attributes.
