(* Modula-2 makefile generator - UNIX System V - AFB 3/87 *)
(* (c) Universitaet Ulm, Sektion Informatik,   D-7900 Ulm *)
DEFINITION MODULE Options;

   TYPE
      VersionsManagement = (none, sccs, rcs);
   VAR
      SYMarchive: BOOLEAN; (* support SYM archive *)
      aronce: BOOLEAN;     (* update SYM archive with one ar-call only *)
      library: BOOLEAN;    (* hold objects in library *)
      profile: BOOLEAN;    (* support profiled library *)
      scanlibs: BOOLEAN;   (* scan libraries in MODPATH and MODRPATH *)
      lookforenv: BOOLEAN; (* look for macro in environment (on -c only) *)
      versmanag: VersionsManagement;

END Options.
