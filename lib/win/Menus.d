(* Modula-2 Library    -  UNIX System V  -     AFB 6/88 *)
(* (c) Universitaet Ulm, Sektion Informatik, D-7900 Ulm *)
DEFINITION MODULE Menus;

   FROM Windows IMPORT Window;

   TYPE
      Menu;
   VAR
      Done: BOOLEAN;

   PROCEDURE CreateMenu(VAR menu: Menu;
			title: ARRAY OF CHAR);

   PROCEDURE AddCommand(menu: Menu;
			cmd: ARRAY OF CHAR;
			cmdno: CARDINAL);

   PROCEDURE AddProcedure(menu: Menu;
			  cmd: ARRAY OF CHAR;
			  cmdproc: PROC);

   PROCEDURE AddSubMenu(menu: Menu;
			cmd: ARRAY OF CHAR;
			submenu: Menu);

   PROCEDURE ExecMenu(menu: Menu; background: Window;
		      VAR selected: CARDINAL);

   PROCEDURE DisposeMenu(VAR menu: Menu);

END Menus.
