DEFINITION MODULE PipeIO;

   FROM StdIO IMPORT FILE, MODE;

   PROCEDURE Popen(VAR f: FILE; cmd: ARRAY OF CHAR; mode: MODE;
                   buffered: BOOLEAN) : BOOLEAN;

   PROCEDURE Pclose(f: FILE) : BOOLEAN;

END PipeIO.
