DEFINITION MODULE Plot;

   FROM StdIO IMPORT FILE;

   (* device independent plotter interface; see plot(3) and plot(5) *)

   PROCEDURE OpenPlot(f: FILE);

   PROCEDURE ClosePlot;

   PROCEDURE Move(xto, yto: INTEGER);

   PROCEDURE Cont(xto, yto: INTEGER);

   PROCEDURE Point(xpoint, ypoint: INTEGER);

   PROCEDURE Line(xfrom, yfrom, xto, yto: INTEGER);

   PROCEDURE String(str: ARRAY OF CHAR);

   PROCEDURE Arc(xcenter, ycenter, xstart, ystart, xend, yend: INTEGER);

   PROCEDURE Circle(xcenter, ycenter, radius: INTEGER);

   PROCEDURE Erase;

   PROCEDURE LineMod(style: ARRAY OF CHAR);

   PROCEDURE Space(xupleft, yupleft, xlowright, ylowright: INTEGER);

   PROCEDURE Reverse(xupleft, yupleft, xlowright, ylowright: INTEGER);

   PROCEDURE Polygon(xcenter, ycenter, xstart, ystart, edges: INTEGER);

   PROCEDURE CharMod(plotchar: CHAR);

END Plot.
