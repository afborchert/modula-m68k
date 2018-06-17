IMPLEMENTATION MODULE StdFuncs;

   FROM Functions IMPORT Real,
      InstallStdFunc1, InstallStdFunc2, InstallStdConst;
   FROM MathLib IMPORT arctan, exp, ln, sin, cos, sqrt;

   PROCEDURE Power(x, y: Real) : Real;
   BEGIN
      RETURN exp(y*ln(x))
   END Power;

   PROCEDURE Log(base, x: Real) : Real;
   BEGIN
      RETURN ln(x)/ln(base)
   END Log;

   PROCEDURE Log10(x: Real) : Real;
   BEGIN
      RETURN Log(10.0, x)
   END Log10;

   PROCEDURE Tan(x: Real) : Real;
   BEGIN
      RETURN sin(x)/cos(x)
   END Tan;

BEGIN
   InstallStdFunc1("arctan", arctan);
   InstallStdFunc1("exp", exp);
   InstallStdFunc1("ln", ln);
   InstallStdFunc1("sin", sin);
   InstallStdFunc1("cos", cos);
   InstallStdFunc1("sqrt", sqrt);
   InstallStdFunc1("log10", Log10);
   InstallStdFunc1("tan", Tan);

   InstallStdFunc2("log", Log);
   InstallStdFunc2("pow", Power);

   InstallStdConst("pi", 4.0*arctan(1.0));
   InstallStdConst("e", exp(1.0));
END StdFuncs.
