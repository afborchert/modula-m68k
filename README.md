# Historic Modula-2 compiler for the Motorola 68020 architecture

## Background

Modula-2 and the origins of our family of Modula-2 compilers have been
designed and developed at the Department of Computer Science, ETH
Zürich in Switzerland (see http://www.inf.ethz.ch) by Niklaus Wirth
and his team.

In December 1981, we licensed the sources of the M2M compiler (4-pass
compiler for the famous Lilith architecture) and derived new compilers
from it for the Concurrent 3200 architecture, the m68k processor and
the SPARCv8 architecture. All these compilers conform to PIM3 (Niklaus
Wirth, Programming in Modula-2, 3rd Edition, Springer-Verlag) but not to
the ISO/IEC standard 10514-1:1996.

Modula-2 and the origins of our family of Modula-2 compilers have been
designed and developed at the Department of Computer Science, ETH
Zürich in Switzerland (see http://www.inf.ethz.ch) by Niklaus Wirth
and his team.

## Architecture

This compiler was initially cross-developed for a Nixdorf Targon/31
(based on the m68k architecture) and later ported the m68k-based
Sun 3 architecture running SunOS 4. These sources are the latest
release we developed for the Sun 3 running SunOS 4.1.x.

## License

We have an agreement with the ETH Zürich that the sources which
have been derived from the M2M-compiler may be freely redistributed
provided that

> all derived sources clearly state that Modula-2 has been designed
> and developed at the Department of Computer Science, ETH Zurich in
> Switzerland.

All sources of the compiler of this distribution may be freely
redistributed if you follow the above term for the ETH-derived sources
*and* the terms of the GNU General Public License, Version 2 (as found
in the file COPYING).

The sources in the directories m2b, m2c, m2e, mdb, mmm, and mprof
may be freely redistributed under the terms of the GNU General Public
License, Version 2 (as found in the file COPYING).

The sources of the library (subdirectories lib and rts) may be freely
redistributed under the terms of the GNU Library General Public
License, Version 2 (as found in the file COPYING.LIB).

Note that the _MathLib_ module has been derived from the GNU library
(distributed under the terms of the GNU Library General Public License)
which in turn has been derived from sources which have been developed
at the University of California, Berkeley: This product includes
software developed by the University of California, Berkeley and its
contributors.

## Downloading binaries

Binaries are available for SunOS 4:
   https://www.mathematik.uni-ulm.de/modula/downloads/sun3/

## See also

More about the historic bootstrapping process that led to this
compiler can be found at the following repositories:
 * https://github.com/afborchert/lilith
 * https://github.com/afborchert/lilith-multipass-modula2-compiler
 * https://github.com/afborchert/modula-3200
