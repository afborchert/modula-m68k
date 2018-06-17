/*
 * mdb -- Modula-2 source level debugger
 * (c) University of Ulm 1983,1985,1987
 * configuration file for mdb
 * afb 12/87
 */

/*
 * select one of the UNIX variants
 */
#define SYSV	0	/* Unix System V or XELOS */
#define	ED7	0	/* Unix Edition VII */
#define BSD42 	1	/*enhanced version of 4.2BSD-UNIX */

/*
 * select one of the processors
 * as supported by your UNIX-version; sometimes it's necessary
 * to define M68010 even if you have M68020
 */
#define	PE3200	0	/* Perkin-Elmer 3200 series */
#ifndef M68		/* if not overrided by -DM68=1 */
#	define	M68010	0	/* Motorola 68010 */
#	define	M68020	1	/* Motorola 68020 */
#	if M68010 || M68020
#		define M68	1	/* Motorola M68xxx family */
#	else
#		define	M68	0
#	endif
#endif

/*
 * select one of the distributors
 */
#define	PE	0	/* Perkin-Elmer */
#define	NIXDORF	0
#define	SUN	1

/*
 * define PTRACEX to 1 if the system call ptracex() is available
 */
#define PTRACEX	0

#if PE3200 && SYSV
#	define CORE_BUG
#	define PTRACEX_BUG 1	/* ptracex() doesn't work in write-mode */
#else
#	if PTRACEX
#		define	PTRACEX_BUG 0
#	endif
#endif

#include <sys/param.h>
#if NIXDORF 
#	define _NFILE MAXUFILE
#endif
#if SUN
#    define _NFILE NOFILE
#endif

#ifndef VERSION
#define VERSION "2.3"
#endif
