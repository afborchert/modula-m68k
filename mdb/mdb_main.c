/*
 *	MODULA 2 - debugger
 *
 *	(c) Andreas Borchert 1983, University of Ulm
 */

#include	<stdio.h>
/*#include	<sys/reg.h>*/
#include	"mdb.h"

#define	USAGE	fprintf(stderr, usage, name), exit(1)

#ifdef DEBUG
char	*usage = { "Usage: %s [-d] [-q] [a.out-file] [corefile | -]\n" };
#else
char	*usage = { "Usage: %s [-q] [a.out-file] [corefile | -]\n" };
#endif
char	*core  = { "core" };
char	*aout  = { "a.out" };

#ifdef DEBUG
int debug = 0;
#endif
int version = 0;	/* Compiler-Version */
int fflag = 0;		/* if on: no command mode; print backtrace only */

void gnu_licence()
{
	printf("mdb version %s\n", VERSION);
	printf("Copyright (C) 1983-1990 University of Ulm, Germany and\n");
	printf("              1986-1989 Free Software Foundation, Inc.\n");
	printf("mdb comes with ABSOLUTELY NO WARRANTY; type `H' for full details\n");
}

main(argc, argv)
	int	argc;
	char	**argv;
{	char	*name;
	long	addr;
	long	value;
	int	quiet = 0;	/* do not print version number on startup */

	name = *argv;
	while ( --argc && **++argv == '-' ) switch ( *++*argv ) {
#ifdef DEBUG
	case 'd' :
		++ debug;
		break;
#endif
	case 'v' :
		if (*++* argv)
			version = atoi(* argv);
		else if (--argc && **++argv)
			version = atoi(* argv);
		else
			USAGE;
		if (version < 0 || version > 3)
		{	fprintf(stderr, "Unkown version: %d\n", version);
			exit(1);
		}
		break;
	case 'f' :
		++fflag;
		break;
	case 'q' :
		++ quiet;
		break;
	default:
		USAGE;
	}
	if (argc)
	{	--argc;
		aout = *argv++;
	}
	if (argc)
	{	--argc;
		core = *argv++;
	}
	aout_init(aout);
#ifndef DEBUG_AOUT
	if (! fflag)
	{	quiet || gnu_licence();
		init(); /* of screen */
	}
	if (strcmp(core, "-"))
		core_init(core);
	if (fflag)
		fast();
	else
		cmdmode();
#endif DEBUG_AOUT
}

#if SYSV || BSD42

extern char * malloc();
extern int strlen();

char * strsave(s)
	char * s;
{	char * cp;

	if ((cp = malloc(strlen(s)+1)) == NULL)
	{	fprintf(stderr, "Not enough memory\n");
		exit(1);
	}
	strcpy(cp, s);
	return cp;
}
#endif
