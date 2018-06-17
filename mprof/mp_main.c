/*
 *	mprof -- main
 */

#include	<stdio.h>

#define	USAGE	(fprintf(stderr, usage, name), exit(1))

char usage[] = { "Usage: %s [-t] [-c] [-n] [-h] [file [profile-file]]\n" };

char sortflag = '\0';	/* default: no sorting at all */
int hflag = 0;		/* if on: suppress the heading */

main(argc, argv)
	int argc;
	char ** argv;
{	char * name;
	char * aoutfile;
	char * monout;

	name = * argv;
	aoutfile = "a.out";
	monout = "mon.out";

	while (--argc && **++argv == '-') switch (*++* argv) {
	case 't':	sortflag = 't'; break;
	case 'c':	sortflag = 'c'; break;
	case 'n':	sortflag = 'n'; break;
	case 'h':	hflag = 1; break;
	default:
		USAGE;
	}
	if (argc)
	{	--argc;
		aoutfile = * argv++;
	}
	if (argc)
	{	--argc;
		monout = * argv++;
	}
	if (argc)
		USAGE;
	aout_init(aoutfile);
	statistic(monout);
}
