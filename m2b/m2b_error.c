/* Andreas Borchert 1983, Univrsity of Ulm */

/*
 * Diverse Fehlerausgaben/ausgaenge
 */

#include	<stdio.h>
#include	<signal.h>
#include	"m2b_mac.h"

#define	MAXERROR	20	/* maximale Anzahl von `error'-Aufrufen */

	int	counter;	/* zaehlt die Fehler */

extern	int	lines_in;
extern	int	lines_out;
extern	int	zeilenanfang;
extern	char	*in_file;
extern	char	*usage_str;
	int	fatalerror = 0;

	char	buferr[] = { "Buffer overflow." };
	char	pusherr[] = { "Modularisize your program !!!" };

/* VARARGS1 */
error ( controlstring , par1 , par2 )
char	*controlstring;
int	par1;
int	par2;
{
	if ( fatalerror )
		return;
	fprintf(stderr,"%s " , in_file);
	if ( lines_in ) {
		if ( zeilenanfang )/* ist das wirklich die Fehlerzeile ??? */
			fprintf(stderr,"(%4d) ",lines_in-1);
		else
			fprintf(stderr,"(%4d) ",lines_in);
		}
	fprintf(stderr,":");
	fprintf(stderr,controlstring,par1,par2);
	fprintf(stderr,"\n");
	++counter;
	if ( counter > MAXERROR )
		quit("Too many errors.");
}

/* VARARGS1 */
fatal ( controlstring , par )
char	*controlstring;
int	par;
{
	if ( fatalerror )
		return;
	++fatalerror;
	fprintf(stderr,"Fatal error in m2b : ");
	fprintf(stderr,controlstring,par);
	fprintf(stderr,"\n");
	shut_down();
#ifdef DEBUG
	dump_all();
#endif
	abort();
}

/* VARARGS1 */
quit ( controlstring , par1 , par2 )
char	*controlstring;
int	par1;
int	par2;
{
	if ( fatalerror )
		return;
	++fatalerror;
	fprintf(stderr,controlstring,par1,par2);
	fprintf(stderr," - QUIT\n");
	shut_down();
#ifdef	DEBUG
	dump_all();
	abort();
#endif
	exit(1);
}

/* VARARGS1 */
warning ( controlstring , par1 , par2 )
char	*controlstring;
int	par1;
int	par2;
{
	if ( fatalerror )
		return;
	fprintf(stderr,"Warning : ");
	--counter;
	error(controlstring,par1,par2);
}

usage ()
{
	if ( fatalerror )
		return;
	fprintf(stderr,"%s\n",usage_str);
	exit(1);		/* ohne shut_down, da usage nur am Anf. vork. */
}

interrupt ()
{
	ignorf(signal ( SIGINT , SIG_IGN ));
	quit("m2b has been interrupted.");
}

quitinterrupt ()
{
	ignorf(signal ( SIGQUIT , SIG_IGN ));
	fprintf(stderr,"Attention: Remove tempfile after debugging !!!");
	fprintf(stderr,"m2b: QUIT\n");
#ifdef DEBUG
	dump_all();
#endif
	abort();
}

sigill ()
{
	ignorf(signal ( SIGILL , SIG_IGN ));
	fatal("Illegal instruction.");
}

sigfpe()
{
	ignorf(signal ( SIGFPE , SIG_IGN ));
	fatal("Arithmetic fault.");
}

sigbus()
{
	ignorf(signal ( SIGBUS , SIG_IGN ));
	fatal("Memory fault.");
}

sigsegv()
{
	ignorf(signal ( SIGSEGV , SIG_IGN ));
	fatal("Segmentation violation.");
}

sigpipe()
{
	ignorf(signal ( SIGPIPE , SIG_IGN ));
	/* stilles Ende */
	++fatalerror; /* damit kein Output mehr erzeugt wird */
	shut_down();
	if ( counter )
		exit(1);
	else
		exit(0);
}

sigterm()
{
	ignorf(signal ( SIGTERM , SIG_IGN ));
	quit("m2b has been killed");
}


error_start ()
{
	counter = 0;
#ifdef notdef
	ignorf(signal ( SIGHUP , SIG_IGN ));
	ignorf(signal ( SIGINT , interrupt ));
	ignorf(signal ( SIGQUIT , quitinterrupt ));
	ignorf(signal ( SIGILL , sigill ));
	ignorf(signal ( SIGFPE , sigfpe ));
	ignorf(signal ( SIGBUS , sigbus ));
	ignorf(signal ( SIGSEGV , sigsegv ));
	ignorf(signal ( SIGPIPE , sigpipe ));
	ignorf(signal ( SIGTERM , sigterm ));
#endif
}

anz_errors ()
{
	return ( counter );
}
