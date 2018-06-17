/*
 *	m2e -- fetch error messages
 */

#include	<stdio.h>

#ifdef SYSV
#define	rindex(s,c)	strrchr(s,c)
extern char * strrchr();
#else
extern char * rindex();
#endif

#if defined(SYSV) && defined(ULM)
#	define	ERROR	"/u/lib/modula/m2_error"
#else
#	define	ERROR	"/usr/ATLAS/ruess/lib/modula/m2_error"
#endif

#define	MAXERRS	400

#define	TRUE	1
#define	FALSE	0

static	long	messages[MAXERRS];
static	FILE	*fp;
static	int	only_numbers;

errnr_init ()
{
	char	buf[BUFSIZ];
	long	pos;
	int	index;


	if ( (fp = fopen(ERROR, "r")) == NULL )
	{	only_numbers = TRUE;
		return;
	}
	only_numbers = FALSE;
	pos = 0;
	while ( fgets(buf,BUFSIZ,fp) )
	{	switch ( sscanf(buf,"%d",&index) ) {
#ifdef DEBUG
		case 0 :
			fatal("in `error'-file : error number missing");

#endif DEBUG
		default :
#ifdef	DEBUG
			if ( messages[index] )
				fatal("in `error'-file : error number %d twice declared",index);
#endif	DEBUG
			messages[index] = pos;

		}
		pos = ftell(fp);
	}
}

errnr(nr)
	int	nr;
{	char	buf[BUFSIZ];
	char * ptr;

	if (nr == 0 || messages[nr])
	{	fseek ( fp , messages[nr] , 0 );
		fscanf( fp , "%d" , &nr );
		fgets(buf, BUFSIZ, fp);
		if (ptr = rindex(buf, '\n'))
			*ptr = '\0';
		if ( buf[0] == '\t' )
			error("%s", buf+1);
		else
			error("%s", buf);
	}
	else
		error("error code : %d\n",nr);
}
