/*
 *	m2e -- fetch error messages
 */

#include	<stdio.h>
#ifdef SYSV
#define rindex(s,c)	strrchr(s,c)
extern char * strrchr();
#else
extern char * rindex();
#endif

extern char * getenv();
extern char * malloc();

extern char * errfile;	/* imported from m2e.c */

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
	char * modlib;
	char * error_file;

	if (errfile)
		error_file = errfile;
	else if (modlib = getenv("MODLIB"))
	{	error_file = malloc(strlen(modlib)+10);
		strcpy(error_file, modlib);
		strcat(error_file, "/m2_error");
	}
	else
		error_file = NULL;
	if (!error_file || (fp = fopen(error_file, "r")) == NULL)
	{	only_numbers = TRUE;
		if (modlib)
			free(error_file);
		return;
	}
	if (modlib)
		free(error_file);
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
