/*
 *	m2e -- fetch source lines
 */

#include	<stdio.h>

#define	FORMAT	"%4d    %s"
#define	WHITE	"****    "
#define	WHITER	"        "
#define	VARARGS	_p1,_p2,_p3,_p4,_p5,_p6

static FILE *src;
static int eof;
static int last_line;
static char buf[BUFSIZ];
static int pos = 0;
extern int listing;


source_init(fp)
	FILE	*fp;
{
	src = fp;
	last_line = 0;
	eof = 0;
}

char * source(line)
	int	line;
{	int	index;

	for ( index = 0 ; index < line-last_line ; ++index )
		if ( fgets(buf, BUFSIZ, src) == NULL )
		{	++eof;
			return "*** end of file ***\n" ;
		}
	last_line = line;
	return buf;
}

/*
 *	calculate exact position
 */

setpos()
{	char * ptr;
	int epos;

	for (ptr = buf, epos = pos, pos = 0; ptr-buf < epos && *ptr; ++ptr)
		if (*ptr == '\t')
			pos += 8-(pos % 8);
		else
			++pos;
}

print_error(line, epos, code)
	int	line, epos, code;
{	int lline;

	if (listing)
		while(!eof && last_line < line)
		{	lline = last_line+1;
			pos = 0;
			printf(FORMAT, lline, source(lline));
		}
	pos = epos;
	if (line != last_line)
	{	printf(FORMAT, line, source(line));
		last_line = line;
	}
	if (listing)
		printf(WHITE);
	else
		printf(WHITER);
	setpos();
	errnr(code);
}

print_rest()
{	int line;
	char * ptr;

	line = last_line;
	while(ptr = source(++line), ! eof)
	{	pos = 0;
		printf(FORMAT, line, ptr);
	}
}

error(VARARGS)
{	int	i;

	for ( i = 0 ; i < pos ; ++i )
		putchar(' ');
	printf("^ ");
	printf(VARARGS);
	putchar('\n');
}
