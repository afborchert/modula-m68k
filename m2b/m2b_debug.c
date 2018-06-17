/* (c) Andreas Borchert 1983, University of Ulm */

/*
 * fuer's entwanzen...
 */

#ifdef DEBUG

#include	<stdio.h>
#include	"pb_mac.h"
#define	STACKSIZE	50

static	char	*stack[STACKSIZE];
static	int	top = 0;

nm_push(string)
char *string;
{
	if ( top >= STACKSIZE ) {
		++top;
		return;
		}
	stack[top++] = string;
}

nm_pop()
{
	if ( top <= 0 )
		fatal("Illegal call of `nm_pop'\n");
	--top;
}

nm_dump()
{
	fprintf(stderr,"Stack :\n-------\n");
	if ( top >= STACKSIZE ) {
		fprintf(stderr,"Stackoverflow !\n");
		top = STACKSIZE-1;
		}
	for ( --top ; top >= 0 ; --top )
		fprintf(stderr,"\t`%s'\n",stack[top]);
}

dump_all()
{
	fprintf(stderr,"Dump of pb :\n------------\n");
	nm_dump();
	in_dump();
	format_dump();
	pass2_dump();
}

#endif DEBUG
