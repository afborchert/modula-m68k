#include	<stdio.h>

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
