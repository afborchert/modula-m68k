/*
 *	search for reference files in MODPATH
 *
 *	principles of operation
 *
 *	for entering a new archive into the list just call
 *
 *	enter(archive)
 *
 *	for getting all archives in the right order call
 *	at first
 *
 *	get_first()
 *
 *	and afterwards
 *
 *	get_next()
 *
 *	until get_next() returns null.
 */

#include <stdio.h>

#ifdef MPROF
#include "mp.h"
#else
#include "mdb.h"
#endif

#ifdef MPROF
#define	my_perror	perror
#define	die		exit
#endif

#if SYSV || BSD42
#define	index(s,c)	strchr(s,c)
extern char * strchr();
#else
extern char * index();
#endif

extern char * calloc();
extern char * getenv();
extern char * malloc();
extern char * strsave();

struct list {
	char * l_archive;
	struct list * l_link;
};

static struct list * ref = NULL;
static struct list * last = NULL;

static init()
{	char * modpath;
	char * cp;
	char * dir;

	if (modpath = getenv("MODPATH"))
		while(1)
		{	if (cp = index(modpath, ':'))
				* cp = '\0';
			if ((dir = malloc(strlen(modpath)+6)) == NULL)
			{	my_perror("malloc");
				die(1);
			}
			if (modpath[0] == '\0')
				strcpy(dir, ".");
			else
				strcpy(dir, modpath);
			strcat(dir, "/REF");
			enter(dir);
			free(dir);
			if (cp)
				* cp = ':';
			else
				break;
			modpath = cp + 1;
		}
	if (modpath = getenv("MODLIB"))
	{	if ((dir = malloc(strlen(modpath)+6)) == NULL)
		{	my_perror("malloc");
			die(1);
		}
		strcpy(dir, modpath);
		strcat(dir, "/REF");
		enter(dir);
		free(dir);
	}
	else
	{	enter("/usr/lib/modula/REF");
		enter("/u/lib/modula/REF");
	}
	enter("REF");
}

enter(archive)
	char * archive;
{	struct list * new;

	if ((new = (struct list *) calloc(sizeof(struct list), 1)) == NULL)
	{	my_perror("calloc");
		die(1);
	}
	new->l_archive = strsave(archive);
	new->l_link = NULL;
	if (last)
		last->l_link = new;
	else
		ref = new;
	last = new;
}

static struct list * next = NULL;

char * get_next()
{	char * res;
	if (! next) return NULL;
	res = next->l_archive;
	next = next->l_link;
	return res;
}

char * get_first()
{	static int init_called = 0;

	if (! init_called)
	{	init();
		++init_called;
	}
	next = ref;
	return get_next();
}
