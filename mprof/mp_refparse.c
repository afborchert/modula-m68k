/*
 *	mprof - reffile parsing
 */

#include	<stdio.h>
#include	<setjmp.h>
#include	"mp_ref.h"
#include	"mp.h"

extern char * archivesearch();
extern char * get_first();
extern char * get_next();
extern char * searchfile();
extern char * calloc();

static void new();

static symbol sym;
static jmp_buf fail;
static struct proc ** list;
static int l_index, l_size;

struct proc ** build(modname, len)
	char *modname;
	int *len;
{	char filename[BUFSIZ];
	char *file;
	char * archive;

	if ((file = searchfile(modname, "r")) == NULL)
	{	archive = get_first();
		do
		{	if (file = archivesearch(archive, modname, "r"))
				break;
		}
		while (archive = get_next());
	}
	if (! file)
	{	fprintf(stderr, "%s: no reference file found\n", modname);
		return NULL;
	}
	if ( refopen(file) ) {
		perror(filename);
		return NULL;
		}
	if ( setjmp(fail) ) {
		fprintf(stderr, "%s: syntax error in reference file\n", file);
		refclose();
		return NULL;
		}
	new(& list, (l_size = 10) * sizeof(struct proc *));
	l_index = 0;
	parse();
	* len = l_index;
	refclose();
	return list;
}

parse()
{
	sym = getsym();
	module();
}

module()
{	struct proc * p;

	if ( sym != Mod )
		error();
	new(&p, sizeof(struct proc));
	head(p->p_name, NAMSIZ, &p->p_num);
	append(p);
	sym = getsym();
	block();
	if ( sym != End )
		error();
	sym = getsym();
}

head(ident, size, nr)
	char ident[];
	int size;
	int *nr;
{
	getnum();		/* LineNum */
	getident(ident, size);	/* Name */
	*nr = getnum();		/* ObjectNum */
}

block()
{
	while ( sym == Mod || sym == Proc || sym == Var ) switch ( sym )
	{	case Mod :	module(); break;
		case Proc :	procedure(); break;
		case Var :	variable(); break;
	}
}

procedure()
{	struct proc * p;

	if ( sym != Proc )
		error();
	new(&p, sizeof(struct proc));
	head(p->p_name, NAMSIZ, &p->p_num);
	getnum(); /* parlength */
	append(p);
	sym = getsym();
	block();
	if ( sym != End )
		error();
	sym = getsym();
}

variable()
{	char dummy[NAMSIZ];

	if ( sym != Var )
		error();
	getnum();		/* LineNumber */
	getident(dummy, NAMSIZ);/* Name */
	readref();		/* VarType */
	readref();		/* AddrMode */
	getnum();		/* Address */
	getnum();		/* Size */
	sym = getsym();
}

/*
 *	allocate space and check
 */

static void new(ptr, size)
	char **ptr;
	int size;
{
	if ( (*ptr = calloc(1, size)) == NULL )
	{	fprintf(stderr, "no space available\n");
		error();
	}
}

append(p)
	struct proc * p;
{
	if (l_index >= l_size)
		if ((list = realloc(list, (l_size += 10) * sizeof(struct proc *)))
			== NULL)
		{	perror("realloc");
			error();
		}
	list[l_index++] = p;
}

error()
{
	longjmp(fail);
}
