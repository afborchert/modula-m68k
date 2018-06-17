/*
 *	mdb - reffile parsing
 */

#include	<stdio.h>
#include	<setjmp.h>
#include	"mdb_ref.h"
#include	"mdb_bp.h"
#include	"mdb_tree.h"

extern char * archivesearch();
extern char * get_first();
extern char * get_next();
extern char * searchfile();
extern char * calloc();

static void new();

extern int version;
extern int fflag;

tree *parse();
tree *module();
tree *block();
tree *procedure();
tree *variable();

static symbol sym;
static jmp_buf fail;
static tree *last;
static int plevel; /* procedure nest level */
static int level;  /* nest level */

tree *build(modname)
	char *modname;
{	char filename[BUFSIZ];
	char *file;
	tree *result;
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
	{	message("%s: no reference file found", modname);
		if (fflag)
			return NULL;
		getfilename(filename, BUFSIZ);
		if (! filename[0]) return NULL;
		if ((file = archivesearch(filename, modname, "r")) == NULL)
			file = filename;
		else
			enter(filename);
	}
	if (refopen(file))
	{	my_perror(file);
		return NULL;
	}
	if (setjmp(fail))
	{	message("%s: syntax error in reference file", modname);
		refclose();
		return NULL;
	}
	result = parse();
	result->t_father = NULL;
	result->t_left = NULL;
	result->t_right = NULL;
	refclose();
	return result;
}

tree *parse()
{	tree *result;
	last = NULL;
	plevel = 0;
	level = 0;
	sym = getsym();
	result = module();
	result->t_father = NULL;
	return result;
}

void link(t)
	tree * t;
{
	if (last) last->t_link = t;
	t->t_link = NULL;
	last = t;
}

tree *module()
{	tree *mod;

	if ( sym != Mod )
		error();
	new(&mod, sizeof(tree));
	mod->t_type = T_MOD;
	mod->t_plevel = plevel;
	if (level == 0)
		link(mod);		/* global modules first */
	head(&mod->t_line, mod->t_name, NAMSIZ, &mod->t_pnum);
	mod->t_parlength = 0;
	sym = getsym();
	mod->t_son = block();

	if (level > 0)		/* local modules later */
		link(mod);

	markfather(mod->t_son, mod);
	mod->t_left = mod->t_right = NULL;
	if (sym != End && sym != old_End)
		error();
	sym = getsym();
	mod->t_left = mod->t_right = NULL;
	return mod;
}

head(line, ident, size, nr)
	word *line;
	char ident[];
	int size;
	word *nr;
{
	*line = getnum();		/* LineNum */
	getident(ident, size);	/* Name */
	*nr = getnum();		/* ObjectNum */
}

tree *block()
{	tree *bl;
	tree *brother;

	++ level;
	bl = NULL;
	while ( sym == Mod || sym == Proc || sym == Var ) {
		switch ( sym ) {
		case Mod :	brother = module(); break;
		case Proc :	brother = procedure(); break;
		case Var :	brother = variable(); break;
		}
		if (brother) {
			brother->t_left = bl;
			if (bl)
				bl->t_right = brother;
			brother->t_right = NULL;
			bl = brother;
			}
		}
	-- level;
	return bl;
}

tree *procedure()
{	tree *proc;

	++plevel;
	if ( sym != Proc )
		error();
	new(&proc, sizeof(tree));
	proc->t_type = T_PROC;
	proc->t_plevel = plevel;
	proc->t_bp[0] = proc->t_bp[1] = proc->t_bp[2] = NULL;
	head(&proc->t_line, proc->t_name, NAMSIZ, &proc->t_pnum);
	if (version != 1 && version != 2)
		proc->t_parlength = getnum();
	else
		proc->t_parlength = 0;
	sym = getsym();
	proc->t_son = block();

	link(proc);

	markfather(proc->t_son, proc);
	if (sym != End && sym != old_End)
		error();
	proc->t_right = NULL;
	proc->t_left = NULL;
	sym = getsym();
	--plevel;
	return proc;
}

tree *variable()
{	tree *var;

	if ( sym != Var )
		error();
	new(&var, sizeof(tree));

	link(var);

	var->t_type = T_VAR;
	var->t_plevel = plevel;
	var->t_line = getnum();		/* LineNumber */
	getident(var->t_name, NAMSIZ);	/* Name */
	var->t_vartype = readref();	/* VarType */
	var->t_addrmode = readref();	/* AddrMode */
	if (version == 1 || version == 2)
		switch(var->t_addrmode)
		{	case old_Abs:	var->t_addrmode = Abs; break;
			case old_Ind:	var->t_addrmode = Ind; break;
			case old_Rel:	var->t_addrmode = Rel; break;
		}
	var->t_address = getnum();	/* Address */
	var->t_size = getnum();		/* Size */
	sym = getsym();
	var->t_son = var->t_left = var->t_right = NULL;
	return var;
}

/*
 *	allocate space and check
 */

static void new(ptr, size)
	char **ptr;
	int size;
{
	if ( (*ptr = calloc(1, size)) == NULL ) {
		message("no space available");
		error();
		}
}

error()
{
	longjmp(fail);
}

markfather(son, father)
	tree *son, *father;
{
	if ( !son ) return;
	while ( son->t_left )
		son = son->t_left;
	son->t_father = father;
	while ( son->t_right ) {
		son = son->t_right;
		son->t_father = father;
		}
}
