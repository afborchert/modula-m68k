/*
 *	mdb - T-window
 */

#include	<stdio.h>
#include	"mdb_ref.h"
#include	"mdb_tree.h"
#include	"mdb_bp.h"
#include	"mdb_view.h"

#if SYSV || BSD42
#define	index(s,c)	strchr(s,c)
#define rindex(s,c)	strrchr(s,c)
extern char * strchr();
extern char * strrchr();
#else
extern char * index();
extern char * rindex();
#endif

#define	SCREEN	30	/* number of lines showed */
#define	ABS(expr)	((expr) > 0 ? (expr) : (-expr))

extern char * searchfile();
extern char * strsave();
extern char * calloc();

static tree * ltonode(line, mod)
	int line;
	struct module * mod;
{	tree * tp;
	tree * old;

	old = NULL;
	tp = mod->m_ptr;
	while (tp->t_link)
	{	if (tp->t_line == line)
			return tp;
		if (! old)
			old = tp;
		else
			if ( ABS(old->t_line-line) > ABS(tp->t_line-line) )
				old = tp;
		tp = tp->t_link;
	}
	if (! old) return NULL;
	return old;
}

view ** textwin(mod, t, line)
	struct module * mod;
	tree * t;
	int line;
{	char buf[BUFSIZ];
	char * file;
	FILE * fp = NULL;
	view ** c_win;
	view ** ptr;
	int skip;
	int act_line;
	char * cp;
	char * modname;

	if (! t) return NULL;
	if (line <= 0)
		line = t->t_line;
	if (! mod) return NULL;
	modname = mod->m_name;
	getfilename(buf, BUFSIZ);
	if (strlen(buf))
		file = buf;
	else if (mod->m_file && (fp = fopen(mod->m_file, "r")))
		file = mod->m_file;
	else if (mod->m_file && (cp = rindex(mod->m_file, '.')) &&
		((* cp = '\0'), file = searchfile(mod->m_file, cp+1),
		 (* cp = '.'), file))
		;
	else if ((file = searchfile(modname, "m2")) == NULL &&
		 (file = searchfile(modname, "mr")) == NULL)
		message("%s: no source file found", modname);
	if ( !file || !*file )
		return NULL;
	if (!fp && (fp = fopen(file, "r")) == NULL)
	{	my_perror(file);
		if (mod->m_file)
		{	cfree(mod->m_file);
			mod->m_file = NULL;
		}
		return NULL;
	}
	if (strcmp(mod->m_file, file))
		mod->m_file = strsave(file);
	skip = line-SCREEN/2 >= 0 ? line-SCREEN/2 : 0;
	for ( act_line = 0 ; act_line < skip ; ++act_line )
		if (! fgets(buf, BUFSIZ, fp))
		{	fclose(fp);
			message("%s has only %d lines", file, act_line);
			return NULL;
		}
	if ( (c_win = (view **) calloc(SCREEN+1, sizeof(view *))) == NULL )
	{	my_perror("calloc");
		fclose(fp);
		return NULL;
	}
	for ( ptr = c_win ; ptr < c_win+SCREEN ; ++ptr )
	{	if ( (*ptr = (view *) calloc(1, sizeof(view))) == NULL )
		{	my_perror("calloc");
			fclose(fp);
			return NULL;
		}
		if (! fgets(buf, BUFSIZ, fp))
		{	*ptr = NULL;
			(*(ptr-1))->v_flags |= V_END;
			fclose(fp);
			return c_win;
		}
		++act_line;
		if (cp = index(buf, '\n'))
			*cp = '\0';
		buf[LINSIZ-7] = '\0';
		if (act_line == 1)
			(*ptr)->v_flags = V_START;
		else
			(*ptr)->v_flags = 0;
		if (act_line == line)
			(*ptr)->v_flags |= V_MOVE;
		sprintf((*ptr)->v_line, "  %3d\t%s", act_line, buf);
		(*ptr)->v_mod = mod;
		(*ptr)->v_addr = 0;
		(*ptr)->v_size = 0;
		(*ptr)->v_lineno = act_line;
		if (act_line == t->t_line)
			(*ptr)->v_node = t;
		else
			(*ptr)->v_node = ltonode(act_line, mod);
		if ((*ptr)->v_node && (*ptr)->v_node->t_father)
			(*ptr)->v_node = (*ptr)->v_node->t_father;
	}
	*ptr = NULL;
	fclose(fp);
	return c_win;
}
