/*
 * GNU licensing stuff...
 *
 * afb 12/90
 */

#include "mdb_ref.h"
#include "mdb.h"
#include "mdb_tree.h"
#include "mdb_view.h"

extern char * calloc();

extern char * copying_info[];
extern int copying_info_lines;

static view ** copying_view = NULL;

static view ** init_view()
{
	view ** new;
	int index;

	new = (view **) calloc(copying_info_lines + 1, sizeof(view *));
	if (! new)
	{	my_perror("calloc");
		return NULL;
	}
	for (index = 0; index < copying_info_lines; ++ index)
	{	new[index] = (view *) calloc(1, sizeof(view));
		strncpy(new[index]->v_line, copying_info[index], LINSIZ);
		new[index]->v_mod = NULL;
		new[index]->v_node = NULL;
		new[index]->v_base = 0;
		new[index]->v_addr = 0;
		new[index]->v_size = 0;
		new[index]->v_lineno = 0;
		new[index]->v_flags = 0;
	}
	new[0]->v_flags |= V_START;
	new[copying_info_lines-1]->v_flags |= V_END;
	return new;
}

view ** get_copying_view()
{
	if (! copying_view)
		copying_view = init_view();
	return copying_view;
}
