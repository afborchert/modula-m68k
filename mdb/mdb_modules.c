/*
 *	mdb - build a M-window
 */

#include	<stdio.h>
#include	"mdb.h"
#include	"mdb_ref.h"
#include	"mdb_bp.h"
#include	"mdb_tree.h"		/* import module structure */
#include	"mdb_view.h"

tree * build();

/*
 *	imported from mdb_aout.c
 */

extern char * calloc();

extern struct module mod[];
extern int a_mod;

static view ** mwin;

static init()
{	struct module * ptr;
	view ** v_ptr;

	if ( (mwin = (view **) calloc(a_mod+1, sizeof(view *))) == NULL ) {
		my_perror("calloc");
		return;
		}
	for ( v_ptr = mwin, ptr = mod ; ptr < mod+a_mod ; ++v_ptr, ++ptr ) {
		if ( (*v_ptr = (view *) calloc(1, sizeof(view))) == NULL ) {
			my_perror("calloc");
			return;
			}
		if (v_ptr == mwin)
			(*v_ptr)->v_flags = V_START;
		else
			(*v_ptr)->v_flags = 0;
		if (! ptr->m_ptr)
			ptr->m_ptr = build(ptr->m_name);
		(*v_ptr)->v_node = ptr->m_ptr;
		strncpy((*v_ptr)->v_line, ptr->m_name, LINSIZ);
		(*v_ptr)->v_mod = ptr;
		if (ptr->m_ptr)
			(*v_ptr)->v_lineno = ptr->m_ptr->t_line;
		else
			(*v_ptr)->v_lineno = 0;
		(*v_ptr)->v_addr = 0;
		(*v_ptr)->v_size = 0;
		}
	(*(v_ptr-1))->v_flags |= V_END;
	*v_ptr = NULL;
}

view ** module_view()
{	static int initflag;

	if (! initflag++)
		init();
	return mwin;
}
