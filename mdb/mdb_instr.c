/*
 * mdb instruction window
 */

#include <stdio.h>
#include "mdb.h"
#include "mdb_ref.h"
#include "mdb_tree.h"
#include "mdb_view.h"

extern char * calloc();
extern struct module * getmod();
extern tree * getproc();

extern int errno;
extern address idot;
extern unsigned idotinc;
extern char tobeprinted[];

view ** instrwin(space, addr)
	int space;
	address addr;
{	struct module * mod;
	tree * p;
	address pstart;
	view ** iwin;
	view ** idot;
	int iwinsize;
	int count = 0;
	static address lastaddr = 0;	/* for efficiency reasons */

#if 0
	if (addr >= 0xf00000)
		return NULL;
#endif
	/*
	 * first look for a start address of the window
	 */
	if (addr > 4 && (mod = getmod(addr-4)))
	{	if (p = getproc(mod, addr-4))
			pstart = procentry(mod, p->t_pnum);
		else
			pstart = mod->m_entry;
	}
	else
		pstart = 0;
	/*
	 * avoid time-consuming disassembling of large functions
	 */
	if (lastaddr > pstart && lastaddr + 0x100 < addr)
		pstart = lastaddr;

	if (! (iwin = (view **) calloc(iwinsize = 100, sizeof(view *))))
	{	my_perror("calloc");
		return NULL;
	}
	idot = iwin;

	while (pstart <= addr || count++ <= 30)
	{	if (idot - iwin + 1 == iwinsize)
		{	iwin = (view **) realloc(iwin,
					     sizeof(view *) * (iwinsize += 20));
			if (! iwin)
			{	my_perror("realloc");
				return NULL;
			}
			idot = iwin + iwinsize - 20 - 1;
		}
		if (! ((*idot) = (view *) calloc(1, sizeof(view))) || 
		    ! one_instr(space, pstart, * idot))
		{	if (idot == iwin)
			{	cfree(iwin);
				if (* idot == NULL)
					my_perror("calloc");
				else
					message("access failed");
				return NULL;
			}
			else
			{	if (* idot)
					cfree(* idot);
				-- idot;
				(*idot)->v_flags |= V_END;
				++ idot;
				break;
			}
		}
		if (! idotinc)
			idotinc = 2;
		if (pstart >= addr && pstart+idotinc > addr)
		{	(*idot)->v_flags |= V_MOVE;
			lastaddr = pstart;
		}
		if (pstart == 0)
			(*idot)->v_flags |= V_START;
		pstart += idotinc;
		++ idot;
	}
	*idot = NULL;
	return iwin;
}

static char text[256];

static void addtext(fmt, p1, p2, p3)
	char * fmt, * p1, * p2, * p3;
{	char buf[127];
	
	sprintf(buf, fmt, p1, p2, p3);
	strncat(text, buf, sizeof(text));
}

/*
 * return 0 on failure
 */
static int one_instr(space, addr, line)
	int space;
	address addr;
	view * line;
{	unsigned int ins;
	struct module * mod;
	tree * p;
	address pstart;
	int ln;
	int result;	/* 0: ok */

	text[0] = '\0';
	idot = addr;
	errno = 0;
	if (space == DATA)
		result = access(READ, idot, space, & ins);
	else
		result = readaout(idot, & ins);
	if (! result)
	{	if (mod = getmod(idot))
		{	addtext("[%s", mod->m_name);
			if (p = getproc(mod, idot))
				addtext(".%s+#%x", p->t_name,
					idot - procentry(mod, p->t_pnum));
			addtext("=#%x", idot);
			if (ln = get_ln(idot))
				addtext(":%d", ln);
			addtext("]");
		}
		else
			addtext("[#%x]", idot);
		addtext("\t");
#if PE
		printins(space, ins);
		idot += idotinc;
#endif
#if M68
		idotinc = print_insn(space, idot);
		idot += idotinc;
#endif
		strcat(text, tobeprinted);
		strncpy(line->v_line, text, LINSIZ);
		line->v_mod = mod;
		line->v_node = p;
		line->v_base = p->t_base;
		line->v_addr = addr;
		line->v_size = idotinc;
		line->v_lineno = ln;
		return 1;	/* ok */
	}
	else	/* error message already printed (i hope) */
		return 0;
}
