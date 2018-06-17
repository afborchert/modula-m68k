/*
 *	mdb - breakpoint handling
 */

#include	<stdio.h>
#include	"mdb_ref.h"
#include	"mdb_tree.h"
#include	"mdb_bp.h"

extern tree * build();
extern tree * getproc();
extern char * calloc();

extern int pid;
extern struct bp * bplist;	    /* list of non-active breakpoints */
       struct abp * abplist = NULL; /* list of possibly active breakpoints */

static new();

set_brkpt(t)
	tree * t;
{	struct abp * brkpt;
	int count[2];
	int cnt;
	int i;

	if (! t || !t->t_bp[1] || !t->t_bp[2])
	{	message("no breakpoint label found");
		return;
	}
	readcount(count);
	if (count[0] == 0 && count[1] == 0)
		del_brkpt(t);
	else
		for (i = 0; i < 3; ++i)
		{	brkpt = t->t_bp[i];
			switch (i)
			{	case 0:
					if (count[0] == -1 || count[1] == -1)
						cnt = -1;
					else if (count[0] > count[1])
						cnt = count[0];
					else
						cnt = count[1];
					break;
				case 1:
				case 2:
					cnt = count[i-1];
					break;
			}
			if (cnt == 0)
			{	del_single(brkpt);
				continue;
			}
			brkpt->abp_initcnt = brkpt->abp_count = cnt;
			if (cnt == -1)
				brkpt->abp_flag |= ABP_ALWAYS;
			else
				brkpt->abp_flag &= ~ABP_ALWAYS;
			if (! (brkpt->abp_flag & ABP_ACTIVE) )
			{	brkpt->abp_flag |= ABP_TOSET;
				brkpt->abp_flag &= ~(ABP_PASSED|ABP_EXEC);
			}
		}
}

del_brkpt(t)
	tree * t;
{	struct abp * brkpt;
	int i;

	if (! t || !t->t_bp[1] || !t->t_bp[2])
	{	message("cannot delete breakpoint");
		return;
	}
	for (i = 0; i < 3; ++ i)
		del_single(t->t_bp[i]);
}

static del_single(brkpt)
	struct abp * brkpt;
{
	brkpt->abp_initcnt = brkpt->abp_count = 0;
	brkpt->abp_flag &= ~ABP_TOSET;
	if (brkpt->abp_flag & ABP_ACTIVE)
	{	brkpt->abp_flag &= ~ABP_ACTIVE;
		if (pid && ! (brkpt->abp_flag & ABP_PASSED))
			/* 1 stands for text segment */
			putlong(brkpt->abp_loc, brkpt->abp_inst, 1);
	}
}

/*
 *	enter the breakpoints of the specified module
 *	into the tree structure
 *
 *	return -1 on failure
 *		0 if ok
 */

init_brkpt(mod)
	struct module * mod;
{	struct bp * bp_p, * pre;
	struct abp * abp_p;
	tree * tp;

	if (! mod)
		return -1;
	if (mod->m_bpset)
		return 0;
	if (! mod->m_ptr)
		if (! (mod->m_ptr = build(mod->m_name)))
			return -1;
	for ( pre = bp_p = bplist ; bp_p ; )
	{	
#if SYSV || BSD42
		if (bp_p->bp_mod != mod ||
#else
		if (getmod(bp_p->bp_addr) != mod ||
#endif
		    (! (tp = getproc(mod, bp_p->bp_addr))))
		{	pre = bp_p; bp_p = bp_p->bp_ptr;
			continue;
		}
		new(&abp_p, sizeof(struct abp));
		abp_p->abp_loc = bp_p->bp_addr;
		abp_p->abp_flag = 0;
		abp_p->abp_link = abplist;
		abplist = abp_p;
		tp->t_bp[bp_p->bp_flag+1] = abp_p;

		if (! tp->t_bp[0])
		{	new(& abp_p, sizeof(struct abp));
			abp_p->abp_loc = procentry(mod, tp->t_pnum);
			abp_p->abp_flag = ABP_SKIP;
			abp_p->abp_link = abplist;
			abplist = abp_p;
			tp->t_bp[0] = abp_p;
		}

		if (pre == bplist)
		{	bplist = bp_p->bp_ptr;
			cfree(bp_p);
			bp_p = bplist;
			pre = bplist;
		}
		else
		{	pre->bp_ptr = bp_p->bp_ptr;
			cfree(bp_p);
			bp_p = pre->bp_ptr;
		}
	}
	mod->m_bpset++;
	return 0;
}

static new(ptr, size)
	char ** ptr;
	int size;
{
	if ( (*ptr = calloc(1, size)) == NULL )
		my_perror("calloc"), cmderror(NULL);
}
