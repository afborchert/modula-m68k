/*
 *	mdb - backtrace of the actual stack
 */

#include	<stdio.h>
#include	<signal.h>
#include	"mdb.h"
#if BSD42
#include	<machine/reg.h>
#else
#include	<sys/reg.h>
#endif
#include	"mdb_ref.h"
#include	"mdb_tree.h"
#include	"mdb_bp.h"
#include	"mdb_view.h"
#include	"mdb_procstr.h"
#if SYSV || BSD42
#include	"mdb_instr.h"
#endif

extern int version;
#if SYSV || BSD42
extern struct instr instr;	/* see mdb_opset.c */
#endif

extern char * calloc();
extern char * realloc();
extern struct module * getmod();
extern tree * getproc();
#if SYSV || BSD42
extern int disasm();
#endif SYSV || BSD42

int pc = 0;			/* mdb_error.c -> mdb_stack.c */
int e_base = 0;
int e_top = 0;
int ppc = 0;			/* mdb_stack.c -> mdb_lcmd.c */
static view **pwin = NULL;

static char * sigtext();
static char * e_realloc();

/*
 *	activation record:
 *
 *	+--------------------+            \
 *	|		     |            |
 *	|    parameters      |            |
 *	|		     |            |
 *	+--------------------+            |
 *	|    static link     |            >  parlength
 *	+--------------------+            |
 *	|      old pc        |            |
 *	+--------------------+            |
 *	|    dynamic link    |            |
 *	+--------------------+ <--- base /
 *	|		     |
 *	|    local vars      |
 *	|		     |
 *	+--------------------+
 *	|    temporaries     |
 *	+--------------------+ <--- top
 *
 */

static init(p)
	struct process * p;
{
	long base, top, limit, pclink;
#if SYSV || BSD42
	long ra;
#endif SYSV
	struct module *mod;
	tree *proc, *pproc;
	int view_size = 30;
	int unknown = 0;
	int stack_depth = 0;
	view **ptr;
	int lineno;
	char lineno_str[8];

	pwin = NULL;
	if (p)
	{	base = p->p_base;
		top = p->p_top;
		pclink = p->p_pc;
		limit = p->p_limit;
		if (! p->p_started)
		{	message("PROCESS not started");
			return;
		}
	}
	else
	{
#if BSD42
		base = getreg(AR6);
#else
		base = getreg(FP);
#endif
		top = getreg(SP);
/***		limit = getreg(R2) & 0x3fffffff;	/* see cm2rt0.s */
		error_mess();		/* set pc, e_base and e_top */
		if (pc)
		{	pclink = pc;
			pc = 0;
		}
		else
			pclink = getreg(PC);
		if (e_base)
		{	base = e_base;
			e_base = 0;
		}
		if (e_top)
		{	top = e_top;
			e_top = 0;
		}
	}
	ppc = pclink;

	if ( (pwin = (view **) calloc(view_size, sizeof(view *))) == NULL ) {
		my_perror("calloc");
		return;
	}

	/*
	 *	initialisize P-window
	 */

	ptr = pwin;
	if ( (*ptr = (view *) calloc(1, sizeof(view))) == NULL ||
	     (*++ptr = (view *) calloc(1, sizeof(view))) == NULL ) {
		my_perror("calloc");
		return;
	}
	/*                       123456789012345678901234 12345678901234 12345678 12345678 12345678 */
	strcpy((*pwin)->v_line, "module                   procedure          base      top       pc  line");
	(*pwin)->v_node = NULL; (*pwin)->v_mod = NULL;
	(*pwin)->v_flags = V_START;
	strcpy((*ptr)->v_line, " ");
	(*ptr)->v_node = NULL; (*ptr)->v_mod = NULL;
	(*ptr)->v_flags = 0;
	++ptr;
	do {
cont:
		if (ptr - pwin == view_size)
		{	pwin = (view **) e_realloc(pwin, sizeof(view *), view_size += 20);
			ptr = pwin + view_size - 20;
		}
		if ( (*ptr = (view *) calloc(sizeof(view), 1)) == NULL ) {
			my_perror("calloc");
			return;
		}
		(*ptr)->v_flags = 0;

		/*
		 *	interrupt ?
		 */
#if SYSV || BSD42
		if (base % 4 != 0)
#else
		if (base == 1)
#endif
		{	long value;

			(*ptr)->v_node = NULL;
			(*ptr)->v_mod = NULL;
			(*ptr)->v_lineno = 0;
#if SYSV || BSD42
			
			/*		+---------+
			 *		| pc      |
			 *  base+16 ->	+---------+
			 *		| 2 words |
			 *  base+8 ->	+---------+
			 *		| signo   |
                         *  base+4 ->   +---------+
			 *              | link    |
			 *  base ->     +---------+
			 *		| regs    | (64 bytes: d0..a7)
			 *		+---------+
			 */
			--base;
			getlong(base+4, & value); /* get signo */
			sprintf((*ptr)->v_line,
			   "<< interrupted with %s >>", sigtext(value));
#ifdef NIXDORF
			getlong(base+16, & pclink);	/* get pc */
			getlong(base-3*4, & limit);
			getlong(base-4, & top);
#endif
#ifdef SUN
#define SCP_OFF(field) ((long) & (((struct sigcontext *)0)->field))
			{	long scp_addr;

				getlong(base+12, & scp_addr);
				getlong(scp_addr + SCP_OFF(sc_sp), & top);
				getlong(scp_addr + SCP_OFF(sc_pc), & pclink);
			}
#endif
			getlong(base, & base);
#else /* ! SYSV */
			while (getlong(limit, & value) == 0 && value != -1)
				limit += 4;
			if (value != -1)
			{	sprintf((*ptr)->v_line,
				"unknown interrupt / stack demolished");
				break;
			}
			limit += 4;
			getlong(limit, & value); /* signal number */
			sprintf((*ptr)->v_line, "<< interrupted with %s >>", sigtext(value));
			limit += 4;  /* skip signal number */
			limit += 64; /* skip floating point registers */
			getlong(limit + 4*2, & top);
			getlong(limit + 4*14, & base);
			limit += 64; /* skip general registers */
			getlong(limit + 4, & pclink); /* program counter */
			getlong(limit + 4*2, & limit);
#endif /* ! SYSV */
			++ptr;
			goto cont; /* continue while-loop */
		}

		mod = getmod(pclink);
		(*ptr)->v_mod = mod;
		if (mod) {
			proc = getproc(mod, pclink);
			(*ptr)->v_node = proc;
		}
		else
		{	proc = NULL;
			(*ptr)->v_node = NULL;
		}

		if (proc && ! proc->t_active)
		{	proc->t_active = 1;
			/* calculate correct base */
			if (proc->t_parlength < 8)
				proc->t_parlength = 8;
			proc->t_base = base + proc->t_parlength;
		}
		if (proc && proc->t_type == T_PROC)
		{	(*ptr)->v_base = base + proc->t_parlength;
			(*ptr)->v_size = proc->t_parlength >> 2;
		}
		else
		{	(*ptr)->v_base = 0;
			(*ptr)->v_size = 0;
		}
		(*ptr)->v_addr = pclink;
		lineno = get_ln(pclink);
		if (proc && proc->t_line > lineno )
			if (proc->t_line > 65200 )	/*sometimes an error occurs in lineno */
			    lineno = proc->t_line - 65280;
			else
			    lineno = proc->t_line;
		if (lineno >= 1)
		{	(*ptr)->v_lineno = lineno;
			sprintf(lineno_str, "%4d", lineno);
		}
		else
		{	(*ptr)->v_lineno = 0;
			strcpy(lineno_str, "    ");
		}
		if (mod && proc)
		{	sprintf((*ptr)->v_line, "%-24s %-14s %8x %8x %8x  %s",
				mod->m_name, proc->t_name,
				base, top, pclink, lineno_str);
			if (lineno <= 0)
				(*ptr)->v_lineno = proc->t_line;
		}
		else if (mod)
		{
			sprintf((*ptr)->v_line, "%-24s %-14s %8x %8x %8x  %s",
				mod->m_name, "unknown",
				base, top, pclink, lineno_str);
}
		else
			sprintf((*ptr)->v_line, "%-24s %-14s %8x %8x %8x  %s",
				"unknown", "unknown",
				base, top, pclink, lineno_str);
		top = base + 8;
		++ptr;
	}
	while ( base && getlong(top-8, &base) == 0 &&
		getlong(top-4, &pclink) == 0 &&
		base &&
		base >= top && stack_depth++ < 100);
	if (ptr - pwin >= view_size)
	{	view_size = ptr-pwin+1;
		pwin = (view **) e_realloc(pwin, sizeof(view *), view_size);
		ptr = pwin+view_size-1;
	}
	(*(ptr-1))->v_flags = V_END;
	*ptr = NULL;
}

view **backtrace()
{
	if (! pwin)
		init(NULL);
	return pwin;
}

extern struct module mod[];
extern int a_mod;

new_stack(p)
	struct process * p;
{	int i;
	tree * tp;
	view ** old_pwin;

	old_pwin = pwin;
	for ( i = 0 ; i < a_mod ; ++i )
		for ( tp = mod[i].m_ptr ; tp ; tp = tp->t_link )
			tp->t_active = 0;
	init(p);
	if (! pwin)
	{	pwin = old_pwin;
		return;
	}
	if (old_pwin)
		kill_view(old_pwin);
}

address localaddr(var)
	tree *var;
{
	if (var->t_father)
		var = var->t_father;
	while (var->t_father && var->t_type == T_MOD)
		var = var->t_father;
	return var->t_base;
}

static char * e_realloc(ptr, size, cnt)
	char * ptr;
	unsigned size;
	unsigned cnt;
{
	if ((ptr = realloc(ptr, size*cnt)) == NULL)
	{	my_perror("realloc");
		pwin = NULL;
		cmderror(NULL);
	}
	return ptr;
}

static char * sigtext(signo)
	int signo;
{
	switch (signo)
	{	case SIGHUP:	return "SIGHUP";
		case SIGINT:	return "SIGINT";
		case SIGQUIT:	return "SIGQUIT";
		case SIGILL:	return "SIGILL";
		case SIGTRAP:	return "SIGTRAP";
		case SIGIOT:	return "SIGIOT";
		case SIGEMT:	return "SIGEMT";
		case SIGFPE:	return "SIGFPE";
		case SIGKILL:	return "SIGKILL";
		case SIGBUS:	return "SIGBUS";
		case SIGSEGV:	return "SIGSEGV";
		case SIGSYS:	return "SIGSYS";
		case SIGPIPE:	return "SIGPIPE";
		case SIGALRM:	return "SIGALRM";
		case SIGTERM:	return "SIGTERM";
		default:	return "undefined";
	}
}
