/*
 *	mdb - build a D-window
 */

#include	<stdio.h>
#include	<signal.h>
#include	<setjmp.h>
#include	"mdb_ref.h"
#include	"mdb_bp.h"
#include	"mdb_tree.h"
#include	"mdb_view.h"
#include	"mdb_procstr.h"
#include	"mdb.h"

extern int coremode;	/* see mdb_core.c */
extern int pid;		/* see mdb_process.c */
extern int version;	/* see mdb_main.c */

extern char * calloc();
extern tree * getproc();
extern tree * getmod();

static char * show_mod ();
static char * show_proc ();
static char * brkpoint ();
static char * show_var ();
static char * dump_var ();

jmp_buf fpe_caught;

catch_fpe()
{
	longjmp(fpe_caught, 1);
}

#if ED7 && PE3200
/*
 *	extern C-compatible assembler routine:
 *	(converts double to float)
 *
 *	usage: singleprec(real1, real2)
 *	       		long real1, real2;
 */

extern float singleprec();
#endif

view ** data_view(father, mod)
	tree *father;
	struct module * mod;
{	tree *ptr;
	int view_len;
	view **d_view;
	view **d_ptr;

	if (! father || !father->t_son) {
		message("no descendent");
		return NULL;
	}
	ptr = father->t_son;
	while ( ptr->t_right )
		ptr = ptr->t_right;
	view_len = 1;
	for ( ; ptr->t_left ; ++view_len, ptr = ptr->t_left )
		;
	if ((d_view = (view **) calloc(view_len+1, sizeof(view *))) == NULL) {
		my_perror("calloc");
		return NULL;
	}
	d_ptr = d_view;
	do {
		if ( (*d_ptr = (view *) calloc(1, sizeof(view))) == NULL ) {
			my_perror("calloc");
			return NULL;
		}
		if (d_ptr == d_view)
			(*d_ptr)->v_flags = V_START;
		else
			(*d_ptr)->v_flags = 0;
		switch ( ptr->t_type ) {
		case T_MOD :
			strcpy( (*d_ptr)->v_line, show_mod(ptr));
			break;
		case T_PROC :
			strcpy( (*d_ptr)->v_line, show_proc(ptr));
			break;
		case T_VAR :
			strcpy( (*d_ptr)->v_line, show_var(ptr));
			break;
		}
		(*d_ptr)->v_node = ptr;
		(*d_ptr)->v_mod = mod;
		(*d_ptr)->v_lineno = ptr->t_line;
		(*d_ptr)->v_addr = 0;
		(*d_ptr)->v_size = 0;
		ptr = ptr->t_right;
		++d_ptr;
	}
	while ( ptr );
	(*(d_ptr-1))->v_flags |= V_END;
	*d_ptr = NULL;
	return d_view;
}

static char * show_mod (mod)
	tree *mod;
{	static char buf[LINSIZ];

	sprintf(buf, "     MOD  %-14s", mod->t_name);
	return buf;
}

static char * show_proc (proc)
	tree *proc;
{	static char buf[LINSIZ];

	sprintf(buf, "   %c PROC %-14s %s",
		proc->t_active ? '*' : ' ',
		proc->t_name,
		brkpoint(proc));
	return buf;
}

static char * brkpoint (proc)
	tree *proc;
{	static char buf[LINSIZ];
	int count[2];

	if (! proc->t_bp[1] || !proc->t_bp[2])
		return "";
	count[0] = proc->t_bp[1]->abp_count;
	count[1] = proc->t_bp[2]->abp_count;
	if (count[0] || count[1])
	{	sprintf(buf, "[%2d,%2d]", count[0], count[1]);
		return buf;
	}
	else
		return "";
}

var_active(tp)
	tree * tp;
{	
	return tp->t_father &&
	       (coremode || pid) &&
	       (tp->t_father->t_type == T_MOD &&
	        (tp->t_father->t_plevel == 0 ||
		 var_active(tp->t_father)) ||
		tp->t_father->t_active);
}

static char * show_var (var)
	tree *var;
{	static char buf[LINSIZ];
	int active;

	if (var_active(var))
		active = 1;
	else
		active = 0;
	sprintf(buf, "   %c VAR  %-14s %s",
		active ? '*' : ' ',
		var->t_name, active ? dump_var(var) : "");
	return buf;
}

static char * dump_var (var)
	tree *var;
{	static char buf[LINSIZ];
	int (* sig_fpe)();
	int addr;
	long long_value;
	char char_value;
#if PE3200 && ED7
	long real2;
	float real;
#else
	double real;
#endif
	char *s_value;
	tree *t;
	int link;		/* -> addr in case of addrmode == Ind */
	struct process pr;

	addr = var->t_address;
	if ( var->t_addrmode != Abs ) {
		if ( var->t_father->t_plevel == 0 )
			addr += globaladdr(var);
		else
			addr = localaddr(var) - addr;
		if ( var->t_addrmode == Ind ) {
			link = addr;
			if ( getlong(addr, &addr) )
				return "address not found";
		}
	}
	if ( getlong(addr, &long_value) )
		return "address not found";
	char_value = (char) ((unsigned)long_value >> 24);
	switch ( var->t_vartype ) {
	case Int:
		sprintf(buf, "%-8s %d", "INTEGER", long_value);
		break;
	case Card:
		sprintf(buf, "%-8s %u", "CARDINAL", long_value);
		break;
	case Char:
		if ( ' ' <= char_value && char_value <= '~' )
			sprintf(buf, "%-8s \"%c\"", "CHAR", char_value);
		else
			sprintf(buf, "%-8s %oC", "CHAR", char_value);
		break;
	case Bool:
		if ( char_value == 1 )
			s_value = "TRUE";
		else if ( char_value == 0 )
			s_value = "FALSE";
		else
			s_value = "undefined";
		sprintf(buf, "%-8s %s", "BOOLEAN", s_value);
		break;
	case Word:
		sprintf(buf, "%-8s %oB", "WORD", long_value);
		break;
	case Real:
#if PE3200 && ED7
		if ( getlong(addr+4, &real2) )
			return "address not found";
#else
		if (addr % sizeof(long) || get(addr, & real, sizeof(double)))
			return "address not found";
#endif
		sig_fpe = signal(SIGFPE, catch_fpe);
		if (setjmp(fpe_caught))
			sprintf(buf, "%-8s undefined", "REAL");
		else
		{
#if PE3200 && ED7
			real = (float) singleprec(long_value, real2);
#endif
			sprintf(buf, "%-8s %e", "REAL", real);
		}
		signal(SIGFPE, sig_fpe);
		break;
	case Arr:
		sprintf(buf, "%-8s at %oB", "ARRAY", addr);
		break;
	case ArrDyn:
		if (! getlong(link-sizeof(long), &long_value))
			var->t_size = long_value+1; /* prep of C-window */
		sprintf(buf, "%-8s at %oB (HIGH = %d)", "DYNAMIC ARRAY", addr,
		   long_value);
		break;
	case Rec:
		sprintf(buf, "%-8s at %oB", "RECORD", addr);
		break;
	case Point:
		if (long_value == NIL)	/* -> NIL ??? */
			sprintf(buf, "%-8s -> NIL", "POINTER");
		else
			sprintf(buf, "%-8s -> %oB", "POINTER", long_value);
		break;
	case Set:
		sprintf(buf, "%-8s %xH", "SET", long_value);
		break;
	case Scal:
		sprintf(buf, "%-8s %oB", "SCALAR", long_value);
		break;
	case Procvar:
		t = getproc(getmod(long_value), long_value);
		if (t && t->t_name)
			sprintf(buf, "%-8s -> %-14s", "PROCVAR", t->t_name);
		else
			sprintf(buf, "%-8s -> undefined", "PROCVAR");
		break;
	case Hide:
		sprintf(buf, "%-8s %oB", "HIDE", long_value);
		break;
	case Process:
		if (long_value == 0 || long_value % sizeof(int))
			sprintf(buf, "%-8s -> undefined", "PROCESS");
		else
		{	get(long_value, & pr, sizeof(struct process));
			t = getproc(getmod(pr.p_pc), pr.p_pc);
			if (t && t->t_name)
				sprintf(buf, "%-8s -> %-14s",
					"PROCESS",
					t->t_name);
			else
				sprintf(buf, "%-8s -> undefined", "PROCESS");
		}
		break;
	default:
		sprintf(buf, "unknown type");
	}
	return buf;
}
