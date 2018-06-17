/*
 *	mdb - C-window
 */

#include	<stdio.h>
#include	"mdb.h"
#include	"mdb_ref.h"
#include	"mdb_tree.h"
#include	"mdb_bp.h"
#include	"mdb_view.h"

extern int version;
extern int noscreen;

#define	SCREEN 23		/* start cwin at addr-SCREEN*4 */

extern char * calloc();

char * ascii();

view ** corewin(var, mod, defaddr, defsize, addrformat, format)
	tree * var;	/* variable to be dumped */
	struct module * mod; /* the global module of the variable */
	address defaddr;
	unsigned defsize;
	char addrformat, format; /*  == 'd', 'o', 'x' or 'c' */
{	view ** cwin;	/* to be returned */
	view ** ptr;	/* -> the actual view-line */
	int len;	/* count of fullwords to be shown */
	long value;
	address addr;
	address maddr;
	char * addtext;
	static int old_len;
	static address old_addr;
	int noscreen = 0;

	if (! noscreen && ! getcorewin(& addr, & len))
		if (defaddr && defsize)
		{	addr = defaddr;
			len = defsize;
		}
		else
		{	addr = getvaraddr(var);
			if (! addr)
				return NULL;
			/*
			 *	in case of a pointer `t_addr' is the
			 *	address of the pointer itself
			 */
		
			if (var->t_vartype == Point || var->t_vartype == Process) {
				if ( getlong(addr, &addr) ) {
					message("illegal address");
					return NULL;
				}
				if (addr == NIL) {
					message("NIL reference");
					return NULL;
				}
			}
			if (version == 1)
				len = var->t_size;
			else if (var->t_vartype == Process)
				len = 5;
			else if (! len)
				/* align len */
			{	len = var->t_size / 4;
				if (var->t_size % 4)
					++len;
			}
		}
	maddr = addr;
	if (! getlong(addr - SCREEN * sizeof(val), & value))
	{	len += SCREEN;
		addr -= SCREEN * sizeof(val);
	}
	if (len > 100)
		len = 100;
	old_len = len;
	old_addr = addr;

	/*
	 *	header lines
	 */

	if ( (cwin = (view **) calloc(len+3, sizeof(view *))) == NULL) {
		my_perror("calloc");
		return NULL;
	}
	ptr = cwin;
	--ptr;

	/*
	 *	dump len fullwords beginning from addr
	 */

	while ( len-- ) {
		if ( (*++ptr = (view *) calloc(1, sizeof(view))) == NULL) {
			my_perror("calloc");
			*ptr = NULL;
			return cwin;
		}
		if (addr == maddr)
			(*ptr)->v_flags |= V_MOVE;
		else
			(*ptr)->v_flags = 0;
		if (getlong(addr, &value)) {
			message("illegal address");
			*ptr = NULL;
			return cwin;
		}
		if (var && var->t_vartype == Process)
		{	char * field;

			switch(len)
			{	case 4: field = "base    %11oB"; break;
				case 3: field = "top     %11oB"; break;
				case 2: field = "limit   %11oB"; break;
				case 1: field = "pc      %11oB"; break;
				case 0:
					if (value)
						field = "started  TRUE";
					else
						field = "started  FALSE";
					break;
			}
			sprintf((*ptr)->v_line, field, value);
		}
		else
		{	char fmt[32];

			switch(addrformat)
			{	case 'o': strcpy(fmt, "[%8oB]"); break;
				case 'x': case 'H':
				case 'h': strcpy(fmt, "[%8xH]"); break;
				case 'd': strcpy(fmt, "[%8d]"); break;
				default : strcpy(fmt, "[%8oB]"); break;
			}
			switch (format)
			{	case 'o': strcat(fmt, " %11oB"); break;
				case 'x': case 'H':
				case 'h': strcat(fmt, " %8xH"); break;
				case 'd': strcat(fmt, " %10d"); break;
				case 'c': strcat(fmt, " %s"); break;
				case 'a': strcat(fmt, " %8xH %10d %11oB %s");
					  break;
				default : strcat(fmt, " %10d"); break;
			}
			if (format == 'a')
				sprintf((*ptr)->v_line, fmt, addr,
					value, value, value, ascii(value));
			else if (format == 'c')
				sprintf((*ptr)->v_line, fmt, addr,
					ascii(value));
			else
				sprintf((*ptr)->v_line, fmt, addr, value);
		}
		if (var && var->t_father)
			(*ptr)->v_node = var->t_father;
		else
			(*ptr)->v_node = var;
		(*ptr)->v_mod = mod;
		(*ptr)->v_addr = addr;
		(*ptr)->v_size = old_len;
		addr += sizeof(long);
	}
	*++ptr = NULL;
	return cwin;
}

/*
 *	show visible character
 */

char * ascii(fullword)
	union {
		long fw;
		char bytes[sizeof(long)];
		} fullword;
{	static char buf[sizeof(long)+3];
	int i;

	buf[0] = '"';
	for ( i = 0 ; i < sizeof(long) ; ++i )
		if (' ' <= fullword.bytes[i] && fullword.bytes[i] <= '~')
			buf[i+1] = fullword.bytes[i];
		else
			buf[i+1] = ' ';
	buf[++i] = '"';
	buf[++i] = '\0';
	return buf;
}

/*
 *	calculate address
 */

getvaraddr(var)
	tree * var;
{	address addr;

	if (var->t_type != T_VAR) {
		message("not a variable");
		return NULL;
	}
	/*
	 *	active ???
	 */

	if (! var_active(var))
	{	message("not active");
		return NULL;
	}
	
	/*
	 *	calculate address
	 */

	addr = var->t_address;
	if ( var->t_addrmode != Abs ) {
		if ( var->t_father->t_plevel == 0 )
			addr += globaladdr(var);
		else
			addr = localaddr(var) - addr;
		if ( var->t_addrmode == Ind )
			if ( getlong(addr, &addr) ) {
				message("address not found");
				return NULL;
			}
	}
	return addr;
}
