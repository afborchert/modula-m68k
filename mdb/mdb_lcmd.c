/*
 *	mdb - line oriented user interface
 *	afb 4/86
 */

#include "mdb.h"

#include <stdio.h>
#include <ctype.h>
#include <setjmp.h>
#include <signal.h>
#if BSD42
#include <machine/reg.h>
#else
#include <sys/reg.h>
#endif
#include "mdb.h"
#include "mdb_ref.h"
#include "mdb_bp.h"
#include "mdb_tree.h"
#include "mdb_procstr.h"
#include "mdb_view.h"
#include "mdb_map.h"

/*
 *	imported functions
 */

extern view ** backtrace();	/* p-window */
extern view ** data_view();	/* d-window */
extern view ** module_view();	/* m-window */
extern view ** corewin();	/* c-window */
extern view ** textwin();	/* t-window */
extern view ** instrwin();	/* i-window */
extern view ** get_copying_view(); /* H-window */
extern int access();
extern void printins();
extern char * searchfile();	/* mdb_ref.c */
extern char * strsave();
extern char * strrchr();
extern char * getenv();
extern char * re_comp();
extern char * re_exec();
extern struct module * getmod();
extern tree * getproc();
extern unsigned get_ln();
extern address getvaraddr();

/*
 *	imported vars
 */

extern int errno;		/* for my_perror */
extern int sys_nerr;
extern char * sys_errlist[];

extern int coremode;		/* see mdb_core.c */
extern int fflag;		/* fast mode: only procedure backtrace */
extern int pid;			/* see mdb_process.c */
extern char * aoutfile;		/* file name of a.out-file */
extern struct map txtmap;
extern struct map datmap;
extern int reglist[];		/* see mdb_process.c */
extern char * regnames[];
extern address idot;		/* see mdb_opset.c */
extern address idotinc;
extern char tobeprinted[];	/* result of printins */
extern address ppc;		/* see mdb_stack.c & mdb_error.c */

/*
 *	exported vars
 */

int noscreen = 1;		/* in this version always true */

/*
 *	local vars
 */

#define MAXARG 4
#define AT_INT 1
#define AT_STR 2

static view ** act_view = NULL;
static view ** dot = NULL;
static view ** pdot = NULL;
static jmp_buf cmd_loop;
static union {
       int arg_int;
       char * arg_string;
} args[MAXARG];
static char arg_type[MAXARG];
static int arg_cnt = 0;
static int cmd_reset = 0;	/* if on: clear buffer on next getcmd call */
static char mode = 0;		/* 'p' for p-window, ... */
static char paneel[80];		/* current status line */
static void print_line();
static void print_header();
static void print_rte();
static void window();
static char getcmd();
static void int_arg();
static void char_arg();
static void str_arg();
static view ** init_dot();
static void breakpoint();
static void print_maps();
static void print_regs();
static void special_cmd();
static void mdbmode();
static void call_vi();
static void shell_cmd();
static int movedot();
static int multiple_move();
static void convert();
static void search();
static void screen_cmd();
static void print_instr();
static void show_instr();
static void help();
static void p_switch();
static void wd_handling();


void cmderror();
void die();
void message();


init()
{
}

/*
 *	work up user commands
 */

cmdmode()
{	char cmd;

	if (coremode)
	{	print_rte();	/* print run time error message */
		window('p', 1);
		if (! dot)
			message("No Modula-2 stack found.");
	}
	if (! dot)
		window('m', 1);

	/* jump on error to this point */

	setjmp(cmd_loop);
	signal(SIGINT, cmderror);

	for(;;)
	{	cmd = getcmd();
		switch (cmd)
		{	case '+':	print_line(multiple_move(1)); break;
			case 0:		print_line(movedot(1)); break;
			case '!':	shell_cmd(); break;
			case '^':	dot = act_view; print_line(1); break;
			case '-':	print_line(multiple_move(-1)); break;
			case '.':	print_line(1); break;
			case '=':	convert(); break;
			case '/':	search(1); print_line(1); break;
			case '?':	search(-1); print_line(1); break;
			case 'b':	screen_cmd('b'); break;
			case 'c':	window('c', 1); break;
			case 'd':	window('d', 1); break;
			case 'e':	print_rte(); break;
			case 'f':	window('f', 1); break;
			case 'h':	help(); break;
			case 'H':	window('H', 1); break;
			case 'i':	window('i', 1); break;
			case 'm':	window('m', 1); break;
			case 'n':	screen_cmd('n'); break;
			case 'p':	window('p', 1); break;
			case 'q':	die(0);
			case 's':	window('s', 1); break;
			case 'u':	screen_cmd('u'); break;
			case 't':	window('t', 1); break;
			case 'v':	call_vi(); break;
			case 'x':	die(0);
			case 'B':	breakpoint('B'); break;
			case 'C':	cont(1, 0); mdbmode(); break;
			case 'D':	breakpoint('D'); break;
			case 'I':	cont(0, 0); mdbmode(); break;
			case 'M':printf("M\n");	p_switch('M'); break;
			case 'P':	p_switch('P'); break;
			case 'R':	run(0); mdbmode(); break;
			case 'S':	pid? cont(0, 1): run(1); mdbmode();
					break;
			case 'W':	run_wds(); mdbmode(); break;
			case '$':	special_cmd(); break;
			default:	cmderror("Unknown command"); break;
		}
	}
}

void cmderror(mess)
	char * mess;
{
	cmd_reset = 1;		/* forget buffer contents */
	if (mess > (char *) 20)	/* mess may be a signal number ! */
		message(mess);
	longjmp(cmd_loop);
}

void message(str, p1, p2, p3, p4, p5, p6)
	char * str;
{
	if (str)
	{	printf(str, p1, p2, p3, p4, p5, p6);
		putchar('\n');
	}
}

void my_perror(s)
	char * s;
{
	perror(s);
}

void readcount(count)
	int count[2];
{
	if (arg_cnt > 0 && arg_type[0] == AT_INT)
	{	count[0] = args[0].arg_int;
		if (arg_cnt > 1 && arg_type[1] == AT_INT)
			count[1] = args[1].arg_int;
		else
			count[1] = 0;
	}
	else
	{	count[0] = -1; count[1] = -1;
	}
}

/*
 *	for core-window: read address and # fullwords
 *	return 1 if ok
 */

int getcorewin(addr, len)
	address * addr;
	int * len;
{
	if (arg_cnt > 0 && arg_type[0] == AT_INT)
	{	* addr = args[0].arg_int;
		* len = 20;
		return 1;
	}
	else
		return 0;
}

void die(code)
	int code;
{
	exit(code);
}

void getargs(buf)
	char * buf;
{	char * cp;
	char line[LINSIZ];

	strcpy(buf, aoutfile); strcat(buf, " ");
	if (arg_cnt > 0 && arg_type[0] == AT_STR)
		strcpy(line, args[0].arg_string);
	else
	{	printf("Enter arguments and i/o redirection\n");
		printf("%s%s ", (cp=getenv("PS1"))?cp:"$ ", aoutfile);
		gets(line);
	}
	strcat(buf, line);
}

/*
 *	dispose the actual window
 *
 *	(only for D-, T- and C-windows)
 */

kill_view(tokill)
	view ** tokill;
{	view ** ptr;

	if (mode != 'd' && mode != 't' && mode != 'c')
		return;
	if (! tokill) return;
	for ( ptr = tokill ; *ptr ; ++ptr )
		cfree(*ptr);
	cfree(tokill);
}

getfilename(buf, bufsize)
	char buf[];
	int bufsize;
{
	if (arg_cnt > 0 && arg_type[0] == AT_STR)
		strncpy(buf, args[0].arg_string, bufsize);
	else
		buf[0] = '\0';
}

/*
 *	local functions
 */

#define LINESIZE 80

/*
 *	return next command from line buffer
 *
 *	command syntax:
 *
 *	line   = cmd { ";" cmd }
 *	cmd    = count letter { arg }
 *	arg    = intarg | strarg
 *	count  = intarg
 *	intarg = digit { digit } [H|B]
 *	strarg = text | "'" text "'" | """ text """
 */

static char getcmd()
{	static char linbuf[LINESIZE];
	static char * cp = NULL;
	static int cmd_cnt = 0;
	static char * prompt = NULL;
	static char cmd;

	if (! prompt)
	{	if (! (prompt = getenv("MDBPS")))
			prompt = "> ";
	}
	if (cmd_reset)		/* forget buffer contents on interrupts */
	{	cp = NULL;
		cmd_reset = 0;
		cmd_cnt = 0;
	}
	if (cmd_cnt > 0)
	{	--cmd_cnt;
		return cmd;
	}
	if (! cp || ! *cp)
	{	printf("%s", prompt);
		if (! gets(linbuf))
		{	putchar('\n');
			arg_cnt = 0; cmd_cnt = 0;
			return 'q';	/* quit */
		}
		cp = linbuf;
	}
	cmd = 0; arg_cnt = 0;
	while (*cp && *cp != ';')
	{	while (isspace(*cp++))
			;
		if (isdigit(* --cp))		/* look for command count */
		{	int_arg(& cp); arg_cnt = 0; cmd_cnt = args[0].arg_int;
		}
		else
			cmd_cnt = 1;
		while (isspace(*cp++))
			;
		cmd = *--cp;		/* pick up command */
		if (cmd == ';' || ! cmd)
		{	cmd = 0;
			break;
		}
		++cp;
		for(;;)				/* look for arguments */
		{	while (isspace(*cp))
				++cp;
			if (! * cp || * cp == ';')
				break;
			if (isdigit(* cp) || *cp == '-' && isdigit(*(cp+1)))
				int_arg(& cp);
			else if (cmd == '$' && arg_cnt == 0)
				char_arg(& cp);
			else
				str_arg(& cp);
		}
	}
	if (* cp == ';')
		++cp;
	--cmd_cnt;
	return cmd;
}

static void int_arg(cpp)
	char ** cpp;
{	int octal = 0;		/* digits interpreted in octal */
	int hex = 0;		/* digits interpreted in hex */
	int decimal = 0;	/* digits interpreted in decimal */
	int result;
	char lastdig;
	int minus = 0;

	if (** cpp == '-')
	{	minus = 1;
		++*cpp;
	}
	while (isxdigit(** cpp))
	{	if (isdigit(** cpp))
		{	decimal = **cpp - '0' + 10*decimal;
			octal = **cpp - '0' + 010*octal;
			hex = **cpp - '0' + 0x10*hex;
		}
		else if (isupper(** cpp))
			hex = **cpp - 'A' + 10 + 0x10*hex;
		else if (islower(** cpp))
			hex = **cpp - 'a' + 10 + 0x10*hex;
		lastdig = ** cpp;
		++*cpp;
	}
	if (lastdig == 'B' || lastdig == 'b')
		result = octal;
	else if (** cpp == 'H' || ** cpp == 'h')
	{	result = hex; ++*cpp;
	}
	else
	{	result = decimal;
	}
	if (minus)
		result = -result;
	if (arg_cnt < MAXARG)
	{	args[arg_cnt].arg_int = result;
		arg_type[arg_cnt] = AT_INT;
		++ arg_cnt;
	}
}

static void char_arg(cpp)
	char ** cpp;
{	char ch = ** cpp;
	char string[2];

	if (ch == ';')
		string[0] = '\0';
	else
		string[0] = ch, string[1], ++*cpp;
	if (arg_cnt < MAXARG)
	{	args[arg_cnt].arg_string = strsave(string);
		arg_type[arg_cnt] = AT_STR;
		++ arg_cnt;
	}
}

static void str_arg(cpp)
	char ** cpp;
{	char * str;
	char del;
	char save;

	del = **cpp;
	if (del != '"' && del != '\'')
		del = ';';
	else
		++*cpp;
	str = *cpp;
	while (* str && * str != del)
		++str;
	save = * str; * str = '\0';
	if (arg_cnt < MAXARG)
	{	args[arg_cnt].arg_string = strsave(* cpp);
		arg_type[arg_cnt] = AT_STR;
		++ arg_cnt;
	}
	* str = save;
	*cpp = str;
	if (save != ';' && save)
		++*cpp;
}

static void special_cmd()
{	char cmd;
	char * cp;
	int i;

	if (arg_cnt == 0 || arg_type[0] != AT_STR)
		cmderror("Unknown command.");
	cmd = args[0].arg_string[0];
	for (cp = args[0].arg_string+1; *cp && isspace(*cp); ++ cp)
		;
	if (* cp)
	{	cp = strsave(cp);
		free(args[0].arg_string);
		if (isdigit(* cp))
		{	arg_type[0] = AT_INT;
			args[0].arg_int = atoi(cp);
			free(cp);
		}
		else
			args[0].arg_string = cp;
	}
	else
	{	--arg_cnt;
		for (i = 0; i < arg_cnt; ++i)
		{	args[i] = args[i+1];
			arg_type[i] = arg_type[i+1];
		}
	}
	switch (cmd)
	{	case 'm':	print_maps(); break;
		case 'r':	print_regs(); break;
		case 'w':	wd_handling(); break;
		default:	cmderror("Unknown command.");
	}
}

static void wd_handling()
{
	if (arg_cnt == 0)
		show_wds();
	else if (arg_cnt == 1 && arg_type[0] == AT_INT)
		new_wd(args[0].arg_int, 1, 0);
	else if (arg_cnt == 2 && arg_type[0] == AT_INT &&
		 arg_type[1] == AT_INT)
		new_wd(args[0].arg_int, 0, args[1].arg_int);
	else
		cmderror("bad argument(s) to $w; usage: $w [address1 [value]]");
}

/*
 *	switch to window 'win'
 *	set dot and mode
 */
static void window(win, print)
	char win;
	int print;
{	view ** old_view;	/* return to old view on error */
	struct module * mod = NULL;
	tree * t = NULL, * tson = NULL, * tbase, * tfather;
	unsigned size = 0;
	unsigned line = 0;
	address addr = 0;
	int space = DATA;
	static char addrformat = 'h', format = 'a'; /* for c-window */

	if ((win == 'c' || win == 'p') && !coremode && !pid)
		cmderror("No core file / running process");
	if (! coremode && ! pid)
		space = TEXT;
	old_view = act_view;
	if (win == 's')
		win = 'd';
	if (win == 'i')
		addr = idot;
	if (win == 'd' || win == 't' || win == 'c' || win == 'f' ||
	    win == 'D' || win == 'i')
	{	if (dot && *dot)
		{	mod = (*dot)->v_mod;
			t = (*dot)->v_node;
			if (mode == 'c' || mode == 'i')
			{	size = (*dot)->v_size;
				addr = (*dot)->v_addr;
			}
			if (mode == 'p' && win == 'i')
				addr = (*dot)->v_addr;
			if (win == 'i')
			{	space = TEXT;
				if (arg_cnt > 0 && arg_type[0] == AT_INT)
					addr = args[0].arg_int;
				if (arg_cnt > 1 && arg_type[1] == AT_STR)
					switch (args[1].arg_string[0])
					{	case 't': case 'T': case '?':
							space = TEXT; break;
						case 'd': case 'D': case '/':
							space = DATA; break;
					}
			}
			if (win == 'c')
			{	if (arg_cnt >= 2 && arg_type[1] == AT_STR)
					format = args[1].arg_string[0];
				if (arg_cnt >= 3 && arg_type[2] == AT_STR)
					addrformat = args[2].arg_string[0];
			}
			line = (*dot)->v_lineno;
			if (win == 'f')
			{	if (mode == 'p')
					cmderror("in d-window only");
				if (t && t->t_father)
				{	t = t->t_father;
					tson = t;
					if (t->t_father)
						t = t->t_father;
				}
				win = 'd';
			}
			else if (win == 'D')
			{	if (t && t->t_father)
					t = t->t_father;
				win = 'd';
			}
			else if (win == 'd' && mode == 'p' &&
				 (t->t_son == NULL || t->t_type == T_MOD) &&
				 t->t_father)
			{	/* there are no local variables nor
				   local procedures; so try to find
				   the surrounding procedure on the
				   stack
				*/
				tfather = t->t_father;
				while (tfather && tfather->t_type == T_MOD)
					tfather = tfather->t_father;
				while (movedot(1) &&
				       (*dot)->v_node != tfather)
					;
				if ((*dot)->v_node != tfather)
					/* shouldn't happen */
					cmderror("no descendent");
				tson = t;
				t = t->t_father;
			}
			if (t && t->t_plevel && win == 'd' && mode == 'p')
			{	tbase = t;
				while (tbase->t_father &&
				       tbase->t_type == T_MOD)
					tbase = tbase->t_father;
				tbase->t_base = (*dot)->v_base;
			}
		}
		else if (win != 'c')
			cmderror("No switch possible");
	}
	if (mode == 'p')
		pdot = dot;
	switch (win)
	{	case 'c': act_view = corewin(t, mod, addr, size,
					addrformat, format); break;
		case 'd': act_view = data_view(t, mod); break;
		case 'i': act_view = instrwin(space, addr); break;
		case 'm': act_view = module_view(); break;
		case 'p': act_view = backtrace(); break;
		case 't': act_view = textwin(mod, t, line); break;
		case 'H': act_view = get_copying_view(); break;
	}
	if (act_view)
	{	if (win == 'p')
			if (pdot)
				dot = pdot;
			else
				dot = pdot = init_dot();
		else if (mod && win == 'm')
		{	for (dot = act_view; dot && *dot; ++ dot)
				if ((*dot)->v_mod == mod)
					break;
			if (! dot || ! *dot)
				dot = init_dot();
		}
		else if (tson)
		{	for (dot = act_view; dot && *dot; ++ dot)
				if ((*dot)->v_node == tson)
					break;
			if (! dot || !*dot)
				dot = init_dot();
		}
		else
			dot = init_dot();
		if (dot)
		{	if (mode == 'd' || mode == 't' || mode == 'c' ||
			    mode == 'i')
				kill_view(old_view);
			mode = win;
			if (print)
				if (mode == 'p')
					{
					print_header();
					}
				else
					print_line(1);
			return;
		}
	}
	/* fail */
	act_view = old_view;
}

/*
 *	print runtime error message
 */
static void print_rte()
{
	message(error_mess());
}

/*
 *	print current line
 */

static void print_line(print)
	int print;
{	if (print && dot && *dot)
		printf("%s\n", (*dot)->v_line);
}

/*
 *	print lines from beginning of view to dot
 */

static void print_header()
{	view ** ptr;

	if (! act_view || ! dot || !* dot)
		return;
	for (ptr = act_view; ptr <= dot; ++ ptr)
		printf("%s\n", (*ptr)->v_line);
}

/*
 *	offset may be +1 or -1
 *	return 1 if ok
 */
static int multiple_move(offset)
{	int cnt = 1;	/* default */
	int result;

	if (arg_cnt > 0 && arg_type[0] == AT_INT)
		cnt = args[0].arg_int;
	while (cnt-- > 0 && (result = movedot(offset)))
		;
	return result;
}

/*
 *	offset may be +1 or -1
 *	return 1 if ok
 */
static int movedot(offset)
{	arg_cnt = 0;	/* remove arguments for window call */
	if (! dot || ! *dot)
		dot = init_dot();
	else if (dot == act_view && offset == -1)
		if ((*dot)->v_flags & V_START)
			return 0;
		else
		{	window(mode, 0);
			if (dot && *dot && !((*dot)->v_flags & V_START) &&
				dot > act_view && *(dot-1))
				--dot;
			else
				return 0;
		}
	else if (((*dot)->v_flags & V_END) && offset == 1)
		return 0;
	else 
	{	dot += offset;
		if (! *dot)
		{	dot -= offset;
			window(mode, 0);
			if (dot && *dot && !((*dot)->v_flags & V_END) &&
				*(dot+1))
				++dot;
			else
				return 0;
		}
	}
	return 1;
}

/*
 *	move dot to first line with non-zero node
 */
static view ** init_dot()
{	view **ptr;

	if (! act_view)
		return NULL;
	for (ptr = act_view; *ptr && !((*ptr)->v_flags & V_MOVE); ++ptr)
		;
	if (* ptr)
		return ptr;
	for (ptr = act_view; *ptr && (*ptr)->v_node == NULL; ++ptr)
		;
	if (! *ptr)
		return act_view;
	return ptr;
}

static void breakpoint(cmd)
	char cmd;
{	char * err;
	tree * t;

	err = "cannot set breakpoint";
	if (! dot || ! *dot || !(*dot)->v_node || !(*dot)->v_mod)
		cmderror(err);
	t = (*dot)->v_node;
	if (t->t_type != T_PROC)
		cmderror("no procedure");
	if (init_brkpt((*dot)->v_mod))
		cmderror(err);
	switch (cmd)
	{	case 'B' :	set_brkpt(t); break;
		case 'D' :	del_brkpt(t); break;
	}
}

static void print_maps()
{	static char hex[] = { "%s %8xH %8xH %8xH\n     %8xH %8xH %8xH\n" };
	static char octal[] = { "%s %11oB %11oB %11oB\n     %11oB %11oB %11oB\n" };
	static char dec[] = { "%s %10d %10d %10d\n     %10d %10d %10d\n" };
	char * format = hex;
	char * cp;

	if (arg_cnt > 0 && arg_type[0] == AT_STR)
		switch(args[0].arg_string[0])
		{	case 'x': case 'X': case 'H': case 'h':
				format = hex; break;
			case 'd': case 'D':
				format = dec; break;
			case 'o': case 'O': case 'b': case 'B':
				format = octal; break;
		}
	printf(format, "TEXT",
		txtmap.b1, txtmap.e1, txtmap.f1,
		txtmap.b2, txtmap.e2, txtmap.f2);
	printf(format, "CORE",
		datmap.b1, datmap.e1, datmap.f1,
		datmap.b2, datmap.e2, datmap.f2);
}

static void print_regs()
{	char ** regn;
	static char dec[] = { "%-6s %10d\n" };
	static char hex[] = { "%-6s %8xH\n" };
	static char octal[] = { "%-6s %11oB\n" };
	char * format = hex;

	if (arg_cnt > 0 && arg_type[0] == AT_STR)
		switch(args[0].arg_string[0])
		{	case 'x': case 'X': case 'H': case 'h':
				format = hex; break;
			case 'd': case 'D':
				format = dec; break;
			case 'o': case 'O': case 'b': case 'B':
				format = octal; break;
		}
	if (! pid && ! coremode)
		cmderror("No core file / running process");
	for (regn = regnames; *regn; ++ regn)
		printf(format, * regn, getreg(reglist[regn-regnames]));
	idot = getreg(PC);
	show_instr(TEXT);
	if (ppc && ppc != getreg(PC))
	{	idot = ppc;
		show_instr(TEXT);
	}
}

/*
 *	start "mdb"-mode after execution of child process
 */

static void mdbmode()
{	tree * old_node;
	tree * t;

	arg_cnt = 0;
	pdot = NULL;
	switch(mode)
	{	case 'm' :
		case 't' :
		case 'i' :
			break;
		case 'p' :
			if (pid || coremode)
			{	dot = NULL;
				window('p', 1);
			}
			else
				window('m', 1);
			break;
		case 'c' :
		case 'd' :
			if ( (pid || coremode) && dot && *dot )
			{	old_node = (*dot)->v_node;
				if (mode == 'c')
					window('c', 1);
				else
				{	t = (*dot)->v_node;
					if (! t)
					{	window('m', 1);
						break;
					}
					if (t->t_father)
						t = t->t_father;
					window('D', 0);
					for (dot = act_view; *dot; ++dot)
						if ((*dot)->v_node == old_node)
							break;
					if (! *dot)
						dot = init_dot();
					print_line(1);
				}
			}
			else
				window('m', 1);
			break;
	}
}

static void call_vi()
{	struct module * mod;
	FILE * fp;		/* check fopen */
	char * file;		/* file to be vi'ed */
	char * cp;		/* for zapping the suffix of file */
	char shellcmd_buf[256];

	if (! dot || !*dot)
	{	message("no applicable source file");
		return;
	}
	mod = (*dot)->v_mod;
	if (! mod)
	{	message("no associated module");
		return;
	}
	if (mod->m_file && (fp = fopen(mod->m_file, "r")))
		file = mod->m_file;
	else if (mod->m_file && (cp = strrchr(mod->m_file, '.')) &&
		((* cp = '\0'), file = searchfile(mod->m_file, cp+1),
		 (* cp = '.'), file))
		;
	else if ((file = searchfile(mod->m_name, "m2")) == NULL &&
		 (file = searchfile(mod->m_name, "mr")) == NULL)
	{	message("%s: no source file found", mod->m_name);
		return;
	}
	fclose(fp);

	/* kludge shell_cmd */
	sprintf(shellcmd_buf, "vi +%d %s", (*dot)->v_lineno, file);
	arg_cnt = 1; arg_type[0] = AT_STR;
	args[0].arg_string = shellcmd_buf;
	shell_cmd();
}

static void shell_cmd()
{	int (* sigint)();
	int (* sigquit)();
	char * cmd;		/* to be executed */
	int pid;
	int stat;
	int w;

	if (arg_cnt > 0 && arg_type[0] == AT_STR)
		cmd = args[0].arg_string;
	else if (! (cmd = getenv("SHELL")))
		cmd = "/bin/sh";
	switch (pid = fork())
	{	case -1:
			perror("fork");
			break;
		case 0:
			execl("/bin/sh", "sh", "-c", cmd, 0);
			perror("execl");
			exit(255);
		default:
			sigint = signal(SIGINT, SIG_IGN);
			sigquit = signal(SIGQUIT, SIG_IGN);
			while ((w = wait(& stat)) != -1 && w != pid)
				;
			stat >>= 8;
			if (stat && stat != 255)
				printf("exit code: %d\n", stat & 255);
			printf("mdb\n");
			signal(SIGINT, sigint);
			signal(SIGQUIT, sigquit);
			break;
	}
}

static void convert()
{	int value;
	char type = 'd';
	char * format;

	if (arg_cnt == 0)
		return;
	if (arg_type[0] == AT_STR)
		value = args[0].arg_string[0];
	else
		value = args[0].arg_int;
	if (arg_cnt == 2 && arg_type[1] == AT_STR)
		type = args[1].arg_string[0];
	switch (type)
	{	case 'd':	format = "%d\n"; break;
		case 'B': case 'b':
		case 'o':	format = "%oB\n"; break;
		case 'H': case 'h':
		case 'x':	format = "%xH\n"; break;
		case 'C':
		case 'c':
				value &= 0377;
				if (isprint(value))
					format = "'%c'\n";
				else
					format = "%oC\n";
				break;
		default:
				cmderror("Unknown conversion type");
	}
	printf(format, value);
}

/*
 *	search in the current window for 'pattern'
 */

#if SYSV 
static void search(forward)
	int forward;	/* =1: forward; else back */
{	char * comp;	/* compiled pattern */
	static char * pattern = NULL;
	static char * lookforpar = NULL;

	if (! lookforpar)
		lookforpar = regcmp("[^\\\\]\$[0-9]", NULL);
	if (arg_cnt > 0 && arg_type[0] == AT_STR)
	{	if (pattern)
			free(pattern);
		pattern = strsave(args[0].arg_string);
		arg_cnt = 0;
		if (regex(lookforpar, pattern))
			cmderror("Invalid regular expression");
	}
	else if (! pattern)
		cmderror("No previous regular expression");
	if ((comp = regcmp(pattern, NULL)) == NULL)
		cmderror("Invalid regular expression");
	if (forward != 1)
		forward = -1;
	while (movedot(forward) && dot && *dot && (*dot)->v_line)
		if (regex(comp, (*dot)->v_line))
		{	free(comp);
			return;
		}
	free(comp);
	if (! dot || !*dot)
		dot = init_dot();
	cmderror("Not found");
}

#else

/*
 *	sometimes available under Unix Edition VII:
 *	
 *	re_comp(pattern) char * pattern;  	syntax see ed(1) Unix Ed. VII
 *	re_exec(string) char * string;
 *
 *	re_comp returns 0 if ok, else a pointer to an error message
 *	re_exec returns 1 on matches
 *
 *	if these functions aren't available replace them by strncmp
 */
/* this procedure used for BSD42 */

static void search(forward)
	int forward;	/* =1: forward; else back */
{	char * comp;	/* result of re_comp */
	static char * pattern = NULL;

	if (arg_cnt > 0 && arg_type[0] == AT_STR)
		pattern = strsave(args[0].arg_string);
	else if (! pattern)
		cmderror("No previous regular expression");
	if (comp = re_comp(pattern))
		cmderror(comp);
	if (forward != 1)
		forward = -1;
	while (movedot(forward) && dot && *dot && (*dot)->v_line)
		if (re_exec((*dot)->v_line) )
			return;
	if (! dot || !*dot)
		dot = init_dot();
	cmderror("Not found");
}
#endif /* ! SYSV */

static void screen_cmd(cmd)
	char cmd;
{	int back = 0;
	static int screen = 23;
	int line, lines;
	int backcnt = 0;
	int forwardcnt = 0;
	char * l = "-------------------------------------------------------------------------------";

	if (arg_cnt > 0 && arg_type[0] == AT_INT && args[0].arg_int > 0)
		screen = args[0].arg_int;
	if (cmd == 'b')
		back = screen;
	else if (cmd == 'u')
		back = (screen-2)/2+1;
	while (back > 0 && movedot(-1))
		back--,backcnt++;
	lines = screen-back;
	if (cmd == 'b' && back)
		++lines;
	for (line = 0; line < lines; ++ line)
	{	if (! back || line > 0)
		{	if (! movedot(1))
				break;
		}
		++forwardcnt;
		if (cmd == 'u' && forwardcnt == (back? backcnt+1: backcnt))
			puts(l);
		print_line(1);
		if (cmd == 'u' && forwardcnt == (back? backcnt+1: backcnt))
		{	puts(l);
			line += 2;
		}
	}
	if (cmd == 'u')
	{	lines = forwardcnt-backcnt-(back? 1: 0);
		for (line = 0; line < lines; ++line)
			if (! movedot(-1))
				break;
	}
}

static void print_instr()
{	static int space = TEXT;

	if (arg_cnt > 0 && arg_type[0] == AT_INT)
		idot = args[0].arg_int;
	if (arg_cnt > 1 && arg_type[1] == AT_STR)
		switch (args[1].arg_string[0])
		{	case 't': case 'T': case '?': space = TEXT; break;
			case 'd': case 'D': case '/': space = DATA; break;
		}
	show_instr(space);
}

static void show_instr(space)
	int space;
{	unsigned int ins;
	struct module * mod;
	tree * p;
	int ln;
	int result;	/* 0: ok */

	errno = 0;
#if PE
	if (space == DATA)
		result = access(READ, idot, space, & ins);
	else
		result = readaout(idot, & ins);
#endif
	if (! result)
	{	if (mod = getmod(idot))
		{	printf("[%s", mod->m_name);
			if (p = getproc(mod, idot))
				printf(".%s+#%x", p->t_name,
					idot - procentry(mod, p->t_pnum));
			printf("=#%x", idot);
			if (ln = get_ln(idot))
				printf(":%d", ln);
			printf("]");
		}
		else
			printf("[#%x]", idot);
		putchar('\t');
#if PE
		printins(space, ins);
		idot += idotinc;
#endif
#if M68
		idot += print_insn(space, idot);
#endif
		puts(tobeprinted);
	}
	else	/* error message already printed (i hope) */
		cmderror(NULL);
}

static void help()
{	static char * htext[] = {
		"+ #lines\t\tmove dot forward",
		"- #lines\t\tmove dot backwards",
		"<return>\t\tprint next line",
		"= n [o|d|x]\t\tconvert n",
		"/pattern\t\tsearch forward for pattern",
		"?pattern\t\tsearch backwards for pattern",
		"b\t\t\tdisplay last page",
		"c [address [f [af]]\tcore window",
		"d\t\t\tdata window",
		"e\t\t\tprint run time error message",
		"f\t\t\tmove to father level (in d-window only)",
		"h\t\t\tthis help text",
		"H\t\t\tGNU COPYING LICENCE",
		"i [address ['d'|'t']]\t\tprint instruction",
		"m\t\t\tmodule window",
		"n\t\t\tdisplay next page",
		"p\t\t\tprocedure backtrace window",
		"q\t\t\tquit",
		"s\t\t\tmove to son level (in d-window only)",
		"u\t\t\tdisplay context of current line",
		"t\t\t\ttext window",
		"v\t\t\tcall vi with current file and line",
		"x\t\t\texit",
		"B [count [count]]\tset breakpoint",
		"C\t\t\tcontinue",
		"D\t\t\tdelete breakpoint",
		"I\t\t\tcontinue but ignore signal",
		"M\t\t\tswitch to current process",
		"P [pdesc]\t\tswitch process",
		"R\t\t\trun",
		"S\t\t\tsingle step",
		"$m\t\t\tprint map",
		"$r\t\t\tdisplay registers and current instruction",
		NULL
		};
	register char ** cpp;

	for (cpp = htext; * cpp; ++ cpp)
		printf("%s\n", * cpp);
}

/*
 *	switch Modula-2 process
 */

static void p_switch(cmd)
	char cmd;	/* 'M' or 'P' */
{	tree * t;
	int addr;
	struct process pr;

	if (! coremode && pid == 0)
		cmderror("No process / core file");
	if (cmd == 'M')
	{
		new_stack(NULL);
		pdot = dot = NULL;
		window('p', 1);
		return;
	}
	if (arg_cnt > 0 && arg_type[0] == AT_INT)
		addr = args[0].arg_int;
	else
	{	if (mode != 'd' && mode != 'c')
			cmderror("Only in C- or D-window allowed");
		if (mode == 'd')
		{	t = (*dot)->v_node;
			if (! t)
				cmderror("Empty node");
			addr = getvaraddr(t);
		}
		else
			addr = (*dot)->v_addr;
		if (! addr) return;
		/***
		if (t->t_vartype != Process)
			cmderror("no PROCESS");
		***/
		getlong(addr, & addr);
	}
	if (addr % 4)
		cmderror("process descriptor address not aligned");
	get(addr, & pr, sizeof(struct process));
	new_stack(& pr);
	dot = pdot = NULL;
	window('p', 1);
	if (! dot || !*dot)
	{	message("PROCESS undefined");
		new_stack(NULL);
		dot = pdot = NULL;
		window('p', 1);
	}
}
