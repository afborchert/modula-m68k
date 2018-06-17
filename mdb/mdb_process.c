/*
 *	mdb - process handling
 */

#include	"mdb.h"
#include	<stdio.h>
#include	<assert.h>
#include	<ctype.h>
#include	<signal.h>
#if SYSV || SUNOS4
#if !NIXDORF && !SUNOS4
#include	<sys/ioctl.h>
#endif
#include	<termio.h>
#else
#include	<sgtty.h>
#endif
#if SYSV || BSD42
#include	<sys/types.h>
#if SYSV
#include	<sys/seg.h>
#endif
#include	<sys/signal.h>
#include	<sys/sysmacros.h>
#endif
#include	<sys/dir.h>
#if BSD42
#include	<machine/reg.h>
#include	<sys/ptrace.h>
#else
#include	<sys/reg.h>
#endif
#include	<sys/user.h>
#include	"mdb_ref.h"
#include	"mdb_tree.h"
#include	"mdb_bp.h"
#include	"mdb_ptrace.h"
#include	"mdb_user.h"

#define ILLEGAL 0x4afc4afcL

int pid = 0;	/* process id of child process */
int signo = 0;
extern char * aoutfile;
extern int coremode;
extern struct abp * abplist;	/* list of breakpoints */

extern char * error_mess();
extern struct module * getmod();
extern tree * getproc();

static tree * runpcs();


int reglist[] = {
#if SYSV || BSD42
	PS,
#else
	RPS,
#endif
	PC,
#if BSD42
	SP,
	AR6,
	AR5,
	AR4,
	AR3,
	AR2,
	AR1,
	AR0,
#else
	R15,
	R14,
	R13,
	R12,
	R11,
	R10,
	R9,
	R8,
#endif
	R7,
	R6,
	R5,
	R4,
	R3,
	R2,
	R1,
	R0
};

#if PE3200 && SYSV
char * regnames[] = { "ps", "pc", "rf", "base", "top", "rc", "rb", "ra",
		      "r9", "r8", "r7", "r6", "r5", "r4", "r3", "limit",
		      "r1", "r0", NULL};
#endif
#if M68
char * regnames[] = { "ps", "pc", "top","base", "a5", "a4", "a3", "a2",
		      "a1", "a0", "d7", "d6", "d5", "d4", "d3", "d2",
		      "d1", "d0", NULL};
#endif

/*
 *	visible routines
 */

tree * run(single)
	int single;
{
	endpcs();
	setup();
	init_wds();
	if (pid)
		return runpcs(0, single);
	else
		return NULL;
}

tree * cont(callisr, single)
	int callisr;
	int single;
{
	if (pid)
		return runpcs(callisr, single);
	else
		cmderror("no process");
	/* NOTREACHED */
}

/*
 *	private routines
 */

static endpcs()
{	struct abp * p;

	if (pid)
	{	ptrace(EXIT, pid, 0, 0);
		pid = 0;
	}
	for ( p = abplist ; p ; p = p->abp_link )
	{	p->abp_flag &= ~(ABP_EXEC | ABP_PASSED | ABP_ACTIVE);
		if (p->abp_count = p->abp_initcnt)
			p->abp_flag |= ABP_TOSET;
	}
}

#define	LINSIZ	80
#define	MAXARG	10

/*
 *	read arguments and call "exec"
 */

static doexec()
{	char args[LINSIZ];
	char * argl[MAXARG];
	char * cp, * cp2;
	char prev;
	int argc;
	char * old_arg;
	int fd;
	int i;

	signal(SIGINT, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
	getargs(args);
	old_arg = NULL;
	i = 0;
	for ( cp = args ; ; ++cp )
		switch ( *cp )
		{	case ' ' : case '\t' : case '\0' :
				if (old_arg)
				{	argl[i++] = old_arg;
					old_arg = NULL;
				}
				if (! *cp)
					goto break2;
				else
					*cp = '\0';
				break;
			case '<' :
				for ( ++cp ; isspace(*cp) ; ++cp )
					;
				if (*cp)
				{	fclose(stdin);
					for (cp2 = cp; * cp2 &&
					     ! isspace(* cp2); ++ cp2)
						;
					prev = * cp2;
					* cp2 = 0;
					if ( open(cp, 0) < 0 )
					{	my_perror(cp);
						exit(0);
					}
					cp = cp2;
					* cp = prev;
				}
				break;
			case '>' :
				for ( ++cp ; isspace(*cp) ; ++cp )
					;
				if (*cp)
				{	fclose(stdout);
					for (cp2 = cp; * cp2 &&
					     ! isspace(* cp2); ++ cp2)
						;
					prev = * cp2;
					* cp2 = 0;
					if ( creat(cp, 0666) < 0 )
					{	my_perror(cp);
						exit(0);
					}
					cp = cp2;
					* cp = prev;
				}
				break;
			default :
				if (! old_arg)
					old_arg = cp;
				break;
		}
break2:
	for ( fd = 3 ; fd < _NFILE ; ++ fd )
		close(fd);
	argl[i++] = NULL;
	signal(SIGINT, SIG_DFL);
	signal(SIGQUIT, SIG_DFL);
	ptrace(SETTRC, 0, 0, 0);
	execv(aoutfile, argl);
	my_perror(aoutfile);
}

static setup()
{
	switch ( pid = fork() )
	{	case 0 :	/* child process */
			doexec();
			exit(0);
		case -1 :	/* failed */
			my_perror("fork");
			pid = 0;
			break;
		default :	/* father */
			bpwait();	/* wait for "exec"-call of child */
	}
}

static bpwait()
{	register int w;
	int stat;
	void (*sigint)();
	void (*sigquit)();
	char mess[80];

	sigint = signal(SIGINT, SIG_IGN);
	sigquit = signal(SIGQUIT, SIG_IGN);
	while ( (w = wait(&stat)) != pid && w != -1 )
		;
	signal(SIGINT, sigint);
	signal(SIGQUIT, sigquit);
	Debug("bpwait: returned from wait: pid=%d, w=%d, stat=%d\n",
		pid, w, stat);
	strcpy(mess, "\n[");
	if ( w == -1 )
	{	my_perror("wait");
		pid = 0;
	}
	else if ((stat & 0177) != 0177)
	{	if (signo = stat & 0177)
		{	readregs();
			sigprint(signo, mess);
		}
		else
			strcat(mess, "process terminated");
		if (stat & 0200)
		{	strcat(mess, " - core dumped");
			core_end();
			core_init("core");
		}
		pid = 0;
		strcat(mess, "]");
		message(mess);
	}
	else
	{	readregs();
		signo = stat >> 8;
		if (signo != SIGTRAP && signo != SIGILL)
		{	sigprint(signo, mess);
			strcat(mess, "]");
			message(mess);
		}
		else
		{	Debug("bpwait: signo=%d\n", signo);
			signo = 0;
		}
	}
}

sigprint(signo, mess)
	int signo;
	char mess[];
{	char *m;
	char buf[32];

	switch (signo)
	{	case SIGHUP  :	  m = "hangup"; break;
		case SIGINT  :	  m = "interrupt"; break;
		case SIGQUIT : 	  m = "quit"; break;
		case SIGILL  : 	  m = "illegal instruction"; break;
		case SIGTRAP : 	  m = "trace trap"; break;
		/* run time error */
		case SIGIOT  :	  m = error_mess(); break;
		case SIGEMT  : 	  m = "EMT instruction"; break;
		case SIGFPE  : 	  m = "arithmetic fault"; break;
		case SIGKILL :    m = "kill"; break;
		case SIGBUS  :    m = "memory fault"; break;
		case SIGSEGV :    m = "segmentation violation"; break;
		case SIGSYS  :    m = "bad argument to system call"; break;
		case SIGPIPE : 	  m = "write on a pipe or link with no one to read it"; break;
		case SIGALRM : 	  m = "alarm clock"; break;
		case SIGTERM : 	  m = "software termination signal"; break;
#ifdef BSD42
		case SIGURG  :	  m = "urgent condition on IO channel"; break;
		case SIGSTOP :	  m = "sendable stop signal not from tty"; break;
		case SIGTSTP :	  m = "stop signal from tty"; break;
		case SIGCONT :	  m = "continue a stopped process"; break;
		case SIGCHLD :	  m = "to parent on child stop or exit"; break;
		case SIGTTIN :	  m = "to readers pgrp upon background tty read"; break;
		case SIGTTOU :	  m = "like TTIN for output if (tp->t_local&LTOSTOP)"; break;
		case SIGIO :	  m = "input/output possible signal"; break;
		case SIGXCPU :	  m = "exceeded CPU time limit"; break;
		case SIGXFSZ :	  m = "exceeded file size limit"; break;
		case SIGVTALRM :  m = "virtual time alarm"; break;
		case SIGPROF :	  m = "profiling time alarm"; break;
		case SIGWINCH :	  m = "window changed"; break;
		case SIGLOST :	  m = "resource lost (eg, record-lock lost)"; break;
		case SIGUSR1 :	  m = "user defined signal 1"; break;
		case SIGUSR2 :	  m = "user defined signal 2"; break;
#endif
		default:	  sprintf(buf, "signal number %d", signo);
				  m = buf;
				  break;
	}
	strcat(mess, m);
}

static setbps()
{	register struct abp * p;

	for ( p = abplist ; p ; p = p->abp_link )
	{	if (p->abp_count == 0)
			continue;
		if (p->abp_flag & ABP_EXEC)
		{	p->abp_flag &= ~ABP_EXEC;
			continue;
		}
		if (p->abp_flag & (ABP_PASSED | ABP_TOSET))
		{	getlong(p->abp_loc, & p->abp_inst);
			putlong(p->abp_loc, ILLEGAL, /* text = */ 1);
			p->abp_flag |= ABP_ACTIVE;
			p->abp_flag &= ~(ABP_PASSED | ABP_TOSET);
			Debug("setbps: breakpoint set at loc 0x%x\n", p->abp_loc);
		}
	}
}

/*
 *	set breakpoint at text-location 0 for preventing restart
 */
static setnullbp()
{
#ifndef BSD42
	if (pid)
	{	putlong(0, ILLEGAL, /* text = */ 1);
		Debug("setnullbp: breakpoint set at loc 0x0\n");
	}
#endif
}

static tree * runpcs(callisr, single)
	int callisr;	/* if on: continue with signal execsig */
	int single;	/* if on: single step execution */
{	static int userpc = 1;
	static int execsig = 0;
#if SYSV || SUNOS4
	static struct termio usrtty;
	static struct termio mdbtty;
#else
	static struct sgttyb usrtty;
	static struct sgttyb mdbtty;
#endif
	static int usrttyset = 0;
	register long addr;
	register tree * t;
	register struct module * mod;
	register struct abp * p;
	int loopcnt;

	assert(pid);
	if (! single)
		message("[running...]");
	if (! callisr)
		execsig = 0;
	loopcnt = 1;
	t = NULL;
#if SYSV || SUNOS4
	ioctl(0, TCGETA, & mdbtty);
#else
	gtty(0, & mdbtty);
#endif
	while (loopcnt--)
	{	if (! single) setbps();
		if (usrttyset)
#if SYSV || SUNOS4
			ioctl(0, TCSETA, & usrtty);
#else
			stty(0, & usrtty);
#endif
		/* continue child proc */
		Debug("runpcs: ");
		if (single)
			Debug("single step: ");
		else
			Debug("continue: ");
		Debug("pid=%d, userpc=0x%x, execsig=%d\n", pid, userpc, execsig);
		if (ptrace(single?SINGLE:CONTIN, pid, userpc, execsig) < 0)
			my_perror("ptrace");
		bpwait();
		if (pid)
		{	/** readregs(); -- done by bpwait() */
			if (! single && getreg(PC) >= sizeof(long))
				setnullbp();
#if SYSV || SUNOS4
			ioctl(0, TCGETA, & usrtty);
			ioctl(0, TCSETA, & mdbtty);
#else
			gtty(0, &usrtty);
			stty(0, &mdbtty);
#endif
			usrttyset = 1;
	
			/*
			 *	look for breakpoint
			 */
	
			execsig = signo;
			if (signo == 0)
			{	addr = getreg(PC);
				Debug("[stopped at 0x%x]\n", addr);
				if (addr == 0)
				{	message("[illegal jump to address 0]");
					break;
				}
				if ((mod = getmod(addr)) &&
				    (t = getproc(mod, addr)) &&
				    t->t_bp[0] && t->t_bp[1] &&
				    t->t_bp[2] &&
				    (t->t_bp[0]->abp_loc == addr ||
				     t->t_bp[1]->abp_loc == addr ||
				     t->t_bp[2]->abp_loc == addr ) )
				{	if (t->t_bp[0]->abp_loc == addr)
						p = t->t_bp[0];
					else if (t->t_bp[1]->abp_loc == addr)
						p = t->t_bp[1];
					else
						p = t->t_bp[2];
					if (!(p->abp_flag & ABP_ALWAYS) &&
					    -- p->abp_count)
						++loopcnt;
					else if (p->abp_flag & ABP_SKIP)
						++loopcnt;
					else
						message("[breakpoint]");
					p->abp_flag |= (ABP_PASSED|ABP_EXEC);
					p->abp_flag &= ~ABP_ACTIVE;
					putlong(p->abp_loc, p->abp_inst, 1);
				}
				else if (! single)
				{
#ifdef DEBUG
					if (mod && t)
						dbg_loc(mod, t, addr);
#endif DEBUG
					message("mysterious stop ?!?");
				}
			}
		}
		else
		{	
#if SYSV || SUNOS4
			ioctl(0, TCSETA, & mdbtty);
#else
			stty(0, &mdbtty);
#endif
			usrttyset = 0;
		}
	}
	if (pid || coremode)
	{
		new_stack(NULL);
	}
	userpc = 1;
	return t;
}

extern int * corhdr;
extern int * endhdr;


static readregs()
{
#if SUNOS4
	static struct regs allregs;

	if (ptrace(PTRACE_GETREGS, pid, & allregs) < 0)
	{	my_perror("ptrace/getregs");
		message("cannot read registers");
	}
	endhdr = (int *) & allregs;
#else /* !SUNOS4 */
	register int i;
	address regbase;

#ifdef DEBUG_PROCESS
	printf("readregs...\n");
#endif DEBUG_PROCESS
	regbase = ptrace(RUREGS, pid, & ((struct user *) 0)->u_ar0, 0);
	/* map regbase to [0..ctob(USIZE)-1] */
	regbase &= AR0_MASK;
	if (USIZE == 2 && regbase < ctob(1))
		regbase += ctob(1);	/* registers in 2nd click */
	endhdr = (int *)((char *)  corhdr + regbase);
#ifdef DEBUG_PROCESS
	printf("\tcorhdr ->  0x%x\n", (int)corhdr);
	printf("\tendhdr ->  0x%x\n", (int)endhdr);
	printf("\tdiff:      %d bytes\n", (char *) endhdr - (char *) corhdr);
	printf("\tUSER size: %d bytes\n", ctob(USIZE));
	printf("\tUSIZE = %d, ctob(2) = %d\n", USIZE, ctob(2));
	{	int addr = ptrace(RUREGS, pid, & ((struct user *)0)->u_ar0, 0);
		printf("\tu->u_ar0 : 0x%x == %d\n", addr, addr);
	}
	printf("\n\t  i reglist[i]  name endhdr[reglist[i]]\n");
#endif DEBUG_PROCESS
	for (i = 0; i < 18; ++i)
	{	endhdr[reglist[i]] =
		    ptrace(RUREGS, pid, (char *) &endhdr[reglist[i]] -
		    (char *) corhdr, 0);
#ifdef DEBUG_PROCESS
		printf("\t%3d        %3d %5s         0x%8x\n",
			i, reglist[i], regnames[i], endhdr[reglist[i]]);
#endif DEBUG_PROCESS
	}
#ifdef DEBUG_PROCESS
	{	FILE * out = fopen("mdb_OUT", "w");
		fprintf(out, "\n\nUSER structure:\n");
		for (i = 0; i < 2*ctob(USIZE); i += sizeof(int)/2)
		{	int value = ptrace(RUREGS, pid, i, 0);
			fprintf(out, "[%4d] 0x%08x ", i, value);
			if (i % sizeof(int))
				fprintf(out, "\n");
		}
		fprintf(out, "\n");
		fclose(out);
	}
	{	int addr;
		do
		{	printf("mdb debug mode > "); scanf("%d", & addr);
			printf("[%4d] 0x%x\n", addr, ptrace(RUREGS, pid, addr, 0));
			addr += sizeof(int);
		} while (addr > sizeof(int));
	}
#endif
#endif /* SUNOS */
}

#ifdef DEBUG
dbg_loc(mod, t, addr)
	struct module * mod;
	tree * t;
	address addr;
{
	Debug("\n\ndebugging information about\n");
	Debug("mysterious stop at 0x%x\n", addr);
	Debug("location in module %s procedure %s\n", mod->m_name, t->t_name);
	Debug("breakpoint locations of this procedure:\n");
	Debug("   start  0x%x\n", t->t_bp[0]->abp_loc);
	Debug("   begin  0x%x\n", t->t_bp[1]->abp_loc);
	Debug("   end    0x%x\n", t->t_bp[2]->abp_loc);
	Debug("end of debugging information\n\n");
}
#endif DEBUG
