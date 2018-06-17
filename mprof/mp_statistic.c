/*
 *	mp -- make profile statistic
 */

#include	<stdio.h>
#include	<sys/types.h>
#include	<sys/stat.h>
#ifdef xelos
#include	<sys/times.h>		/* get clock frequency */
#endif
#include	"mp.h"

#if SYSV || BSD42
#define	UNIT short
#else
#define	UNIT int
#endif

extern char * calloc();

extern struct module mod[];
extern int a_mod;
extern char sortflag;		/* 0, 't', 'c' or 'n' */
extern int hflag;

static void show();
static void sort();

static struct line ** lines;	/* output lines (if to be sorted) */
static struct line ** lp;	/* current line */
static int line_cnt = 0;	/* calculate # output lines */
#ifdef xelos
static int tickspersec;
#else
# if SUN
static int tickspersec = 50;	/* SUN 3 has one tick in 20 msec */
/* static int tickspersec = 100; * SUN 4 would have one tick in 10 msec */
# else
static int tickspersec = 60;	/* not OK in Australia */
# endif
#endif

/*
 *	layout of the monitor file:
 *
 *	struct hdr hdr;			(* header *)
 *	struct cnt cntbuf[hdr.ncount];  (* count buffer *)
 *	UNIT buf[];			(* profil(2) buffer: rest of the file *)
 */

struct hdr {
	UNIT * lowpc;
	UNIT * highpc;
	int ncount;			/* # words in count buffer */
};

struct cnt {
	unsigned cvalue;
	long cncall;
};

struct line {			/* sorted by ... if sortflag == ... */
	double l_ptime;		/* time in percentage */	/* 't' */
	double l_time;		/* time in seconds */
	long l_calls;		/* # calls */			/* 'c' */
	char * l_mod;		/* module */			/* 'n' */
	char * l_proc;		/* procedure */			/* 'n' */
};

/*
 *	HEADER and FORMAT accordingly to system dependant conventions of prof
 */
#if SYSV || BSD42
/*                12345 1234567 1234567 1234567 12345678901 */
#define HEADER " %Time Seconds Cumsecs  #Calls   msec/call  Module.Procedure\n"
#define FORMAT1 "%6.1f%8.2f%8.2f"
#define FORMAT2 " %7ld %11.4f  "
/*                  #Calls   msec/call  */
/*                 1234567 12345678901  */
#define FORMAT2_ "                      "
#define FORMAT3 "%s.%s\n"
#define SHOW1(m,p,t,cs,s)	t,s,cs
#define SHOW2(c,ms)		c,ms
#define SHOW3(m,p,t,cs,s)	m,p
#else
#define HEADER "  module      procedure %time     secs  #call  ms/call\n"
#define FORMAT1 "%8.8s %14.14s%6.1f%9.2f"
#define FORMAT2 " %6ld  %7.2f"
#define FORMAT2_ ""
#define FORMAT3 "\n"
#define SHOW1(m,p,t,cs,s)	m,p,t,s
#define SHOW2(c,ms)		c,ms
#define SHOW3(m,p,t,cs,s)	0
#endif

statistic(monout)
	char * monout;		/* monitor file */
{	struct module * m;	/* module */
	struct proc * p;	/* procedure */
	unsigned UNIT ccnt;	/* count in profil(2) buffer */
	unsigned pcl;		/* [pcl..pch]: ccnt hits */
	unsigned pch;
	unsigned bufs;		/* size of profil(2) buffer */
	double ftime;		/* temporary time variable */
	double actime;		/* accumulated time */
	double totime;		/* time total used */
	double scale;		/* scale factor (highpc-lowpc)/bufs*UNIT */
	FILE * pfile;		/* -> monitor file */
	struct hdr h;		/* header of monitor file */
	struct cnt * cp;	/* current entry in count buffer */
	struct cnt * cbuf;	/* -> count buffer */
	int i;			/* index of profil(2) buffer */
	struct proc ** pp;	/* mod->m_ptr ... */
	unsigned highpc;	/* h.highpc */
	unsigned lowpc;		/* h.lowpc */
	struct stat stbuf;	/* of monitor file for getting the size */
#ifdef xelos
	struct tms tmsbuf;	/* result of times(2) */
#endif

	if ((pfile = fopen(monout, "r")) == NULL)
	{	perror(monout);
		exit(1);
	}
	fstat(fileno(pfile), & stbuf);
	fread((char *) & h, sizeof(struct hdr), 1, pfile);
	lowpc = h.lowpc;
	highpc = h.highpc;
	bufs = stbuf.st_size - sizeof(struct hdr) - h.ncount*sizeof(struct cnt);
#ifdef xelos
	times(& tmsbuf);
	tickspersec = tmsbuf.tms_cfreq;
#endif

	/* work up count buffer */
	if ((cbuf = (struct cnt *)
		calloc(h.ncount, sizeof(struct cnt))) == NULL)
	{	perror("mprof");
		exit(1);
	}
	fread((char *) cbuf, sizeof(struct cnt), h.ncount, pfile);
	for (cp = cbuf; cp < &cbuf[h.ncount]; cp++)
	{	if (! cp->cvalue || !cp->cncall)
			continue;
		if ((m = getmod(cp->cvalue)) && (p = getproc(m, cp->cvalue)))
		{	p->p_ncall = cp->cncall;
			++ line_cnt;
		}
	}

	scale = highpc - lowpc;
	scale /= bufs/sizeof(UNIT);

	/* work up profil(2) buffer */
	i = 0;
	totime = 0.0;
	while (fread((char *) &ccnt, sizeof(ccnt), 1, pfile))
	{	if (ccnt == 0)
		{	++i;
			continue;
		}
		pcl = lowpc + scale * i;
		pch = lowpc + scale * (i+1);
		ftime = ccnt;
		totime += ftime;
		/* not perfect; ftime should be partitioned */
		if ((m = getmod(pch)) && (p = getproc(m, pch)))
		{	if (p->p_time == 0.0)
				++line_cnt;
			p->p_time += ftime;
		}
		else if ((m = getmod(pcl)) && (p = getproc(m, pcl)))
		{	if (p->p_time == 0.0)
				++line_cnt;
			p->p_time += ftime;
		}
		++i;
	}
	if (totime == 0.0)
	{	fprintf(stderr, "No time accumulated\n");
		exit(1);
	}

	if (sortflag && (lines = (struct line **)
			calloc(line_cnt, sizeof(struct line *))) == NULL)
		perror("mprof"), exit(1);
	line_cnt = 0; lp = lines;
	
	/*
	 *	printing
	 */

	if (! hflag)
		printf("%s", HEADER);
	actime = 0.0;
	for (m = mod; m-mod < a_mod; ++m)
	{	if (! m->m_ptr)
			continue;
		for (pp = m->m_ptr; pp - m->m_ptr < m->m_procs; ++pp)
		{	p = * pp;
			if (! p->p_ncall && p->p_time == 0.0)
				continue;
			ftime = p->p_time/totime;
			actime += p->p_time;
			show(m->m_name, p->p_name, 100*ftime,
				actime/tickspersec, p->p_time/tickspersec,
				p->p_ncall, p->p_time);
		}
	}

	if (sortflag)
	{	sort();
		sortflag = '\0';	/* indication for show */
		actime = 0.0;
		for (lp = lines; lp-lines < line_cnt; ++ lp)
			show((*lp)->l_mod, (*lp)->l_proc, (*lp)->l_ptime,
			     (actime += (*lp)->l_time), (*lp)->l_time,
			     (*lp)->l_calls, (*lp)->l_time * 60);
	}
}

static void show(m, p, pt, cs, s, c, t)
	char * m;	/* module name */
	char * p;	/* procedure name */
	double pt;	/* % Time */
	double cs;	/* Cumsecs */
	double s;	/* Seconds */
	long c;		/* # Calls */
	float t;	/* p->p_time */
{
	if (sortflag)
	{	if ((* lp = (struct line *) calloc(1, sizeof(struct line)))
			== NULL)
			perror("mprof"), exit(1);
		(*lp)->l_ptime = pt;
		(*lp)->l_time = s;
		(*lp)->l_calls = c;
		(*lp)->l_mod = m;
		(*lp)->l_proc = p;
		++ lp; ++ line_cnt;
	}
	else
	{	printf(FORMAT1, SHOW1(m, p, pt, cs, s));
		if (c)
			printf(FORMAT2, SHOW2(c, t/(c*tickspersec/1000.0)));
		else
			printf(FORMAT2_);
		printf(FORMAT3, SHOW3(m, p, pt, cs, s));
	}
}

static int cmp_line(l1, l2)
	struct line ** l1, ** l2;
{
	switch (sortflag)
	{	case 't':	if ((*l2)->l_time > (*l1)->l_time)
					return 1;
				else if ((*l2)->l_time < (*l1)->l_time)
					return -1;
				else
					return 0;
		case 'c':	return (*l2)->l_calls - (*l1)->l_calls;
		case 'n':
			if ((*l1)->l_mod == (*l2)->l_mod)
				return strcmp((*l1)->l_proc, (*l2)->l_proc);
			return strcmp((*l1)->l_mod, (*l2)->l_mod);
		default:
			return 0;
	}
}

static void sort()
{
	qsort(lines, line_cnt, sizeof(struct line *), cmp_line);
}
