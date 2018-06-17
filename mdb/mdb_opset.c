#if PE
/*
 *	source from 'adb' UNIX Edition VII
 *	modified for mdb - afb 5/86
 */

#include <stdio.h>
#include "mdb.h"
#include "mdb_instr.h"
#include "mdb_ref.h"
#include "mdb_tree.h"

#define	printf	textout

/*
 *	exported vars
 */

address idot;		/* dot of current instruction */
int idotinc;		/* dot increment = size of current instruction */
struct instr instr;	/* struct for information about disassembled instr */
char tobeprinted[256];	/* result of printins() */

/*
 *	imported vars
 */

extern struct module mod[];	/* see mdb_aout.c */
extern int a_mod;
#if !(SYSV || BSD42)
extern address glist[];		/* see mdb_aout.c */
#endif

/*
 *	imported funcs
 */

struct module * getmod();
tree * getproc();
void readaout();
address procentry();
tree * build();

/*
 *	local funcs
 */

static unsigned int get();
static unsigned short getsh();
static void prints();
static char * rxrxop();
static void psymoff();
static char * localname();

/*
 * Perkin-Elmer opcode mnemonics & types
 */
static struct {
	char *opmnem;
	int  optype;
} optab[] = {
	 0,	0,
	"balr",	RR,
	"btcr",	BT+RR,
	"bfcr",	BF+RR,
	"nr",	RR,
	"clr",	RR,
	"or",	RR,
	"xr",	RR,
	"lr",	RR,
	"cr",	RR,
	"ar",	RR,
	"sr",	RR,
	"mhr",	RR,
	"dhr",	RR,
	 0,	0,
	 0,	0,
	"srls",	SF,
	"slls",	SF,
	"chvr",	RR,
	"lper", RR,
	 0,	0,
	"lger", RR,
	"lgdr", RR,
	"lcer", RR,
	"lpsr",	RR,
	 0,	0,
	 0,	0,
	 0,	0,
	"mr",	RR,
	"dr",	RR,
	 0,	0,
	 0,	0,
	"btbs",	BT+BBACK+SF,
	"btfs",	BT+SF,
	"bfbs",	BF+BBACK+SF,
	"bffs",	BF+SF,
	"lis",	SF,
	"lcs",	SF,
	"ais",	SF,
	"sis",	SF,
	"ler",	RR,
	"cer",	RR,
	"aer",	RR,
	"ser",	RR,
	"mer",	RR,
	"der",	RR,
	"fxr",	RR,
	"flr",	RR,
	 0,     0,
	 0,     0,
	"pbr",  RR,
	"lpdr", RR,
	"exhr",	RR,
	 0,	0,
	 0,	0,
	"lcdr", RR,
	"ldr",  RR,
	"cdr",  RR,
	"adr",  RR,
	"sdr",  RR,
	"mdr",  RR,
	"ddr",  RR,
	"fxdr", RR,
	"fldr", RR,
	"sth",	RX1,
	"bal",	BAL+RX1,
	"btc",	BT+RX1,
	"bfc",	BF+RX1,
	"nh",	RX1,
	"clh",	RX1,
	"oh",	RX1,
	"xh",	RX1,
	"lh",	RX1,
	"ch",	RX1,
	"ah",	RX1,
	"sh",	RX1,
	"mh",	RX1,
	"dh",	RX1,
	 0,	0,
	 0,	0,
	"st",	RX1,
	"am",	RX1,
	 0,     0,
	 0,     0,
	"n",	RX1,
	"cl",	RX1,
	"o",	RX1,
	"x",	RX1,
	"l",	RX1,
	"c",	RX1,
	"a",	RX1,
	"s",	RX1,
	"m",	RX1,
	"d",	RX1,
	"cr12",	RX1,
	"cr16",	RX1,
	"ste",	RX1,
	"ahm",	RX1,
	"pb",   RX1,
	"lra",  RX1,
	"atl",	RX1,
	"abl",	RX1,
	"rtl",	RX1,
	"rbl",	RX1,
	"le",	RX1,
	"ce",	RX1,
	"ae",	RX1,
	"se",	RX1,
	"me",	RX1,
	"de",	RX1,
	"stbp", RX1,
	"lpb",  RX1,
	"std",  RX1,
	"stme",	RX1,
	"lme",	RX1,
	"lhl",	RX1,
	"tbt",	RX1,
	"sbt",	RX1,
	"rbt",	RX1,
	"cbt",	RX1,
	"ld",   RX1,
	"cd",   RX1,
	"ad",   RX1,
	"sd",   RX1,
	"md",   RX1,
	"dd",   RX1,
	"stmd", RX1,
	"lmd",  RX1,
	 0,	0,
	 0,	0,
	"stde", RX1,
	 0,	0,
	"led",  RX1,
	 0,	0,
	 0,	0,
	"lde",  RX1,
	"brk",  SF,
	 0,	0,
	 0,	0,
	 0,	0,
	"",     RXRX+RX1,
	 0,	0,
	 0,	0,
	 0,	0,
	"srhls",SF,
	"slhls",SF,
	"stbr",	RR,
	"lbr",	RR,
	"exbr",	RR,
	"epsr",	RR,
	"wbr",	RR,
	"rbr",	RR,
	"whr",	RR,
	"rhr",	RR,
	"wdr",	RR,
	"rdr",	RR,
	 0,	0,
	"ssr",	RR,
	"ocr",	RR,
	 0,	0,
	 0,	0,
	 0,	0,
	 0,	0,
	 0,	0,
	"ledr", RR,
	"legr", RR,
	"ldgr", RR,
	"lder", RR,
	 0,	0,
	 0,	0,
	 0,	0,
	 0,	0,
	 0,	0,
	 0,	0,
	 0,	0,
	 0,	0,
	 0,	0,
	 0,	0,
	 0,	0,
	 0,	0,
	 0,	0,
	 0,	0,
	 0,	0,
	 0,	0,
	 0,	0,
	 0,	0,
	 0,	0,
	 0,	0,
	 0,	0,
	 0,	0,
	 0,	0,
	 0,	0,
	"bxh",	RX1,
	"bxle",	RX1,
	"lpsw",	RX1,
	"thi",	RI1,
	"nhi",	RI1,
	"clhi",	RI1,
	"ohi",	RI1,
	"xhi",	RI1,
	"lhi",	RI1,
	"chi",	RI1,
	"ahi",	RI1,
	"shi",	RI1,
	"srhl",	RI1,
	"slhl",	RI1,
	"srha",	RI1,
	"slha",	RI1,
	"stm",	RX1,
	"lm",	RX1,
	"stb",	RX1,
	"lb",	RX1,
	"clb",	RX1,
	"al",	RX1,
	"wb",	RX1,
	"rb",	RX1,
	"wh",	RX1,
	"rh",	RX1,
	"wd",	RX1,
	"rd",	RX1,
	 0,	0,
	"ss",	RX1,
	"oc",	RX1,
	"psf",  RX1,
	"ts",	RX1,
	"svc",	RX1,
	"sint",	RX1,
	"scp",	RX1,
	 0,	0,
	"bdcs", RX1,
	"la",	RX1,
	"tlat",	RX1,
	"rwdcs",RR,
	"ecs",  RI1,
	"rrl",	RI1,
	"rll",	RI1,
	"srl",	RI1,
	"sll",	RI1,
	"sra",	RI1,
	"sla",	RI1,
	 0,	0,
	 0,	0,
	 0,	0,
	"ti",	RI2,
	"ni",	RI2,
	"cli",	RI2,
	"oi",	RI2,
	"xi",	RI2,
	"li",	RI2,
	"ci",	RI2,
	"ai",	RI2,
	"si",	RI2,
	 0,	0,
	 0,	0,
	 0,	0,
	 0,	0
};

/*
 * Branch condition names
 */
struct {
	char *true;
	char *false;
} brtab[] = {
	"nop",	"b",
	"bm",	"bnm",
	"bp",	"bnp",
	"bne",	"be",
	"bo",	"bno",
	0,	0,
	0,	0,
	0,	0,
	"bl",	"bnl",
	0,	0,
	0,	0,
	0,	0,
	0,	0,
	0,	0,
	0,	0,
	0,	0
};

/*
 * Names of registers
 */
static char *regname[16] = {
	"r0", "r1", "limit", "r3", "r4", "r5", "r6", "r7",
	"r8", "r9", "ra", "rb", "rc", "top", "base", "rf"
};

static void textout(fmt, p1, p2, p3, p4, p5)
{	static char buf[128];

	sprintf(buf, fmt, p1, p2, p3, p4, p5);
	strcat(tobeprinted, buf);
}

/*
 * Disassemble instruction at loc in space idsp, whose first word is in ins
 * Return length of instruction
 */
int disasm(loc, idsp, ins)
	register address loc;
	int idsp;
	unsigned int ins;
{
	instr.op = ins>>24;
	instr.reg[0] = (ins>>20)&017;
	instr.reg[1] = (ins>>16)&017;
	instr.type = optab[instr.op].optype;
	switch (instr.type & 017) {

	case SF:
	case RR:
		return(2);

	case RI1:
		instr.disp = ins&0xffff;
		/* Propagate sign */
		if (ins&0x8000)
			instr.disp |= (-1)<<16;
		return(4);

	case RI2:
		instr.disp = get(loc+2, idsp);
		return(6);

	case RX1:
		ins = get(loc+2, idsp);
		switch(ins>>30) {

		/* RX2 -- negative offset */
		case 3:
			instr.type += RX2-RX1;
			instr.disp = ((-1)<<16) | (unsigned)ins>>16;
			instr.disp += loc + 4;
			return(4);
	
		/* RX2 -- positive offset */
		case 2: 
			instr.type += RX2-RX1;
			instr.disp = (ins>>16) & 0x7fff;
			instr.disp += loc + 4;
			return(4);
	
		/* RX1 */
		case 0:
			instr.disp = (unsigned)ins>>16;
			return(4);
	
		/* RX3 */
		case 1:
			instr.type += RX3-RX1;
			instr.reg[2] = (ins>>24) & 017;
			ins &= 0xffffff;
			if (ins & 0x800000)	/* negative offset */
				ins |= (0xff<<24);
			instr.disp = ins;
			return(6);
		}

	default:
		return(0);
	}
}

/*
 * Print instruction at idot in space idsp, whose first word has been fetched
 * into ins, in assembly format
 */

void printins(/* f,??? */ idsp, ins)
	int idsp;
	register ins;
{
	register int n;
	register int type;
	register int opmod;

	tobeprinted[0] = '\0';
	if ((idotinc = disasm(idot, idsp, ins)) == 0) {	/* illegal opcode */
		prints("???");
		idotinc = 2;
		return;
	}

	type = instr.type;
	n = instr.reg[0];

	if (type & RXRX) {                             /* RX-RX */
		opmod = (unsigned short) getsh(idot+idotinc, idsp) >> 8;
		printf("%s\t", rxrxop(opmod));
		if (opmod & 0x80)
			printf("=%d,", n);
		else
			printf("%s,", regname[n]);
	} else if ((type & (BT|BF)) == 0)               /* non-branch */
		printf("%s\t%s,", optab[instr.op].opmnem, regname[n]);
	else if (type&BT && (n = (int) brtab[n].true) == 0 ||
	    type&BF && (n = (int) brtab[n].false) == 0) {	/* branch */
		printf("%s\t%d,", optab[instr.op].opmnem, instr.reg[0]);
	} else {					/* extended branch */
		prints(n);
		switch (type & 017) {

		case RR:
			printf("r\t");
			break;

		case RX1:
		case RX2:
		case RX3:
			printf("\t");
			break;

		case SF:
			printf("s\t");
			n = instr.reg[1]<<1;
			instr.reg[1] = 0;
			if (type & BBACK)
				instr.disp = idot - n;
			else
				instr.disp = idot + n;
			type = (type & ~SF) | RX1;
			break;
		}
	}

	printdisp(type);

	if (type & RXRX) {
		ins = (ins & 0xff000000) | (get(idot+idotinc, idsp) & 0xffffff);
		idotinc += disasm(idot+idotinc, idsp, ins);
		type = instr.type;
		n = instr.reg[0];
		if (opmod & 0x40)
			printf(",=%d,", n);
		else
			printf(",%s,", regname[n]);
		printdisp(type);
	}
}

/*
 * Print operand part of instruction
 */
printdisp(type)
{
	register n;

	switch (type & 017) {

	case RR:
		prints(regname[instr.reg[1]]);
		break;

	case SF:
		psymoff(instr.reg[1], NSYM);
		break;

	case RI1:
	case RI2:
	case RX1:
	case RX2:
		psymoff(instr.disp, type&(BT|BF|BAL)? ISYM : DSYM);
		if (n = instr.reg[1])
			printf("(%s)", regname[n]);
		break;

	case RX3:
		psymoff(instr.disp, type&(BT|BF|BAL)? ISYM : DSYM);
		if (n = instr.reg[1]) {
			printf("(%s", regname[n]);
			if (n = instr.reg[2])
				printf(",%s", regname[n]);
			prints(")");
		}
		break;

	}
}

static void psymoff(disp, symtype)
	address disp;
	int symtype;
{	struct module * mod;
	tree * t;
	address pentry;
	char * varname;

	if (symtype == ISYM && (mod = getmod(disp)) &&
		(t = getproc(mod, disp)))
	{	printf("%s.%s", mod->m_name, t->t_name);
		if (disp != (pentry = procentry(mod, t->t_pnum)))
			printf("+#%x", disp-pentry);
	}
	else if (symtype == DSYM && (varname = localname(disp)))
		printf("%s", varname);
	else
		printf("#%x", disp);
}

static char * localname(disp)
	int disp;	/* not address */
{	struct module * m;
	struct module * mp;
	struct module * lm;
	tree * t;
	tree * varlist;
	char buf[63];

	if (! (m = getmod(idot)) || ! (t = getproc(m, idot)))
		return NULL;
	if (instr.reg[1] == /* base = */ 14 && instr.reg[2] == 0)
		for (varlist = t->t_son; varlist; varlist = varlist->t_left)
		{	if (varlist->t_type != T_VAR ||
				varlist->t_addrmode != Rel)
				continue;
			if (varlist->t_address - t->t_parlength == disp)
				return varlist->t_name;
		}
	else if (instr.reg[1] == 0 && disp)
	{	lm = NULL;
#if SYSV || BSD42
		for (mp = mod; mp-mod < a_mod; ++ mp)
		{	if (! mp->m_global)
				continue;
			if (disp < mp->m_global)
			{	mp = lm;
				break;
			}
			else if (disp == mp->m_global)
				break;
			lm = mp;
		}
#else
		for (mp = mod; mp-mod < a_mod; ++ mp)
		{	if (! glist[mp-mod])
				continue;
			if (disp < glist[mp-mod])
			{	mp = lm;
				break;
			}
			else if (disp == glist[mp-mod])
				break;
			lm = mp;
		}
#endif
		if (mp && mp-mod < a_mod)
		{	if (! mp->m_ptr && !(mp->m_ptr = build(mp->m_name)))
				return NULL;
			for (varlist = mp->m_ptr->t_son; varlist;
				varlist = varlist->t_left)
			{	if (varlist->t_type != T_VAR ||
					varlist->t_addrmode != Rel)
					continue;
#if SYSV || BSD42
				if (varlist->t_address + mp->m_global == disp)
#else
				if (varlist->t_address + glist[mp-mod] == disp)
#endif
					if (mp == m)
						return varlist->t_name;
					else
					{	strcpy(buf, mp->m_name);
						strcat(buf, ".");
						strcat(buf, varlist->t_name);
						return buf;
					}
			}
		}
	}
	return NULL;
}

/*
 * Generate name of RX/RX Function
 */
static char * rxrxop(opmod)
{
	switch (opmod & 0x3f) {
		case 0x00:      return("mvtu");
		case 0x01:      return("move");
		case 0x02:      return("cpan");
		case 0x03:      return("pmv");
		case 0x04:      return("umv");
		case 0x21:      return("movep");
		case 0x22:      return("cpanp");
		case 0x23:      return("pmva");
		case 0x24:      return("umva");
		default:        return("rxrx");
	}
}

static unsigned int get(addr, space)
	address addr;
	int space;
{	val value;

	if (space == DATA)
		access(READ, addr, space, & value);
	else
		readaout(addr, & value);
	return value;
}

static unsigned short getsh(addr, space)
	address addr;
	int space;
{	return (short) get(addr, space);
}

static void prints(s)
	char * s;
{	printf("%s", s);
}
#endif
