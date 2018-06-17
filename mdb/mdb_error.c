/*
 *	mdb -- get run time error information
 */

#include	"mdb.h"
#include	<stdio.h>
#if BSD42
#include	<machine/reg.h>
#else
#include	<sys/reg.h>
#endif
#include	"mdb_error.h"
#include	"mdb_instr.h"

#if PE
extern int disadm();		/* see mdb_opset.c */

extern struct instr instr;	/* see mdb_opset.c */
extern int reglist[];		/* see mdb_process.c */
#define REGLISTI(i)	17-(i)	/* register number -> index into reglist */
#endif

extern unsigned code_addr;
extern unsigned rge_err;
#if SYSV || BSD42
extern unsigned top_addr;
extern unsigned base_addr;
#endif
extern unsigned version;
extern unsigned pc;
extern unsigned e_base;
extern unsigned e_top;

static char * range_err();
static char * othererror();

char * error_mess()
{	long err;
	char * str;
	int top, limit;

#if PE3200
	pc = getreg(R15);
#endif
#if M68
	if (rge_err)
		getlong(rge_err+4, & pc);
	else
		pc = 0;
	if (! pc)
		pc = getreg(PC);
#endif
	if (pc % 2 || pc < 256)
		pc = getreg(PC);
#if PE3200
	if (version == 1 || version == 2)
		return NULL;
#endif
	top = getreg(SP);
	limit = getreg(R2);
	if (code_addr)
		getlong(code_addr, & err);
	else
		err = E_OK;
#if SYSV || BSD42
	if (base_addr)
		getlong(base_addr, & e_base);
	if (top_addr)
		getlong(top_addr, & e_top);
#endif
	if (err == E_OK && (str = othererror(pc, top)))
		return str;
	switch (err)
	{	case E_OK : str = "no error code"; pc = getreg(PC); break;
		case E_HALT : str = "call to procedure HALT"; break;
		case E_CASE : str = "attempt to find case failed"; break;
		case E_STACK : str = "stack overflow"; break;
		case E_CREND : str = "coroutine end"; break;
		case E_PRIO : str = "priority error"; break;
		case E_FRET : str = "function does not return any value"; break;
		case E_RANGE :
			str = range_err();
			break;
		default :
#if !(SYSV || BSD42)
			if (top >= limit)
				str = "stack overflow: top >= limit";
			else
#endif SYSV
				str = "undefined error code";
			pc = getreg(PC);
			break;
	}
	return str;
}

static char * range_err()
{	static char buf[80];
	struct rge_err range_err;
	unsigned addr;

	/*
	 *	read "rge_err" structure
	 */

	addr = rge_err;
        getlong(addr, & range_err.r_type);
	getlong(addr += 4, & range_err.r_pc);
	getlong(addr += 4, & range_err.r_value);
	getlong(addr += 4, & range_err.r_low);
	getlong(addr += 4, & range_err.r_high);


	pc = range_err.r_pc;
	switch (range_err.r_type)
	{	case T_UNSIGNED:
			sprintf(buf, "CARDINAL %lu out of range [%lu..%lu]",
				range_err.r_value, range_err.r_low,
				range_err.r_high);
			break;
		case T_SIGNED:
			sprintf(buf, "INTEGER %ld out of range [%ld..%ld]",
				range_err.r_value, range_err.r_low,
				range_err.r_high);
			break;
		case T_SIGN:
			sprintf(buf, "INTEGER/CARDINAL conflict");
			break;
		case T_DYNARR:
			sprintf(buf, "index beyond bounds of dynamic array");
			break;
		default:
			sprintf(buf, "undefined range error code");
			pc = 0;
			break;
	}
	return buf;
}

static char * othererror(pc, top)
	address pc, top;
{	val ins;
	int len;
	struct instr ibuf;
	address addr;

#if PE
	getlong(pc, & ins);
	if (! (len = disasm(pc, TEXT, ins)))
		return NULL;
	ibuf = instr;
	if (len != disasm(pc, DATA, ins))
		return "instruction in core has been overwritten";
	if (ibuf.type == RX1 || ibuf.type == RX2 || ibuf.type == RX3)
	{	addr = ibuf.disp + getreg(reglist[REGLISTI(ibuf.reg[1])]);
		if (ibuf.type == RX3)
			addr += getreg(reglist[REGLISTI(ibuf.reg[2])]);
		if (addr == NIL)
			return "reference through NIL";
#if PE3200 && SYSV
		if (addr >= USRSTACK*NBPS + MAXSTACK*NBPC)
			return "address beyond maximal stack size";
#endif
	}
#endif
	return NULL;
}
