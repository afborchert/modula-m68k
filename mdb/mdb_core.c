/*
 *	mdb - core file handling
 */

#include	"mdb.h"
#include	<stdio.h>
#if SYSV || BSD42
#include	<sys/sysmacros.h>
#include	<sys/types.h>
#if SYSV
#include	<sys/seg.h>
#endif
#include	<sys/signal.h>
#endif
#include	<sys/dir.h>
#if BSD42
#include	<machine/reg.h>
#include	<a.out.h>
#else
#include	<sys/reg.h>
#endif
#include	<sys/user.h>
#ifdef M68
#	include	<sys/core.h>
#	ifdef BSD42		/* SUN 3 */
#		include <sun3/vmparam.h>
#		if USRSTACK == 0x0E000000	/* SunOS 4.1 Bug */
#			undef USRSTACK
#			define USRSTACK 0x0E0000000
#		endif
#		define stacktop(siz) (USRSTACK)
#		define stackbas(siz) (USRSTACK - siz)
#		define TXTRNDSIZ SEGSIZ
#	endif
#	ifdef TXTALIGN
#		undef	TXTRNDSIZ
#		define TXTRNDSIZ TXTALIGN
#	endif
#else
#	include	<core.h>
#	ifdef CORE_BUG			/* only on Perkin-Elmer */
#		undef stacktop
#		undef stackbas
#		define stacktop(siz) (0xF00000 + siz)
#		define stackbas(siz) (0xF00000)
#	endif
#endif

#include	"mdb_map.h"
#include	"mdb_user.h"
#include	"mdb_config.h"

int coremode = 0;		/* is a core file opened ??? */

/*
 *	in corhdr we have the core structure of the process
 */

#if BSD42
struct core corebuf;
#endif
int _corhdr[ctob(USIZE)/sizeof(int)];
struct user * corhdr = (struct user *) _corhdr;

/*
 *	endhdr points to the saved registers
 */

int *endhdr;
static FILE *fp;
static int txtsiz, datsiz, stksiz;

struct map datmap;

/*
 *	initialisize the map above
 */

core_init(core)
	char	*core;		/* core file name */
{	int	magic;		/* magic number of core file */
	address	regbase;

	if ((fp = fopen(core, "r")) == NULL)
	{	my_perror(core);
		return;
	}
#if BSD42
	if (fread(& corebuf, sizeof(struct core), 1, fp) != 1)
	{	my_perror(core);
		fclose(fp);
		return;
	}
	magic = corebuf.c_aouthdr.a_magic;
	txtsiz = corebuf.c_tsize;
	datsiz = corebuf.c_dsize;
	stksiz = corebuf.c_ssize;
	if (magic == NMAGIC)	/* read-only text */
		datmap.b1 = N_DATADDR(corebuf.c_aouthdr);
	else
		datmap.b1 = N_TXTADDR(corebuf.c_aouthdr);
	datmap.e1 = datmap.b1 + datsiz;
	datmap.f1 = corebuf.c_len;
#else
	if ( fread(corhdr, sizeof(char), ctob(USIZE), fp) != ctob(USIZE) )
	{	my_perror(core);
		return;
	}
	magic = corhdr->u_exdata.ux_mag;
	txtsiz = ctob(corhdr->u_tsize);
	datsiz = ctob(corhdr->u_dsize);
	stksiz = ctob(corhdr->c_ssize);
	datmap.b1 = (magic==0410 || magic==0411? round(txtsiz, TXTRNDSIZ): 0);
	datmap.e1 = (magic==0410 || magic==0411? datmap.b1: txtsiz) + datsiz;
	datmap.f1 = ctob(USIZE);
#endif
#if SYSV || BSD42
	datmap.b2 = stackbas(stksiz);
	datmap.e2 = stacktop(stksiz);
#else
	datmap.b2 = corhdr->u_sseg << 16;
	datmap.e2 = datmap.b2 + stksiz;
#endif
#if BSD42
	datmap.f2 = corebuf.c_len + datsiz;
	/* following code assumes that /usr/include/machine/reg.h
	   is consistent with struct regs from /usr/include/sys/core.h
	*/
	endhdr = (int *) & corebuf.c_regs;
#else
	datmap.f2 = ctob(USIZE) + (magic == 0410 ? datsiz : datmap.e1);

	regbase =  (address) corhdr->u_ar0;
	/* map regbase to [0..ctob(USIZE)-1] */
	regbase &= AR0_MASK;
	if (USIZE == 2 && regbase < ctob(1))
		regbase += ctob(1);	/* registers in 2nd click */
	endhdr = (int *)  (( char*)corhdr  +  regbase);
#endif

	++coremode;
#ifdef DEBUG_CORE
	debug_core();
#endif
}

#ifdef DEBUG_CORE
debug_core()
{	int * ip;

	printf("debug_core...\n");
#if BSD42
	printf("\tcorhdr:\n");
	printf("\t   c_regs: %d or 0x%x\n", &corhdr->c_regs,
		&corhdr->c_regs);
	printf("\t   c_regs & AR0_MASK: %d or 0x%x\n", (int)&corhdr->u_ar0 & AR0_MASK,
		(int)&corhdr->u_ar0 & AR0_MASK);
	printf("\t   c_regs & 2047: %d or 0x%x\n", (int)&corhdr->c_regs & 2047,
		(int) &corhdr->c_regs & 2047);
#else
	printf("\tcorhdr:\n");
	printf("\t   u_ar0: %d or 0x%x\n", corhdr->u_ar0,
		corhdr->u_ar0);
	printf("\t   u_ar0 & AR0_MASK: %d or 0x%x\n", (int)corhdr->u_ar0 & AR0_MASK,
		(int)corhdr->u_ar0 & AR0_MASK);
	printf("\t   u_ar0 & 2047: %d or 0x%x\n", (int)corhdr->u_ar0 & 2047,
		(int) corhdr->u_ar0 & 2047);
	for (ip = (int *) corhdr; ip < (int *) corhdr + sizeof(_corhdr); ++ ip)
		printf("\t   corhdr[%3d]: 0x%8x %10d\n",
			ip - (int *) corhdr, * ip, * ip);
	printf("\tcorhdr -> 0x%x\n", corhdr);
	printf("\tendhdr -> 0x%x\n", endhdr);
	printf("\tdiff:     %d bytes\n", (char *) endhdr - (char *) corhdr);
#endif
	printf("end of debug_core\n");
	fflush(stdout);
}
#endif DEBUG_CORE
core_end()
{
	if (! coremode) return;
	fclose(fp);
	coremode = 0;
}

/*
 *	return value of a register
 */

getreg(roffs)
	int	roffs;
{
	return endhdr[roffs];
}

readcore(addr, value)
	address addr;
	long * value;
{
	return readm(fp, datmap, addr, value);
}
