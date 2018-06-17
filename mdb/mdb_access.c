/*
 *	mdb - general read/write access to core / child process
 */

#include	<stdio.h>
#include	<assert.h>
#include	"mdb_ptrace.h"
#include	"mdb.h"

extern int pid;		/* process id of sub process */
extern int coremode;
extern int errno;

/*
 * general access routine to child process and/or core (or a.out) file
 * mode: READ or WRITE
 * addr: start address, needn't to be on a word boundary
 * space: DATA or TEXT
 * value: input or result value
 */
access(mode, addr, space, value)
	int mode;
	address addr;
	int space;
	unsigned long * value;
{	long w;
	int pmode;
#if SYSV || BSD42
	unsigned int ibuf[2];
	int rmode;
#endif

	errno = 0;
	if (pid)	/* tracing on ?? */
	{	if (mode == READ)
		{	pmode = (space == DATA) ? RDUSER : RIUSER;
#if SYSV || BSD42
			w = ptrace(pmode, pid, addr&~03, 0);
			switch (addr & 03) {
			case 1:
				w <<= 8;
				w |= (ptrace(pmode, pid, addr+3, 0) >> 24) &
				     0xff;
				break;
			case 2:
				w <<= 16;
				w |= (ptrace(pmode, pid, addr+2, 0) >> 16) &
				     0xffff;
				break;
			case 3:
				w <<= 24;
				w |= (ptrace(pmode, pid, addr+1, 0) >> 8) &
				     0xffffff;
				break;
			}
#else
			w = ptrace(pmode, pid, addr&~01, 0);
			if (addr & 01)
			{	w <<= 8;
				w |= ptrace(pmode, pid, addr+3, 0) & 0377;
			}
#endif
			*value = w;
		}
		else
		{	/* write to child process */
			assert(addr % 2 == 0);
			pmode = (space == DATA) ? WDUSER : WIUSER;
#if SYSV || BSD42
			rmode = (space == DATA)? RDUSER: RIUSER;
			if (addr % 4 == 0)
				ptrace(pmode, pid, addr, * value);
			else
			{
#if PTRACEX
				ptracex(rmode,
					pid, addr & ~03, ibuf, sizeof(ibuf));
#else
				ibuf[0] = ptrace(rmode, pid, addr & ~03, 0);
				ibuf[1] = ptrace(rmode, pid, addr+2, 0);
#endif
				ibuf[0] &= 0xffff0000;
				ibuf[1] &= 0xffff;
				ibuf[0] |= ((* value) >> 16) & 0xffff;
				ibuf[1] |= ((* value) << 16) & 0xffff0000;
#if !PTRACEX || PTRACEX_BUG	/* ptracex doesn't work for pmode = 4, 5 */
				ptrace(pmode, pid, addr & ~03, ibuf[0]);
				ptrace(pmode, pid, addr+2, ibuf[1]);
#else
				ptracex(pmode, pid, addr & ~03, ibuf,
					sizeof(ibuf));
#endif
			}
#else
			ptrace(pmode, pid, addr, *value);
#endif
		}
	}
	else		/* reading from core or a.out file */
	{	assert(mode == READ);
		if (coremode)
			return readcore(addr, value);
		else
			return readaout(addr, value);
	}
	if (errno)
	{	my_perror("access");
		message("access failed: mode=%s, addr=%x, space=%s",
			mode==READ? "READ": "WRITE", addr,
			space==DATA? "DATA": "TEXT");
		if (mode == READ)
			*value = 0;
		return 1;
	}
	return 0;	/* ok */
}

/*
 * some easy-use access routines
 */
getlong(addr, value)
	address addr;
	long * value;
{
	return access(READ, addr, DATA, value);
}

putlong(addr, value, space)
	address addr;
	long value;
	int space;
{
	return access(WRITE, addr, space, &value);
}

/*
 * get a multiple of long (size must be a multiple of long)
 */
get(addr, ptr, size)
	address addr;
	long * ptr;
	unsigned size;
{

	while(size > 0)
	{	if (access(READ, addr, DATA, ptr))
			return 1;
		size -= sizeof(long);
		addr += sizeof(long);
		++ptr;
	}
	return 0; /* ok */
}
