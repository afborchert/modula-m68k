/*
 *	mdb - read map-dependent
 */

#include	<stdio.h>
#include	"mdb.h"
#include	"mdb_map.h"

/*
 *	set value to the contents of specified address
 *	(for text and data segment)
 *	return -1 on failure
 *		0 if ok
 */

readm(fp, mp, addr, value)
	FILE *fp;
	struct map mp;
	address addr;
	long *value;	/* OUT-parameter */
{	struct map * m = &mp;

	if ( within(m->b1, addr, m->e1) )
		*value = readlong(fp, addr + m->f1 - m->b1);
	else if ( within(m->b2, addr, m->e2) )
		*value = readlong(fp, addr + m->f2 - m->b2);
	else
		return -1;
	return 0;
}

/* check:  low <= in < high */

within(low, in, high)
	address low, in, high;
{
	return low <= in && in < high;
}

long readlong(fp, offset)
	FILE *fp;
	address offset;
{	long value;

	if (fseek(fp, offset, 0))
	{	my_perror("fseek");
		return 0L;
	}
	if (fread(&value, sizeof(long), 1, fp) != 1)
		my_perror("read access");
	return value;
}

/* the result should be a multiple of `b' */

long round(a,b)
long a, b;
{	long w;

	w = ((a+b-1)/b)*b;
	return w;
}
