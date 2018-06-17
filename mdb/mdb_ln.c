/*
 *	line number handling
 *
 *	afb 3/85
 */

#include	<stdio.h>
#include	"mdb.h"

extern char * calloc();

/*
 *	exports
 *
 *	enter_ln(address, linenumber)		(* called from mdb_aout.c *)
 *
 *	unsigned get_ln(address)
 */

/*
 *	line number structure
 *	will be allocated while reading the a.out file
 */

struct ln {
	unsigned ln_addr;	/* address of line number label */
	unsigned ln_number;	/* line number */
	struct ln * ln_link;
};

static struct ln * ln_list = NULL;
static int count = 0;

enter_ln(address, linenumber)
	unsigned address, linenumber;
{	struct ln * new;

	if (ln_list && ln_list->ln_addr == address)
	{	ln_list->ln_number = linenumber;
		return;
	}
	if ((new = (struct ln *) calloc(1, sizeof(struct ln))) == NULL)
	{	static int nospace = 0;

		if (! nospace)
		{	++nospace;
			my_perror("enter_ln");
		}
		return;
	}
	new->ln_addr = address;
	new->ln_number = linenumber;
	new->ln_link = ln_list;
	++ count;
	ln_list = new;
}

static struct list {
	unsigned address;
	unsigned number;
} * list;

/*
 *	return -1 on failure
 */

unsigned get_ln(address)
	unsigned address;
{	static int init = 0;
	int index, leftindex, rightindex;
	int cmp;

	if (! count)
		return -1;
	if (! init)
	{	l_init();
		init = 1;
		if (! count)	/* no space available */
			return -1;
	}

	/* binary search */
	leftindex = 0;
	rightindex = count-1;
	while (leftindex <= rightindex)
	{	index = (leftindex + rightindex + 1) / 2; /* round up ! */
		cmp = address - list[index].address;
		if (cmp <= 0)
			rightindex = index - 1;
		if (cmp >= 0)
			leftindex = index + 1;
	}
	if (cmp <= 0 && index > 0)
		-- index;
	return list[index].number;
}

static int cmp_list(l, r)
	struct list * l, * r;
{
	return l->address - r->address;
}

static l_init()
{	int index;
	struct ln * ptr;

	if ((list = (struct list *) calloc(count, sizeof(struct list))) == NULL)
	{	count = 0;
		free_list();
		return;
	}
	ptr = ln_list;
	for (index = 0; index < count; ++index)
	{	list[count-index-1].address = ptr->ln_addr;
		list[count-index-1].number = ptr->ln_number;
		ptr = ptr->ln_link;
	}
	qsort(list, count, sizeof(struct list), cmp_list);
	free_list();
}

static free_list()
{	struct ln * ptr;

	while (ptr = ln_list)
	{	ln_list = ln_list->ln_link;
		cfree(ptr);
	}
}
