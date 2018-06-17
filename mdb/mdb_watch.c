/*
 * mdb -- watch dogs
 * afb 2/88
 */

#include "mdb.h"

extern char * malloc();
extern void message();
extern int getlong();

extern int pid;
extern int signo;

static show_wd();
static struct watchdog * lookfor();

static struct watchdog {
	address wd_loc;		/* location of watch dog */
	val wd_val;		/* value to be looked for or old value */
	short wd_flags;
	struct watchdog * wd_link;
} * wdlist = NULL;

#define	WD_VAL	0x01		/* look for given value */
#define WD_CHG	0x02		/* look for changing value */
#define WD_SLP  0x04		/* sleeping watchdog? */

new_wd(loc, chg, value)
	address loc;
	int chg;
	val value;
{	struct watchdog * wdp;
	val current_val;

	if (! (wdp = lookfor(loc)))
	{	if (! (wdp = (struct watchdog *)
				malloc(sizeof(struct watchdog))))
		{	message("No space available");
			return;
		}
		wdp->wd_link = wdlist;
		wdlist = wdp;
	}
	if (getlong(loc, & current_val) && pid)
	{	message("cannot access %xH", loc);
		return;
	}
	else
		current_val = 0;
	wdp->wd_loc = loc;
	wdp->wd_flags = 0;
	if (chg)
	{	wdp->wd_val = current_val;
		wdp->wd_flags |= WD_CHG;
	}
	else
	{	wdp->wd_val = value;
		if (current_val == value)
			wdp->wd_flags |= WD_SLP;
		wdp->wd_flags |= WD_VAL;
	}
	show_wd(wdp);
}

/*
 * return 1, if any dog is barking...
 */
int check_wds()
{	struct watchdog * wdp;
	val current_val;
	int rval = 0;

	for (wdp = wdlist; wdp; wdp = wdp->wd_link)
	{	if (WD_SLP & wdp->wd_flags)
			continue;
		getlong(wdp->wd_loc, & current_val);
		if (WD_CHG & wdp->wd_flags && current_val != wdp->wd_val ||
		    WD_VAL & wdp->wd_flags && current_val == wdp->wd_val)
		{	printf("[watch dog at loc %xH: new value = %xH]\n",
				wdp->wd_loc, current_val);
			wdp->wd_flags |= WD_SLP;
			rval = 1;
		}
	}
	return rval;
}

init_wds()
{	struct watchdog * wdp;
	val current_val;

	for (wdp = wdlist; wdp; wdp = wdp->wd_link)
	{	getlong(wdp->wd_loc, & current_val);
		if (WD_CHG & wdp->wd_flags)
			wdp->wd_val = current_val;
		wdp->wd_flags |= ~WD_SLP;
	}
}

show_wds()
{	struct watchdog * wdp;

	for (wdp = wdlist; wdp; wdp = wdp->wd_link)
		show_wd(wdp);
}

run_wds()
{
	if (pid)
		cont(0, 1);
	else
		run(1);
	while (pid && ! check_wds() && ! signo)
		cont(0, 1);
}

/*
 * private routines
 */
static show_wd(wdp)
	struct watchdog * wdp;
{	val current_val;

	if (getlong(wdp->wd_loc, & current_val))
		current_val = 0;
	printf("[%6xH] %08xH ", wdp->wd_loc, current_val);
	if (WD_CHG & wdp->wd_flags)
		if (pid && wdp->wd_val != current_val)
			printf("!%08xH ", wdp->wd_val);
		else
			printf("?********H ");
	else
		printf("?%08xH ", wdp->wd_val);
	if (WD_SLP & wdp->wd_flags)
		printf("sleeping\n");
	else
		printf("awake\n");
}

static struct watchdog * lookfor(addr)
	address addr;
{	struct watchdog * wdp;

	for (wdp = wdlist; wdp; wdp = wdp->wd_link)
		if (wdp->wd_loc == addr)
			return wdp;
	return wdp;
}
