/*
 *	see rts/check.s
 */

struct rge_err {
       int r_type;
       int r_pc;
       int r_value;
       int r_low;
       int r_high;
};

/*
 *	values of r_type
 */

#define	T_UNSIGNED	1
#define	T_SIGNED	2
#define	T_SIGN		3
#define	T_DYNARR	4

/*
 *	values of ".code"
 */

#define	E_OK	0	/* no error */
#define	E_HALT	1	/* call to procedure halt */
#define	E_CASE	2	/* no case label */
#define	E_STACK	3	/* stack overflow */
#define	E_CREND	4	/* coroutine end */
#define	E_PRIO	5	/* priority error */
#define	E_FRET	6	/* function returns no value */
#define	E_RANGE	7	/* range check fails */
