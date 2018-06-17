/*
 *	breakpoint structures
 */

#include "mdb.h"

/*
 *	this structure will be allocated while reading the a.out file
 */

struct bp {
       short bp_proc;	/* procedure number */
       char bp_flag;	/* 0: "begin"-point, 1: "end"-point */
       address bp_addr;/* breakpoint address */
#if    BSD42 || SYSV 
       struct module * bp_mod;
#endif
       struct bp * bp_ptr; /* link */
};

/*
 *	this structure will be used for (potencial) active breakpoints
 */

struct abp {
	address abp_loc;	/* breakpoint address */
	int abp_inst;		/* saved instruction at abp_loc */
	int abp_flag;		/* bitfield, see below */
	int abp_count;
	int abp_initcnt;
	struct abp * abp_link;	/* link throug all abp-nodes */
};

/*
 *	flags
 */

#define	ABP_ACTIVE	01	/* breakpoint set ??? */
#define	ABP_PASSED	02
#define	ABP_TOSET	04
#define	ABP_ERROR	010
#define	ABP_EXEC	020
#define	ABP_ALWAYS	040
#define	ABP_SKIP	0100
