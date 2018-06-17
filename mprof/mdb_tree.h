/*
 *	mdb - tree structure
 */

#include "mdb.h"
#define	NAMSIZ	14

struct tr {
	char t_type;
	char t_name[NAMSIZ];
	word t_line;			/* line number in source */

	word t_pnum;			/* procedure number, if any */
	word t_parlength;		/* length of par field in bytes */
	char t_active;
	long t_base;			/* only if t_active == TRUE */
	int t_plevel;			/* procedure nest level */
	struct abp * t_bp[3];		/* breakpoints */

	word t_vartype;			/* type of variable, if any */
	word t_addrmode;		/* only for variables */
	word t_address;			/*    - " " - */
	word t_size;			/*    - " " - */

	/* links */

	struct tr *t_link;		/* link chain through all nodes */
	struct tr *t_father;
	struct tr *t_son;
	struct tr *t_right;
	struct tr *t_left;
	};

typedef struct tr tree;

/* values of t_type : */

#define	T_MOD	1
#define	T_PROC	2
#define	T_VAR	3

struct module {
	char * m_name;	  /* module name (max. 8(Ed.VII)/24(XELOS) letters) */
	char m_bpset;	  /* -> mdb_brkpt.c */
	address m_entry;  /* address of external module symbol */
	char * m_file; 	  /* source file of module */
#if BSD42 || SYSV
	address m_global; /* address of global variables */
	int m_proccnt;	  /* # procedures */
	union {
		struct pchain {
			int pc_num;	/* procedure number */
			address pc_entry;
			struct pchain * pc_link;
		} * _p_chain;
		struct pfield {
			address pf_entry;
		} * _p_field;
	} m_procs_;
#endif xelos
	tree *m_ptr;
	};

#if BSD42 || SYSV
#define	m_pchain	m_procs_._p_chain
#define	m_pfield	m_procs_._p_field
#endif
