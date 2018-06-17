/*
 *	mprof -- global types
 */

#include "mp_config.h"

#define	NAMSIZ	14

typedef unsigned int address;
typedef unsigned int val;

#if BSD42
#define FILHSZ  sizeof(struct exec)
#define SYMESZ  sizeof(struct nlist)
#define SYMNMLEN 8
#define N_SO 0x64
#define N_FUN 0x24
#define N_SLINE 0x44
#define N_ENTRY 0xa4
#define SYMNMLEN 8
#endif

struct proc {
	char p_name[NAMSIZ];
	int p_num;
	unsigned p_addr;
	float p_time;
	long p_ncall;
};

struct module {
	char * m_name;
	unsigned m_entry;
	int m_procs; /* # procedures */
	struct proc ** m_ptr;
#ifdef SYSV
	char * m_file; 	  /* source file of module */
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
#endif SYSV
};

#ifdef SYSV
#define	m_pchain	m_procs_._p_chain
#define	m_pfield	m_procs_._p_field
#endif

#ifndef SYSV
#define	void int
#endif

