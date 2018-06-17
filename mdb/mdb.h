/*
 *	global definitions
 *	mdb
 */

#ifndef GLOBDEF_DEF
#define	GLOBDEF_DEF

#include "mdb_config.h"

typedef unsigned long address;
typedef unsigned val;

#define	READ	1
#define	WRITE	2

#define	TEXT	1
#define	DATA	2

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

#if SYSV && PE3200
#define	NIL	(0xefffff)
#else
#define	NIL	(-1)
#endif

#endif

#if !SYSV
#define void int
#endif

#ifdef DEBUG
extern int debug;
#define Debug	(!debug)?0:printf
#else
#define	Debug	(1)?0:
#endif
