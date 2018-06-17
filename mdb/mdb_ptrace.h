/*
 * ptrace requests
 */
#define	SETTRC	0	/* child request for being traced */
#define	RIUSER	1	/* read from child process (text segment) */
#define	RDUSER	2	/* read from child process (data segment) */
#define	WDUSER	5	/* write to child process (data segment) */
#define WIUSER	4	/* write to child process (text segment) */
#define	RUREGS	3	/* read from user structure */
#define	WUREGS	6	/* write to user structure */
#define	CONTIN	7	/* continue execution of child */
#define	EXIT	8	/* exit() for child */
#define	SINGLE	9	/* single step execution of child */
