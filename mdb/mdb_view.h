/*
 *	mdb - view structure
 */

#define	LINSIZ	79

struct t_view {
	char v_line[LINSIZ];	/* output text */
	struct module *v_mod;	/* for the P-window */
	tree *v_node;		/* for the D-window */
	long v_base;		/* for P/D switches */
	long v_addr;		/* for the C-window */
	long v_size;
	int v_lineno;		/* line number in source text */
	int v_flags;
	};
typedef struct t_view view;

#define V_START	01		/* first line of window */
#define	V_END	02		/* last line of window */
#define	V_MOVE	04		/* move dot to this line */
