/*
 *	mdb - instruction structure
 */

struct instr {
	int type;	/* instruction format */
	char op;	/* op-code */
	char reg[3];	/* registers */
	int disp;	/* displacement */
};

/*
 * instruction formats
 */
#define	SF	1
#define RR      2
#define	RX1	3
#define	RX2	4
#define	RX3	5
#define	RI1	6
#define	RI2	7

#define	BT	0x10	/* branch instruction (true)	*/
#define	BF	0x20	/* branch instruction (false)	*/
#define	BBACK	0x40	/* branch backwards short	*/
#define BAL	0x80	/* branch & link */
#define RXRX    0x100   /* RX/RX format                 */


#if neverdefined
typedef struct {
	int mode;
	int reg;
} effaddr;		/* effective address */

struct instr {
	int size;	/* size attribute */
	int opset;	/* set of operands */
	effaddr src;	/* source */
	effaddr dest;	/* destination */
};

/* opset */

#define S_EA	01
#define	S_DEST	02
#define	S_SRC	04
#define	S_REG1	0100

/* size attribute */

#define S_BYTE	1
#define S_WORD	2
#define	S_LONG	4
#define	NO_SIZE	0

#endif

/* symbol types */

#define	NSYM	1
#define	ISYM	2
#define	DSYM	3
