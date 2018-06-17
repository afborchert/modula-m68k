/*
 *	the field names are taken from adb :
 *
 *	b1, e1, f1 : text segment
 *	b2, e2, f2 : data segment
 *
 *	`b' stands for "begin", `e' for "end" and `f' for "offset"
 */

struct map {
	long	b1;
	long	e1;
	long	f1;
	long	b2;
	long	e2;
	long	f2;
	};
