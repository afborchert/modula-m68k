/*
 *	the field names are taken from adb :
 *
 *	b1, e1, f1 : text segment
 *	b2, e2, f2 : data segment
 *
 *	`b' stands for "begin", `e' for "end" and `f' for "offset"
 */

struct map {
	address b1; address e1; address f1;
	address b2; address e2; address f2;
};
