/*
 * some defines dependant from /usr/include/sys/user.h
 */
#ifdef BSD42
#	include <machine/param.h>
#	define USIZE UPAGES
#endif
#define AR0_MASK (ctob(USIZE)-1)	/* mask u.u_ar0 with this */
