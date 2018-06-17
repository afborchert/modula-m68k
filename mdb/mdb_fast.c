/*
 *	print backtrace and run-time error message
 */

#include	"mdb_ref.h"
#include	"mdb_tree.h"
#include	"mdb_view.h"

extern char * error_mess();
extern view ** backtrace();

extern int coremode;

fast()
{	char * mess;
	view ** v;

	mess = error_mess();
	if (! coremode)
		exit(1);
	if (! (v = backtrace()))
	{	printf("%s\n", mess);
		printf("\nno Modula-2 stack found.\n");
	}
	else
	{	while (*v)
			printf("%s\n", (*v++)->v_line);
		printf("\n%s\n", mess);
	}
}
