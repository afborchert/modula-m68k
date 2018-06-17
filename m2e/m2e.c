/*
 *	m2e -- print error messages of the compiler
 *
 *	rev afb 6/84: make a listing if `-L' is set
 *	rev afb 7/90: error file can be passed as argument
 */

#include	<stdio.h>
#include	"m2e_symbols.h"

typedef	unsigned short	word;
typedef	unsigned long fullword;

int	skip[ANYCON+1];

word	get_word();
fullword get_fullword();

char	*usage = { "Usage: %s [-4] [-L] [-e errfile] sourcefile [interpassfile]\n" };
int	pass4_flag = 0;
int	listing = 0;
int	no_il = 0;
char * errfile = NULL;	/* file with error texts */

#ifdef LOCAL
#	define	USAGE	fprintf(stderr,usage,name) , exit(1)
#else
#	define	USAGE	exit(0)
#endif

main(argc, argv)
	int argc;
	char ** argv;
{	FILE *fp1, *fp2;
	char *name;

	name = *argv;
	while (--argc && **++argv == '-') switch (*++* argv) {
	case '4' :	pass4_flag = 1; break;
	case 'L' :	listing = 1; break;
	case 'e' :	if (--argc)
				errfile = *++argv;
			else
				USAGE;
			break;
	default :
		USAGE;
	}
	if (argc != 2)
		if (argc == 1 && listing)
			++no_il;
		else
			USAGE;
	if ( (fp1 = fopen(*argv,"r")) == NULL )
	{	perror(* argv);
		exit(1);
	}
	if (! no_il && (fp2 = fopen(*++argv, "r")) == NULL)
		if (listing)
			++no_il;
		else
		{	perror(* argv);
			exit(1);
		}
	source_init(fp1);
	if (no_il)
		print_rest();
	else
	{	if (! pass4_flag)
			skip_init();
		errnr_init();
		work(fp2);
		if (listing)
			print_rest();
	}
	exit(0);
}

int	line;

/*
 *	scan interpassfile
 */

work(fp)
	FILE *fp;
{	word	w;
	int	index;
	short	sw;
	int pos;

	if (! pass4_flag)
		while (w = get_word(fp), pos = w % 0x100, 1) switch (w/0x100) {
		case EOL :
			line = get_fullword(fp);
			break;
		case ERRORSY :
			print_error(line, pos, get_fullword(fp));
			break;
		case OPTION : /* skip 1 + 1/2 word */
			get_fullword(fp);
			get_word(fp);
			break;
		case EOP :
			return;
		default :
			if (w/0x100 <= ANYCON)
				for (index = 0; index < skip[w/0x100]; ++index)
					get_fullword(fp);
			else
			{	fprintf(stderr, "No interpass file\n");
				exit(1);
			}
			break;
		}
	else
		while(line = get_word(fp), ! feof(fp))
		{	pos = get_word(fp);
			sw = get_word(fp);
			print_error(line, pos, sw);
		}
}

word get_word(fp)
	FILE *fp;
{	word	buf;

	if ( fread(&buf, sizeof(word), 1, fp) )
		return buf;
	else if (! pass4_flag)
	{	fprintf(stderr, "Unexpected EOF in IL-file\n");
		exit(1);
	}
}

fullword get_fullword(fp)
	FILE * fp;
{	fullword buf;

	if (fread(& buf, sizeof(fullword), 1, fp))
		return buf;
	else
	{	fprintf(stderr, "Unexpected EOF in IL-file\n");
		exit(1);
	}
}

skip_init()
{	int	index;

	for ( index = EOP ; index <= ANYCON ; ++index )
		skip[index] = 0;
	skip[PROCEDURESY] = 1;
	skip[MODULESY] = 1;
	skip[IDENT] = 1;
	skip[INTCON] = 1;
	skip[CARDCON] = 1;
	skip[INTCARCON] = 2;
	skip[REALCON] = 2;
	skip[CHARCON] = 1;
	skip[STRINGCON] = 2;
	skip[BIGSETCON] = 1;
	skip[OPTION] = 2;
	skip[NAMESY] = 1;
	skip[FIELD] = 1;
	skip[ANYCON] = 2;
	skip[SYMBOLSY] = 1;
	skip[DEFINITIONSY] = 1;
}
