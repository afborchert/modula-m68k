/*
 *   pb - PASCAL beautifier
 *
 *   (c) Andreas Borchert 1983, University of Ulm
 *
 *   Version 4.00
 *
 *   Sorry, the comments have been written in german.
 *
 */

#include	<stdio.h>
#include	<ctype.h>
#include	<sys/types.h>
#include	<sys/stat.h>

#include	"pb_optstr.h"
#include	"pb_symbols.h"
#include	"pb_mac.h"

char version[] =	{ "Version 4.00" };
char *usage_str =	{ "Usage: m2b [-c column] [-h] [-k] [-l length] [-o] [-p] [-s step] [-u] [-v] [infile [outfile] ]" };
char nulldev[] =	{ "/dev/null" };
char pb_name[] =	{ "pb" };
extern	char *in_file;
extern	char *out_file;
extern	char	*word[];
extern	int	wsym[];
struct	stat	instat_buffer,outstat_buffer;

extern	OPTSTR	opttab[];

main ( argc,argv ) 
int	argc;
char	**argv;
{

	int	tablen;
	FILE	*fp;
	
	BEGIN("main");

	error_start();
	
	/*
	 * die Options/Flags bearbeiten
	 */
	
	tablen = optinit();
	options ( argc , argv , opttab , tablen );

	/*
	 * nun die Dateien eroeffnen
	 */

	if ( in_file == NULL )
		fp = stdin;
	else if ( (fp = fopen(in_file,"r")) == NULL )
		quit("Can't open : %s ",in_file);
	if ( out_file && strcmp ( out_file , "" ) == 0 ) 
		out_file = nulldev;
	else if ( out_file && strcmp ( in_file , out_file ) == 0 && argc > 2 )
			quit("Don't destroy your source !!!");

	/* bei pb x > x ... */

	fstat ( fileno(fp) , &instat_buffer );
	fstat ( fileno(stdout) , &outstat_buffer );
	
	if ( ! ( instat_buffer.st_mode & S_IFCHR ) ) /* Terminal ? */
		if ( instat_buffer.st_dev == outstat_buffer.st_dev &&
		     instat_buffer.st_ino == outstat_buffer.st_ino ) {
			fclose ( fp );
			quit("Sorry...Please try it again ( retyping your source ). ");
			}
	
	/*
	 * initialisieren
	 */

	ssym_init();
	match_init(word,wsym,NORW);
	errnr_init();

	/*
	 * den Formatierer starten
	 */

	if ( fp != stdin )
		fclose(fp);
	in_start ( in_file );
	pass2_start( out_file );
	putstart();
	unit();
	read_to_eof();

	/*
	 * Schluss
	 */

	shut_down();
	if ( anz_errors() )
		exit(1);
	END
}
