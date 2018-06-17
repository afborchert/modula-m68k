/* (c) Andreas Borchert 1983, University of Ulm */

/*
 * Bearbeitung der Optionen
 */

#include	<stdio.h>
#include	<ctype.h>
#include	"pb_mac.h"
#include	"pb_optstr.h"

static	int	intarg;
static	char	*getname();
static	OPTSTR	*find();

options ( argc , argv , tab , tablen )
int	argc;
char	**argv;
OPTSTR	*tab;
int	tablen;
{
	OPTSTR	*ptr;
	char	*opt_name;
	OPTSTR	*opt_ptr;
	char	ch;
	
	for ( ptr = tab ; ptr-tab < tablen ; ++ptr )
		ptr->opt_used = 0;
	while ( argc > 0 && argv ) {
		opt_name = getname ( *argv );
		opt_ptr = find ( opt_name , tab , tablen );
		if ( opt_ptr == NULL )
			usage();
		switch ( opt_ptr->opt_type ) {
	
		case 'f' :			/* simple flag */
			*(opt_ptr->opt_spez.f.addr) =
				~(opt_ptr->opt_spez.f.dflt);
			break;
	
		case 'i' :			/* with integer argument */
			if ( getint ( *argv ) ) {
				if ( getint ( *++argv ) )
					quit("Argument of `%s' missing",
						opt_name);
				--argc;
				}
			if ( intarg < opt_ptr->opt_spez.i.arg_low )
				quit("Argument of `%s' must be greater than %d",
					opt_name,opt_ptr->opt_spez.i.arg_low);
			if ( intarg > opt_ptr->opt_spez.i.arg_high )
				quit("Argument of `%s' must be less than %d",
					opt_name,opt_ptr->opt_spez.i.arg_high);
			*(opt_ptr->opt_spez.i.arg_addr) = intarg;
			break;
	
		case 's':			/* with string argument */
			if ( strcmp ( opt_ptr->opt_name , "" ) ) {
				*(opt_ptr->opt_spez.s.str_addr) = *++argv;
					/* take next */
				--argc;
				}
			else if ( strcmp ( *argv , "-" ) == 0 )
				*(opt_ptr->opt_spez.s.str_addr) = "";
			else
				*(opt_ptr->opt_spez.s.str_addr) = *argv;
			break;
	
		default :
			if ( isvisible(opt_ptr->opt_type) )
				fatal(
				"Illegal optiontype found in optiontable : `%c'",
				opt_ptr->opt_type);
			else
				fatal("Illegal optiontype found in optiontable : %o ( octal )", (int) opt_ptr->opt_type);
		}
		++opt_ptr->opt_used;
		++argv;
		--argc;
		} /* of while */
	
	/*
	 * Defaults einsetzen fuer unbenutzte Optionen
	 */
	
	for ( ptr = tab ; ptr-tab < tablen ; ++ptr )
		if ( ptr->opt_used == 0 )
			switch ( ptr->opt_type ) {
	
			case 'f' :
				*(ptr->opt_spez.f.addr) = ptr->opt_spez.f.dflt;
				break;
	
			case 'i' :
				*(ptr->opt_spez.i.arg_addr) = ptr->opt_spez.i.arg_dflt;
				break;
	
			case 's' :
				*(ptr->opt_spez.s.str_addr) = ptr->opt_spez.s.str_dflt;
				break;
	
			default :
				if ( isvisible(ptr->opt_type) )
					fatal("Illegal optiontype found in optiontable : `%c'",ptr->opt_type);
				else
					fatal("Illegal optiontype found in optiontable : %o (octal)",(int) ptr->opt_type);
			}
}


static	char	*
getname ( string )
char	*string;
{
	static	char	result[16];
	char	*ptr;

	for ( ptr = result ; !isdigit(*string) && ptr-result < 16 ; ++ptr ) {
		*ptr = *string;
		++string;
		}
	*ptr = '\0';
	return ( result );
}

static	OPTSTR	*
find ( name , tab , tablen )
char	*name;
OPTSTR	*tab;
int	tablen;
{
	OPTSTR	*ptr;
	
	if ( strcmp(name,"-") == 0 )
		name = "";
	for ( ptr = tab ; ptr-tab < tablen ; ++ptr )
		if ( strlen(ptr->opt_name) == 0
		     && name[0] != '-' || strcmp(name,ptr->opt_name) == 0 )
			if ( ptr->opt_used == 0 || ptr->opt_repl )
				return ( ptr );
	return ( NULL );
}

static	int
getint	( string )
char	*string;
{
	char	*ptr;
	for ( ptr = string ; ; ++ptr )
		if ( *ptr == '\0' )
			return ( 1 );	/* nicht erfolgreich */
		else if ( isdigit ( *ptr ) )
			break;
	sscanf ( ptr , "%d" , &intarg );
	return ( 0 );			/* erfolgreich */
}
