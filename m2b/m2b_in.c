/* (c) Andreas Borchert 1983, University of Ulm */

/*
 * Einleseroutinen ( zeilenweise ), die MODULA-spezifisch arbeiten.
 * (Kommentar & Stringerkennung )
 *
 * Es erfolgt eine Analyse, ob ueberhaupt eine Textquelle vorliegt
 * (Zaehlen von Nichtascii und nichtsichtbaren Zeichen).
 * Es folgt bei Kommentaren eine Analyse ihrer Positionierung, in
 * Abhaengigkeit davon wird spaeter bei der Ausgabe der Kommentar
 * plaziert.
 * Ansonsten werden die Zeichen auch an die Ausgaberoutinen weiter-
 * geleitet.
 */

#include	<stdio.h>
#include	<ctype.h>
#include	"m2b_cmtstr.h"
#include	"m2b_strstr.h"
#include	"m2b_mac.h"

#define	MAXCMT		 20
#define	MAXSTR		 20

#define	MANY_ILLEGAL	( no_ascii*5 + no_visible > 13 )

static	FILE	*in_fp;
	char	*in_file;	/* Filenamen der Inputdatei */
	int	eof_found = 0;
static	char	in_buf[BUFSIZ];
static	char	*in_ptr;
static	int	no_ascii = 0;	/* Zaehler */
static	int	no_visible = 0;

	int	lines_in = 0;	/* Zaehler der gelesenen Zeilen */
	int	zeilenstart = 0;/* =1,falls unmittelbar am Start der Zeile */
	int	zeilenanfang = 0;/*=1,falls seit Beginn der Zeile nur ' '  */
static	int	ev_zeilenanfang = 0;
static	int	in_comment = 0; /* =1,falls innerhalb eines Kommentars */
static	int	in_string = 0;	/* =1,falls innerhalb eines Strings */
	int	commenton = 0;
	int	stringon = 0;
extern	int	formator_on;

	CMTSTR	cmt[MAXCMT];	/* Kommentare der aktuellen Zeile */
	int	cmt_index = 0;
	int	cmt_cnt = 0;	/* Index fuer obiges Feld */
	int	cmt_start = -1;	/* Spalte des Beginns des letzten Kommentars */
extern	int	comcolumn;	/* die Kommentarspalte */
	int	cmt_level = 0;	/* Kommentare duerfen in MODULA verschachtelt sein */

	STRSTR	str[MAXSTR];	/* Strings der aktuellen Zeile */
	int	str_index = 0;
	int	str_cnt = 0;

extern	int	newline_wanted;	/* fuer putline() in pb_format.c */
	int	cmt_at_eoln;	/* fuer den Parser */

in_start ( filename )
char	*filename;
{
	BEGIN("in_start");
	if ( filename == NULL ) {
		in_file = "stdin";
		in_fp = stdin;
		}
	else if ( (in_fp = fopen(filename,"r")) == NULL )
		quit("Can't open: %s",filename);
	else
		in_file = filename;
	in_ptr = in_buf;
	*in_ptr = '\0';
	in_comment = in_string = commenton = stringon = 0;
	eof_found = FALSE;
	formator_on = TRUE;
	END
}

in_getbuffer()
{
	register	int	c;
	register	char	ch;
	register	char	old_ch;
	register	int	column;
	register	int	empty_line;	/* = TRUE, falls leere Zeile */
	
	BEGIN("in_getbuffer");
	ev_zeilenanfang = zeilenstart = zeilenanfang = 1;
	column = 0;
	ch = '\0';
	if ( !in_comment ) {
		cmt_cnt = 0;
		cmt_start = -1;
		}
	else {
		cmt[0].cs_start = in_buf;
		cmt[0].cs_end = NULL;
		}
	str_cnt = 0;
	str_index = 0;
	in_string = FALSE;

	/*
	 * leere Zeilen ( ausserhalb von Kommentaren ) werden gesondert
	 * behandelt und erscheinen eventuell ( falls es nicht zu viele
	 * werden ) auch in der Ausgabe.
	 */

	if ( feof(in_fp) ) {
		++eof_found;
		in_ptr = in_buf;
		*in_ptr = '\0';
		return;
		}
	empty_line = in_comment ? FALSE : TRUE;
	empty_line &= formator_on;
	do {
		++lines_in;
		column = -1;
		for ( in_ptr = in_buf ; (c = getc(in_fp)) != (int)'\n' &&
				     c != EOF &&
				     in_ptr < in_buf + BUFSIZ ;
				     ++in_ptr ) {
			++column;
			old_ch = ch;
			ch = (char) c;
			if ( !formator_on ) {
				if ( ch != '+' || old_ch != '%' )
					goto direct_copy;
				cmt_cnt = 1;
				cmt_level = 1;
				in_comment = TRUE;
				cmt[0].cs_start = MAX(in_buf,in_ptr-3);
				cmt[0].cs_end = NULL;
				cmt[0].cs_type = 'a';
				ev_zeilenanfang = zeilenanfang = FALSE;
				formator_on = TRUE;
				}

			/* "unanstaendiges" schon hier aussortieren */

			if ( !isascii(ch) ) {
				ch = ' ';
				error("Non-ASCII character found.");
				++no_ascii;
				if ( MANY_ILLEGAL )
					quit("No MODULA 2 source");
				}
			/*
			 * noetig, um exakt die Spalte zu wissen, wo
			 * in der Quelle ein Kommentar beginnt
			 */

			if ( ch == '\t' && !in_comment && !in_string ) {
				int	pos;
				for ( pos = column+1 ; pos % 8 ; ++pos )
					++column;
				ch = ' ';
				}

			/*
			 * Formfeed erlauben wir, weil es ein gaengiger
			 * Trick ist, damit einen Seitenvorschub auf
			 * dem Drucker zu erzeugen.
			 */

			if ( !isvisible(ch) && ch != '\t' && ch != '\f' ) {
				if ( !in_string && !in_comment )
					ch = ' '; /* ausblenden */
				error("Invisible character found.");
				++no_visible;
				if ( MANY_ILLEGAL )
					quit("No MODULA-source");
				}

			/*
			 * Kommentare erkennen :
			 */

			if ( ch == '*' && old_ch == '(' && !in_string ) {
				empty_line = FALSE;
				++cmt_level;
				if ( ! in_comment ) {
					if ( ch == '*' && ev_zeilenanfang )
						zeilenanfang = TRUE;
					++cmt_cnt;
					if ( cmt_cnt > MAXCMT )
					quit("Too many comments in this line");
					++in_comment;
					cmt[cmt_cnt-1].cs_start = in_ptr-1;
					cmt[cmt_cnt-1].cs_end = NULL;
					if ( column <= 1 && zeilenanfang )
						cmt[cmt_cnt-1].cs_type = 'l';
					else if ( zeilenanfang &&
						column < comcolumn-20 )
						cmt[cmt_cnt-1].cs_type = 'a';
					else if ( zeilenanfang )
						cmt[cmt_cnt-1].cs_type = 'C';
					else
						cmt[cmt_cnt-1].cs_type = 'c';
					cmt_start = column-1;
					cmt[cmt_cnt-1].cs_column = cmt_start;
					ev_zeilenanfang = zeilenanfang = FALSE;
					}
				}
			else if ( !isspace(ch) && !in_comment && !in_string ) {
				empty_line = FALSE;
				if ( zeilenanfang )
					zeilenanfang = FALSE;
				else
					ev_zeilenanfang = FALSE;
				if ( cmt_cnt && cmt[cmt_cnt-1].cs_type != 'l')
					cmt[cmt_cnt-1].cs_type = 'o';
				}
			else if ( ch == ')' && old_ch == '*' && !--cmt_level ) {
				empty_line = FALSE;
				cmt[cmt_cnt-1].cs_end = in_ptr;
				in_comment = 0;
				if ( cmt[cmt_cnt-1].cs_type == 'm' )
					cmt[cmt_cnt-1].cs_type = 'l';
				}
			if ((ch == '\'' || ch == '"') && !in_comment)
				if ( in_string && ch == str[str_cnt-1].ss_ch ){
					int	hch;

					hch = getc(in_fp);
					if ( hch == str[str_cnt-1].ss_ch ) {
						*in_ptr = hch;
						++in_ptr;
						goto direct_copy;
						}
					ungetc(hch,in_fp);
					in_string = FALSE;
					str[str_cnt-1].ss_end = in_ptr;
					}
				else if ( !in_string ) {
					++in_string;
					++str_cnt;
					if ( str_cnt > MAXSTR )
						quit("Too many strings in this line");
					str[str_cnt-1].ss_start = in_ptr;
					str[str_cnt-1].ss_end = NULL;
					str[str_cnt-1].ss_ch = ch;
					}
			else if ( ch == ' ' && old_ch == ' ' && !in_comment
				&& !in_string ) {
				--in_ptr;
				continue;
				}
direct_copy:
			*in_ptr = ch;
			}
		if ( empty_line && c != EOF )
			++newline_wanted;
		}
	while ( empty_line && c != EOF && formator_on );
	if ( ferror(in_fp) )
		quit("Read error in %s",in_file);
	if ( in_ptr >= in_buf+BUFSIZ-1 )
		quit("Line too long");
	if ( in_string ) {
		error("Nonterminated string");
		*in_ptr++ = '\'';
		in_string = 0;
		str[str_cnt-1].ss_end = in_ptr-1;
		}
	else if ( in_comment ) {
		cmt[cmt_cnt-1].cs_type = 'm';
		cmt[cmt_cnt-1].cs_end = in_ptr-1;
		}
	else
		*in_ptr++ = ' ';
	if ( cmt_cnt > 0 && cmt[cmt_cnt-1].cs_type == 'c' )
		cmt_at_eoln = TRUE;
	else
		cmt_at_eoln = FALSE;
	cmt_index = 0;	/* aktueller Kommentar */
	str_index = 0;	/* aktueller String */
	if ( c == EOF )
		++eof_found;
	if ( c == EOF && (in_comment || commenton) )
		quit("EOF in comment found");
	*in_ptr = '\0';
	in_ptr = in_buf;
	END
}

getch ()
{
	int	blank_zurueck;
	static	int	string_zurueck = 0;

	BEGIN("getch");
	if ( string_zurueck ) {
		++stringon;
		string ( &str[str_index] );
		in_ptr = str[str_index].ss_end+1;
		stringon = FALSE;
		++str_index;
		string_zurueck = FALSE;
		RETURN ( * ( in_ptr-1 ) )
		}
	blank_zurueck = FALSE;
	if ( eof_found ) {
		if ( !formator_on )
			quit("(*%%-*) not closed.");
		RETURN ( EOF_CH )
		}
	if ( *in_ptr == '\0' ) {
		in_getbuffer();
		if ( !formator_on )
			putline();
		}
	if ( cmt_index < cmt_cnt && in_ptr == cmt[cmt_index].cs_start )
		putch(' ');	/* pro{hello}gram != program */
	while ( cmt_index < cmt_cnt && in_ptr == cmt[cmt_index].cs_start ) {
		++commenton;
		blank_zurueck = TRUE;
		comment ( &cmt[cmt_index] );
		in_ptr = cmt[cmt_index].cs_end+1;
		if ( !formator_on ) {
			commenton = FALSE;
			cmt_index = 0;
			cmt_cnt = 0;
			break;
			}
		commenton = cmt[cmt_index].cs_type == 'm' ? TRUE : FALSE;
		if ( commenton ) {
			cmt[0].cs_start = in_buf;
			cmt[0].cs_end   = NULL;
			cmt[0].cs_type  = 'm';
			cmt[0].cs_column= 0;
			cmt_index = 0;
			cmt_cnt = 1;
			in_getbuffer();
			}
		else
			++cmt_index;
		}
	if ( !formator_on ) {
		putch(*in_ptr);
		++in_ptr;
		return(' ');
		}
	if ( blank_zurueck ) {
		putch(' ');
		RETURN(' ')
		}
	if ( str_index < str_cnt && in_ptr == str[str_index].ss_start ) {
		/* ***
		putch(' ');
		*** */
		string_zurueck = TRUE;
		RETURN(' ');
		}
	if ( stringon || commenton )
		fatal("Error in `getch'");
	putch ( *in_ptr );
	++in_ptr;
	if ( formator_on )
		RETURN ( *(in_ptr-1) )
	else
		RETURN ( ' ' )
}

ungetch (ch)
char	ch;
{
	BEGIN("ungetch");
	if ( isspace(ch) )
		RETURN(0)
	if ( in_ptr > in_buf ) {
		*--in_ptr = ch;
		back(1);	/* geht, da garantiert kein Kommentar */
		eof_found = 0;
		}
	else
		fatal("Illegal use of `ungetch'");
	END
}

/*
 * nach `end.' ist noch lange nicht Schluss !
 * Denn es koennen noch Kommentare folgen. Um diese noch
 * abzuarbeiten, gibt es...
 */

read_to_eof()
{
	while ( !eof_found && getch() == ' ')
		;
	if ( !eof_found )	/* also doch noch mehr als Kommentare ! */
		error("EOF in MODULA-source expected");
	else
		putline ();	/* Rest noch ausgeben */
}

in_end ()
{
	BEGIN("in_end");
	fclose(in_fp);
	END
}

/*
 * fuer's entwanzen...
 */

#ifdef DEBUG

in_dump ()
{
	int	i;
	char	*ptr;

	fprintf(stderr,"pb_in.c :\n---------\n");
	for ( ptr = in_buf ; (isvisible(*ptr)||*ptr=='\t') && ptr-in_buf < BUFSIZ ; ++ptr )
		;
	*ptr = '\0';
	/* `pos_ptr' geht davon aus, dass 'in_buf = `' die Laenge 10 hat */
	fprintf(stderr,"in_buf = `%s'\n",in_buf);
	pos_ptr(in_ptr,'^');
	fprintf(stderr," in_ptr\n");
	fprintf(stderr,"Comments :\n");
	for ( i = 0 ; i < cmt_cnt ; ++i ) {
		pos_ptr(cmt[i].cs_start,'^');
		fprintf(stderr," cmt[%d].cs_start\n",i);
		pos_ptr(cmt[i].cs_end,'$');
		fprintf(stderr," cmt[%d].cs_end\n",i);
		}
	for ( i = 0 ; i < cmt_cnt ; ++i ) {
		if ( i == cmt_index )
			fprintf(stderr,"->\t");
		else
			fprintf(stderr,"%2d\t",i);
		fprintf(stderr,"`%c' %3d ",cmt[i].cs_type,cmt[i].cs_column);
		if ( cmt[i].cs_end == NULL )
			fprintf(stderr,"Not closed.\n");
		else {
			char	hch = *(cmt[i].cs_end+1);
			*(cmt[i].cs_end+1) = '\0';
			fprintf(stderr,"`%s'\n",cmt[i].cs_start);
			*(cmt[i].cs_end+1) = hch;
			}
		}
	fprintf(stderr,"Strings :\n");
	for ( i = 0 ; i < str_cnt ; ++i ) {
		if ( i == str_index )
			fprintf(stderr,"->\t");
		else
			fprintf(stderr,"%2d\t",i);
		if ( str[i].cs_end == NULL )
			fprintf(stderr,"Not closed.\n");
		else {
			char	hch = *(str[i].ss_end+1);
			*(str[i].ss_end+1) = '\0';
			fprintf(stderr,"`%s'\n",str[i].ss_start);
			*(str[i].ss_end+1) = hch;
			}
		}
	fprintf(stderr,"newline_wanted = %d\n",newline_wanted);
	if ( eof_found )
		fprintf(stderr,"EOF found.\n");
	fprintf(stderr,"lines_in = %d\n",lines_in);
	fprintf(stderr,"cmt_level = %d\n",cmt_level);
	fprintf(stderr,"in_comment = ");
	print_bool(in_comment);
	fprintf(stderr,"in_string = ");
	print_bool(in_string);
	fprintf(stderr,"commenton = ");
	print_bool(commenton);
	fprintf(stderr,"stringon = ");
	print_bool(stringon);
	fprintf(stderr,"formator_on = ");
	print_bool(formator_on);
	fprintf(stderr,"\n\n\n");
}

pos_ptr(ptr,ch)
char	*ptr;
char	ch;
{
	int	i;

	if ( ptr < in_buf || ptr > in_buf+BUFSIZ ) {
		fprintf(stderr,"%c ???",ch);
		return;
		}
	for ( i = 0 ; i < ptr-in_buf+10 ; ++i )
		putc(' ',stderr);
	putc(ch,stderr);
}

print_bool(var)
int	var;
{
	if ( var )
		fprintf(stderr,"TRUE\n");
	else
		fprintf(stderr,"FALSE\n");
}

#endif DEBUG
