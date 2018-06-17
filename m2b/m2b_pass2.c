/* (c) Andreas Borchert 1983, University of Ulm */

/*
 * Pass 2 : Nachformatierungen
 *
 * Bei markierten Zeilen justieren,
 * bei zu langen Zeilen - falls moeglich - Abhilfe schaffen,
 * und endgueltige Ausgabe.
 * Mehrfache Leerzeilen ( ausserhalb von Kommentaren ) werden
 * gekuerzt.
 */


/*
 * Bemerkung :
 * Spalten beginnen hier ab 0, nicht ab 1 !
 * negativer Spaltenwert steht fuer garkeinen.
 */

#include	<stdio.h>
#include	<ctype.h>
#include	"pb_mac.h"
#include	"pb_temp.h"	/* Struktur der temp. Datei */

extern char * calloc();

extern TMPSTR * t_read ();


#define	MAXEMPTY 1		/* maximale Anzahl von zshgd Leerzeilen */
extern	int	linelength;	/* importiert aus pb_format.c	*/
extern	char	buferr[];

	/* zur Kommunikation mit pb_in.c */

	int	newline_wanted;	/* Anzahl zusaetzlich gewuenschter Leerz. */

static	TMPSTR	buffer;		/* Buffer fuer einen Record der Tempfile */
static	char	*buf_ptr;	/* zeigt auf die Textzeile im Buffer */
static	int	empty_line;	/* == TRUE, falls Leerzeile vorliegt */
static	int	maxmark;	/* Zur Ber. der endgueltigen Position */
static	int	minmark;
static	int	counter;	/* zaehlt die Zeilen bei pass2_write */
static	int	mark_lines;	/* zaehlt die Zeilen mit mark >= 0 */
static	TMPSTR	*ptr;		/* zeigt auf static-buffer von `temp' */
static	FILE	*out_fp;	/* Ausgabefile */
	char	*out_file;	/* falls gegeben: Ausgabedateiname */
	int	lines_out = 0;	/* zaehlt die tatsaechlich ausgeg. Zeilen */

/*
 * Erste Funktion, die von diesem Modul aufgerufen soll,
 * was jedoch nicht kontrolliert wird
 */

pass2_start ( filename )
char	*filename;
{
	if ( filename != NULL ) {
		ignore(close(creat(filename,0600)));
		if ( (out_fp=fopen(filename,"w")) == NULL )
			quit("Can't creat %s",filename);
		out_file = filename;
		}
	else {
		out_fp = stdout;
		out_file = "stdout";
		}
	buf_ptr = buffer.ts_line;
	empty_line = 0;
	temp_open();
	counter = 0;
	buffer.ts_prelines = 0;
	buffer.ts_postlines = 0;
	pass2_open_rge();
}

/*
 * Die Ausgabefunktionen des Formatiermoduls in pb_format.c
 */

pass2_write ( string )
char	*string;
{
	if ( strlen(string) + buf_ptr-buffer.ts_line >= BUFSIZ )
		quit ( buferr );
	*buf_ptr = '\0';
	strcat ( buf_ptr , string );
	buf_ptr += strlen(string);
}

pass2_flushline ( mark , free_col , cmt , pre , post )
int	mark;		/* in welcher Spalte (geg.) das zu just. Zeichen ist */
int	free_col;	/* Spalte, wo der freie Bereich beginnt */
int	cmt;		/* Wo der (hintere) Kommentar ist.(Spalte) */
int	pre;		/* Leerzeilen davor */
int	post;		/* Leerzeilen danach */
{
	buffer.ts_postlines = MAX(post,newline_wanted);
	newline_wanted = 0;
	if ( mark >= 0 )
		++mark_lines;
	buffer.ts_linelength = linelength; /* die kann sich aendern !!! */
	buffer.ts_mark = mark;
	if ( mark > 0 ) {
		if ( mark < minmark + 20 )
			maxmark = mark > maxmark ? mark : maxmark;
		minmark = mark < minmark ? mark : minmark;
		}
	buffer.ts_free = free_col;
	buffer.ts_comment = cmt;
	*buf_ptr = '\0';
	buf_ptr = buffer.ts_line;
	buffer.ts_prelines = pre;

	/*
	 * recsize gibt die dynamische Bufferlaenge ohne die Laengenangabe
	 * selbst an !
	 */

	buffer.ts_recsize = sizeof(TMPSTR)-BUFSIZ+strlen(buffer.ts_line)+1-sizeof(int);
	if ( cmt != 0 ) {	/* falls nicht die ganze Zeile Kommentar */
		for ( ; *buf_ptr ; ++buf_ptr )
			if ( !isspace(*buf_ptr) )
				break;
		if ( *buf_ptr == '\0' ) {	/* ganze Zeile leer ? */
			++buffer.ts_prelines;
			return;
			}
		buf_ptr = buffer.ts_line;
		}

	/*
	 * aus Effiziensgruenden wird die ersten Zeilen eines Formatierber.,
	 * die mit mark == -1 uebergeben werden, nicht ueber die Tempfile
	 * geschickt, sondern sofort ausgegeben
	 */

	if ( mark_lines ) {
		temp_write ( &buffer );
		++counter;
		}
	else {
		format();
		pass2_print();
		}
	buffer.ts_prelines = 0;
	buffer.ts_postlines = 0;
}

/*
 * mit pass2_open_rge und pass2_close_rge klammert man einen
 * von anderen unabhaengigen Formatierbereich.
 * So ein Bereich wird jeweils in die temporaere Datei
 * geschrieben.
 */

pass2_open_rge ()
{
	if ( counter )	/* wenn der alte Bereich noch nicht geschl. wurde */
		pass2_close_rge();
	counter = 0;
	maxmark = 0;
	minmark = 200;
	mark_lines = 0;
}

pass2_close_rge ()
{
	if ( !counter )	/* wenn kein Formatierbereich vorhanden */
		return;
	temp_rewind ( "r" );	/* Bereich neu aus der Tempfile lesen... */
	maxmark = maxmark - 20 > minmark ? minmark+20 : maxmark;
	for ( ; counter > 0 ; --counter ) {
		if ( (ptr = t_read()) == (TMPSTR *) NULL )
			quit("Read error.");
		strncpy ( buffer.ts_line , ptr->ts_line , BUFSIZ );
		buffer.ts_linelength = ptr->ts_linelength;
		buffer.ts_prelines = ptr->ts_prelines;
		buffer.ts_postlines = ptr->ts_postlines;
		buffer.ts_mark = ptr->ts_mark;
		buffer.ts_free = ptr->ts_free;
		buffer.ts_comment = ptr->ts_comment;
		format ();	/* ...und nachformatieren */
		pass2_print ();		/* endgueltige Ausgabe	  */
		}
	temp_rewind ( "w" );	/* wieder in Schreibmodus */
}

/*
 * Nachformatierung :
 * jetzt ist maxmark bekannt und es kann endgueltig justiert werden.
 * Weiter wird letzte Hand an den letzten Kommentar der jeweiligen
 * Zeile gelegt ( falls der ueber `linelength' darueberhinaus ragt )
 */

static
format ()		/* es wird `buffer' benutzt */
{
	int	index;

	if ( buffer.ts_comment == 0 )	/* ganze Zeile Kommentar ? */
		return;
	if ( buffer.ts_mark < maxmark && buffer.ts_mark > 0 ) { 
		char *ptr = calloc ( (unsigned)(BUFSIZ+maxmark-buffer.ts_mark) ,
						 sizeof ( char ) );
		if ( ptr == NULL )
			quit("No space available");

		/* justieren... */

		strncpy(ptr,buffer.ts_line,buffer.ts_mark);
		for ( index = 0 ; index < maxmark-buffer.ts_mark ; ++index )
			ptr[buffer.ts_mark+index] = ' ';
		strncpy(ptr+maxmark,buffer.ts_line+buffer.ts_mark,BUFSIZ-
			maxmark);

		/* den Kommentar wieder auf alte Spalte setzen */

		if ( buffer.ts_free > 0 ) {
			char	*hptr;
			hptr = ptr + buffer.ts_free+maxmark-buffer.ts_mark+1;
			for ( index = 0 ; index < maxmark-buffer.ts_mark &&
					  *hptr == ' ' ; ++index )
				strcpy ( hptr , hptr+1 );
			buffer.ts_free += maxmark-buffer.ts_mark;
			buffer.ts_comment += maxmark-buffer.ts_mark-index;
			}

		/* Zeile zu lang geworden ??? */

		if (strlen(ptr) > BUFSIZ)
			eliminate ( ptr , strlen(ptr)-BUFSIZ );

		/* zurueckkopieren */

		strncpy(buffer.ts_line,ptr,BUFSIZ);
		cfree(ptr);
		}

	/* Zeile ueber `linelength' ??? */

	*(buffer.ts_line + BUFSIZ - 1) = '\0'; /* zur Sicherheit */
	if (strlen(buffer.ts_line) > linelength && linelength )
		eliminate ( buffer.ts_line ,
			strlen(buffer.ts_line)-linelength);
	if (strlen(buffer.ts_line) > linelength && linelength )
		warning("Outputline %d longer than %d characters.",lines_out+1,linelength);
}

static
eliminate ( string , anzahl )	/* es wird `buffer' benutzt */
char	*string;	/* aus welchem String */
int	anzahl;		/* wieviele Blanks herausgenommen werden sollen */
{
	char	*ptr;

	if ( anzahl < 0 )
		anzahl = 0;
	if ( !anzahl )
		return;

	/*
	 * Einen eventuell vorhanden Kommentar nach links schieben
	 */

	if ( buffer.ts_free > 0 && buffer.ts_comment > 0 ) {
		ptr = string + buffer.ts_free+1;
		for ( ; *ptr == ' ' && anzahl ; ptr ) {
			strcpy ( ptr , ptr+1 );
			--anzahl;
			}
		}

	/*
	 * Falls das nicht ausreicht, auf die exakte Justierung verzichten
	 */

	if ( buffer.ts_mark > 0 && maxmark > buffer.ts_mark && anzahl ) {
		ptr = string + buffer.ts_mark;
		for ( ; *ptr == ' ' && anzahl ; ptr ) {
			strcpy ( ptr , ptr+1 );
			--anzahl;
			}
		}

	/*
	 * Im Notfall nicht so weit einruecken
	 */

	if ( anzahl && buffer.ts_comment != 0 ) { /* Auf Kommentar aufpassen */
		ptr = string;
		for ( ; *ptr == ' ' && anzahl ; ptr ) {
			strcpy ( ptr , ptr+1 );
			--anzahl;
			}
		}
}

/*
 * Einzige Ausgaberoutine des Formatieres ( von stderr abgesehen )
 */

static
pass2_print ()
{
	int	index;
	static	lastpostlines = MAXEMPTY;	/* am Anfang keine Leerzeilen */

	/*
	 * erst hier gibt es ein '\n' !
	 */

	buffer.ts_prelines = buffer.ts_prelines+lastpostlines > MAXEMPTY ?
			     MAXEMPTY - lastpostlines :
			     buffer.ts_prelines;
	for ( index = 0 ; index < buffer.ts_prelines ; ++index )
		fprintf(out_fp,"\n");
	fprintf(out_fp,"%s\n",buffer.ts_line);
	buffer.ts_postlines= buffer.ts_postlines > MAXEMPTY ?
			     MAXEMPTY :
			     buffer.ts_postlines;
	for ( index = 0 ; index < buffer.ts_postlines ; ++index )
		fprintf(out_fp,"\n");
	lastpostlines = buffer.ts_postlines;
	++lines_out;
}

/*
 * Beendigung des Pass 2 :
 */


pass2_end ()
{
	if ( counter )
		pass2_close_rge();
	ignore(fclose ( out_fp ));
}

/*
 * Im Fehlerfall zur Verhinderung von Schleifen
 */

p2_fast_end ()
{
	ignore(fclose ( out_fp ));
}

/*
 * fuers entwanzen...
 */

#ifdef DEBUG

pass2_dump ()
{
	char	*ptr,hch;
	int	i;

	fprintf(stderr,"pb_pass2.c:\n------------\n\n");
	fprintf(stderr,"buffer :\n");
	for ( ptr = buffer.ts_line ; isvisible(*ptr) || *ptr == '\t' ;
		++ptr )
		;
	hch = *ptr;
	*ptr = '\0';
	fprintf(stderr,"buffer.ts_line = `%s'\n",buffer.ts_line);
	*ptr = hch;
	if ( buf_ptr >= buffer.ts_line && buf_ptr < buffer.ts_line + BUFSIZ ) {
		for ( i = 0 ; i < buf_ptr-buffer.ts_line+18 ; ++i )
			putc(' ',stderr);
		fprintf(stderr,"^ buf_ptr\n");
		}
	else
		fprintf(stderr,"buf_ptr ???\n");
	fprintf(stderr,"buffer.ts_recsize = %d\n",buffer.ts_recsize);
	fprintf(stderr,"buffer.ts_prelines = %d\n",buffer.ts_prelines);
	fprintf(stderr,"buffer.ts_linelength = %d\n",buffer.ts_linelength);
	fprintf(stderr,"buffer.ts_mark = %d\n",buffer.ts_mark);
	fprintf(stderr,"buffer.ts_free = %d\n",buffer.ts_free);
	fprintf(stderr,"buffer.ts_comment = %d\n",buffer.ts_comment);
	fprintf(stderr,"\n");
	fprintf(stderr,"counter = %d\n",counter);
	fprintf(stderr,"minmark = %d\n",minmark);
	fprintf(stderr,"maxmark = %d\n",maxmark);
	fprintf(stderr,"lines_out = %d\n",lines_out);
	fprintf(stderr,"empty_lines = %d\n",empty_lines);
}

#endif DEBUG
