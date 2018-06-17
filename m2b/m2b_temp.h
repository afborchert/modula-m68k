/* (c) Andreas Borchert 1983, University of Ulm */

/*
 * Struktur des Tempfiles
 */

/*
 * Es ist unbedingt notwendig, dass ts_line die letzte Komponente der
 * Struktur ist. (Da aus Platzgruenden nicht der komplette Buffer in
 * die Tempfile geschrieben wird).
 * Entsprechend muss unbedingt ts_recsize die erste Komponente sein !
 * WICHTIG : ts_prelines  m u s s  die zweite Komponente sein !
 */

typedef struct {

	int	ts_recsize;		/* dyn. Groesse eines Records */
	int	ts_prelines;		/* Anz. Leerzeilen zuvor */
	int	ts_postlines;		/* Anz. Leerzeilen danach */
	int	ts_linelength;		/* entspricht akt. linelength */
	int	ts_mark;		/* Spalte der Markierung   	*/
	int	ts_free;		/* kleinstmoegl. Kommentarspalte */
	int	ts_comment;		/* Kommentarspalte */
	char	ts_line[BUFSIZ];	/* die Textzeile		*/

	} TMPSTR;
