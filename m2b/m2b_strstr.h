/* (c) Andreas Borchert 1983, University of Ulm */

/*
 * Struktur fuer Strings
 */

typedef struct {

	char	*ss_start;	/* zeigt auf ' */
	char	*ss_end;	/* zeigt auf ' */
	char	ss_ch;		/* == ' oder "  */
	
	} STRSTR;
