/* (c) Andreas Borchert 1983, University of Ulm */

/*
 * Struktur fuer Kommentare
 */

typedef	struct {

	char	*cs_start;	/* zeigt auf '(' */
	char	*cs_end;	/* zeigt auf ')' */
	char	cs_type;	/* Typ : 'l' = linksbuendig 
				         'a' = aligned
					 'c' = in die Kommentarspalte
					 'C' = analog, jedoch allein in d. Zeile
					 'o' = in den Kontext hinein
					 'm' = mehrzeiliger Kommentar */
	int	cs_column;	/* wo er in der Quelle stand */
	} CMTSTR;
