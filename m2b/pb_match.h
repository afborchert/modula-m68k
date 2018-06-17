/* (c) Andreas Borchert 1983, University of Ulm */

/*
 * Struktur der Schluesselworttabelle
 */

typedef	struct	hashtab	{

	char		*ht_keyword;
	int		ht_symbol;
	struct	hashtab	*ht_next;

	} HASHTAB;
