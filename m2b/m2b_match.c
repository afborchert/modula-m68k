/* (c) Andreas Borchert 1983, University of Ulm */

/*
 * Routinen zur Schluesselworterkennung :
 */

#include	<stdio.h>
#include	<ctype.h>
#include	"pb_mac.h"
#include	"pb_match.h"

#define	HASHCONST	271

static	HASHTAB	*table[HASHCONST];

/*
 * die erste aufzurufende Routine, die die Hashtabelle anlegt
 */

match_init ( keywords , symbols , anzahl )
char	**keywords;
int	*symbols;		/* die dazugehoerigen Symbole */
int	anzahl;			/* der Schluesselwoerter */
{
	int	index;
	int	hashindex;

	BEGIN("match_init");
	for ( index = 0 ; index < HASHCONST ; ++index )
		table[index] = NULL;

	for ( index = 0 ; index < anzahl ; ++index ) {
		hashindex = hash(keywords[index]);
		insert(hashindex,keywords[index],symbols[index]);
		}
	/*
	 * fuer Test u. Optimierungsversuche gegebenenfalls
	 * die Hashtabelle ausgeben
	 */

#ifdef HASHDUMP
	hashdump(table);
#endif
	END
}

/*
 * Einfuegung eines einzelnen Schluesselwortes bei bekanntem
 * Index der Hashtabelle
 */

static
insert ( hashindex , keyword , symbol )
int	hashindex;
char	*keyword;
int	symbol;
{

	HASHTAB	*ptr;

	BEGIN("insert");

	ptr = table[hashindex];
	if ( (table[hashindex] = (HASHTAB *) calloc ( (unsigned) 1 , sizeof(HASHTAB) )) == NULL )
		quit("No space available");
	table[hashindex]->ht_next = ptr;
	table[hashindex]->ht_keyword = keyword;
	table[hashindex]->ht_symbol = symbol;
	END
}

/*
 * Bestimmung des Index der Hashtabelle eines bestimmten Wortes.
 * Die Schluesselwoerter sollten ziemlich breit auf der Tabelle
 * verteilt sein fuer eine effiziente Suche.
 */

static	int
hash ( word )
char	*word;
{
	int	length;
	register int val;

	BEGIN("hash");
	length = strlen(word);
	val = word[0]<<1+word[length-1]>>2+length;
	if (val < 0)
		val = - val;
	RETURN ( val % HASHCONST );
}

/*
 * match() liefert, falls `word' in der Tabelle eingetragen worden ist
 * sein Symbol, ansonsten 0
 */

int
match ( word )
char	*word;
{
	int	hashindex;
	HASHTAB	*ptr;

	BEGIN("match");
	hashindex = hash(word);
	for ( ptr = table[hashindex] ; ptr != NULL ; ptr = ptr->ht_next )
		if ( strcmp(ptr->ht_keyword,word) == 0 )
			return(ptr->ht_symbol);
	RETURN(0);
}

#ifdef HASHDUMP

/*
 * um die Qualitaet der hash() Funktion zu kontrollieren :
 */

hashdump(table)
HASHTAB	*table[];
{
	int	index;
	HASHTAB	*ptr;

	BEGIN("hashdump");
	for ( index = 0 ; index < HASHCONST ; ++index ) {
		printf("%4d | ",index);
		for ( ptr = table[index] ; ptr != NULL ; ptr = ptr->ht_next )
			printf("%-10s ",ptr->ht_keyword);
		printf("\n");
		}
	END
}
#endif
