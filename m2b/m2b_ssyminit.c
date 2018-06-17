/* (c) Andreas Borchert 1983, University of Ulm */

/*
 * Initialisierung von ssym
 */

#include	"m2b_symbols.h"

int	ssym[ASCII];

ssym_init ()
{
	int	index;

	for (index = 0; index < ASCII; ++index)
		ssym[index] = ILLEGAL;
	ssym[ '+' ] = PLUS;
	ssym[ '-' ] = MINUS;
	ssym[ '*' ] = TIMES;
	ssym[ '/' ] = SLASH;
	ssym[ '.' ] = POINT;
	ssym[ ';' ] = SEMICOLON;
	ssym[ ',' ] = COMMA;
	ssym[ ':' ] = DOUBLEPOINT;
	ssym[ '\'' ] = STRING;
	ssym[ '"' ] = STRING;
	ssym[ '(' ] = LPAREN;
	ssym[ ')' ] = RPAREN;
	ssym[ '[' ] = LBRACKET;
	ssym[ ']' ] = RBRACKET;
	ssym[ '{' ] = LBRACE;
	ssym[ '}' ] = RBRACE;
	ssym[ '^' ] = PTRCH;
	ssym[ '|' ] = ORCHAR;

	ssym[ '#' ] = CMP;
	ssym[ '&' ] = AND;
	ssym[ '~' ] = NOT;
	        
	for (index = 'a'; index < 'z'; ++index)
		ssym[index] = index;
}
