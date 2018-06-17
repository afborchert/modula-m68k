/* (c) Andreas Borchert 1983, University of Ulm */

/*
 *	Der Parser : die Seele des `m2b'
 */

#include	<stdio.h>
#include	<ctype.h>
#include	"m2b_symbols.h"
#include	"m2b_keywords.h"
#include	"m2b_mac.h"

#define	nl_allowed 1

	int	notfirst;
extern	int	step;
extern	int	kflag;
extern	int	stringon;
extern	int	cmt_at_eoln; /* imported from m2b_in.c */

extern	int	ssym[ASCII];

static	int	inrange = FALSE;	/* if on: recognize the symbol ".." */

#define	MAXUNGET	20

int	unget[MAXUNGET];
int	ungetindex = { 0 };
int	keyword = { FALSE };
char	ch;
int	sym;

int	vflag;	/* analog zu `pb' */
int	loc_vflag;
int	pflag;
int	loc_pflag;

getsym()
{
	register	index;
	register	symbol;
	register	cmp;
	char		alpha[IDENTLEN];

	BEGIN("getsym");
	keyword = FALSE;
	if ( ungetindex ) {
		--ungetindex;
		return ( unget[ungetindex] );
		}
	while ( (ch = getch()) == ' ' );
	if ( ch == EOF_CH )
		quit("Unexpected EOF in MODULA-2 source");
	if ( isalpha(ch) ) {
		index = 0;
		do {
			if ( index < IDENTLEN )
				alpha[index++] = ch;
			ch = getch();
			}
		while ( isalpha(ch) || isdigit(ch) );
		ungetch(ch);
		alpha[index] = '\0';
		if ( symbol = match(alpha) ) {
			keyword = TRUE;
			make_keyword();
			RETURN(symbol);
			}
		else
			RETURN(IDENT);
		}
	else if ( isdigit(ch) ) {		/* NUMBER */
		while ( isdigit(ch) || (ch >= 'A' && ch <= 'F') )
			ch = getch();
		if ( ch != 'H' && ch != 'B' && ch != 'C' &&
		     ch != 'h' && ch != 'b' && ch != 'c' )
			if ( ch == '.' && !inrange ) {
				ch = getch();
				while ( isdigit(ch) )
					ch = getch();
				if ( ch == 'E' || ch == 'e' )
					if ( (ch = getch()) != '+' &&
					      ch != '-' && !isdigit(ch) )
						error("error in real constant");
					else {
						while(isdigit(ch=getch()));
						ungetch(ch);
						}
				else
					ungetch(ch);
				}
			else
				ungetch(ch);
		RETURN(NUMBER);
		}
	else if ( ch == ':' ) {
		ch = getch();
		if ( ch == '=' )
			RETURN(BECOMES)
		else {
			ungetch(ch);
			RETURN(DOUBLEPOINT);
			}
		}
	else if ( ch == '<' ) {
		ch = getch();
		if ( ch == '=' )
			RETURN(CMP)
		else if ( ch == '>' )
			RETURN(CMP)
		else {
			ungetch(ch);
			RETURN(CMP);
			}
		}
	else if ( ch == '>' ) {
		ch = getch();
		if ( ch == '=' )
			RETURN(CMP)
		else {
			ungetch(ch);
			RETURN(CMP);
			}
		}
	else if ( ch == '=' ) {
		ch = getch();
		if ( ch == '<' )
			RETURN(CMP)
		else if ( ch == '>' )
			RETURN(CMP)
		else {
			ungetch(ch);
			RETURN(CMP);
			}
		}
	else if ( ch == '.' ) {
		ch = getch();
		if ( ch == '.' && inrange )
			RETURN(POINTS)
		else {
			ungetch(ch);
			RETURN(POINT)
			}
		}
	else if ( ssym[ch] != ILLEGAL ) {
		RETURN ( ssym[ch] );
		}
	else {
		error ( "Illegal Character found : `%c'",(char)ch );
		RETURN ( getsym() );
		}
}

ungetsym ( sym )
int	sym;
{
	BEGIN("ungetsym");
	if ( ungetindex > MAXUNGET )
		quit ( "Unrecoverable Error" );
	unget[ungetindex] = sym;
	++ungetindex;
#ifdef TRACE
	if ( tflag )
		fprintf(stderr,"ungetsym ( %d )\n" , sym );
#endif
	END
}

/*
 *	Die Nummerierung der Funktionen entpricht den Nummern
 *	der MODULA-2 Syntax in "Programming in Modula-2" von
 *	N. Wirth, Appendix 1
 */

/* 96 */

unit()
{
	putspace();
	sym = getsym();
	putline();
	switch ( sym ) {
	case IMPLEMENTATION :
		sym = check(MODULE,46);
		/* fall into */
	case MODULE :
		programmodule();
		break;
	case DEFINITION :
		sym = check(MODULE,46);
		defmodule();
		break;
	default :
		errnr(46);
		getsym();
		programmodule();
		break;
	}
	appendline();
	putline();
}

/* 94 */

programmodule()
{
	sym = check(IDENT,20);
	if ( (sym = getsym()) == LBRACKET ) {
		sym = getsym();
		expression();
		if ( sym != RBRACKET )
			errnr(22);
		else
			sym = getsym();
		}
	appendline();
	if ( sym != SEMICOLON )
		errnr(23);
	else
		sym = getsym();
	pushpos(step);
	while ( sym == IMPORT || sym == FROM )
		import();
	poppos();
	putspace();
	block();
	if ( sym != IDENT )
		errnr(20);
	check(POINT,53);
	appendline();
}

/* 88 */

defmodule()
{
	check(IDENT,20);
	check(SEMICOLON,23);
	appendline();
	pushpos(step);
	sym = getsym();
	while ( sym == IMPORT || sym == FROM )
		import();
	putspace();
	if ( sym == EXPORT )
	{	errnr(301);
		export();
	}
	putspace();
	while ( sym == CONST || sym == TYPE || sym == VAR ||
		sym == PROCEDURE )
		definition();
	poppos();
	putline();
	if ( sym != M_END )
		errnr(40);
	check(IDENT,20);
	check(POINT,53);
	appendline();
}

/* 90 */

definition()
{
	putrange();
	switch ( sym ) {
	case CONST :
		putrange();
		putline();
		pushpos(step);
		while ( (sym = getsym()) == IDENT ) {
			putline();
			constdecl();
			appendline();
			if ( sym != SEMICOLON ) {
				errnr(23);
				ungetsym ( sym );
				}
			}
		poppos();
		putspace();
		break;
	case TYPE :
		putrange();
		putline();
		pushpos(step);
		while ( (sym = getsym()) == IDENT )
		{	putline();
			sym = getsym();
			if (sym != CMP && sym != SEMICOLON && !keyword)
			{	sym = CMP;
				errnr(43);
			}
			if (sym == CMP )
			{	mark_ch();
				appendline();
				sym = getsym();
				type();
			}
			appendline();
			if (sym == IDENT || keyword)
			{	errnr(23);
				ungetsym(sym);
			}
			else if (sym != SEMICOLON)
				do
					sym = getsym();
				while (! keyword && sym != IDENT);
		}
		poppos();
		putspace();
		break;
	case VAR :
		putrange();
		putline();
		pushpos(step);
		while ( (sym = getsym()) == IDENT ) {
			putline();
			vardecl();
			appendline();
			if ( sym != SEMICOLON ) {
				errnr(23);
				ungetsym ( sym );
				}
			}
		poppos();
		putspace();
		break;
	case PROCEDURE :
		putline();
		prochead();
		appendline();
		if ( sym != SEMICOLON )
			errnr(23);
		else
			sym = getsym();
		putspace();
		break;
	}
}

constdecl ()
{
	check(CMP,43);	/* @@@ */
	mark_ch();
	sym = getsym();
	expression();
}

/* 87 */

import ()
{
	if ( sym == FROM ) {
		check(IDENT,20);
		check(IMPORT,61);
		}
	putline();
	sym = getsym();
	identlist(FALSE);
	appendline();
	if ( sym != SEMICOLON )
		errnr(23);
	else
		sym = getsym();
}

/* 86 */

export()
{
	if ( (sym = getsym()) == QUALIFIED )
		sym = getsym();
	putline();
	identlist(FALSE);
	appendline();
	if ( sym != SEMICOLON )
		errnr(23);
	else
		sym = getsym();
}

/* 74 */

block()
{
	pushpos(step);
	while ( sym == CONST || sym == TYPE || sym == VAR ||
		sym == PROCEDURE || sym == MODULE )
		declaration();
	poppos();
	putline();
	if ( sym != M_BEGIN && sym != M_END ) {
		errnr(38);
		sym = M_BEGIN;
		}
	if ( sym == M_BEGIN ) {
		pushpos(step);
		sym = getsym();
		statsequ();
		poppos();
		putline();
		}
	if ( sym != M_END )
		errnr(40);
	sym = getsym();
}

/* 75 */

declaration()
{
	switch ( sym ) {
	case CONST :
		putrange();
		putline();
		pushpos(step);
		while ( (sym = getsym()) == IDENT ) {
			putline();
			constdecl();
			appendline();
			if ( sym != SEMICOLON ) {
				errnr(23);
				ungetsym(sym);
				}
			}
		poppos();
		break;
	case TYPE :
		putrange();
		putline();
		pushpos(step);
		while ( (sym = getsym()) == IDENT ) {
			putline();
			check(CMP,43); /* @@@ */
			mark_ch();
			appendline();
			sym = getsym();
			type();
			appendline();
			if ( sym != SEMICOLON ) {
				errnr(23);
				ungetsym(sym);
				}
			}
		poppos();
		break;
	case VAR :
		putrange();
		putline();
		pushpos(step);
		while ( (sym = getsym()) == IDENT ) {
			putline();
			vardecl();
			appendline();
			if ( sym != SEMICOLON ) {
				errnr(23);
				ungetsym(sym);
				}
			}
		poppos();
		break;
	case PROCEDURE :
		putline();
		putrange();
		procdecl();
		appendline();
		if ( sym != SEMICOLON )
			errnr(23);
		else
			sym = getsym();
		putspace();
		break;
	case MODULE :
		putline();
		putrange();
		moddecl();
		appendline();
		if ( sym != SEMICOLON )
			errnr(23);
		else
			sym = getsym();
		putspace();
		break;
	}
}

/* 83 */

moddecl()
{
	check(IDENT,20);
	if ( (sym = getsym()) == LBRACKET ) {
		expression();
		if ( sym != RBRACKET )
			errnr(22);
		else
			sym = getsym();
		}
	appendline();
	pushpos(step);
	if ( sym != SEMICOLON )
		errnr(23);
	else
		sym = getsym();
	while ( sym == FROM || sym == IMPORT )
		import();
	if ( sym == EXPORT )
		export();
	poppos();
	block();
	if ( sym != IDENT )
		errnr(20);
	else
		sym = getsym();
}

/* 73 */

prochead()
{
	check(IDENT,20);
	loc_pflag = pflag || cmt_at_eoln;
	if ( (sym = getsym()) == LPAREN )
		formalpar();
}

/* 79 */

formalpar()
{
	int	notfirst;

	putrange();
	if ( loc_pflag )
		notfirst = FALSE;
	appendline();
	if ( sym != LPAREN )
		errnr(45);
	else
		sym = getsym();
	while ( sym == VAR || sym == IDENT ) {
		if ( loc_pflag && notfirst )
			putline();
		else if ( loc_pflag ) {
			justify_par();
			notfirst = TRUE;
			}
		fpsection();
		appendline();
		if ( sym != SEMICOLON && sym != RPAREN )
			errnr(23);
		else if ( sym == SEMICOLON )
			sym = getsym();
		}
	if (loc_pflag && notfirst) {
		poppos();
		putrange();
		}
	appendline();
	if ( sym != RPAREN )
		errnr(41);
	else
		sym = getsym();
	if ( sym == DOUBLEPOINT ) {
		sym = getsym();
		qualident();
		}
}

/* 81 */

fpsection()
{
	if ( sym == VAR )
		sym = getsym();
	identlist(loc_pflag);
	if ( loc_pflag )
		mark_ch();
	appendline();
	if ( sym != DOUBLEPOINT )
		errnr(37);
	else
		sym = getsym();
	formaltype();
}

/* 82 */

formaltype()
{
	if ( sym == ARRAY ) {
		if ( (sym = getsym()) == LBRACKET ) {
			errnr(129);
			while ( !keyword )
				sym = getsym(); /* bis zum OF */
			}
		appendline();
		if ( sym != OF )
			errnr(36);
		else
			sym = getsym();
		}
	qualident();
}

/* 72 */

procdecl()
{
	prochead();
	appendline();
	if ( sym != SEMICOLON )
		errnr(23);
	else
		sym = getsym();
	block();
	appendline();
	if ( sym != IDENT )
		errnr(20);
	else
		sym = getsym();
}

/* 59 */

statsequ()
{
	int	first = TRUE;

	if ( sym == M_END )
		return;
	do {
		if ( first )
			first = FALSE;
		else
			sym = getsym();
		statement();
		if ( sym == SEMICOLON )
			appendline();
		if ( sym == MODULE || sym == PROCEDURE ||
		     sym == CONST || sym == VAR || sym == TYPE ) {
			errnr(52);
			ungetsym(sym);
			ungetsym(SEMICOLON);
			ungetsym(IDENT);
			sym = M_END;
			break;
			}
		if ( sym != SEMICOLON && sym != M_END && sym != ELSE &&
		     sym != UNTIL && sym != ORCHAR && sym != ELSIF ) {
			sym = SEMICOLON;
			appendline();
			errnr(23);
			}
		}
	while ( sym == SEMICOLON );
}

/* 53 */

statement()
{
	switch ( sym ) {
	case IDENT : /* assignment or procedure call */
		putline();
		designator();
		if ( sym == CMP ) {
			errnr(26);
			sym = BECOMES;
			}
		if ( sym == BECOMES ) {
			sym = getsym();
			expression();
			}
		else if ( sym == LPAREN )
			actpars();
		break;
	case IF :
		putline();
		sym = getsym();
		expression();
		appendline();
		if_part();
		break;
	case CASE :
		putline();
		sym = getsym();
		expression();
		appendline();
		case_part();
		break;
	case WHILE :
		putline();
		sym = getsym();
		expression();
		appendline();
		while_part();
		break;
	case REPEAT :
		putline();
		pushpos(step);
		putrange();
		sym = getsym();
		statsequ();
		poppos();
		if ( sym != UNTIL )
			errnr(32);
		else
			sym = getsym();
		putline();
		expression();
		break;
	case LOOP :
		putline();
		pushpos(step);
		sym = getsym();
		statsequ();
		poppos();
		putline();
		if ( sym != M_END )
			errnr(40);
		else
			sym = getsym();
		break;
	case FOR :
		putline();
		check(IDENT,20);
		check(BECOMES,26);
		sym = getsym();
		expression();
		if ( sym != TO )
			errnr(56);
		else
			sym = getsym();
		expression();
		if ( sym == BY ) {
			sym = getsym();
			expression();
			}
		appendline();
		for_part();
		break;
	case WITH :
		putline();
		sym = getsym();
		designator();
		appendline();
		pushpos(step);
		if ( sym != DO )
			errnr(34);
		else
			sym = getsym();
		statsequ();
		poppos();
		putline();
		if ( sym != M_END )
			errnr(40);
		else
			sym = getsym();
		break;
	case EXIT :
		putline();
		sym = getsym();
		break;
	case M_RETURN :
		putline();
		sym = getsym();
		if ( sym == NUMBER || sym == IDENT || sym == PLUS ||
		     sym == MINUS || sym == LBRACE || sym == NOT ||
		     sym == LPAREN || sym == STRING )
			expression();
		break;

	/* diverse Fehlerfaelle */

	case THEN :
		putline();
		error("error in IF statement");
		appendline();
		if_part();
		break;
	case OF :
		putline();
		errnr(35);
		appendline();
		case_part();
		break;
	case DO :
		putline();
		errnr(33);
		appendline();
		while_part();
		break;
	case TO :
		putline();
		errnr(60);
		appendline();
		sym = getsym();
		expression();
		if ( sym == BY ) {
			sym = getsym();
			expression();
			}
		appendline();
		for_part();
		break;
	case BY :
		putline();
		errnr(60);
		expression();
		appendline();
		for_part();
		break;
	}
}

if_part ()
{
	if ( sym != THEN )
		errnr(28);
	else
		sym = getsym();
	if ( sym != ELSIF && sym != ELSE ) {	
		pushpos(step);
		statsequ();
		poppos();
		}
	while ( sym == ELSIF ) {
		putline();
		sym = getsym();
		expression();
		appendline();
		if ( sym != THEN )
			errnr(28);
		else
			sym = getsym();
		if ( sym != ELSIF && sym != ELSE ) {
			pushpos(step);
			statsequ();
			poppos();
			}
		}
	if ( sym == ELSE ) {
		putline();
		sym = getsym();
		pushpos(step);
		statsequ();
		poppos();
		}
	putline();
	if ( sym != M_END )
		errnr(40);
	else
		sym = getsym();
}

case_part()
{
	if ( sym != OF )
		errnr(36);
	else {
		inrange = TRUE;
		sym = getsym();
		}
	if (sym != ORCHAR)
	{	pushpos(2);
		putline();
		caselabellist();
		appendline();
		if ( sym != DOUBLEPOINT )
			errnr(37);
		else
			sym = getsym();
		poppos();
		pushpos(step);
		pushpos(step);
		statsequ();
		poppos();
		poppos();
	}
	inrange = FALSE;
	while ( sym == ORCHAR ) {
		inrange = TRUE;
		putline();
		sym = getsym();
		if (sym == ELSE || sym == ORCHAR || sym == M_END)
			continue;
		appendline();
		pushpos(step);
		case_();
		poppos(step);
		inrange = FALSE;
		}
	if ( sym == ELSE ) {
		putline();
		pushpos(step);
                sym = getsym();
		statsequ();
		poppos();
		}
	putline();
	if ( sym != M_END )
		errnr(40);
	else
		sym = getsym();
}

/* 65 */

case_()
{
	caselabellist();
	appendline();
	if ( sym != DOUBLEPOINT )
		errnr(37);
	else
		sym = getsym();
	pushpos(step);
	statsequ();
	poppos();
}

/* 37 */

caselabellist()
{
	caselbls();
	while ( sym == COMMA ) {
		inrange = TRUE;
		sym = getsym();
		caselbls();
		}
}

/* 38 */

caselbls()
{
	expression();
	if ( sym != RBRACE && sym != COMMA && sym != DOUBLEPOINT && sym != POINTS ) {
		sym = POINTS;
		errnr(59);
		}
	if ( sym == POINTS ) {
		sym = getsym();
		appendline();
		expression();
		}
	inrange = FALSE;
}

while_part()
{
	if ( sym != DO )
		errnr(34);
	else
		sym = getsym();
	pushpos(step);
	statsequ();
	poppos(step);
	putline();
	if ( sym != M_END )
		errnr(40);
	else
		sym = getsym();
}

for_part ()
{
	if ( sym != DO )
		errnr(34);
	else
		sym = getsym();
	pushpos(step);
	statsequ();
	poppos(step);
	putline();
	if ( sym != M_END )
		errnr(40);
	else
		sym = getsym();
}

/* 52 */

actpars()
{
	if ( sym != LPAREN )
		errnr(45);
	else
		sym = getsym();
	if ( sym != RPAREN )
		explist();
	appendline();
	if ( sym != RPAREN )
		errnr(41);
	else
		sym = getsym();
}

/* 46 */

explist()
{
	expression();
	while ( sym == COMMA ) {
		appendline();
		sym = getsym();
		expression();
		}
}

/* 47 */

expression()
{
	simpleexpr();
	if ( sym == BECOMES ) {
		errnr(27);
		sym = CMP;
		}
	if ( sym == CMP || sym == IN ) {
		sym = getsym();
		appendline();
		simpleexpr();
		}
}

/* 48 */

simpleexpr()
{
	if ( sym == PLUS || sym == MINUS ) {
		sym = getsym();
		appendline();
		}
	term();
	while ( sym == PLUS || sym == MINUS || sym == OR ) {
		sym = getsym();
		appendline();
		term();
		}
}

/* 49 */

term()
{
	factor();
	while ( sym == TIMES || sym == SLASH || sym == DIV ||
		sym == MOD || sym == AND ) {
		sym = getsym();
		appendline();
		factor();
		}
}

/* 50 */

factor()
{
	switch ( sym ) {
	case NUMBER :
		appendline();
		sym = getsym();
		break;
	case STRING :
		appendline();
		sym = getsym();
		break;
	case IDENT :
		designator();	/* leicht erweiterte Syntax */
		/* fall into */
	case LBRACE :
		if ( sym == LBRACE ) {
			inrange = TRUE;
			sym = getsym();
			if ( sym != RBRACE ) {
				inrange = TRUE;
				caselbls();	/* element */
				while ( sym == COMMA ) {
					inrange = TRUE;
					sym = getsym();
					caselbls();  /* element */
					}
				appendline();
				if ( sym != RBRACE )
					errnr(58);
				else
					sym = getsym();
				}
			else {
				appendline();
				sym = getsym();
				}
			inrange = FALSE;
			}
		else if ( sym == LPAREN )
			actpars();
		break;
	case LPAREN :
		sym = getsym();
		expression();
		appendline();
		if ( sym != RPAREN )
			errnr(41);
		else
			sym = getsym();
		break;
	case NOT :
		sym = getsym();
		appendline();
		factor();
		break;
	default :
		errnr(27);
		while ( !keyword && sym != SEMICOLON && sym != COMMA &&
			sym != POINTS && sym != LPAREN && sym != LBRACE ) {
			putline();
			sym = getsym();
			}
		break;
	}
}

/* 28 */

identlist (flag)
int	flag;
{
	/* first putline() has been done */
	appendline();
	if ( sym != IDENT )
		errnr(20);
	else
		sym = getsym();
	while ( sym == COMMA ) {
		appendline();
		check(IDENT,20);
		if ( flag )
			putline();
		else
			appendline();
		sym = getsym();
		}
}

/* 44 */

vardecl()
{
	loc_vflag = vflag || cmt_at_eoln;
	identlist(loc_vflag);
	mark_ch();
	appendline();
	if ( sym != DOUBLEPOINT )
		errnr(37);
	else
		sym = getsym();
	type();
}

/* 24 */

type()
{
	switch ( sym ) {
	case IDENT :
	case LPAREN :
	case LBRACKET :
		simpletype();
		break;
	case ARRAY :
		sym = getsym();
		simpletype();
		while ( sym == COMMA ) {
			sym = getsym();
			simpletype();
			}
		appendline();
		if ( sym != OF )
			errnr(36);
		else
			sym = getsym();
		type();
		break;
	case RECORD :
		pushpos(step);
		putline();
		pushpos(step);
		sym = getsym();
		fieldsequ();
		poppos();
		putline();
		if ( sym != M_END )
			errnr(40);
		else
			sym = getsym();
		poppos();
		break;
	case SET :
		check(OF,36);
		sym = getsym();
		simpletype();
		break;
	case POINTER :
		check(TO,56);
		appendline();
		sym = getsym();
		type();
		break;
	case PROCEDURE :
		if ( (sym = getsym()) == LPAREN ) {
			sym = getsym();
			if ( sym == VAR || sym == ARRAY || sym == IDENT ) {
				if ( sym == VAR )
					sym = getsym();
				formaltype();
				while ( sym == COMMA ) {
					sym = getsym();
					if ( sym == VAR )
						sym = getsym();
					formaltype();
					}
				}
			if ( sym != RPAREN )
				errnr(41);
			else
				sym = getsym();
			if ( sym == DOUBLEPOINT ) {
				sym = getsym();
				qualident();
				}
			}
		break;
	default :
		errnr(44);
		while ( sym != SEMICOLON && !keyword )
			sym = getsym();
		break;
	}
}

/* 32 */

fieldsequ()
{
	fieldlist();
	while ( sym == SEMICOLON ) {
		appendline();
		sym = getsym();
		fieldlist();
		}
}

/* 33 */

fieldlist()
{
	if (sym == IDENT || sym == CASE)
		putline();
	switch ( sym ) {
	case IDENT :
		identlist(FALSE);
		appendline();
		if ( sym != DOUBLEPOINT )
			errnr(37);
		else
			sym = getsym();
		type();
		break;
	case CASE :
		sym = getsym();
		if (sym != DOUBLEPOINT)
			qualident();	/* erweiterte Syntax */
		appendline();
		if (sym != DOUBLEPOINT)
			errnr(300);
		if ( sym == DOUBLEPOINT ) {
			sym = getsym();
			qualident();
			}
		appendline();
		if ( sym != OF )
			errnr(36);
		else {
			inrange = TRUE;
			sym = getsym();
			}

		/*
		 *	variant : jedoch beim ersten Mal anders formatiert
		 */

		if (sym != ORCHAR)
		{	pushpos(2);
			putline();
			caselabellist();
			appendline();
			if ( sym != DOUBLEPOINT )
				errnr(37);
			else
				sym = getsym();
			poppos();
			pushpos(step);
			pushpos(step);
			fieldsequ();	/* fuehrt zu Beginn ein putline aus */
			poppos(); poppos();
		}

		inrange = FALSE;
		while ( sym == ORCHAR ) {
			inrange = TRUE;
			putline();
			sym = getsym();
			if (sym == M_END || sym == ORCHAR || sym == ELSE)
				continue;
			appendline();
			variant();
			inrange = FALSE;
			}
		if ( sym == ELSE ) {
			putline();
			pushpos(step);
			sym = getsym();
			fieldsequ();
			poppos();
			}
		putline();
		if ( sym != M_END )
			errnr(40);
		else
			sym = getsym();
		break;
	case ORCHAR :
	case M_END :
	case SEMICOLON :
		break;
	default :
		error("error in RECORD type");
		while ( !keyword && sym != SEMICOLON && sym != ORCHAR )
			sym = getsym();
		break;
	}
}

/* 36 */

variant()
{
	caselabellist();
	appendline();
	if ( sym != DOUBLEPOINT )
		errnr(37);
	else
		sym = getsym();
	pushpos(step*2);
	fieldsequ();
	poppos();
}

/* 26 */

simpletype()
{
	switch ( sym ) {
	case LPAREN :
		sym = getsym();
		identlist(FALSE);
		if ( sym != RPAREN )
			errnr(41);
		else
			sym = getsym();
		break;
	case IDENT :
		qualident();
		if (sym != LBRACKET)
			break;
		/* fall into */
	case LBRACKET :
		inrange = TRUE;
		sym = getsym();
		expression();
		if ( sym != POINTS )
			errnr(59);
		else
			sym = getsym();
		expression();
		inrange = FALSE;
		if ( sym != RBRACKET )
			errnr(22);
		else
			sym = getsym();
		break;
	default :
		errnr(49);
		while ( !keyword && sym != SEMICOLON && sym != ORCHAR
			&& sym != COMMA )
			sym = getsym();
		break;
	}
}

/* 45 */

designator ()
{
	qualident();
	while ( sym == POINT || sym == LBRACKET || sym == PTRCH ) {
		appendline();
		switch ( sym ) {
		case POINT :
			if ( (sym = getsym()) != IDENT )
				errnr(20);
			else {
				appendline();
				sym = getsym();
				}
			break;
		case LBRACKET :
			sym = getsym();
			explist();
			appendline();
			if ( sym != RBRACKET )
				errnr(22);
			else
				sym = getsym();
			break;
		case PTRCH :
			appendline();
			sym = getsym();
			break;
		}
		}
}

/* 11 */

qualident ()
{
	appendline();
	if ( sym != IDENT )
		errnr(20);
	else
		sym = getsym();
	while ( sym == POINT ) {
		appendline();
		sym = getsym();
		appendline();
		if ( sym != IDENT )
			errnr(20);
		else
			sym = getsym();
		}
}

check(exp,err)
int	exp;
int	err;
{
	int	merkesymbol;

	if ( (sym = getsym()) != exp ) {
		errnr(err);
		merkesymbol = sym;
		if ( (sym = getsym()) == exp )
			return(sym);
		else {
			ungetsym(sym);
			sym = merkesymbol;
			}
		}
	return(sym);
}
