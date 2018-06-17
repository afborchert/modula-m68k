/* (c) Andreas Borchert 1983, University of Ulm */

/*
 * diverse Macros fuer pb...
 */

#define	MAX(a,b)	((a)>(b) ? a : b)
#define	MIN(a,b)	((a)<(b) ? a : b)
#define isvisible(ch)	(' '<=(char)(ch) && (char)(ch)<='~')
#define	TRUE		(-1)
#define	FALSE		(0)
#define	EOF_CH		('\0')

#ifdef lint
#	define	ignore(a)	Ignore((char *) (a))
#	define	ignorf(a)	Ignorf((int (*)())(a))
#else
#	define	ignore(a)	a
#	define	ignorf(a)	a
#endif

#ifdef DEBUG
#	define	BEGIN(string)	nm_push(string);
#	define	END		{ nm_pop(); }
#	define	RETURN(expr)	{ nm_pop(); return(expr);}
#else
#	define	BEGIN(string)	/***/
#	define	END		/***/
#	define	RETURN(expr)	return(expr);
#endif
