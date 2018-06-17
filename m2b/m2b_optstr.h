/* (c) Andreas Borchert 1983, University of Ulm */

/*
 * Struktur fuer die Optionen, mit deren Hilfe unabhaengig vom
 * speziellen Kommando die Argumente bearbeitet werden koennen.
 */

typedef struct {

	char	*opt_name;	/* der Name der Option */
	char	opt_type;	/* Typ der Option */
	int	opt_used;	/* Counter */
	int	opt_repl;	/* regelt mehrfache Benutzung */
	union {
		struct  {
			int	*addr;		/* bei Typ 'f' */
			int	dflt;
			} f;
		struct	{
			int	*arg_addr;	/* bei Typ 'i' */
			int	arg_dflt;
			int	arg_low;	/* untere Grenze */
			int	arg_high;	/* obere Grenze */
			} i;
		struct	{
			char	**str_addr;	/* bei Typ 's' */
			char	*str_dflt;
			} s;
		} opt_spez;
	} OPTSTR;
