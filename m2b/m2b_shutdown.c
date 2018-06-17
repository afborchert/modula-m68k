/* (c) Andreas Borchert 1983, University of Ulm */

/*
 * Geregelte Terminierung von pb im Fehlerfall
 */

extern	int	fatalerror;

shut_down ()
{
	if ( fatalerror )
		alarm ( (unsigned) 20 ); /* garantierter Abbruch */
	in_end();
	if ( !fatalerror )
		pass2_end();
	else
		p2_fast_end();
	if ( temp_topen() )
		temp_close();
}
