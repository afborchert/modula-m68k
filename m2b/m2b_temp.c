/* (c) Andreas Borchert 1983, University of Ulm */

/*
 * Management des Tempfiles
 */

#include	<stdio.h>
#include	"pb_mac.h"
#include	"pb_temp.h"

extern char * mktemp();

static	FILE	*tmpfp;
static	TMPSTR	tmp_buf;			/* Ein Record des Tempfiles */
static	char	*tfname;		/* Name des Tempfiles */
static	int	topen = 0;		/* Selbstkontrolle */

/*
 * wird ein einziges Mal und vor allen anderen Routinen dieses
 * Moduls aufgerufen ( mit Kontrolle )
 */

temp_open ()
{
	if ( topen )
		fatal("Illegal use of `temp_open'.");
	tfname = mktemp("/tmp/pbXXXXX");
	ignore(close(creat(tfname,0600)));
	if ( (tmpfp=fopen(tfname,"w")) == NULL )
		quit("Can't allocate tempfile.");
	topen = TRUE;
}

/*
 * in der ersten Komponente der TMPSTR ist abgespeichert, wie viel
 * Platz die Struktur tatsaechlich benoetigt. Das ist wegen der
 * variablen Stringlaenge von tmp_buf.ts_line
 */

TMPSTR	*
t_read ()
{
	if ( !topen )
		fatal("Illegal use of `t_read'.");
	if ( fread(&tmp_buf.ts_recsize,sizeof(int),1,tmpfp) == 0 )
		return(NULL);
	if ( fread(&tmp_buf.ts_prelines,tmp_buf.ts_recsize,1,tmpfp) )
		return(&tmp_buf);
	else
		return(NULL);
}

temp_write ( tmp_buf )
TMPSTR	*tmp_buf;
{
	if ( !topen )
		fatal("Illegal use of `temp_write'.");
	if ( fwrite(tmp_buf,tmp_buf->ts_recsize+sizeof(int),1,tmpfp) )
		return;
	else
		quit("Write error.");
}

temp_rewind ( type )
char	*type;
{
	if ( !topen )
		fatal("Illegal use of `temp_rewind'.");
	if ( freopen ( tfname , type , tmpfp ) == NULL )
		quit("Read/Write Error");
}

temp_close ()
{
	if ( !topen )
		fatal("Illegal use of `temp_close'.");
	topen = 0;
	ignore ( fclose ( tmpfp ) );
	ignore ( unlink ( tfname ) );
}

temp_topen ()
{
	return ( topen );
}
