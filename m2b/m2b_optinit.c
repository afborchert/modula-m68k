/* (c) Andreas Borchert 1983, University of Ulm */

/*
 * Initialisierung der Optionstabelle
 */
#include	<stdio.h>
#include	"pb_mac.h"
#include	"pb_optstr.h"
#include	"pb_opttab.h"

/*
 * die Flags/Variablen, die von den Optionen beeinflusst werden
 */

extern	char	*pb_name;
extern	int	comcolumn;
extern	int	hflag;
extern	int	kflag;
extern	int	linelength;
extern	int	oflag;
extern	int	pflag;
extern	int	step;
extern	int	uflag;
extern	int	vflag;
extern	char	*in_file;
extern	char	*out_file;

/*
 * der Init des `union'-Teils der OPTSTR ( geht n i c h t per
 * Initialisizer ! )
 */

optinit ()
{
	opttab[0].opt_spez.s.str_addr = &pb_name;
	opttab[0].opt_spez.s.str_dflt = "pb";
	opttab[1].opt_spez.i.arg_addr = &comcolumn;
	opttab[1].opt_spez.i.arg_dflt = 40;
	opttab[1].opt_spez.i.arg_low  = 10;
	opttab[1].opt_spez.i.arg_high = 140;
	opttab[2].opt_spez.f.addr     = &hflag;
	opttab[2].opt_spez.f.dflt     = FALSE;
	opttab[3].opt_spez.f.addr     = &kflag;
	opttab[3].opt_spez.f.dflt     = FALSE;
	opttab[4].opt_spez.i.arg_addr = &linelength;
	opttab[4].opt_spez.i.arg_dflt = 79;
	opttab[4].opt_spez.i.arg_low  = 0;
	opttab[4].opt_spez.i.arg_high = 140;
	opttab[5].opt_spez.f.addr     = &oflag;
	opttab[5].opt_spez.f.dflt     = FALSE;
	opttab[6].opt_spez.f.addr     = &pflag;
	opttab[6].opt_spez.f.dflt     = FALSE;
	opttab[7].opt_spez.i.arg_addr = &step;
	opttab[7].opt_spez.i.arg_dflt = 3;
	opttab[7].opt_spez.i.arg_low  = 0;
	opttab[7].opt_spez.i.arg_high = 20;
	opttab[8].opt_spez.f.addr     = &uflag;
	opttab[8].opt_spez.f.dflt     = FALSE;
	opttab[9].opt_spez.f.addr     = &vflag;
	opttab[9].opt_spez.f.dflt     = FALSE;
	opttab[10].opt_spez.s.str_addr= &in_file;
	opttab[10].opt_spez.s.str_dflt= NULL;
	opttab[11].opt_spez.s.str_addr= &out_file;
	opttab[11].opt_spez.s.str_dflt= NULL;
	return ( 12 );
}
