#ifdef MPROF
/*
 *	mprof - service routines for reffile reading
 */
#else
/*
 *	mdb - service routines for reffile reading
 */
#endif

#include	<sys/types.h>
#include	<sys/dir.h>
#include	<stdio.h>
#include	<ar.h>
#ifdef MPROF
#include	"mp.h"
#include	"mp_ref.h"
#else
#include	"mdb_ref.h"
#include	"mdb_tree.h"
#endif
#ifdef MPROF
#define	my_perror	perror
#endif


#if SYSV || BSD42
#define	rindex(s,c)	strrchr(s,c)
#define index(s,c)	strchr(s,c)
extern char * strrchr();
extern char * strchr();
#else
extern char * rindex();
extern char * index();
#endif

extern char * getenv();
extern char * strsave();

extern struct module mod[];
extern int a_mod;

static FILE *fp = NULL;
static int highbyte;
static word curword;

struct module * findmod();

char * searchfile(modname, suffix)
	char * modname;
	char * suffix;
{	DIR * dir;
	static struct direct *dirbuf;
	static int dummy = 0;		/* rindex needs a 0-byte */
	char *ptr;
	char * srcpath = getenv("SRCPATH");
	char * cp = NULL;
	char buf[BUFSIZ];
	struct module * mp;
	char * srcname;

#if defined(MPROF) && ! (SYSV || BSD42)
	srcname = NULL;
#else
	if (srcname = (mp = findmod(modname))? mp->m_file: NULL)
	{	srcname = strsave(srcname);
		if ((cp = rindex(srcname, '.')))
			* cp = '\0';
		cp = NULL;
	}
#endif
	do
	{	if (srcpath)
		{
			if (* srcpath == ':')
				++srcpath;
			cp = index(srcpath, ':');
			if (cp)
				* cp = '\0';
			if ((dir = opendir(srcpath)) == NULL)
			{	my_perror(srcpath);
				if (cp) * cp = ':';
				continue;
			}
			strcpy(buf, srcpath);
		}
		else if ((dir = opendir("." )) == NULL)
	{
				return NULL;
			}
		else
			strcpy(buf, ".");
		while (dirbuf = readdir(dir))
		{	
			if (! dirbuf->d_ino) continue;
			if ((ptr = rindex(dirbuf->d_name,'.')) == NULL)
				continue;
			if ( strcmp(++ptr, suffix) )
			continue;
			--ptr;
			*ptr = '\0';
#if SYSV || BSD42
			if ((!srcname || strncmp(srcname, dirbuf->d_name, 11)) &&
			    strncmp(modname, dirbuf->d_name, 11)) continue;
#else
			if ((!srcname || strncmp(srcname, dirbuf->d_name, 8)) &&
			    strncmp(modname, dirbuf->d_name, 8)) continue;
#endif
			closedir(dir);
			*ptr = '.';
			if (cp) * cp = ':';
			strcat(buf, "/");
			strcat(buf, dirbuf->d_name);
			if (srcname)
				free(srcname);
			return strsave(buf);
		}
		closedir(dir);
		if (cp) * cp = ':';
	}
	while (srcpath && (srcpath = index(srcpath, ':')));
	if (srcname)
		free(srcname);
	return NULL;
}

/*
 *	look for modname with given suffix in an archive
 *	if modname found prepare "refopen" for opening
 *	the archive
 */

int ar_open = 0;	/* if on: we read from an archive file */
char * ar_filename;	/* name of the file in the archive file */
char * ar_name;		/* name of the archive file */
long ar_seek;		/* position of the file in the archive */

char * archivesearch(archive, modname, suffix)
	char * archive;
	char * modname;
	char * suffix;
{	FILE * fp;
#if SYSV || BSD42
	char magic[SARMAG];
#else
	int magic;
#endif
	static struct ar_hdr header;
	int fsize;
	char * ptr;

	ar_open = 0;
	if ((fp = fopen(archive, "r")) == NULL)
		return NULL;
#if SYSV || BSD42
	if (fread(magic, SARMAG, 1, fp) != 1)
#else
	if (fread(& magic, sizeof(int), 1, fp) != 1)
#endif
	{	fclose(fp);
		return NULL;
	}
#if SYSV || BSD42
	if (strncmp(magic, ARMAG, SARMAG))
#else
	if (magic != ARMAG)
#endif
	{	fclose(fp);
		return NULL;
	}
	while (fread(& header, sizeof(header), 1, fp) &&
#if SYSV || BSD42
	       ((fsize = atoi(header.ar_size)) % 2? ++fsize: 1) &&
#else
	       (header.ar_size % 2 ? ++header.ar_size : 1) &&
#endif
	       (ar_seek = ftell(fp)) &&
#if SYSV || BSD42
	       fseek(fp, fsize, 1) != -1)
#else
	       fseek(fp, header.ar_size, 1) == 0)
#endif
	{
#if SYSV
		if ((ptr = rindex(header.ar_name, '/')) == NULL) continue;
		* ptr = '\0';
#endif
#if BSD42
		if ((ptr = index(header.ar_name, ' ')) == NULL) continue;
		* ptr = '\0';
#endif
		if ( (ptr = rindex(header.ar_name,'.')) == NULL ) continue;
		if (strcmp(++ptr, suffix)) continue;
		--ptr;
		*ptr = '\0';
#if SYSV || BSD42
		if (strncmp(modname, header.ar_name, 11)) continue;
#else
		if (strncmp(modname, header.ar_name, 8)) continue;
#endif
		fclose(fp);
		*ptr = '.';
		ar_filename = strsave(header.ar_name);
		ar_name = strsave(archive);
		ar_open = 1;
		return header.ar_name;
	}
	fclose(fp);
	return NULL;
}

/*
 * return -1 on failure
 */
int refopen(filename)
	char *filename;
{
	highbyte = 0;
	if (ar_open && strcmp(filename, ar_filename) == 0)
	{	if ( (fp = fopen(ar_name, "r")) == NULL)
			return -1;
		if (fseek(fp, ar_seek, 0))
		{	fclose(fp);
			return -1;
		}
	}
	else if ( (fp = fopen(filename,"r")) == NULL )
		return -1;
	else
		ar_open = 0;
	return 0;
}

refclose()
{
	ar_open = 0;
	if (fp)
		return fclose(fp);
	else
		return -1;
}

/*
 *	despite to common sense the bytes are packed reverse in every word
 */

char readref()
{
	highbyte = !highbyte;
	if (! highbyte)
		return curword / 0400;
	else {
		fread(&curword, sizeof(word), 1, fp);
		return curword % 0400;
		}
}

symbol getsym()
{	symbol sym;

	sym = (symbol) readref();
	if (sym > Undef )
		sym = Undef;
	return sym;
}

word getnum()
{	word w;

	w = (word) readref();
	w = w * 0400 + (word) readref();
	return w;
}

getident(ptr, len)
	char	*ptr;
	int	len;
{	char	ch;

	while ( len-- && (ch = readref()) )
		*ptr++ = ch;
	if (++len)
		*ptr++ = '\0';
	else while ( (ch = readref()) )
		;
}

struct module * findmod(modname)
	char * modname;
{	register struct module * mp;

	for (mp = mod; mp-mod < a_mod; ++ mp)
		if (strcmp(modname, mp->m_name) == 0)
			return mp;
	return NULL;
}
