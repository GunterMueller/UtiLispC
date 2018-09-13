#include	<stdio.h>
#include "constdef.h"
#include "defvar.h"
#ifndef HAVE_DLOPEN
#ifndef __SVR4
#include	<a.out.h>
#endif
#endif
#include	<sys/file.h>
#ifdef HAVE_DLOPEN
#include <dlfcn.h>
#endif

#undef DEBUG

static int	pid;
extern int codeareasize;
#ifdef HAVE_DLOPEN
char *make_base_name(char *str)
{
  int last_i,i,len=strlen(str);
  static char buf[64];

  for(last_i=i=0;i<len;i++)
    if(str[i]=='/') last_i=i+1;
  strcpy(buf,str+last_i);
  for(i=len-last_i-1;i>=0;i--)
    if(buf[i]=='.') {buf[i]=0;break;}
  return buf;
}

const char c_chartable[]="________________________________________________0123456789_______ABCDEFGHIJKLMNOPQRSTUVWXYZ______abcdefghijklmnopqrstuvwxyz_____________________________________________________________________________________________________________________________________";

char *translate(char *str,const char *table)
{
  int c,i;
  static char ret[256];

  for(i=0;(c=str[i])!=0;i++)
    ret[i] = table[c];
  ret[i]=0;
  return ret;
}

/**
 * 文字列長のチェックをやっていないので非常に危険
 * load に失敗したかどうかをプログラムで捕まえることができない．
 * loadに成功しても nil を返す場合があるので
 */
WORD proglink(int n, char *first[],char **fp)
{
  char	tmpfile[1024];
  char  start_label[1024];
  char	ldcommand[4096];
  char	ldfiles[4096];
  char	**argp = first;
  char  *startfile=first[0];
  int	sysret;
  register char	*codep, *p, *q;
  void *handle;
  static int loadcount=0;
  int tmpfname(char *);

  {
    WORD ret=nil;
    int i;
    for(i=0;i<n;i++){
      char *filename=first[i];
#if 0
      fprintf(stderr,"dlopen %s \n",filename);
#endif
      if((handle = dlopen(filename, RTLD_LAZY))==NULL){
	char strbuf[1024];
	int wsize;
	snprintf(strbuf,sizeof(strbuf),"Failed in dlopen: %s\n%s",filename,dlerror());
	strbuf[sizeof(strbuf)-1]='\0';
	l(0)=alstring(strbuf,fp);
	ioerr(l(0),fp-1);
      }
      sprintf(start_label,"init_code_%s",
	    translate(make_base_name(filename),c_chartable));
      codep = (char *)dlsym(handle, start_label);
#if 0
      fprintf(stderr,"codep of dlopen %s = 0x%x\n",filename,codep);
#endif
      if(codep!=NULL){
        ret=(WORD)(*(int (*)())codep)(fp);
      }
      else{
	dlclose(handle);
	if((handle = dlopen(filename, RTLD_LAZY|RTLD_GLOBAL))==NULL){
	  fprintf(stderr,"Failed in dlopen: %s\n%s\n",filename,dlerror());
	  return nil;
	}
      }
    }
    return ret;
  }
}
  
#else /* HAVE_DLOPEN */
WORD proglink(int n, char *first[],char **fp)
{
	register int	fd;
#ifdef mips
	struct filehdr filehdr;
	struct aouthdr aouthdr;
#else
	struct exec	header;
#endif
	char	tmpfile[64];
	char	ldcommand[4096];
	char	ldfiles[4096];
	char	**argp = first;
	int	sysret;
	register char	*codep, *p, *q;
	WORD str;

	pid = getpid();
	p = ldfiles;
	while ( --n >= 0 ) {
		for ( q = *argp++; *q != '\0'; *p++ = *q++ );
		*p++ = ' ';
	}
	*p = '\0';
	if ( tmpfname(tmpfile) == -1 ) return (WORD)-1;
	checkstr(str,value(utilisp_path));
#ifndef NOMALLOC
	sprintf(ldcommand, "ld -Bstatic -A %s %s -lc -o %s", stringcodes(str), ldfiles, tmpfile);
#ifdef DEBUG
	fputs(ldcommand,stderr);
	putc('\n',stderr); 
#endif /* DEBUG */
	if((sysret=system(ldcommand))!=0)
		return nil;
	fd = open(tmpfile, 0);
#ifdef mips
	read(fd, &filehdr, sizeof(filehdr));
	read(fd, &aouthdr, sizeof(aouthdr));
#else /* mips */
	read(fd, &header, sizeof(header));
#endif /* mips */
	close(fd);
#ifdef mips
	codep = (char *)malloc(aouthdr.tsize + aouthdr.dsize + aouthdr.bsize);
#else /* mips */
	codep = (char *)malloc(header.a_text + header.a_data + header.a_bss);
#endif /* mips */
#else /* NOMALLOC */
	codep=codeareatop+codeareasize;
#endif /* NOMALLOC */
#ifdef SPARC
	sprintf(ldcommand, "ld -Bstatic -A %s -T %x %s -lc -o %s", stringcodes(str), codep, ldfiles, tmpfile);
#else
	sprintf(ldcommand, "ld -A %s -T %x %s -lc -o %s", stringcodes(str), codep, ldfiles, tmpfile);
#endif
#ifdef DEBUG
	fputs(ldcommand,stderr);
	putc('\n',stderr); 
#endif /* DEBUG */
	if((sysret=system(ldcommand))!=0)
		return nil;
	fd = open(tmpfile, 0);
#ifdef mips
	read(fd, &filehdr, sizeof(filehdr));
	read(fd, &aouthdr, sizeof(aouthdr));
	lseek(fd, N_TXTOFF(filehdr,aouthdr), 0);
#ifdef NOMALLOC
        alloc_codearea(aouthdr.tsize + aouthdr.dsize + aouthdr.bsize,fp);
#endif /* NOMALLOC */
	read(fd, codep, aouthdr.tsize+aouthdr.dsize);
	bzero(codep + aouthdr.tsize+aouthdr.dsize, aouthdr.bsize);
#else /* mips */
	read(fd, &header, sizeof(header));
#if 0
	printf("headersize=0x%x,N_TXTADDR=0x%x,N_DATAADDR=0x%x,N_TXTOFF=0x%x,N_DATOFF=0x%x,N_SYMOFF=0x%x,N_STROFF=0x%x\n",sizeof(header),N_TXTADDR(header),N_DATADDR(header),N_TXTOFF(header),N_DATOFF(header),N_SYMOFF(header),N_STROFF(header));
#endif
	lseek(fd, N_TXTOFF(header), 0);
#ifdef NOMALLOC
	alloc_codearea(header.a_text + header.a_data + header.a_bss,fp);
#endif /* NOMALLOC */
	read(fd, codep, header.a_text + header.a_data);
	bzero(codep + header.a_text + header.a_data, header.a_bss);
#endif /* mips */
	close(fd);
	unlink(tmpfile); 
#ifdef SPARC
	return (WORD)(*(int (*)())codep)(fp);
#else
	return (WORD)(*(int (*)())codep+sizeof(header))(fp);
#endif
}
#endif /* HAVE_DLOPEN*/

int tmpfname(char *s)
{
	register int	fd;
	register int	count = 1000;	

retry:
	sprintf(s, "/tmp/uti%d", pid);
	if ( (fd = open(s, O_WRONLY | O_CREAT | O_EXCL, 0600)) < 0 ) {
		if ( --count <= 0 ) {
			fputs("cannot allocate temporally file name\n",stderr);
			return -1;
		}
		pid += 1000;
		goto retry;
	}
	close(fd);
	return 0;
}
