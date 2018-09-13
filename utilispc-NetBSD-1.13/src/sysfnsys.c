#include <stdio.h>
#include <time.h>
#include <errno.h>
#include "constdef.h"
#include "defvar.h"

WORD call_f(na,fp)
WORD *fp;
{
  WORD string;
  int rval, pid;

  if(na!=1)parerr();
#ifdef AP1000
  checkstr(string,ag(0));
  return itofix(sys_call(stringcodes(string)));
#else
  pid=vfork();
  if(pid<0){
    perror("vfork failed");
    fprintf(stderr,"vfork failed\n");
    return itofix(-1);
  }
  if(pid==0){
    checkstr(string,ag(0));
    execl("/bin/csh","csh","-cef",stringcodes(string),0);
    exit(0);
  }
  wait(&rval);
  return itofix(rval);
#endif /* AP1000 */
}

/* tanaka 1991/2/26 */
WORD syscall_f(na,fp)
WORD *fp;
{
  WORD a;
  int codenum,t,i;
  int args[8];

  if(na<1 || na>9)parerr();
  codenum=fixtoi(slow_checkfix(a,ag(na-1)));
  for(i=0;i<na-1;i++){
    a=ag(na-2-i);
    if((t=tag(a))==FIXNUM)
      args[i]=fixtoi(a);
    else if(t==OTHERS && (t=otag(a))==STRING)
      args[i]=(int)stringcodes(a);
    else nofixerr(a);
  }
  for(;i<8;i++)args[i]=0;
  return (itofix(syscall(codenum,args[0],args[1],args[2],args[3],args[4],args[5],args[6],args[7])));
}

WORD errno_f(na,fp)
WORD *fp;
{
#ifdef AP1000
  if (na!=0)parerr();
  return (itofix(sys_errno()));
#else
#if 0
  extern int errno;
#endif

  if (na!=0)parerr();
  return (itofix(errno));
#endif
}

WORD cd_f(na,fp)
WORD *fp;
{
  WORD a;
  char *string;

  if(na==1)
    string=(char *)stringcodes(checkstr(a,pop()));
  else if(na==0)
    string="HOME";
  else parerr();
  return itofix(chdir(string));
}

WORD putenv_f(na,fp)
WORD *fp;
{
  char *varstr,*envstr,*newstr;
  int len,putenv();
  WORD a;

  if(na!=2)parerr();
  envstr=(char *)stringcodes(checkstr(a,ag(0)));
  varstr=(char *)stringcodes(checkstr(a,ag(1)));
  len=strlen(envstr)+strlen(varstr)+4;
  if((newstr=(char *)malloc(len))==NULL){
    fputs("Can't malloc any memory\n",stderr);
    exit(1);
  }
  sprintf(newstr,"%s=%s",varstr,envstr);
  if(putenv(newstr)){
    free(newstr);
    return nil;
  }
  return true;
}

WORD getenv_f(na,fp)
WORD *fp;
{
  char *envstr,*envptr,*getenv();
  WORD a;

  if(na!=1)parerr();
  envstr=(char *)stringcodes(checkstr(a,pop()));
  if((envptr=getenv(envstr))==NULL)
    return nil;
  return alstring(envptr,fp);
}

WORD getpid_f(na,fp)
WORD *fp;
{
  if(na!=0)parerr();
  return itofix(getpid());
}

WORD argv_f(na,fp)
WORD *fp;
{
  int i;

  if(na!=0)parerr();
  l(0)=nil;
  for(i=argc-1;i>=0;i--){
    l(0)=alcons(alstring(argv[i],fp),l(0),fp-1);
  }
  return l(0);
}

WORD localtime_f(na,fp)
WORD *fp;
{
  WORD vec;
  long timeval;
  struct tm *tmbuf;
  int i;

  if(na!=1)parerr();
  checkvec(vec,ag(0));
  if(fixtoi(osize(vec))<9)nofixerr(vec);
  time(&timeval);
  tmbuf=localtime(&timeval);
  for(i=0;i<9;i++)
    vecelm(vec,i)=itofix(*((int *)tmbuf+i));
  return vec;
}
