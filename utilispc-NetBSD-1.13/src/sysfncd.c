/* @(#)sysfncd.c	1.6 90/07/30 */

#include "constdef.h"
#include "defvar.h"

WORD getd_f(int na, WORD *fp)
{
  WORD sym,def;

  if(na!=1)parerr();
  def=definition(checksym(sym,ag(0)));
  if(spec(def))return udferr1(sym,fp);
  return def;
}

WORD putd_f(int na, WORD *fp)
{
  WORD sym;

  if(na!=2)parerr();
  definition(checksym(sym,ag(1)))=ag(0);
  return sym;
}

WORD make_undefined_f(int na, WORD *fp)
{
  WORD sym;

  if(na!=1)parerr();
  definition(checksym(sym,ag(0)))=(WORD)uderr;
  return sym;
}

WORD definedp_f(int na, WORD *fp)
{
  WORD sym,def;

  if(na!=1)parerr();
  def=definition(checksym(sym,ag(0)));
  if(spec(def))return nil;
  return true;
}

WORD specialp_f(int na, WORD *fp)
{
  WORD sym,def;

  if(na!=1)parerr();
  def=definition(checksym(sym,ag(0)));
  if(spec(def) && def!=uderr)return true;
  return nil;
}

WORD funcname_f(int na, WORD *fp)
{
  WORD a,sym;

  if(na==2){
    checksym(sym,ag(0));
    return funcname(checkcode(a,ag(1)))=sym;
  }
  else if(na==1) 
    return funcname(checkcode(a,ag(0)));
  parerr();
}

WORD codesize_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return osize(checkcode(a,ag(0)));
}

WORD maxarg_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return maxarg(checkcode(a,ag(0)));
}

WORD minarg_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return minarg(checkcode(a,ag(0)));
}

#ifndef NO_DYNAMIC_LOAD

WORD system_code_load_f(int na, WORD *fp)
{
  WORD a,codeaddr;
  int i;
  char *files[100];

  if(na==0)parerr();
  for(i=0;i<na;i++)
    files[i]=(char *)stringcodes(checkstr(a,ag(na-1-i)));
  if((int)(codeaddr=(WORD)proglink(na,files,fp))<0)return nil;
  return codeaddr;
}

WORD program_link_f(int na, WORD *fp)
{
  WORD a,tmpcode,retlist;
  FP *codeaddr;
  int i;
  char *files[100];

  if(na==0)parerr();
  for(i=0;i<na;i++)
    files[i]=(char *)stringcodes(checkstr(a,ag(na-1-i)));
  if((int)(codeaddr=(FP*)proglink(na,files,fp))<0)return nil;
  for(retlist=nil;*codeaddr!=(FP)0;codeaddr++){
    tmpcode=alcode(0,fp);
    codetop(tmpcode)= *codeaddr;
    funcname(tmpcode)= nil;
    minarg(tmpcode)=maxarg(tmpcode)=0;
    retlist=alcons(tmpcode,retlist,fp);
  }
  return retlist;
}
#endif /* NO_DYNAMIC_LOAD */
