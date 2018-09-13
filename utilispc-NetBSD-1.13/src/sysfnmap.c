/* @(#)sysfnmap.c	1.7 90/11/28 */

#include "constdef.h"
#include "defvar.h"

WORD map_f(int na, WORD *fp)
{
  WORD list;

  if(na!=2)parerr();
  for(l(0)=list=ag(1);tag(list)==CONS;list=ag(1)=cdr(ag(1))){
    l(1)=list;
    funcall(ag(0),1,fp-2);
  }
  return l(0);
}

WORD mapc_f(int na, WORD *fp)
{
  WORD list;


  if(na!=2)parerr();
  for(l(0)=list=ag(1);tag(list)==CONS;list=ag(1)=cdr(ag(1))){
    l(1)=car(list);
    funcall(ag(0),1,fp-2);
  }
  return l(0);
}

WORD funcall1(WORD func, WORD arg, WORD *fp)
{
  l(0)=arg;
  return funcall(func,1,fp-1);
}

WORD maplist_f(int na, WORD *fp)
{
  WORD list;

  if(na!=2)parerr();
  if(tag(list=ag(1))!=CONS)return nil;
  l(0)=l(1)=alcons(funcall1(ag(0),list,fp),nil,fp);
  for(ag(1)=list=cdr(ag(1));tag(list)==CONS;list=ag(1)=cdr(ag(1))){
    l(0)= cdr(l(0))=alcons(funcall1(ag(0),list,fp-2),nil,fp-2);
  }
  return l(1);
}

WORD mapcar_f(int na, WORD *fp)
{
  WORD list;
  WORD *sp;

  if(na!=2)parerr();
  if(tag(list=ag(1))!=CONS)return nil;
  for(sp=fp,na=0;tag(list)==CONS;list=ag(1)=cdr(ag(1)),na++){
    *--sp=funcall1(ag(0),car(list),sp);
  }
  return allist(na,sp,nil);
}

WORD mapcan_f(int na, WORD *fp)
{
  WORD list;
  int i;

  if(na!=2)parerr();
  for(list=ag(1),i=0;!atom(list);list=ag(1)=cdr(ag(1)),i++)
    l(i)=funcall1(ag(0),car(list),fp-i);
  return nconc_f(i,fp-i);
}

WORD mapcon_f(int na, WORD *fp)
{
  WORD list;
  int i;

  if(na!=2)parerr();
  for(list=ag(1),i=0;!atom(list);list=ag(1)=cdr(ag(1)),i++)
    l(i)=funcall1(ag(0),list,fp-i);
  return nconc_f(i,fp-i);
}

WORD mapv_f(int na, WORD *fp)
{
  WORD vec;
  int i,len;

  if(na!=2)parerr();
  checkvec(vec,ag(1));
  l(0)=vec;
  for(i=0,len=fixtoi(osize(vec));i<len;i++){
    funcall1(ag(0),reference(l(0),i),fp-1);
  }
  return l(0);
}

WORD mapvector_f(int na, WORD *fp)
{
  WORD vec;
  int i,len;

  if(na!=2)parerr();
  checkvec(vec,ag(1));
  l(0)=alvector(len=fixtoi(osize(vec)),fp);
  for(i=0;i<len;i++)
    vecelm(l(0),i)=funcall1(ag(0),vecelm(ag(1),i),fp-1);
  return l(0);
}

