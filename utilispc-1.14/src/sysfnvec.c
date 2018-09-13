/* @(#)sysfnvec.c	1.6 90/07/30 */

#include "constdef.h"
#include "defvar.h"

WORD vref_f(int na, WORD *fp)
{
  WORD subscript,a;
  int index;

  if(na!=2)parerr();
  index=fixtoi(slow_checkfix(subscript,ag(0)));
  checkvec(a,ag(1));
  if(index<0 || index>=fixtoi(osize(a)))
    inderr(subscript,fp);
  return vecelm(a,index);
}

WORD vset_f(int na, WORD *fp)
{
  WORD subscript,vector,val;
  int index;

  if(na!=3)parerr();
  val=ag(0);
  index=fixtoi(slow_checkfix(subscript,ag(1)));
  checkvec(vector,ag(2));
  if(index<0 || index>=fixtoi(osize(vector)))
    inderr(subscript,fp);
  return vecelm(vector,index)=val;
}

WORD vector_f(int na, WORD *fp)
{
  WORD filler,size,vec;
  int i,isize,t,len;

  if(na==2){
    l(0)=ag(0);
    isize=fixtoi(slow_checkfix(size,ag(1)));
  }
  else if(na==1){
    l(0)=nil;
    isize=fixtoi(slow_checkfix(size,ag(0)));
  }
  else parerr();
  if(isize<0) nosymerr(size);
  vec=alvector(isize,fp-1);
  filler=l(0); /* restore filler */
  if((t=tag(filler))==CONS){
    for(i=0,len=fixtoi(osize(vec));i<len&&tag(filler)==CONS;filler=cdr(filler))
      vecelm(vec,i++)=car(filler);
    for(;i<isize;i++)
      vecelm(vec,i)=nil;
  }
  else if(t==OTHERS && otag(filler)==VECTOR){
    len=((fixtoi(osize(vec))>fixtoi(osize(filler))?fixtoi(osize(filler)):fixtoi(osize(vec))));
    for(i=0;i<len;i++)
      vecelm(vec,i)=vecelm(filler,i);
    for(;i<isize;i++)
      vecelm(vec,i)=nil;
  }
  else 
    for(i=0,len=fixtoi(osize(vec));i<len;i++)
      vecelm(vec,i)=filler;
  return vec;
}

WORD vector_length_f(int na, WORD *fp)
{
  WORD vec;

  if(na!=1)parerr();
  checkvec(vec,pop());
  return osize(vec);
}

WORD reference_f(int na, WORD *fp)
{
  WORD vec,index;
  int i;

  if(na!=2)parerr();
  i=fixtoi(slow_checkfix(index,pop()));
  checkvec(vec,pop());
  if(i<0||i>=fixtoi(osize(vec)))
    inderr(index,fp);
  return reference(vec,i);
}

WORD deref_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  checkref(a,ag(0));
  return deref(a);
}

WORD setref_f(int na, WORD *fp)
{
  WORD a,val;

  if(na!=2)parerr();
  val=ag(0);
  checkref(a,ag(1));
  deref(a)=val;
  return val;
}

WORD referred_vector_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  checkref(a,ag(0));
  for(;tag3(otag(a))!=OTAGS;a-=4);
  if(otag(a)!=VECTOR)nosymerr(ag(0));
  return a;
}

WORD referred_index_f(int na, WORD *fp)
{
  WORD a;
  int i;

  if(na!=1)parerr();
  checkref(a,ag(0));
  for(i=0;tag3(otag(a))!=OTAGS;i++,a-=4);
  if(otag(a)!=VECTOR)nosymerr(ag(0));
  return itofix(i-2);
}

WORD fill_vector_f(int na, WORD *fp)
{
  WORD vec,filler;
  int i,t,len;

  if(na!=2)parerr();
  filler=pop();
  checkvec(vec,pop());
  if((t=tag(filler))==CONS){
    for(i=0,len=fixtoi(osize(vec));i<len&&tag(filler)==CONS;filler=cdr(filler))
      vecelm(vec,i++)=car(filler);
  }
  else if(t==OTHERS && otag(filler)==VECTOR){
    len=((fixtoi(osize(vec))>fixtoi(osize(filler))?fixtoi(osize(filler)):fixtoi(osize(vec))));
    for(i=0;i<len;i++)
      vecelm(vec,i)=vecelm(filler,i);
  }
  else 
    for(i=0,len=fixtoi(osize(vec));i<len;i++)
      vecelm(vec,i)=filler;
  return vec;
}
