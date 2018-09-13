/* @(#)sysfnpred.c	1.6 90/07/30 */

#include "constdef.h"
#include "defvar.h"

WORD atom_f(int na, WORD *fp)
{
  if(na!=1)parerr();
  if(tag(ag(0))==CONS)return nil;
  return true;
}

WORD numberp_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  if(!((int)(a=ag(0))&3))return true;
  else if(tag(a)!=OTHERS)return nil;
#ifndef NO_BIGNUM
  else if(otag(a)==BIGNUM)return true;
#endif
#ifndef NO_FLONUM
  else if(otag(a)==FLONUM)return true;
#endif
  return nil;
}

WORD stringp_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  if(tag(a=ag(0))!=OTHERS || otag(a)!=STRING)return nil;
  return true;
}

WORD vectorp_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  if(tag(a=ag(0))!=OTHERS || otag(a)!=VECTOR)return nil;
  return true;
}

WORD integerp_f(int na, WORD *fp)
{
  WORD a;
  int t;

  if(na!=1)parerr();
  if((t=tag(a=ag(0)))==FIXNUM)return true;
  else if(t!=OTHERS) return nil;
#ifndef NO_BIGNUM
  else if(otag(a)!=BIGNUM)return nil;
#endif
  return true;
}
#ifndef NO_FLONUM
WORD floatp_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  if(tag(a=ag(0))!=OTHERS || otag(a)!=FLONUM)return nil;
  return true;
}
#endif

#ifndef NO_BIGNUM
WORD bigp_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  if(tag(a=ag(0))!=OTHERS || otag(a)!=BIGNUM)return nil;
  return true;
}
#endif
WORD streamp_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  if(tag(a=ag(0))!=OTHERS || otag(a)!=STREAM)return nil;
  return true;
}

WORD string_streamp_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  if(tag(a=ag(0))!=OTHERS || otag(a)!=STREAM|| !(mode(a)&STRINGSTREAM))return nil;
  return true;
}

WORD referencep_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  if(tag(a=ag(0))!=OTHERS || tag3(otag(a))==OTAGS)return nil;
  return true;
}
WORD codep_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  if(tag(a=ag(0))!=OTHERS || otag(a)!=CODE)return nil;
  return true;
}

WORD fixp_f(int na, WORD *fp)
{
  if(na!=1)parerr();
  if(tag(ag(0))!=FIXNUM)return nil;
  return true;
}

WORD symbolp_f(int na, WORD *fp)
{
  if(na!=1)parerr();
  if(tag(ag(0))!=SYMBOL)return nil;
  return true;
}

WORD consp_f(int na, WORD *fp)
{
  if(na!=1)parerr();
  if(tag(ag(0))!=CONS)return nil;
  return true;
}

WORD predef_f(int na, WORD *fp) /* この関数はマニュアルには載っていないので無視する */
         
{
  return udferr(nil,fp);
}

WORD eq_f(int na, WORD *fp)
{
  if(na!=2)parerr();
  if(ag(0)!=ag(1))return nil;
  return true;
}

WORD neq_f(int na, WORD *fp)
{
  if(na!=2)parerr();
  if(ag(0)==ag(1))return nil;
  return true;
}

WORD equal_f(int na, WORD *fp)
{
  if(na!=2)parerr();
  if(equal(ag(0),ag(1)))return true;
  return nil;
}

WORD null_f(int na, WORD *fp)
{
  if(na!=1)parerr();
  if(ag(0)!=nil)return nil;
  return true;
}

int equal(WORD a, WORD b)
{
  int i,len;
  if(a==b) return 1;
  switch(tag(a)){
  case FIXNUM:
  case SYMBOL:
    return 0;
  case OTHERS:
    if(tag(b)!=OTHERS)return 0;
    switch(otag(a)){
#ifndef NO_BIGNUM
    case BIGNUM:
      if(otag(b)!=BIGNUM)return 0;
      if(osize(a)!=osize(b))return 0;
      for(i=0,len=fixtoi(osize(a));i<len;i++)
	if(bigcodes(a,i)!=bigcodes(b,i))return 0;
      return 1;
#endif
#ifndef NO_FLONUM
    case FLONUM:
      if(otag(b)!=FLONUM)return 0;
      if(floval(a)==floval(b))return 1;
      return 0;
#endif
    case VECTOR:
      if(otag(b)!=VECTOR)return 0;
      if(osize(a)!=osize(b))return 0;
      for(i=0,len=fixtoi(osize(a));i<len;i++)
	if(vecelm(a,i)!=vecelm(b,i))return 0;
      return 1;
    case STRING:
      if(otag(b)!=STRING)return 0;
      if(osize(a)!=osize(b))return 0;
      for(i=0,len=fixtoi(osize(a));i<len;i++)
	if(*(stringcodes(a)+i)!= *(stringcodes(b)+i))return 0;
      return 1;
    default:
      return 0;
    }
  case CONS:
    if((tag(b)==CONS)&&equal(car(a),car(b))&&equal(cdr(a),cdr(b)))
      return 1;
    return 0;
  default: /* コンパイラをごまかすためのダミー */
    return 0;
  }
}
    
WORD predefinedp_f(int na, WORD *fp)
{
  WORD sym,def;

  if(na!=1)parerr();
  checksym(sym,ag(0));
  if((def=definition(sym))==(WORD)uderr)return nil;
#ifndef OBJECTSPECIAL
  if(tag(def)==OTHERS){
    if((int *)def<etext)return true;
    else if(otag(def)==CODE && (int *)codetop(def)<etext)return true;
  }
#else
  if(tag(def)==OTHERS)return true;
#endif
  return nil;
}
