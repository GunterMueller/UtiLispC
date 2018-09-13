/* @(#)sysfnfix.c	1.6 90/07/30 */

#include "constdef.h"
#include "defvar.h"

WORD fixzerop_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1) parerr();
#if !defined(SPARC) || !defined(USEBUSERR)
  if((int)(a=ag(0))==0) return true;
  else{
    if((int)a&3) nofixerr(a);
    return nil;
  }
#else
  if((int)tsub(checkfix(a,ag(0)),0)==0) return true;
  else return nil;
#endif
}

WORD fixplusp_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1) parerr();
  if((int)slow_checkfix(a,ag(0))>0) return true;
  else return nil;
}

WORD fixminp_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1) parerr();
  if((int)slow_checkfix(a,ag(0))<0) return true;
  else return nil;
}

WORD fixeq_f(int na, WORD *fp)
{
  WORD a0,a1;

  if(na!=2) parerr();
#if !defined(SPARC) || !defined(USEBUSERR)
  a0=ag(0);
  a1=ag(1);
  if(((int)a0|(int)a1)&3){
    if((int)a0&3) nofixerr(a0);
    else nofixerr(a1);
  }
  if((int)a0==(int)a1) return true;
  else return nil;
#else
  if((int)tsub(checkfix(a0,ag(0)),checkfix(a1,ag(1)))==0) return true;
  else return nil;
#endif
}

WORD fixneq_f(int na, WORD *fp)
{
  WORD a0,a1;

  if(na!=2) parerr();
#if !defined(SPARC) || !defined(USEBUSERR)
  a0=ag(0);
  a1=ag(1);
  if(((int)a0|(int)a1)&3){
    if((int)a0&3) nofixerr(a0);
    else nofixerr(a1);
  }
  if((int)a0!=(int)a1) return true;
#else
  if((int)tsub(checkfix(a0,ag(0)),checkfix(a1,ag(1)))!=0) return true;
#endif
  else return nil;
}

#ifdef __GNUC__
#define INLINE inline
#else
#define INLINE
#endif
static INLINE void errfix(int na, WORD *fp)
{
  for(;na>0;na--) 
    if((int)(*(fp-na)) & 3)
      nofixerr(*(fp-na));
}

WORD fixgt_f(int na, WORD *fp)
{
  WORD a1,a2;
#if !defined(SPARC) || !defined(USEBUSERR)
  int tags,na1;
#endif

#if !defined(SPARC) || !defined(USEBUSERR)
  if((na1=na)==0) return true;
  tags=(int)(a2=pop());
  while(--na1>0){
    tags|=(int)(a1=pop());
    if((int)tsub(a1,a2)>0)
      a2=a1;
    else{
      if(tags&3)
	errfix(na-na1+1,fp);
      return nil;
    }
  }
  if(tags&3)
    errfix(na,fp);
  return true;
#else
  if(na==0) return true;
  slow_checkfix(a2,pop());
  while(--na>0){
    if((int)tsub(checkfix(a1,pop()),a2)>0) 
      a2=a1;
    else 
      return nil;
  }
  return true;
#endif
}

WORD fixge_f(int na, WORD *fp)
{
  WORD a1,a2;
#if !defined(SPARC) || !defined(USEBUSERR)
  int tags,na1;
#endif

  if(na==0)return true;
#if !defined(SPARC) || !defined(USEBUSERR)
  na1=na;
  tags=(int)(a2=pop());
  while(--na>0){
    tags|=(int)(a1=pop());
    if((int)tsub(a1,a2)>=0)
      a2=a1;
    else{
      if(tags&3)
	errfix(na1-na+1,fp);
      return nil;
    }
  }
  if(tags&3)
    errfix(na1,fp);
  return true;
#else
  slow_checkfix(a2,pop());
  while(--na>0){
    if((int)tsub(checkfix(a1,pop()),a2)>=0)a2=a1;
    else return nil;
  }
  return true;
#endif
}

WORD fixlt_f(int na, WORD *fp)
{
  WORD a1,a2;
#if !defined(SPARC) || !defined(USEBUSERR)
  int tags,na1;
#endif

  if(na==0)return true;
#if !defined(SPARC) || !defined(USEBUSERR)
  na1=na;
  tags=(int)(a2=pop());
  while(--na>0){
    tags|=(int)(a1=pop());
    if((int)tsub(a1,a2)<0)
      a2=a1;
    else{
      if(tags&3)
	errfix(na1-na+1,fp);
      return nil;
    }
  }
  if(tags&3)
    errfix(na1,fp);
  return true;
#else
  slow_checkfix(a2,pop());
  while(--na>0){
    if((int)tsub(checkfix(a1,pop()),a2)<0)a2=a1;
    else return nil;
  }
  return true;
#endif
}

WORD fixle_f(int na, WORD *fp)
{
  WORD a1,a2;
#if !defined(SPARC) || !defined(USEBUSERR)
  int tags,na1;
#endif

  if(na==0)return true;
#if !defined(SPARC) || !defined(USEBUSERR)
  na1=na;
  tags=(int)(a2=pop());
  while(--na>0){
    tags|=(int)(a1=pop());
    if((int)tsub(a1,a2)<=0)
      a2=a1;
    else{
      if(tags&3)
	errfix(na1-na+1,fp);
      return nil;
    }
  }
  if(tags&3)
    errfix(na1,fp);
  return true;
#else
  slow_checkfix(a2,pop());
  while(--na>0){
    if((int)tsub(checkfix(a1,pop()),a2)<=0)a2=a1;
    else return nil;
  }
  return true;
#endif
}

WORD fixdifference_f(int na, WORD *fp)
{
  WORD a1,rval;
#if !defined(SPARC) || !defined(USEBUSERR)
  int tags,na1;
#endif

  if(na==0)parerr();
#if !defined(SPARC) || !defined(USEBUSERR)
  if(na==1)
    return (WORD)(-(int)checkfix(rval,pop()));
  na1=na;
  tags=(int)(rval=pop());
  while(--na>1){
    tags|=(int)(a1=pop());
    rval=tadd(a1,rval);
  }
  tags|=(int)(a1=pop());
  rval=tsub(a1,rval);
  if(tags&3)
    errfix(na1,fp);
  return rval;
#else
  checkfix(rval,pop());
  if(na==1)
    return tsub((WORD)0,rval);
  while(--na>1)
    rval=tadd(checkfix(a1,pop()),rval);
  return tsub(checkfix(a1,pop()),rval);
#endif
}

WORD fixplus_f(int na, WORD *fp)
{
  WORD a1,rval;
#if !defined(SPARC) || !defined(USEBUSERR)
  int tags,na1;
#endif

#if !defined(SPARC) || !defined(USEBUSERR)
  if(na==0) return 0;
  na1=na;
  tags=(int)(rval=pop());
  while(--na1>0){
    tags|=(int)(a1=pop());
    rval=tadd(a1,rval);
  }
  if(tags&3)
    errfix(na,fp);
  return rval;
#else
  rval=(WORD)0;
  while(na-->0)
    rval=tadd(rval,checkfix(a1,pop()));
  return rval;
#endif
}

WORD fixtimes_f(int na, WORD *fp)
{
  int rval;
  WORD a;
#if !defined(SPARC) || !defined(USEBUSERR)
  int tags,na1;
#endif

  if(na==0) return itofix(1);
#if !defined(SPARC) || !defined(USEBUSERR)
  na1=na;
  tags=(int)pop();
  rval=fixtoi(tags);
  while(--na>0){
    tags|=(int)(a=pop());
    rval*=fixtoi(a);
  }
  if(tags&3)
    errfix(na1,fp);
  return itofix(rval);
#else
  rval=fixtoi(slow_checkfix(a,pop()));
  while(--na>0)
    rval*=fixtoi(slow_checkfix(a,pop()));
  return itofix(rval);
#endif
}

WORD fixquotient_f(int na, WORD *fp) /* bugged version of div */
         
{
  int rval;
  WORD a;
#if !defined(SPARC) || !defined(USEBUSERR)
  int tags,na1;
#endif

  if(na==0) parerr();
#if !defined(SPARC) || !defined(USEBUSERR)
  na1=na;
  rval=fixtoi(tags=(int)ag(na-1));
  while(--na>0){
    if((a=pop())==itofix(0))zerodiverr(fp);
    tags|=(int)a;
    rval/=fixtoi(a);
  }
  if(tags&3)
    errfix(na1,fp+1);
  return itofix(rval);
#else
  rval=fixtoi(slow_checkfix(a,ag(na-1)));
  while(--na>0){
    if(slow_checkfix(a,pop())==itofix(0))zerodiverr(fp);
    rval/=fixtoi(a);
  }
  return itofix(rval);
#endif
}

WORD fixremainder_f(int na, WORD *fp) /* bugged version */
         
{
  WORD a,b;

  if(na!=2) parerr();
#if !defined(SPARC) || !defined(USEBUSERR)
  b=ag(0);
  if((int)b==0) zerodiverr(fp);
  if(((int)b|(int)(a=ag(1)))&3){
    if((int)b&3)nofixerr(b);
    else nofixerr(a);
  }
  return itofix(fixtoi(a)%fixtoi(b));
#else
  slow_checkfix(b,ag(0));
  if(b==itofix(0)) zerodiverr(fp);
  return itofix(fixtoi(slow_checkfix(a,ag(1)))%fixtoi(b));
#endif
}

WORD fixadd1_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return tadd(checkfix(a,ag(0)),(WORD)FIX1);
}

WORD fixsub1_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return tsub(checkfix(a,ag(0)),(WORD)FIX1);
}

WORD logor_f(int na, WORD *fp)
{
  WORD rval;
  int na1;

  if(na==0)return (WORD)0;
  na1=na;
  rval=pop();
  while(--na>0)
    rval=(WORD)((int)rval|(int)pop());
  if((int)rval&3)
    errfix(na1,fp);
  return rval;
}

WORD logand_f(int na, WORD *fp)
{
  WORD a,rval;

  rval= (WORD)-FIX1;
  while(na-->0)
    rval=(WORD)((int)rval&(int)slow_checkfix(a,pop()));
  return rval;
}

WORD logxor_f(int na, WORD *fp)
{
  WORD a,rval;

  rval=(WORD)0;
  while(na-->0)
    rval=(WORD)((int)rval^(int)slow_checkfix(a,pop()));
  return rval;
}


WORD logshift_f(int na, WORD *fp)
{
  WORD a,b;
  int shift;

  if(na!=2)parerr();
  shift=fixtoi(slow_checkfix(b,ag(0)));
  slow_checkfix(a,ag(1));
#if 0
  printf("a=0x%x,-shift=%d,a>>(-shift)=0x%x\n",a,-shift,(int)a>>(-shift));
#endif
  if(shift>0)
    return (WORD)((int)a<<shift);
#if 0
  printf("a=0x%x,-shift=%d,a>>(-shift)=0x%x\n",a,-shift,(int)a>>(-shift));
#endif
  return (WORD)(((int)a>>(-shift))&0xfffffff0);
}

WORD fixexpt_f(int na, WORD *fp)
{
  WORD a,b;
  int x,exp,rval=1;

  if(na!=2)parerr();
  exp=fixtoi(slow_checkfix(b,ag(0)));
  x=fixtoi(slow_checkfix(a,ag(1)));
  while(--exp>=0)rval*=x;
  return itofix(rval);
}

