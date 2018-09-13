/* @(#)sysfnflo.c	1.6 90/07/30 */

#include <math.h>
#include "constdef.h"
#include "defvar.h"

#define bin32 (256.0*256.0*256.0*256.0)
WORD flozerop_f(int na, WORD *fp)
{
  WORD a;
  double fval;
  
  if(na!=1)parerr();
  fval=floval(checkflo(a,*fp));
  if(fval==0.0)return true;
  return nil;
}

WORD floplusp_f(int na, WORD *fp)
{
  WORD a;
  double fval;
  
  if(na!=1)parerr();
  fval=floval(checkflo(a, *fp));
  if(fval>0.0)return true;
  return nil;
}
WORD flominsp_f(int na, WORD *fp)
{
  WORD a;
  double fval;
  
  if(na!=1)parerr();
  fval=floval(checkflo(a, *fp));
  if(fval<0.0)return true;
  return nil;
}

WORD floeq_f(int na, WORD *fp)
{
  WORD a;
  double fval0,fval1;

  if(na!=2)parerr();
  fval0=floval(checkflo(a,*fp));
  fval1=floval(checkflo(a,*(fp+1)));
  if(fval0==fval1)return true;
  return nil;
}

WORD floneq_f(int na, WORD *fp)
{
  WORD a;
  double fval0,fval1;

  if(na!=2)parerr();
  fval0=floval(checkflo(a,*fp));
  fval1=floval(checkflo(a,*(fp+1)));
  if(fval0!=fval1)return true;
  return nil;
}

WORD flodifference_f(int na, WORD *fp)
{
  WORD *tmpstack,a;
  int i;
  double fval;

  if(na<1)parerr();
  if(na==1)return alflonum(-floval(checkflo(a,pop())),fp);
  fval=floval(checkflo(a,*(tmpstack=fp+na-1)));
  for(i=0;i<na-1;i++)
    fval-=floval(checkflo(a,*--tmpstack));
  return alflonum(fval,fp);
}

WORD floplus_f(int na, WORD *fp)
{
  WORD *tmpstack,a;
  double fval=(double)0;

  for(tmpstack=fp+na;na-->0;)
    fval+=floval(checkflo(a,*--tmpstack));
  return alflonum(fval,fp);
}

WORD flotimes_f(int na, WORD *fp)
{
  WORD *tmpstack,a;
  double fval;

  fval=1.0;
  for(tmpstack=fp+na;na-->0;)
    fval*=floval(checkflo(a,*--tmpstack));
  return alflonum(fval,fp);
}

WORD floquotient_f(int na, WORD *fp)
{
  WORD *tmpstack,a;
  int i;
  double fval,fval1;

  if(na<1)parerr();
  fval=floval(checkflo(a,*(tmpstack=fp+na-1)));
  for(i=0;i<na-1;i++){
    fval1=floval(checkflo(a,*--tmpstack));
    if(fval1==0.0) zerodiverr(fp);
    fval/=fval1;
  }
  return alflonum(fval,fp);
}

WORD floremainder_f(int na, WORD *fp)
{
  WORD a;
  double fval0,fval1,fval;

  if(na!=2)parerr();
  fval1=floval(checkflo(a,*fp));
  if(fval1==0.0) zerodiverr(fp);
  fval0=floval(checkflo(a,*(fp+1)));
  fval=fval0-(int)(fval0/fval1)*fval1;
  return alflonum(fval,fp);
}

WORD floadd1_f(int na, WORD *fp)
{
  WORD a;
  double fval;

  if(na!=1)parerr();
  fval=floval(checkflo(a,*fp))+1.0;
  return alflonum(fval,fp);
}

WORD flosub1_f(int na, WORD *fp)
{
  WORD a;
  double fval;

  if(na!=1)parerr();
  fval=floval(checkflo(a,*fp))-1.0;
  return alflonum(fval,fp);
}

WORD fix_f(int na, WORD *fp)
{
  WORD a;
  double fval;
  int t;

  if(na!=1)parerr();
  if((t=tag(a=ag(0)))==FIXNUM)return a;
  checkothers(a,a);
#ifndef NO_BIGNUM
  if((t=otag(a))==BIGNUM)return a;
#else
  t=otag(a);
#endif
  if(t!=FLONUM)nofixerr(a);
  fval=floval(checkflo(a,*fp));
#ifndef NO_BIGNUM
  if(fval>(double)MAXFIX || fval<(double)MINFIX)
    return flotobig(fval,fp);
#endif
  return itofix((int)fval);
}
#ifndef NO_BIGNUM

WORD flotobig(double fval, WORD *fp)
{
  int i,sign=0,size;
  unsigned int val;
  WORD new;

  if(fval<0){fval= -fval;sign=1;}
  for(i=1;fval>bin32/2.0;i++)fval/=bin32;
  new=albignum(size=i,fp);
  if(!sign){
    for(;i>0;--i){
      val=(unsigned int)fval;
      bigcodes(new,i-1)=val;
      fval=(fval-(double)val)*bin32;
    }
  }
  else{
    for(;i>0;--i){
      val=(unsigned int)fval;
      bigcodes(new,i-1)= ~val;
      fval=(fval-(double)val)*bin32;
    }
    uadd(&bigcodes(new,0),1,size*4);
  }
  return normbig(new);
}
#endif

WORD float_f(int na, WORD *fp)
{
  WORD a;
  int t;

  if(na!=1)parerr();
  if((t=tag(a= *fp))==FIXNUM)return alflonum((double)fixtoi(a),fp);
  if(t!=OTHERS)nofixerr(a);
  t=otag(a);
#ifndef NO_BIGNUM
  if(t==BIGNUM){
    return alflonum(bigtoflo(a),fp);
  }
#endif
  if (t==FLONUM)
    return a;
  else return (nofixerr(a),a);
}

#ifndef NO_BIGNUM

double bigtoflo(WORD a)
{
  int len;
  double fval=0.0,fval1;

  len=fixtoi(osize(a));
  if((int)bigcodes(a,len-1)<0){
    for(fval1=1.0;len-->0;fval1*=bin32)
      fval=fval*bin32+(double)(bigcodes(a,len));
    return fval-fval1;
  }
  else{
    while(len-->0)fval=fval*bin32+(double)(bigcodes(a,len));
    return fval;
  }
}
#endif

WORD flogt_f(int na, WORD *fp)
{
  WORD a;
  double fval0,fval1;

  if(na<1)parerr();
  fval0=floval(checkflo(a,pop()));
  while(--na>0){
    fval1=floval(checkflo(a,pop()));
    if(fval1<=fval0){
      return nil;
    }
    fval0=fval1;
  }
  return true;
}

WORD floge_f(int na, WORD *fp)
{
  WORD a;
  double fval0,fval1;

  if(na<1)parerr();
  fval0=floval(checkflo(a,pop()));
  while(--na>0){
    fval1=floval(checkflo(a,pop()));
    if(fval1<fval0){
      return nil;
    }
    fval0=fval1;
  }
  return true;
}

WORD flolt_f(int na, WORD *fp)
{
  WORD a;
  double fval0,fval1;

  if(na<1)parerr();
  fval0=floval(checkflo(a,pop()));
  while(--na>0){
    fval1=floval(checkflo(a,pop()));
    if(fval1>=fval0){
      return nil;
    }
    fval0=fval1;
  }
  return true;
}

WORD flole_f(int na, WORD *fp)
{
  WORD a;
  double fval0,fval1;

  if(na<1)parerr();
  fval0=floval(checkflo(a,pop()));
  while(--na>0){
    fval1=floval(checkflo(a,pop()));
    if(fval1>fval0){
      return nil;
    }
    fval0=fval1;
  }
  return true;
}
