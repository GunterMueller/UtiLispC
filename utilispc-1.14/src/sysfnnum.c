/* @(#)sysfnnum.c	1.6 90/07/30 */

#include <stdio.h>
#include <math.h>
#include "constdef.h"
#include "defvar.h"
#define max(x,y) (x>y?x:y)
void printp(void) {}
/* ヒープ上でもっとも新しく確保されたbignumを正規化する。 */
#ifndef NO_BIGNUM
WORD normbig(WORD a)
{
  int i,val;

  i=fixtoi(osize(a));
  if((val=bigcodes(a,i-1))==0){
    for(;i>0;i--)
      if((val=bigcodes(a,i-1))!=0)break;
    if((int)val<0)i++;
  }
  else if(val== -1){
    for(;i>0;i--)
      if((int)(val=bigcodes(a,i-1))!= -1)break;
    if((int)val>=0)i++;
  }
  if(i>1){
    heap=(WORD *)(a-OTHERS)+i+BIGSIZE;
    osize(a)=itofix(i);
    return a;
  }
  if(val>MAXFIX||val<MINFIX){
    heap=(WORD *)(a-OTHERS)+4;
    osize(a)=itofix(i);
    return a;
  }
  heap=(WORD *)(a-OTHERS);
  return itofix(val);
}

WORD copybig(WORD a, WORD *fp)
{
  int len;
  WORD b;

  l(0)=a;
  b=albignum(len=fixtoi(osize(a)),fp-1);
  a=l(0);
  while(len-->0)
    bigcodes(b,len)=bigcodes(a,len);
  return b;
}
#endif

WORD zerop_f(int na, WORD *fp)
{
  int t;
  WORD a;

  if(na!=1)parerr();
  else if((t=tag(a=ag(0)))==FIXNUM){
    if(fixtoi(a)!=0)return nil;
    return true;
  }
  else if(t!=OTHERS)nofixerr(a);
  else {
    t=otag(a);
#ifndef NO_FLONUM
    if(t==FLONUM){
      if(floval(a)!=0.0)return nil;
      return true;
    }
#endif
#ifndef NO_BIGNUM
    if(t==BIGNUM)
      return nil;
#endif
    return (nofixerr(a),a);
  }
}

WORD plusp_f(int na, WORD *fp)
{
  int t;
  WORD a;

  if(na!=1)parerr();
  else if((t=tag(a=ag(0)))==FIXNUM){
    if(fixtoi(a)>0)return true;
    return nil;
  }
  else if(t!=OTHERS)nofixerr(a);
  else{
    t=otag(a);
#ifndef NO_FLONUM
    if(t==FLONUM){
      if(floval(a)>0.0)return true;
      return nil;
    }
#endif
#ifndef NO_BIGNUM
    if(t==BIGNUM){
      if(bigcodes(a,fixtoi(osize(a))-1)&SIGNMASK)return nil;
      return true;
    }
#endif
    return (nofixerr(a),a);
  }
}

WORD minusp_f(int na, WORD *fp)
{
  int t;
  WORD a;

  if(na!=1)parerr();
  else if((t=tag(a=ag(0)))==FIXNUM){
    if(fixtoi(a)<0)return true;
    return nil;
  }
  else if(t!=OTHERS)nofixerr(a);
  else{
    t=otag(a);
#ifndef NO_FLONUM
    if(t==FLONUM){
      if(floval(a)<0.0)return true;
      return nil;
    }
#endif
#ifndef NO_BIGNUM
    if(t==BIGNUM){
      if(bigcodes(a,fixtoi(osize(a))-1)&SIGNMASK)return true;
      return nil;
    }
#endif
    return (nofixerr(a),a);
  }
}

WORD oddp_f(int na, WORD *fp)
{
  WORD a;
  int t;

  if(na!=1)parerr();
  if((t=tag(a=ag(0)))==FIXNUM){
    if((int)a&16)return true;
    else return nil;
  }
#ifndef NO_BIGNUM
  else if(t!=OTHERS)nofixerr(a);
  else if(otag(a)!=BIGNUM)nofixerr(a);
  if(bigcodes(a,0)&1)return true;
#else
  else nofixerr(a);
#endif
  return nil;
}

#ifndef NO_FLONUM
WORD plusflo(int na, double fval, WORD *fp)
{
  WORD a;

  while(na-->0)
    fval+=coflo(a,pop(),fp);
  return alflonum(fval,fp);
}
#endif /* NO_FLONUM */

#ifndef NO_BIGNUM
WORD extbig(WORD big, int from, int to, WORD *fp)
{
  int i;

  heap=(WORD *)(big-OTHERS)+to+BIGSIZE;
  if(heap>heapbottom){
    osize(big)=itofix(from);
    l(0)=big;
    gc(to+BIGSIZE,fp-1);
    big=l(0);
    if((WORD *)(big-OTHERS)+from+BIGSIZE!=(WORD *)heap){
      big=copybig(big,fp);
    }
    heap=(WORD *)(big-OTHERS)+to+BIGSIZE;
  }
  if(bigcodes(big,from-1)&SIGNMASK)
    for(i=from;i<to;i++)bigcodes(big,i)=0xffffffff;
  else
    for(i=from;i<to;i++)bigcodes(big,i)=0;
  osize(big)=itofix(to);
  return big;
}
WORD extzbig(WORD big, int from, int to, WORD *fp)
{
  int i;

  heap=(WORD *)(big-OTHERS)+to+BIGSIZE;
  if(heap>heapbottom){
    l(0)=big;
    gc(to+BIGSIZE,fp-1);
    big=l(0);
    if((WORD *)(big-OTHERS)+from+BIGSIZE!=(WORD *)heap){
      big=copybig(l(0),fp);
    }
    heap=(WORD *)(big-OTHERS)+to+BIGSIZE;
  }
  for(i=from;i<to;i++)bigcodes(big,i)=0;
  osize(big)=itofix(to);
  return big;
}

WORD itobig(int val, WORD *fp)
{
  WORD big;

  big=albignum(1,fp);
  bigcodes(big,0)=(unsigned int)val;
  return big;
}

WORD plusbig(int na, WORD bigval, WORD *fp)
{
  WORD a,b,*sp;
  int t,answersize,size;

  answersize=fixtoi(osize(bigval))*4;
  l(0)=bigval;
  for(sp=fp;na-->0;sp++){
    if((t=tag(a= *sp))==FIXNUM){
      answersize=uadd(&bigcodes(l(0),0),fixtoi(a),answersize);
    }
    else if(t!=OTHERS)nooterr(a);
    else{
      t=otag(a);
#ifndef NO_FLONUM
      if(t==FLONUM){
	osize(l(0))=itofix(answersize/4);
	return plusflo(na+1,bigtoflo(l(0)),sp);
      }
#endif
      if(t!=BIGNUM)nofixerr(a);
      else {
	if((size=fixtoi(osize(a))*4)>answersize){
	  l(0)=extbig(l(0),answersize/4,size/4,fp-1);
	  b= *sp;answersize=size;
	}
	else if(size<answersize){
	  l(1)=extbig(copybig(a,fp-1),size/4,answersize/4,fp-1);
	  l(0)=copybig(l(0),fp-2);
	  b=l(1);
	}
	else b=a;
	answersize=madd(&bigcodes(l(0),0),&bigcodes(b,0),answersize);
	heap=(WORD *)(l(0)+BIGCODES+answersize);
      }
    }
  }
  osize(l(0))=itofix(answersize/4);
  return normbig(l(0));
}
#endif

WORD plus_f(int na, WORD *fp)
{
  int val=0,t;
  WORD a;

  while(na-->0){
    if((t=tag(a=see()))==OTHERS){
      t=otag(a);
#ifndef NO_FLONUM
      if(t==FLONUM){
	if(val!=0)
	  return plusflo(na+1,(double)val,fp);
	return plusflo(na,floval(a),fp+1);
      }
#endif
#ifndef NO_BIGNUM
      if(t==BIGNUM){
	if(val!=0)
	  return plusbig(na+1,itobig(val,fp),fp);
	return plusbig(na,copybig(a,fp),fp+1);
      }
#endif
      nofixerr(a);
    }
    else if(t!=FIXNUM)nofixerr(a);
    fp++;
    val+=fixtoi(a);
    if(val<MINFIX||val>MAXFIX){
#ifndef NO_BIGNUM
      return plusbig(na,itobig(val,fp),fp);
#else
#ifndef NO_FLONUM
      return alflonum((double)val,fp);
#endif /* NO_FLONUM */
#endif
    }
  }
  return itofix(val);
}

#ifndef NO_FLONUM
WORD diffflo(int na, double fval, WORD *fp)
{
  WORD a;

  while(na-->0)
    fval-=coflo(a,pop(),fp);
  discard();
  return alflonum(fval,fp);
}
#endif /* NO_FLONUM */

#ifndef NO_BIGNUM
WORD diffbig(int na, WORD bigval, WORD *fp)
{
  WORD a,b,*sp;
  int t,answersize,size;

  answersize=fixtoi(osize(bigval))*4;
  l(0)=bigval;
  for(sp=fp;na-->0;sp++){
    if((t=tag(a= *sp))==FIXNUM){
      answersize=uadd(&bigcodes(l(0),0),-fixtoi(a),answersize);
    }
    else if(t!=OTHERS)nooterr(a);
    else{
      t=otag(a);
#ifndef NO_FLONUM
      if(t==FLONUM){
	osize(l(0))=itofix(answersize/4);
	return diffflo(na+1,bigtoflo(l(0)),sp);
      }
#endif
      if(t!=BIGNUM)nofixerr(a);
      else{
	if((size=fixtoi(osize(a))*4)>answersize){
	  l(0)=extbig(l(0),answersize/4,size/4,fp-1);
	  b= a;answersize=size;
	}
	else if(size<answersize){
	  l(1)=extbig(copybig(a,fp-1),size/4,answersize/4,fp-1);
	  l(0)=copybig(l(0),fp-2);
	  b=l(1);
	}
	else b=a;
	answersize=msub(&bigcodes(l(0),0),&bigcodes(b,0),answersize);
	heap=(WORD *)(l(0)+BIGCODES+answersize);
      }
    }
  }
  osize(l(0))=itofix(answersize/4);
  return normbig(l(0));
}
#endif

WORD difference_f(int na, WORD *fp)
{
  int val,t;
  WORD a;

  if(na<1)parerr();
  a= *(fp+na-1);
  if((t=tag(a))==OTHERS){
#ifndef NO_FLONUM
    if(otag(a)==FLONUM)
      return diffflo(na-1,floval(a),fp);
#endif
#ifndef NO_BIGNUM
    if(otag(a)==BIGNUM)
      return diffbig(na-1,copybig(a,fp),fp);
#endif
    nofixerr(a);
  }
  val=fixtoi(a);
  for(;--na>0;fp++){
    if((t=tag(a= *fp))==OTHERS){
#ifndef NO_FLONUM
      if(otag(a)==FLONUM){
	return diffflo(na,(double)val,fp);
      }
#endif
#ifndef NO_BIGNUM
      if(otag(a)==BIGNUM)
	return diffbig(na,itobig(val,fp),fp);
#endif
      nofixerr(a);
    }
    else if(t!=FIXNUM)nofixerr(a);
    val-=fixtoi(a);
    if(val<MINFIX||val>MAXFIX){
#ifndef NO_BIGNUM
      return diffbig(na-1,itobig(val,fp),fp);
#else
#ifndef NO_FLONUM
      return alflonum((double)val,fp);
#endif /* NO_FLONUM */
#endif
    }
  }
  discard();
  return itofix(val);
}

#ifndef NO_FLONUM
WORD timesflo(int na, double fval, WORD *fp)
{
  WORD a,*sp;

  for(sp=fp;na-->0;)
    fval*=coflo(a,*sp++,fp);
  return alflonum(fval,fp);
}
#endif /* NO_FLONUM */

#ifndef NO_BIGNUM
WORD timesbig(int na, WORD bigval, WORD *fp)
{
  WORD a,*sp;
  int t,answersize,size;

  answersize=fixtoi(osize(bigval))*4;
  l(0)=bigval;
  for(sp=fp;--na>0;sp++){
    if((t=tag(a= *sp))==FIXNUM){
      answersize=umul(&bigcodes(l(0),0),answersize+4,fixtoi(a));
    }
    else if(t!=OTHERS)nooterr(a);
    else{
      t=otag(a);
#ifndef NO_FLONUM
      if(t==FLONUM){
	osize(l(0))=itofix(answersize/4);
	return timesflo(na,bigtoflo(l(0)),sp);
      }
#endif
      if(t!=BIGNUM)nofixerr(a);
      else{
	size=fixtoi(osize(a))*4;
	osize(l(0))=itofix(answersize/4);
	l(0)=extzbig(l(0),answersize/4,(answersize+size)/4,fp-1);
	answersize+=size;
	answersize=mmul(&bigcodes(l(0),0),answersize,&bigcodes(a,0),size);
      }
    }
  }
  osize(l(0))=itofix(answersize/4);
  return normbig(l(0));
}
#else
WORD timesfix(int na,WORD val,WORD *fp)
{
  WORD a,b,*sp;
  int t,answersize,size,tmp,ival;

  ival=fixtoi(val);
  for(sp=fp;--na>0;sp++){
    if((t=tag(a= *sp))==FIXNUM){
      ival*=fixtoi(a);
    }
    else if(t!=OTHERS)nooterr(a);
#ifndef NO_FLONUM
    else if((t=otag(a))==FLONUM){
      return timesflo(na,(double)ival,sp);
    }
#endif
    else nofixerr(a);
  }
  return itofix(ival);
}
#endif

WORD times_f(int na, WORD *fp)
{
  int t;
  WORD a;

  if(na==0) return itofix(1);
  if((t=tag(a= ag(0)))==OTHERS){
#ifndef NO_FLONUM
    if(otag(a)==FLONUM){
      return timesflo(na-1,floval(a),fp+1);
    }
#endif
#ifndef NO_BIGNUM
    if(otag(a)==BIGNUM){
      return timesbig(na,copybig(a,fp),fp+1);
    }
#endif
    nofixerr(a);
  }
  else if(t!=FIXNUM)nofixerr(a);
#ifndef NO_BIGNUM
  return timesbig(na,itobig(fixtoi(a),fp),fp+1);
#else
  return timesfix(na,a,fp+1);
#endif
}

#ifndef NO_BIGNUM
int mmdiv(int *a0, int len0, int *a1, int len1)
{
  int size;

  if(*(a0+len0/4-1)<0){
    size=uadd(a0,-1,len0);
    size=mdiv(a0,size,a1,len1);
    if(*(a1+len1/4-1)>=0)
      size=uadd(a0,1,size&0xffff);
  }
  else{
    size=mdiv(a0,len0,a1,len1);
    if(*(a1+len1/4-1)<0)
      size=uadd(a0,1,size&0xffff);
  }
  return size;
}

int mmrem(int *a0, int len0, int *a1, int len1, WORD *fp)
{
  unsigned int rsize,qsize,size;

  if(*(a0+len0/4-1)<0){
    qsize=usub(a0,1,len0);
    if(qsize<len1+4){
      l(0)=(WORD)((WORD)a1-BIGCODES);
      a0=(int *)(extbig((WORD)a0-BIGCODES,qsize/4,(len1+4)/4,fp-1)+BIGCODES);
      a1=&bigcodes(l(0),0);
      qsize=len1+4;
    }
    qsize=mdiv(a0,qsize,a1,len1)&0xffff;
    rsize=uadd((int *)((char *)a0+(size=max(qsize,len0-len1))),1,len1);
    if(*(a1+len1/4-1)>=0){
      rsize=msub((int *)((char *)a0+max(len0-len1,qsize)),a1,len1);
    }
    return size|(rsize<<16);
  }
  else{
    qsize=mdiv(a0,len0,a1,len1);
    if(*(a1+len1/4-1)<0){
      qsize&=0xffff;
      rsize=msub((int *)((char *)a0+(size=max(len0-len1,qsize))),a1,len1);
      return size|(rsize<<16);
    }
    else
      return qsize;
  }
}

WORD quobig(int na, WORD bigval, WORD *fp)
{
  WORD a,*sp;
  int t,answersize,size,tmp;

  answersize=fixtoi(osize(bigval))*4;
  l(0)=bigval;
  for(sp=fp;na-->0;sp++){
    if((t=tag(a= *sp))==FIXNUM){
      tmp=fixtoi(a);
      if(tmp==0)zerodiverr(fp);
      answersize=mmdiv(&bigcodes(l(0),0),answersize,&tmp,4)&0xffff;
    }
    else{
      size=fixtoi(osize(a))*4;
      if(size>answersize+4)return itofix(0);
      else if(size>=answersize){
	l(0)=extbig(l(0),answersize/4,size/4+1,fp-1);
	answersize=size+4;
      }
      answersize=mmdiv(&bigcodes(l(0),0),answersize,&bigcodes(a,0),size)&0xffff;
    }
  }
  osize(l(0))=itofix(answersize/4);
  return normbig(l(0));
}
#endif

WORD quotient_f(int na, WORD *fp) /* fixnumしかサポートしていません */
         
{
  WORD a;
  int i,t,flag,val;
  double fval,fval1;

  if(na<1)parerr();
  for(flag=i=0;i<na;i++){
    if((t=tag(a=ag(i)))==OTHERS){
      t=otag(a);
#ifndef NO_FLONUM
      if(t==FLONUM){flag=2;break;}
#endif
#ifndef NO_BIGNUM
      if(t==BIGNUM){
	if(flag==0)flag=1;
	continue;
      }
#endif
      nofixerr(a);
    }
    else if(t!=FIXNUM)nofixerr(a);
  }
  switch(flag){
  case 0:
    val=fixtoi(ag(na-1));
    for(i=na-2;i>=0;i--){
      if((t=fixtoi(ag(i)))==0)zerodiverr(fp);
      val/=t;
    }
    return itofix(val);
#ifndef NO_BIGNUM
  case 1:
    if(tag(a=ag(na-1))==FIXNUM)
      return quobig(na-1,itobig(fixtoi(a),fp),fp);
    return quobig(na-1,copybig(ag(na-1),fp),fp);
#endif
#ifndef NO_FLONUM
  case 2:
    fval=coflo(a,ag(na-1),fp);
    for(i=na-2;i>=0;i--){
      if((fval1=coflo(a,ag(i),fp))==0.0)zerodiverr(fp);
      fval/=fval1;
    }
    return alflonum(fval,fp);
#endif /* NO_FLONUM */
  }
  return nil; /* dummy */
}
#ifndef NO_BIGNUM

WORD rembig(WORD a, WORD b, WORD *fp)
{
  int sizea,sizeb,sizeq,sizer,i;

  sizea=fixtoi(osize(a))*4;
  sizeb=fixtoi(osize(b))*4;
  if(sizeb>sizea+4)return a;
  else if(sizeb>=sizea){
    l(0)=b;
    a=extbig(a,sizea/4,sizeb/4+1,fp-1);
    sizea=sizeb+4;
    b=l(0);
  }
  sizeq=mmrem(&bigcodes(a,0),sizea,&bigcodes(b,0),sizeb,fp);
  sizer=(int)((unsigned)sizeq>>16);
  sizeq&=0xffff;
  for(i=0;i<sizer/4;i++)bigcodes(a,i)=bigcodes(a,i+sizeq/4);
  osize(a)=itofix(sizer/4);
  return normbig(a);
}
#endif

double fmod(double a, double b)
{
  return a-((int)(a/b))*b;
}

WORD remainder_f(int na, WORD *fp)
{
  WORD a,b,c;
  int t;
  double fval;
 
  if(na!=2)parerr();
  if((t=tag(a=ag(1)))==OTHERS){
    t=otag(a);
#ifndef NO_FLONUM
    if(t==FLONUM){
      if((t=tag(b=ag(0)))==OTHERS){
	if(otag(b)==FLONUM){
	  if(floval(b)==0.0)zerodiverr(fp);
	  fval=fmod(floval(a),floval(b));
	  return alflonum(fval,fp);
	}
#ifndef NO_BIGNUM
	else if(otag(b)==BIGNUM){
	  fval=fmod(floval(a),bigtoflo(b));
	  return alflonum(fval,fp);
	}
#endif
	else nosymerr(b);
      }
      else if(t!=FIXNUM)nofixerr(b);
      if((t=fixtoi(b))==0)zerodiverr(fp);
      fval=fmod(floval(a),(double)t);
      return alflonum(fval,fp);
    }
#endif /* NO_FLONUM */
#ifndef NO_BIGNUM
    if(t==BIGNUM){
      if((t=tag(b=ag(0)))==OTHERS){
#ifndef NO_FLONUM
	if(otag(b)==FLONUM){
	  if(floval(b)==0.0)zerodiverr(fp);
	  fval=fmod(bigtoflo(a),floval(b));
	  return alflonum(fval,fp);
	}
#endif /* NO_FLONUM */
	else if(otag(b)==BIGNUM)
	  return rembig(copybig(a,fp),b,fp);
	else nosymerr(b);
      }
      else if(t!=FIXNUM)nofixerr(b);
      if(b==itofix(0))zerodiverr(fp);
      c=itobig(fixtoi(b),fp);
      return rembig(copybig(a,fp),c,fp);
    }
#endif /* NO_BIGNUM */
    nofixerr(a);
  }
  else if(t!=FIXNUM)nofixerr(a);
  if((t=tag(b=ag(0)))==OTHERS){
    t=otag(b);
#ifndef NO_FLONUM
    if(t==FLONUM){
      if(floval(b)==0.0)zerodiverr(fp);
      fval=fmod((double)fixtoi(a),floval(b));
      return alflonum(fval,fp);
    }
#endif

#ifndef NO_BIGNUM
    if(t==BIGNUM)
      return rembig(itobig(fixtoi(a),fp),b,fp);
#endif
    nofixerr(b);
  }
  else if(t!=FIXNUM)nofixerr(b);
  if(b==itofix(0))zerodiverr(fp);
  return itofix(fixtoi(a)%fixtoi(b));
}

WORD add1_f(int na, WORD *fp)
{
  WORD a;
  int t,size;

  if(na!=1)parerr();
  if((t=tag(a=ag(0)))==FIXNUM){
#ifndef NO_BIGNUM
    if(a!=itofix(MAXFIX))return (WORD)((int)a+(int)itofix(1));
    return itobig(MAXFIX+1,fp);
#else
    return (WORD)((int)a+(int)itofix(1));
#endif
  }
  else if(t!=OTHERS)nofixerr(a);
#ifndef NO_FLONUM
  t=otag(a);
  if(t==FLONUM)return alflonum(floval(a)+1.0,fp);
#endif
#ifndef NO_BIGNUM
  else if(t==BIGNUM){
    a=copybig(a,fp);
    size=fixtoi(osize(a))*4;
    size=uadd(&bigcodes(a,0),1,size);
    osize(a)=itofix(size/4);
    return normbig(a);
  }
#endif
  return (nofixerr(a),a);
}

WORD sub1_f(int na, WORD *fp)
{
  WORD a;
  int t,size;

  if(na!=1)parerr();
  if((t=tag(a=ag(0)))==FIXNUM){
#ifndef NO_BIGNUM
    if(a!=itofix(MINFIX))return (WORD)((int)a-(int)itofix(1));
    return itobig(MINFIX-1,fp);
#else
    return (WORD)((int)a-(int)itofix(1));
#endif
  }
  else if(t!=OTHERS)nofixerr(a);
  t=otag(a);
#ifndef NO_FLONUM
  if(t==FLONUM)return alflonum(floval(a)-1.0,fp);
#endif
#ifndef NO_BIGNUM
  else if(t==BIGNUM){
    a=copybig(a,fp);
    size=fixtoi(osize(a))*4;
    size=usub(&bigcodes(a,0),1,size);
    osize(a)=itofix(size/4);
    return normbig(a);
  }
#endif
  return (nofixerr(a),a);
}

#ifndef NO_BIGNUM
WORD negbig2(WORD big, WORD *fp)
{
  WORD new;
  int size,i;

  size=fixtoi(osize(big));
  new=albignum(size+1,fp);
  for(i=0;i<size;i++)bigcodes(new,i)=0;
  size=msub(&bigcodes(new,0),&bigcodes(big,0),size*4);
  osize(new)=itofix(size/4);
  return normbig(new);
}
#endif

WORD abs_f(int na, WORD *fp)
{
  WORD a,b;
  int t,val;
  double fval;

  if(na!=1)parerr();
  if((t=tag(a=ag(0)))==FIXNUM){
#ifndef NO_BIGNUM
    if((val=fixtoi(a))==MINFIX){
      b=albignum(1,fp);
      bigcodes(b,0)= -MINFIX;
      return b;
    }
    else if(val<0)
#else
    if((val=fixtoi(a))<0)
#endif
      return itofix(-val);
    else return a;
  }
  else {
    t=otag(a);
#ifndef NO_FLONUM
    if(t==FLONUM){
      if((fval=floval(a))<0)
	return alflonum(-floval(a),fp);
      else return a;
    }
#endif
#ifndef NO_BIGNUM
    if(t==BIGNUM){
      if(bigcodes(a,fixtoi(osize(a))-1)&SIGNMASK){
	return negbig2(a,fp);
      }
      return a;
    }
#endif
    return (nofixerr(a),a);
  }
}

WORD minus_f(int na, WORD *fp)
{
  WORD a,b;
  int t,val;

  if(na!=1)parerr();
  if((t=tag(a=pop()))==FIXNUM){
    val=fixtoi(a);
#ifndef NO_BIGNUM
    if(val==MINFIX){
      b=albignum(1,fp);
      bigcodes(b,0)= -MINFIX;
      return b;
    }
#endif
    return itofix(-val);
  }
  else{
    t=otag(a);
#ifndef NO_FLONUM
    if(t==FLONUM){
      return alflonum(-floval(a),fp);
    }
#endif
#ifndef NO_BIGNUM
    if(t==BIGNUM)
      return negbig2(a,fp);
#endif
    return (nofixerr(a),a);
  }
}

#ifndef NO_FLONUM
WORD sin_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return alflonum(sin(coflo(a,pop(),fp)),fp);
}

WORD cos_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return alflonum(cos(coflo(a,pop(),fp)),fp);
}

WORD tan_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return alflonum(tan(coflo(a,pop(),fp)),fp);
}

WORD arcsin_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return alflonum(asin(coflo(a,pop(),fp)),fp);
}

WORD arccos_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return alflonum(acos(coflo(a,pop(),fp)),fp);
}

WORD arctan_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return alflonum(atan(coflo(a,pop(),fp)),fp);
}

WORD sqrt_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return alflonum(sqrt(coflo(a,pop(),fp)),fp);
}

WORD log_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return alflonum(log(coflo(a,pop(),fp)),fp);
}

WORD log10_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return alflonum(log10(coflo(a,pop(),fp)),fp);
}

WORD exp_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return alflonum(exp(coflo(a,pop(),fp)),fp);
}

WORD expt_f(int na, WORD *fp)
{
  if(na!=2)parerr();
  return udferr(nil,fp);
}

WORD exptflo_f(int na, WORD *fp)
{
  WORD a;
  double fval0,fval1;

  if(na!=2)parerr();
  fval1=floval(checkflo(a,pop()));
  fval0=floval(checkflo(a,pop()));
  return alflonum(exp(fval1*log(fval0)),fp);
}
#endif

#ifndef NO_BIGNUM
int cmpbigfix(WORD a, WORD b) /* a:big,b:fix */
         
{
  if(bigcodes(a,fixtoi(osize(a))-1)&SIGNMASK) /* bignum is negative */
    return -1;
  else return 1;
}

int cmpbigabs(WORD a, WORD b)
{
  int i;

  if(osize(a)>osize(b)){
    if(bigcodes(a,fixtoi(osize(a))-1)&SIGNMASK)return -1;
    return 1;
  }
  else if(osize(a)<osize(b)){
    if(bigcodes(a,fixtoi(osize(a))-1)&SIGNMASK)return 1;
    return -1;
  }
  for(i=fixtoi(osize(a))-1;i>=0;i--){
    if(bigcodes(a,i)>bigcodes(b,i))return 1;
    else if(bigcodes(a,i)<bigcodes(b,i))return -1;
  }
  return 0;
}

int cmpbigbig(WORD a, WORD b)
{
  if(bigcodes(a,fixtoi(osize(a))-1)&SIGNMASK){
    if(bigcodes(b,fixtoi(osize(b))-1)&SIGNMASK){ /* compare abs */
      return cmpbigabs(a,b);
    }
    else return -1;
  }
  if(bigcodes(b,fixtoi(osize(b))-1)&SIGNMASK)return 1;
  return cmpbigabs(a,b);
}

#endif
int cmpnum(WORD a, WORD b, WORD *fp) /* if a>b: 1, a=b:0 a<b:-1 */
             
{
  int t,val0,val1;
  double fval0,fval1;

  if((t=tag(a))==OTHERS){
    t=otag(a);
#ifndef NO_FLONUM
    if(t==FLONUM){
      fval0=floval(a);
      if((t=tag(b))==OTHERS){
	t=otag(b);
	if(t==FLONUM)
	  fval1=floval(b);
#ifndef NO_BIGNUM
	else if(t==BIGNUM)
	  fval1=bigtoflo(b);
#endif
	else nosymerr(b);
      }
      else if(t==FIXNUM)fval1=(double)fixtoi(b);
      else nofixerr(b);
      return (fval0>fval1?1:(fval0==fval1?0:-1));
    }
#endif
#ifndef NO_BIGNUM
    if(t==BIGNUM){
      if((t=tag(b))==OTHERS){
	t=otag(b);
#ifndef NO_FLONUM
	if(t==FLONUM){
	  fval0=bigtoflo(a);
	  fval1=floval(b);
	  return (fval0>fval1?1:(fval0==fval1?0:-1));
	}
#endif
	if(t==BIGNUM)
	  return cmpbigbig(a,b);
	nosymerr(b);
      }
      else if(t==FIXNUM)return cmpbigfix(a,b);
      else nofixerr(b);
    }
#endif /* NO_BIGNUM */
    nosymerr(b);
  }
  else if(t==FIXNUM){
    if((t=tag(b))==OTHERS){
      t=otag(b);
#ifndef NO_FLONUM
      if(t==FLONUM){
	fval0=(double)fixtoi(a);
	fval1=floval(b);
	return (fval0>fval1?1:(fval0==fval1?0:-1));
      }  
#endif
#ifndef NO_BIGNUM
      if(t==BIGNUM)
	return -cmpbigfix(b,a);
#endif
      nosymerr(b);
    }
    else if(t==FIXNUM){
      val0=fixtoi(a);val1=fixtoi(b);
      return (val0>val1?1:(val0==val1?0:-1));
    }
    else nofixerr(b);
  }
  else 
    nofixerr(a);
}

WORD greaterp_f(int na, WORD *fp)
{
  WORD a1,a2;

  if(na==0)return true;
  a2= pop();
  while(--na>0){
    if(cmpnum(a1=pop(),a2,fp)>0)a2=a1;
    else {
      return nil;
    }
  }
  return true;
}

WORD lessp_f(int na, WORD *fp)
{
  WORD a1,a2;

  if(na==0)return true;
  a2= pop();
  while(--na>0){
    if(cmpnum(a1=pop(),a2,fp)<0)a2=a1;
    else {
      return nil;
    }
  }
  return true;
}

WORD max_f(int na, WORD *fp)
{
  WORD a1,a2;

  if(na<1) parerr();
  a2= pop();
  while(--na>0)
    if(cmpnum(a1=pop(),a2,fp)>0)a2=a1;
  return a2;
}

WORD min_f(int na, WORD *fp)
{
  WORD a1,a2;

  if(na<1) parerr();
  a2= pop();
  while(--na>0)
    if(cmpnum(a1=pop(),a2,fp)<0)a2=a1;
  return a2;
}

