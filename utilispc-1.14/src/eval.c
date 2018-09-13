/* @(#)eval.c	1.6 90/07/30 */

#include "constdef.h"
#include "defvar.h"

#ifdef AP1000
#include "string.h"
#include "/usr/cap/sys.c7/include/ccell.c7.h"
#endif

/* 関数evalはsrcをevalして、その結果を返す              */
WORD eval(WORD src, WORD *fp)
{
  int t=tag(src),na;
  WORD a,d,val,rval,car_a;
  WORD *bs,*sp;

  if(t==CONS){
#ifdef AP1000
  if(cprobe()!=0 && fixtoi(value(interrupt_mask))!=0){  /* when any cells are*/
                                              /* calling me, funcall break */
    value(interrupt_mask)=itofix(0);
    l(0)=src;
    funcall(value(message_handler),0,fp-1);
    src=l(0);
}
#endif
    d=cdr(src);
    a=car(src);
    if((t=tag(val=a))==SYMBOL){
#if 0
    loop1:
#endif
      a=definition(a);
      if((t=tag(a))==CONS){
	pushcb(val);
	goto conseval;
      }
      else if(t==OTHERS){
#ifndef OBJECTSPECIAL
	if((int *)a<etext){
	  pushcb(val);
	  a-=OTHERS; /* for escaping from MIPS C compiler optimizer*/
	  a=((FP)a)(d,fp);
	  discardcb();
	  return a;
	}
	else if(otag(a)==CODE)
	  goto codeeval;
#else
	if((t=otag(a))==CODE)
	  goto codeeval;
	else if(t==SPECIAL){
	  pushcb(val);
	  a=(spectop(a))(d,fp); /* この中でgcあり pushの必要なし */
	  discardcb();
	  return a;
	}
#endif
      }
#if 0
      else if(t==SYMBOL){
	goto loop1;
      }
#endif
      fnerr(a,fp);
    }
    else if(t==OTHERS && otag(a)==CODE){
    codeeval:
      pushcb(a);
      sp=fp-2;
      l(0)=d;l(1)=a;
      for(na=0;tag(d)==CONS;d= l(0)=cdr(l(0)),na++,sp--)
	*(sp-1)=EVAL(car(d),sp,a,d,t);
      a=(codetop(l(1)))(na,sp);
      discardcb();
      return a;
    }
    else if(t==CONS){
      pushcb(a);
    conseval:
      if((car_a=car(a))==lambda){
	a=cdr(a);
	sp=fp-3;
	l(2)=cdr(a);l(1)=car(a);l(0)=d;
	for(na=0;tag(d)==CONS;d=l(0)=cdr(l(0)),na++,sp--)
	  *(sp-1)=EVAL(car(d),sp,a,d,t);
	sp=fp-3;
	d=l(1);
	for(t=0,bs=bstack;t<na;d=l(1)=cdr(l(1)),t++,bs-=2,sp--){
	  if(tag(d)!=CONS)
	    parerr();
	  if(tag(a=car(d))==CONS)
	    a=car(a);
	  checksym(a,a);
	  *(bs-1)= a;
	  *(bs-2)=value(a);
	  value(a)= *(sp-1);
	}
	bstack=bs;
	checkb(0);
	for(a=l(1);tag(a)==CONS;a=l(1)=cdr(l(1)),na++){ 
	  if(tag(d=car(a))==CONS){
	    for(rval=nil,l(0)=val=cdr(d);tag(val)==CONS;l(0)=val=cdr(l(0)))
	      rval=eval(car(val),fp-3);
	    checkcons(car_a,car(l(1)));
	    *(bs-1)=checksym(d,car(car_a));
	    *(bs-2)=value(d);
	    value(d)=rval;
	    bstack=bs=bs-2;
	    checkb(0);
	  }
	  else parerr();
	}
	for(d=nil,car_a=l(2);tag(car_a)==CONS;car_a=l(2)=cdr(l(2)))
	  d=EVAL(car(car_a),fp-3,car_a,d,t);
	for(t=0;t<na;t++,bs+=2)
	  value(*(bs+1))= *bs;
	bstack=bs;
	discardcb();
	return d;
      }
      else if(car_a==macro){
	a=cdr(a);
	l(0)=d;
	d=funcall(a,1,fp-1);
	d=eval(d,fp);
	discardcb();
	return d;
      }
      else fnerr(a,fp);
    }
    else fnerr(a,fp);
  }
  else if(t!=SYMBOL)
    return src;
  else if((a=value(src))==UNBOUND)
    return (ubverr(src,fp),src);
  else return a;
}

/*  関数funcallはna個スタック上に積まれた引数をaでfuncallする */
/* change funcall not to use pushb, popb 1990 3/31 */
WORD funcall(WORD a, int na, WORD *fp)
{
  int t;
  WORD d,val,rval,car_a;
  WORD *sp,*bs;

  if((t=tag(val=a))==SYMBOL){
#if 0
  loop1:
#endif
    d=a;
    a=definition(a);
    if((t=tag(a))==CONS){
      pushcb(val);
      goto conseval;
    }
#ifndef OBJECTSPECIAL
    else if(t==OTHERS){
      if((int *)a<etext){
	pushcb(d);
	a=udferr(*fp,fp);
	discardcb();
	return a;
      }
      else if(otag(a)==CODE)
	goto codeeval;
      else
	fnerr(a,fp);
    }
#else
    else if(t==OTHERS && (t=otag(a))==CODE)
      goto codeeval;
#endif    
#if 0
    else if(t==SYMBOL)
      goto loop1;
#endif
    fnerr(a,fp);
  }
  else if(t==OTHERS && otag(a)==CODE){
  codeeval:
    pushcb(a);
    rval=(codetop(a))(na,fp);
    discardcb();
    return rval;
  }
  else if(t==CONS){
    pushcb(a);
  conseval:
    if((car_a=car(a))==lambda){
      a=cdr(a);
      car_a=cdr(a); /* body */
      a=car(a);	/* llist */
      sp=fp+na-1;
      bs=bstack;
      checkb(na);
      for(t=0;t<na;t++){
	if(tag(a)!=CONS) parerr();  /* 93/10/30 Iwasaki */
	if(tag(car(a))==CONS){
	  checkcons(d,car(a));
	  *(bs-1)=(d=car(d));
	}
	else 
	  *(bs-1)=(d=car(a));
	checksym(d,d);
	*(bs-2)=value(d);
	value(d)= *sp--;
	a=cdr(a);
	bs-=2;
      }
      bstack=bs;
      enter(3);
      l(2)=car_a;l(1)=a;l(0)=nil;
      for(;tag(a)==CONS;l(1)=a=cdr(l(1)),na++){ 
	/* 仮引数のデフォルト値 */
	if(tag(d=car(a))==CONS){
	  for(rval=nil,l(0)=val=cdr(d);tag(val)==CONS;l(0)=val=cdr(l(0)))
	    rval=EVAL(car(val),sp,a,d,t);
	  checkcons(d,car(l(1)));
	  *(bs-1)=checksym(d,car(d));
	  *(bs-2)=value(d);
	  value(d)=rval;
	  bs=bs-2;
	  bstack=bs;
	  checkb(0);
	}
	else parerr();
      }
      for(d=nil,car_a=l(2);tag(car_a)==CONS;car_a=l(2)=cdr(l(2))){
#if 1
	d=EVAL(car(car_a),sp,car_a,d,t);
#else
	d=eval(car(car_a),sp);
#endif
      }
      for(bs=bstack,t=0;t<na;t++){
	bs+=2;
	a= *(bs-2);
	value(*(bs-1))=a;
      }
      bstack=bs;
      discardcb();
      return d;
    }
    else fnerr(a,fp);
  }
  else fnerr(a,fp);
}


WORD apply(WORD a, WORD d, WORD *fp)
{
  int na;

  for(na=0;tag(d)==CONS;d=cdr(d),na++)
    *--fp=car(d);
  return funcall(a,na,fp);
}

