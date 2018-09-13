/* @(#)sysfnlist.c	1.6 90/07/30 */

#include "constdef.h"
#include "defvar.h"

#define ccar(a,l) car(checkcons(a,l))
#define ccdr(a,l) cdr(checkcons(a,l))

WORD cr_f(int na, WORD *fp)
{
  if(na!=1)parerr();
  return ag(0);
}

WORD car_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return ccar(a,ag(0));
}

WORD cdr_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return ccdr(a,ag(0));
}

WORD caar_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return ccar(a,ccar(a,ag(0)));
}
WORD cdar_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return ccdr(a,ccar(a,ag(0)));
}
WORD cadr_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return ccar(a,ccdr(a,ag(0)));
}
WORD cddr_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return ccdr(a,ccdr(a,ag(0)));
}

WORD caaar_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return ccar(a,ccar(a,ccar(a,ag(0))));
}
WORD caadr_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return ccar(a,ccar(a,ccdr(a,ag(0))));
}
WORD cadar_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return ccar(a,ccdr(a,ccar(a,ag(0))));
}
WORD caddr_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return ccar(a,ccdr(a,ccdr(a,ag(0))));
}
WORD cdaar_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return ccdr(a,ccar(a,ccar(a,ag(0))));
}
WORD cdadr_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return ccdr(a,ccar(a,ccdr(a,ag(0))));
}
WORD cddar_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return ccdr(a,ccdr(a,ccar(a,ag(0))));
}
WORD cdddr_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return ccdr(a,ccdr(a,ccdr(a,ag(0))));
}

WORD caaaar_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return ccar(a,ccar(a,ccar(a,ccar(a,ag(0)))));
}
WORD caaadr_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return ccar(a,ccar(a,ccar(a,ccdr(a,ag(0)))));
}
WORD caadar_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return ccar(a,ccar(a,ccdr(a,ccar(a,ag(0)))));
}
WORD caaddr_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return ccar(a,ccar(a,ccdr(a,ccdr(a,ag(0)))));
}
WORD cadaar_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return ccar(a,ccdr(a,ccar(a,ccar(a,ag(0)))));
}
WORD cadadr_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return ccar(a,ccdr(a,ccar(a,ccdr(a,ag(0)))));
}
WORD caddar_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return ccar(a,ccdr(a,ccdr(a,ccar(a,ag(0)))));
}
WORD cadddr_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return ccar(a,ccdr(a,ccdr(a,ccdr(a,ag(0)))));
}
WORD cdaaar_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return ccdr(a,ccar(a,ccar(a,ccar(a,ag(0)))));
}
WORD cdaadr_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return ccdr(a,ccar(a,ccar(a,ccdr(a,ag(0)))));
}
WORD cdadar_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return ccdr(a,ccar(a,ccdr(a,ccar(a,ag(0)))));
}
WORD cdaddr_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return ccdr(a,ccar(a,ccdr(a,ccdr(a,ag(0)))));
}
WORD cddaar_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return ccdr(a,ccdr(a,ccar(a,ccar(a,ag(0)))));
}
WORD cddadr_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return ccdr(a,ccdr(a,ccar(a,ccdr(a,ag(0)))));
}
WORD cdddar_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return ccdr(a,ccdr(a,ccdr(a,ccar(a,ag(0)))));
}
WORD cddddr_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return ccdr(a,ccdr(a,ccdr(a,ccdr(a,ag(0)))));
}

WORD fifth_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return ccar(a,ccdr(a,ccdr(a,ccdr(a,ccdr(a,ag(0))))));
}
WORD sixth_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return ccar(a,ccdr(a,ccdr(a,ccdr(a,ccdr(a,ccdr(a,ag(0)))))));
}
WORD seventh_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return ccar(a,ccdr(a,ccdr(a,ccdr(a,ccdr(a,ccdr(a,ccdr(a,ag(0))))))));
}

WORD cons_f(int na, WORD *fp)
{
  if(na!=2)parerr();
  return alcons(ag(1),ag(0),fp);
}

WORD ncons_f(int na, WORD *fp)
{
  if(na!=1)parerr();
  return alcons(ag(0),nil,fp);
}

WORD xcons_f(int na, WORD *fp)
{
  if(na!=2)parerr();
  return alcons(ag(0),ag(1),fp);
}

/* lastはnilに対して働かないことに注意 */

WORD last_f(int na, WORD *fp)
{
  WORD d,w;

  if(na!=1)parerr();
  for(checkcons(d,ag(0));tag(w=cdr(d))==CONS;d=w);
  return d;
}

WORD length_f(int na, WORD *fp)
{
  WORD d;
  int len=0;

  if(na!=1)parerr();
  for(d=ag(0);tag(d)==CONS;len++,d=cdr(d));
  return itofix(len);
}

WORD nth_f(int na, WORD *fp)
{
  int n;
  WORD a,d;

  if(na!=2)parerr();
  checkcons(d,ag(0));
  if((n=fixtoi(slow_checkfix(a,ag(1))))<0)nosymerr(a);
  while(--n>=0)checkcons(d,cdr(d));
  return car(d);
}

WORD nthcdr_f(int na, WORD *fp)
{
  int n;
  WORD a,d;

  if(na!=2)parerr();
  d= ag(0); /* ここでは、dがリストであるかのチェックを行なっていはいけない */
  if((n=fixtoi(slow_checkfix(a,ag(1))))<0)nosymerr(a);
  while(--n>=0)checkcons(d,d),d=cdr(d);
  return d;
}

WORD list_f(int na, WORD *fp)
{
  return allist(na,fp,nil);
}

WORD list__f(int na, WORD *fp)
{
  if(na==0)parerr();
  return allist(na-1,fp+1,ag(0));
}

WORD append_f(int na, WORD *fp)
{
  WORD d,*sp,*ap;
  int len;

  if(na<=0)return nil;
  for(len=0,ap=fp+na-1,sp=fp;--na>0;ap--){
    for(d= *ap;tag(d)==CONS;len++,d=cdr(d))
      *--sp=car(d);
  }
  return allist(len,sp,ag(0));
}

WORD reverse_f(int na, WORD *fp)
{
  WORD d,dd;

  if(na!=1)parerr();
  if(tag(d= ag(0))!=CONS)return d;
  dd=nil;
  while(tag(d)==CONS){
    dd=alcons(car(d),dd,fp);
    d=ag(0)=cdr(ag(0));
  }
  return dd;
}

WORD nconc_f(int na, WORD *fp)
{
  WORD top,oldtop,d,dd;

  if(na==0)return nil;
  top=pop();
  while(--na>0){
    d=pop();
    if(tag(d)!=CONS)continue;
    oldtop=top;
    top=d;
    d=cdr(dd=d);
    while(tag(d)==CONS)
      d=cdr(dd=d);
    cdr(dd)=oldtop;
  }
  return top;
}

WORD nreverse_f(int na, WORD *fp)
{
  WORD d,dd,ddd;

  if(na!=1)parerr();
  ddd=d=ag(0);dd=nil;
  while(tag(ddd)==CONS){
    d=ddd;ddd=cdr(d);
    cdr(d)=dd;
    dd=d;
  }
  return d;
}

WORD rplaca_f(int na, WORD *fp)
{
  WORD y,rval;

  if(na!=2)parerr();
  y= ag(0);
  checkcons(rval,ag(1));
  car(rval)=y;
  return rval;
}

WORD rplacd_f(int na, WORD *fp)
{
  WORD y,rval;

  if(na!=2)parerr();
  y= ag(0);
  checkcons(rval,ag(1));
  cdr(rval)=y;
  return rval;
}

WORD memq_f(int na, WORD *fp)
{
  WORD item,d;

  if(na!=2)parerr();
  item=ag(1);
  for(d=ag(0);tag(d)==CONS;d=cdr(d))
    if(car(d)==item)
      return d;
  return nil;
}

WORD delq_f(int na, WORD *fp)
{
  WORD item,d,dd,top;
  int n;

  if(na==3){
    if(tag(d=ag(0))!=FIXNUM)parerr();
    n=fixtoi(d);
    fp++;
  }
  else if(na==2)
    n=1000000; /* test */
  else parerr();
  top=ag(0);
  item=ag(1);
  while(tag(top)==CONS && car(top)==item)top=cdr(top);
  if(top==nil)return nil;
  d=cdr(dd=top);
  while(tag(d)==CONS && n){
    if(car(d)!=item)
      dd= cdr(dd)=d;
    else n--;
    d=cdr(d);
  }
  cdr(dd)=d;
  return top;
}

WORD remq_f(int na, WORD *fp)
{
  WORD item,d,dd;
  int n;

  if(na==3){
    if(tag(d= ag(0))!=FIXNUM)parerr();
    n=fixtoi(d);
    fp++;
  }
  else if(na==2)
    n=1000000; /* test */
  else parerr();
  for(dd=ag(0),item=ag(1),n=0;tag(dd)==CONS;dd=cdr(dd))
    if(car(dd)!=item){
      l(0)=car(dd); n++; fp--;
    }
  return allist(n,fp,dd);
}

WORD member_f(int na, WORD *fp)
{
  WORD item,d;

  if(na!=2)parerr();
  d=ag(0);
  item=ag(1);
  while(tag(d)==CONS){
    if(equal(car(d),item))
      return d;
    d=cdr(d);
  }
  return nil;
}

WORD mem_f(int na, WORD *fp)
{
  WORD d;

  if(na!=3)parerr();
  d=ag(0);
  while(tag(d)==CONS){
    l(0)=ag(1);		/* item */
    l(1)=car(d);
    if(funcall(ag(2),2,fp-2)!=nil)return ag(0);
    d=ag(0)=cdr(ag(0));
  }
  return nil;
}

WORD every_f(int na, WORD *fp)
{
  WORD d;

  if(na!=2)parerr();
  d=ag(1);		/* list */
  while(tag(d)==CONS){
    l(0)=car(d);
    if(funcall(ag(0),1,fp-1)==nil)return nil;
    d=ag(1)=cdr(ag(1));
  }
  return true;
}

WORD some_f(int na, WORD *fp)
{
  WORD d;

  if(na!=2)parerr();
  d=ag(1);		/* list */
  while(tag(d)==CONS){
    l(0)=car(d);
    if(funcall(ag(0),1,fp-1)!=nil)return ag(1);
    d=ag(1)=cdr(ag(1));
  }
  return nil;
}

WORD assq_f(int na, WORD *fp)
{
  WORD item,d,dd,a;

  if(na!=2)parerr();
  d= ag(0);item= ag(1);
  while(tag(d)==CONS){
    if(ccar(a,dd=car(d))==item)return dd;
	d=cdr(d);
  }
  return nil;
}
WORD assoc_f(int na, WORD *fp)
{
  WORD item,d,dd,a;

  if(na!=2)parerr();
  d= ag(0);item= ag(1);
  while(tag(d)==CONS){
    if(equal(ccar(a,dd=car(d)),item))return dd;
	d=cdr(d);
  }
  return nil;
}

WORD ass_f(int na, WORD *fp)
{
  WORD a,d;

  if(na!=3)parerr();
  d=ag(0);		/* alist */
  while(tag(d)==CONS){
    l(0)=ag(1);		/* item */
    l(1)=ccar(a,car(d));
    if(funcall(ag(2),2,fp-2)!=nil)return car(ag(0));
    d=ag(0)=cdr(ag(0));
  }
  return nil;
}

WORD copy(WORD d, WORD *fp)
{
  WORD a;
  int n;

  if(tag(d)!=CONS)return d;
  for(n=0,l(0)=d;tag(d)==CONS;n++){
    l(0)=d;
    a=copy(car(d),fp-1);
    d=cdr(l(0));
    l(0)=a;
    fp--;
  }
  return allist(n,fp,d);
}

WORD copy_f(int na, WORD *fp)
{
  if(na!=1)parerr();
  return copy(ag(0),fp);
}

WORD subst(WORD dst, WORD src, WORD d, WORD *fp)
{
  WORD a;
  
  if(d==src)return dst;
  else if(tag(d)!=CONS)return d;
  l(0)=dst;l(1)=src;l(2)=d;
  l(3)=subst(l(0),l(1),car(l(2)),fp-3);
  a=subst(l(0),l(1),cdr(l(2)),fp-4);
  return alcons(l(3),a,fp);
}

WORD subst_f(int na, WORD *fp)
{
  if(na!=3)parerr();
  return subst(ag(2),ag(1),ag(0),fp);
}




