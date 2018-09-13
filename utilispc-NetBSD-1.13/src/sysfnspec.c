/* @(#)sysfnspec.c	1.6 90/07/30 */

#include <stdio.h>
#include "constdef.h"
#include "defvar.h"

/* gc のための情報はpushしてある。 */
WORD and_e(WORD a, WORD *fp)
{
  WORD rval;

  l(0)=a;
  for(rval=true;tag(a)==CONS;a=l(0)=cdr(l(0)))
    if((rval=eval(car(a),fp-1))==nil)break;
  return rval;
}

WORD or_e(WORD a, WORD *fp)
{
  WORD rval;

  l(0)=a;
  for(rval=nil;tag(a)==CONS;a=l(0)=cdr(l(0)))
    if((rval=eval(car(a),fp-1))!=nil)break;
  return rval;
}

WORD cond_e(WORD a, WORD *fp)
{
  WORD rval,d;

  l(0)=a;
  for(;tag(a)==CONS;a=l(0)=cdr(l(0))){
    checkcons(d,car(a));
    if((rval=eval(car(d),fp-1))!=nil){
      l(0)=a=cdr(car(l(0)));
      for(;tag(a)==CONS;a=l(0)=cdr(l(0)))
	rval=eval(car(a),fp-1);
      return rval;
    }
  }
  return nil;
}

WORD progn_e(WORD a, WORD *fp)
{
  WORD rval=nil;

  for(l(0)=a;tag(a)==CONS;a=l(0)=cdr(l(0))){
    rval=eval(car(a),fp-1);
  }
  return rval;
}

WORD selectq_e(WORD a, WORD *fp)
{
  WORD key,pattern,b;

  if(tag(a)!=CONS)parerr();
  l(0) = a;
  key=eval(car(a),fp-1);
  a = l(0);
  for(a=cdr(a);tag(a)==CONS;a=cdr(a)){
    if(tag(b=car(a))!=CONS)parerr();
    if(tag(pattern=car(b))==CONS){
      for(;tag(pattern)==CONS;pattern=cdr(pattern))
	if(car(pattern)==key)
	  return progn_e(cdr(car(a)),fp);
      continue;
    }
    else if(pattern==true)
      return progn_e(cdr(car(a)),fp);
    else if(pattern==key)
      return progn_e(cdr(car(a)),fp);
  }
  return nil;
}

WORD prog_e(WORD a, WORD *fp)
{
  WORD *tmpcstack,val,rval,body,sym;
  WORD *orgstack,arglist;
  int args=0;
  jmp_buf env;

  pushe();
  l(2)=a;
  estack->envtag=PROGENV;
  estack->envobj=l(2);
  estack->envbuf=(jmp_buf *)&env;
  estack->bstack=bstack;
  orgstack=bstack;
  tmpcstack=cstack;
  if(_setjmp(env)){
    while(bstack<estack->bstack){
      val=popb();
      value(popb())=val;
    }
    cstack=tmpcstack;
    if(progtag==(WORD)0){
      rval=progval;
      goto unbound;
    }
    l(1)=body=progval;
    goto goent;
  }
  else{
    for(l(1)=arglist=car(a);tag(arglist)==CONS;arglist=l(1)=cdr(l(1)),args++){
      if(tag(sym=car(arglist))!=CONS){
	pushb(sym);pushb(value(sym));value(sym)=nil;
      }
      else{
	estack->bstack=bstack;
	for(l(3)=sym,val=nil,l(0)=body=cdr(car(arglist));tag(body)==CONS;body=l(0)=cdr(l(0)))
	  val=eval(car(body),fp-4);
	pushb(sym=car(l(3)));pushb(value(sym));value(sym)=val;
      }
    }
    estack->bstack=bstack;
    l(1)=body=cdr(l(2));
  goent:
    for(;tag(body)==CONS;body=l(1)=cdr(l(1)))
      if(tag(car(body))==CONS)
        eval(car(body),fp-3);
    rval=nil;
  unbound:
    estack++;
    while(bstack<orgstack){
      val=popb();
      value(popb())=val;
    }
    return rval;
  }
}

WORD catch_e(WORD a, WORD *fp)
{
  WORD *tmpbstack=bstack,val,body;
  WORD *tmpcstack=cstack;
  jmp_buf env;

  if(_setjmp(env)){
    while(bstack<tmpbstack){
      val=popb();
      value(popb())=val;
    }
    estack++;
    cstack=tmpcstack;
    return catchval;
  }
  else{
    lstack=fp;
    l(0)=a;
    l(1)=eval(car(a),fp-1);
    pushe();
    estack->envtag=CATCHENV;
    estack->envobj=l(1);
    estack->envbuf=(jmp_buf *)&env;
    for(val=nil,l(0)=body=cdr(l(0));tag(body)==CONS;body=l(0)=cdr(l(0)))
      val=eval(car(body),fp-2);
    estack++;
    return val;
  }
}

WORD go_e(WORD a, WORD *fp)
{
  struct lispenv *envptr;
  WORD d,w;

  a=car(a);
  for(envptr=estack;envptr<estackbottom;envptr++)
    if(envptr->envtag==PROGENV){
      for(d=envptr->envobj;tag(d)==CONS;d=cdr(d))
	if(tag(w=car(d))!=CONS && w==a){
	  l(0)=d;
	  eval_unwind(envptr,fp-1);
	  d=l(0);
	  progval=cdr(d);
	  progtag=(WORD)8;
	  estack=envptr;
	  _longjmp(*(envptr->envbuf),1);
	}
    }
  goerr(a,fp);
}

WORD quote_e(WORD a, WORD *fp)
{
  checkcons(a,a);
  return car(a);
}
WORD comment_e(WORD a, WORD *fp)
{
  return nil;
}

WORD prog1_e(WORD a, WORD *fp)
{
  l(0)=a;
  if(tag(a)!=CONS)parerr();
  l(1)=eval(car(a),fp-1);
  for(l(0)=a=cdr(l(0));tag(a)==CONS;a=l(0)=cdr(l(0)))
    eval(car(a),fp-2);
  return l(1);
}

WORD prog2_e(WORD a, WORD *fp)
{
  l(0)=a;
  if(tag(a)!=CONS)parerr();
  eval(car(a),fp-1);
  return prog1_e(cdr(l(0)),fp);
}

WORD push_e(WORD a, WORD *fp)
{
  WORD l0,list,val;

  if(tag(a)!=CONS || tag(cdr(a))!=CONS) parerr();
  checksym(l0,car(cdr(a)));
  l(0)=l0;
  val=eval(car(a),fp-1);
  if((a=value(l(0)))==UNBOUND) ubverr(l(0),fp);
  list=alcons(val,a,fp-1);
  return value(l(0))=list;
}

WORD pop_e(WORD a, WORD *fp)
{
  WORD rval,d,car_a;

  if(tag(a)!=CONS) parerr();
  checkcons(d,value(checksym(car_a,car(a))));
  rval=car(d);
  value(car_a)=cdr(d);
  return rval;
}

WORD setq_e(WORD a, WORD *fp)
{
  WORD val,rval,sym,*sp;
  int na;

  for(rval=nil,na=0,sp=fp-1;tag(a)==CONS;na++,sp-=2){
    checksym(sym,car(a));
    *(sp-1)=sym;
    checkcons(a,cdr(a));
    l(0)=a;
    *(sp-2)=rval=eval(car(a),sp-1); /* changed by tan 1988/11/28 */
    a=cdr(l(0));
  }
  for(;na-->0;sp+=2){
    val= *sp;
    value(*(sp+1))=val;
  }
  return rval;
}

WORD loop_e(WORD a, WORD *fp)
{
  WORD *tmpbstack=bstack,val,body;
  WORD *tmpcstack=cstack;
  jmp_buf env;
  
  if(_setjmp(env)){
    while(bstack<tmpbstack){
      val=popb();
      value(popb())=val;
    }
    cstack=tmpcstack;
    estack++;
    return loopval;
  }
  else{
    pushe();
    estack->envtag=LOOPENV;
    estack->envobj=a; /* changed by tan 1988/11/28 */
    estack->envbuf=(jmp_buf *)&env;
    l(0)=a;
    for(;;)
      for(body=l(1)=l(0);tag(body)==CONS;l(1)=body=cdr(l(1)))
	eval(car(body),fp-2);
  }
}

WORD unwind_protect_e(WORD a, WORD *fp)
{
  WORD body;

  pushe();
  estack->envtag=UNWINDENV;
  if(tag(a)!=CONS)parerr();
  estack->envobj=cdr(a);
  estack->envbuf=(jmp_buf *)NULL;
  estack->bstack=bstack;
  estack->cstack=cstack;
  l(0)=eval(car(a),fp);
  l(1)=estack->envobj;
  estack++;
  for(body=l(1);tag(body)==CONS;l(1)=body=cdr(l(1)))
    eval(car(body),fp-2);
  return l(0);
}

int match(WORD pattern, WORD key)
{
  int t;

  if(pattern==nil){
    if(key==nil)
      return 1;
    else
      return 0;
  }
  else if((t=tag(pattern))==SYMBOL){
    bind(pattern,key);
    return 1;
  }
  else if(t!=CONS){
    if(pattern==key)return 1;
  }
  else if(car(pattern)==quote){
    if(car(cdr(pattern))==key)
      return 1;
  }
  else if(tag(key)==CONS && match(car(pattern),car(key)) &&
	  match(cdr(pattern),cdr(key)))
    return 1;
  return 0;
}

void unbind(WORD *tmpstack)
{
  WORD val;

  while(bstack<tmpstack){
    val=popb();
    value(popb())=val;
  }
}
      
WORD match_e(WORD a, WORD *fp)
{
  WORD key,rval,*tmpbstack;

  l(0)=a;
  key=eval(car(a),fp-1);
  tmpbstack=bstack;
  for(a=cdr(l(0));tag(a)==CONS;a=l(0)=cdr(l(0)))
    if(match(car(car(a)),key)){
      rval=progn_e(cdr(car(a)),fp-1);
      unbind(tmpbstack);
      return rval;
    }
    else
      unbind(tmpbstack);
  return nil;
}

/* from sysfneval.c */

WORD let_e(WORD a, WORD *fp)
{
  WORD arglist,body,val,rval,*sp,*bs;
  int args=0,t;

  enter(3);
  l(0)=a;
  for(l(1)=arglist=car(a);tag(arglist)==CONS;arglist=l(1)=cdr(l(1)),args++){
    *(sp-1)=car(car(arglist));
    body=l(2)=cdr(car(arglist));
    if(tag(body)==CONS){
      rval=eval(car(body),sp-1);
      if(tag(cdr(l(2)))==CONS)parerr();
    }
    else parerr();
    sp-=2;
    *sp=rval;
  }
  for(t=0,sp=fp-3,bs=bstack;t<args;t++,sp-=2,bs-=2){
    *(bs-1)= *(sp-1);
    *(bs-2)=value(*(sp-1));
    value(*(sp-1))= *(sp-2);
  }
  bstack=bs;
  for(rval=nil,l(0)=body=cdr(l(0));tag(body)==CONS;body=l(0)=cdr(l(0)))
    rval=eval(car(body),fp-1);
  for(t=0;t<args;t++){
    val=popb();
    value(popb())=val;
  }
  return rval;
}

WORD lets_e(WORD a, WORD *fp)
{
  WORD arglist,body,rval,val;
  int args=0,t;

  l(0)=a;
  for(l(1)=arglist=car(a);tag(arglist)==CONS;arglist=l(1)=cdr(l(1)),args++){
    pushb(l(2)=car(car(arglist)));
    pushb(value(l(2)));
    for(l(3)=body=cdr(car(arglist)),val=nil;tag(body)==CONS;body=l(3)=cdr(l(3)))
      val=eval(car(body),fp-4);
    value(l(2))=val;
  }
  for(rval=nil,l(1)=body=cdr(l(0));tag(body)==CONS;body=l(1)=cdr(l(1)))
    rval=eval(car(body),fp-2);
  for(t=0;t<args;t++){
    val=popb();
    value(popb())=val;
  }
  return rval;
}

WORD exit_f(int na, WORD *fp)
{
  struct lispenv *envptr;

  if(na==0)loopval=nil;
  else loopval=ag(0);
  for(envptr=estack;envptr<estackbottom;envptr++)
    if(envptr->envtag==LOOPENV){
      eval_unwind(envptr,fp);
      estack=envptr;
      _longjmp(*(envptr->envbuf),1);
    }
  reterr(loopval,fp);
}

WORD return_f(int na, WORD *fp)
{
  struct lispenv *envptr;

  if(na==0)progval=nil;
  else progval=ag(0);
  for(envptr=estack;envptr<estackbottom;envptr++)
    if(envptr->envtag==PROGENV){
      eval_unwind(envptr,fp);
      progtag=(WORD)0;
      estack=envptr;
      _longjmp(*(envptr->envbuf),1);
    }
  reterr(progval,fp);
}

WORD throw_f(int na, WORD *fp)
{
  struct lispenv *envptr;
  WORD a;

  if(na==0)parerr();
  a=ag(na-1);
  if(na>=2)catchval=ag(0);
  else catchval=nil;
  for(envptr=estack;envptr<estackbottom;envptr++)
    if(envptr->envtag==CATCHENV && envptr->envobj==a){
      eval_unwind(envptr,fp);
      estack=envptr;
      _longjmp(*(envptr->envbuf),1);
    }
  ctcherr(a,fp);
}

void eval_unwind(struct lispenv *envptr, WORD *fp)
{
  struct lispenv *ep;
  WORD body;

  for(ep=estack;ep<envptr;ep++){
    if(ep->envtag==UNWINDENV){
      unbind(ep->bstack);
      cstack=ep->cstack;
      estack=ep+1;
      for(body=l(0)=ep->envobj;body!=nil;body=l(0)=cdr(l(0)))
	eval(car(body),fp-1);
    }
  }
}
