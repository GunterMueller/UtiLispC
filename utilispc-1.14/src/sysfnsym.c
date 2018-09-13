/* @(#)sysfnsym.c	1.6 90/07/30 */

#include "constdef.h"
#include "defvar.h"

WORD set_f(int na, WORD *fp)
{
  WORD newval,sym;

  if(na!=2)parerr();
  newval=ag(0);
  checksym(sym,ag(1));
  return value(sym)=newval;
}

WORD make_unbound_f(int na, WORD *fp)
{
  WORD sym;

  if(na!=1)parerr();
  checksym(sym,ag(0));
  value(sym)=UNBOUND;
  return sym;
}

WORD boundp_f(int na, WORD *fp)
{
  WORD sym;

  if(na!=1)parerr();
  checksym(sym,ag(0));
  if(value(sym)!=UNBOUND)return true;
  return nil;
}

WORD get_f(int na, WORD *fp)
{
  WORD name,plistp,sym,a;

  if(na!=2)parerr();
  name=ag(0);
  checksym(sym,ag(1));
  for(plistp=plist(sym);tag(plistp)==CONS;plistp=cdr(a)){
    checkcons(a,cdr(plistp));
    if(car(plistp)==name)return car(a);
  }
  return nil;
}

WORD putprop_f(int na, WORD *fp)
{
  WORD a,name,val,plistp,sym;

  if(na!=3)parerr();
  name=ag(0);
  val=ag(1);
  checksym(sym,ag(2));
  for(plistp=plist(sym);tag(plistp)==CONS;plistp=cdr(cdr(plistp)))
    if(car(plistp)==name)return car(cdr(plistp))=val;
  l(0)=sym;
  a=alcons(val,plist(sym),fp-1);
  plist(l(0))=alcons(ag(0),a,fp-1);
  return ag(1);
}

WORD remprop_f(int na, WORD *fp)
{
  WORD name,sym,top,plistp;

  if(na!=2)parerr();
  name=ag(0);
  top=plist(sym=ag(1));
  if(tag(top)!=CONS)return nil;
  if(car(top)==name){
    plist(sym)=cdr(cdr(top));
    return nil;
  }
  for(plistp=cdr(cdr(top));tag(plistp)==CONS;plistp=cdr(cdr(plistp)))
    if(car(plistp)!=name){
      top=cdr(cdr(top))=plistp;
    }
  cdr(cdr(top))=plistp;
  return nil;
}

WORD plist_f(int na, WORD *fp)
{
  WORD sym;

  if(na!=1)parerr();
  checksym(sym,ag(0));
  return plist(sym);
}

WORD setplist_f(int na, WORD *fp)
{
  WORD sym,val;

  if(na!=2)parerr();
  val=ag(0);
  checksym(sym,ag(1));
  return plist(sym)=val;
}

WORD pname_f(int na, WORD *fp)
{
  WORD sym;

  if(na!=1)parerr();
  checksym(sym,ag(0));
  return prname(sym);
}

WORD setpname_f(int na, WORD *fp)
{
  WORD sym,val;

  if(na!=2)parerr();
  checksym(sym,ag(1));
  checkstr(val,ag(0));
  return prname(sym)=val;
}


WORD symbol_f(int na, WORD *fp)
{
  WORD pname;

  if(na!=1)parerr();
  checkstr(pname,ag(0));
  return alsymbol(pname,fp);
}

WORD symbol_copy_f(int na, WORD *fp)
{
  WORD old,new;

  if(na!=1)parerr();
  checksym(old,ag(0));
  new=alsymbol(prname(old),fp);
  plist(new)=plist(old);
  value(new)=value(old);
  definition(new)=definition(old);
  return new;
}

WORD gensym_f(int na, WORD *fp) /* test */
         
{
  static char top[9]="g";
  static int gennum=0;
  char symstr[256];
  WORD a;

  switch(na){
  case 2:
    slow_checkfix(a,pop());
    gennum=fixtoi(a);
  case 1:
    checkstr(a,pop());
    strncpy(top,stringcodes(a),8);
    top[8]=0;
  case 0:
    sprintf(symstr,"%s%04d",top,gennum++);
    return alsym(symstr,strlen(symstr),fp);
  }
  parerr();
}
