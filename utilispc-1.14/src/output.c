/* @(#)output.c	1.6 90/07/30 */

#include <stdio.h> /* for debugging */
#include "constdef.h"
#include "defvar.h"
#include "read.h"

int slash=0;

/* ストリームに文字を一文字出力する */

void FLUSH(WORD stream)
{
  int len,wlen;

  if(mode(stream)&STRINGSTREAM){
    if(fildes(stream)+(len=curpos(stream)-recend(stream))>fixtoi(osize(filname(stream))))
      ioerr(stream,lstack);
    memmove(stringcodes(filname(stream))+fildes(stream),buftop(stream)+recend(stream),len);
#ifdef notdef
    *(stringcodes(filname(stream))+fildes(stream)+len)=0;
#endif
    fildes(stream)+=len;
  }
  else{
    for(wlen=0,len=curpos(stream)-recend(stream);wlen<len;)
      wlen+=write(fildes(stream),buftop(stream)+recend(stream)+wlen,len-wlen);
  }
  recend(stream)=curpos(stream);
}

void PUTC(int c, WORD stream)
{
  *(buftop(stream)+curpos(stream)++)=c;
  if(c=='\n'){
    FLUSH(stream);
    recend(stream)=curpos(stream)=0;
  }
  if(curpos(stream)>=BUFSIZE){
    FLUSH(stream);
    recend(stream)=curpos(stream)=0;
  }
  return;
}

/* Cの文字列をストリームに出力する */

void PUTS(char *str, WORD stream)
{
  while(*str)
    PUTC(*str++,stream);
}

WORD print_fixnum_f(int na, WORD *fp)
{
  WORD ll,stream;
  char strbuf[STRBUFSIZE];
  if(na<1)parerr();
  checkstream(stream,value(standard_output));
  if(!(mode(stream)&OUTOPEN))ioerr(stream,fp);
  sprintf(strbuf,"%d",fixtoi(ll=ag(na-1)));
  PUTS(strbuf,stream);
  return ll;
}

WORD print_symbol_f(int na, WORD *fp)
{
  WORD ll,stream,str,rtable;
  int flag=0,i;

  if(na==2){
    ll=ag(1);
    flag=(ag(0)==nil?0:1);
  }
  else if(na==1)
    ll=ag(0);
  else parerr();
  checkstream(stream,value(standard_output));
  if(!(mode(stream)&OUTOPEN))ioerr(stream,fp);
  checkvec(rtable,value(readtable));
  if(osize(rtable)!=itofix(256))nosymerr(rtable);
  str=prname(ll);
  if(flag && ((int)vecelm(rtable,*(stringcodes(str)))&SLASHTOPBIT))
    PUTC('/',stream);
  PUTC(*(stringcodes(str)),stream);
  for(i=1;i<fixtoi(osize(str));i++){
    if(flag && ((int)vecelm(rtable,*(stringcodes(str)+i))&SLASHBIT))
      PUTC('/',stream);
    PUTC(*(stringcodes(str)+i),stream);
  }
  return ll;
}

WORD print_vector_f(int na, WORD *fp)
{
  WORD ll,stream;
  char strbuf[STRBUFSIZE];
  if(na<1)parerr();
  checkstream(stream,value(standard_output));
  if(!(mode(stream)&OUTOPEN))ioerr(stream,fp);
  sprintf(strbuf,"V#%d",(int)(ll=ag(na-1)));
  PUTS(strbuf,stream);
  return ll;
}

WORD print_stream_f(int na, WORD *fp)
{
  WORD a,ll,stream;
  char strbuf[STRBUFSIZE];

  if(na<1)parerr();
  checkstream(stream,value(standard_output));
  if(!(mode(stream)&OUTOPEN))ioerr(stream,fp);
  PUTS("S#",stream);
  if(tag(a=filname(ll=ag(na-1)))==FIXNUM) {
    sprintf(strbuf,"%d",fixtoi(a));
    PUTS(strbuf,stream);
  } else{
    l(0)=a;
    l(1)=true;
    funcall(value(print_string),2,fp-2);
  }
  return ll;
}

WORD print_code_f(int na, WORD *fp)
{
  WORD ll,stream;

  if(na<1)parerr();
  checkstream(stream,value(standard_output));
  if(!(mode(stream)&OUTOPEN))ioerr(stream,fp);
  PUTS("C#",stream);
  PUTS((char *)stringcodes(prname(funcname(ll=ag(na-1)))),stream);
  return ll;
}

WORD print_reference_f(int na, WORD *fp)
{
  WORD ll,stream;
  char strbuf[STRBUFSIZE];

  if(na<1)parerr();
  checkstream(stream,value(standard_output));
  if(!(mode(stream)&OUTOPEN))ioerr(stream,fp);
  sprintf(strbuf,"R#%d",(int)(ll=ag(na-1)));
  PUTS(strbuf,stream);
  return ll;
}

WORD print_object_f(int na, WORD *fp)
{
  WORD slash=nil,ll;
  if(na==2){
    ll=ag(1);
    slash=ag(0);
  }
  else if(na==1)
    ll=ag(0);
  else parerr();
  switch(tag(ll)){
  case FIXNUM: /* fixnumの表示 */
    l(0)=ll;
    l(1)=slash;
    return funcall(value(print_fixnum),2,fp-2);
  case SYMBOL: /* シンボルを表示する */
    l(0)=ll;
    l(1)=slash;
    return funcall(value(print_symbol),2,fp-2);
  case CONS:
    l(0)=ll;
    l(1)=slash;
    return funcall(value(print_cons),2,fp-2);
  case OTHERS:
    switch(otag(ll)){
#ifndef NO_FLONUM
    case FLONUM:
      l(0)=ll;
      l(1)=slash;
      return funcall(value(print_flonum),2,fp-2);
#endif

#ifndef NO_BIGNUM
    case BIGNUM:
      l(0)=ll;
      l(1)=slash;
      return funcall(value(print_bignum),2,fp-2);
#endif
    case VECTOR:
      l(0)=ll;
      l(1)=slash;
      return funcall(value(print_vector),2,fp-2);
    case STRING:
      l(0)=ll;
      l(1)=slash;
      return funcall(value(print_string),2,fp-2);
    case STREAM:
      l(0)=ll;
      l(1)=slash;
      return funcall(value(print_stream),2,fp-2);
    case CODE:
      l(0)=ll;
      l(1)=slash;
      return funcall(value(print_code),2,fp-2);
    }
  default: /* コンパイラをだますためにここにする */
    l(0)=ll;
    l(1)=slash;
    return funcall(value(print_reference),2,fp-2);
  }	
}

#ifndef NO_FLONUM
/* 浮動小数点を出力する */

WORD print_flonum_f(int na, WORD *fp)
{
  int dig,expt;
  WORD a,ll,stream;
  double fval;
  char strbuf[STRBUFSIZE];

  if(na<1)parerr();
  ll=ag(na-1);
  checkstream(stream,value(standard_output));
  if(!(mode(stream)&OUTOPEN))ioerr(stream,fp);
  dig=fixtoi(slow_checkfix(a,value(digits)));
  fval=floval(ll);
  if(fval==0.0)expt=0;
  else if (fval<= -1.0 || fval>=1.0)
    for(expt=0;fval<=-1.0 || fval>=1.0;expt++)fval/=10.0;
  else {
    for(expt=0;fval>-1.0 && fval<1.0;expt--)fval*=10.0;
    expt++;fval/=10.0;
  }
  sprintf(strbuf,"%+*.*f^",dig+2,dig,fval);
  PUTS(strbuf,stream);
  if(expt>=0)PUTC('+',stream);
  else {PUTC('-',stream);expt= -expt;}
  sprintf(strbuf,"%02d",expt);
  PUTS(strbuf,stream);
  return (ll);
}
#endif /* NO_FLONUM */


#ifndef NO_BIGNUM
/* bignumを出力する */
WORD print_bignum_f(int na, WORD *fp)
{
  int size,i,len,j;
  WORD new,ll;
  char buf[10];

  if(na<1)parerr();
  ll=ag(na-1);
  size=fixtoi(osize(ll));
  if((int)bigcodes(ll,size-1)<0){
    PUTC('-',value(standard_output));
    l(0)=negbig2(ll,fp);
    if(tag(l(0))==FIXNUM){
      sprintf(buf,"%u",fixtoi(l(0)));
      for(j=0;buf[j];j++)PUTC(buf[j],value(standard_output));
      return ag(na-1);
    }
  }
  else
    l(0)=copybig(ll,fp);
  if(size==1){
    sprintf(buf,"%u",*(unsigned int *)(l(0)+BIGCODES));
    for(j=0;buf[j];j++)PUTC(buf[j],value(standard_output));
    return ag(na-1);
  }
  l(0)=extzbig(l(0),size,size+1,fp-1);
  new=albignum(size*6/5,fp-1);
  len=mconv(&bigcodes(l(0),0),size*4,&bigcodes(new,0));
  sprintf(buf,"%u",*(unsigned int *)(new+BIGCODES+len-4));
  for(j=0;buf[j];j++)PUTC(buf[j],value(standard_output));
  for(i=len-4;i>0;i-=4){
    sprintf(buf,"%09u",*(unsigned int *)(new+BIGCODES+i-4));
    for(j=0;j<9;j++)PUTC(buf[j],value(standard_output));
  }
  return ag(na-1);
}
#endif
    
/* コンスセルを表示する */

void prcons(WORD ll, WORD slash, int level, int length, WORD *fp)
{
  void prrest(WORD,WORD,int,int,WORD *);
  if(level>=fixtoi(value(printlevel)) && value(printlevel)!=itofix(0)){
    l(0)=question;
    l(1)=slash;
    print_object_f(2,fp-2);
    return;
  }
  PUTC('(',value(standard_output));
  prrest(ll,slash,level,length,fp);
}

void prrest(WORD ll,WORD slash,int level,int length,WORD *fp)
{
  if(length>=fixtoi(value(printlength)) && value(printlength)!=itofix(0)){
    l(0)=qqq;
    l(1)=slash;
    print_object_f(2,fp-2);
    PUTC(')',value(standard_output));
    return;
  }
  if(tag(car(ll))!=CONS){
    l(0)=car(ll);
    l(1)=slash;
    print_object_f(2,fp-2);
  }
  else prcons(car(ll),slash,level+1,1,fp);
  if(tag(cdr(ll))==CONS){
    PUTC(' ',value(standard_output));
    prrest(cdr(ll),slash,level,length+1,fp);
  }
  else if(cdr(ll)==nil)PUTC(')',value(standard_output));
  else{
    PUTS(" . ",value(standard_output));
    l(0)=cdr(ll);
    l(1)=slash;
    print_object_f(2,fp-2);
    PUTC(')',value(standard_output));
  }
}

WORD print_cons_f(int na, WORD *fp)
{
  WORD slash=nil,ll;
  if(na==2){
    ll=ag(1);
    slash=ag(0);
  }
  else if(na==1)
    ll=ag(0);
  else parerr();
  prcons(ll,slash,1,1,fp);
  return ag(na-1);
}


/* ストリングを表示する */

WORD print_string_f(int na, WORD *fp)
{
  WORD ll,stream,rtable;
  int flag=0,i,size;

  if(na==2){
    ll=ag(1);
    flag=(ag(0)==nil?0:1);
  }
  else if(na==1)
    ll=ag(0);
  else parerr();
  checkstream(stream,value(standard_output));
  if(!(mode(stream)&OUTOPEN))ioerr(stream,fp);
  checkvec(rtable,value(readtable));
  if(osize(rtable)!=itofix(256))nosymerr(rtable);
  if(flag)PUTC('\"',stream);
  size=fixtoi(osize(ll));
  for(i=0;i<size;i++){
    if(flag&&((int)vecelm(rtable,*(stringcodes(ll)+i))&STRQBIT))
      PUTC('\"',stream);
    PUTC(*(stringcodes(ll)+i),stream);
  }
  if(flag)PUTC('\"',stream);
  return ll;
}
