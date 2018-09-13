/* @(#)sysfnio.c	1.6 90/07/30 */

#include <fcntl.h>
#include "constdef.h"
#include "defvar.h"
#include "read.h"

extern WORD dfltrdt_v,dfltmct_v;
extern int slash;

WORD read_f(int na, WORD *fp)
{
  WORD stream,val,rval;

  if(na==0)
    return read_l(fp,1);
  else if(na==1){
    checkstream(stream,ag(0));
    bind(standard_input,stream);
    rval=read_l(fp,1);
    val=popb();
    value(popb())=val;
    return rval;
  }
  else
    parerr();
}

WORD readquote_f(int na, WORD *fp)
{
  if(na!=0)
    parerr();
  return alcons(quote,alcons(read_f(na,fp),nil,fp),fp);
}


WORD printsub(int na, int flag, WORD *fp)
{
  WORD stream,val,rval;

  if(na==2){
    checkstream(stream,ag(0));
    bind(standard_output,stream);
    l(0)=ag(1);
    l(1)=true;
    rval=print_object_f(2,fp-2);
    if(flag)terpri_f(0,fp-2);
    FLUSH(stream);
    val=popb();
    value(popb())=val;
    return rval;
  }
  else if(na==1){
    l(0)=ag(0);
    l(1)=true;
    rval=print_object_f(2,fp-2);
    if(flag)terpri_f(0,fp-2);
    FLUSH(value(standard_output));
    return rval;
  }
  else parerr();
}
WORD prin1_f(int na, WORD *fp)
{
  return printsub(na,0,fp);
}

WORD print_f(int na, WORD *fp)
{
  return printsub(na,1,fp);
}

WORD princ_f(int na, WORD *fp)
{
  WORD stream,val,rval;

  if(na==2){
    checkstream(stream,ag(0));
    bind(standard_output,stream);
    l(0)=ag(1);
    l(1)=nil;
    rval=print_object_f(2,fp-2);
    FLUSH(stream);
    val=popb();
    value(popb())=val;
    return rval;
  }
  else if(na==1){
    l(0)=ag(0);
    l(1)=nil;
    rval=print_object_f(2,fp-2);
    FLUSH(value(standard_output));
    return rval;
  }
  parerr();
  return nil;
}

WORD terpri_f(int na, WORD *fp)
{
  WORD stream;

  if(na==1)
    checkstream(stream,ag(0));
  else if(na==0)
    checkstream(stream,value(standard_output));
  else
    parerr();
  PUTC('\n',stream);
  FLUSH(stream);
  return nil;
}

WORD flush_f(int na, WORD *fp)
{
  WORD stream;

  if(na==1)
    checkstream(stream,ag(0));
  else if(na==0)
    checkstream(stream,value(standard_output));
  else
    parerr();
  FLUSH(stream);
  return nil;
}

WORD cursor_f(int na, WORD *fp)
{
  WORD stream;

  if(na==1)
    checkstream(stream,ag(0));
  else if(na==0)
    checkstream(stream,value(standard_output));
  else
    parerr();
  return itofix(curpos(stream));
}

WORD string_stream_index_f(int na, WORD *fp)
{
  WORD stream;
  int smode;

  if(na!=1)parerr();
  checkstream(stream,ag(0));
  if((smode=mode(stream))&STRINGSTREAM){
    if(smode&OUTOPEN)
      return itofix(fildes(stream)+curpos(stream)-recend(stream));
    else if(smode&INOPEN)
      return itofix(fildes(stream)+curpos(stream));
  }
  nofixerr(stream);
}

WORD string_stream_limit_f(int na, WORD *fp)
{
  WORD stream;

  if(na!=1)parerr();
  checkstream(stream,ag(0));
  if(mode(stream)&STRINGSTREAM)
    return osize(filname(stream));
  nofixerr(stream);
}

WORD colleft_f(int na, WORD *fp)
{
  WORD stream;

  if(na==1)
    checkstream(stream,ag(0));
  else if(na==0)
    checkstream(stream,value(standard_output));
  else
    parerr();
  return itofix(80-curpos(stream));
}

WORD tab_f(int na, WORD *fp)
{
  WORD a,stream;
  int col;

  if(na==2)
    checkstream(stream,pop());
  else if(na==1)
    checkstream(stream,value(standard_output));
  else
    parerr();
  col=fixtoi(slow_checkfix(a,*fp));
  if(curpos(stream)<=col){
    while(curpos(stream)<col)PUTC(' ',stream);
  }
  else {
    PUTC('\n',stream);
    while(curpos(stream)<col)PUTC(' ',stream);
  }
  return nil;
}

WORD linelength_f(int na, WORD *fp)
{
  return itofix(80);
}

WORD intern_f(int na, WORD *fp)
{
  WORD obv,str,sym,val,rval;
  int t;

  if(na==2){
    l(0)=checkvec(obv,ag(0));
    if((t=tag(str=ag(1)))==SYMBOL)sym=str;
    else if(t==OTHERS && otag(str)==STRING)
      sym=alsymbol(str,fp-1);
    else nosymerr(str);
  }
  else if(na==1){
    l(0)=checkvec(obv,value(obvector));
    if((t=tag(str=ag(0)))==SYMBOL)sym=str;
    else if(t==OTHERS && otag(str)==STRING)
      sym=alsymbol(str,fp-1);
    else nosymerr(str);
  }
  else parerr();
  pushb(obvector);
  pushb(value(obvector));
  value(obvector)=l(0);
  rval=internsym(sym,fp);
  val=popb();
  value(popb())=val;
  return rval;
}

WORD intern_soft_f(int na, WORD *fp)
{
  WORD obv,str,val,rval;

  if(na==2){
    checkvec(obv,ag(0));
    checkstr(str,ag(1));
  }
  else if(na==1){
    checkvec(obv,value(obvector));
    checkstr(str,*fp);
  }
  else parerr();
  pushb(obvector);
  pushb(value(obvector));
  value(obvector)=obv;
  rval=intern_soft(stringcodes(str),fixtoi(osize(str)),fp);
  val=popb();
  value(popb())=val;
  return rval;
}

WORD tyi_f(int na, WORD *fp)
{
  int c;
  WORD stream;

  if(na==1)
    checkstream(stream,ag(0));
  else if(na==0)
    checkstream(stream,value(standard_input));
  else parerr();
  c=(unsigned int)GETCERR(stream,fp);
  return itofix(c);
}

WORD tyipeek_f(int na, WORD *fp)
{
  int c;
  WORD stream;

  if(na==1)
    checkstream(stream,ag(0));
  else if(na==0)
    checkstream(stream,value(standard_input));
  else parerr();
  c=(unsigned int)PEEKCERR(stream,fp);
  return itofix(c);
}

WORD readch_f(int na, WORD *fp)
{
  int c;
  char buf[2];
  WORD stream;

  if(na==1)
    checkstream(stream,ag(0));
  else if(na==0)
    checkstream(stream,value(standard_input));
  else parerr();
  c=(unsigned int)GETCERR(stream,fp);
  buf[0]=c;buf[1]=0;
  return intern(buf,1,fp);
}

WORD tyo_f(int na, WORD *fp)
{
  WORD stream,a;
  int c;

  if(na==2){
    checkstream(stream,ag(0));
    c=fixtoi(checkchar(a,ag(1)));
  }
  else if(na==1){
    checkstream(stream,value(standard_output));
    c=fixtoi(checkchar(a,ag(0)));
  }
  else parerr();
  PUTC(c,stream);
  return a;
}

WORD stream_f(int na, WORD *fp)
{
  WORD a,ptr;

  if(na!=1)parerr();
  if(tag(a=ag(0))==FIXNUM) ptr=alstream(a,fp);
  else ptr=alstream(checkstr(a,a),fp);
  mode(ptr)=0;
  return ptr;
}

WORD string_stream_f(int na, WORD *fp)
{
  WORD a,ptr;

  if(na!=1)parerr();
  ptr=alstream(checkstr(a,ag(0)),fp);
  mode(ptr)=STRINGSTREAM;
  return ptr;
}

WORD inopen_f(int na, WORD *fp)
{
  WORD a,b;
  int fd;

  if(na!=1)parerr();
  checkstream(a,ag(0));
  if(mode(a)&(INOPEN|OUTOPEN))openerr(a,fp);
  if(mode(a)&STRINGSTREAM){
    fildes(a)=0;
  }
  else if (tag(b=filname(a))==FIXNUM)
    fildes(a)=fixtoi(b);
  else {
    if((fd=open(stringcodes(b),O_RDONLY))<0){
      openerr(a,fp);
    }
    fildes(a)=fd;
  }
  l(0)=a;
  value(openfiles)=alcons(a,value(openfiles),fp-1);
  a=l(0);
  recend(a)=curpos(a)=0;
  mode(a)|=INOPEN;
  return a;
}

WORD appendopen_f(int na, WORD *fp)
{
  WORD a,b;
  int fd;

  if(na!=1)parerr();
  checkstream(a,ag(0));
  if(mode(a)!=0)openerr(a,fp);
  if(mode(a)&STRINGSTREAM)
    openerr(a,fp);
  else if (tag(b=filname(a))==FIXNUM)
    fildes(a)=fixtoi(b);
  else {
    if((fd=open(stringcodes(b),O_WRONLY))<0)
      openerr(a,fp);
    lseek(fd,0,2);
    fildes(a)=fd;
  }
  l(0)=a;
  value(openfiles)=alcons(a,value(openfiles),fp-1);
  a=l(0);
  recend(a)=curpos(a)=0;
  mode(a)=INOPEN|OUTOPEN;
  return a;
}

WORD outopen_f(int na, WORD *fp)
{
  WORD a,b;
  int fd;

  if(na!=1)parerr();
  checkstream(a,ag(0));
  if(mode(a)&(INOPEN|OUTOPEN))openerr(a,fp);
  if(mode(a)&STRINGSTREAM){
    fildes(a)=0;
  }
  else if (tag(b=filname(a))==FIXNUM)
    fildes(a)=fixtoi(b);
  else {
    if((fd=open(stringcodes(b),O_TRUNC|O_WRONLY|O_CREAT,0666))<0){
      openerr(a,fp);
    }
    fildes(a)=fd;
  }
  l(0)=a;
  value(openfiles)=alcons(a,value(openfiles),fp-1);
  a=l(0);
  recend(a)=curpos(a)=0;
  mode(a)|=OUTOPEN;
  return a;
}

WORD close_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  checkstream(a,ag(0));
  if(!mode(a))openerr(a,fp);
  if(mode(a)&OUTOPEN)FLUSH(a);
  if(mode(a)&STRINGSTREAM){
    mode(a)=STRINGSTREAM;
    l(0)=a;
    l(1)=value(openfiles);
    value(openfiles)=delq_f(2,fp-2);
  }
  else{
    mode(a)=0;
    close(fildes(a));
    l(0)=a;
    l(1)=value(openfiles);
    value(openfiles)=delq_f(2,fp-2);
  }
  return a;
}

WORD stream_mode_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  checkstream(a,ag(0));
  if((mode(a)&(OUTOPEN|INOPEN))==(OUTOPEN|INOPEN))return appendopen;
  else if(mode(a)&INOPEN)return inopen;
  else if(mode(a)&OUTOPEN)return outopen;
  return nil;
}

WORD alloc_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return checkstr(a,ag(0));
}

WORD readmacro_f(int na, WORD *fp)
{
  WORD fn,c;
  int ch;

  if(na!=2)parerr();
  fn=ag(0);
  ch=fixtoi(checkchar(c,ag(1)));
  *(int *)(value(readtable)+VECELM+ch*4)|=MACROCHBIT;
  vecelm(value(macrotable),ch)=fn;
  return nil;
}

#define RBUFSIZE 8192

WORD readline_f(int na, WORD *fp)
{
  char buf[RBUFSIZE];
  WORD stream;
  int i;

  if(na==1)checkstream(stream,ag(0));
  else if(na==0)checkstream(stream,value(standard_input));
  else parerr();
  for(i=0;i<RBUFSIZE;i++)
    if((buf[i]=GETCERR(stream,fp))=='\n')break;
  buf[i]=0;
  return alstring(buf,fp);
}

WORD current_line_f(int na, WORD *fp)
{
  WORD stream;
  int i;
  char buf[RBUFSIZE];

  if(na==1)checkstream(stream,*fp);
  else if(na==0)checkstream(stream,value(standard_input));
  else parerr();
  for(i=0;i<RBUFSIZE;i++)
    if((buf[i]= *(buftop(stream)+i))=='\n')break;
  buf[i]=0;
  return alstring(buf,fp);
}
WORD skipline_f(int na, WORD *fp)
{
  WORD stream;
  int i;

  if(na==1)checkstream(stream,*fp);
  else if(na==0)checkstream(stream,value(standard_input));
  else parerr();
  for(i=0;i<RBUFSIZE;i++)
    if(GETCERR(stream,fp)=='\n')break;
  return nil;
}

WORD single_character_f(int na, WORD *fp)
{
  WORD ch,rtable;
  unsigned int c;

  if(na==2)
    checkvec(rtable,pop());
  else if(na==1)
    checkvec(rtable,value(readtable));
  else parerr();
  c=(unsigned int)fixtoi(checkchar(ch,*fp));
  vecelm(rtable,c)=(WORD)((int)vecelm(rtable,c)|(0x30300<<2));
  return nil;
}

int sym_slashlen(WORD a)
{
  WORD rtable;
  int ret,size,i;

  checkvec(rtable,value(readtable));
  if(osize(rtable)!=itofix(256))nosymerr(rtable);
  ret=size=fixtoi(osize(a));
  if(size==0) return 0;
  if((int)vecelm(rtable,*(stringcodes(a)))&SLASHTOPBIT)
    ret++;
  for(i=1;i<size;i++)
    if((int)vecelm(rtable,*(stringcodes(a)+i))&SLASHBIT)
      ret++;
  return ret;
}

int str_slashlen(WORD a)
{
  WORD rtable;
  int ret,size,i;

  checkvec(rtable,value(readtable));
  if(osize(rtable)!=itofix(256))nosymerr(rtable);
  ret=size=fixtoi(osize(a))+2;
  for(i=0;i<size;i++)
    if((int)vecelm(rtable,*(stringcodes(a)+i))&STRQBIT)
      ret++;
  return ret;
}

#ifndef NO_BIGNUM
int big_slashlen(WORD a, WORD *fp)
{
  int size,sign=0,len;
  unsigned char buf[10];
  WORD new;

  size=fixtoi(osize(a));
  if((int)bigcodes(a,size-1)<0){
    sign++;
    l(0)=negbig2(a,fp);
  }
  else
    l(0)=copybig(a,fp);
  if(size==1){
    sprintf(buf,"%u",*(unsigned int *)(l(0)+BIGCODES));
    return strlen(buf)+sign;
  }
  l(0)=extzbig(l(0),size,size+1,fp-1);
  new=albignum(size*6/5,fp-1);
  len=mconv(&bigcodes(l(0),0),size*4,&bigcodes(new,0));
  sprintf(buf,"%u",*(unsigned int *)(new+BIGCODES+len-4));
  return strlen(buf)+(len-4)/4*9+sign;
}
#endif

WORD atomlength_f(int na, WORD *fp)
{
  WORD a;
  int ival,len;

  if(na!=1)parerr();
  if(tag(a= *fp)==CONS)nosymerr(a);
  else if(tag(a)==SYMBOL)return itofix(sym_slashlen(prname(a)));
  else if(tag(a)==FIXNUM){
    len=0;
    ival=fixtoi(a);
    if(ival<0){len++;ival= -ival;}
    for(;ival>0;len++)ival/=10;
    if(len==0)len=1;
    return itofix(len);
  }
  else if(otag(a)==STRING)return itofix(str_slashlen(a));
#ifndef NO_FLONUM
  else if(otag(a)==FLONUM)return itofix(fixtoi(value(digits))+7);
#endif
#ifndef NO_BIGNUM
  else if(otag(a)==BIGNUM)return itofix(big_slashlen(a,fp));
#endif
  else return itofix(8);
}

WORD charleft_f(int na, WORD *fp)
{
  WORD stream;

  if(na==1)
    checkstream(stream,ag(0));
  else if(na==0)
    checkstream(stream,value(standard_output));
  else
    parerr();
  return itofix(recend(stream)-curpos(stream));
}
