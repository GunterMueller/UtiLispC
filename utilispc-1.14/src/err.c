/* @(#)err.c	1.6 90/07/30 */

#include "constdef.h"
#include "defvar.h"

extern int bstacksize;

/* 未束縛のシンボルを評価したした時に生ずる */


extern int open (const char *, int, ...);

void ubverr(WORD a, WORD *fp)
{
  l(0)=a;
  l(1)= *cstack;
  funcall(value(err_unbound_variable),2,fp-2);
  ret2break(fp);
}


/* 型の違うリスプオブジェクトに対して操作をした時に生ずる */

void typeerr(WORD a, WORD *fp)
{
  l(0)=a;
  l(1)= *cstack;
  funcall(value(err_argument_type),2,fp-2);
  ret2break(fp);
}

void typeerr1(WORD a)
{
  typeerr(a,lstack);
}

/* 未定義の関数呼び出しを行なった時 */

WORD udferr(WORD a, WORD *fp)
{
  l(0)= *cstack;
  l(1)= a;
  return funcall(value(err_undefined_function),2,fp-2);
}

WORD udferr1(WORD a, WORD *fp)
{
  l(0)= a;
  l(1)= *cstack;
  return funcall(value(err_undefined_function),2,fp-2);
}

/* lambda関数の形がおかしい時 */

void fnerr(WORD a, WORD *fp)
{
  l(0)=a;
  l(1)= *cstack;
  funcall(value(err_function),2,fp-2);
  ret2break(fp);
}

void varerr(WORD a, WORD *fp)
{
  l(0)=a;
  l(1)= *cstack;
  funcall(value(err_variable),2,fp-2);
  ret2break(fp);
}

/* 引数チェック */

#ifdef INVALID_ACCESS_JUMP
void parerrsub()
#else
void parerr(void)
#endif
{
  WORD *fp=lstack;
  l(0)=nil;
  l(1)= *cstack;
  funcall(value(err_number_of_arguments),2,fp-2);
  ret2break(fp);
}

void inderr(WORD a, WORD *fp)
{
  l(0)=a;
  l(1)= *cstack;
  funcall(value(err_index),2,fp-2);
  ret2break(fp);
}

void readerr(WORD a, WORD *fp)
{
  l(0)=a;
  if(cstack<cstackbottom)
    l(1)= *cstack;
  else
    l(1)=nil;
  funcall(value(err_read),2,fp-2);
  ret2break(fp);
}

/* 入出力のエラー */

void ioerr(WORD a, WORD *fp)
{
  l(0)=a;
  if(cstack<cstackbottom)
    l(1)= *cstack;
  else
    l(1)=nil;
  funcall(value(err_io),2,fp-2);
  ret2break(fp);
}

void openerr(WORD a, WORD *fp)
{
  l(0)=a;
  if(cstack<cstackbottom)
    l(1)= *cstack;
  else
    l(1)=nil;
  funcall(value(err_open_close),2,fp-2);
  ret2break(fp);
}

void eoferr(WORD a, WORD *fp)
{
  char *ttyname(int);
  l(0)=a;
#if 0
  printf("enter eoferr\n");
#endif
#ifdef AP1000
  if(a!=value(terminal_input))
    close_f(1,fp-1);
#else
  close_f(1,fp-1);
  if(a==value(terminal_input)){
#if 0
    printf("terminal_input\n");
#endif
    if((fildes(a)=open(ttyname(2),0))<0){
      extern int open (const char *, int, ...);
      fprintf(stderr,"Can't reopen terminal-input\n");
      exit(1);
    }
    curpos(sistream)=recend(sistream)=0;
    mode(sistream)=INOPEN;
  }
#endif
  if(cstack<cstackbottom)
    l(1)= *cstack;
  else
    l(1)=nil;
  funcall(value(err_end_of_file),2,fp-2);
  ret2break(fp);
}

/* prog形式の外でreturn文を実行した時 */

void reterr(WORD a, WORD *fp)
{
  l(0)=a;
  l(1)= *cstack;
  funcall(value(err_return),2,fp-2);
  ret2break(fp);
}

void goerr(WORD a, WORD *fp)
{
  l(0)=a;
  l(1)= *cstack;
  funcall(value(err_go),2,fp-2);
  ret2break(fp);
}

/* throwに対応するcatchがないとき */

void ctcherr(WORD a, WORD *fp)
{
  l(0)=a;
  l(1)= *cstack;
  funcall(value(err_catch),2,fp-2);
  ret2break(fp);
}


void fpoferr(WORD *fp)
{
  l(0)=nil;
  l(1)= *cstack;
  funcall(value(err_floating_overflow),2,fp-2);
  ret2break(fp);
}

void bufferr(WORD *fp)
{
  l(0)=nil;
  l(1)= *cstack;
  funcall(value(err_buffer_overflow),2,fp-2);
  ret2break(fp);
}


void zerodiverr(WORD *fp)
{
  l(0)=nil;
  l(1)= *cstack;
  funcall(value(err_zero_division),2,fp-2);
  ret2break(fp);
}

#ifdef HAVE_SIGNAL

/* コントロールCを押した時にここで捕まえる */

void sigint()
{
#ifdef SIGSETMASK
  sigsetmask(0);
#else
  signal(SIGINT,sigint);
#endif
  funcall(value(attention_handler),0,lstack);
  ret2break(lstack);
}

#ifndef NO_FLOVAL

void sigfpe_ovf()
{
  WORD *fp;

#ifdef SIGSETMASK
  sigsetmask(0);
#endif
  fp=lstack;
  l(0)=nil;
  l(1)= *cstack;
  funcall(value(err_floating_overflow),2,fp-2);
}

void sigfpe_div()
{
  WORD *fp;

#ifdef SIGSETMASK
  sigsetmask(0);
#endif
  fp=lstack;
  l(0)=nil;
  funcall(value(err_zero_division),1,fp-1);
}

#endif /* NO_FLOVAL */

void sigill()
{
  char *mes2="bstack overflow\n";
  char *mes3="cstack overflow\n";
  char *mes4="estack overflow\n";
  char *mes5="@@@ Return to break loop\n";
  int flag=0;

#ifdef SIGSETMASK
  sigsetmask(0);
#else
  signal(SIGILL,sigill);
#endif
  if(bstack<bstacktop+0x10){
    flag++,write(1,mes2,strlen(mes2));
    bstack=bstacktop+0x10;
  }
  if(cstack<cstacktop+0x10){
    flag++,write(1,mes3,strlen(mes3));
    cstack=cstacktop+0x10;
  }
  if(estack<estacktop+0x10){
    flag++,write(1,mes4,strlen(mes4));
    estack=estacktop+0x10;
  }
  write(1,mes5,strlen(mes5));
  ret2break(lstack);
}
#endif /* HAVE_SIGNAL */

/* 束縛スタック上のデータをリストにして返す */

WORD oldvalue_f(int na, WORD *fp)
{
  WORD a,*from,*to;
  int level;

  if(na==1){
    level=fixtoi(slow_checkfix(a,ag(0)));
    if(level<0)nosymerr(a);
    if(level==0)level= -1;
  }
  else if(na==0)level= -1;
  else
    parerr();
  from=bstack;
  to=bstackbottom;
  for(l(0)=nil;level--!=0 && from<to;){
    if(*(to-2)==UNBOUND)
      l(0)=alcons(alcons(*(to-1),unbound_variable,fp-1),l(0),fp-1);
    else
      l(0)=alcons(alcons(*(to-1),*(to-2),fp-1),l(0),fp-1);
    to-=2;
  }
  return l(0);
}

/* 関数スタック上のデータをリストにして返す */

WORD backtrace_f(int na, WORD *fp)
{
  WORD *p,*looplimit,a;
  int len;

  looplimit=cstackbottom-1;
  if(na==1){
    len=fixtoi(slow_checkfix(a,ag(0)));
    if(cstack+len<looplimit)
      looplimit=cstack+len;
  }
  else if(na!=0)
    parerr();
  for(l(0)=nil,p=looplimit;p>cstack;p--){
    if(tag(*p)!=OTHERS || otag(*p)!=CODE)
      l(0)=alcons(*p,l(0),fp-1);
    else
      l(0)=alcons(funcname(*p),l(0),fp-1);
  }
  return l(0);
}

