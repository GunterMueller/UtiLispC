/* @(#)sysfnmis.c	1.6 90/07/30 */

#include "constdef.h"
#include "defvar.h"
#ifdef AP1000
#include "/usr/cap/sys.c7/include/ccell.c7.h"
#endif

WORD quit_f(int na,WORD *fp)
{
  exit(0);
}

WORD abend_f(int na, WORD *fp)
{
  exit(1);
}
WORD utilisp_f(int na, WORD *fp)
{
  bind(standard_input,value(terminal_input));
  bind(standard_output,value(terminal_output));
  bind(prompt,topprompt);
  for(;;){
    l(0)=value(terminal_output);
    flush_f(1,fp-1);
    l(0)=read_f(0,fp);
    l(0)=eval(l(0),fp-1);
    l(1)=true;
    value(question)=print_object_f(2,fp-2);
    terpri_f(0,fp);
  }
}
WORD break_f(int na, WORD *fp)
{
  jmp_buf env;
  WORD *tmpbstack=bstack,val;
  WORD *tmpcstack=cstack;
  struct lispenv *tmpestack=estack;
  
  if(na!=0)parerr();
  lstack=fp;
  if(_setjmp(env)){
    lstack=fp;
    eval_unwind(tmpestack,fp);
    estack=tmpestack;
    while(bstack<tmpbstack){
      val=popb();
      value(popb())=val;
    }
    cstack=tmpcstack;
    if(breakval!=0)goto breakent;
    breakval=1;
#if 0
    estack++;
#endif
    return nil;
  }
  else{
    pushe();
    estack->envtag=BREAKENV;
    estack->envobj=nil;
    estack->envbuf=(jmp_buf *)&env;
  }
breakent:
  bind(standard_input,value(terminal_input));
  bind(standard_output,value(terminal_output));
  bind(intern_v,value(intern_v));
  bind(prompt,value(prompt));
  bind(readtable,value(readtable));
  bind(macrotable,value(macrotable));
  value(prompt)=breakprompt;
  for(;;){
    l(0)=value(terminal_output);
    flush_f(1,fp-1);
    l(0)=read_f(0,fp-1);
    l(0)=eval(l(0),fp-1);
    l(1)=true;
    value(question)=print_object_f(2,fp-2);
    terpri_f(0,fp-1);
  }
}


void ret2break(WORD *fp)
{
  struct lispenv *envptr;

  for(envptr=estack;envptr<estackbottom;envptr++)
    if(envptr->envtag==BREAKENV || envptr->envtag==TOPENV){
      _longjmp(*(envptr->envbuf),1);
    }
  reterr(nil,fp);
}

WORD toplevel_f(int na, WORD *fp)
{
  struct lispenv *envptr;

  for(envptr=estack;envptr<estackbottom;envptr++)
    if(envptr->envtag==TOPENV){
      _longjmp(*(envptr->envbuf),1);
    }
  reterr(nil,fp);
}

WORD unbreak_f(int na, WORD *fp)
{
  breakval=0; /* to up */
  ret2break(fp);
}

WORD time_f(int na, WORD *fp)
         
#ifdef AP1000
{
  double dgettime(),dtime;
  WORD a;
  int time,utime,hz=60;

  dtime=dgettime();
  if(na==2){
    hz=fixtoi(slow_checkfix(a,ag(0)));
    fp++;na--;
  }
  time=(int)(dtime*(double)hz);
  if(na==1){
    utime=time;
    eval(ag(0),fp);
    dtime=dgettime();
    time=(int)(dtime*(double)hz);
    return itofix(time-utime);
  }
  else if(na==0)
    return itofix(time);
  else parerr();
}
#else
{
  struct tms buffer;
  time_t utime;
  
  times(&buffer);
  if(na==1){
    utime=buffer.tms_utime;
    eval(*fp,fp);
    times(&buffer);
    return itofix(buffer.tms_utime-utime);
  }
  else if(na==0)
    return itofix(buffer.tms_utime);
  else parerr();
}
#endif

WORD address_f(int na, WORD *fp)
{
  WORD a;
  int t;

  if(na!=1)parerr();
  if((t=tag(a=ag(0)))==FIXNUM)return a;
  else if(t==SYMBOL)return itofix((int)(a-SYMBOL));
  else if(t==CONS)return itofix((int)(a-CONS));
  else return itofix((int)(a-OTHERS));
}

WORD peek_f(int na, WORD *fp)
{
  WORD a;
  int len,i;
  char *addr;

  len=fixtoi(slow_checkfix(a,ag(0)));
  addr=(char *)fixtoi(slow_checkfix(a,ag(1)));
  if(addr<(char *)heaptop || addr>(char *)heapbottom)return nil;
  a=alstrspace(len,fp);
  for(i=0;i<len;i++)*(stringcodes(a)+i)= *(addr+i);
  return a;
}

WORD rand_f(int na, WORD *fp)
{
  if(na!=0) parerr();
  return itofix(rand());
}

WORD srand_f(int na, WORD *fp)
{
  WORD a;
  int seed;

  if(na!=1) parerr();
  seed=fixtoi(slow_checkfix(a,ag(0)));
  srand(seed);
  return itofix(seed);
}
