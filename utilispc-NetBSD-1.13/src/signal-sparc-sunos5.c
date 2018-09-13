#include "constdef.h"
#include "defvar.h"

#ifdef HAVE_SYS_REG_H
#include <sys/reg.h>
#endif

#ifdef HAVE_UCONTEXT_H
#include <ucontext.h>
#endif

int getreg(ucontext_t *uap,int regno){
  int *sp=(int *)uap->uc_mcontext.gregs[SP];
  if(16 <= regno && regno<=31){
    return sp[regno-16];
  } 
  else if(regno==0){ /* g0 always return 0 */
    return 0; 
  }
  else {
    return uap->uc_mcontext.gregs[regno-1+G1];
  }
}

int getaddr(ucontext_t *uap){
  int pcreg=uap->uc_mcontext.gregs[PC];
  int instruction= *(int *)pcreg;
  int reg0=(instruction>>14)&31;

  if(instruction&0x2000){ /* reg+imm13 */
    int offset= (instruction & 0x1fff);
    if(offset&0x1000) offset-=0x2000;
    return getreg(uap,reg0)+offset;
  }
  else{
    int reg1=instruction&31;
    return getreg(uap,reg0)+getreg(uap,reg1);
  }
}

#ifdef INVALID_ACCESS_JUMP
void sigsegv(int sig, siginfo_t *siginfo, ucontext_t *uap){
  int addr=getaddr(uap);
  signal(SIGSEGV,sigsegv);
  if(addr==PARERR){
    parerrsub();
  }
  sigill();
}
#endif /* INVALID_ACCESS_JUMP */

#ifdef ALIGN_TYPE_CHECK

/* WORD alignmentのあってないアクセスをしたとき(型エラー) */
void sigbus(int sig, siginfo_t *siginfo, ucontext_t *uap){
  int pcreg=uap->uc_mcontext.gregs[PC];
  int reg;

  signal(SIGBUS,sigbus);
  reg=((*(int *)pcreg>>14)&31);
  typeerr((WORD)getreg(uap,reg),lstack);
}
#endif /*  ALIGN_TYPE_CHECK */

#ifdef TAGGED_ARITHMETIC

/* tagged arithmetic を使った計算で、型が違う時 */
void sigemt(int sig, siginfo_t *siginfo, ucontext_t *uap){
  int pcreg=uap->uc_mcontext.gregs[PC];
  int reg;
  WORD val;

  signal(SIGEMT,sigemt);
  reg=((*(int *)pcreg>>14)&31);
  if(tag(val=(WORD)getreg(uap,reg))!=FIXNUM)
    typeerr(val,lstack);
  reg=(*(int *)pcreg&31);
  if(tag(val=(WORD)getreg(uap,reg))!=FIXNUM)
    typeerr(val,lstack);
}
#endif /* TAGGED_ARITHMETIC */
