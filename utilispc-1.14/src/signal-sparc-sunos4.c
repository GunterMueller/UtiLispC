#include "constdef.h"
#include "defvar.h"

int oregs[8],gregs[8];
int getreg(struct sigcontext *scp, int reg)
{
  if(reg>15)
    return *(int *)(scp->sc_sp+4*(reg-16));
  else if(reg==8)
    return scp->sc_o0;
  else if(reg==1)
    return scp->sc_g1;
  else if(reg<8){
    return gregs[reg];
  }
  else
    return oregs[reg-8];
}

void sigsegv(int sig,int cod,struct sigcontext *scp,char *addr)
{
  WORD func;
  static char mes1[]="Stack overflow\n";

  if((int)addr==((int)UNBOUND+VALUE)){
    sigill();
  }
  else if((int)addr==PARERR){
    sigsetmask(0);
    parerrsub();
  }
}

#ifdef ALIGN_TYPE_CHECK
/* WORD alignmentのあってないアクセスをしたとき(型エラー) */
void c_sigbus(int sig,int cod,struct sigcontext *scp,char *addr)
{
  int i,j,reg;
  WORD func,val,pos;

  sigsetmask(0);
  reg=((*(int *)scp->sc_pc>>14)&31);
  typeerr((WORD)getreg(scp,reg),lstack);
}

#endif /*  ALIGN_TYPE_CHECK */

#ifdef TAGGED_ARITHMETIC

/* tagged arithmetic を使った計算で、型が違う時 */
void c_sigemt(int sig,int cod,struct sigcontext *scp,char *addr)
{
  WORD val;
  int reg;

  sigsetmask(0);
  reg=((*(int *)scp->sc_pc>>14)&31);
  if(tag(val=(WORD)getreg(scp,reg))!=FIXNUM)
    typeerr(val,lstack);
  reg=((*(int *)scp->sc_pc)&31);
  if(tag(val=(WORD)getreg(scp,reg))!=FIXNUM)
    typeerr(val,lstack);
}
#endif /* TAGGED_ARITHMETIC */

