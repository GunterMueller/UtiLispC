#include "constdef.h"
#include "defvar.h"

/* WORD alignment�Τ��äƤʤ����������򤷤��Ȥ�(�����顼) */
void sigbus(int sig,int cod,struct sigcontext *scp)
{
  int i,j,reg;
  WORD func,val,pos;

  sigsetmask(0);
  reg=((*(int *)scp->sc_pc>>21)&31);
  typeerr((WORD)scp->sc_regs[reg],lstack);
}
