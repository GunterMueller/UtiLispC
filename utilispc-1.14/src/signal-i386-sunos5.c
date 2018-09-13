#include "constdef.h"

#ifdef HAVE_SYS_REG_H
#include <sys/reg.h>
#endif

#ifdef HAVE_UCONTEXT_H
#include <ucontext.h>
#endif

#ifdef INVALID_ACCESS_JUMP
void sigsegv(int sig, siginfo_t *siginfo, ucontext_t *uap){
  int pcreg=uap->uc_mcontext.gregs[PC];
  signal(SIGSEGV,(void (*))sigsegv);
  if(*(int *)pcreg==0xfff805c7){ /* mov $0,-8 */
    parerrsub();
  }
  sigill();
}
#endif /* INVALID_ACCESS_JUMP */
