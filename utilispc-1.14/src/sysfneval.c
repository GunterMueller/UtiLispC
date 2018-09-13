#include "constdef.h"
#include "defvar.h"

WORD eval_f(int na, WORD *fp)
{
  if(na!=1)parerr();
  return eval(ag(0),fp+1);
}

WORD apply_f(int na, WORD *fp)
{
  if(na!=2)parerr();
  return apply(ag(1),ag(0),fp+2);
}

WORD funcall_f(int na, WORD *fp)
{
  if(na<1)parerr();
  return funcall(ag(na-1),na-1,fp);
}



