#include "constdef.h"
#include "defvar.h"

/* Cの文字列に対して適当なハッシュ関数を計算する */

int hashstr(char *str, int len)
{
  int hval=0,i;

  for(i=0;i<len;i++)
    hval+=(*str++)<<(i%6);
  return hval&(MAXFIX);
}

int hashall(WORD a)
{
  int t,i,len,val;

  if((t=tag(a))==CONS)
    return ((hashall(car(a))<<1)+hashall(cdr(a)))<<3;
  else if(t==FIXNUM)return (int)a;
  else if(t==SYMBOL)
    a=prname(a);
  if((t=otag(a))!=STRING){
    switch(t){
#ifndef NO_FLONUM
    case FLONUM:
      return (int)itofix((*(int *)(a+5)+*(int *)(a+9))&0x7ffffff);
#endif
#ifndef NO_BIGNUM
    case BIGNUM:
      len=fixtoi(osize(a));
      for(i=val=0;i<len;i++)val+=bigcodes(a,i);
      return (int)itofix(val&0x7ffffff);
#endif
    default: /* vec,stream,reference */
      return 0;
    }
  }
  return (int)itofix(hashstr(stringcodes(a),fixtoi(osize(a))));
}

WORD hash_f(int na, WORD *fp)
{
  if(na!=1)parerr();
  return (WORD)hashall(ag(0));
}
