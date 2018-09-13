/* @(#)alloc.c	1.7 90/07/30 */

#include <math.h>
#include "constdef.h"
#include "defvar.h"

WORD alcons(WORD a, WORD d, WORD *fp)
{
  WORD ptr;

  ptr=(char *)heap+CONS;
  heap+=CONSSIZE;
  if(heap>=heapbottom){
    l(0)=a;l(1)=d;
    gc(2,fp-2);
    a=l(0);d=l(1);
    ptr=(char *)heap+CONS;
    heap+=CONSSIZE;
    if(heap>=heapbottom){
      fputs("alcons\n",stderr);
      exit(1);
    }
  }
  car(ptr)=a;cdr(ptr)=d;
  return ptr;
}

WORD allist(int na, WORD *fp, WORD last)
{
  WORD ptr,*sp;
  int size=na*CONSSIZE;

  if(na==0)
    return last;
  ptr=(char *)heap+CONS;
  heap+=size;
  if(heap>=heapbottom){
    l(0)=last;
    gc(size,fp-1);
    last=l(0);
    ptr=(char *)heap+CONS;
    heap+=size;
    if(heap>=heapbottom){
      fputs("allist\n",stderr);
      exit(1);
    }
  }
  car(ptr)= *fp;
  cdr(ptr)=last;
  ptr+=CONSSIZE*sizeof(WORD);
  for(sp=fp+1;--na>0;ptr+=CONSSIZE*sizeof(WORD)){
    car(ptr)= *sp++;cdr(ptr)=ptr-CONSSIZE*sizeof(WORD);
  }
  return ptr-8;
}

WORD alsymbol(WORD pname, WORD *fp)
{
  WORD ptr;

  ptr=(WORD)heap+SYMBOL;
  heap+=SYMBOLSIZE;
  if(heap>=heapbottom){
    l(0)=pname;
    gc(4,fp-1);
    pname=l(0);
    ptr=(WORD)heap+SYMBOL;
    heap+=SYMBOLSIZE;
    if(heap>=heapbottom){
      fputs("alsymbol\n",stderr);
      exit(1);
    }
  }
  value(ptr)=UNBOUND;
  prname(ptr)=pname;
  plist(ptr)=nil;
  definition(ptr)=(WORD)uderr;
  return ptr;
}

/* pname:Cの文字列 */

WORD alsym(char *pname, int len, WORD *fp)
{
  return alsymbol(alnstring(pname,len,fp),fp);
}

#ifndef NOSYMROOT
void setroot(WORD sym, char *pname, int len, WORD *fp)
{
  WORD str;

  str=alnstring(pname,len,fp);
  value(sym)=UNBOUND;
  prname(sym)=str;
  plist(sym)=nil;
  definition(sym)=(WORD)uderr;
}
#endif

/* strbuf(Cの文字列)に対応するLISPの文字列型を作る */

WORD alstring(char *strbuf, WORD *fp)
{
  return alnstring(strbuf,strlen(strbuf),fp);
}

/* strbufから始まるn文字の文字列(NULL文字では終らない) */
/* に対応す文字列型を返す                              */

WORD alnstring(char *strbuf, int n, WORD *fp)
{
  int size=(n+4)/4+STRINGSIZE;
  WORD ptr;

  ptr=(char *)heap+OTHERS;
  heap+=size;
  if(heap>=heapbottom){
    gc(size,fp);
    ptr=(char *)heap+OTHERS;
    heap+=size;
    if(heap>=heapbottom){
      fputs("alstring\n",stderr);
      exit(1);
    }
  }
  otag(ptr)=STRING;	/* set tag */
  osize(ptr)=itofix(n);
  memcpy(stringcodes(ptr),strbuf,n);
  *(stringcodes(ptr)+n)=0;
  return ptr;
}

WORD alstrspace(int n, WORD *fp)
{
  int size=(n+4)/4+STRINGSIZE;
  WORD ptr;

  ptr=(char *)heap+OTHERS;
  heap+=size;
  if(heap>=heapbottom){
    gc(size,fp);
    ptr=(char *)heap+OTHERS;
    heap+=size;
    if(heap>=heapbottom){
      fputs("alstring\n",stderr);
      exit(1);
    }
  }
  otag(ptr)=STRING;	/* set tag */
  osize(ptr)=itofix(n);
  *(stringcodes(ptr)+n)=0;
  return ptr;
}

/* sizeの大きさの配列 */

WORD alvector(int size, WORD *fp)
{
  WORD ptr;
  int i;

  ptr=(char *)heap+OTHERS;
  heap+=size+VECSIZE;
  if(heap>=heapbottom){
    gc(size+VECSIZE,fp);
    ptr=(char *)heap+OTHERS;
    heap+=size+VECSIZE;
    if(heap>=heapbottom){
      fputs("alvector\n",stderr);
      exit(1);
    }
  }
  otag(ptr)=VECTOR;
  osize(ptr)=itofix(size);
  for(i=0;i<size;i++)
    vecelm(ptr,i)=nil;
  return ptr;
}

#ifndef NO_BIGNUM
/* sizeで示されるbignumの領域を確保する */
WORD albignum(int size, WORD *fp) /* caution this routine does not change heap */
         
{
  WORD ptr;

  ptr=(char *)heap+OTHERS;
  heap+=size+BIGSIZE;
  if(heap>=heapbottom){
    gc(size+BIGSIZE,fp);
    ptr=(char *)heap+OTHERS;
    heap+=size+BIGSIZE;
    if(heap>=heapbottom){
      fputs("albignum\n",stderr);
      exit(1);
    }
  }
  otag(ptr)=BIGNUM;
  osize(ptr)=itofix(size);
  return ptr;
}
#endif

WORD alstream(WORD name, WORD *fp)
{
  WORD ptr;

  ptr=(char *)heap+OTHERS;
  heap+=STREAMSIZE;
  if(heap>=heapbottom){
    l(0)=name;
    gc(STREAMSIZE,fp-1);
    name=l(0);
    ptr=(char *)heap+OTHERS;
    heap+=STREAMSIZE;
    if(heap>=heapbottom){
      fputs("alstream\n",stderr);
      exit(1);
    }
  }
  otag(ptr)=STREAM;
  osize(ptr)=itofix(0);
  filname(ptr)=name;
  fildes(ptr)= 0;
  mode(ptr)=NOTOPEN;
  return ptr;
}

/* sizeの大きさのコード領域を確保する(sizeはリテラル) */

WORD alcode(int size, WORD *fp)
{
  WORD ptr;
  int i;

  ptr=(char *)heap+OTHERS;
  heap+=size+CODESIZE;
  if(heap>=heapbottom){
    gc(size+CODESIZE,fp);
    ptr=(char *)heap+OTHERS;
    heap+=size+CODESIZE;
    if(heap>=heapbottom){
      fputs("alcode\n",stderr);
      exit(1);
    }
  }
  otag(ptr)=CODE;
  funcname(ptr)=nil;
  minarg(ptr)=maxarg(ptr)=itofix(0);
  for(i=0;i<size;i++)
    literal(ptr,i)=nil;
  osize(ptr)=itofix(size);
  return ptr;
}

#ifdef OBJECTSPECIAL
/* 特殊形式に対応する領域を確保する */

WORD alspec(WORD *fp)
{
  WORD ptr;

  ptr=(char *)heap+OTHERS;
  heap+=4;
  if(heap>=heapbottom){
    gc(4,fp);
    ptr=(char *)heap+OTHERS;
    heap+=4;
    if(heap>=heapbottom){
      fputs("alspec\n",stderr);
      exit(1);
    }
  }
  otag(ptr)=SPECIAL;
  return ptr;
}
#endif

#ifndef NO_FLONUM
/* fvalに対応する浮動小数点型を作る */

WORD alflonum(double fval, WORD *fp)
{
  WORD ptr;

  if(isnan(fval) || !finite(fval)) fpoferr(fp);
  ptr=(char *)heap+OTHERS;
  heap+=FLOSIZE;
  if(heap>=heapbottom){
    gc(FLOSIZE,fp);
    ptr=(char *)heap+OTHERS;
    heap+=FLOSIZE;
    if(heap>=heapbottom){
      fputs("alflonum\n",stderr);
      exit(1);
    }
  }
  otag(ptr)=FLONUM;
  store_floval(ptr,fval);
  return ptr;
}
#endif
