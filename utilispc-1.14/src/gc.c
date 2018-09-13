/* @(#)gc.c	1.7 90/07/30 */

#include "constdef.h"
#include "defvar.h"

static int gccount=0,gctime=0;
WORD *gheaptop,*gheapbottom;
#if defined(__GNUC__) && defined(SPARC)
register WORD *gheap asm ("g5");
#else
WORD *gheap;
#endif
WORD *marktop;
extern int heapsize,lstacksize;
extern int allheapsize;
extern int codeareasize;

/* copying gc */

WORD copyobj(WORD a, int t, int *lgheaptop, int lgheapsize)
{
  int i,j,len;
  WORD b,c;

  if(t==SYMBOL){
    b=value(a);
    if((unsigned int )b-(unsigned int)lgheaptop<lgheapsize && tag(b)==SYMBOL)
      return b;
    c=(WORD)gheap+SYMBOL;
    gheap+=SYMBOLSIZE;
    value(c)=a;
    prname(c)=b;
    value(a)=c;
    return c;
  }
  else if(t==CONS){
    b=car(a);
    if((unsigned int)b-(unsigned int)lgheaptop<lgheapsize && tag(b)==CONS)return b;
    c=(WORD)gheap+CONS;
    gheap+=CONSSIZE;
    car(c)=a;
    cdr(c)=b;
    car(a)=c;
    return c;
  }
  c=(WORD)gheap+OTHERS;
  switch(t=otag(a)){
#ifndef NO_FLONUM
  case FLONUM:
    gheap+=FLOSIZE;
    store_floval(c,floval(a));
    break;
#endif

#ifdef OBJECTSPECIAL
  case SPECIAL:
    gheap+=SPECSIZE;
    spectop(c)=spectop(a);
    break;
#endif
  case CODE:
    len=fixtoi(osize(a));
    gheap+=CODESIZE+len;
    osize(c)=itofix(len);
    funcname(c)=funcname(a);
    codetop(c)=codetop(a);
    minarg(c)=minarg(a);
    maxarg(c)=maxarg(a);
    for(i=0;i<len;i++)
      literal(c,i)= literal(a,i);
    break;
  case STREAM:
    gheap+=STREAMSIZE;
    memcpy(c+BLOCKSIZE,a+BLOCKSIZE,(STREAMSIZE-1)*sizeof(WORD));
    break;
#ifndef NO_BIGNUM
  case BIGNUM:
#endif
  case VECTOR:
    len=fixtoi(osize(a));
    gheap+=VECSIZE+len;
    osize(c)=itofix(len);
    for(i=0;i<len;i++)
      vecelm(c,i)=vecelm(a,i);
    break;
  case STRING:
    len=fixtoi(osize(a));
    gheap+=STRINGSIZE+(len+4)/4;
    osize(c)=itofix(len);
    memcpy(stringcodes(c),stringcodes(a),len+1);
    break;
  default:
    if((unsigned int)t-(unsigned int)lgheaptop<lgheapsize && tag(t)==OTHERS)
	return (WORD)t;
    for(i=0;tag3(t=otag(a))!=OTAGS;i++,a-=4)
      if((unsigned int)t-(unsigned int)lgheaptop<lgheapsize && tag((WORD)t)==OTHERS)
	return reference((WORD)t,i-VECSIZE);
    if(t!=VECTOR){fputs("gc error in reference\n",stderr);abort();}
    len=fixtoi(osize(a));
    gheap+=len+VECSIZE;
    osize(c)=itofix(len);
    for(j=0;j<len;j++)
      vecelm(c,j)= vecelm(a,j);
    otag((WORD)a)=(int)c;
    otag(c)=t;
    return reference((WORD)c,i-VECSIZE);
  }
  otag((WORD)a)=(int)c;
  otag(c)=t;
  return c;
}

static inline WORD Copyobj(WORD a, int *lheaptop, unsigned int lheapsize, int *lgheaptop, unsigned int lgheapsize)
{
  int t;
  if((t=tag(a))==FIXNUM || lheapsize<=(unsigned int)a-(unsigned int)lheaptop)
    return (WORD)a;
  return copyobj(a,t,lgheaptop,lgheapsize);
}

void sweep(void)
{
  int *sweepptr,t,i,len;
  int *lheaptop=(int *)heaptop;
  unsigned int lheapsize=(unsigned int)heapbottom-(unsigned int)lheaptop;
  int *lgheaptop=(int *)gheaptop;
  unsigned int lgheapsize=(unsigned int)gheapbottom-(unsigned int)lgheaptop;
  WORD a;

  for(sweepptr=(int *)gheaptop;sweepptr<(int *)gheap;){
    if((t=tag(a= *(WORD *)sweepptr))==CONS){
      *(WORD *)sweepptr=Copyobj(*(WORD *)(sweepptr+1),lheaptop,lheapsize,lgheaptop,lgheapsize);
      *(WORD *)(sweepptr+1)=Copyobj(cdr(a),lheaptop,lheapsize,lgheaptop,lgheapsize);
      sweepptr+=CONSSIZE;
      continue;
    }
    else if(t==SYMBOL){
      *(WORD *)sweepptr=Copyobj(*(WORD *)(sweepptr+1),lheaptop,lheapsize,lgheaptop,lgheapsize);
      *(WORD *)(sweepptr+1)=Copyobj(prname(a),lheaptop,lheapsize,lgheaptop,lgheapsize);
      *(WORD *)(sweepptr+2)=Copyobj(plist(a),lheaptop,lheapsize,lgheaptop,lgheapsize);
      *(WORD *)(sweepptr+3)=Copyobj(definition(a),lheaptop,lheapsize,lgheaptop,lgheapsize);
      sweepptr+=SYMBOLSIZE;
      continue;
    }
    switch((int)(a)){
#ifndef NO_FLONUM
    case FLONUM:
      sweepptr+=FLOSIZE;
      break;
#endif
#ifndef NO_BIGNUM
    case BIGNUM:
      sweepptr+=BIGSIZE+fixtoi(*(sweepptr+1));
      break;
#endif
    case STRING:
      sweepptr+=STRINGSIZE+(fixtoi(*(sweepptr+1))+4)/4;
      break;
#ifdef OBJECTSPECIAL
    case SPECIAL:
      sweepptr+=SPECSIZE;
      break;
#endif
    case VECTOR:
      len= fixtoi(*(WORD *)(sweepptr+1));
      for(i=VECSIZE;i<VECSIZE+len;i++)
	*(sweepptr+i)=(int)Copyobj((WORD)(*(sweepptr+i)),lheaptop,lheapsize,lgheaptop,lgheapsize);
      sweepptr+=VECSIZE+len;
      break;
    case STREAM:
      *(sweepptr+(FILNAME+OTHERS)/4)=
	(int)Copyobj((WORD)(*(sweepptr+(FILNAME+OTHERS)/4)),lheaptop,lheapsize,lgheaptop,lgheapsize);
      sweepptr+=STREAMSIZE;
      break;
    case CODE:
      len=fixtoi(*(WORD *)(sweepptr+1));
      *(sweepptr+(FUNCNAME+OTHERS)/4)=
	(int)Copyobj((WORD)(*(sweepptr+(FUNCNAME+OTHERS)/4)),lheaptop,lheapsize,lgheaptop,lgheapsize);
      for(i=0;i<len;i++)
	*(sweepptr+(LITERAL+OTHERS)/4+i)=
	  (int)Copyobj((WORD)(*(sweepptr+(LITERAL+OTHERS)/4+i)),lheaptop,lheapsize,lgheaptop,lgheapsize);
      sweepptr+=CODESIZE+len;
      break;
    default:
      fprintf(stderr,"Internal gc error\n");exit(1);
    }
  }
}
WORD copy_gc(int size, WORD *fp)
{
  struct tms buffer;
  time_t utime;
  int *ptr,i;
  struct lispenv *envptr;
  int *lheaptop;
  unsigned int lheapsize;
  int *lgheaptop;
  unsigned int lgheapsize;
  void (*oldsigint)();
  WORD retval;

  times(&buffer);
  utime=buffer.tms_utime;
  oldsigint=signal(SIGINT,SIG_IGN);
  if(heaptop==allheaptop)
    gheap=gheaptop=allheaptop+heapsize/sizeof(int);
  else gheap=gheaptop=allheaptop;
  gheapbottom=gheaptop+heapsize/sizeof(int);
  lheaptop=(int *)heaptop;
  lheapsize=(unsigned int)heapbottom-(unsigned int)lheaptop;
  lgheaptop=(int *)gheaptop;
  lgheapsize=(unsigned int)gheapbottom-(unsigned int)lgheaptop;
  for(i=0;i<sizeof(root)/sizeof(WORD);i++)
    root[i]=(WORD)Copyobj(root[i],lheaptop,lheapsize,lgheaptop,lgheapsize);
#ifndef NOSYMROOT
  for(i=0;i<SYMROOTSIZE;i++)
    symroot[i]=(WORD)Copyobj(symroot[i],lheaptop,lheapsize,lgheaptop,lgheapsize);
#endif
  for(ptr=(int *)bstack;ptr<(int *)bstackbottom;ptr++)
    *ptr=(int)Copyobj((WORD)(*ptr),lheaptop,lheapsize,lgheaptop,lgheapsize);
  for(ptr=(int *)cstack;ptr<(int *)cstackbottom;ptr++)
    *ptr=(int)Copyobj((WORD)(*ptr),lheaptop,lheapsize,lgheaptop,lgheapsize);
  for(envptr=estack;envptr<estackbottom;envptr++)
    envptr->envobj=(WORD)Copyobj(envptr->envobj,lheaptop,lheapsize,lgheaptop,lgheapsize);
  for(ptr= (int *)lstackbottom-1;ptr>=(int *)fp;ptr--)
    *ptr=(int)Copyobj((WORD)(*ptr),lheaptop,lheapsize,lgheaptop,lgheapsize);
  sweep();
  heaptop=gheaptop;heapbottom=gheapbottom;heap=gheap;
  signal(SIGINT,oldsigint);
  gccount++;
  times(&buffer);
  gctime+=buffer.tms_utime-utime;
  if(value(gc_hook)!=UNBOUND){
    pushb(gc_hook);
    pushb(retval=value(gc_hook));
    value(gc_hook)=UNBOUND;
    l(0)=itofix(size);
    retval=funcall(retval,1,fp-1);
    value(gc_hook)=popb();
    discardb();
    return retval;
  }
  return nil;
}

/* mark and sweep gc */

int garbage_top[3];
/* 0 : GARBAGE_LINK */
/* 1 : Pointer to next garbage link */
/* 2 : Pointer to next live cell */
#define GARBAGE_LINK 68
/* 0: GARBAGE_LINK2 */
/* 1 : pointer to next garbage link */
/* 2 : --- live cell */
#define GARBAGE_LINK2 76
#define GARBAGE_1 84

static inline int GC_MARKED(WORD a, int *heaptop, int *marktop)
{
  int word_offset=((int)a>>7);
  int bit_offset=((int)a>>2)&31;
  return marktop[word_offset]&(1<<bit_offset);
}

static inline int MARK_WORD(WORD a, int *heaptop, int *marktop)
{
  int word_offset=((int)a>>7);
  return marktop[word_offset];
}

static inline int GC_MARKED_WORD(int a, int mark_word, int *heaptop)
{
  return (mark_word>>(((int)a>>2)&31)) & 1;
}

static inline void SET_GCMARK(WORD a, int *heaptop, int *marktop)
{
  int word_offset=((int)a>>7);
  int bit_offset=((int)a>>2)&31;
  marktop[word_offset]|=(1<<bit_offset);
}

static inline void SET_MARK_WORD(WORD a, int val, int *heaptop, int *marktop)
{
  int word_offset=((int)a>>7);
  marktop[word_offset]=val;
}

void markobj(WORD a, WORD *fp)
{
  int t,i,len;
  WORD b,c,d,*sp=fp;
  int *lheaptop=(int *)heaptop;
  int *lmarktop=(int *)((int)marktop-(((int)lheaptop>>7)<<2));
  unsigned int lheapsize=(unsigned int)heapbottom-(unsigned int)lheaptop;
  int word_offset,bit_offset,mark_word;

  if((t=tag(a))==FIXNUM || lheapsize<(unsigned int)a-(unsigned int)lheaptop)
    return;
  goto loop;
  while(sp<fp){
    a= *sp++;
    if((t=tag(a))==FIXNUM || lheapsize<(unsigned int)a-(unsigned int)lheaptop)
      continue;
  loop:
    if(t==SYMBOL){
      word_offset=((int)a>>7);
      bit_offset=((int)a>>2)&31;
      if((mark_word=lmarktop[word_offset])&(1<<bit_offset))
	continue;
      if(bit_offset>=30){
	lmarktop[word_offset]=mark_word|(1<<bit_offset);
	lmarktop[word_offset+1]|=(1<<(bit_offset-30));
      }
      else{
	lmarktop[word_offset]=mark_word|(5<<bit_offset);
      }
      b=value(a);
      c=definition(a);
      d=plist(a);
      a=prname(a);
      if(tag(b)!=FIXNUM && (unsigned int)b-(unsigned int)lheaptop<=lheapsize)
	*--sp=b;
      if(tag(c)!=FIXNUM && (unsigned int)c-(unsigned int)lheaptop<=lheapsize)
	*--sp=c;
      if(tag(d)!=FIXNUM && (unsigned int)d-(unsigned int)lheaptop<=lheapsize)
	*--sp=d;
      if((t=tag(a))==FIXNUM || lheapsize<(unsigned int)a-(unsigned int)lheaptop)
	continue;
      goto loop;
    }
    if(GC_MARKED(a,lheaptop,lmarktop))
      continue;
    SET_GCMARK(a,lheaptop,lmarktop);
    if(t==CONS){
      b=cdr(a);
      a=car(a);
      if(tag(b)!=FIXNUM && (unsigned int)b-(unsigned int)lheaptop<=lheapsize)
	*--sp=b;
      if((t=tag(a))==FIXNUM || lheapsize<(unsigned int)a-(unsigned int)lheaptop)
	continue;
      goto loop;
    }
    switch(t=otag(a)){
#ifndef NO_FLONUM
    case FLONUM:
#endif
#ifndef NO_BIGNUM
    case BIGNUM:
#endif
    case STRING:
#ifdef OBJECTSPECIAL
    case SPECIAL:
#endif
      continue;
    case STREAM:
      a=filname(a);
      if((t=tag(a))==FIXNUM || lheapsize<(unsigned int)a-(unsigned int)lheaptop)
	continue;
      goto loop;
    case CODE:
      len=fixtoi(osize(a));
      for(i=0;i<len;i++){
	b=literal(a,i);
	if(tag(b)!=FIXNUM && (unsigned int)b-(unsigned int)lheaptop<=lheapsize)
	  *--sp=b;
      }
      a=funcname(a);
      if((t=tag(a))==FIXNUM || lheapsize<(unsigned int)a-(unsigned int)lheaptop)
	continue;
      goto loop;
    case VECTOR:
    vector_mark:
      len=fixtoi(osize(a));
      for(i=0;i<len;i++){
	b=vecelm(a,i);
	if(tag(b)!=FIXNUM && (unsigned int)b-(unsigned int)lheaptop<=lheapsize)
	  *--sp=b;
      }
      continue;
    default: /* reference */
      for(;tag3(t=otag(a))!=OTAGS;a-=4);
      if(t!=VECTOR){fputs("gc error in reference\n",stderr);abort();}
      goto vector_mark;
    }
  }
}

void mark_all(WORD *fp)
{
  int i;
  WORD *ptr;
  int *lbstackbottom=(int *)bstackbottom;
  int *lcstackbottom=(int *)cstackbottom;
  struct lispenv *lestackbottom=estackbottom;
  struct lispenv *envptr;

  for(i=0;i<sizeof(root)/sizeof(WORD);i++)
    markobj(root[i],fp);
#ifndef NOSYMROOT
  for(i=0;i<SYMROOTSIZE;i++)
    markobj(symroot[i],fp);
#endif
  for(ptr=(WORD *)bstack;ptr<(WORD *)lbstackbottom;ptr++)
    markobj(*ptr,fp);
  for(ptr=(WORD *)cstack;ptr<(WORD *)lcstackbottom;ptr++)
    markobj(*ptr,fp);
  for(envptr=estack;envptr<lestackbottom;envptr++)
    markobj(envptr->envobj,fp);
  for(ptr= (WORD *)lstackbottom-1;ptr>=fp;ptr--)
    markobj(*ptr,fp);
}

int init_make_first_bit;
char first_bit[0x10000];

void make_first_bit(void)
{
  int i,j;

  for(i=0;i<0x10000;i++){
    for(j=0;j<16;j++)
      if((i>>j)&1)
	break;
    if(j==16)
      first_bit[i]= -1;
    else
      first_bit[i]= j;
  }
}

void make_garbage_link(void)
{
  int *ptr;
  int *newptr,*last_ptr;
  int mark_word,size;
  int *lheaptop=(int *)heaptop;
  int *lmarktop=(int *)((int)marktop-(((int)lheaptop>>7)<<2));
  int *lheapbottom=(int *)heapbottom;
  int *lgheap=(int *)gheap;
  int bit_offset,word_offset;
  int nbits,tmp;
  unsigned int tmp_mark;
  char *lfirst_bit=first_bit;

  last_ptr=&garbage_top[0];
  lgheap=(int *)gheaptop;
  ptr=(int *)lheaptop;
  mark_word=MARK_WORD((WORD)ptr,lheaptop,lmarktop);
  bit_offset=0;
  for(;;){
    if(mark_word & (1<<bit_offset)){
    found:
      if(ptr-last_ptr==2){
	last_ptr[0]=GARBAGE_LINK2;
      }
      else{
	last_ptr[0]=GARBAGE_LINK;
	last_ptr[2]=(int)ptr;
      }
    loop:
      do {
	switch (*ptr){
#ifndef NO_FLONUM
	case FLONUM: size=FLOSIZE; 
	  break;
#endif
#ifndef NO_BIGNUM
	case BIGNUM: size=BIGSIZE+fixtoi(ptr[1]);
	  break;
#endif
	case VECTOR: size=VECSIZE+fixtoi(ptr[1]);
	  break;
	case STRING: size=STRINGSIZE+(fixtoi(ptr[1])+4)/4;
	  break;
	case STREAM: size=STREAMSIZE;
	  break;
	case CODE: size=CODESIZE+fixtoi(ptr[1]);
	  break;
#ifdef OBJECTSPECIAL
	case SPECIAL: size=SPECSIZE; break;
#endif
	default: /* cons or symbol */
	  size=2;
	}
	newptr=ptr+size;
	bit_offset+=size;
	if(bit_offset>31){
	  mark_word=MARK_WORD((WORD)newptr,lheaptop,lmarktop);
	  SET_MARK_WORD((WORD)newptr,(int)(lgheap+((int *)((int)newptr&0xffffff80)-ptr)),lheaptop,lmarktop);
	  bit_offset&=31;
	}
	ptr=newptr;
	lgheap+=size;
	if(ptr>=(int *)lheapbottom){
	  break;
	}
      } while(mark_word & (1<<bit_offset));
      newptr=ptr+1;
      bit_offset++;
      if(bit_offset>31){
	bit_offset&=31;
	mark_word=MARK_WORD((WORD)newptr,lheaptop,lmarktop);
	if(mark_word & (1<<bit_offset)){
	  lgheap++; /* don't remove garbage_1 for saving time */
	  SET_MARK_WORD((WORD)newptr,(int)lgheap,lheaptop,lmarktop);
	  ptr[0]=GARBAGE_1;
	  ptr=newptr;
	  goto loop;
	}
	SET_MARK_WORD((WORD)newptr,(int)lgheap,lheaptop,lmarktop);
	if((int *)newptr>=(int *)lheapbottom){
	  last_ptr=ptr;
	  ptr=newptr;
	  break;
	}
      }
      else if(mark_word & (1<<bit_offset)){
	lgheap++; /* don't remove garbage_1 for saving time */
	ptr[0]=GARBAGE_1;
	ptr=newptr;
	goto loop;
      }
      last_ptr=ptr;
      ptr[1]=(int)lgheap;
      ptr=newptr;
    }
    ptr++;
    bit_offset++;
    if(bit_offset>31){
    word_align:
      bit_offset=0;
      word_offset=((int)ptr>>7);
      mark_word=lmarktop[word_offset];
      lmarktop[word_offset]=(int)lgheap;
      if((int *)ptr>=(int *)lheapbottom)
	break;
      if(!mark_word){
	for(;;) {
	  ptr+=32;
	  word_offset++;
	  mark_word=lmarktop[word_offset];
	  lmarktop[word_offset]=(int)lgheap;
	  if(mark_word)
	    break;
	  if((int *)ptr>=(int *)lheapbottom)
	    goto endloop;
	}
      }
      else{
	for(tmp_mark=mark_word,nbits=0;;tmp_mark>>=16,nbits+=16){
	  if((tmp=lfirst_bit[tmp_mark&0xffff])>=0){
	    tmp+=nbits;
	    ptr+=tmp;
	    bit_offset+=tmp;
	    goto found;
	  }
	}
      }
    }
    else{
      tmp_mark=(unsigned int)mark_word>>bit_offset;
      if(!tmp_mark){
	ptr+=32-bit_offset;
	goto word_align;
      }
      for(nbits=0;;tmp_mark>>=16,nbits+=16){
	if((tmp=lfirst_bit[tmp_mark&0xffff])>=0){
	  tmp+=nbits;
	  ptr+=tmp;
	  bit_offset+=tmp;
	  goto found;
	}
      }
    }
  }
 endloop:
  gheap=(WORD *)lgheap;
  if(last_ptr==(int *)lheapbottom){
    last_ptr[0]=GARBAGE_LINK2;
    last_ptr[1]=(int)lgheap;
  }
  else{
    last_ptr[0]=GARBAGE_LINK;
    last_ptr[1]=(int)lgheap;
    last_ptr[2]=(int)lheapbottom;
  }
}

int newpointer(WORD a, int *lheaptop, int *lmarktop, unsigned int lheapsize)
{
  int *ptr,*newptr;
  int offset=0,t,size,ot;
  int bit_offset;

  if((t=tag(a))==FIXNUM || lheapsize<(unsigned int)a-(unsigned int)lheaptop)
    return (int)a;
  ptr=(int *)(((int)a>>2)<<2);
  if(t==OTHERS){
    for(;tag3(ot=ptr[0])!=OTAGS;ptr--)
      offset+=4;
    bit_offset=((int)ptr>>2)&31;
    goto others;
  }
  if(t==CONS) {
    bit_offset=((int)ptr>>2)&31;
    size=2;
    goto cons_symbol;
  }
  else{
    bit_offset=((int)ptr>>2)&31;
    size=4;
    goto cons_symbol;
  }
  for(;;){
    ot=ptr[0];
  others:
    switch (ot){
#ifndef NO_FLONUM
    case FLONUM: size=FLOSIZE; break;
#endif
#ifndef NO_BIGNUM
    case BIGNUM: size=BIGSIZE+fixtoi(ptr[1]); break;
#endif
    case VECTOR: size=VECSIZE+fixtoi(ptr[1]); break;
    case STRING: size=STRINGSIZE+(fixtoi(ptr[1])+4)/4; break;
    case STREAM: size=STREAMSIZE; break;
    case CODE: size=CODESIZE+fixtoi(ptr[1]); break;
#ifdef OBJECTSPECIAL
    case SPECIAL: size=SPECSIZE; break;
#endif
    case GARBAGE_1: size=1; break;
    case GARBAGE_LINK:
    case GARBAGE_LINK2:
      return (int)(ptr[1]|t)+offset;
    default: /* cons or symbol */
      size=2;
    }
  cons_symbol:
    newptr=ptr+size;
    bit_offset+=size;
    if(bit_offset>31){
      offset-=((int *)((int)newptr&0xffffff80)-ptr)*4;
      return (int)((MARK_WORD((WORD)newptr,lheaptop,lmarktop)|t)+offset);
    }
    offset-=size*4;
    ptr=newptr;
  }
}

void relocate_pointers(WORD *fp)
{
  int *ptr;
  int i,len;
  struct lispenv *envptr;
  int *lheaptop=(int *)heaptop;
  int *lheapbottom=(int *)heapbottom;
  unsigned int lheapsize=(unsigned int)lheapbottom-(unsigned int)lheaptop;
  int *lmarktop=(int *)((int)marktop-(((int)lheaptop>>7)<<2));
  int *lbstackbottom;
  int *lcstackbottom;
  struct lispenv *lestackbottom;

  for(i=0;i<sizeof(root)/sizeof(WORD);i++)
    root[i]=(WORD)newpointer(root[i],lheaptop,lmarktop,lheapsize);
#ifndef NOSYMROOT
  for(i=0;i<SYMROOTSIZE;i++)
    symroot[i]=(WORD)newpointer(symroot[i],lheaptop,lmarktop,lheapsize);
#endif
  lbstackbottom=(int *)bstackbottom;
  for(ptr=(int *)bstack;ptr<(int *)lbstackbottom;ptr++)
    *ptr=newpointer((WORD)(*ptr),lheaptop,lmarktop,lheapsize);
  lcstackbottom=(int *)cstackbottom;
  for(ptr=(int *)cstack;ptr<(int *)lcstackbottom;ptr++)
    *ptr=newpointer((WORD)(*ptr),lheaptop,lmarktop,lheapsize);
  lestackbottom=estackbottom;
  for(envptr=estack;envptr<lestackbottom;envptr++)
    envptr->envobj=(WORD)newpointer((WORD)(envptr->envobj),lheaptop,lmarktop,lheapsize);
  for(ptr= (int *)lstackbottom-1;ptr>=(int *)fp;ptr--)
    *ptr=newpointer((WORD)(*ptr),lheaptop,lmarktop,lheapsize);
  /* sweep heap */
  ptr=(int *)garbage_top[2];
  for(;;){
    switch (*ptr){
#ifndef NO_FLONUM
    case FLONUM: ptr+=FLOSIZE; continue;
#endif
#ifndef NO_BIGNUM
    case BIGNUM: ptr+=BIGSIZE+fixtoi(ptr[1]); continue;
#endif
    case VECTOR: 
      len=fixtoi(ptr[1]);
      for(i=0;i<len;i++)
	ptr[i+2]=newpointer((WORD)ptr[i+2],lheaptop,lmarktop,lheapsize);
      ptr+=VECSIZE+len;
      continue;
    case STRING: 
      ptr+=STRINGSIZE+(fixtoi(ptr[1])+4)/4; continue;
    case STREAM: 
      ptr[2]=newpointer((WORD)ptr[2],lheaptop,lmarktop,lheapsize);
      ptr+=STREAMSIZE; 
      continue;
    case CODE: 
      len=fixtoi(ptr[1]);
      ptr[2]=newpointer((WORD)ptr[2],lheaptop,lmarktop,lheapsize);
      for(i=0;i<len;i++)
	ptr[i+6]=newpointer((WORD)ptr[i+6],lheaptop,lmarktop,lheapsize);
      ptr+=CODESIZE+len; 
      continue;
#ifdef OBJECTSPECIAL
    case SPECIAL: 
      ptr+=SPECSIZE; 
      continue;
#endif
    case GARBAGE_LINK: /* sentinel */
      ptr=(int *)ptr[2];
      if(ptr>=(int *)lheapbottom) return;
      continue;
    case GARBAGE_LINK2:
      ptr+=2;
      if(ptr>=(int *)lheapbottom) return;
      continue;
    case GARBAGE_1:
      ptr++; continue;
    default: /* cons or symbol */
      ptr[0]=newpointer((WORD)ptr[0],lheaptop,lmarktop,lheapsize);
      ptr[1]=newpointer((WORD)ptr[1],lheaptop,lmarktop,lheapsize);
      ptr+=2;
      continue;
    }
  }
}

void pack_heap(void)
{
  int *ptr,*dest,*lheapbottom=(int *)heapbottom;
  int size,t,i;
  int a;

  ptr= (int *)garbage_top[2];
  dest=(int *)heaptop;
  for(;;){
    if(ptr>=lheapbottom)
      break;
    switch((t=ptr[0])){
#ifndef NO_FLONUM
    case FLONUM:
      dest[0]=t;
      dest[1]=ptr[1];
      dest[2]=ptr[2];
      dest+=FLOSIZE;
      ptr+=FLOSIZE;
      continue;
#endif
#ifndef NO_BIGNUM
    case BIGNUM:
#endif
    case VECTOR:
      a=ptr[1];
      dest[0]=t;
      dest[1]=a;
      size=fixtoi(a)+VECSIZE;
      for(i=2;i<size;i++)
	dest[i]=ptr[i];
      dest+=size;
      ptr+=size;
      continue;
    case STRING:
      a=ptr[1];
      dest[0]=t;
      dest[1]=a;
      size=(fixtoi(a)+4)/4+STRINGSIZE;
      for(i=2;i<size;i++)
	dest[i]=ptr[i];
      dest+=size;
      ptr+=size;
      continue;
    case STREAM:
      dest[0]=t;
      for(i=1;i<STREAMSIZE;i++)
	dest[i]=ptr[i];
      dest+=STREAMSIZE;
      ptr+=STREAMSIZE;
      continue;
    case CODE:
      a=ptr[1];
      dest[0]=t;
      dest[1]=a;
      size=fixtoi(a)+CODESIZE;
      for(i=2;i<size;i++)
	dest[i]=ptr[i];
      dest+=size;
      ptr+=size;
      continue;
#ifdef OBJECTSPECIAL
    case SPECIAL: 
      dest[0]=t;
      dest[1]=ptr[1];
      dest[2]=ptr[2];
      dest[3]=ptr[3];
      dest+=SPECSIZE;
      ptr+=SPECSIZE;
      continue;
#endif
    case GARBAGE_LINK2:
      ptr=ptr+2;
      if(ptr>=lheapbottom)
	return;
      continue;
    case GARBAGE_LINK:
      ptr=(int *)ptr[2];
      if(ptr>=lheapbottom)
	return;
      continue;
    case GARBAGE_1:
      dest++;
      ptr++;
      continue;
    default:
      dest[0]=t;
      dest[1]=ptr[1];
      dest+=2;
      ptr+=2;
    }
  }
}


WORD mark_gc(int size, int offset, WORD *fp)
{
  struct tms buffer;
  time_t utime;
  void (*oldsigint)();
  WORD retval;

  if(!init_make_first_bit){
    make_first_bit();
    init_make_first_bit=1;
  }
  times(&buffer);
  utime=buffer.tms_utime;
  oldsigint=signal(SIGINT,SIG_IGN);
  /* clear marks */
  gheaptop=heaptop+offset/4;
  gheapbottom=gheaptop+heapsize/sizeof(int);
  marktop=heapbottom+2;
  memset(marktop,0,heapsize/32+8);
  /* mark roots */
  mark_all(fp);
  /* make garbage link */
  make_garbage_link();
  /* relocate pointers */
  relocate_pointers(fp);
  /* packing heap */
  pack_heap();
  if(offset){
    memmove(gheaptop,heaptop,heapsize);
  }
  allheaptop=heaptop=gheaptop;
  heapbottom=gheapbottom;heap=gheap;
  allheapbottom=heaptop+allheapsize/sizeof(int);
  signal(SIGINT,oldsigint);
  gccount++;
  times(&buffer);
  gctime+=buffer.tms_utime-utime;
  if(value(gc_hook)!=UNBOUND){
    pushb(gc_hook);
    pushb(retval=value(gc_hook));
    value(gc_hook)=UNBOUND;
    l(0)=itofix(size);
    retval=funcall(retval,1,fp-1);
    value(gc_hook)=popb();
    discardb();
    return retval;
  }
  return nil;
}

WORD gc(int size, WORD *fp)
{
  switch(gctype){
  case 0:
    return copy_gc(size,fp);
  case 1:
    return mark_gc(size,0,fp);
  }
  return nil;
}

WORD extendheap(int newheapsize, WORD *fp)
{
  int *ptr;
  WORD *newheaptop;
  int i,newallheapsize;
  struct lispenv *envptr;
  void (*oldsigint)();
  int *lheaptop;
  unsigned int lheapsize;
  int *lgheaptop;
  unsigned int lgheapsize;

  if(newheapsize<heapsize)return nil;
  if(gctype==1)
    newallheapsize=newheapsize+(newheapsize+31)/32+16;
  else
    newallheapsize=newheapsize*2;
#ifndef NOMALLOC
  if(gctype==1){
    if((newheaptop=(WORD *)malloc(newallheapsize+128))==NULL){
      return nil;
    }
    newheaptop = (WORD *)(((int)newheaptop+127) & ~127);
  }
  else if((newheaptop=(WORD *)malloc(newallheapsize))==NULL){
    return nil;
  }
#else
  if((newheaptop=(WORD *)sbrk(newallheapsize-allheapsize))!=allheapbottom){
    char buf[256];
    fputs("alloc failed\n",stderr);
    sprintf(buf,"0x%x 0x%x\n",newheaptop,allheapbottom);
    fputs(buf,stderr);
    return nil;
  }
  while(allheaptop!=heaptop)gc(0,fp);
  newheaptop=heaptop;
#endif
  oldsigint=signal(SIGINT,SIG_IGN);
  heapsize=newheapsize;
  allheapsize=newallheapsize;
  if(gctype==1)
    gheap=gheaptop=newheaptop;
  else
    gheap=gheaptop=newheaptop+heapsize/sizeof(WORD);
  gheapbottom=gheaptop+heapsize/sizeof(WORD);
  lheaptop=(int *)heaptop;
  lheapsize=(unsigned int)heapbottom-(unsigned int)lheaptop;
  lgheaptop=(int *)gheaptop;
  lgheapsize=(unsigned int)gheapbottom-(unsigned int)lgheaptop;
#ifndef NOMALLOC
  for(i=0;i<sizeof(root)/sizeof(WORD);i++)
    root[i]=(WORD)Copyobj(root[i],lheaptop,lheapsize,lgheaptop,lgheapsize);
#ifndef NOSYMROOT
  for(i=0;i<SYMROOTSIZE;i++)
    symroot[i]=(WORD)Copyobj(symroot[i],lheaptop,lheapsize,lgheaptop,lgheapsize);
#endif
  for(ptr=(int *)bstack;ptr<(int *)bstackbottom;ptr++)
    *ptr=(int)Copyobj((WORD)(*ptr),lheaptop,lheapsize,lgheaptop,lgheapsize);
  for(ptr=(int *)cstack;ptr<(int *)cstackbottom;ptr++)
    *ptr=(int)Copyobj((WORD)(*ptr),lheaptop,lheapsize,lgheaptop,lgheapsize);
  for(envptr=estack;envptr<estackbottom;envptr++)
    envptr->envobj=(WORD)Copyobj(envptr->envobj,lheaptop,lheapsize,lgheaptop,lgheapsize);
  for(ptr= (int *)lstackbottom-1;ptr>=(int *)fp;ptr--)
    *ptr=(int)Copyobj((WORD)(*ptr),lheaptop,lheapsize,lgheaptop,lgheapsize);
  sweep();
  heaptop=gheaptop;heapbottom=gheapbottom;heap=gheap;
  free(allheaptop);
#endif /* NOMALLOC */
  allheaptop=newheaptop;
  allheapbottom=allheaptop+allheapsize/sizeof(WORD);
  heapbottom=heaptop+heapsize/sizeof(int);
  signal(SIGINT,oldsigint);
  return true;
}

char *alloc_codearea(int size, WORD *fp)
{
  WORD *newheaptop;
  int *ptr,i;
  struct lispenv *envptr;
  int oldcodeareasize;
  void (*oldsigint)();
  int *lheaptop;
  unsigned int lheapsize;
  int *lgheaptop;
  unsigned int lgheapsize;

  size=(size+127)&0xffffff80; /* lower 7 bits must be zero */
  if((WORD *)sbrk(size)!=allheapbottom){
    fputs("alloc_codeare failed\n",stderr);
    return 0;
  }
  while(allheaptop!=heaptop)gc(0,fp);
  newheaptop=(WORD *)((char *)allheaptop+size);
  if(gctype==0){ /* copy gc */
    oldsigint=signal(SIGINT,SIG_IGN);
    gheap=gheaptop=newheaptop+heapsize/sizeof(WORD);
    gheapbottom=gheaptop+heapsize/sizeof(WORD);
    lheaptop=(int *)heaptop;
    lheapsize=(unsigned int)heapbottom-(unsigned int)lheaptop;
    lgheaptop=(int *)gheaptop;
    lgheapsize=(unsigned int)gheapbottom-(unsigned int)lgheaptop;
    for(i=0;i<sizeof(root)/sizeof(WORD);i++)
      root[i]=(WORD)Copyobj(root[i],lheaptop,lheapsize,lgheaptop,lgheapsize);
#ifndef NOSYMROOT
    for(i=0;i<SYMROOTSIZE;i++)
      symroot[i]=(WORD)Copyobj(symroot[i],lheaptop,lheapsize,lgheaptop,lgheapsize);
#endif
    for(ptr=(int *)bstack;ptr<(int *)bstackbottom;ptr++)
      *ptr=(int)Copyobj((WORD)(*ptr),lheaptop,lheapsize,lgheaptop,lgheapsize);
    for(ptr=(int *)cstack;ptr<(int *)cstackbottom;ptr++)
      *ptr=(int)Copyobj((WORD)(*ptr),lheaptop,lheapsize,lgheaptop,lgheapsize);
    for(envptr=estack;envptr<estackbottom;envptr++)
      envptr->envobj=(WORD)Copyobj(envptr->envobj,lheaptop,lheapsize,lgheaptop,lgheapsize);
    for(ptr= (int *)lstackbottom-1;ptr>=(int *)fp;ptr--)
      *ptr=(int)Copyobj((WORD)*ptr,lheaptop,lheapsize,lgheaptop,lgheapsize);
    sweep();
    heaptop=gheaptop;heapbottom=gheapbottom;heap=gheap;
    allheaptop=newheaptop;
    allheapbottom=allheaptop+heapsize*2/sizeof(WORD);
    signal(SIGINT,oldsigint);
  }
  else{ /* mark  & scan gc */
    mark_gc(0,size,fp);
  }
  oldcodeareasize=codeareasize;
  codeareasize+=size;
  return codeareatop+oldcodeareasize;
}

WORD gc_f(int na, WORD *fp)
{
  WORD a;

  if(na==0)return gc(0,fp);
  else if(na==1){
    slow_checkfix(a,ag(0));
    return gc(fixtoi(a),fp+1);
  }
  else
    parerr();
}

WORD extendheap_f(int na, WORD *fp)
{
  WORD a;

  if(na==1){
    slow_checkfix(a,ag(0));
    return extendheap(fixtoi(a),fp+1);
  }
  else
    parerr();
}

WORD extendheapK_f(int na, WORD *fp)
{
  WORD a;

  if(na==1){
    slow_checkfix(a,ag(0));
    return extendheap(1024*fixtoi(a),fp+1);
  }
  else
    parerr();
}

WORD gccount_f(int na, WORD *fp)
{
  if(na!=0)parerr();
  return itofix(gccount);
}

WORD gctime_f(int na, WORD *fp)
{
  if(na!=0)parerr();
  return itofix(gctime);
}

WORD heapsize_f(int na, WORD *fp)
{
  if(na!=0)parerr();
  return itofix(heapsize);
}

WORD heapsizeK_f(int na, WORD *fp)
{
  if(na!=0)parerr();
  return itofix(heapsize/1024);
}

WORD heapfree_f(int na, WORD *fp)
{
  return itofix((char *)heapbottom-(char *)heap);
}

WORD heapfreeK_f(int na, WORD *fp)
{
  return itofix(((char *)heapbottom-(char *)heap)/1024);
}

WORD symsize_f(int na, WORD *fp)
{
  if(na!=0)parerr();
  return itofix(0);
}

WORD symfree_f(int na,WORD *fp)
{
  return itofix(0);
}
WORD fixsize_f(int na,WORD *fp)
{
  if(na!=0)parerr();
  return itofix(0);
}

WORD fixfree_f(int na,WORD *fp)
{
  if(na!=0)parerr();
  return itofix(0);
}

WORD stacksize_f(int na, WORD *fp)
{
  if(na!=0)parerr();
  return itofix(lstacksize);
}

WORD stack_used_f(int na, WORD *fp)
{
  if(na!=0)parerr();
  return itofix((int)(fp-((char *)lstack-(char *)lstacktop)));
}
WORD othersize_f(int na, WORD *fp)
{
  if(na!=0)parerr();
  return itofix(0);
}

WORD otherfree_f(int na, WORD *fp)
{
  if(na!=0)parerr();
  return itofix(0);
}

WORD stack_bottom_f(int na, WORD *fp)
{
  return itofix(0);
}
WORD stack_top_f(int na, WORD *fp)
{
  return itofix(0);
}
WORD stack_base_f(int na, WORD *fp)
{
  return itofix(0);
}

#define WMWORD 4

void init_stackWM(WORD *fp)
{
  int *ptr;
  struct lispenv *envptr;

  for(ptr= (int *)lstacktop;ptr<(int *)fp;ptr++)
    *ptr=WMWORD;
  for(ptr=(int *)bstacktop;ptr<(int *)bstack;ptr++)
    *ptr=WMWORD;
  for(ptr=(int *)cstacktop;ptr<(int *)cstack;ptr++)
    *ptr=WMWORD;
  for(envptr=estacktop;envptr<estack;envptr++)
    envptr->envobj=(WORD)WMWORD;
}

WORD init_stackWM_f(int na, WORD *fp)
{
  if(na!=0) parerr();
  init_stackWM(fp);
  return nil;
}

WORD stackWM_f(int na, WORD *fp)
{
  int *ptr;
  struct lispenv *envptr;

  if(na!=0) parerr();
  l(0)=nil;
  l(1)=nil;
  for(envptr=estack;envptr<estack;envptr++)
    if(envptr->envobj!=(WORD)WMWORD)break;
  l(0)=alcons(itofix((int)estackbottom-(int)envptr),
	      itofix((int)estackbottom-(int)estacktop),fp-2);
  l(1)=alcons(l(0),nil,fp-2);
  for(ptr=(int *)cstacktop;ptr<(int *)cstack;ptr++)
    if(*ptr!=WMWORD) break;
  l(0)=alcons(itofix((int)cstackbottom-(int)ptr),
	      itofix((int)cstackbottom-(int)cstacktop),fp-2);
  l(1)=alcons(l(0),l(1),fp-2);
  for(ptr=(int *)bstacktop;ptr<(int *)bstack;ptr++)
    if(*ptr!=WMWORD) break;
  l(0)=alcons(itofix((int)bstackbottom-(int)ptr),
	      itofix((int)bstackbottom-(int)bstacktop),fp-2);
  l(1)=alcons(l(0),l(1),fp-2);
  for(ptr=(int *)lstacktop;ptr<(int *)fp;ptr++)
    if(*ptr!=WMWORD) break;
  l(0)=alcons(itofix((int)lstackbottom-(int)ptr),
	      itofix((int)lstackbottom-(int)lstacktop),fp-2);
  l(1)=alcons(l(0),l(1),fp-2);
  return l(1);
}
