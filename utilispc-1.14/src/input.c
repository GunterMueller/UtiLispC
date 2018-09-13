/* @(#)input.c	1.2 90/07/30 */

#include "constdef.h"
#include "defvar.h"
#include "read.h"

int Default_readtable[256]={
  0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,	/* 00 */
  0x4,0x28300,0x28300,0x4,0x4,0x4,0x4,0x4,
  0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,	/* 10 */
  0x4,0x4,0x4,0x4,0x20b00,0x4,0x4,0x4,
  0x28300,0x4,0x20700,0x4,0x4,0x4,0x4,0x20b00,	/* 20 */
  0x24300,0x21300,0x4,0x220,0x4,0x260,0x2002200,0x380,
  0x210,0x210,0x210,0x210,0x210,0x210,0x210,0x210,	/* 30 */
  0x210,0x210,0x4,0x20308,0x4,0x4,0x4,0x4,
  0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,	/* 40 */
  0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,
  0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,	/* 50 */
  0x4,0x4,0x4,0x4,0x4,0x4,0x1000104,0x4,
  0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4, /* 60 */
  0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4, /* 70 */
  0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4, /* 80 */
  0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4, /* 90 */
  0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4, /* a0 */
  0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4, /* b0 */
  0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4, /* c0 */
  0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4, /* d0 */
  0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4, /* e0 */
  0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4,0x4 /* f0 */
};

unsigned char GETC(WORD stream, WORD *fp)
{
  int c,i,e,ecount,len;
  WORD a,b;

  if(curpos(stream)>=recend(stream)){
    if(mode(stream)&STRINGSTREAM){
      fildes(stream)+=recend(stream);
      if((i=fildes(stream))+BUFSIZE<(len=fixtoi(osize(filname(stream))))){
	memmove(buftop(stream),stringcodes(filname(stream))+i,BUFSIZE);
	recend(stream)=BUFSIZE;
      }
      else if(i>=len) readerr(stream,fp);
      else {
	memmove(buftop(stream),stringcodes(filname(stream))+i,len-i);
	recend(stream)=len-i;
      }
      curpos(stream)=0;
    }
    else{
      if((e=recend(stream)=read(fildes(stream),buftop(stream),BUFSIZE))<=0)
	readerr(stream,fp);
      curpos(stream)=0;
      if(stream==value(terminal_input)){
	for(i=ecount=0;i<e;i++){
	  if(*(buftop(stream)+i)=='!'){
	    if(++ecount>=10)exit(1);
	  }
	  else ecount=0;
	}
      }
    }
  }
  if((c=peekch(stream))=='\n'){
    memmove(buftop(stream),buftop(stream)+curpos(stream)+1,recend(stream)-curpos(stream)-1);
    if(mode(stream)&STRINGSTREAM){
      fildes(stream)+=curpos(stream)+1;
    }
    recend(stream)-=curpos(stream)+1;
    curpos(stream)=0;
    if(stream==value(terminal_input)){
      checkstr(a,value(prompt));
      checkstream(b,value(terminal_output));
      write(fildes(b),stringcodes(a),fixtoi(osize(a)));
    }
    return c;
  }
  curpos(stream)++;
  return c;
}

/* ストリームの次に読まれるデータを返す */

unsigned char PEEKC(WORD stream, WORD *fp)
{
  int i,ecount,e,len;

  if(curpos(stream)>=recend(stream)){
    if(mode(stream)&STRINGSTREAM){
      fildes(stream)+=recend(stream);
      if((i=fildes(stream))+BUFSIZE<(len=fixtoi(osize(filname(stream))))){
	memmove(buftop(stream),stringcodes(filname(stream))+i,BUFSIZE);
	recend(stream)=BUFSIZE;
      }
      else if(i>=len) readerr(stream,fp);
      else {
	memmove(buftop(stream),stringcodes(filname(stream))+i,len-i);
	recend(stream)=len-i;
      }
      curpos(stream)=0;
    }
    else{
      if((e=recend(stream)=read(fildes(stream),buftop(stream),BUFSIZE))<=0)
	readerr(stream,fp);
      curpos(stream)=0;
      if(stream==value(terminal_input)){
	for(i=ecount=0;i<e;i++){
	  if(*(buftop(stream)+i)=='!'){
	    if(++ecount>=10)exit(1);
	  }
	  else ecount=0;
	}
      }
    }
  }
  return peekch(stream);
}

unsigned char GETCERR(WORD stream, WORD *fp)
{
  int c,i,e,ecount,len;
  WORD a,b;

  if(curpos(stream)>=recend(stream)){
    if(mode(stream)&STRINGSTREAM){
      fildes(stream)+=recend(stream);
      if((i=fildes(stream))+BUFSIZE<(len=fixtoi(osize(filname(stream))))){
	memmove(buftop(stream),stringcodes(filname(stream))+i,BUFSIZE);
	recend(stream)=BUFSIZE;
      }
      else if(i>=len) eoferr(stream,fp);
      else {
	memmove(buftop(stream),stringcodes(filname(stream))+i,len-i);
	recend(stream)=len-i;
      }
      curpos(stream)=0;
    }
    else{
      if((e=recend(stream)=read(fildes(stream),buftop(stream),BUFSIZE))<=0)
	eoferr(stream,fp);
      curpos(stream)=0;
      if(stream==value(terminal_input)){
	for(i=ecount=0;i<e;i++){
	  if(*(buftop(stream)+i)=='!'){
	    if(++ecount>=10)exit(1);
	  }
	  else ecount=0;
	}
      }
    }
  }
  if((c=peekch(stream))=='\n'){
    memmove(buftop(stream),buftop(stream)+curpos(stream)+1,recend(stream)-curpos(stream)-1);
    if(mode(stream)&STRINGSTREAM){
      fildes(stream)+=curpos(stream)+1;
    }
    recend(stream)-=curpos(stream)+1;
    curpos(stream)=0;
    if(stream==value(terminal_input)){
      checkstr(a,value(prompt));
      checkstream(b,value(terminal_output));
      write(fildes(b),stringcodes(a),fixtoi(osize(a)));
    }
    return c;
  }
  curpos(stream)++;
  return c;
}

/* ストリームの次に読まれるデータを返す */

unsigned char PEEKCERR(WORD stream, WORD *fp)
{
  int i,ecount,e,len;

  if(curpos(stream)>=recend(stream)){
    if(mode(stream)&STRINGSTREAM){
      fildes(stream)+=recend(stream);
      if((i=fildes(stream))+BUFSIZE<(len=fixtoi(osize(filname(stream))))){
	memmove(buftop(stream),stringcodes(filname(stream))+i,BUFSIZE);
	recend(stream)=BUFSIZE;
      }
      else if(i>=len) eoferr(stream,fp);
      else {
	memmove(buftop(stream),stringcodes(filname(stream))+i,len-i);
	recend(stream)=len-i;
      }
      curpos(stream)=0;
    }
    else{
      if((e=recend(stream)=read(fildes(stream),buftop(stream),BUFSIZE))<=0)
	eoferr(stream,fp);
      curpos(stream)=0;
      if(stream==value(terminal_input)){
	for(i=ecount=0;i<e;i++){
	  if(*(buftop(stream)+i)=='!'){
	    if(++ecount>=10)exit(1);
	  }
	  else ecount=0;
	}
      }
    }
  }
  return peekch(stream);
}

WORD dfltrdt_v,dfltmct_v;
WORD rdqt_c;

/* リードテーブル、マクロテーブルの初期化を行なう */
/* 標準のストリームを設定する                     */

void init_rtable(void)
{
  int i;

  dfltrdt_v=alvector(256,lstack);
  for(i=0;i<256;i++)
    vecelm(dfltrdt_v,i)=(WORD)(Default_readtable[i]<<2);
  value(default_readtable)= value(readtable)=dfltrdt_v;
  dfltmct_v=alvector(256,lstack);
  value(default_macrotable)= value(macrotable)=dfltmct_v;
  for(i=0;i<256;i++)
    vecelm(dfltmct_v,i)=nil;
  vecelm(dfltmct_v,'\'')=definition(readquote);
  sistream=alstream(alnstring("terminal-in",11,lstack),lstack);
  fildes(sistream)=0;
  curpos(sistream)=recend(sistream)=0;
  mode(sistream)=INOPEN;
  value(terminal_input)=value(standard_input)=sistream;
  sostream=alstream(alnstring("terminal-out",12,lstack),lstack);
  fildes(sostream)=1;
  mode(sostream)=OUTOPEN;
  curpos(sostream)=recend(sostream)=0;
  value(terminal_output)=value(standard_output)=sostream;
  value(openfiles)=nil;
}

#define LFEEDCH 10

unsigned char skipblank(WORD stream, WORD rtable, WORD *fp, int flag)
{
  int t;
  unsigned char c;

  if(flag){
    for(;;){
      while((t=(int)vecelm(rtable,(c=PEEKCERR(stream,fp))))&BLANKBIT)
	GETCERR(stream,fp);
      if(!(t&COMBEGBIT))return c;
      while(GETCERR(stream,fp)!=LFEEDCH);
    }
  }
  else{
    for(;;){
      while((t=(int)vecelm(rtable,(c=PEEKC(stream,fp))))&BLANKBIT)
	GETC(stream,fp);
      if(!(t&COMBEGBIT))return c;
      while(GETC(stream,fp)!=LFEEDCH);
    }
  }
}

#ifndef NO_BIGNUM
WORD dig2big(unsigned char *buf, int index, WORD rtable, WORD *fp)
{
  int t,i,rdflag=0,size,asize;
  WORD big,temp;

  if((t=(int)vecelm(rtable,*buf))&SIGNBIT){
    if(t&ALTBIT)rdflag=1;
    buf++;index--;
  }
  big=albignum(size=(index+1)/9+3,fp);
  temp=albignum(size,fp);
  for(i=0;i<size;i++){bigcodes(big,i)=0;bigcodes(temp,i)=0;}
  asize=4;
  for(i=0;i<index;i++){
    asize=umul(&bigcodes(big,0),asize+4,10);
    bigcodes(temp,0)=(buf[i]-'0');
    asize=madd(&bigcodes(big,0),&bigcodes(temp,0),asize);
  }
  osize(big)=itofix(asize/4);
  if(rdflag)return normbig(negbig2(big,fp));
  return normbig(big);
}
#endif


/* 標準入力ストリームからリスプオブジェクトを１つ読んでくる */
/* EOFの時はeofエラーを、リスプオブジェクト以外のものを読ん */
/* だ時には、readエラーを呼ぶ                               */
/* flagが1の時はeofエラー、そうでないときはreadエラー */

WORD read_l(WORD *fp, int flag)
{
  int c,t,i,rdflag,d,na,j;
  WORD rtable,stream,mtable,rval;
  double fval,frac;
#if 0
  unsigned char strbuf[STRBUFSIZE];
#else
  unsigned char *strbuf, *newbuf;
  int strbufsize=STRBUFSIZE;
#endif
  
  strbuf=(unsigned char *)alloca(strbufsize);
  checkvec(rtable,value(readtable));
  if(osize(rtable)!=itofix(256))nosymerr(rtable);
  checkstream(stream,value(standard_input));
  if(!(mode(stream)&INOPEN))ioerr(stream,fp);
 loop:
  t=(int)vecelm(value(readtable),c=skipblank(value(standard_input),value(readtable),fp,flag));
  if(t&RPARBIT){ /* 過剰な ) は無視される */
    GETCERR(value(standard_input),fp);
    goto loop;
  }
  else if(t&LPARBIT){ /* ( */
    GETC(value(standard_input),fp);
    for(na=0;;na++){
      t=(int)vecelm(value(readtable),c=skipblank(value(standard_input),value(readtable),fp,0));
      if(t&RPARBIT){
	GETC(value(standard_input),fp); 
	return allist(na,fp,nil);
      }
      else if(t&DOTBIT){
	GETC(value(standard_input),fp);
	rval=read_l(fp,0);
	t=(int)vecelm(value(readtable),c=skipblank(value(standard_input),value(readtable),fp,0));
	if(!(t&RPARBIT))
	  readerr(value(standard_input),fp);
	GETC(value(standard_input),fp);
#if 0
	for(i=0;i<na;i++)
	  rval=alcons(*fp++,rval,fp);
	return rval;
#else
	return allist(na,fp,rval);
#endif
      }
      rval=read_l(fp,0);
      *--fp=rval;
    }
  }
  else if(t&MACROCHBIT){ /* ' ` など */
    checkvec(mtable,value(macrotable));
    if(osize(mtable)!=itofix(256))nosymerr(mtable);
    GETC(value(standard_input),fp);
    return funcall(vecelm(mtable,c),0,fp);
  }
  else if(t&STRQBIT){ /* " */
    for(i=0,GETC(value(standard_input),fp);;){
      while(!((int)vecelm(value(readtable),(c=GETC(value(standard_input),fp)))&STRQBIT)){
	strbuf[i++]=c;
#if 0
	if(i>=STRBUFSIZE)bufferr(fp);
#else
	if(i>=strbufsize){
	  newbuf=(unsigned char *)alloca(strbufsize*2);
	  for(j=0;j<strbufsize;j++)newbuf[j]=strbuf[j];
	  strbufsize*=2;
	  strbuf=newbuf;
	}
#endif
      }
      if((int)vecelm(value(readtable),c=PEEKC(value(standard_input),fp))&STRQBIT){
	strbuf[i++]=c;GETC(value(standard_input),fp);
#if 0
	if(i>=STRBUFSIZE)bufferr(fp);
#else
	if(i>=strbufsize){
	  newbuf=(unsigned char *)alloca(strbufsize*2);
	  for(j=0;j<strbufsize;j++)newbuf[j]=strbuf[j];
	  strbufsize*=2;
	  strbuf=newbuf;
	}
#endif
	continue;
      }
      break;
    }
    strbuf[i]=0;
    return alnstring((char*)strbuf,i,fp);
  }
  else if(t&SINGLEBIT){ /* 一文字でシンボルとなるキャラクタ */
    GETC(value(standard_input),fp);
    strbuf[0]=c;strbuf[1]=0;
    l(0)=alnstring((char*)strbuf,1,fp);
    return funcall(value(intern_v),1,fp-1);
  }
  /* 後は数字か又はシンボル */
  rdflag=0; /* 数字の時の正負、指数部の正負 */
  i=0; /* 文字のインデックス */
  if(t&SIGNBIT){
    if(t&ALTBIT)rdflag=1;
    strbuf[i++]=c;GETC(value(standard_input),fp);
    t=(int)vecelm(value(readtable),c=PEEKC(value(standard_input),fp));
  }
  if(!(t&DIGBIT)) goto issy;
  for(d=0;(t=(int)vecelm(value(readtable),c=PEEKC(value(standard_input),fp)))&DIGBIT;){
    strbuf[i++]=c;GETC(value(standard_input),fp);
    d=d*10+(c-'0');
#ifndef NO_BIGNUM
    if(d>(MAXFIX+1) || (d==(MAXFIX+1) && !rdflag)){
      while((t=(int)vecelm(value(readtable),c=PEEKC(value(standard_input),fp)))&DIGBIT){
	strbuf[i++]=c;GETC(value(standard_input),fp);
#if 0
	if(i>=STRBUFSIZE)bufferr(fp);
#else
	if(i>=strbufsize){
	  newbuf=(unsigned char *)alloca(strbufsize*2);
	  for(j=0;j<strbufsize;j++)newbuf[j]=strbuf[j];
	  strbufsize*=2;
	  strbuf=newbuf;
	}
#endif
      }
#ifndef NO_FLONUM
      if((t&POINTBIT)||(t&EXPNTBIT)){
	for(d=(rdflag?1:0),fval=0.0;d<i;d++) /* tanaka 1989/4/1 */
	  fval=fval*10.0+(double)(strbuf[d]-'0');
	goto floent;
      }
#endif
      return dig2big(strbuf,i,value(readtable),fp);
    }
#endif
  }
  if(t&TERMBIT){ /* fixnumだったわけだ */
    if(rdflag)return itofix(-d);
    else return itofix(d);
  }
#ifndef NO_FLONUM
  fval=(double)d;
 floent:
  if(t&POINTBIT){
    frac=0.1;
    strbuf[i++]=c;GETC(value(standard_input),fp);
#if 0
    if(i>=STRBUFSIZE)bufferr(fp);
#else
    if(i>=strbufsize){
      newbuf=(unsigned char *)alloca(strbufsize*2);
      for(j=0;j<strbufsize;j++)newbuf[j]=strbuf[j];
      strbufsize*=2;
      strbuf=newbuf;
    }
#endif
    while((t=(int)vecelm(value(readtable),c=PEEKC(value(standard_input),fp)))&DIGBIT){
      strbuf[i++]=c;GETC(value(standard_input),fp);
#if 0
      if(i>=STRBUFSIZE)bufferr(fp);
#else
      if(i>=strbufsize){
	newbuf=(unsigned char *)alloca(strbufsize*2);
	for(j=0;j<strbufsize;j++)newbuf[j]=strbuf[j];
	strbufsize*=2;
	strbuf=newbuf;
      }
#endif
      fval+=frac*(double)(c-'0');
      frac/=10.0;
    }
    if(t&TERMBIT){
      if(rdflag)
	return alflonum(-fval,fp);
      return alflonum(fval,fp);
    }
  }
  if(t&EXPNTBIT){
    strbuf[i++]=c;GETC(value(standard_input),fp);
#if 0
    if(i>=STRBUFSIZE)bufferr(fp);
#else
    if(i>=strbufsize){
      newbuf=(unsigned char *)alloca(strbufsize*2);
      for(j=0;j<strbufsize;j++)newbuf[j]=strbuf[j];
      strbufsize*=2;
      strbuf=newbuf;
    }
#endif
    if((t=(int)vecelm(value(readtable),c=PEEKC(value(standard_input),fp)))&SIGNBIT){
      if(t&ALTBIT)rdflag|=2;
      strbuf[i++]=c;GETC(value(standard_input),fp);
#if 0
      if(i>=STRBUFSIZE)bufferr(fp);
#else
      if(i>=strbufsize){
	newbuf=(unsigned char *)alloca(strbufsize*2);
	for(j=0;j<strbufsize;j++)newbuf[j]=strbuf[j];
	strbufsize*=2;
	strbuf=newbuf;
      }
#endif
      t=(int)vecelm(value(readtable),c=PEEKC(value(standard_input),fp));
    }
    if(!(t&DIGBIT))goto issy;
    for(d=0;(t=(int)vecelm(value(readtable),c=PEEKC(value(standard_input),fp)))&DIGBIT;){
      strbuf[i++]=c;GETC(value(standard_input),fp);
#if 0
      if(i>=STRBUFSIZE)bufferr(fp);
#else
      if(i>=strbufsize){
	newbuf=(unsigned char *)alloca(strbufsize*2);
	for(j=0;j<strbufsize;j++)newbuf[j]=strbuf[j];
	strbufsize*=2;
	strbuf=newbuf;
      }
#endif
      d=d*10+(c-'0');
    }
    if(!(t&TERMBIT))goto issy;
    if(rdflag&2) /* negative expt part */
      while(d-->0)fval/=10.0;
    else
      while(d-->0)fval*=10.0;
    if(rdflag&1)
      return alflonum(-fval,fp);
    return alflonum(fval,fp);
  }
#endif /* NO_FLONUM */
 issy:
  while(!(t&TERMBIT)){
    if(t&ESCBIT){
      GETC(value(standard_input),fp);c=PEEKC(value(standard_input),fp);
    }
    strbuf[i++]=c;GETC(value(standard_input),fp);
#if 0
    if(i>=STRBUFSIZE)bufferr(fp);
#else
    if(i>=strbufsize){
      newbuf=(unsigned char *)alloca(strbufsize*2);
      for(j=0;j<strbufsize;j++)newbuf[j]=strbuf[j];
      strbufsize*=2;
      strbuf=newbuf;
    }
#endif
    t=(int)vecelm(value(readtable),c=PEEKC(value(standard_input),fp));
  }
  strbuf[i]=0;
  l(0)=alnstring((char*)strbuf,i,fp);
  return funcall(value(intern_v),1,fp-1);
}


/* pnameを名前とするシンボルを作り、それをinternする */

WORD intern(char *pname, int len, WORD *fp)
{
  return internsym(alsymbol(alnstring(pname,len,fp),fp),fp);
}
/* pnameを名前とするシンボルを作り、それをinternする */

WORD intern_soft(char *pname, int len, WORD *fp)
{
  int hval;
  WORD a,d=nil,dd,vec;

  checkvec(vec,value(obvector));
  hval=hashstr(pname,len)%(fixtoi(osize(vec)));
  if(atom(d= vecelm(vec,hval)))goto notfound;
  while(!atom(d)){
    if(tag(a=car(d))==SYMBOL){
      if(osize(prname(a))==itofix(len) && !memcmp(stringcodes(prname(a)),pname,len)){
	return d;
      }
    }
    dd=d;
    d=cdr(d);
  }
notfound:
  return nil;
}

/* すでに出来ているシンボルをinternする */

WORD internsym(WORD sym, WORD *fp)
{
  int hval,ilen;
  WORD a,d,dd,vec,str,len,str1;
  WORD hl;

  str=prname(checksym(sym,sym));
  ilen=fixtoi(len=osize(str));
  checkvec(vec,value(obvector));
  hval=hashstr((char *)stringcodes(str),fixtoi(osize(str)))%(fixtoi(osize(vec)));
  if(atom(d= vecelm(vec,hval)))goto notfound;
  while(tag(d)==CONS){
    if(atom(a=car(d))){
      str1=prname(checksym(a,a));
      if(osize(str1)==len &&
	 !memcmp(stringcodes(str1),stringcodes(str),ilen))
	return a;
    }
    dd=d;
    d=cdr(d);
  }
  l(0)=dd;
  l(1)=sym;
  hl=alcons(sym,nil,fp-2);
  cdr(l(0))=hl;
  return l(1);
notfound:
  l(0)=vec;
  l(1)=sym;
  hl=alcons(sym,nil,fp-2);
  vecelm(l(0),hval)=hl;
  return l(1);
}
