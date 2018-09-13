/* @(#)sysfnstg.c	1.6 90/07/30 */

#include "constdef.h"
#include "defvar.h"

WORD character_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  return checkchar(a,ag(0));
}

WORD string_f(int na, WORD *fp)
{
  WORD a;
  int t;
  unsigned char buf[2];

  if(na!=1)parerr();
  if((t=tag(a=ag(0)))==SYMBOL)
    return prname(a);
  else if(t==FIXNUM && 0<=fixtoi(a) && fixtoi(a)<=255){
    buf[0]=fixtoi(a);
    buf[1]=0;
    return alnstring(buf,1,fp);
  }
  else if(t==OTHERS && otag(a)==STRING)
    return a;
  else
    nosymerr(a);
}

WORD string_length_f(int na, WORD *fp)
{
  WORD a;

  if(na!=1)parerr();
  checkstr(a,ag(0));
  return osize(a);
}

WORD string_lessp_f(int na, WORD *fp)
{
  WORD a,b;
  int len0,len1;

  if(na!=2)parerr();
  len0=fixtoi(osize(checkstr(a,ag(0))));
  len1=fixtoi(osize(checkstr(b,ag(1))));
  if(len0<=len1){
    if(strncmp(stringcodes(a),stringcodes(b),len0)>0)return true;
    else return nil;
  }
  else{
    if(strncmp(stringcodes(a),stringcodes(b),len1)>=0)return true;
    else return nil;
  }
}

WORD substring_f(int na, WORD *fp)
{
  WORD str,a,b;
  int start=0,end,newend,len;

  switch(na){
  case 3:
    checkstr(str,ag(2));
    end=fixtoi(osize(str));
    if((newend=fixtoi(slow_checkfix(a,ag(0))))>end || newend<0)
      inderr(itofix(newend),fp);
    end=newend;
    if((start=fixtoi(slow_checkfix(a,ag(1))))<0 || start>end)
      inderr(itofix(start),fp);
    goto label;
  case 2:
    checkstr(str,ag(1));
    end=fixtoi(osize(str));
    if((start=fixtoi(slow_checkfix(a,ag(0))))<0 || start>end)
      inderr(itofix(start),fp);
    goto label;
  case 1:
    checkstr(str,ag(0));
    end=fixtoi(osize(str));
    if(end<0) 
      inderr(itofix(end),fp);
  label:
    len=end-start;
    l(0)=str;
    b=alstrspace(len,fp-1);
    memcpy(stringcodes(b),stringcodes(l(0))+start,len);
    return b;
  default:
    parerr();
  }
}

WORD string_append_f(int na, WORD *fp)
{
  WORD *tmpstack=fp+na,a,rval;
  int i,len;
  unsigned char *str;

  for(i=0,len=0;i<na;i++){
    checkstr(a,*--tmpstack);
    len+=fixtoi(osize(a));
  }
  rval=alstrspace(len,fp);
  tmpstack=fp+na;
  for(i=0,str=stringcodes(rval);i<na;i++,str+=len){
    len=fixtoi(osize(checkstr(a,*--tmpstack)));
    memcpy(str,stringcodes(a),len);
  }
  return rval;
}

WORD string_reverse_f(int na, WORD *fp)
{
  WORD a,b;
  int i,len;

  if(na!=1)parerr();
  checkstr(a,ag(0));
  ag(0)=a;
  b=alstrspace(len=fixtoi(osize(a)),fp);
  a=ag(0);
  for(i=0;i<len;i++)
    *(stringcodes(b)+len-i-1)= *(stringcodes(a)+i);
  return b;
}

WORD string_nreverse_f(int na, WORD *fp)
{
  int i,j,len,tmp;
  unsigned char *str;
  WORD a;

  if(na!=1)parerr();
  checkstr(a,ag(0));
  str=stringcodes(a);
  len=fixtoi(osize(a));
  for(i=0,j=len-1;i<j;i++,j--){
    tmp=str[i];
    str[i]= str[j];
    str[j]= tmp;
  }
  return a;
}

static int string_search_char(unsigned char *str, int c, int from, int end)
{
  int i;

  for(str+=i=from;i<end;i++)
    if(*str++ ==c)return i;
  return -1;
}

WORD string_search_char_f(int na, WORD *fp)
{
  int from=0,minindex,index,end;
  unsigned char ch,*ptr;
  WORD str,a,c;

  switch(na){
  case 3:
    if((from=fixtoi(slow_checkfix(a,ag(0))))<0)
      inderr(itofix(from),fp);
    fp++;
  case 2:
    ptr=stringcodes(checkstr(str,ag(0)));
    end=fixtoi(osize(str));
    if(tag(c=ag(1))==CONS){
      for(minindex= 1000000;tag(c)==CONS;c=cdr(c)){
	ch=fixtoi(checkchar(a,car(c)));
	if((index=string_search_char(ptr,ch,from,end))>=0 && index<minindex)
	  minindex=index;
      }
    }
    else{
      ch=fixtoi(checkchar(c,c));
      minindex=string_search_char(ptr,ch,from,end);
    }
    break;
  default:
    parerr();
  }
  if(minindex<0)return nil;
  return itofix(minindex);
}

static int string_search_not_char(unsigned char *str, int c, int from, int end)
{
  int i;

  for(str+=i=from;i<end;i++)
    if(*str++ !=c)return i;
  return -1;
}

WORD string_search_not_char_f(int na, WORD *fp)
{
  int from=0,mindex,end;
  unsigned char ch,*ptr;
  WORD str,a,c;

  switch(na){
  case 3:
    slow_checkfix(a,ag(0));
    fp++;
    if((from=fixtoi(a))<0)
      inderr(itofix(from),fp);
  case 2:
    checkstr(str,ag(0));
    ptr=stringcodes(str);
    end=fixtoi(osize(str));
    if(tag(c=ag(1))==CONS){
      ch=fixtoi(checkchar(a,car(c)));
      mindex=string_search_not_char(ptr,ch,from,end);
    }
    else{
      ch=fixtoi(checkchar(c,c));
      mindex=string_search_not_char(ptr,ch,from,end);
    }
    break;
  default:
    parerr();
  }
  if(mindex<0)return nil;
  return itofix(mindex);
}

WORD string_search_f(int na, WORD *fp)
{
  int from=0,len,end;
  unsigned char *src,*dst;
  WORD str,a;

  switch(na){
  case 3:
    if((from=fixtoi(slow_checkfix(a,ag(0))))<0)
      inderr(itofix(from),fp);
    fp++;
  case 2:
    checkstr(str,ag(0));
    dst=stringcodes(str);
    end=fixtoi(osize(str));
    checkstr(str,ag(1));
    src=stringcodes(str);
    len=fixtoi(osize(str));
    for(dst+=from;from+len<=end;from++,dst++)
      if(!memcmp(src,dst,len))
	return itofix(from);
    return nil;
  default:
    parerr();
  }
}

WORD getchar_f(int na, WORD *fp)
{
  int index;
  WORD a;

  if(na!=2)parerr();
  index=fixtoi(slow_checkfix(a,ag(0)));
  checkstr(a,ag(1));
  return intern(stringcodes(a)+index,1,fp);
}

WORD sref_f(int na, WORD *fp)
{
  WORD string,index;

  if(na!=2)parerr();
  slow_checkfix(index,ag(0));
  checkstr(string,ag(1));
  if((int)index<0 || (int)index>=(int)osize(string))inderr(index,fp);
  return itofix(*(stringcodes(string)+fixtoi(index)));
}

WORD sset_f(int na, WORD *fp)
{
  WORD string,index,character;

  if(na!=3)parerr();
  checkchar(character,ag(0));
  slow_checkfix(index,ag(1));
  checkstr(string,ag(2));
  if((int)index<0 || (int)index>=(int)osize(string))inderr(index,fp);
  return itofix(*(stringcodes(string)+fixtoi(index))=fixtoi(character));
}

WORD string_equal_f(int na, WORD *fp)
{
  WORD a,b;
  int len;

  if(na!=2)parerr();
  checkstr(a,ag(0));
  checkstr(b,ag(1));
  if((len=fixtoi(osize(a)))!=fixtoi(osize(b)))return nil;
  if(memcmp(stringcodes(a),stringcodes(b),len))return nil;
  return true;
}

WORD cutout_f(int na, WORD *fp)
{
  WORD len,pos,str,a;
  int ll,wl,i,j,slen;
  unsigned int val;
  unsigned char *p;

  if(na!=3)parerr();
  slow_checkfix(len,ag(0));
  slow_checkfix(pos,ag(1));
  checkstr(str,ag(2));
  slen=fixtoi(osize(str));
  if((ll=fixtoi(len))<0 || ll+fixtoi(pos)>slen)
    inderr(len,fp);
#ifdef NO_BIGNUM
  if(ll>3)inderr(len,fp);
  for(val=0,p=stringcodes(str)+fixtoi(pos);ll>0;ll--)
    val=((val<<8)| *p++);
  return itofix(val);
#else
  if(ll<=4){
    for(val=0,p=stringcodes(str)+fixtoi(pos);ll>0;ll--)
      val=((val<<8)| *p++);
    if(val<=MAXFIX)
      return itofix(val);
    if((int)val>=0){
      a=albignum(1,fp);
    }
    else{
      a=albignum(2,fp);
      bigcodes(a,1)=0;
    }
    bigcodes(a,0)=val;
    return a;
  }
  l(0)=str;
/* wl=(ll+4 ( not 3)) /4; because return value must be positive */
  a=albignum((wl=(ll+4)/4),fp-1);
  str=l(0);
  p=stringcodes(str)+fixtoi(pos)+ll-1;
  for(i=0;i<wl-1;i++){
    for(val=0,j=0;j<32;j+=8)
      val|=(*p--)<<j;
    bigcodes(a,i)=val;
  }
  for(j=val=0,ll&=3;ll>0;ll--,j+=8)
    val|=(*p--)<<j;
  bigcodes(a,i)=val;
  return normbig(a);
#endif /* NO_BIGNUM */
}

WORD spread_f(int na, WORD *fp)
{
  WORD len,val,str;
  int ll,v,vlen,j,k,t;
  unsigned char *p;

  if(na!=2)parerr();
  slow_checkfix(len,ag(0));
  if((ll=fixtoi(len))<0) inderr(len,fp);
  str=alnstring("",ll,fp);
  if((t=tag(val=ag(1)))==FIXNUM){
    for(v=fixtoi(val),p=stringcodes(str)+ll-1;ll>0;ll--,p--,v>>=8)
      *p=v;
    return str;
  }
#ifdef NO_BIGNUM
  nofixerr(val);
#else
  else if(t!=OTHERS) nofixerr(val);
  else if(otag(val)!=BIGNUM) nofixerr(val);
  vlen=fixtoi(osize(val));
  v=bigcodes(val,0);
  p=stringcodes(str)+ll-1;
  for(j=0,k=0;ll>0;ll--,p--){
    *p=v;
    if(++k==4){
      if(++j>=vlen) v=0;
      else v=bigcodes(val,j);
      k=0;
    }
    else v>>=8;
  }
  return str;
#endif
}

WORD translate_f(int na, WORD *fp)
{
  WORD tab,str;
  unsigned char *tabp,*strp;
  int len;

  if(na!=2)parerr();
  checkstr(tab,ag(0));
  checkstr(str,ag(1));
  if(osize(tab)!=itofix(256))nosymerr(tab);
  tabp=stringcodes(tab);
  strp=stringcodes(str);
  len=fixtoi(osize(str));
  for(;len-->0;strp++)
    *strp= tabp[*strp];
  return str;
}

WORD string_amend_f(int na, WORD *fp)
{
  int from=0,len;
  WORD a,src,dst;

  switch(na){
  case 3:
    from=fixtoi(slow_checkfix(a,ag(0)));
    fp++;
  case 2:
    src=checkstr(src,ag(0));
    dst=checkstr(dst,ag(1));
    if(from<0 || (len=fixtoi(osize(src)))+from>fixtoi(osize(dst)))
      inderr(itofix(from),fp);
    memmove(stringcodes(dst)+from,stringcodes(src),len);
    return dst;
  default:
    parerr();
  }
}

void memcpy_or(unsigned char *to, unsigned char *from, int size)
{
  while(size-->0)
    *to++|= *from++;
}

WORD string_amend_or_f(int na, WORD *fp)
{
  int from=0,len;
  WORD a,src,dst;

  switch(na){
  case 3:
    from=fixtoi(slow_checkfix(a,ag(0)));
    fp++;
  case 2:
    src=checkstr(src,ag(0));
    dst=checkstr(dst,ag(1));
    if(from<0 || (len=fixtoi(osize(src)))+from>fixtoi(osize(dst)))
      inderr(itofix(from),fp);
    memcpy_or(stringcodes(dst)+from,stringcodes(src),len);
    return dst;
  default:
    parerr();
  }
}

void memcpy_xor(unsigned char *to, unsigned char *from, int size)
{
  while(size-->0)
    *to++^= *from++;
}

WORD string_amend_xor_f(int na, WORD *fp)
{
  int from=0,len;
  WORD a,src,dst;

  switch(na){
  case 3:
    from=fixtoi(slow_checkfix(a,ag(0)));
    fp++;
  case 2:
    src=checkstr(src,ag(0));
    dst=checkstr(dst,ag(1));
    if(from<0 || (len=fixtoi(osize(src)))+from>fixtoi(osize(dst)))
      inderr(itofix(from),fp);
    memcpy_xor(stringcodes(dst)+from,stringcodes(src),len);
    return dst;
  default:
    parerr();
  }
}

void memcpy_and(unsigned char *to, unsigned char *from, int size)
{
  while(size-->0)
    *to++&= *from++;
}

WORD string_amend_and_f(int na, WORD *fp)
{
  int from=0,len;
  WORD a,src,dst;

  switch(na){
  case 3:
    from=fixtoi(slow_checkfix(a,ag(0)));
    fp++;
  case 2:
    src=checkstr(src,ag(0));
    dst=checkstr(dst,ag(1));
    if(from<0 || (len=fixtoi(osize(src)))+from>fixtoi(osize(dst)))
      inderr(itofix(from),fp);
    memcpy_and(stringcodes(dst)+from,stringcodes(src),len);
    return dst;
  default:
    parerr();
  }
}
WORD make_string_f(int na, WORD *fp)
{
  int ch=0,ll;
  WORD c,len,ptr;
  unsigned char *p;

  switch(na){
  case 2:
    ch=fixtoi(checkchar(c,ag(0)));
    fp++;
  case 1:
    ll=fixtoi(slow_checkfix(len,ag(0)));
    if(ll<0) nosymerr(len);
    ptr=alstrspace(ll,fp);
    for(p=stringcodes(ptr);ll-->0;)*p++ =ch;
    break;
  default:
    parerr();
  }
  return ptr;
}

WORD bset_f(int na, WORD *fp)
{
  WORD index,string,c;
  int ind;
  unsigned char *str;

  if(na!=3)parerr();
  c=ag(0);
  ind=fixtoi(slow_checkfix(index,ag(1)));
  str=stringcodes(checkstr(string,ag(2)));
  if(ind<0 || ind>=fixtoi(osize(string))*8)inderr(itofix(ind),fp);
  if(c==nil)*(str+(ind/8))&=~(0x80>>(ind&7));
  else *(str+(ind/8))|=(0x80>>(ind&7));
  return c;
}

WORD bref_f(int na, WORD *fp)
{
  WORD index,string;
  int ind;
  unsigned char *str;

  if(na!=2)parerr();
  ind=fixtoi(slow_checkfix(index,ag(0)));
  str=stringcodes(checkstr(string,ag(1)));
  if(ind<0 || ind>=fixtoi(osize(string))*8)inderr(itofix(ind),fp);
  if(*(str+(ind/8))&(0x80>>(ind&7)))return true;
  return nil;
}

static int strmatch(unsigned char *exp, int explen, unsigned char *str, int stlen)
{
  int i;

  if(explen==0){
    if(stlen==0) return 1;
    else return 0;
  }
  switch(*exp){
  case '*':
    for(i=0;i<=stlen;i++)
      if(strmatch(exp+1,explen-1,str+i,stlen-i))return 1;
    return 0;
  case '?':
    if(stlen==0) return 0;
    return strmatch(exp+1,explen-1,str+1,stlen-1);
  default:
    if(stlen==0) return 0;
    if(*exp== *str)return strmatch(exp+1,explen-1,str+1,stlen-1);
    return 0;
  }
}

WORD string_match_f(int na, WORD *fp)
{
  WORD a,b;
  unsigned char *str,*exp;

  if(na!=2)parerr();
  str=stringcodes(checkstr(a,ag(0)));
  exp=stringcodes(checkstr(b,ag(1)));
  if(strmatch(exp,fixtoi(osize(b)),str,fixtoi(osize(a))))return true;
  return nil;
}

