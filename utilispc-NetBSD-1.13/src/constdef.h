/* @(#)constdef.h	1.6 7/30/90 */
#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>

#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#include <sys/times.h>
#include <signal.h>
#include <setjmp.h>

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <sys/syscall.h>
#include <ctype.h>
#include <string.h>

#ifdef HAVE_VFORK_H
#include <vfork.h>
#endif

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif

#ifdef HAVE_IEEE_HANDLER
#include <floatingpoint.h>
#endif

typedef char *WORD;
typedef WORD (* FP)();

#if defined(AP1000) && !defined(APLIB)
#define open sys_open
#define read sys_read
#define write sys_write
#define close sys_close
#define chdir sys_chdir
#define getenv sys_getenv
#define time sys_time 
#define getpid sys_getpid
#define putenv sys_putenv
#define signal sys_signal
#define syscall sys_syscall
#define localtime sys_localtime
#define times sys_times
#define sigsetmask(x) 
#endif

#ifndef _setjmp
#define _setjmp setjmp
#endif

#ifndef _longjmp
#define _longjmp longjmp
#endif

/*  C-Utilisp におけるデータの内部表現   */
#define TAGBIT 3

#define tag(l) ((int)(l)&3)
#define tag3(l) ((int)(l)&7)

#define FIXNUM 0
#define SYMBOL 1
#define CONS 2
#define OTHERS 3

#define OTAGS 4
/*  1. 整数型(fixnum)                    */
/*  ポインタ                             */
/*  +--------------------------------+   */
/*  +                            0000+   */
/*  +--------------------------------+   */
/* 上位 28 bit に実体が入る              */

#define FIX1 (16)
#define itofix(i) (WORD)((i)<<4)
#define fixtoi(f) ((int)(f)>>4)
#define MAXFIX (256*256*256*8-1)
#define MINFIX (-256*256*256*8)

#ifdef TAGGED_ARITHMETIC
#ifdef __GNUC__
__inline static const WORD tadd(WORD a,WORD b)
{
  WORD value;

  __asm("taddcctv %2,%1,%0" 
	: "=r" (value)
	: "r" (a) 
	, "r" (b) : "cc","g2");
  return value;
}
__inline static const WORD tsub(WORD a,WORD b)
{
  WORD value;

  __asm("tsubcctv %2,%1,%0" 
	: "=r" (value)
	: "r" (b) 
	, "r" (a) : "cc","g2");
  return value;
}
#else __GNUC__
WORD tadd(),tsub();
#endif __GNUC__
#else /* TAGGED_ARITHMETIC */
#define tadd(a,b) (WORD)((int)a+(int)b)
#define tsub(a,b) (WORD)((int)a-(int)b)
#endif /* TAGGED_ARITHMETIC */


/*  2. シンボルアトム(symbol)            */
/*  ポインタ                             */
/*  +--------------------------------+   */
/*  +                              01+   */
/*  +--------------------------------+   */
/*  本体                                 */
/*  +--------------------------------+   */
/*  +          値セル                +   */
/*  +--------------------------------+   */
/*  +          表示名                +   */
/*  +--------------------------------+   */
/*  +          属性リスト            +   */
/*  +--------------------------------+   */
/*  +          関数定義              +   */
/*  +--------------------------------+   */


#define VALUE -1
#define PNAME 3
#define PLIST 7
#define DEFINITION 11

#define value(l) (*(WORD *)((l)+VALUE))
#define prname(l) (*(WORD *)((l)+PNAME))
#define plist(l) (*(WORD *)((l)+PLIST))
#define definition(l) (*(WORD *)((l)+DEFINITION))

/* 便宜上、未束縛な値セルには次の値を入れる */
#define UNBOUND ((WORD)0xfffffff1)

#define SYMBOLSIZE 4

/*  3. コンスセル(cons)                  */
/*  ポインタ                             */
/*  +--------------------------------+   */
/*  +                              10+   */
/*  +--------------------------------+   */
/*  本体                                 */
/*  +--------------------------------+   */
/*  +          car部                 +   */
/*  +--------------------------------+   */
/*  +          cdr部                 +   */
/*  +--------------------------------+   */

#define CDR 2
#define CAR -2

#define car(l) (*(WORD *)((l)-2))
#define cdr(l) (*(WORD *)((l)+2))
#define CONSSIZE 2

/*  4. それ以外のデータ型                */
/*  ポインタ                             */
/*  +--------------------------------+   */
/*  +                              11+   */
/*  +--------------------------------+   */
/*  本体                                 */
/*  +--------------------------------+   */
/*  |         オブジェクタグ      100+   */
/*  +--------------------------------+   */
/*  |      サイズ(可変長の場合)      +   */
/*  +--------------------------------+   */
/*  |                                |   */

#define TAGPOS -3
#define BLOCKSIZE 1

#define otag(l) (*(int *)((l)+TAGPOS))
#define osize(l) (*(WORD *)((l)+BLOCKSIZE))

#ifndef NO_FLONUM
#define FLONUM 4
#endif

#ifndef NO_BIGNUM
#define BIGNUM 12
#endif

#define VECTOR 28
#define STRING 36
#define STREAM 44
#define CODE 52
#ifdef OBJECTSPECIAL
#define SPECIAL 60
#endif

#ifndef NO_FLONUM
/*  4.1 浮動小数点                       */
/* 本体                                  */
/*  +--------------------------------+   */
/*  |                          000100|   */
/*  +--------------------------------+   */
/*  |  double 1ワード目              +   */
/*  +--------------------------------+   */
/*  |  double 2ワード目              |   */
/*  +--------------------------------+   */

#define FLOVAL 1
#if defined(__GNUC__) && defined(sparc)
static inline double floval(WORD addr)
{
  double val;
  asm volatile( "ld %1,%0\n\tld %2,%R0" :  "=f"(val) 
        : "m"(*(int *)(addr+FLOVAL)), "m"(*(int *)(addr+FLOVAL+4))); 
  return val;
}
static inline store_floval(WORD addr,double val)
{
  asm volatile( "st %2,%0\n\tst %R2,%1" : :
	       "m"(*(int *)(addr+FLOVAL)),"m"(*(int *)(addr+FLOVAL+4)),
               "f"(val));
}
#else
#define floval(l) (*(double *)((l)+FLOVAL))
#define store_floval(addr,val) (*(double *)((addr)+FLOVAL)=val)
#endif
#define FLOSIZE 3
#endif /* NO_FLONUM */

#ifndef NO_BIGNUM
/*  4.2 BIGNUM                           */
/* 本体                                  */
/*  +--------------------------------+   */
/*  |                          001100|   */
/*  +--------------------------------+   */

#define BIGCODES 5
#define bigcodes(l,i) (*(unsigned int *)((l)+BIGCODES+(i)*4))

#define SIGNMASK 0x80000000
#define BIGSIZE 2
#endif

#define deref(l) (*(WORD *)((l)+TAGPOS))
#define reference(vec,index) (WORD)((vec)+8+(index)*4)

/*  4.4 ベクター                         */
/* 本体                                  */
/*  +--------------------------------+   */
/*  |                          011100|   */
/*  +--------------------------------+   */
/*  | 配列の長さ(fixnum)         0000|   */
/*  +--------------------------------+   */
/*  |    配列要素(index 0)           |   */
/*  +--------------------------------+   */
/*  |    配列要素(index 1)           |   */
/*  |    ....                        |   */

#define VECELM 5
#define vecelm(l,i) (*(WORD *)((l)+VECELM+(unsigned int)(i)*4))
#define VECSIZE 2

/*  4.5 文字列                           */
/* 本体                                  */
/*  +--------------------------------+   */
/*  |                          100100|   */
/*  +--------------------------------+   */
/*  | 文字列の長さ(fixnum)           |   */
/*  +--------------------------------+   */
/*  | 文字0 | 文字1 | 文字2 | 文字3  |   */
/*  +--------------------------------+   */
/*  | ....                           |   */
/*  | ....          | NUL   |        |   */
/*   文字列の長さ ->                     */
/* 途中で NUL 文字(0x00)を含むことが可能 */
/* 文字列の長さ + 1 の領域を確保し，     */
/* 最後に NUL 文字を入れる(デバッグ用)   */

#define STRINGCODES 5
#define stringcodes(l) ((unsigned char *)((l)+STRINGCODES))
#define STRINGSIZE 2

/*  4.6 ストリーム                       */
/* 本体                                  */
/*  +--------------------------------+   */
/*  |                          101100|   */
/*  +--------------------------------+   */
/*  | サイズ(fixnum,unused)      0000|   */
/*  +--------------------------------+   */
/*  | ファイル名(Lisp string)      11|   */
/*  +--------------------------------+   */
/*  | カーソルポジション(C int)      |   */
/*  +--------------------------------+   */
/*  | レコード終端インデックス(C int)|   */
/*  +--------------------------------+   */
/*  | モード(C int)                  |   */
/*  +--------------------------------+   */
/*  | 行番号(C int)                  |   */
/*  +--------------------------------+   */
/*  | ファイルディスクリプタ(C int)  |   */
/*  +--------------------------------+   */
/*  | バッファ(512バイト)            |   */
/*     ...                               */
/*  |                                |   */
/*  +--------------------------------+   */


#define FILNAME 5 /* if string stream this field is the string */
#define CURPOS 9
#define RECEND 13
#define MODE 17
#define LINEIO 21
#define FILDES 25
#define BUFTOP 29
#define BUFSIZE 512
#define filname(l) (*(WORD *)((l)+FILNAME))
#define fildes(l) (*(int *)((l)+FILDES))
#define curpos(l) (*(int *)((l)+CURPOS))
#define recend(l) (*(int *)((l)+RECEND))
#define buftop(l) ((unsigned char *)((l)+BUFTOP))
#define peekch(l) (*(buftop(l)+curpos(l)))
#define mode(l) (*(int *)((l)+MODE))
#define STREAMSIZE 136

#define NOTOPEN 0
#define INOPEN 1
#define OUTOPEN 2
#define STRINGSTREAM 4
/*  4.7 コード                           */
/* 本体                                  */
/*  +--------------------------------+   */
/*  |                          110100|   */
/*  +--------------------------------+   */
/*  | リテラル領域サイズ         0000|   */
/*  +--------------------------------+   */
/*  | 関数名(Lisp Symbol)          01|   */
/*  +--------------------------------+   */
/*  | コードトップ(C 関数ポインタ)   |   */
/*  +--------------------------------+   */
/*  | 最小引数サイズ(Fixnum)     0000|   */
/*  +--------------------------------+   */
/*  | 最大引数サイズ(Fixnum)     0000|   */
/*  +--------------------------------+   */
/*  | リテラル 0                     |   */
/*  +--------------------------------+   */
/*  | リテラル 1                     |   */
/*  | ...                            |   */

#define FUNCNAME 5
#define CODETOP 9
#define MINARG 13
#define MAXARG 17
#define LITERAL 21
#define CODESIZE 6

#define funcname(l) (*(WORD *)((l)+FUNCNAME))
#define codetop(l) (*(FP *)((l)+CODETOP))
#define minarg(l) (*(WORD *)((l)+MINARG))
#define maxarg(l) (*(WORD *)((l)+MAXARG))
#define literal(l,i) (*(WORD *)((l)+LITERAL+i*4))

/*  4.8 スペシャル                       */
/* Cの関数の先頭アドレスの下位2ビットが  */
/* 必ず 00 で etext より若いアドレスに   */
/* 他の Lisp オブジェクトが来ない時は    */
/* Lisp オブジェクトにしない             */

#ifdef OBJECTSPECIAL
/* 本体                                  */
/*  +--------------------------------+   */
/*  |                          111100|   */
/*  +--------------------------------+   */
/*  | Cの関数の先頭アドレス          |   */
/*  +--------------------------------+   */
#define SPECTOP 1
#define spectop(l) (*(FP *)((l)+SPECTOP)) 
#define SPECSIZE 2
#endif

#define code(l) (tag(l)==OTHERS && *(int *)((l)-3)==CODE)
#define atom(l) (((int)(l)&3)!=CONS)
#define consp(l) (((int)(l)&3)==CONS)
#define symbol(l) (((int)(l)&3)==SYMBOL)
#ifdef OBJECTSPECIAL
#define spec(l) (tag(l)==OTHERS && *(int *)((l)-3)==SPECIAL)
#else
#define spec(l) (tag(l)==OTHERS && (int *)(l)<&etext)
#endif

/*               スタック操作マクロ               */
#define enter(n) (sp=fp-(n))
#define push(l) (*--sp=(l))
#define pop() (*fp++)
#define see() (*fp)
#define discard() (fp++)
#define l(n) *(fp-(n+1))
#define ag(n) *(fp+(n))

#ifdef INVALID_ACCESS_JUMP
#define stack_overflow() value(UNBOUND)=0
#else
#define stack_overflow() sigill()
#endif

#ifdef NO_MMAP_STACK_CHECK
#define pushb(l) (bstack<=bstacktop?(WORD)stack_overflow():(*--bstack=(l)))
#else
#define pushb(l) (*--bstack=(l))
#endif
#define popb() (*bstack++)
#define discardb() (bstack++)
#define bind(sym,val) (pushb(sym),pushb(value(sym)),value(sym)=val)
#ifdef NO_MMAP_STACK_CHECK
#define checkb(n) ((bstack-n*2)<=bstacktop ? stack_overflow() : 0)
#else
#define checkb(n)
#endif

#ifdef NO_MMAP_STACK_CHECK
#define pushcb(l) (cstack<=cstacktop?(WORD)stack_overflow():(*--cstack=(l)))
#else
#define pushcb(l) (*--cstack=(l))
#endif
#define discardcb() (cstack++)

#ifdef NO_MMAP_STACK_CHECK
#define pushe() (--estack<=estacktop ?  stack_overflow() : 0)
#else
#define pushe() (--estack)
#endif


/* エラー処理を呼ぶマクロ */

#ifdef ALIGN_TYPE_CHECK
#define nofixerr(l) (*(int *)l=0)
#define nosymerr(l) (value(l)=(WORD)0)
#define noconserr(l) (car(l)=(WORD)0)
#define nooterr(l) (otag(l)=0)
#else /* ALIGN_TYPE_CHECK */
#define nofixerr typeerr1
#define nosymerr typeerr1
#define noconserr typeerr1
#define nooterr typeerr1
#endif /* ALIGN_TYPE_CHECK */

#ifdef INVALID_ACCESS_JUMP
#define PARERR 0xfffffff8
#define parerr() (*(int *)PARERR=0)
#endif


/*               コアーションを行なうマクロ       */
#ifdef TAGGED_ARITHMETIC
#ifdef __GNUC__
#define avoid_g2() ({__asm("" : : : "g2");0;})
#else
#define avoid_g2() 0
#endif
#define slow_checkfix(x,y) (avoid_g2(),x=tsub(y,(WORD)0))
#define checkfix(x,y) (avoid_g2(),x=(y))

#else /* TAGGED_ARITHMETIC */

#define slow_checkfix(x,y) (tag(x=(y))!=FIXNUM?(nofixerr(x),x):x)
#define checkfix(x,y) (tag(x=(y))!=FIXNUM?(nofixerr(x),x):x)
#endif /* TAGGED_ARITHMETIC */

#ifdef ALIGN_TYPE_CHECK
#define checksym(x,y) (x=(y))
#define checkcons(x,y) (x=(y))
#define checkothers(x,y) (x=(y))
#else
#define checksym(x,y) (tag(x=(y))!=SYMBOL?(nosymerr(x),x):x)
#define checkcons(x,y) (tag(x=(y))!=CONS?(noconserr(x),x):x)
#define checkothers(x,y) (tag(x=(y))!=OTHERS?(nooterr(x),x):x)
#endif

#define checkstr(x,y) (tag(x=(tag(x=(y))==SYMBOL?prname(x):x))!=OTHERS\
			?(nooterr(x),x):(otag(x)!=STRING?(nofixerr(x),x):x))
#define checkchar(x,y) (tag(x=(y))!=FIXNUM?x=itofix(*stringcodes(checkstr(x,x)))\
			:((x<itofix(0)||x>itofix(255))?(nosymerr(x),x):x))
#define checkstream(x,y) (tag(x=(y))!=OTHERS?(nooterr(x),x):\
					(otag(x)!=STREAM?(nosymerr(x),x):x))
#define checkvec(x,y) (tag(x=(y))!=OTHERS?(nooterr(x),x):\
				(otag(x)!=VECTOR?(nosymerr(x),x):x))
#define checkref(x,y) (tag(x=(y))!=OTHERS?(nooterr(x),x):\
			(tag3(otag(x))==OTAGS?(nosymerr(x),x):x))
#define checkcode(x,y) (tag(x=(y))!=OTHERS?(nooterr(x),x):\
			(otag(x)!=CODE?(nosymerr(x),x):x))
#ifndef NO_FLONUM
#define checkflo(x,y) (tag(x=(y))!=OTHERS?(nooterr(x),x):\
			(otag(x)!=FLONUM?(nosymerr(x),x):x))
#ifdef NO_BIGNUM
#define coflo(x,y,fp) (tag(x=(y))==FIXNUM?(double)(fixtoi(x)):\
		(tag(x)!=OTHERS?(nooterr(x),0.0):\
		(otag(x)==FLONUM?floval(x):(nofixerr(x),0.0))))
#else /*  NO_BIGNUM */
#define coflo(x,y,fp) (tag(x=(y))==FIXNUM?(double)(fixtoi(x)):\
		(tag(x)!=OTHERS?(nooterr(x),0.0):\
		(otag(x)==FLONUM?floval(x):\
		(otag(x)==BIGNUM?bigtoflo(x):(nofixerr(x),0.0)))))
#endif /*  NO_BIGNUM */
#endif /* NO_FLONUM */

#ifndef NO_BIGNUM
#define checkbig(x,y) (tag(x=(y))!=OTHERS?(nooterr(x),x):\
			(otag(x)!=BIGNUM?(nosymerr(x),x):x))
#endif

struct lispenv {
  int envtag;
  WORD envobj;
  WORD *bstack;
  WORD *cstack;
  jmp_buf *envbuf;
};

#define EVAL(src,fp,a,d,t) \
  (((t=tag(a=(src)))==SYMBOL)? \
   (((d=value(a))==UNBOUND)?(ubverr(a,fp),a):d) : \
   (t==CONS)?eval(a,fp):a)


#ifndef HAVE_MEMMOVE
#ifdef HAVE_BCOPY
#define memmove(s1,s2,len) bcopy(s2,s1,len)
#else
void *memmove(void *s1, const void *s2, size_t n);
#endif
#endif
