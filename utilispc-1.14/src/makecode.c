/* %W% %G% */
/*
 * make & init. codepieces.
 */
#include "constdef.h"
#include "defvar.h"
#include "cmpdef.h"

WORD make_codepiece(struct code_inf **ca, WORD *fp)
{
  int		i, nl, n;
  WORD	c, a;
  
  n = 0;
  l(0)=l(1)=l(2)=l(3)=l(4)=l(5)=nil;
  while (ca[n]!=(struct code_inf *)0) {
    nl=ca[n]->c_nliteral;
    l(0)=c=alcode(nl,fp-5);
    codetop(c)=ca[n]->c_codetop;
    minarg(c)=itofix(ca[n]->c_minarg); 
    maxarg(c)=itofix(ca[n]->c_maxarg);
    l(4)=alstring(ca[n]->c_literal,fp-5);
    l(4)=l(3)=string_stream_f(1,fp-5);
    inopen_f(1,fp-5);
    funcname(l(0))=read_f(1,fp-5); /* save here tempolarily */
    for (i=0;i<nl;i++){  /* read literals */
      l(4)=l(3);	 /* 'read' breaks his arg, so, copy it everytime. */
      literal(l(0),i)=read_f(1,fp-5);
    }
    l(4)=l(3);
    close_f(1,fp-5);
    if(ismacro(ca[n])) c=alcons(macro,l(0),fp-4);
    else c=l(0);
    if(tag((a=funcname(l(0))))==CONS){ /* when defprop */
      funcname(l(0))=l(3)=car(a);l(4)=l(0);l(5)=cdr(a);
      putprop_f(3,fp-6);
    }
    else definition(a)=c;
    if (n) {
      cdr(l(2))=c=alcons(funcname(l(0)),nil,fp-3);
      l(2)=c;
    }
    else l(2)=l(1)=alcons(funcname(l(0)),nil,fp-3);
    n++;
  }
  return l(1);
}

void init_cmpcode(char **p, WORD *fp)
{
  WORD c;
  
  while (*p != (char *)0){
    l(0)=alstring(*p,fp);
    l(0)=string_stream_f(1,fp-1);
    l(0)=inopen_f(1,fp-1);
    c=read_f(1,fp-1);
    close_f(1,fp-1);  /* no gc in close */
    eval(c,fp);
    p++;
  }
}

/*
 * for new compiler version (Iwasaki)
 */
WORD make_codepiece2(struct code_inf *cp, WORD *fp)
{
	int	nl, i;
	WORD	a, ret;

	nl = cp->c_nliteral;
	l(0) = a = alcode(nl, fp);
	codetop(a) = cp->c_codetop;
	minarg(a) = itofix(cp->c_minarg);
	maxarg(a) = itofix(cp->c_maxarg);
	l(1) = alstring(cp->c_literal, fp - 1);
	l(1) = string_stream_f(1, fp - 2);
	inopen_f(1, fp - 2);
	for ( i = 0; i < nl; i++ ) {
		l(2) = l(1);
		literal(l(0), i) = read_f(1, fp - 3);
	}
	close_f(1, fp - 2);
	a = l(0);
	switch ( cp->c_info ) {
	case INFO_MACRO:
		if ( nl < 1 ) break;
		funcname(a) = literal(a, 0);
		a = alcons(macro, a, fp - 1);
		ret = literal(l(0), 0);
		definition(ret) = a;
		return ret;
	case INFO_FUNC:
		if ( nl < 1 ) break;
		funcname(a) = ret = literal(a, 0);
		definition(ret) = a;
		return ret;
	case INFO_PROP:
		if ( nl < 2 ) break;
		l(1) = ret = literal(a, 0);	/* sym */
		funcname(a) = ret;
		l(2) = a;			/* value */
		l(3) = literal(a, 1);		/* name */
		putprop_f(3, fp - 4);
		return l(0);
	}
	return a;
}
