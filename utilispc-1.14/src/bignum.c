#include <stdio.h>
#include <stdlib.h>
#include "constdef.h"
#include "defvar.h"
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef TEST
int main(int ac,char **ag){
	unsigned int a[40],b[40],c[40],d[40];
	int i,j,k,l,m,n;
	j=3;
	switch(j){
	case 0:  /*addition & subtraction test*/
		{
			n=4;
			for(i=0;i<n;i++)a[i]=0;
			a[0]=2;
			for(i=0;i<n;i++)printf("a[%d]= %x \n",i,a[i]);
			for(i=0;i<n;i++)b[i]=0xffffffff;
			b[0]=0xfffffffe;
			for(i=0;i<n;i++)printf("b[%d]= %x \n",i,b[i]);
			m=msub(a,0,b,0,a,n);
			printf("m= %d\n",m);
			for(i=0;i<m;i++)printf("c[%d]= %x \n",i,a[i]);
			break;
		}
	case 1: /*multiplication test*/
		{
			m=6;
			n=4;
			for(i=0;i<m;i++)a[i]=0xffffffff;
			for(i=0;i<m;i++)printf("a[%d]= %x \n",i,a[i]);
			for(i=0;i<n;i++)b[i]=0xffffffff;
			for(i=0;i<n;i++)printf("b[%d]= %x \n",i,b[i]);
			mmul(a,b,c,m,n);
			for(i=0;i<m+n;i++)printf("c[%d]= %x \n",i,c[i]);
			break;
		}
	case 2: /*division test*/
		{
			m=6;
			n=4;
			for(i=0;i<m;i++)c[i]=0xffffffff;
			c[5]=1;
			vprint(c,m);
			for(i=0;i<n;i++)b[i]=0xffffffff;
			b[3]=0xffff;
			vprint(b,n);
			k=mdiv(c,m,b,n,a);
			printf("k= %x\n",k);
			vprint(a,k/16);
			vprint(c,k%16);
			break;
		}
	case 3: /*division test*/
		{
			for(i=32;i<400;i++){
				for(j=32;j<=i;j++){
					m=gen1(c,i);
					n=genf(b,j);
					/*printf("dividend ");vprint(c,m);*/
					/*printf("divisor  ");vprint(b,n);*/
					k=mdiv(c,m,b,n,a);
					n=genf(b,j);
					/*printf("quotient ");vprint(a,k/16);*/
					/*printf("remainder");vprint(c,k%16);*/

					/*for(l=k%16;l<m;l++)c[l]=0;*/
					/*printf("multiplier ");vprint(b,n);*/
					/*printf("multiplicand");vprint(a,k/16);*/
					mmul(b,a,d,n,k/16);
					madd(d,0,c,0,d,m);/*vprint(d,m);*/
					gen1(c,i);
					for(l=0;l<m;l++){
						if(c[l]!=d[l])
							printf("i= %d j= %d c[%d]= %x d[%d]= %x\n",
							    i,j,l,c[l],l,d[l]);
					}

				}
			}
			break;
		}
	case 4:
		{

			a[0]=0xcafeca01;
			a[1]=0xfe5f7600;
			a[2]=0x6d61946f;
			a[3]=0xa660ea14;
			a[4]=0xc4373c56;
			a[5]=0xa70c3;

			/*	for(i=0;i<5;i++)a[i]=0;*/
			/*	a[5]=1;*/

			k=mconv1(a,6);
			vtrace(1,a,k);
			break;
		}
	case 5:
		{
			for(i=0;i<4;i++)a[i]=0;
			a[3]=0x20;
			i=udiv(a,4,0x7,b);
			vtrace(1,b,i);
			printf("r= %x\n",a[0]);
			break;
		}
	case 6:
		{
			a[0]=1;
			b[0]=0xffffffff;
			b[1]=0;
			vprint(a,1);
			vprint(b,2);
			mmul(a,b,d,1,2);
			vprint(d,2);
			break;
		}
	}
	return 0;
}
#endif

/*
  1995/9/1
  long longのmaddはgccの未熟さのため遅いので, 当分は使わない. 
*/
#if 1

/* madd */
/*補数表示で a[k+n-1],a[k+n-2],...,a[k] + b[l+n-1],b[l+n-2],...,b[l]*/
/*  -> s[n-1],s[n-2],...,s[0] にいれ, overflowがあればs[n]にもsetする;*/
/* sの語長(nまたはn+1)をかえす. a配列とs配列は同じでよい. */

int _madd(unsigned int *a, int k, unsigned int *b, int l, unsigned int *s, int n)
{
	unsigned int a0,b0,s0;
	int i=0;
	for(;;){
nocarry:
		a0=a[i+k];
		b0=b[i+l];
		s0=a0+b0;
		s[i]=s0;
		if(++i>=n){
			if((signed int)s0<0 && (signed int)a0>=0
			    && (signed int)b0>=0){
				s[n]=0;
				return n+1;
			}
			if((signed int)s0>=0 && (signed int)a0<0
			    && (signed int)b0<0){
				s[n]=0xffffffff;
				return n+1;
			}
			return n;
		}
		if(s0>=a0)
			goto nocarry;
carry:
		a0=a[i+k];
		b0=b[i+l];
		s0=a0+b0+1;
		s[i]=s0;
		if(++i>=n){
			if((signed int)s0<0 && (signed int)a0>=0
			    && (signed int)b0>=0){
				s[n]=0;
				return n+1;
			}
			if((signed int)s0>=0 && (signed int)a0<0
			    && (signed int)b0<0){
				s[n]=0xffffffff;
				return n+1;
			}
			return n;
		}
		if(s0<=a0) goto carry;
	}
}
/* msub */
/*補数表示で a[k+n-1],a[k+n-2],...,a[k] - b[l+n-1],b[l+n-2],...,b[l]*/
/*  -> d[n-1],d[n-2],...,d[0] にいれ, overflowがあればd[n]にもsetする;*/
/* dの語長(nまたはn+1)をかえす. a配列とd配列は同じでよい. */

int _msub(unsigned int *a, int k, unsigned int *b, int l, unsigned int *d, int n)
{
	unsigned int a0,b0,d0;
	int i=0;
	for(;;){
nocarry:
		a0=a[i+k];
		b0=b[i+l];
		d0=a0-b0;
		d[i]=d0;
		if(++i>=n){
			if((signed int)d0<0 && (signed int)a0>=0
			    && (signed int)b0<0){
				d[n]=0;
				return n+1;
			}
			if((signed int)d0>=0 && (signed int)a0<0
			    && (signed int)b0>=0){
				d[n]=0xffffffff;
				return n+1;
			}
			return n;
		}
		if(d0<=a0)
			goto nocarry;
carry:
		a0=a[i+k];
		b0=b[i+l];
		d0=a0-b0-1;
		d[i]=d0;
		if(++i>=n){
			if((signed int)d0<0 && (signed int)a0>=0
			    && (signed int)b0<0){
				d[n]=0;
				return n+1;
			}
			if((signed int)d0>=0 && (signed int)a0<0
			    && (signed int)b0>=0){
				d[n]=0xffffffff;
				return n+1;
			}
			return n;
		}
		if(d0>=a0) goto carry;
	}
}

#else
/* madd */
/*補数表示で a[k+n-1],a[k+n-2],...,a[k] + b[l+n-1],b[l+n-2],...,b[l]*/
/*  -> s[n-1],s[n-2],...,s[0] にいれ, overflowがあればs[n]にもsetする;*/
/* sの語長(nまたはn+1)をかえす. a配列とs配列は同じでよい. */

int _madd(unsigned int *a,int k,
          unsigned int *b,int l,
          unsigned int *s,int n)
{
  unsigned long long a0,b0,s0;
  unsigned int a1,b1,s1;
  int i;
  if(n&1){
    a1=a[k];
    b1=b[l];
    s1=a1+b1;
    s[0]=s1;
    i=1;
    if(n==1){
      if((signed int)s1<0 && (signed int)a1>=0
	 && (signed int)b1>=0){
	s[1]=0;
	return 2;
      }
      if((signed int)s1>=0 && (signed int)a1<0
	 && (signed int)b1<0){
	s[1]=0xffffffff;
	return 2;
      }
      return 1;
    }
    if(s1<a1)
      goto carry;
  }
  else i=0;
  for(;;){
nocarry:
    a0=((unsigned long long)a[i+k+1]<<32)|(unsigned long long)a[i+k];
    b0=((unsigned long long)b[i+l+1]<<32)|(unsigned long long)b[i+l];
    s0=a0+b0;
    s[i]=(unsigned int)(s0&0xffffffff);
    s[i+1]=(unsigned int)(s0>>32);
    i+=2;
    if(i>=n){
      if((signed long long )s0<0 && (signed long long)a0>=0
	 && (signed long long)b0>=0){
	s[n]=0;
	return n+1;
      }
      if((signed long long)s0>=0 && (signed long long)a0<0
	 && (signed long long)b0<0){
	s[n]=0xffffffff;
	return n+1;
      }
      return n;
    }
    if(s0>=a0)
      goto nocarry;
  carry:
    a0=((unsigned long long)a[i+k+1]<<32)|(unsigned long long)a[i+k];
    b0=((unsigned long long)b[i+l+1]<<32)|(unsigned long long)b[i+l];
    s0=a0+b0+1;
    s[i]=(unsigned int)(s0&0xffffffff);
    s[i+1]=(unsigned int)(s0>>32);
    i+=2;
    if(i>=n){
      if((signed long long)s0<0 && (signed long long)a0>=0
	 && (signed long long)b0>=0){
	s[n]=0;
	return n+1;
      }
      if((signed long long)s0>=0 && (signed long long)a0<0
	 && (signed long long)b0<0){
	s[n]=0xffffffff;
	return n+1;
      }
      return n;
    }
    if(s0<=a0) goto carry;
  }
}
/* msub */
/*補数表示で a[k+n-1],a[k+n-2],...,a[k] - b[l+n-1],b[l+n-2],...,b[l]*/
/*  -> d[n-1],d[n-2],...,d[0] にいれ, overflowがあればd[n]にもsetする;*/
/* dの語長(nまたはn+1)をかえす. a配列とd配列は同じでよい. */

int _msub(unsigned int *a, int k,
	  unsigned int *b, int l,
          unsigned int *d, int n)
{
  unsigned long long a0,b0,d0;
  unsigned int a1,b1,d1;
  int i;
  if(n&1){
    a1=a[k];
    b1=b[l];
    d1=a1-b1;
    d[0]=d1;
    i=1;
    if(n==1){
      if((signed int)d1<0 && (signed int)a1>=0
	 && (signed int)b1<0){
	d[1]=0;
	return 2;
      }
      if((signed int)d1>=0 && (signed int)a1<0
	 && (signed int)b1>=0){
	d[1]=0xffffffff;
	return 2;
      }
      return 1;
    }
    if(d1>=a1)
      goto carry;
  }
  else i=0;
  for(;;){
nocarry:
    a0=((unsigned long long)a[i+k+1]<<32)|(unsigned long long)a[i+k];
    b0=((unsigned long long)b[i+l+1]<<32)|(unsigned long long)b[i+l];
    d0=a0-b0;
    d[i]=(unsigned int)(d0&0xffffffff);
    d[i+1]=(unsigned int)(d0>>32);
    i+=2;
    if(i>=n){
      if((signed long long )d0<0 && (signed long long)a0>=0
	 && (signed long long)b0<0){
	d[n]=0;
	return n+1;
      }
      if((signed long long)d0>=0 && (signed long long)a0<0
	 && (signed long long)b0>=0){
	d[n]=0xffffffff;
	return n+1;
      }
      return n;
    }
    if(d0<=a0)
      goto nocarry;
  carry:
    a0=((unsigned long long)a[i+k+1]<<32)|(unsigned long long)a[i+k];
    b0=((unsigned long long)b[i+l+1]<<32)|(unsigned long long)b[i+l];
    d0=a0-b0-1;
    d[i]=(unsigned int)(d0&0xffffffff);
    d[i+1]=(unsigned int)(d0>>32);
    i+=2;
    if(i>=n){
      if((signed long long)d0<0 && (signed long long)a0>=0
	 && (signed long long)b0<0){
	d[n]=0;
	return n+1;
      }
      if((signed long long)d0>=0 && (signed long long)a0<0
	 && (signed long long)b0>=0){
	d[n]=0xffffffff;
	return n+1;
      }
      return n;
    }
    if(d0>=a0) goto carry;
  }
}
#endif

/* uadd */
/*補数表示で a[k+n-1],a[k+n-2],...,a[k] + x  (xは単一語長)*/
/*  -> a[K+n-1],a[k+n-2],...,a[k] にいれ, overflowがあればa[k+n]にもsetする;*/
/* aの語長(nまたはn+1)をかえす. */

int _uadd(unsigned int *a, int k, int sx, int n)
{
  unsigned int x=(unsigned int)sx;
	unsigned int a0,s0;
	int i=0;
	for(;;){
		a0=a[i+k];
		s0=a0+x;
		a[i+k]=s0;
		if(++i>=n){
			if((signed int)s0<0 && (signed int)a0>=0
			    && (signed int)x>=0){
				a[n]=0;
				return n+1;
			}
			if((signed int)s0>=0 && (signed int)a0<0
			    && (signed int)x<0){
				a[n]=0xffffffff;
				return n+1;
			}
			return n;
		}
		if((signed int)s0>=0 && (signed int)a0<0
		    && (signed int)x>=0)x=1;
		else 
			if((signed int)s0<0 && (signed int)a0>=0
			    && (signed int)x<0)x=0xffffffff;
			else return n;
	}
}

/* usub */
/*補数表示で a[k+n-1],a[k+n-2],...,a[k] - x (xは単一語長)*/
/*  -> a[K+n-1],a[k+n-2],...,a[k] にいれ, overflowがあればa[k+n]にもsetする;*/
/* aの語長(nまたはn+1)をかえす. */

int _usub(unsigned int *a, int k, int sx, int n)
{
  unsigned int x=(unsigned int)sx;
	unsigned int a0,d0;
	int i=0;
	for(;;){
		a0=a[i+k];
		d0=a0-x;
		a[i+k]=d0;
		if(++i>=n){
			if((signed int)d0<0 && (signed int)a0>=0
			    && (signed int)x<0){
				a[n]=0;
				return n+1;
			}
			if((signed int)d0>=0 && (signed int)a0<0
			    && (signed int)x>=0){
				a[n]=0xffffffff;
				return n+1;
			}
			return n;
		}
		if((signed int)d0>=0 && (signed int)a0<0
		    && (signed int)x<0)x=0xffffffff;
		else 
			if((signed int)d0<0 && (signed int)a0>=0
			    && (signed int)x>=0)x=1;
			else return n;
	}
}


/* mmul */
/*正整数同士の乗算で, u[0],u[1],...,u[m-1] と v[0],v[1],...,v[n-1] (添字0の方*/
/*がleast significant)をかけ, p[0],p[1],...,p[m+n-1]におく. */
/*関数の先頭で p[0],p[1],...,p[m-1]をクリアしないと, */
/*p[0..m+n-1]=u[0..m-1]*v[0..n-1]+p[0..m-1]となる. */


void _mmul(unsigned int *u, unsigned int *v, unsigned int *p, unsigned int m, unsigned int n)
{
	unsigned i,j,k,vj,ui,pij;
	unsigned long long pijk;

	for(i=0;i<m;i++)p[i]=0; /* u*v+p を計算するときはコメントアウトする*/
	for(j=0;j<n;j++){
		k=0;
		vj=v[j];
		if(vj!=0){
			for(i=0;i<m;i++){
				ui=u[i];
				pij=p[i+j];

				/* temp=ui * vj + pij + k;  */
				/* k=temp/2^32;             */
				/* pij=temp%2^32;           */

				pijk=(unsigned long long)ui
				    *(unsigned long long)vj
				    +(unsigned long long)pij
				    +(unsigned long long)k;
				k=(unsigned)(pijk>>32);
				pij=(unsigned)(pijk&0xffffffff);
				p[i+j]=pij;
			}
		}
		p[j+m]=k;
	}
}

/* 除算の下請け関数*/
int scount (unsigned int x)
{
	int n=0;
	if (x < 0x8000) {
		x=x<<16;
		n=n+16;
	}
	if (x < 0x800000) {
		x=x<<8;
		n=n+8;
	}
	if (x < 0x8000000) {
		x=x<<4;
		n=n+4;
	}
	if (x < 0x20000000) {
		x=x<<2;
		n=n+2;
	}
	if (x < 0x40000000) {
		x=x<<1;
		n=n+1;
	}
	return n;
}

/* 除算の下請け関数*/

void mshiftl(unsigned int *a, int m, int s)
{
	unsigned long long x;
	int i;
                x=a[m-1];
                x=x<<32;
                for(i=m-1;i>0;i--){


                        x=(x+a[i-1])<<s;

                        a[i]=x>>32;

                        x=x<<(32-s);
                }
                a[0]=x>>(32-s);
}

void mshiftr(unsigned int *a, int m, int s)            /*s>=0*/
                
        
{
        unsigned long long x,y;
        int i;
                x=a[0];
                for(i=1;i<m;i++){

                        y=a[i];
                        x=(x+(y<<32))>>s;

                        a[i-1]=x & 0xffffffff;

                        x=x>>(32-s);
                }
                a[m-1]=(x>>s) & 0xffffffff;
}

/* mdiv */
/*正整数同士の除算で, u[0],u[1],...,u[m-1] を v[0],v[1],...,v[n-1] (添字0の方*/
/*がleast significant)でわり, 商をq[0],q[1],...,q[k-1]に, */
/*剰余をu[0],u[1],...,u[n-1]におく. k*2^16を返す. */

#if 0
int _mdiv(unsigned int *u,int m,
          unsigned int *v,int n,
	  unsigned int *q,unsigned int *r)
{
	int i,j,p1;
	unsigned int q1,vn1,vn2,uin,uin1,uin2,a,d,p,k,b,pp[n+1];
	unsigned long long x,y;
	i=n-1;
	for(;;){
		if(i<0){
			u[m]=0;
			m=m+1;
			break;
		}
		if(u[i+m-n]<v[i])break;
		if(u[i+m-n]==v[i]){
			i=i-1;
			continue;
		}
		u[m]=0;
		m=m+1;
		break;
	}
	p1=scount(v[n-1]);
	mshiftl(u,m,p1);
	/*vtrace(1,u,m);*/
	mshiftl(v,n,p1);
	/*vtrace(2,v,n);*/
	v[n]=0;
	if(m<=n){
		q[0]=0;
		mshiftr(u,m,p1);
		return (m<<16)+1;
	}
	vn1=v[n-1];
	vn2=(n>1)?v[n-2]:0;
	for(i=m-n-1;i>=0;i--){
		uin=u[i+n];
		uin1=u[i+n-1];
		uin2=((i+n)>1)?u[i+n-2]:0;
		if(vn1==uin)q1=0xffffffff;
		else 
			q1=(((unsigned long long)uin<<32)+(unsigned long long)uin1)
			    /(unsigned long long)vn1;
		while(1){
                        y=((unsigned long long)uin<<32)
                          +(unsigned long long)uin1
			  -((unsigned long long)q1*(unsigned long long)vn1);
                        if((y>>32)>0)break;
			y=(y<<32)+(unsigned long long)uin2;
			x=(unsigned long long)vn2 * (unsigned long long)q1;
			if(x>y)q1=q1-1;
			else break;
		}
		if(1){
			_umul(v,n,(unsigned int)q1,pp); /*vtrace(0,pp,n+1);*/
			_msub(u,i,pp,0,pp,n+1);
			for(j=0;j<=n;j++)u[i+j]=pp[j];/*vtrace(1,u,m);*/
		} else {
			k=0;
			b=0;
			for(j=0;j<n;j++){
				y=(unsigned long long)v[j]*(unsigned long long)q1 
				    +(unsigned long long)k;/*printf("v[%d]= %x q1= %x k= %x\n",
				                                      j,v[j],q1,k);*/
				p=y & 0xffffffff;
				k=y>>32; 
				a=u[i+j]; 
				d=a-p-b;
				u[i+j]=d; 
				b=(a<d+b)?1:0;
				/*printf("p= %x a= %x d= %x b= %x\n",p,a,d,b);*/			}
			u[i+n]=u[i+n]-k-b;/*vtrace(2,u,m);*/
		}
		if(u[i+n]>=0x80000000){
			_madd(u+i,0,v,0,u+i,n+1);
			/*for(j=0;j<=n;j++)u[i+j]=p[j];*/
			/*vtrace(5,u,m);*/
			q1=q1-1;
		}
		q[i]=q1;
		/*trace("q12",q1);*/
	}
	mshiftr(u,n,p1);
	if(q[m-n-1]>=0x80000000){
		q[m-n]=0;
		m=m+1;
	}
	return (n<<16)+(m-n);
}
#else
int _mdiv(unsigned int *u, int m, unsigned int *v, int n, unsigned int *q, unsigned int *r)
{
	int i,j,p1;
	unsigned int q1,vn1,vn2,uin,uin1,uin2,a,d,p,k,b;
	unsigned long long x,y;
	i=n-1;
	for(;;){
		if(i<0){
			u[m]=0;
			m=m+1;
			break;
		}
		if(u[i+m-n]<v[i])break;
		if(u[i+m-n]==v[i]){
			i=i-1;
			continue;
		}
		u[m]=0;
		m=m+1;
		break;
	}
	p1=scount(v[n-1]);
	mshiftl(u,m,p1);                      /*変更*/
	/*vtrace(1,u,m);*/
	mshiftl(v,n,p1);                      /*変更*/
	/*vtrace(2,v,n);*/
	v[n]=0;
	if(m<=n){
		q[0]=0;
		mshiftr(u,m,p1);              /*変更*/
		return (m<<16)+1;
	}
	vn1=v[n-1];
	vn2=(n>1)?v[n-2]:0;
	for(i=m-n-1;i>=0;i--){
		uin=u[i+n];
		uin1=u[i+n-1];
		uin2=((i+n)>1)?u[i+n-2]:0;
		if(vn1==uin)q1=0xffffffff;
		else 
			q1=(((unsigned long long)uin<<32)
                             +(unsigned long long)uin1)
			    /(unsigned long long)vn1;
		while(1){
                        y=((unsigned long long)uin<<32)
                          +(unsigned long long)uin1
			  -((unsigned long long)q1*(unsigned long long)vn1);
                        if((y>>32)>0)break;
			y=(y<<32)+(unsigned long long)uin2;
			x=(unsigned long long)vn2 * (unsigned long long)q1;
			if(x>y)q1=q1-1;
			else break;
		}
		k=0;                           /*ここから変更*/
		b=0;
		for(j=0;j<n;j++){
			y=(unsigned long long)v[j]*(unsigned long long)q1 
			    +(unsigned long long)k;
			p=y & 0xffffffff;
			k=y>>32; 
			a=u[i+j]; 
			d=a-p-b;
			u[i+j]=d; 
                        if(b)b=(a<=d)?1:0;else b=(a<d)?1:0;
		}                             /*ここまで変更*/
		u[i+n]=u[i+n]-k-b;
		if(u[i+n]>=0x80000000){
			_madd(u+i,0,v,0,u+i,n+1);
			/*for(j=0;j<=n;j++)u[i+j]=p[j];*/
			/*vtrace(5,u,m);*/
			q1=q1-1;
		}
		q[i]=q1;
		/*trace("q12",q1);*/
	}
	mshiftr(u,n,p1);                   /*変更*/
	if(q[m-n-1]>=0x80000000){
		q[m-n]=0;
		m=m+1;
	}
	return (n<<16)+(m-n);
}
#endif

int udiv(unsigned int *u, int m, unsigned int x, unsigned int *q)
{
	int i;
	unsigned long long y;
#if 0
	printf("test\n");
	vtrace(2,u,m);
#endif
	if(u[m-1]>=x){
		y=0;
		m=m+1;
	} else y=(unsigned long long)u[m-1];
	for(i=m-2;i>=0;i--){
		y=(y<<32)+(unsigned long long)u[i];
		q[i]=y/x;
		y=y%x;
	}
	u[0]=y & 0xffffffff;
#if 0
	vtrace(4,q,m-1);
#endif
	return m-1;
}


/* mconv */
/*正整数表現のx[0],x[1],...,x[n-1] を10^9で割った次々の剰余を*/
/*d[0],d[1],...,d[k-1]におく. kの値を返す*/

int _mconv(unsigned int *x, int n, unsigned int *d)
{
	int i,j=0;
	unsigned long long r;
	for(;;){
		if(n==0)break;
		if(x[n-1]<0x3b9aca00){
			n=n-1;
			r=x[n];
		} else r=0;
#if 0
		printf("n= %d\n",n);
#endif
		for(i=n-1;i>=0;i--){
			r=(r<<32)+x[i];
#if 0
			longtrace("r0",r);
#endif
			x[i]=r/(unsigned long long)0x3b9aca00;
#if 0
			printf("x[%d]= %x\n",i,x[i]);
#endif
			r=r%(unsigned long long)0x3b9aca00;
#if 0
			longtrace("r1",r);
#endif
		}
#if 0
		longtrace("r",r);
#endif
		d[j]=r;
		j=j+1;
	}
	return j;
}


int mconv1(unsigned int *u, int n)
{
	int i=0;
	while(n>0){
#if 0
		vtrace(3,u,n+i);
#endif
		n=udiv(u+i,n,0x3b9aca00,u+i+1);
#if 0
		printf("i= %d, n= %d\n",i,n);
#endif
		i++;
	}

	return i;
}


int mnorm(unsigned int *a, int n)
{
	int i=n-1;
	unsigned int x=a[0];
	if(a[n-1]>=0x80000000){
		a[0]=0;
		while(a[i]==0xffffffff){
#if 0
			printf("%d %x\n",i,a[i]);
#endif
			i--;
		}
		a[0]=x;
		if(a[i]<0x80000000) i++;
	}
	else {
		a[0]=0xffffffff;
		while(a[i]==0){
#if 0
			printf("%d %x\n",i,a[i]);
#endif
			i--;
		}
		a[0]=x;
		if(a[i]>=0x80000000) i++;
	}
	return i+1;
}

int mcompl(unsigned int *a, int m)
{
	unsigned long long a0=0,d0=0;
	int carry=0,i;
	for(i=0;i<m;i++){
		a0=a[i];
		d0=0-a0-carry;
		if(a0>=0x80000000 || (a0<0x80000000 && d0>=0x80000000))carry=1;
		else carry=0;
		a[i]=d0;
	}
	if(a0>=0x80000000 && d0>=0x80000000){
		a[m]=0;
		return m+1;
	} else return 0;
}



void _umul(unsigned int *u, int m, int sx, unsigned int *p)
{
  unsigned int x=(unsigned int)sx;
	unsigned int k=0;
	unsigned long long y;
	int i;
	for(i=0;i<m;i++){
		y=(unsigned long long)u[i]*(unsigned long long)x
		    +(unsigned long long)k;
		p[i]=y & 0xffffffff;
		k=y>>32;
	}
	p[m]=k;
}




void vtrace(int i, unsigned int *u, int m)
{
	int j;
	printf("point %d\n",i);
	for(j=0;j<m;j++)printf("a[%d]= %x\n",j,u[j]);
}

void trace(char *s, unsigned int x)
{
	printf("%s %x\n",s,x);
}

void longtrace(char *s, long long unsigned int x)
{
	unsigned int y=x;
	printf("%s %x %x\n",s,(unsigned int)(x>>32),y);
}

void vprint(unsigned int *x, int n)
{
	int i;
	for (i=n-1;i>=0;i--)printf("%x ",x[i]);
	printf("\n");
}

int genf(unsigned int *a, int n)
{
	int i=n/32,j=n%32,m;
	for(m=0;m<i;m++)a[m]=0xffffffff;
	if(j==0)a[i]=0;
	else a[i]=0xffffffff>>(32-j);
	return i+1;
}

int gen1(unsigned int *a, int n)
{
	int i=n/32,j=n%32,m;
	for(m=0;m<i;m++)a[m]=0;
	a[i]=1<<j;
	return i+1;
}


int madd(unsigned int *a, unsigned int *b, int size)
{
  int rsize=_madd(a,0,b,0,a,size/4);

  return mnorm(a,rsize)*4;
}

int msub(unsigned int *a, unsigned int *b, int size)
{
  int rsize=_msub(a,0,b,0,a,size/4);

  return mnorm(a,rsize)*4;
}

int uadd(unsigned int *a, int v, int size)
{
  int rsize=_uadd(a,0,(unsigned int)v,size/4);

  return mnorm(a,rsize)*4;
}

int usub(unsigned int *a, int v, int size)
{
  int rsize=_usub((unsigned int *)a,0,(unsigned int)v,size/4);

  return mnorm(a,rsize)*4;
}

int umul(unsigned int *sa, int sasize, int v)
{
  unsigned int *a=(unsigned int *)sa;
  size_t asize=(size_t)sasize;
  unsigned int *b,*c;

  if(a[asize/4-2]<0){
    c=alloca(asize);
    _umul(a,asize/4-1,v,c);
    b=alloca(asize);
    memset(b,0,asize-4);
    b[asize/4-1]=v;
    msub(c,b,asize);
    if((int)v<0){
      memcpy(b+1,a,asize-4);
      b[0]=0;
      msub(c,b,asize);
    }
    memcpy(a,c,asize);
  }
  else if((int)v<0){
    c=alloca(asize);
    _umul(a,asize/4-1,v,c);
    b=alloca(asize);
    memcpy(b+1,a,asize-4);
    b[0]=0;
    msub(c,b,asize);
    memcpy(a,c,asize);
  }
  else{
    _umul(a,asize/4-1,v,a);
  }
  return mnorm(a,asize/4)*4;
}

int mmul(unsigned int *a, int asize, unsigned int *b, int bsize)
{
  unsigned int *abs_a=(unsigned int *)a,*abs_b=(unsigned int *)b,*c;
  int sign_a=0,sign_b=0,i,csize;

  asize-=bsize;
  if((int)a[asize/4-1]<0){
    c=alloca(asize+4);
    memcpy(c,a,asize);
    c[asize/4]=0xffffffff;
    sign_a=1;
    abs_a=alloca(asize+4);
    memset(abs_a,0,asize+4);
    asize=msub(abs_a,c,asize+4);
  }
  else abs_a=a;
  if((int)b[bsize/4-1]<0){
    sign_b=1;
    c=alloca(bsize+4);
    memcpy(c,b,bsize);
    c[bsize/4]=0xffffffff;
    abs_b=alloca(bsize+4);
    memset(abs_b,0,bsize+4);
    bsize=msub(abs_b,c,bsize+4);
  }
  else abs_b=b;
  csize=asize+bsize;
  c=alloca(csize);
  _mmul(abs_a,abs_b,c,asize/4,bsize/4);
  if(sign_a^sign_b){ /* return minus value */
    memset(a,0,csize);
    asize=msub(a,c,csize);
  }
  else{
    for(i=csize/4-1;i>=0;i--)
      if(c[i])break;
    if(i<0 || (int)c[i]<0 ) i++;
    asize=(i+1)*4;
    for(;i>=0;i--)
      a[i]=c[i];
  }
  return mnorm(a,asize/4)*4;
}

int mdiv(unsigned int *a, int asize, unsigned int *b, int bsize)
{
  unsigned int *abs_a=a,*abs_b=b,*q,*r,*c;
  int i;
  int sign_a=0,sign_b=0,rsize,qsize;

  if((int)a[asize/4-1]<0){
    c=alloca(asize+4);
    memcpy(c,a,asize);
    c[asize/4]=0xffffffff;
    sign_a=1;
    abs_a=alloca(asize+8);
    memset(abs_a,0,asize+4);
    asize=msub(abs_a,c,asize+4);
  }
  else {
    abs_a=alloca(asize+4);
    memcpy(abs_a,a,asize);
    asize=mnorm(abs_a,asize/4)*4;
  }
  if((int)b[bsize/4-1]<0){
    sign_b=1;
    c=alloca(bsize+4);
    memcpy(c,b,bsize);
    c[bsize/4]=0xffffffff;
    abs_b=alloca(bsize+8);
    memset(abs_b,0,bsize+4);
    bsize=msub(abs_b,c,bsize+4);
  }
  else {
    abs_b=alloca(bsize+4);
    memcpy(abs_b,b,bsize);
    bsize=mnorm(abs_b,bsize/4)*4;
  }
  q=alloca(asize+4);
  rsize=_mdiv(abs_a,asize/4,abs_b,bsize/4,q,NULL);
  qsize=rsize&0xffff;
  rsize>>=16;
  r=alloca(bsize);
  memcpy(r,abs_a,rsize*4);
  if(sign_a^sign_b){
    memset(a,0,qsize*4+4);
    q[qsize]=0;
    qsize=msub(a,q,qsize*4+4)/4;
    qsize=usub(a,1,qsize*4)/4;
  }
  else
    memcpy(a,q,qsize*4);
  if(sign_a){
    if(sign_b){ /* good */
      memset(a+qsize,0,rsize*4);
      rsize=msub(a+qsize,r,rsize*4)/4;
    }
    else{
      memset(a+qsize,0,rsize*4);
      for(i=rsize;i<bsize/4;i++)
	a[qsize+i]= -1;
      msub(a+qsize,r,rsize*4);
      rsize=madd(a+qsize,b,bsize)/4;
    }
  }
  else if(sign_b){ /* 10 % -3 -> 4 */
    memcpy(a+qsize,r,rsize*4);
    if(bsize>rsize*4)
      memset(a+qsize+rsize,0,bsize-rsize*4);
    rsize=madd(a+qsize,b,bsize)/4;
  }
  else /* good */
    memcpy(a+qsize,r,rsize*4);
  return (rsize<<18)|(qsize<<2);
}

int mconv(unsigned int *a, int asize, unsigned int *b)
{
  return _mconv(a,asize/4,b)*4;
}

