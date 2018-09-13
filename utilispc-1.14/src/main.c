/* @(#)main.c	1.7 7/30/90 */

#include "constdef.h"
#include <stdio.h>

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#include <sys/resource.h>

#ifdef HAVE_SYS_FILE_H
#include <sys/file.h>
#endif

#include <sys/mman.h>


#define MAIN
#include "defvar.h"

#define	ALLOC	-1
#define	HEAP	-2
#define	SYM	-3
#define	USAGE	-4
#define	STKSML	-5
#define	STKOPT	-6

int heapsize = 16384;		/* default */
int allheapsize;
int lstacksize=128;
int bstacksize=64;
int cstacksize=32;
int estacksize=32;
#ifdef NOMALLOC
int mallocsize=16;
int malloccheck;
#endif
int codeareasize=0;
int pagesize;

void errmsg(int id)
{
  switch(id){
  case USAGE:
    fputs("Usage: ",stderr);
    fputs(argv[0],stderr);
    fputs(" [-h size] [-S size] [-f size] [-F fname]\n",stderr);
    fputs("          [-cs size][-ls size][-bs size][-es size][-n][-N]\n",stderr);
    fputs(" [-d fname][-m size]\n",stderr);
    exit(1);
  case HEAP:
    fputs(argv[0],stderr);
    fputs(": Too small heap area.\n",stderr);
    exit(1);
  case SYM:
    fputs(argv[0],stderr);
    fputs(": Too small symbol area.\n",stderr);
    exit(1);
  case ALLOC:
    fputs(argv[0],stderr);
    fputs(": Heap allocation failed.\n",stderr);
    exit(1);
  case STKSML:
    fputs(argv[0],stderr);
    fputs(": Too small stack size.\n",stderr);
    exit(1);
  case STKOPT:
    /*	fprintf(stderr,"%s%s\n","Caution! stacksize is defined",
	" by limit command. (ignored)");*/
    break;
  default:
    exit(1);
  }
}

void undumpsetup(char *dumpfile)
{
#ifdef NOMALLOC
  char buf[4*9];
  char str[256];
  int fd,mfd;
  WORD *file_lstacktop;
  int file_heapused;
  char *addr;
  if((fd=open(dumpfile,O_RDONLY))<0){
    fputs("Dump File ",stderr);
    fputs(dumpfile,stderr);
    fputs(" is not found\n",stderr);
    exit(1);
  }
  read(fd,buf,4*9);
  memcpy(&file_lstacktop,buf,sizeof(WORD *));
  memcpy(&lstacksize,buf+4,sizeof(int));
  memcpy(&bstacksize,buf+4*2,sizeof(int));
  memcpy(&cstacksize,buf+4*3,sizeof(int));
  memcpy(&estacksize,buf+4*4,sizeof(int));
  memcpy(&mallocsize,buf+4*5,sizeof(int));
  memcpy(&mallocptr,buf+4*6,sizeof(int));
  memcpy(&codeareasize,buf+4*7,sizeof(int));
  memcpy(&file_heapused,buf+4*8,sizeof(int));
  if(file_heapused>heapsize){
    fputs("Heap requared ",stderr);
    sprintf(str,"%d",file_heapused);
    fputs(str,stderr);
    fputs(" bytes\n",stderr);
    exit(1);
  }
#ifndef NO_MMAP_STACK_CHECK
  addr=sbrk(0);
  addr=(char *)(((int)addr+pagesize-1)&~(pagesize-1));
  brk(addr);
  addr=sbrk(pagesize);
  mfd=open("/dev/zero",0);
  mmap(addr,pagesize,0,MAP_SHARED|MAP_FIXED,mfd,0);
  if((lstacktop=
      (WORD *)sbrk(lstacksize+bstacksize+cstacksize+estacksize+
		   mallocsize+codeareasize+allheapsize+pagesize*3))
     !=(WORD *)file_lstacktop){
    fputs("Illegal version of Utilisp/C\n",stderr);
    exit(1);
  }
#else
  if((lstacktop=
      (WORD *)sbrk(lstacksize+bstacksize+cstacksize+estacksize+
		   mallocsize+codeareasize+allheapsize))
     !=(WORD *)file_lstacktop){
    fputs("Illegal version of Utilisp/C\n",stderr);
    exit(1);
  }
#endif /* NO_MMAP_STACK_CHECK */
#ifndef NO_MMAP_STACK_CHECK
    mmap((char *)lstacktop+lstacksize,
	 pagesize,0,MAP_SHARED|MAP_FIXED,mfd,0);
    bstacktop=(WORD *)((char *)lstacktop+lstacksize+pagesize);
    mmap((char *)bstacktop+bstacksize,
	 pagesize,0,MAP_SHARED|MAP_FIXED,mfd,0);
    cstacktop=(WORD *)((char *)bstacktop+bstacksize+pagesize);
    mmap((char *)cstacktop+cstacksize,
	 pagesize,0,MAP_SHARED|MAP_FIXED,mfd,0);
    close(mfd);
    estacktop=(struct lispenv *)((char *)cstacktop+cstacksize+pagesize);
#else
    bstacktop=(WORD *)((char *)lstacktop+lstacksize);
    cstacktop=(WORD *)((char *)bstacktop+bstacksize);
    estacktop=(struct lispenv *)((char *)cstacktop+cstacksize);
#endif /* NO_MMAP_STACK_CHECK */
  malloctop=(char *)estacktop+estacksize;
  codeareatop=malloctop+mallocsize;
  allheaptop=(WORD *)(codeareatop+codeareasize);
  read(fd,symroot,SYMROOTSIZE*sizeof(WORD));
  read(fd,root,ROOTSIZE*sizeof(WORD));
  read(fd,malloctop,mallocptr-malloctop);
  read(fd,codeareatop,codeareasize);
  read(fd,allheaptop,file_heapused);
  close(fd);
  lstack=lstackbottom=lstacktop+lstacksize/sizeof(WORD);
  bstack=bstackbottom=bstacktop+bstacksize/sizeof(WORD);
  cstack=cstackbottom=cstacktop+cstacksize/sizeof(WORD);
  estack=estackbottom=estacktop+estacksize/sizeof(struct lispenv);
  allheapbottom=allheaptop+allheapsize/sizeof(int);
  heaptop=allheaptop;
  heap=heaptop+file_heapused;
  heapbottom=heaptop+heapsize/sizeof(int);
  initsig();
#ifndef OBJECTSPECIAL
  uderr=(WORD)udferr;
  uderr+=OTHERS; /* for cc -O2 bug */
#endif
  defeof_f(0,lstack);
#endif
}

#ifdef AP1000
int lispsys_size;
char *lispsys_buf;
void cell_main(void)
{
  char *dumpfile=NULL;
  int ignored=0;
  int *msgp,rsize;
  int ac,i,len;
  char **av,*cp;

  msgp=(int *)h_recv(0);
  ac=msgp[0];
  if((av=(char **)malloc(ac*sizeof(char *)))==NULL){
    printf("Error in malloc\n");
    cell_exit(1);
  }
  for(i=0;i<ac;i++){
    msgp=(int *)h_recv(0);
    len=msgp[0];
    if((av[i]=(char *)malloc(len))==NULL){
      printf("Error in malloc\n");
      cell_exit(1);
    }
    msgp=(int *)h_recv(0);
    strcpy(av[i],msgp);
    if(getcid()==0)
      printf("argv[0x%x]=(%s)\n",i,av[i]);
  }
  msgp=(int *)h_recv(0);
  lispsys_size=msgp[0];
  if((lispsys_buf=(char *)malloc(lispsys_size+1))==NULL){
    printf("Error in malloc\n");
    cell_exit(1);
  }
  msgp=(int *)h_recv(0);
  rsize=readmsg(lispsys_buf,lispsys_size);
  if(rsize!=lispsys_size){
    printf("Error in read rsize=%d, lispsys_size=%d\n",rsize,lispsys_size);
    cell_exit(1);
  }
  lispsys_buf[lispsys_size]=0;
#else
int main(int ac, char **av)
{
  register char *cp;
  int ignored=0;
  char *dumpfile=NULL;
  int fd;

#endif
  argv = av;
  argc = ac;
  
#if 0
  {
    int i;
    for(i=0;i<ac;i++)
      printf("%d : %s\n",i,argv[i]);
  }
#endif
  av++;
  while(--ac > 0){
    cp = av++[0];
    if(cp[0] == '-'){
      switch(cp[1]){
      case 'h':
	if ((--ac <= 0) || (heapsize = atoi(av++[0])) == 0)
	  errmsg(USAGE);
	break;
      case 's':
	errmsg(STKOPT);
	break;
      case 'l':
	if(cp[2]!='s')break;
	if ((--ac <= 0) || (lstacksize = atoi(av++[0])) == 0)
	  errmsg(USAGE);
	ignored=1;
	break;
      case 'c':
	if(cp[2]!='s')break;
	if ((--ac <= 0) || (cstacksize = atoi(av++[0])) == 0)
	  errmsg(USAGE);
	ignored=1;
	break;
      case 'b':
	if(cp[2]!='s')break;
	if ((--ac <= 0) || (bstacksize = atoi(av++[0])) == 0)
	  errmsg(USAGE);
	ignored=1;
	break;
      case 'e':
	if(cp[2]!='s')break;
	if ((--ac <= 0) || (estacksize = atoi(av++[0])) == 0)
	  errmsg(USAGE);
	ignored=1;
	break;
      case 'd':
	if (--ac <=0)
	  errmsg(USAGE);
	dumpfile=av++[0];
	break;
#ifdef NOMALLOC
      case 'm':
	if ((--ac <= 0) || (mallocsize = atoi(av++[0])) == 0)
	  errmsg(USAGE);
	ignored=1;
	break;
      case 'M':
	malloccheck=1;
	break;
#endif
      case 'N':
	no_lispsys_l=1;
	break;
      default:
	if(!strcmp("-gctype",cp)){
	  if(--ac <=0) errmsg(USAGE);
	  gctype=atoi(av++[0]);
	  if(gctype!=0 && gctype!=1){
	    fprintf(stderr,"Unknown GC type [%d]\nGC type must be 0 or 1\n",gctype);
	    gctype=0;
	  }
	}
	break;
      }
    }
  }
  heapsize *= 1024;
  if(gctype==1)
    allheapsize=heapsize+(heapsize+31)/32+16;
  else
    allheapsize=heapsize*2;
  lstacksize *= 1024;
  bstacksize *= 1024;
  cstacksize *= 1024;
  estacksize *= 1024;
#ifndef NO_MMAP_STACK_CHECK
  pagesize=getpagesize();
  lstacksize=(lstacksize+pagesize-1)&~(pagesize-1);
  bstacksize=(bstacksize+pagesize-1)&~(pagesize-1);
  cstacksize=(cstacksize+pagesize-1)&~(pagesize-1);
  estacksize=(estacksize+pagesize-1)&~(pagesize-1);
#else
  pagesize=0;
#endif /*  NO_MMAP_STACK_CHECK */
  if(dumpfile==NULL){
#ifdef NOMALLOC
#ifndef NO_MMAP_STACK_CHECK
    char *addr;
    addr=sbrk(0);
    addr=(char *)(((int)addr+pagesize-1)&~(pagesize-1));
    brk(addr);
    addr=sbrk(pagesize);
    fd=open("/dev/zero",0);
    mmap(addr,pagesize,0,MAP_SHARED|MAP_FIXED,fd,0);
#endif /*  NO_MMAP_STACK_CHECK */
    mallocsize *=1024;
    if((lstacktop=
	(WORD *)sbrk(lstacksize+bstacksize+cstacksize+estacksize+
		     mallocsize+allheapsize+pagesize*3))
       ==(WORD *)-1)
      errmsg(ALLOC);
#ifndef NO_MMAP_STACK_CHECK
    bstacktop=(WORD *)((char *)lstacktop+lstacksize+pagesize);
    cstacktop=(WORD *)((char *)bstacktop+bstacksize+pagesize);
    mmap((char *)lstacktop+lstacksize,
	 pagesize,0,MAP_SHARED|MAP_FIXED,fd,0);
    mmap((char *)bstacktop+bstacksize,
	 pagesize,0,MAP_SHARED|MAP_FIXED,fd,0);
    mmap((char *)cstacktop+cstacksize,
	 pagesize,0,MAP_SHARED|MAP_FIXED,fd,0);
    close(fd);
    estacktop=(struct lispenv *)((char *)cstacktop+cstacksize+pagesize);
#else
    bstacktop=(WORD *)((char *)lstacktop+lstacksize);
    cstacktop=(WORD *)((char *)bstacktop+bstacksize);
    estacktop=(struct lispenv *)((char *)cstacktop+cstacksize);
#endif /*  NO_MMAP_STACK_CHECK */
    mallocptr=malloctop=(char *)estacktop+estacksize;
    *(int *)malloctop=0;
    *(char **)(malloctop+4)=NULL;
    mallocptr+=8;
    codeareatop=malloctop+mallocsize;
    allheaptop=(WORD *)codeareatop;
#else /* NOMALLOC */
    if((lstacktop = (WORD *)malloc(lstacksize+pagesize)) == 0) errmsg(ALLOC);
    if((bstacktop = (WORD *)malloc(bstacksize+pagesize)) == 0) errmsg(ALLOC);
    if((cstacktop = (WORD *)malloc(cstacksize+pagesize)) == 0) errmsg(ALLOC);
    if((estacktop = (struct lispenv *)malloc(estacksize+pagesize)) == 0) errmsg(ALLOC);
    if(gctype==1){
      if((allheaptop=(WORD *)malloc(allheapsize+128))==NULL)errmsg(ALLOC);
      allheaptop=(WORD *)(((int)allheaptop+127) & ~127);
    }
    else{
      if((allheaptop=(WORD *)malloc(allheapsize))==NULL)errmsg(ALLOC);
    }
#endif
    lstack=lstackbottom=lstacktop+lstacksize/sizeof(WORD);
    bstack=bstackbottom=bstacktop+bstacksize/sizeof(WORD);
    cstack=cstackbottom=cstacktop+cstacksize/sizeof(WORD);
    estack=estackbottom=estacktop+estacksize/sizeof(struct lispenv);
#if !defined(NO_MMAP_STACK_CHECK) && !defined(NOMALLOC)
    lstack=lstackbottom=lstacktop+(lstacksize+pagesize)/sizeof(WORD);
    bstack=bstackbottom=bstacktop+(bstacksize+pagesize)/sizeof(WORD);
    cstack=cstackbottom=cstacktop+(cstacksize+pagesize)/sizeof(WORD);
    estack=estackbottom=estacktop+(estacksize+pagesize)/sizeof(struct lispenv);
    lstacktop=(WORD *)(((int)lstacktop+pagesize-1)&~(pagesize-1));
    bstacktop=(WORD *)(((int)bstacktop+pagesize-1)&~(pagesize-1));
    cstacktop=(WORD *)(((int)cstacktop+pagesize-1)&~(pagesize-1));
    estacktop=(struct lispenv *)(((int)estacktop+pagesize-1)&~(pagesize-1));
    fd=open("/dev/zero",0);
    mmap((char *)lstacktop,pagesize,0,MAP_SHARED|MAP_FIXED,fd,0);
    lstacktop=(WORD *)((int)lstacktop+pagesize);
    mmap((char *)bstacktop,pagesize,0,MAP_SHARED|MAP_FIXED,fd,0);
    bstacktop=(WORD *)((int)bstacktop+pagesize);
    mmap((char *)cstacktop,pagesize,0,MAP_SHARED|MAP_FIXED,fd,0);
    cstacktop=(WORD *)((int)cstacktop+pagesize);
    mmap((char *)estacktop,pagesize,0,MAP_SHARED|MAP_FIXED,fd,0);
    estacktop=(struct lispenv *)((int)estacktop+pagesize);
    close(fd);
#endif
    allheapbottom=allheaptop+allheapsize/sizeof(int);
    heaptop=heap=allheaptop;
    heapbottom=heaptop+heapsize/sizeof(int);
  }
  else{
    if(ignored==1)
      fputs("Stack size of dump file is not changed\n",stderr);
    undumpsetup(dumpfile);
  }
  init_stackWM(lstackbottom);
  setup();
  return 0;
}

WORD dumpfile_f(int na, WORD *fp)
{
#ifdef NOMALLOC
  char buf[4*9];
  unsigned char *filename;
  WORD a;
  int fd;
  int file_heapused;
  int oldcurpos;

  if(na!=1)parerr();
  l(0)=value(terminal_input);
  oldcurpos=curpos(l(0));
  curpos(l(0))=recend(l(0)); /* clear terminal-input */
  checkstr(a,ag(0));
  filename=stringcodes(a);
  if((fd=open(filename,O_WRONLY|O_CREAT|O_TRUNC,0666))<0)
    return nil;
  while(allheaptop!=heaptop)
    gc(0,fp);
  file_heapused=(int)heap-(int)heaptop;
  memcpy(buf,&lstacktop,sizeof(WORD *));
  memcpy(buf+4,&lstacksize,sizeof(int));
  memcpy(buf+4*2,&bstacksize,sizeof(int));
  memcpy(buf+4*3,&cstacksize,sizeof(int));
  memcpy(buf+4*4,&estacksize,sizeof(int));
  memcpy(buf+4*5,&mallocsize,sizeof(int));
  memcpy(buf+4*6,&mallocptr,sizeof(int));
  memcpy(buf+4*7,&codeareasize,sizeof(int));
  memcpy(buf+4*8,&file_heapused,sizeof(int));
  write(fd,buf,4*9);
  write(fd,symroot,SYMROOTSIZE*sizeof(WORD));
  write(fd,root,ROOTSIZE*sizeof(WORD));
  write(fd,malloctop,mallocptr-malloctop);
  write(fd,codeareatop,codeareasize);
  write(fd,allheaptop,file_heapused);
  close(fd);
  l(0)=value(terminal_input);
  curpos(l(0))=oldcurpos;
#endif
  return true;
}  

#ifdef NOMALLOC
char *malloc(int size)
{
  char buf[256];
  char *oldmallocptr=mallocptr;
  int *ptr,*oldptr,*bestptr,*bestoldptr;
  int bestsize,tmpsize;

  if(malloccheck){
    sprintf(buf,"malloc 0x%x bytes\n",size);
    fputs(buf,stderr);fflush(stderr);
  }
  size=(size+3)&0xfffffffc;
  for(bestptr=NULL,bestsize=mallocsize,oldptr=NULL,ptr=(int *)malloctop;
      ptr!=NULL;
      oldptr=ptr,ptr= *(int **)(ptr+1)){
    if(malloccheck){
      sprintf(buf,"loop in malloc 0x%x,0x%x\n",ptr,*(int *)ptr);
      fputs(buf,stderr);fflush(stderr);
    }
    if((tmpsize= *(int *)ptr)>=size && bestsize>tmpsize){
      bestsize=tmpsize;
      bestoldptr=oldptr;
      bestptr=ptr;
    }
  }
  if(bestptr!=NULL){
    if(bestsize-size<2*sizeof(int)){
      *(bestoldptr+1)= *(bestptr+1);
      return (char *)bestptr+sizeof(int);
    }
    *(bestptr)=bestsize-size-sizeof(int);
    *(int *)((char *)bestptr+bestsize-size)=size;
    if(malloccheck){
      sprintf(buf,"malloc returns 0x%x\n",(char *)bestptr+bestsize-size+sizeof(int));
      fputs(buf,stderr);fflush(stderr);
    }
    return (char *)bestptr+bestsize-size+sizeof(int);
  }
  if(mallocptr+size+sizeof(int)>malloctop+mallocsize){
    sprintf(buf,"malloc exceed 0x%x byte over max size 0x%x bytes\n",
	    mallocptr+size+sizeof(int)-malloctop-mallocsize,
	    mallocsize);
    fputs(buf,stderr);
    fputs("Please extend malloc size\n",stderr);
    fflush(stderr);
    exit(1);
  }
  mallocptr+=size+sizeof(int);
  *(int *)oldmallocptr=size;
  if(malloccheck){
    sprintf(buf,"malloc returns 0x%x\n",oldmallocptr+sizeof(int));
    fputs(buf,stderr);fflush(stderr);
  }
  return oldmallocptr+sizeof(int);
}
void free(char *ptr)
{
  int *ptr1,*ptr2,*ptr3=(int *)(ptr-sizeof(int)),size;
  char buf[256];

  size= *ptr3;
  if(malloccheck){
    sprintf(buf,"free 0x%x,0x%x bytes\n",ptr,*ptr3);
    fputs(buf,stderr);fflush(stderr);
  }
  for(ptr1=(int *)malloctop,ptr2=NULL;ptr1!=NULL;ptr2=ptr1,ptr1= *(int **)(ptr1+1)){
    if(malloccheck){
      sprintf(buf,"loop in free 0x%x,0x%x\n",ptr1,*ptr1);
      fputs(buf,stderr);fflush(stderr);
    }
    if(ptr3>ptr2 && ptr3<ptr1){
      if((char *)ptr2+ *ptr2+sizeof(int)==(char *)ptr3 
	 && ptr2!=(int *)malloctop){
	*ptr2+=size+sizeof(int);
	ptr3=ptr2;
      }
      else 
	*(int **)(ptr2+1)=ptr3;
      if((char *)ptr3+size+sizeof(int)==(char *)ptr1){
	*ptr3+= *ptr1+sizeof(int);
	*(int **)(ptr3+1)= *(int **)(ptr1+1);
      }
      else
	*(int **)(ptr3+1)=ptr1;
      return;
    }
  }
  if((char *)ptr2+ *ptr2+sizeof(int)==(char *)ptr3 && ptr2!=(int *)malloctop){
    *ptr2+=size+sizeof(int);
    ptr3=ptr2;
  }
  else 
    *(int **)(ptr2+1)=ptr3;
  *(int **)(ptr3+1)=NULL;
}    
char *realloc(char *addr,int size)
{
  int oldsize= *(int *)(addr-4);
  char *newaddr;
  char buf[256];

  if(malloccheck){
    sprintf(buf,"realloc 0x%x bytes\n",size);
    fputs(buf,stderr);fflush(stderr);
  }
  newaddr=malloc(size);
  memcpy(newaddr,addr,oldsize);
  free(addr);
  return newaddr;
}
#endif





