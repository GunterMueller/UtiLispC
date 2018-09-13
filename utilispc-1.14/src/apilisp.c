#include <stdio.h>
#include <chost.c7.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>

#define SYS_OPEN  0
#define SYS_READ  1
#define SYS_WRITE 2
#define SYS_CLOSE 3
#define SYS_CALL 4
#define SYS_CHDIR 5
#define SYS_GETENV 6
#define SYS_TIME 7
#define SYS_GETPID 8
#define SYS_PUTENV 9
#define SYS_SYSCALL 10
#define SYS_ERRNO 11
#define SYS_LOCALTIME 12

host_main(ac,av)
int ac;
char **av;
{
  register char *cp;
  int ncelx = getncelx();
  int ncely = getncely();
  int ncel = getncel();
  int i,rval;
  int *msgp,ccid;
  struct stat stbuf1;
  struct stat stbuf2;
  int lispsys_size,fd;
  char* lispsys_buf;
  char **rest_av;
  int rest_ac,argsize,len;
  extern int errno;

  if((rest_av=(char **)malloc(ac*sizeof(char *)))==NULL) exit(1);
  rest_av[0]= *av++;
  rest_ac=1;
  while(--ac > 0){
    cp = *av++;
    if(cp[0] == '-'){
      switch(cp[1]){
      case 'N':
	ncel = atoi(*av++);ac--;
	switch(ncel){
	case 1024: ncelx=32; ncely=32; break;
	case  512: ncelx=32; ncely=16; break;
	case  256: ncelx=16; ncely=16; break;
	case  128: ncelx=16; ncely= 8; break;
	case   64: ncelx= 8; ncely= 8; break;
	case   32: ncelx= 8; ncely= 4; break;
	case   16: ncelx= 4; ncely= 4; break;
	case    8: ncelx= 4; ncely= 2; break;
	case    4: ncelx= 2; ncely= 2; break;
	case    2: ncelx= 2; ncely= 1; break;
	case    1: ncelx= 1; ncely= 1; break;
	default:   ncelx= 4; ncely= 2;}
	continue;
      case 'x':
	ncelx = atoi(*av++); ac--; continue;;
      case 'y':
	ncely = atoi(*av++); ac--; continue;
      }
    }
    rest_av[rest_ac++]= cp;;
  }
  cconfxy(ncelx,ncely);
  ccreat(UTILISP,0,NULL);
  cbroad(0,0,&rest_ac,4);
  for(i=0;i<rest_ac;i++){
    len=strlen(rest_av[i])+1;
    cbroad(0,0,&len,4);
    cbroad(0,0,rest_av[i],len);
  }
  stat(LISPSYS,&stbuf1);
  stat(APSYS,&stbuf2);
  lispsys_size=(int)stbuf1.st_size+(int)stbuf2.st_size;
  if((lispsys_buf=(char *)malloc(lispsys_size))==NULL){
    fprintf(stderr,"Failed in malloc\n");
    exit(1);
  }
  if((fd=open(LISPSYS,0))<0){
    fprintf(stderr,"Failed opening file %s\n",LISPSYS);
    exit(1);
  }
  if(read(fd,lispsys_buf,stbuf1.st_size)<0){
    fprintf(stderr,"Failed reading file %s\n",LISPSYS);
    exit(1);
  }
  close(fd);
  if((fd=open(APSYS,0))<0){
    fprintf(stderr,"Failed opening file %s\n",LISPSYS);
    exit(1);
  }
  if(read(fd,lispsys_buf+stbuf1.st_size,stbuf2.st_size)<0){
    fprintf(stderr,"Failed reading file %s\n",LISPSYS);
    exit(1);
  }
  close(fd);
  cbroad(0,0,&lispsys_size,4);
  cbroad(0,0,lispsys_buf,lispsys_size);
  while((msgp=(int *)crecv())!=NULL){
    ccid=msgp[1];
    switch (msgp[0]){
    case SYS_OPEN:{
      int fd,buf[2];
      fd=open(msgp+4,msgp[2],msgp[3]);
      buf[0]=fd;
      l_asend(ccid,0,0,buf,4);
      continue;
    }
    case SYS_CLOSE:{
      int buf[1];
      buf[0]=close(msgp[2]);
      l_asend(ccid,0,0,buf,4);
      continue;
    }
    case SYS_READ:{
      int size=msgp[3];
      int *buf=(int *)malloc(size+4);
      buf[0]=read(msgp[2],buf+1,size);
      l_asend(ccid,0,0,buf,size+4);
      free(buf);
      continue;
    }
    case SYS_WRITE:{
      int buf[1];
      buf[0]=write(msgp[2],msgp+4,msgp[3]);
      l_asend(ccid,0,0,buf,4);
      continue;
    }
    case SYS_CALL:{
     int rval, pid,buf[1];

#if 0
     if((pid=fork())<0){
       perror("vfork failed");
       rval= -1;
     }
     else if(pid==0){
       execl("/bin/csh","csh","-cef",msgp[2]);
       exit(0);
     }
     else
       wait(&rval);
     buf[0]=rval;
#else
     buf[0]=system((char *)(msgp+2));
#endif
     l_asend(ccid,0,0,buf,4);
     continue;
    }
    case SYS_CHDIR:{
      int buf[1];
      buf[0]=chdir(msgp+2);
      l_asend(ccid,0,0,buf,4);
      continue;
    }
    case SYS_ERRNO:{
      int buf[1];
      buf[0]=errno;
      l_asend(ccid,0,0,buf,4);
      continue;
    }
    case SYS_GETPID:{
      int buf[1];
      buf[0]=getpid();
      l_asend(ccid,0,0,buf,4);
      continue;
    }
    case SYS_TIME:{
      time_t timeval;
      int buf[1];

      time(&timeval);
      buf[0]=(int)timeval;
      l_asend(ccid,0,0,buf,4);
      continue;
    }
    case SYS_LOCALTIME:{
      time_t timeval;
      struct tm *tmbuf;
      int buf[10];

      timeval=msgp[2];
      tmbuf=localtime(&timeval);
      for(i=0;i<9;i++)
	buf[i+1]= *((int *)tmbuf+i);
      l_asend(ccid,0,0,buf,40);
      continue;
    }
    case SYS_GETENV:{
      char *str,*getenv();
      int size,buf[1024];

      if((str=getenv(msgp+2))==NULL){
	buf[0]=0;
	size=4;
      }
      else{
	buf[0]=1;
	strcpy(buf+1,str);
	size=4+(strlen(str)+4)*4/4;
      }
      l_asend(ccid,0,0,buf,size);
      continue;
    }
    case SYS_PUTENV:{
      int buf[1],size=strlen(msgp+2)+1;
      char *newenv=(char *)malloc(size);

      strcpy(newenv,msgp+2);
      buf[0]=putenv(newenv);
      l_asend(ccid,0,0,buf,4);
      continue;
    }
    case SYS_SYSCALL:{
      int args[8],j,rval,buf[1];
      for(j=3,i=0;i<8;i++)
	if(msgp[j]<0){
	  args[i]=msgp[j+1];
	  j+=2;
	}
	else{
	  printf("len=%d\n",msgp[j]);
	  args[i]= malloc(msgp[j]*4);
	  strcpy(args[i],msgp+j+1);
	  printf("str=%s\n",msgp+j+1);
	  j+=msgp[j]+1;
	}
      buf[0]=syscall(msgp[2],args[0],args[1],args[2],args[3],
		   args[4],args[5],args[6],args[7]);
      l_asend(ccid,0,0,buf,4);
      continue;
    }
    }
  }
  waitstable();
}

