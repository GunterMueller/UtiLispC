#define APLIB
#include "constdef.h"
#include "defvar.h"
#include "string.h"
#include "/usr/cap/sys.c7/include/ccell.c7.h"
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

int retint(ptr)
unsigned char *ptr;
{
	int c= *ptr++;
	c=(c<<8)+(*ptr++);
	c=(c<<8)+(*ptr++);
	c=(c<<8)+(*ptr++);
	return c;
}

int sys_open(filename,flag,mod)
char *filename;
{
	int flen=strlen(filename);
	int* buf=(int *)alloca(flen+20);
	int *msgp;
	int fd;

#if 0
	printf("open(%s,%d,%d)\n",filename,flag,mod);
	fflush(stdout);
#endif
	/*  debug(strcat("open:", filename));*/
	if(strncmp("cell",filename,4)==0){
		sscanf(filename,"cell%d",&fd);
		return fd + 256;
	}
	else{
		buf[0]=SYS_OPEN;
		buf[1]=getcid();
		buf[2]=flag;
		buf[3]=mod;
		memcpy(buf+4,filename,flen+1);
		h_send(0,buf,flen+20);
		msgp=(int *)h_recv(0);
		return retint(msgp);
	};
}

sys_read(fd,data,size)
char *data;
{
	char string[600];
	char string1[600];
	char string2[600];
	int* msgp;
	int buf[4];
	int rdsize;
	char buffer[600];
#if 0
	printf("read\n");
	fflush(stdout);
#endif
#if 0
        if(fixtoi(value(macro))>=1){
        sprintf(string2,"read cid=%d,fd=%d,last_input=%d\n",
           getcid(),fd,fixtoi(value(last_input)));
        host_write(1,string2,strlen(string2));
        }
#endif
	if(fd < 1280 && fd>=256){/*cell read*/
	  msgp=(int *)l_arecv(fd-256,ANY_TASK,ANY_TYPE);
	  rdsize=readmsg(buffer,sizeof(buffer));
	  memcpy(data,buffer+4,*(int *)buffer);
#if 0
        if(fixtoi(value(macro))>=2){
        host_write(1,data,*(int *)buffer);
        }
#endif
	  return retint(buffer);
	}
	else if(fd==0){
	  if(fixtoi(value(last_input))==4096)
	    return host_read(fd,data,size);
	  else if(fixtoi(value(last_input))==4095){
	    msgp=(int *)crecv();
	    rdsize=readmsg(buffer,sizeof(buffer));
	    value(last_input)=itofix(getmcid());
	    buffer[*(int *)buffer+4]=0;
	    memcpy(data,buffer+4,*(int *)buffer);
#if 0
            if(fixtoi(value(macro))>=2){
            host_write(1,data,*(int *)buffer);
        }
#endif
	    return retint(buffer);
	  }
	  else{
	    msgp=(int *)l_arecv(fixtoi(value(last_input)),ANY_TASK,ANY_TYPE);
	    rdsize=readmsg(buffer,sizeof(buffer));
	    memcpy(data,buffer+4,*(int *)buffer);
#if 0
            if(fixtoi(value(macro))>=2){
            host_write(1,data,*(int *)buffer);
        }
#endif
	    return retint(buffer);
	  }
	}
	else{
	        return host_read(fd,data,size);
	}
}

host_read(fd,data,size)
char *data;
{
	int* msgp;
	int buf[4];
	buf[0]=SYS_READ;
	buf[1]=getcid();
	buf[2]=fd;
	buf[3]=size;
	h_send(0,buf,16);
	msgp=(int *)h_recv(0);
	memcpy(data,msgp+1,retint(msgp));
#if 0
        if(fixtoi(value(macro))>=2){
        host_write(1,data,retint(msgp));
        }
#endif
#if 0
	printf("end of read(size=%d)\n",retint(msgp));
	fflush(stdout);
#endif
	return retint(msgp);
}

sys_write(fd,data,size)
char *data;

{
	char string[600];
	char string1[600];
	int *msgp;
	char buffer[600];
	if(size==0)return 0;

	if(fd < 1280 && fd>=256){/*cell write*/
	  memcpy(string+4,data,size);
	  *(int *)string=size;
	  l_asend(fd - 256,0,0,string,size+4);
	  return size;
	}
	else if(fd==1){
	  if(fixtoi(value(last_input))==4096) /* host write */
	    return host_write(fd,data,size);
	  if(fixtoi(value(last_input))==4095){ /*broad write*/
	    memcpy(string+4,data,size);
	    *(int *)string=size;
	    cbroad(0,0,string,size+4);
	    return size;
	  }
	  else{
	    memcpy(buffer+4,data,size);
	    buffer[size+4]=0;
	    *(int *)buffer=size;
	    l_asend(fixtoi(value(last_input)),0,0,buffer,size+4);
	    return size;
	  }
	}
        else{
                return host_write(fd,data,size);
	}
}

host_write(fd,data,size)
char *data;
{
	int *msgp,*buf=(int *)alloca((size+3)/4*4+16);
	buf[0]=SYS_WRITE;
	buf[1]=getcid();
	buf[2]=fd;
	buf[3]=size;
	memcpy(buf+4,data,size);
	h_send(0,buf,(size+3)/4*4+16);
	msgp=(int *)h_recv(0);
#if 0
	printf("end of write\n");
	fflush(stdout);
#endif
	return retint(msgp);
}

sys_close(fd)
{
	int buf[3],*msgp;
        if(fd>=256) return 0;
	buf[0]=SYS_CLOSE;
	buf[1]=getcid();
	buf[2]=fd;
	h_send(0,buf,12);
	msgp=(int *)h_recv(0);
	return retint(msgp);
}

sys_call(str)
char *str;
{
  int size=strlen(str)+3/4*4;
  int *msgp,*buf=(int *)alloca(8+size);
  buf[0]=SYS_CALL;
  buf[1]=getcid();
  strcpy(buf+2,str);
  h_send(0,buf,8+(strlen(str)+3)/4*4);
  msgp=(int *)h_recv(0);
  return retint(msgp);
}

sys_chdir(dir)
char *dir;
{
  int buf[1024],len,*msgp;

  buf[0]=SYS_CHDIR;
  buf[1]=getcid();
  len=strlen(dir);
  strcpy(buf+2,dir);
  h_send(0,buf,8+(len+4)/4*4);
  msgp=(int*)h_recv(0);
  return retint(msgp);
}

sys_errno()
{
  int buf[2],*msgp;

  buf[0]=SYS_ERRNO;
  buf[1]=getcid();
  h_send(0,buf,8);
  msgp=(int*)h_recv(0);
  return retint(msgp);
}

sys_getpid()
{
  int buf[2],*msgp;

  buf[0]=SYS_GETPID;
  buf[1]=getcid();
  h_send(0,buf,8);
  msgp=(int*)h_recv(0);
  return retint(msgp);
}

sys_time(timeval)
long *timeval;
{
  int buf[2],*msgp;

  buf[0]=SYS_TIME;
  buf[1]=getcid();
  h_send(0,buf,8);
  msgp=(int*)h_recv(0);
  if(timeval!=NULL)
    *timeval=retint(msgp);
  return retint(msgp);
}

int *sys_localtime(timeval)
long *timeval;
{
  int buf[3],*msgp;

  buf[0]=SYS_LOCALTIME;
  buf[1]=getcid();
  buf[2]= *timeval;
  h_send(0,buf,12);
  msgp=(int*)h_recv(0);
  return msgp+1;
}

sys_getenv(str)
char *str;
{
  int buf[1024],*msgp;
  
  buf[0]=SYS_GETENV;
  buf[1]=getcid();
  strcpy(buf+2,str);
  h_send(0,buf,8+(strlen(str)+4)/4*4);
  msgp=(int*)h_recv(0);
  if(retint(msgp)==0) return NULL;
  else return (int)(msgp+1);
}

sys_putenv(str)
char *str;
{
  int buf[1024],*msgp;
  
  buf[0]=SYS_PUTENV;
  buf[1]=getcid();
  strcpy(buf+2,str);
  h_send(0,buf,8+(strlen(str)+4)/4*4);
  msgp=(int*)h_recv(0);
  return retint(msgp);
}

sys_syscall(num,a0,a1,a2,a3,a4,a5,a6,a7)
{
  int i,j,len,a[8],buf[1024],*msgp;

  buf[0]=SYS_SYSCALL;
  buf[1]=getcid();
  buf[2]=num;
  a[0]=a0; a[1]=a1; a[2]=a2; a[3]=a3;
  a[4]=a4; a[5]=a5; a[6]=a6; a[7]=a7;
  for(j=3,i=0;i<8;i++)
    if((a[i]&3) || a[i]<(int)heaptop || a[i]>=(int)heapbottom ||
       *(int *)(a[i]-8)!=36 || *(int *)(a[i]-4)&15 ){
      buf[j++]= -1;
      buf[j++]=a[i];
    }
    else{
      buf[j++]= len=(strlen(a[i])+4)/4;
      strcpy(buf+j,a[i]);
      j+=len;
    }
  h_send(0,buf,j*4);
  msgp=(int *)h_recv(0);
  return retint(msgp);
}

void *sys_signal()
{
#if 0
	printf("signal\n");
	fflush(stdout);
#endif
}
sys_times(buf)
struct tms *buf;
{
	buf->tms_utime=0;
}

h_printf(str,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,
             a10,a11,a12,a13,a14,a15,a16,a17,a18,a19)
char *str;
{
  char buf[1024];

  sprintf(buf,str,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,
                  a10,a11,a12,a13,a14,a15,a16,a17,a18,a19);
  host_write(1,buf,strlen(buf));
}
