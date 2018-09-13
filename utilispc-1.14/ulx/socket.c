/* Copyright    Massachusetts Institute of Technology    1988	*/
/*
 * THIS IS AN OS DEPENDENT FILE! It should work on 4.2BSD derived
 * systems.  VMS and System V should plan to have their own version.
 *
 * This code was cribbed from lib/X/XConnDis.c.
 *
 * modification for utilisp by ken
 *
 */
#include <sys/types.h>
#include <sys/errno.h>
#include <sys/time.h>

#include <stdio.h>
#include <X11/Xos.h>
#include <X11/Xproto.h>
#include <errno.h>
#include <netinet/in.h>
#include <sys/ioctl.h>
#include <netdb.h> 
#include <sys/socket.h>


/* for Utilisp */
#include <constdef.h>
#include <defvar.h>
#include <cmpdef.h>

#include <sys/un.h>
#ifndef X_UNIX_PATH
#define X_UNIX_PATH "/tmp/.X11-unix/X"
#endif /* X_UNIX_PATH */
void bcopy();

WORD connect_to_server_f(), unixread_f(), unixwrite_f(),
        fd_read_bytes_f(), fd_write_bytes_f(),
	fd_close_f(),
	fd_wait_for_input_f();

static struct code_inf
  connect_to_server_info = { 0,0,0,0, connect_to_server_f, "connect-to-server " },
  fd_close_info = { 0,0,0,0, fd_close_f, "fd-close " },
  fd_wait_for_input_info = { 0,0,0,0, fd_wait_for_input_f, "fd-wait-for-input " },
  fd_read_bytes_info = { 0,0,0,0, fd_read_bytes_f, "fd-read-bytes " },
  fd_write_bytes_info = { 0,0,0,0, fd_write_bytes_f, "fd-write-bytes " };

static struct code_inf *inf_array[]={
	& connect_to_server_info,
	& fd_close_info,
	& fd_wait_for_input_info,
	& fd_read_bytes_info,
	& fd_write_bytes_info,
	NULL
};

/* 
 * Attempts to connect to server, given host and display. Returns file 
 * descriptor (network socket) or 0 if connection fails.
 */

WORD init_code_init(fp)
WORD *fp;
{
	WORD make_codepiece();
	return make_codepiece(inf_array, fp);
}

WORD init_code_socket(fp)
WORD *fp;
{
	WORD make_codepiece();
	return make_codepiece(inf_array, fp);
}

WORD connect_to_server_f (na, fp)	/* char *host, int display */
int na;
WORD *fp;
{
	WORD a;
	unsigned char *host;
	int display;

	struct sockaddr_in inaddr;	/* INET socket address. */
	struct sockaddr *addr;		/* address to connect to */
	struct hostent *host_ptr;
	int addrlen;			/* length of address */
	struct sockaddr_un unaddr;	/* UNIX socket address. */
	extern char *getenv();
	extern struct hostent *gethostbyname();
	int fd;				/* Network socket */

	if(na != 2) parerr();
	display = fixtoi(checkfix(a, ag(0)));
	host = stringcodes(checkstr(a, ag(1)));

	if ((host[0] == '\0') || strcmp("unix", host) == 0) {
	  /* Connect locally using Unix domain. */
	  unaddr.sun_family = AF_UNIX;
	  sprintf(unaddr.sun_path, "%s%d", X_UNIX_PATH, display);
	  /*
	  (void) strcpy(unaddr.sun_path, X_UNIX_PATH);
	  sprintf(&unaddr.sun_path[strlen(unaddr.sun_path)], "%d", display);
	  */
	  /*printf(unaddr.sun_path); fflush(stdout);*/
	  addr = (struct sockaddr *) &unaddr;
	  addrlen = strlen(unaddr.sun_path) + 3;
	  /*
	   * Open the network connection.
	   */
	  if ((fd = socket((int) addr->sa_family, SOCK_STREAM, 0)) < 0)
	      goto errret;
	} else
	{
	    /* Get the statistics on the specified host. */
	    if ((inaddr.sin_addr.s_addr = inet_addr(host)) == -1){
	      if ((host_ptr = gethostbyname(host)) == NULL){
		  /* No such host! */
		  errno = EINVAL;
		  goto errret;
	      }
	      /* Check the address type for an internet host. */
	      if (host_ptr->h_addrtype != AF_INET){
		  /* Not an Internet host! */
		  errno = EPROTOTYPE;
		  goto errret;
	      }
	      /* Set up the socket data. */
	      inaddr.sin_family = host_ptr->h_addrtype;
	      bcopy((char *)host_ptr->h_addr, 
		    (char *)&inaddr.sin_addr, 
		    sizeof(inaddr.sin_addr));
	  } 
	  else {
	      inaddr.sin_family = AF_INET;
	  }
	  addr = (struct sockaddr *) &inaddr;
	  addrlen = sizeof (struct sockaddr_in);
	  inaddr.sin_port = display + X_TCP_PORT;
	  inaddr.sin_port = htons(inaddr.sin_port);
	  /*
	   * Open the network connection.
	   */
	  if ((fd = socket((int) addr->sa_family, SOCK_STREAM, 0)) < 0){
	    goto errret;
	    /* make sure to turn off TCP coalescence */
#ifdef TCP_NODELAY
	    {
	      int mi = 1;
	      setsockopt (fd, IPPROTO_TCP, TCP_NODELAY, &mi, sizeof (int));
	    }
#endif
	  }
	}

	/*
	 * Changed 9/89 to retry connection if system call was interrupted.  This
	 * is necessary for multiprocessing implementations that use timers,
	 * since the timer results in a SIGALRM.	-- jdi
	 */
	while (connect(fd, addr, addrlen) == -1) {
	  if (errno != EINTR) {
	    (void) close (fd);
	    goto errret;
	  }
	}
	return itofix(fd);

      errret:
	return nil;
}

WORD fd_read_bytes_f(na, fp)	/* fixnum fd, string str, fixnum start, len */
WORD *fp;
{
    WORD a;

    int fd, len, readbytes, total;
    /* int buflen; */
    unsigned char *str;
    
    if(na != 4) parerr();
    
    fd = fixtoi(checkfix(a, ag(3)));
    str = stringcodes(checkstr(a, ag(2)));
    /* buflen = fixtoi(osize(a)); */
    str += fixtoi(checkfix(a, ag(1)));	/* start pos in str */
    len = fixtoi(checkfix(a, ag(0)));

    /* if(start + len >= buflen) some_error(); */
    total = 0;
    while (len > 0){
      readbytes = read(fd, str, len);
      total += readbytes; str += readbytes; len -= readbytes;
    }
    return total >= 0 ? itofix(total) : nil;
}

WORD fd_write_bytes_f(na, fp)
WORD *fp;
{
	WORD a;
	int fd, start, len;
	int wrotebytes;
	unsigned char *str;

	if(na != 4) parerr();

	fd = fixtoi(checkfix(a, ag(3)));
	str = stringcodes(checkstr(a, ag(2)));
	start = fixtoi(checkfix(a, ag(1)));
	len = fixtoi(checkfix(a, ag(0)));

	wrotebytes = write(fd, str + start, len);
	return wrotebytes >= 0 ? itofix(wrotebytes) : nil;
}

#if 0
WORD unixread_f(na, fp)		/* int fd, char *str ,int length */
WORD *fp;
{
	WORD a;

	int fd, n, len;
	unsigned char *str;

	if (na == 2){
	  len = fixtoi(osize(checkstr(a, ag(0))));
	  str = stringcodes(a);
	}
	else if(na == 3){
	  len = fixtoi(checkfix(a, ag(0)));
	  str = stringcodes(checkstr(a, ag(1)));
	  n = fixtoi(osize(a));
	  if(n < len) len = n;
	}
	else parerr();

	fd = fixtoi(checkfix(a, ag(na - 1)));

	n = read(fd, str, len);
	return n >=0 ? itofix(n) : nil;
}

WORD unixwrite_f(na, fp)	/* int fd, char *str ,int length */
WORD *fp;
{
	WORD a;

	int fd, n, len;
	unsigned char *str;

	if (na == 2){
	  len = fixtoi(osize(checkstr(a, ag(0))));
	  str = stringcodes(a);
	}
	else if(na == 3){
	  len = fixtoi(checkfix(a, ag(0)));
	  str = stringcodes(checkstr(a, ag(1)));
	  n = fixtoi(osize(a));
	  if(n < len) len = n;
	}
	else parerr();

	fd = fixtoi(checkfix(a, ag(na - 1)));

	n = write(fd, str, len);
	return n >=0 ? itofix(n) : nil;
}
#endif

WORD fd_close_f(na, fp)
WORD *fp;
{
	WORD a;
	int fd;

	if(na !=1) parerr();
	fd = fixtoi(checkfix(a, ag(0)));
	close(fd);
	return nil;
}

#define ERROR -1
#define INTERRUPT -2
#define TIMEOUT 0
#define SUCCESS 1


/* Length of array needed to hold all file descriptor bits */
#define CHECKLEN ((NUMBER_OF_FDS+8*sizeof(int)-1) / (8 * sizeof(int)))

extern int errno;

/*
 * This function waits for input to become available on 'fd'.  If timeout is
 * 0, wait forever.  Otherwise wait 'timeout' seconds.  If input becomes
 * available before the timer expires, return SUCCESS.  If the timer expires
 * return TIMEOUT.  If an error occurs, return ERROR.  If an interrupt occurs
 * while waiting, return INTERRUPT.
 */
#if 0
WORD fd_wait_for_input_f(na, fp)	/* fixnum fd,fixnum timeout */
WORD *fp;
{
    struct timeval timer;
    register int i;
    fd_set checkfds;

    int fd;
    int timeout;

    WORD a;

    if(na != 2) parerr();
    fd = fixtoi(checkfix(a,ag(1)));
    if(ag(0)==nil) timeout=-1;
    else timeout = fixtoi(checkfix(a,ag(0)));

    if (fd < 0 || fd >= FD_SETSIZE) {
	fprintf(stderr, "Bad file descriptor argument: %d to fd_wait_for_input\n", fd);
	fflush(stderr);
    }

    FD_ZERO(& checkfds);
    FD_SET(fd, &checkfds);

    if (timeout >= 0) {
	timer.tv_sec = timeout;
	timer.tv_usec = 0;
	i = select(FD_SETSIZE,& checkfds, (int *)0, (int *)0, &timer);
    } else
      i = select(FD_SETSIZE,& checkfds, (int *)0, (int *)0, (struct timeval *)0);

    if (i < 0)
      /* error condition */
      if (errno == EINTR)
	return itofix(INTERRUPT);
      else
	return itofix(ERROR);
    else if (i == 0)
      return itofix(TIMEOUT);
    else
      return itofix(SUCCESS);
}
#endif

WORD fd_wait_for_input_f(na, fp)	/* fixnum fd,fixnum timeout */
WORD *fp;
{
    struct timeval timer;
    register int i;
    fd_set checkfds;

    int fd;
    int timeout;
    int flag;

    WORD a, b;

    if(na != 2) parerr();

    if(ag(0)==nil) timeout=-1;
    else timeout = fixtoi(checkfix(a,ag(0)));

    FD_ZERO(& checkfds);
    if (tag(a=ag(1)) == FIXNUM) {
      flag = 1;
      fd = fixtoi(a);
      if (fd < 0 || fd >= FD_SETSIZE) {
	fprintf(stderr, "Bad file descriptor argument: %d to fd_wait_for_input\n", fd);
	fflush(stderr);
      }
      FD_SET(fd, &checkfds);
    } else {
      flag = 0;
      for (a = ag(1); !atom(a); a = cdr(a)) {
        fd = fixtoi(checkfix(b,car(a)));
        if (fd < 0 || fd >= FD_SETSIZE) {
	  fprintf(stderr, "Bad file descriptor argument: %d to fd_wait_for_input\n", fd);
	  fflush(stderr);
        }
        FD_SET(fd, &checkfds);
      }
    }

    if (timeout >= 0) {
	timer.tv_sec = timeout;
	timer.tv_usec = 0;
	i = select(FD_SETSIZE,& checkfds, (int *)0, (int *)0, &timer);
    } else
      i = select(FD_SETSIZE,& checkfds, (int *)0, (int *)0, (struct timeval *)0);

    if (i < 0)
      /* error condition */
      if (errno == EINTR)
	return itofix(INTERRUPT);
      else
	return itofix(ERROR);
    else if (i == 0)
      return itofix(TIMEOUT);
    else if (flag == 1)
      return itofix(SUCCESS);
    else {
      i = 0;
      for (fd = 0; fd < FD_SETSIZE; fd++)
	if (FD_ISSET(fd, & checkfds))
		l(i++) = itofix(fd);
      return allist(i, fp - i, nil);
    }
}
