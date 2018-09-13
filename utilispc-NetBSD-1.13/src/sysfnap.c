#include "constdef.h"
#include "defvar.h"
#include "/usr/cap/sys.c7/include/ccell.c7.h"

WORD getcid_f(na,fp)
WORD *fp;
{
  if(na!=0) parerr();
  return itofix(getcid());
}

WORD getncel_f(na,fp)
WORD *fp;
{
  if(na!=0) parerr();
  return itofix(getncel());
}

WORD getncelx_f(na,fp)
WORD *fp;
{
  if(na!=0) parerr();
  return itofix(getncelx());
}

WORD getncely_f(na,fp)
WORD *fp;
{
  if(na!=0) parerr();
  return itofix(getncely());
}

WORD sync_f(na,fp)
WORD *fp;
{
  WORD a;
  int no,stat;

  if(na!=2) parerr();
  no=fixtoi(slow_checkfix(a,ag(1)));
  stat=fixtoi(slow_checkfix(a,ag(0)));
  return itofix(sync(no,stat));
}

WORD cstat_f(na,fp)
WORD *fp;
{
  if(na!=0) parerr();
  return itofix(cstat());
}

WORD gstat_f(na,fp)
WORD *fp;
{
  if(na!=0) parerr();
  return itofix(gstat());
}

WORD pstat_f(na,fp)
WORD *fp;
{
  WORD a;
  int stat;

  if(na!=1) parerr();
  stat=fixtoi(slow_checkfix(a,ag(0)));
  return itofix(pstat(stat));
}

