/* @(#)sysfnerr.c	1.6 90/07/30 */

#include "constdef.h"
#include "defvar.h"

void err_all(char *msg, int na, WORD *fp)
{
  WORD a,a0,a1;

  PUTC('\n',value(terminal_output));
  PUTS(msg,value(terminal_output));
  PUTC('\n',value(terminal_output));
  FLUSH(value(terminal_output));
  a0=nil;
  if(cstack<cstackbottom-1)
    a1= *(cstack+1);
  else
    a1= nil;
  switch(na){
  case 2:
    a1=ag(0);a0=ag(1);break;
  case 1:
    a0=ag(0);
  }
  l(0)=a0;l(1)=value(terminal_output);
  princ_f(2,fp-2);
  PUTS(" -- ",value(terminal_output));
  if(tag(a1)==SYMBOL && tag(a=definition(a1))==OTHERS && otag(a)==CODE){
    l(0)=a1;l(1)=value(terminal_output);
    princ_f(2,fp-2);
  }
  else{
    l(0)=a1;l(1)=value(terminal_output);
    princ_f(2,fp-2);
  }
  l(0)=value(terminal_output);
  terpri_f(1,fp-1);
  funcall(value(break_v),0,fp);
  ret2break(fp);
}

WORD err_argument_type_f(int na, WORD *fp)
{
  err_all("@@@ illegal argument type",na,fp);
}

WORD err_buffer_overflow_f(int na, WORD *fp)
{
  err_all("@@@ string buffer overflow",na,fp);
}

WORD err_catch_f(int na, WORD *fp)
{
  err_all("@@@ catching structure not found",na,fp);
}

WORD err_end_of_file_f(int na, WORD *fp)
{
  err_all("@@@ end of file reached while reading",na,fp);
}

WORD err_floating_overflow_f(int na, WORD *fp)
{
  err_all("@@@ floating-point overflow",na,fp);
}

WORD err_function_f(int na, WORD *fp)
{
  err_all("@@@ illegal function ",na,fp);
}

WORD err_go_f(int na, WORD *fp)
{
  err_all("@@@ go label not found",na,fp);
}

WORD err_index_f(int na, WORD *fp)
{
  err_all("@@@ string or vector index out of range",na,fp);
}

WORD err_io_f(int na, WORD *fp)
{
  err_all("@@@ error in input/output",na,fp);
}

WORD err_number_of_arguments_f(int na, WORD *fp)
{
  err_all("@@@ mismatched number of arguments",na,fp);
}

WORD err_open_close_f(int na, WORD *fp)
{
  err_all("@@@ err in open/close",na,fp);
}

WORD err_read_f(int na, WORD *fp)
{
  err_all("@@@ illegal object read",na,fp);
}

WORD err_return_f(int na, WORD *fp)
{
  err_all("@@@ catching structure not found",na,fp);
}

WORD err_unbound_variable_f(int na, WORD *fp)
{
  err_all("@@@ unbound variable",na,fp);
}

WORD err_undefined_function_f(int na, WORD *fp)
{
  err_all("@@@ undefined function",na,fp);
}

WORD err_variable_f(int na, WORD *fp)
{
  err_all("@@@ illegal lambda/prog variable",na,fp);
}

WORD err_zero_division_f(int na, WORD *fp)
{
  err_all("@@@ division by zero",na,fp);
}

