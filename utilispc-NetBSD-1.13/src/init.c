/* @(#)init.c	1.7 90/07/30 */

#include "constdef.h"
#include "defvar.h"

void setup(void)
{
  WORD *fp=lstack,obvec;
  void init_var(void),init_subr(void),init_spec(void),init_deffunc(void);
  void initsig(void),init_rtable(void),readrc(char *,WORD *);

#ifndef OBJECTSPECIAL
  uderr=(WORD)udferr;
  uderr+=OTHERS; /* for cc -O2 bug */
#else
  uderr=alspec();
  spectop(uderr)=udferr;
#endif
#ifndef NOSYMROOT
  obvec=alvector(0x80,fp);
  setroot(nil,"nil",3,fp);
  value(nil)=nil;
  setroot(obvector,"obvector",8,fp);
  value(obvector)=obvec;
  internsym(obvector,fp);
  init_var();
  internsym(nil,fp);
  setroot(gc_hook,"gc-hook",7,fp);
  internsym(gc_hook,fp);
#else
  nil=alsym("nil",3,fp);
  value(nil)=nil;
  l(0)=alvector(0x80,fp);
  obvector=alsym("obvector",8,fp-1);
  value(obvector)=l(0);
  internsym(obvector,fp);
  init_var();
  internsym(nil);
  gc_hook=intern("gc-hook",7,fp);
#endif
  init_subr();
  init_spec();
  init_deffunc();
  /* read lispsys.l */
#ifdef HAVE_SIGNAL
  initsig();
#endif
  init_rtable();
#ifdef UTILISPLIB
  init_codes(fp);
  defeof_f(1,fp);
#else
  if(no_lispsys_l)
    defeof_f(1,fp);
  else
    readrc(LISPSYS,lstack);
#endif
}
#ifndef UTILISPLIB
void readrc(char *filename, WORD *fp)
{
  WORD defeof,a;
#ifdef AP1000
  extern int lispsys_size;
  extern char *lispsys_buf;
#endif

  defeof=alcode(0,fp);
  *(FP *)(defeof+CODETOP)=defeof_f;
  value(err_end_of_file)=defeof;
#ifdef AP1000
  l(0)=alnstring(lispsys_buf,lispsys_size,fp);
  l(1)=string_stream_f(1,fp-1);
#else
  checksym(a,system_lib_path);
  l(0)=value(a);
  l(1)=alstring(filename,fp-1);
  l(0)=string_append_f(2,fp-2);
  l(0)=stream_f(1,fp-1);
#endif
  inopen_f(1,fp-1);
  for(;;){
    eval(read_f(1,fp-1),fp-1);
  }
}
#endif

WORD defeof_f(int na, WORD *fp)
{
  WORD *tmpbstack=bstack,*tmpcstack=cstack,val;
  jmp_buf env;
  struct lispenv *tmpestack=estack;
  
  if(_setjmp(env)){
    lstack=lstackbottom;
    eval_unwind(tmpestack,fp);
    estack=tmpestack;
    while(bstack<tmpbstack){
      val=popb();
      value(popb())=val;
    }
    cstack=tmpcstack;
  }
  else {
#ifndef AP1000
    checkstr(val,value(prompt));
    write(fildes(sostream),stringcodes(val),fixtoi(osize(val)));
#endif
  }
  pushe();
  estack->envtag=TOPENV;
  estack->envobj=nil;
  estack->envbuf=(jmp_buf *)&env;
  value(standard_input)=value(terminal_input);
  value(standard_output)=value(terminal_output);
  cstack=cstackbottom;
  value(err_end_of_file)=err_end_of_file;
  for(;;)
    funcall(value(toplevel_v),0,lstack);
}

#ifdef HAVE_SIGNAL
void initsig(void)
{
  void sigint(),sigill();


#ifdef ALIGN_TYPE_CHECK
  {
    void sigbus();
    signal(SIGBUS,sigbus);
  }
#else
  signal(SIGBUS,sigill);
#endif

#ifdef INVALID_ACCESS_JUMP
  {
    void sigsegv();
    signal(SIGSEGV,sigsegv);
  }
#else
  signal(SIGSEGV,sigill);
#endif

#ifdef TAGGED_ARITHMETIC
  {
    void sigemt();
    signal(SIGEMT,sigemt);
  } 
#endif

#ifdef HAVE_IEEE_HANDLER
  {
    void sigfpe_div(), sigfpe_ovf();
    ieee_handler("set","divisidon",sigfpe_div);
    ieee_handler("set","overflow",sigfpe_ovf);
  }
#endif
  signal(SIGINT,sigint);
  signal(SIGILL,sigill);
}
#endif /* HAVE_SIGNAL */

#ifndef NOSYMROOT
struct var {
  WORD addr;
  char *name;
} vars[]={
  {lambda,"lambda"},{macro,"macro"},{true,"t"},
  {system_lib_path,"system_lib_path"},
  {system_header_path,"system_header_path"},
  {lower_case,"lower-case"},{upper_case,"upper-case"},{openfiles,"openfiles"},
  {standard_input,"standard-input"},{standard_output,"standard-output"},
  {terminal_input,"terminal-input"},{terminal_output,"terminal-output"},
  {prompt,"prompt"},{digits,"digits"},{printlevel,"printlevel"},
  {printlength,"printlength"},
  {default_obvector,"default-obvector"},{intern_v,"intern"},
  {readtable,"readtable"},{macrotable,"macrotable"},
  {default_readtable,"default-readtable"},
  {default_macrotable,"default-macrotable"},
  {unbound_variable,"*UnBoundVariable*"},
  {version,"version"},{appendopen,"appendopen"},
  {inopen,"inopen"},{outopen,"outopen"},{question,"?"},{qqq,"???"},
  {attention_handler,"attention-handler"},
  {utilisp_path,"utilisp_path"},
#ifdef AP1000
  interrupt_mask,"interrupt-mask"},{last_input,"last-input"},
  {message_handler,"message-handler"},
#endif
#else
struct var {
  WORD *addr;
  char *name;
} vars[]={
  {&lambda,"lambda"},{&macro,"macro"},{&true,"t"},
  {&system_lib_path,"system_lib_path"},
  {&system_header_path,"system_header_path"},
  {&lower_case,"lower-case"},{&upper_case,"upper-case"},{&openfiles,"openfiles"},
  {&standard_input,"standard-input"},{&standard_output,"standard-output"},
  {&terminal_input,"terminal-input"},{&terminal_output,"terminal-output"},
  {&prompt,"prompt"},{&digits,"digits"},{&printlevel,"printlevel"},
  {&printlength,"printlength"},
  {&default_obvector,"default-obvector"},{&intern_v,"intern"},
  {&readtable,"readtable"},{&macrotable,"macrotable"},
  {&default_readtable,"default-readtable"},
  {&default_macrotable,"default-macrotable"},
  {&unbound_variable,"*UnBoundVariable*"},
  {&version,"version"},{&appendopen,"appendopen"},
  {&inopen,"inopen"},{&outopen,"outopen"},{&question,"?"},{&qqq,"???"},
  {&attention_handler,"attention-handler"},
  {&utilisp_path,"utilisp_path"
#ifdef AP1000
  &interrupt_mask,"interrupt-mask"},{&last_input,"last_input"},
  {&message_handler,"message-handler"},
#endif
#endif
};

void init_var(void)
{
  int i;
  WORD *fp=lstack;
  extern char *version_string;
  char buf[256];
  char *utilisp_lib_path,*utilisp_header_path;

  for(i=0;i<sizeof(vars)/sizeof(struct var);i++){
#ifndef NOSYMROOT
    setroot(vars[i].addr,vars[i].name,strlen(vars[i].name),fp);
    internsym(vars[i].addr,fp);
#else
    *vars[i].addr=intern(vars[i].name,strlen(vars[i].name),fp);
#endif
  }
  value(prompt)=(topprompt=alstring("> ",fp));
  breakprompt=alstring("@ ",fp);
  if((utilisp_lib_path=getenv("UTILISP_LIB_PATH"))==NULL)
    utilisp_lib_path=UTILISP_LIB_PATH;
  value(system_lib_path)=alstring(utilisp_lib_path,fp);
  if((utilisp_header_path=getenv("UTILISP_HEADER_PATH"))==NULL)
    utilisp_header_path=UTILISP_HEADER_PATH;
  value(system_header_path)=alstring(utilisp_header_path,fp);
  value(utilisp_path)=alstring(UTILISP,fp);
#ifdef AP1000
  value(last_input)=itofix(4096);
  value(interrupt_mask)=itofix(0);
#endif
  value(version)=alstring(version_string,fp);
  value(digits)=itofix(7);
  value(printlevel)=itofix(4);
  value(printlength)=itofix(10);
  value(default_obvector)=value(obvector);
  value(true)=true;
  value(intern_v)=intern_v;
  for(i=0;i<256;i++)buf[i]=i;
  for(i='a';i<='z';i++)buf[i]=i+('A'-'a');
  value(upper_case)=alnstring(buf,256,fp);
  for(i=0;i<256;i++)buf[i]=i;
  for(i='A';i<='Z';i++)buf[i]=i+('a'-'A');
  value(upper_case)=alnstring(buf,256,fp);
}


struct subr{
  char *fcname;
  FP cdtop;
  int minag,maxag;
} subrs[]={
  {"backtrace",backtrace_f,0,1},
/* gc.c */
  {"gc",gc_f,0,1},
  {"gccount",gccount_f,0,0},
  {"gctime",gctime_f,0,0},
  {"extendheap",extendheap_f,1,1},
  {"extendheapK",extendheapK_f,1,1},
  {"heapsize",heapsize_f,0,0},
  {"heapsizeK",heapsizeK_f,0,0},
  {"heapfree",heapfree_f,0,0},
  {"symsize",symsize_f,0,0},
  {"symfree",symfree_f,0,0},
  {"heapfreeK",heapfreeK_f,0,0},
  {"heapfreeK",heapfreeK_f,0,0},
  {"fixsize",fixsize_f,0,0},
  {"fixfree",fixfree_f,0,0},
  {"stacksize",stacksize_f,0,0},
  {"stack-used",stack_used_f,0,0},
  {"stack-bottom",stack_bottom_f,0,0},
  {"stack-top",stack_top_f,0,0},
  {"stack-base",stack_base_f,0,0},
  {"othersize",othersize_f,0,0},
  {"otherfree",otherfree_f,0,0},
  {"init-stackWM",init_stackWM_f,0,0},
  {"stackWM",stackWM_f,0,0},
/* sysfncd.c */
  {"getd",getd_f,1,1},
  {"putd",putd_f,2,2},
  {"make-undefined",make_undefined_f,1,1},
  {"definedp",definedp_f,1,1},
#ifndef NO_DYNAMIC_LOAD
  {"system-code-load",system_code_load_f,1,-1},
#endif
  {"specialp",specialp_f,1,1},
  {"funcname",funcname_f,1,1},
  {"codesize",codesize_f,1,1},
#ifndef NO_DYNAMIC_LOAD
  {"program-link",program_link_f,1,1},
#endif
  {"maxarg",maxarg_f,1,1},
  {"minarg",minarg_f,1,1},
/* sysfneval.c */
  {"eval",eval_f,1,1},
  {"apply",apply_f,2,2},
  {"funcall",funcall_f,1,-1},
/* sysfnerr.c */
  {"oldvalue",oldvalue_f,0,1},
/* sysfnfix.c */
  {"0=",fixzerop_f,1,1},
  {"0<",fixplusp_f,1,1},
  {"0>",fixminp_f,1,1},
  {"=",fixeq_f,2,2},
  {"#",fixneq_f,2,2},
  {"<>",fixneq_f,2,2},
  {">",fixgt_f,1,-1},
  {">=",fixge_f,1,-1},
  {"<",fixlt_f,1,-1},
  {"<=",fixle_f,1,-1},
  {"-",fixdifference_f,1,-1},
  {"+",fixplus_f,0,-1},
  {"*",fixtimes_f,0,-1},
  {"/",fixquotient_f,1,-1},
  {"\\",fixremainder_f,2,2},
  {"1+",fixadd1_f,1,1},
  {"1-",fixsub1_f,1,1},
  {"logor",logor_f,0,-1},
  {"logand",logand_f,0,-1},
  {"logxor",logxor_f,0,-1},
  {"logshift",logshift_f,2,2},
  {"^",fixexpt_f,2,2},
#ifndef NO_FLONUM
/* sysfnflo.c */
  {"0=$",flozerop_f,1,1},
  {"0<$",floplusp_f,1,1},
  {"0>$",flominsp_f,1,1},
  {"=$",floeq_f,2,2},
  {"#$",floneq_f,2,2},
  {"-$",flodifference_f,1,-1},
  {"<>$",floneq_f,2,2},
  {"+$",floplus_f,0,-1},
  {"*$",flotimes_f,0,-1},
  {"/$",floquotient_f,1,-1},
  {"\\$",floremainder_f,2,2},
  {"1+$",floadd1_f,1,1},
  {"1-$",flosub1_f,1,1},
  {"fix",fix_f,1,1},
  {"float",float_f,1,1},
  {">$",flogt_f,1,-1},
  {">=$",floge_f,1,-1},
  {"<$",flolt_f,1,-1},
  {"<=$",flole_f,1,-1},
#endif
/* sysfnhash.c */
  {"hash",hash_f,1,1},
/* sysfnio.c */
  {"read",read_f,0,1},
  {"prin1",prin1_f,1,2},
  {"print",print_f,1,2},
  {"princ",princ_f,1,2},
  {"terpri",terpri_f,0,1},
  {"flush",flush_f,0,1},
  {"cursor",cursor_f,0,1},
  {"colleft",colleft_f,0,1},
  {"tab",tab_f,1,2},
  {"linelength",linelength_f,0,1},
  {"intern",intern_f,1,2},
  {"intern-soft",intern_soft_f,1,2},
  {"tyi",tyi_f,0,1},
  {"tyipeek",tyipeek_f,0,1},
  {"readch",readch_f,0,1},
  {"tyo",tyo_f,1,2},
  {"stream",stream_f,1,1},
  {"inopen",inopen_f,1,1},
  {"appendopen",appendopen_f,1,1},
  {"outopen",outopen_f,1,1},
  {"close",close_f,1,1},
  {"stream-mode",stream_mode_f,1,1},
  {"alloc",alloc_f,1,1},
  {"readmacro",readmacro_f,2,4},
  {"readline",readline_f,0,1},
  {"current-line",current_line_f,0,1},
  {"skipline",skipline_f,0,1},
  {"single-character",single_character_f,1,2},
  {"atomlength",atomlength_f,1,1},
  {"string-stream",string_stream_f,1,1},
  {"string-stream-index",string_stream_index_f,1,1},
  {"string-stream-limit",string_stream_limit_f,1,1},
  {"charleft",charleft_f,0,1},
  {"print:object",print_object_f,1,2},
/* sysfnlist.c */
  {"cr",cr_f,1,1},
  {"fifth",fifth_f,1,1},
  {"sixth",sixth_f,1,1},
  {"seventh",seventh_f,1,1},
  {"first",car_f,1,1},
  {"second",cadr_f,1,1},
  {"third",caddr_f,1,1},
  {"fourth",cadddr_f,1,1},
  {"caar",caar_f,1,1},
  {"cadr",cadr_f,1,1},
  {"cdar",cdar_f,1,1},
  {"cddr",cddr_f,1,1},
  {"caaar",caaar_f,1,1},
  {"caadr",caadr_f,1,1},
  {"cadar",cadar_f,1,1},
  {"caddr",caddr_f,1,1},
  {"cdaar",cdaar_f,1,1},
  {"cdadr",cdadr_f,1,1},
  {"cddar",cddar_f,1,1},
  {"cdddr",cdddr_f,1,1},
  {"caaaar",caaaar_f,1,1},
  {"caaadr",caaadr_f,1,1},
  {"caadar",caadar_f,1,1},
  {"caaddr",caaddr_f,1,1},
  {"cadaar",cadaar_f,1,1},
  {"cadadr",cadadr_f,1,1},
  {"caddar",caddar_f,1,1},
  {"cadddr",cadddr_f,1,1},
  {"cdaaar",cdaaar_f,1,1},
  {"cdaadr",cdaadr_f,1,1},
  {"cdadar",cdadar_f,1,1},
  {"cdaddr",cdaddr_f,1,1},
  {"cddaar",cddaar_f,1,1},
  {"cddadr",cddadr_f,1,1},
  {"cdddar",cdddar_f,1,1},
  {"cddddr",cddddr_f,1,1},
  {"car",car_f,1,1},
  {"cdr",cdr_f,1,1},
  {"cons",cons_f,2,2},
  {"ncons",ncons_f,1,1},
  {"xcons",xcons_f,2,2},
  {"copy",copy_f,1,1},
  {"last",last_f,1,1},
  {"length",length_f,1,1},
  {"nth",nth_f,2,2},
  {"nthcdr",nthcdr_f,2,2},
  {"list",list_f,0,-1},
  {"list*",list__f,1,-1},
  {"append",append_f,0,-1},
  {"reverse",reverse_f,1,1},
  {"nconc",nconc_f,0,-1},
  {"nreverse",nreverse_f,1,1},
  {"rplaca",rplaca_f,2,2},
  {"rplacd",rplacd_f,2,2},
  {"subst",subst_f,3,3},
  {"memq",memq_f,2,2},
  {"member",member_f,2,2},
  {"mem",mem_f,3,3},
  {"delq",delq_f,2,3},
  {"remq",remq_f,2,3},
  {"every",every_f,2,2},
  {"some",some_f,2,2},
  {"assq",assq_f,2,2},
  {"assoc",assoc_f,2,2},
  {"ass",ass_f,3,3},
/* sysfnmap.c */
  {"map" ,map_f,2,2},
  {"mapc",mapc_f,2,2},
  {"maplist",maplist_f,2,2},
  {"mapcar",mapcar_f,2,2},
  {"mapcan",mapcan_f,2,2},
  {"mapcon",mapcon_f,2,2},
  {"mapv",mapv_f,2,2},
  {"mapvector",mapvector_f,2,2},
/* sysfnmis.c */
  {"quit",quit_f,0,0},
  {"abend",abend_f,0,0},
  {"time",time_f,0,2},
  {"address",address_f,1,1},
  {"peek",peek_f,2,2},
  {"unbreak",unbreak_f,0,-1},
  {"rand",rand_f,0,0},
  {"srand",srand_f,1,1},
/* sysfnnum.c */
  {"zerop",zerop_f,1,1},
  {"plusp",plusp_f,1,1},
  {"minusp",minusp_f,1,1},
  {"oddp",oddp_f,1,1},
  {"plus",plus_f,0,-1},
  {"difference",difference_f,1,-1},
  {"times",times_f,0,-1},
  {"quotient",quotient_f,1,-1},
  {"remainder",remainder_f,2,2},
  {"add1",add1_f,1,1},
  {"sub1",sub1_f,1,1},
  {"abs",abs_f,1,1},
  {"minus",minus_f,1,1},
#ifndef NO_FLONUM
  {"sin",sin_f,1,1},
  {"cos",cos_f,1,1},
  {"tan",tan_f,1,1},
  {"arcsin",arcsin_f,1,1},
  {"arccos",arccos_f,1,1},
  {"arctan",arctan_f,1,1},
  {"sqrt",sqrt_f,1,1},
  {"log",log_f,1,1},
  {"log10",log10_f,1,1},
  {"exp",exp_f,1,1},
  {"^$",exptflo_f,2,2},
  {"expt",expt_f,2,2},
#endif
  {"greaterp",greaterp_f,0,-1},
  {"lessp",lessp_f,0,-1},
  {"max",max_f,1,-1},
  {"min",min_f,1,-1},
/* sysfnpred.c */
  {"atom",atom_f,1,1},
  {"numberp",numberp_f,1,1},
  {"stringp",stringp_f,1,1},
  {"vectorp",vectorp_f,1,1},
  {"referencep",referencep_f,1,1},
  {"symbolp",symbolp_f,1,1},
  {"predefinedp",predefinedp_f,1,1},
  {"listp",consp_f,1,1},
  {"streamp",streamp_f,1,1},
  {"consp",consp_f,1,1},
  {"codep",codep_f,1,1},
  {"predef",predef_f,1,1},
  {"eq",eq_f,2,2},
  {"neq",neq_f,2,2},
  {"equal",equal_f,2,2},
  {"null",null_f,1,1},
  {"not",null_f,1,1},
  {"fixp",fixp_f,1,1},
  {"string-streamp",string_streamp_f,1,1},
#ifndef NO_FLONUM
  {"floatp",floatp_f,1,1},
#endif
  {"integerp",integerp_f,1,1},
#ifndef NO_BIGNUM
  {"bigp",bigp_f,1,1},
#endif
/* sysfnspec.c */
  {"return",return_f,0,-1},
  {"throw",throw_f,1,-1},
  {"exit",exit_f,0,-1},
/* sysfnstg.c */
  {"character",character_f,1,1},
  {"string",string_f,1,1},
  {"string-length",string_length_f,1,1},
  {"string-lessp",string_lessp_f,2,2},
  {"substring",substring_f,1,3},
  {"string-append",string_append_f,0,-1},
  {"string-reverse",string_reverse_f,1,1},
  {"string-nreverse",string_nreverse_f,1,1},
  {"string-search-char",string_search_char_f,2,3},
  {"string-search-not-char",string_search_not_char_f,2,3},
  {"string-search",string_search_f,2,3},
  {"getchar",getchar_f,2,2},
  {"sref",sref_f,2,2},
  {"sset",sset_f,3,3},
  {"string-equal",string_equal_f,2,2},
  {"cutout",cutout_f,3,3},
  {"spread",spread_f,2,2},
  {"translate",translate_f,2,2},
  {"string-amend",string_amend_f,2,3},
  {"string-amend-or",string_amend_or_f,2,3},
  {"string-amend-xor",string_amend_xor_f,2,3},
  {"string-amend-and",string_amend_and_f,2,3},
  {"make-string",make_string_f,1,2},
  {"bset",bset_f,3,3},
  {"bref",bref_f,2,2},
  {"string-match",string_match_f,2,2},
/* sysfnsym.c */
  {"set",set_f,2,2},
  {"make-unbound",make_unbound_f,1,1},
  {"boundp",boundp_f,1,1},
  {"get",get_f,2,2},
  {"putprop",putprop_f,3,3},
  {"remprop",remprop_f,2,2},
  {"plist",plist_f,1,1},
  {"setplist",setplist_f,2,2},
  {"pname",pname_f,1,1},
  {"setpname",setpname_f,2,2},
  {"symbol",symbol_f,1,1},
  {"symbol-copy",symbol_copy_f,1,1},
  {"gensym",gensym_f,0,2},
/* sysfnsys.c */
  {"systemcall",call_f,1,1},
  {"systemchdir",cd_f,1,1},
  {"systemgetenv",getenv_f,1,1},
  {"systemputenv",putenv_f,2,2},
  {"syscall",syscall_f,1,-1},
  {"errno",errno_f,0,0},
  {"getpid",getpid_f,0,0},
  {"localtime",localtime_f,1,1},
  {"argv",argv_f,0,0},
/* sysfnvec.c */
  {"vref",vref_f,2,2},
  {"vset",vset_f,3,3},
  {"vector",vector_f,1,2},
  {"vector-length",vector_length_f,1,1},
  {"reference",reference_f,2,2},
  {"deref",deref_f,1,1},
  {"setref",setref_f,2,2},
  {"referred-vector",referred_vector_f,1,1},
  {"referred-index",referred_index_f,1,1},
  {"fill-vector",fill_vector_f,2,2},
  {"dumpfile",dumpfile_f,1,1},
#ifdef AP1000
/* sysfnap.c */
  {"getcid",getcid_f,0,0},
  {"getncel",getncel_f,0,0},
  {"getncelx",getncelx_f,0,0,}
  {"getncely",getncely_f,0,0},
  {"sync",sync_f,2,2,}
  {"cstat",cstat_f,0,0},
  {"gstat",gstat_f,0,0,}
  {"pstat",pstat_f,1,1},
#endif
};
struct specdef{
  char *fcname;
  FP cdtop;
} specs[]={
  {"and",and_e},{"or",or_e},{"cond",cond_e},{"selectq",selectq_e},
  {"prog",prog_e},{"catch",catch_e},{"function",quote_e},
  {"go",go_e},{"comment",comment_e},{"progn",progn_e},
  {"prog1",prog1_e},{"prog2",prog2_e},{"push",push_e},{"pop",pop_e},{"setq",setq_e},
  {"loop",loop_e},{"match",match_e},{"let",let_e},{"lets",lets_e},
  {"unwind-protect",unwind_protect_e},
};
	


void init_subr(void)
{
  int i;
  WORD ptr,aptr,*fp=lstack;

  for(i=0;i<sizeof(subrs)/sizeof(struct subr);i++){
    ptr=alcode(0,fp);
    codetop(ptr)=subrs[i].cdtop;
    l(0)=ptr;
    aptr=intern(subrs[i].fcname,strlen(subrs[i].fcname),fp-1);
    ptr=l(0);
    funcname(ptr)= aptr;
    minarg(ptr)=itofix(subrs[i].minag);
    maxarg(ptr)=itofix(subrs[i].maxag);
    definition(aptr)=ptr;
  }
}
WORD err_argument_type_f(),err_buffer_overflow_f(),err_catch_f();
WORD err_end_of_file_f(),err_floating_overflow_f(),err_function_f();
WORD err_go_f(),err_index_f(),err_io_f(),err_number_of_arguments_f();
WORD err_open_close_f(),err_read_f(),err_return_f(),err_unbound_variable_f();
WORD err_undefined_function_f(),err_variable_f(),err_zero_division_f();
WORD print_fixnum_f(),print_symbol_f(),print_cons_f();
WORD print_flonum_f(),print_reference_f();
#ifndef NO_BIGNUM
WORD print_bignum_f();
#endif
WORD print_vector_f(),print_string_f(),print_stream_f(),print_code_f();
#ifndef NOSYMROOT
struct deffunc{
  WORD var;
  char *fcname;
  FP cdtop;
  int minag,maxag;
} deffuncs[]={
  {toplevel_v,"toplevel",toplevel_f,0,0},
  {utilisp_v,"utilisp",utilisp_f,0,0},
  {break_v,"break",break_f,0,0},
  {err_argument_type,"err:argument-type",err_argument_type_f,1,2},
  {err_buffer_overflow,"err:buffer-overflow",err_buffer_overflow_f,1,2},
  {err_catch,"err:catch",err_catch_f,1,2},
  {err_end_of_file,"err:end-of-file",err_end_of_file_f,1,2},
#ifndef NO_FLONUM
  {err_floating_overflow,"err:floating-overflow",err_floating_overflow_f,1,2},
#endif
  {err_function,"err:function",err_function_f,1,2},
  {err_go,"err:go",err_go_f,1,2},
  {err_index,"err:index",err_index_f,1,2},
  {err_io,"err:io",err_io_f,1,2},
  {err_number_of_arguments,"err:number-of-arguments",err_number_of_arguments_f,1,2},
  {err_open_close,"err:open-close",err_open_close_f,1,2},
  {err_read,"err:read",err_read_f,1,2},
  {err_return,"err:return",err_return_f,1,2},
  {err_unbound_variable,"err:unbound-variable",err_unbound_variable_f,1,2},
  {err_undefined_function,"err:undefined-function",err_undefined_function_f,1,2},
  {err_variable,"err:variable",err_variable_f,1,2},
  {err_zero_division,"err:zero-division",err_zero_division_f,1,2},
  {readquote,"readquote",readquote_f,0,0},
  {print_fixnum,"print:fixnum",print_fixnum_f,1,2},
  {print_symbol,"print:symbol",print_symbol_f,1,2},
  {print_cons,"print:cons",print_cons_f,1,2},
#ifndef NO_FLONUM
  {print_flonum,"print:flonum",print_flonum_f,1,2},
#endif
#ifndef NO_BIGNUM
  {print_bignum,"print:bignum",print_bignum_f,1,2},
#endif
  {print_reference,"print:reference",print_reference_f,1,2},
  {print_vector,"print:vector",print_vector_f,1,2},
  {print_string,"print:string",print_string_f,1,2},
  {print_stream,"print:stream",print_stream_f,1,2},
  {print_code,"print:code",print_code_f,1,2}
#else
struct deffunc{
  WORD *var;
  char *fcname;
  FP cdtop;
  int minag,maxag;
} deffuncs[]={
  {&toplevel_v,"toplevel",toplevel_f,0,0},
  {&utilisp_v,"utilisp",utilisp_f,0,0},
  {&break_v,"break",break_f,0,0},
  {&err_argument_type,"err:argument-type",err_argument_type_f,1,2},
  {&err_buffer_overflow,"err:buffer-overflow",err_buffer_overflow_f,1,2},
  {&err_catch,"err:catch",err_catch_f,1,2},
  {&err_end_of_file,"err:end-of-file",err_end_of_file_f,1,2},
#ifndef NO_FLONUM
  {&err_floating_overflow,"err:floating-overflow",err_floating_overflow_f,1,2},
#endif
  {&err_function,"err:function",err_function_f,1,2},
  {&err_go,"err:go",err_go_f,1,2},
  {&err_index,"err:index",err_index_f,1,2},
  {&err_io,"err:io",err_io_f,1,2},
  {&err_number_of_arguments,"err:number-of-arguments",err_number_of_arguments_f,1,2},
  {&err_open_close,"err:open-close",err_open_close_f,1,2},
  {&err_read,"err:read",err_read_f,1,2},
  {&err_return,"err:return",err_return_f,1,2},
  {&err_unbound_variable,"err:unbound-variable",err_unbound_variable_f,1,2},
  {&err_undefined_function,"err:undefined-function",err_undefined_function_f,1,2},
  {&err_variable,"err:variable",err_variable_f,1,2},
  {&err_zero_division,"err:zero-division",err_zero_division_f,1,2},
  {&readquote,"readquote",readquote_f,0,}0
#endif
};

void init_deffunc(void)
{
  int i;
  WORD ptr,aptr,*fp=lstack;

  for(i=0;i<sizeof(deffuncs)/sizeof(struct deffunc);i++){
    ptr=alcode(0,fp);
    codetop(ptr)=deffuncs[i].cdtop;
#ifndef NOSYMROOT
    aptr=deffuncs[i].var;
    setroot(aptr,deffuncs[i].fcname,strlen(deffuncs[i].fcname),lstack);
    internsym(aptr,lstack);
#else
    *(deffuncs[i].var)=aptr=intern(deffuncs[i].fcname,strlen(deffuncs[i].fcname),lstack);
#endif
    funcname(ptr)=aptr;
    minarg(ptr)=itofix(deffuncs[i].minag);
    maxarg(ptr)=itofix(deffuncs[i].maxag);
    definition(aptr)=ptr;
    value(aptr)=aptr;
  }
  value(attention_handler)=break_v;
  value(toplevel_v)=utilisp_v;
}

void init_spec(void)
{
  int i;
  WORD ptr,aptr,*fp=lstack;

  for(i=0;i<sizeof(specs)/sizeof(struct specdef);i++){
    aptr=intern(specs[i].fcname,strlen(specs[i].fcname),lstack);
#ifndef OBJECTSPECIAL
    if(tag((WORD)specs[i].cdtop)!=0){ /* fatal error */
      fprintf(stderr,"Function address of Special form must be word aligned\n");
      exit(1);
    }
    ptr=((char *)specs[i].cdtop+OTHERS);
#else
    ptr=alspec(fp);
    spectop(ptr)=specs[i].cdtop;
    funcname(ptr)=aptr;
#endif
    definition(aptr)=ptr;
  }
#ifndef NOSYMROOT
  setroot(quote,"quote",5,fp);
  internsym(quote,fp);
#else
  quote=intern("quote",5,fp);
#endif
#ifndef OBJECTSPECIAL
  i=(int)quote_e;
  i+=OTHERS;
  definition(quote)=(WORD)i;
#else
  ptr=alspec(fp);
  spectop(ptr)=quote_e;
  funcname(ptr)=quote;
  definition(quote)=(WORD)ptr;
#endif
}
