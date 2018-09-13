/* @(#)defvar.h	1.6 90/07/30 */

#ifdef MAIN
#define extern
#endif
extern int argc;
extern char **argv;
#define LOOPENV 0
#define PROGENV 1
#define CATCHENV 2
#define TOPENV 3
#define BREAKENV 4
#define UNWINDENV 5
/* 基本的なシンボルア君外�弘� */
#ifndef NOSYMROOT
#define nil (WORD)((char *)symroot+SYMBOL)
#define true (WORD)((char *)symroot+SYMBOL+16*1)
#define lambda (WORD)((char *)symroot+SYMBOL+16*2)
#define macro (WORD)((char *)symroot+SYMBOL+16*3)
#define obvector (WORD)((char *)symroot+SYMBOL+16*4)
#define system_lib_path (WORD)((char *)symroot+SYMBOL+16*5)
#define lower_case (WORD)((char *)symroot+SYMBOL+16*6)
#define upper_case (WORD)((char *)symroot+SYMBOL+16*7)
#define openfiles (WORD)((char *)symroot+SYMBOL+16*8)
#define standard_input (WORD)((char *)symroot+SYMBOL+16*9)
#define standard_output (WORD)((char *)symroot+SYMBOL+16*10)
#define terminal_input (WORD)((char *)symroot+SYMBOL+16*11)
#define terminal_output (WORD)((char *)symroot+SYMBOL+16*12)
#define prompt (WORD)((char *)symroot+SYMBOL+16*13)
#define digits (WORD)((char *)symroot+SYMBOL+16*14)
#define printlevel (WORD)((char *)symroot+SYMBOL+16*15)
#define printlength (WORD)((char *)symroot+SYMBOL+16*16)
#define intern_v (WORD)((char *)symroot+SYMBOL+16*17)
#define readtable (WORD)((char *)symroot+SYMBOL+16*18)
#define macrotable (WORD)((char *)symroot+SYMBOL+16*19)
#define default_readtable (WORD)((char *)symroot+SYMBOL+16*20)
#define default_macrotable (WORD)((char *)symroot+SYMBOL+16*21)
#define default_obvector (WORD)((char *)symroot+SYMBOL+16*22)
#define attention_handler (WORD)((char *)symroot+SYMBOL+16*23)
#define quote (WORD)((char *)symroot+SYMBOL+16*24)
#define break_v (WORD)((char *)symroot+SYMBOL+16*25)
#define toplevel_v (WORD)((char *)symroot+SYMBOL+16*26)
#define utilisp_v (WORD)((char *)symroot+SYMBOL+16*27)
/* エラーを扱う関数 */
#define err_argument_type (WORD)((char *)symroot+SYMBOL+16*28)
#define err_buffer_overflow (WORD)((char *)symroot+SYMBOL+16*29)
#define err_catch (WORD)((char *)symroot+SYMBOL+16*30)
#define err_end_of_file (WORD)((char *)symroot+SYMBOL+16*31)
#define err_floating_overflow (WORD)((char *)symroot+SYMBOL+16*32)
#define err_function (WORD)((char *)symroot+SYMBOL+16*33)
#define err_go (WORD)((char *)symroot+SYMBOL+16*34)
#define err_index (WORD)((char *)symroot+SYMBOL+16*35)
#define err_io (WORD)((char *)symroot+SYMBOL+16*36)
#define err_number_of_arguments (WORD)((char *)symroot+SYMBOL+16*37)
#define err_open_close (WORD)((char *)symroot+SYMBOL+16*38)
#define err_return (WORD)((char *)symroot+SYMBOL+16*39)
#define err_unbound_variable (WORD)((char *)symroot+SYMBOL+16*40)
#define err_undefined_function (WORD)((char *)symroot+SYMBOL+16*41)
#define err_variable (WORD)((char *)symroot+SYMBOL+16*42)
#define err_zero_division (WORD)((char *)symroot+SYMBOL+16*43)
#define err_read (WORD)((char *)symroot+SYMBOL+16*44)


/* 補遺 */
#define unbound_variable (WORD)((char *)symroot+SYMBOL+16*45)
#define inopen (WORD)((char *)symroot+SYMBOL+16*46)
#define appendopen (WORD)((char *)symroot+SYMBOL+16*47)
#define outopen (WORD)((char *)symroot+SYMBOL+16*48)
#define question (WORD)((char *)symroot+SYMBOL+16*49)
#define qqq (WORD)((char *)symroot+SYMBOL+16*50)
#define version (WORD)((char *)symroot+SYMBOL+16*51)
#define readquote (WORD)((char *)symroot+SYMBOL+16*52)
#define utilisp_path (WORD)((char *)symroot+SYMBOL+16*53)

/* print を funcallによって行なうように改造 */
#define print_fixnum (WORD)((char *)symroot+SYMBOL+16*54)
#define print_symbol (WORD)((char *)symroot+SYMBOL+16*55)
#define print_cons (WORD)((char *)symroot+SYMBOL+16*56)
#define print_flonum (WORD)((char *)symroot+SYMBOL+16*57)
#define print_bignum (WORD)((char *)symroot+SYMBOL+16*58)
#define print_reference (WORD)((char *)symroot+SYMBOL+16*59)
#define print_vector (WORD)((char *)symroot+SYMBOL+16*60)
#define print_string (WORD)((char *)symroot+SYMBOL+16*61)
#define print_stream (WORD)((char *)symroot+SYMBOL+16*62)
#define print_code (WORD)((char *)symroot+SYMBOL+16*63)
#define gc_hook (WORD)((char *)symroot+SYMBOL+16*64)
#define system_header_path (WORD)((char *)symroot+SYMBOL+16*65)
#ifdef AP1000
#define interrupt_mask (WORD)((char *)symroot+SYMBOL+16*66)
#define last_input (WORD)((char *)symroot+SYMBOL+16*67)
#define message_handler (WORD)((char *)symroot+SYMBOL+16*68)
#endif

#ifdef AP1000
#define SYMROOTSIZE (4*69)
#else
#define SYMROOTSIZE (4*66)
#endif
extern WORD symroot[SYMROOTSIZE];
#define topprompt root[0]
#define breakprompt root[1]
#define sistream root[2]
#define sostream root[3]
#define ROOTSIZE 4
extern WORD root[ROOTSIZE];
#else /* NO SYMROOT */
#define nil root[0]
#define true root[1]
#define lambda root[2]
#define macro root[3]
#define obvector root[4]
#define uderr root[5]
#define system_lib_path root[6]
#define lower_case root[7]
#define upper_case root[8]
#define openfiles root[9]
#define standard_input root[10]
#define standard_output root[11]
#define terminal_input root[12]
#define terminal_output root[13]
#define prompt root[14]
#define digits root[15]
#define printlevel root[16]
#define printlength root[17]
#define intern_v root[18]
#define readtable root[19]
#define macrotable root[20]
#define default_readtable root[21]
#define default_macrotable root[22]
#define default_obvector root[23]
#define usebq root[24]
#define break_v root[25]
#define attention_handler root[26]
#define toplevel_v root[27]
#define utilisp_v root[28]
#define quote root[29]
#define sistream root[30]
#define sostream root[31]
#define topprompt root[32]
#define breakprompt root[33]
/* エラーを扱う関数 */
#define err_argument_type root[34]
#define err_buffer_overflow root[35]
#define err_catch root[36]
#define err_end_of_file root[37]
#define err_floating_overflow root[38]
#define err_function root[39]
#define err_go root[40]
#define err_index root[41]
#define err_io root[42]
#define err_number_of_arguments root[43]
#define err_open_close root[44]
#define err_return root[45]
#define err_unbound_variable root[46]
#define err_undefined_function root[47]
#define err_variable root[48]
#define err_zero_division root[49]
#define obvect root[50]
#define err_read root[51]

/* 補遺 */
#define unbound_variable root[52]
#define inopen root[53]
#define outopen root[54]
#define question root[55]
#define qqq root[56]
#define version root[57]
#define appendopen root[58]
#define readquote root[59]
#define utilisp_path root[60]
#define system_header_path root[61]

#ifdef AP1000
#define interrupt_mask root[62];
#define last_input root[63];
#define message_handler root[64];
#endif

#ifdef AP1000
#define ROOTSIZE 65
#else
#define ROOTSIZE 62
#endif

extern WORD root[ROOTSIZE];
#endif

extern WORD loopval,progval,progtag,catchval,catchtag;
extern int breakval;
extern WORD *heaptop,*heap,*heapbottom;
extern WORD *allheaptop,*allheapbottom;
extern WORD *lstacktop,*lstack,*lstackbottom;
extern WORD *bstacktop,*bstack,*bstackbottom;
extern WORD *cstacktop,*cstack,*cstackbottom;
extern char *codeareatop;
extern struct lispenv *estacktop,*estack,*estackbottom;
extern char *mallocptr,*malloctop;
#ifndef NOSYMROOT
extern WORD uderr;
#endif

extern int no_lispsys_l;
extern int gctype;
#ifdef MAIN
#undef extern
#endif
#ifndef OBJECTSPECIAL
extern int etext;
#endif

/* eval.c */
WORD eval(WORD, WORD*), funcall(WORD,int, WORD*), apply(WORD, WORD, WORD*);
/* err.c */
#ifdef __GNUC__
#define NORETURN __attribute__ ((noreturn))
#else
#define NORETURN 
#endif
void ubverr(WORD, WORD*) NORETURN;
WORD udferr(WORD,WORD*);
WORD udferr1(WORD,WORD*);
void fnerr(WORD, WORD*) NORETURN;

#ifndef INVALID_ACCESS_JUMP
void parerr(void) NORETURN;
#endif

void typeerr(WORD,WORD *) NORETURN;
void typeerr1(WORD) NORETURN;
void zerodiverr(WORD *) NORETURN;
void readerr(WORD, WORD*) NORETURN;
void eoferr(WORD, WORD*) NORETURN;
void ioerr(WORD, WORD*) NORETURN;
void goerr(WORD, WORD*) NORETURN;
void reterr(WORD, WORD*) NORETURN;
void ctcherr(WORD, WORD*) NORETURN;
void openerr(WORD, WORD*) NORETURN;
void inderr(WORD, WORD*) NORETURN;
void sigsegv();
void sigbus();
/* input.c */
WORD read_l(WORD *,int),internsym(WORD, WORD*);
WORD intern(char *,int,WORD *),intern_soft(char *,int,WORD *);
unsigned char GETC(WORD,WORD*);
unsigned char PEEKC(WORD,WORD*);
unsigned char GETCERR(WORD,WORD*);
unsigned char PEEKCERR(WORD,WORD*);
/* output.c */
void FLUSH(WORD);
void PUTC(int,WORD);
void PUTS(char *,WORD);
/* init.c */
void setup(void);
WORD defeof_f(int,WORD*);
/* alloc.c */
WORD alcons(WORD,WORD,WORD*),allist(int,WORD*,WORD),alsymbol(WORD, WORD*);
WORD alsym(char*,int,WORD*),alstring(char*,WORD*),alnstring(char*,int,WORD*);
WORD alstrspace(int,WORD *),alvector(int,WORD*),albignum(int,WORD*);
WORD alcode(int, WORD*),alstream(WORD, WORD*),alflonum(double,WORD*);
void setroot(WORD,char *,int,WORD *);
#ifdef OBJECTSPECIAL
WORD alspec(WORD*);
#endif

/* sysfnhash.c */
int hashstr(char *,int);
/* sysfnio.c */
WORD terpri_f(int, WORD*),inopen_f(int, WORD*),read_f(int, WORD*);
WORD string_stream_f(int, WORD*),print_object_f(int,WORD*);
WORD stream_f(int,WORD*),close_f(int, WORD*),flush_f(int, WORD*);
WORD princ_f(int,WORD*);
/* sysfnlist.c */
WORD delq_f(int,WORD*);
/* sysfnnum.c */
WORD negbig2(WORD, WORD*),copybig(WORD, WORD*),extzbig(WORD,int,int,WORD*);
WORD normbig(WORD),extbig(WORD,int,int,WORD*);
/* sysfnmis.c */
WORD utilisp_f(int,WORD*);
void ret2break(WORD *) NORETURN;
/* sysfnerr.c */
void err_all(char *,int,WORD *) NORETURN;
/* sysfnflo.c */
WORD flotobig(double,WORD*);
double bigtoflo(WORD);
/* sysfnlist.c */
WORD nconc_f(int,WORD*);
/* sysfnpred.c */
int equal(WORD,WORD);
/* sysfnspec.c */
void eval_unwind(struct lispenv *, WORD *);
/* sysfnsym.c */
WORD putprop_f(int, WORD*);
/* proglink.c */
WORD proglink(int,char **,char **);
/* gc.c */
void init_stackWM(WORD *);
WORD gc(int, WORD *);
/* muls.s */
int uadd(int *,int,int);
int usub(int *,int,int);
int madd(int *,int *,int);
int msub(int *,int *,int);
int umul(int *,int,int);
int mmul(int *,int,int *,int);
int mdiv(int *,int,int *,int);
int mconv(int *,int,int *);
/* makecode.c */
void init_codes(WORD *);

WORD backtrace_f(int,WORD *);
/* gc.c */
WORD gc_f(int,WORD *),gccount_f(int,WORD *),gctime_f(int,WORD *);
WORD heapsize_f(int,WORD *),heapfree_f(int,WORD *),symsize_f(int,WORD *);
WORD extendheap_f(int,WORD *);
WORD heapsizeK_f(int,WORD *),heapfreeK_f(int,WORD *),extendheapK_f(int,WORD *);
WORD symfree_f(int,WORD *),fixsize_f(int,WORD *),fixfree_f(int,WORD *);
WORD stacksize_f(int,WORD *),stack_used_f(int,WORD *);
WORD stack_bottom_f(int,WORD *),stack_top_f(int,WORD *),stack_base_f(int,WORD *);
WORD othersize_f(int,WORD *),otherfree_f(int,WORD *);
WORD init_stackWM_f(int,WORD *),stackWM_f(int,WORD *);
/* sysfncd.c */
WORD getd_f(int,WORD *),putd_f(int,WORD *),make_undefined_f(int,WORD *);
WORD definedp_f(int,WORD *),specialp_f(int,WORD *);
WORD funcname_f(int,WORD *),codesize_f(int,WORD *),maxarg_f(int,WORD *);
WORD minarg_f(int,WORD *),program_link_f(int,WORD *);
#ifndef NO_DYNAMIC_LOAD 
WORD system_code_load_f(int,WORD *);
#endif
/* sysfnerr.c */
WORD oldvalue_f(int,WORD *);
/* sysfneval.c */
WORD eval_f(int,WORD *),apply_f(int,WORD *),funcall_f(int,WORD *);
/* sysfnfix.c */
WORD fixzerop_f(int,WORD *),fixplusp_f(int,WORD *),fixminp_f(int,WORD *);
WORD fixeq_f(int,WORD *),fixneq_f(int,WORD *),fixgt_f(int,WORD *);
WORD fixge_f(int,WORD *),fixlt_f(int,WORD *),fixle_f(int,WORD *);
WORD fixdifference_f(int,WORD *);
WORD fixplus_f(int,WORD *),fixtimes_f(int,WORD *),fixquotient_f(int,WORD *);
WORD fixremainder_f(int,WORD *),fixadd1_f(int,WORD *);
WORD fixsub1_f(int,WORD *),logor_f(int,WORD *),logand_f(int,WORD *);
WORD logxor_f(int,WORD *),logshift_f(int,WORD *);
WORD fixexpt_f(int,WORD *);
#ifndef NO_FLONUM
/* sysfnflo.c */
WORD flozerop_f(int,WORD *),floplusp_f(int,WORD *),flominsp_f(int,WORD *);
WORD floeq_f(int,WORD *),floneq_f(int,WORD *);
WORD flodifference_f(int,WORD *),floplus_f(int,WORD *);
WORD flotimes_f(int,WORD *),floquotient_f(int,WORD *);
WORD floremainder_f(int,WORD *),floadd1_f(int,WORD *),flosub1_f(int,WORD *);
WORD fix_f(int,WORD *),float_f(int,WORD *),flogt_f(int,WORD *);
WORD floge_f(int,WORD *),flolt_f(int,WORD *),flole_f(int,WORD *);
#endif
/* sysfnhash.c */
WORD hash_f(int,WORD *);
/* sysfnio.c */
WORD read_f(int,WORD *),readquote_f(int,WORD *),prin1_f(int,WORD *);
WORD print_f(int,WORD *),princ_f(int,WORD *),terpri_f(int,WORD *);
WORD flush_f(int,WORD *),cursor_f(int,WORD *),colleft_f(int,WORD *);
WORD tab_f(int,WORD *),linelength_f(int,WORD *),intern_f(int,WORD *);
WORD intern_soft_f(int,WORD *),tyi_f(int,WORD *),tyipeek_f(int,WORD *);
WORD readch_f(int,WORD *),tyo_f(int,WORD *),stream_f(int,WORD *);
WORD inopen_f(int,WORD *),outopen_f(int,WORD *),close_f(int,WORD *);
WORD stream_mode_f(int,WORD *),appendopen_f(int,WORD *);
WORD readmacro_f(int,WORD *),readline_f(int,WORD *),current_line_f(int,WORD *);
WORD skipline_f(int,WORD *);
WORD single_character_f(int,WORD *),atomlength_f(int,WORD *),alloc_f(int,WORD *);
WORD string_stream_f(int,WORD *);
WORD string_stream_index_f(int,WORD *),string_stream_limit_f(int,WORD *);
WORD charleft_f(int,WORD *),print_object_f(int,WORD *);
/* sysfnlist.c */
WORD cr_f(int,WORD *),fifth_f(int,WORD *),sixth_f(int,WORD *),seventh_f(int,WORD *);
WORD caar_f(int,WORD *),cadr_f(int,WORD *),cdar_f(int,WORD *);
WORD cddr_f(int,WORD *);
WORD caaar_f(int,WORD *),caadr_f(int,WORD *),cadar_f(int,WORD *),caddr_f(int,WORD *);
WORD cdaar_f(int,WORD *),cdadr_f(int,WORD *),cddar_f(int,WORD *);
WORD cdddr_f(int,WORD *);
WORD caaaar_f(int,WORD *),caaadr_f(int,WORD *),caadar_f(int,WORD *);
WORD caaddr_f(int,WORD *);
WORD cadaar_f(int,WORD *),cadadr_f(int,WORD *),caddar_f(int,WORD *);
WORD cadddr_f(int,WORD *);
WORD cdaaar_f(int,WORD *),cdaadr_f(int,WORD *),cdadar_f(int,WORD *);
WORD cdaddr_f(int,WORD *);
WORD cddaar_f(int,WORD *),cddadr_f(int,WORD *),cdddar_f(int,WORD *);
WORD cddddr_f(int,WORD *);
WORD car_f(int,WORD *),cdr_f(int,WORD *),cons_f(int,WORD *);
WORD ncons_f(int,WORD *),xcons_f(int,WORD *),copy_f(int,WORD *),last_f(int,WORD *);
WORD length_f(int,WORD *),nth_f(int,WORD *),nthcdr_f(int,WORD *);
WORD list_f(int,WORD *),list__f(int,WORD *);
WORD append_f(int,WORD *),reverse_f(int,WORD *),nconc_f(int,WORD *);
WORD nreverse_f(int,WORD *);
WORD rplaca_f(int,WORD *),rplacd_f(int,WORD *),subst_f(int,WORD *),memq_f(int,WORD *),member_f(int,WORD *);
WORD mem_f(int,WORD *),delq_f(int,WORD *),remq_f(int,WORD *);
WORD every_f(int,WORD *),some_f(int,WORD *),assq_f(int,WORD *),assoc_f(int,WORD *),ass_f(int,WORD *);
/* sysfnmap.c */
WORD map_f(int,WORD *),mapc_f(int,WORD *),maplist_f(int,WORD *);
WORD mapcar_f(int,WORD *),mapv_f(int,WORD *),mapvector_f(int,WORD *);
WORD mapcan_f(int,WORD *),mapcon_f(int,WORD *);
/* sysfnmis.c */
WORD quit_f(int,WORD *),abend_f(int,WORD *);
WORD toplevel_f(int,WORD *),break_f(int,WORD *),time_f(int,WORD *),address_f(int,WORD *),peek_f(int,WORD *);
WORD utilisp_f(int,WORD *),unbreak_f(int,WORD *);
WORD rand_f(int,WORD *),srand_f(int,WORD *);
/* sysfnnum.c */
WORD zerop_f(int,WORD *);
WORD plusp_f(int,WORD *),minusp_f(int,WORD *),oddp_f(int,WORD *),plus_f(int,WORD *),difference_f(int,WORD *);
WORD times_f(int,WORD *),quotient_f(int,WORD *),remainder_f(int,WORD *);
WORD add1_f(int,WORD *),sub1_f(int,WORD *),abs_f(int,WORD *);
WORD minus_f(int,WORD *),greaterp_f(int,WORD *);
WORD lessp_f(int,WORD *),max_f(int,WORD *);
WORD min_f(int,WORD *);
#ifndef NO_FLONUM
WORD sin_f(int,WORD *),cos_f(int,WORD *),tan_f(int,WORD *);
WORD arcsin_f(int,WORD *),arccos_f(int,WORD *),arctan_f(int,WORD *);
WORD sqrt_f(int,WORD *),log_f(int,WORD *),log10_f(int,WORD *);
WORD exp_f(int,WORD *),expt_f(int,WORD *),exptflo_f(int,WORD *);
#endif
/* sysfnpred.c */
WORD atom_f(int,WORD *),numberp_f(int,WORD *),stringp_f(int,WORD *);
WORD vectorp_f(int,WORD *),referencep_f(int,WORD *);
WORD symbolp_f(int,WORD *),consp_f(int,WORD *),codep_f(int,WORD *);
WORD predef_f(int,WORD *),eq_f(int,WORD *),neq_f(int,WORD *),streamp_f(int,WORD *);
WORD equal_f(int,WORD *),null_f(int,WORD *),fixp_f(int,WORD *);
WORD floatp_f(int,WORD *),integerp_f(int,WORD *),bigp_f(int,WORD *);
WORD string_streamp_f(int,WORD *),predefinedp_f(int,WORD *);
/* sysfnspec.c */
WORD and_e(WORD,WORD *),or_e(WORD,WORD *);
WORD cond_e(WORD,WORD *),selectq_e(WORD,WORD *),prog_e(WORD,WORD *),catch_e(WORD,WORD *),go_e(WORD,WORD *),quote_e(WORD,WORD *);
WORD comment_e(WORD,WORD *),progn_e(WORD,WORD *),prog1_e(WORD,WORD *);
WORD prog2_e(WORD,WORD *),push_e(WORD,WORD *),pop_e(WORD,WORD *),setq_e(WORD,WORD *);
WORD loop_e(WORD,WORD *),match_e(WORD,WORD *),let_e(WORD,WORD *);
WORD lets_e(WORD,WORD *),exit_f(int,WORD *),return_f(int,WORD *),throw_f(int,WORD *);
WORD unwind_protect_e(WORD,WORD *);
/* sysfnstg.c */
WORD character_f(int,WORD *),string_f(int,WORD *),string_length_f(int,WORD *);
WORD string_lessp_f(int,WORD *),substring_f(int,WORD *);
WORD string_append_f(int,WORD *),string_reverse_f(int,WORD *);
WORD string_nreverse_f(int,WORD *),string_search_char_f(int,WORD *);
WORD string_search_not_char_f(int,WORD *);
WORD string_search_f(int,WORD *),getchar_f(int,WORD *),sref_f(int,WORD *);
WORD sset_f(int,WORD *),string_equal_f(int,WORD *);
WORD cutout_f(int,WORD *),spread_f(int,WORD *),translate_f(int,WORD *);
WORD string_amend_f(int,WORD *),string_amend_or_f(int,WORD *);
WORD string_amend_xor_f(int,WORD *),string_amend_and_f(int,WORD *),make_string_f(int,WORD *);
WORD bset_f(int,WORD *);
WORD bref_f(int,WORD *),string_match_f(int,WORD *);
/* sysfnsym.c */
WORD set_f(int,WORD *),make_unbound_f(int,WORD *);
WORD boundp_f(int,WORD *),get_f(int,WORD *),putprop_f(int,WORD *),remprop_f(int,WORD *);
WORD plist_f(int,WORD *),setplist_f(int,WORD *),pname_f(int,WORD *);
WORD setpname_f(int,WORD *),symbol_f(int,WORD *),symbol_copy_f(int,WORD *),gensym_f(int,WORD *);
/* sysfnsys.c */
WORD call_f(int,WORD *),cd_f(int,WORD *),getenv_f(int,WORD *);
WORD localtime_f(int,WORD *),getpid_f(int,WORD *);
WORD putenv_f(int,WORD *);
WORD syscall_f(int,WORD *),errno_f(int,WORD *);
WORD argv_f(int,WORD *);
/* sysfnvec.c */
WORD vref_f(int,WORD *),vset_f(int,WORD *);
WORD vector_f(int,WORD *),vector_length_f(int,WORD *),reference_f(int,WORD *),deref_f(int,WORD *);
WORD setref_f(int,WORD *),referred_vector_f(int,WORD *),referred_index_f(int,WORD *);
WORD fill_vector_f(int,WORD *);
WORD dumpfile_f(int,WORD *);
#ifdef AP1000
/* sysfnap.c */
WORD getcid_f(int,WORD *),getncel_f(int,WORD *),getncelx_f(int,WORD *);
WORD getncely_f(int,WORD *);
WORD sync_f(int,WORD *),cstat_f(int,WORD *),gstat_f(int,WORD *);
WORD pstat_f(int,WORD *);
#endif
