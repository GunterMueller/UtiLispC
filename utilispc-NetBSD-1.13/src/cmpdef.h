/*  %W% %G% */
/*
 * include file for compiler 
 */

struct code_inf {
	int 	c_info;
	int		c_nliteral;
	int		c_minarg;
	int		c_maxarg;
	WORD  (*c_codetop)();
	char	*c_literal;
};
#define ismacro(inf) (((inf)->c_info) & 1)
#define isprop(inf) (((inf)->c_info) & 2)
#define lit(n) literal(*cstack,n)
#define dis(n) (fp-(n))
WORD allist(int, WORD *, WORD),alcons(WORD, WORD, WORD *),alstring(char *, WORD *),alsymbol(WORD, WORD *),alflonum(double, WORD *),eval(WORD, WORD *),funcall(WORD, int, WORD *);

/*
 * 91/05/17 Iwasaki
 */
#define	INFO_NONE	0
#define	INFO_FUNC	1
#define	INFO_MACRO	2
#define	INFO_PROP	3
