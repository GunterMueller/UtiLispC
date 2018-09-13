# UtiLispC

UtiLisp (University of Tokyo Interactive LISt Processor). Source forked from https://dell.tanaka.ecc.u-tokyo.ac.jp/~ktanaka/UtiLisp/.

"UtiLisp, which is not well known outside Japan, is a Lisp dialect of classical MacLisp: eval top loop, shallow binding, dynamic scope rule interpreter, with macro and readmacro facilities.

There are four versions of UtiLisp:

UtiLisp360 on main frame machines like Hitac M series, Facom M/S series, IBM 3000 series
UtiLisp68 on MC68000 (24 bit address) for Sun 1, Macintosh etc.
UtiLisp32 on MC68010, 68020 (32 bit address) for Sun 2, 3 and on Vax etc.
UtiLisp/C on SPARC for Sun 4 and Sparc Station." [Wada 1990]

This repository deals with UtiLisp/C.

See http://www.softwarepreservation.org/projects/LISP/utilisp/ for more details.


Note: The NetBSD version of utilispc 1.13 builds fine after adding "-m32" to the src/Makefile.
