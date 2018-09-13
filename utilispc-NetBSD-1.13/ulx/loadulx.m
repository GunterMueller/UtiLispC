divert(-1)dnl
changecom(;)
changequote(`{',`}')

define({LISPLIB}, {lisplib})
;define({LIB}, {/usr/local/lib/utilisp/ulx})
;define({PREF}, {ulx})

define({loadlisplib},{ifelse($#,0,,$#,1,{_loadlisplib({$1})}
,{_loadlisplib({$1})
loadlisplib(shift($@))})})
define({_loadlisplib},{(load "LISPLIB/{$1}")})

define({codeload},{ifelse($#,0,,$#,1,{_codeload({$1.o})}
,{_codeload({$1.o})
codeload(shift($@))})})
define({_codeload},{	"LIB/$1"})

define({loadulx},{ifelse($#,0,,$#,1,{_loadulx($1)}
,{_loadulx($1)
loadulx(shift($@))})})

define({_loadulx},{ifelse(eval(index($1,{.o})>=0),1,
{divert(2)define({_codes},incr(_codes))_codeload(basename({$1}).o)
divert(-1)})
ifelse(eval(regexp($1,{\.o?l$})>=0),1,
{divert(1)(load "PREF/basename({$1}).l")
divert(-1)})})

ifdef({ALLSRC},
{define({_loadulx},{divert(1)(load "PREF/basename({$1}).l")
divert(-1)})})
    
define({basename},{patsubst($1,{\..*$})})

define({_codes},{0})
divert{}dnl
;;; -*- Mode: Lisp -*-
;;; script for loading ULX (UtiLisp X interface library)
;;;
{(defun load (filename (show))
  (cond ((or (eq (sref filename 0) (character "/"))
	     (eq (sref filename 0) (character "."))))
	(t (setq filename (string-append system_lib_path filename))))
  (princ filename)(terpri)
  (exfile filename show))}

(code-load "LIB/socket.o")

loadlisplib(readmacro.l, common.l, struct.l, defun.l, deftype.l)
(load "PREF/defsetf.l")
(load "PREF/commonlib.l")

loadulx(longarith.o,depdefs.l,dependent.o,display.o,buffer.ol,bufmac.l)
loadulx(ulx.o,macros.l,input.ol,requests.ol,graphics.o)
loadulx(gcontext.ol,attributes.o,fonts.o,manager.l)
loadulx(text.ol,translate.l,ulxlib.l)
divert
ifelse(_codes,0,,{{(code-load '(}
undivert(2){	))}})

undivert(1)
