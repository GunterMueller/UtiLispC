(setq printlevel 0 printlength 0)
(defun getpid () 0)
(macro program-load (l)
   (do ((l l (cdr l)) (e) (flist) (fns))
       ((null l)
        `(program-load*
            ,(/-gen-tmpfile "/tmp/uti-")
            ',(nreverse flist)
            ',(nreverse fns)))
       (setq e (eval (car l)))
       (cond ((stringp e) (push e flist))
             ((consp e) (push e fns))
             ((symbolp e) (push (pname e) flist)))))
(defun program-load* (file flist fns)
   (lets ((tmps (string-append file ".c")) (tmpo (string-append file ".o"))
          (standard-output (outopen (stream tmps))) (pairs))
    (prog1 (catch 'progload
              (/-gen-header)
              (setq pairs (mapcar fns (function /-make-s)))
	      (format "FP cfndata[]={/n")
	      (do ((tmppair pairs (cdr tmppair))
		   (n (length pairs) (1- n)))
		  ((atom tmppair))
		  (format "/c_f,/n" (caar tmppair)))
	      (format "(FP)0};/n")
              (close standard-output)
              (call (string-append "cc -c -o " tmpo " " tmps))
              (do ((codes (nreverse(apply (function program-link) `(,tmpo . ,flist)))
                          (cdr codes))
                   (fns fns (cdr fns))
                   (s))
                  ((or (null fns) (null codes)) (nreverse s))
                  (funcname (car codes) (caar fns))
                  (push (putd (caar fns) (car codes)) s)))
      )))
;           (call (string-append "rm " tmps " " tmpo)))))
(defun /-make-s (x)
  (let ((args nil)(ret nil))
    (match x
	   ((lispfn cfn argtype checklist restype)
	    (do ((al argtype (cdr al))
		 (n (/1- (length argtype)) (/1- n)))
		((atom al))
		(selectq (car al)
		      ('string (push `(stringcodes(checkstr a (ag ,n))) args))
		      ('fix (push `(fixtoi(checkfix a (ag ,n)))args))
		      ('float (push `(floval (checkflo a (ag ,n)))args))
		      ('any (push (ag ,n)args))
		      (t (/-gen-error "argument-type" (car al)))))
	    (setq args(nreverse args))
	    (setq ret
		  (selectq restype
			   ('string `(return(alstring (,cfn . ,args) fp)))
			   ('fix `(return(itofix (,cfn . ,args))))
			   ('float `(return(alflonum (,cfn . ,args) fp)))
			   ('nil `(return (,cfn . ,args) nil))))
	    (cond ((eq restype 'float)
		   (c-dump-c cfn 
			     `(block 
			       ((WORD a) (WORD (alflonum))(double (,cfn)))
			       ,ret)))
		  (t
		   (c-dump-c cfn 
			     `(block 
			       ((WORD a) (WORD (alflonum))(WORD (alstring)))
			       ,ret))))
	    `(,cfn . 0))
	   (t (/-gen-error "illegal form" x)))))
(defun /-gen-header nil
  (let ((tmppath0 (string-append system_lib_path "constdef.h"))
	(tmppath1 (string-append system_lib_path "defvar.h")))
    (format "#include /s/n" tmppath0)
    (format "#include /s/n" tmppath1)
    (c-dump-c 'initload '(block ((extern FP cfndata/[/])) (return ((WORD)cfndata))))))
(defun /-gen-data (lispfn n)
  (format "/c_f," lispfn))
(defun /-gen-tmpfile ((prefix "./uti-"))
   (string-append prefix (getenv "USER") (number-image (getpid))))
(defun /-gen-error (msg x)
   (lets ((standard-output terminal-output))
    (format "/c --- /s/n" msg x)
    (throw 'progload)))
(putd 'sun3-p `(lambda (x) ,(/0= (call "sun3"))))
(defun c-dump-c (fn codes)
;   (format "WORD /c_f(na, fp)/n" (pname fn))
   (format "WORD /c_f(na, fp)/n" fn)
   (format "WORD *fp;/n")
   (cond ((null codes) (format "{}")) (t (c-dump-1 codes))))
(defun c-dump (x)
   (cond ((atom x) (princ x))
	 ((or (stringp (car x))(consp (car x)))
	  (princ (car x))(c-dump-args (cdr x)))
	 ((get (car x) 'dump) (funcall (get (car x) 'dump) (cdr x)))
	 (t (c-dump (car x)) (c-dump-args (cdr x)))))
(defun c-dump-0 (x) (lets ((out-brace)) (c-dump x)))
(defun c-dump-1 (x) (lets ((out-brace t)) (c-dump x)))
(defun c-dump-0-s (x) (c-dump-0 x) (dump-semicolon t))
(defun c-dump-1-s (x) (c-dump-1 x) (dump-semicolon t))
(defun c-dump-args (argl) (princ "(") (c-dump-comma argl) (princ ")"))
(defun c-dump-comma (argl)
   (do ((l argl (cdr l)))
       ((or (null l) (null (cdr l))) (and l (c-dump-0 (car l))))
       (c-dump-0 (car l))
       (dump-comma)))
(defun c-dump-local (sym)
  (cond ((and (consp sym)(eq (car sym)'extern))
	 (format "extern /c /c;/n" (cadr sym)(caddr sym)))
	(t
	 (format "/c "(car sym))
	 (c-dump (cadr sym))
	 (format ";/n"))))
;   (format "/c /s;/n" (cond ((c-intvar sym) "int") (t "WORD")) sym))
;(defun c-intvar (sym) (eq (sref sym 0) (character "i")))
(defun c-binary-op (op)
   (dump-left-paren)
   (c-dump-0 (first x))
   (princ op)
   (c-dump-0 (second x))
   (dump-right-paren))
(defprop = (lambda (x) (c-binary-op " = ")) dump)
(defprop > (lambda (x) (c-binary-op " > ")) dump)
(defprop < (lambda (x) (c-binary-op " < ")) dump)
(defprop == (lambda (x) (c-binary-op " == ")) dump)
(defprop <= (lambda (x) (c-binary-op " <= ")) dump)
(defprop >= (lambda (x) (c-binary-op " >= ")) dump)
(defprop + (lambda (x) (c-binary-op " + ")) dump)
(defprop - (lambda (x) (c-binary-op " - ")) dump)
(defprop -= (lambda (x) (c-binary-op " -= ")) dump)
(defprop += (lambda (x) (c-binary-op " += ")) dump)
(defprop && (lambda (x) (c-binary-op " && ")) dump)
(defprop || (lambda (x) (c-binary-op " || ")) dump)
(defprop for
   (lambda (x)
      (princ "for ")
      (dump-left-paren)
      (c-dump-comma (first (car x)))
      (dump-semicolon)
      (c-dump-0 (second (car x)))
      (dump-semicolon)
      (c-dump-comma (third (car x)))
      (dump-right-paren t)
      (c-dump-1 (second x)))
   dump)
(defprop if
   (lambda (x (pred (first x)) (then (second x)) (rest (cddr x)))
    (princ "if ")
    (dump-left-paren)
    (c-dump-0 pred)
    (dump-right-paren)
    (c-dump-1 then)
    (cond ((consp rest) (princ "else ") (c-dump-1 (car rest)))))
   dump)
(defprop switch
   (lambda (x (a (car x)) (rest (cdr x)))
    (princ "switch ")
    (dump-left-paren)
    (c-dump-0 a)
    (dump-right-paren)
    (dump-left-brase t)
    (do ((l rest (cdr l)) (p))
	((null l))
	(setq p (car l))
	(cond ((eq (car p) 'default) (princ "default"))
	      (t (princ "case ") (c-dump-0 (car p))))
	(dump-colon t)
	(mapc (cdr p) (function c-dump-0-s)))
    (dump-right-brase t))
    dump)
(defprop block
   (lambda (x)
    (cond ((or (first x) out-brace)
	   (dump-left-brase t)
	   (mapc (first x) (function c-dump-local))))
    (mapc (cdr x) (function (lambda (a) (or (atom a) (c-dump-0-s a)))))
    (and (or (first x) out-brace) (dump-right-brase t)))
   dump)
(defprop predef-call
   (lambda (x) (format "/s_f" (first x)) (c-dump-args (cdr x)))
   dump)
(defun dump-left-paren ((nl)) (princ "(") (and nl (terpri)))
(defun dump-right-paren ((nl)) (princ ")") (and nl (terpri)))
(defun dump-left-brase ((nl)) (princ "{") (and nl (terpri)))
(defun dump-right-brase ((nl)) (princ "}") (and nl (terpri)))
(defun dump-semicolon ((nl)) (princ ";") (and nl (terpri)))
(defun dump-colon ((nl)) (princ ":") (and nl (terpri)))
(defun dump-comma ((nl)) (princ ",") (and nl (terpri)))
