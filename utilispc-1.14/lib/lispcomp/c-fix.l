(defun c:fix-subr (args lfn cfn const loc jmp)
   (do ((l (c:fix-subr-prep args lfn const) (cdr l)) (firstarg t nil)
	(easy (memq cfn '(tadd tsub))) (tmp (c:get-cvar)) (a (c:get-cvar))
	(i (c:get-ivar)) (x) (codes))
       ((atom l)
	(c:gen-final
	   (cond ((null codes) `(itofix ,const)) (easy a) (t `(itofix ,i)))
	   loc
	   jmp)
	(c:make-begin codes))
       (setq x (car l))
       (cond (firstarg
	      (cond ((fixp x)
		     (c:gen (cond (easy `(= ,a (itofix ,x))) (t `(= ,i ,x)))))
		    (t (c:gen (c:form x a nil)
			      (c:check-fix x a a)
			      (and (not easy) `(= ,i (fixtoi ,a)))))))
	     ((fixp x)
	      (c:gen (cond (easy `(= ,a (,cfn ,a (itofix ,x))))
			   (t `(= ,i (,cfn ,i ,x))))))
	     (t (c:gen (c:form x tmp nil)
		       (c:check-fix x tmp tmp)
		       (cond (easy `(= ,a (,cfn ,a ,tmp)))
			     (t `(= ,i (,cfn ,i (fixtoi ,tmp))))))))))

(defun c:fix-subr-prep (args lfn const)
   (do ((l args (cdr l)) (firstarg t nil) (c const) (firstfix) (res)
	(afn (cond ((eq lfn '//) '*) ((eq lfn '-) '+) (t lfn))) (x))
       ((atom l)
	(cond ((= c const) (nreverse res))
	      (firstfix `(,(funcall lfn firstfix c) . ,(cdr (nreverse res))))
	      (t (push c res) (nreverse res))))
       (setq x (car l))
       (cond ((c:constantp x)
	      (setq x (c:constant-value x))
	      (cond ((not (fixp x)) (c:typerr (car l)))
		    ((and firstarg (memq lfn '(- //)))
		     (setq firstfix x)
		     (push x res))
		    (firstarg (setq c x))
		    (t (setq c (funcall afn c x)))))
	     (t (push x res)))))

(defnprop /+
   (lambda (args loc jmp) (c:fix-subr args (function /+) 'tadd 0 loc jmp))
   inline)

(defnprop /-
   (lambda (args loc jmp)
    (cond ((consp (cdr args))
	   (c:fix-subr args (function /-) 'tsub 0 loc jmp))
	  (t (c:fix-subr `(0 . ,args) (function /-) 'tsub 0 loc jmp))))
   inline)

(defnprop /*
   (lambda (args loc jmp) (c:fix-subr args (function /*) '* 1 loc jmp))
   inline)

(defnprop //
   (lambda (args loc jmp) (c:fix-subr args (function //) '// 1 loc jmp))
   inline)

(defnprop /1+
   (lambda (args loc jmp)
    (match args
     ((x) (funcall (get '/+ 'inline) `(,x 1) loc jmp)) (t (c:argerr args))))
   inline)

(defnprop /1-
   (lambda (args loc jmp)
    (match args
     ((x) (funcall (get '/- 'inline) `(,x 1) loc jmp)) (t (c:argerr args))))
   inline)

(defun c:fix-rel (args loc jmp relop)
   (do ((l args (cdr l)) (sp sp) (ag) (a (c:get-cvar)) (onaflag) (x) (codes))
       ((atom l)
	(c:gen (cond ((<= (length args) 1) (c:symbol 't loc jmp))
	             (t `(if ,(c:make-rel-cond ag (cdr ag) relop)
			     ,(c:symbol t loc jmp)
			     ,(c:symbol nil loc jmp)))))
	(c:make-begin codes))
       (setq x (car l))
       (cond ((fixp x) (push `(itofix ,x) ag))
	     ((c:constantp x)
	      (setq x (c:constant-value x))
	      (cond ((fixp x) (push `(itofix ,x) ag))
		    (t (c:typerr (car l)))))
	     ((and (symbolp x) (not (c:specialp x)) (c:local-sym x))
	      (push (c:var (cdr (c:local-sym x))) ag))
	     (t (cond (onaflag (c:gen (c:assign (incr sp 1) a))
			       (rplaca onaflag (c:value sp))))
	        (c:gen (c:form x a nil) (c:check-fix x a a))
		(setq onaflag (push a ag))))))

(defun c:make-rel-cond (l1 l2 relop)
   (do ((l1 l1 (cdr l1)) (l2 l2 (cdr l2)) (res))
       ((atom l2) (cond ((cdr res) `(&& . ,res)) (t (car res))))
       (push `(,relop (cast (int) (tsub ,(car l2) ,(car l1))) 0) res)))

(defun c:fix-rel2 (args loc jmp relop)
   (cond ((= (length args) 2) (c:fix-rel args loc jmp relop))
	 (t (c:argerr args))))

(defun c:fix-zerorel (args loc jmp relop)
   (cond ((= (length args) 1) (c:fix-rel `(0 ,(car args)) loc jmp relop))
	 (t (c:argerr args))))

(defnprop >= (lambda (args loc jmp) (c:fix-rel args loc jmp '>=)) inline)

(defnprop > (lambda (args loc jmp) (c:fix-rel args loc jmp '>)) inline)

(defnprop < (lambda (args loc jmp) (c:fix-rel args loc jmp '<)) inline)

(defnprop <= (lambda (args loc jmp) (c:fix-rel args loc jmp '<=)) inline)

(defnprop = (lambda (args loc jmp) (c:fix-rel2 args loc jmp '==)) inline)

(defnprop # (lambda (args loc jmp) (c:fix-rel2 args loc jmp '!=)) inline)

(alias <> #)

(defnprop /0= (lambda (args loc jmp) (c:fix-zerorel args loc jmp '==)) inline) 

(defnprop /0> (lambda (args loc jmp) (c:fix-zerorel args loc jmp '>)) inline) 

(defnprop /0< (lambda (args loc jmp) (c:fix-zerorel args loc jmp '<)) inline) 
