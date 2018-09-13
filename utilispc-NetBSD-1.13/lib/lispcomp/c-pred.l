(mapc '(symbolp consp listp atom fixp bigp intergerp floatp numberp stringp
	vectorp referencep streamp codep eq neq equal not null boundp
	make-unbound putd definedp specialp make-undefined symbol gensym
	zerop /0= /0=$ plusp /0< /0<$ minusp /0> /0>$ oddp = =$ # <> 
        #$ <>$ lessp < <$ greaterp > >$ <= <=$ >= >=$ string-equal 
        string-lessp string-match getchar stream-mode comment)
   (function (lambda (x) (putprop x 'SYMBOL 'c:subr-result))))

(mapc '(/+ /- /1+ /1- * // fix length string-length character cutout
        vector-length reffered-index linelength cursor colleft
	atomlength tyi tyipeek tyo minarg maxarg)
   (function (lambda (x) (putprop x 'FIXNUM 'c:subr-result))))

(mapc '(/+$ /-$ /1+$ /1-$ *$ //$ float)
   (function (lambda (x) (putprop x 'FLONUM 'c:subr-result))))

(mapc '(cons ncons nconc append list rplaca rplacd)
   (function (lambda (x) (putprop x 'CONS 'c:subr-result))))

(mapc '(string substring string-append string-reverse string-nreverse)
   (function (lambda (x) (putprop x 'STRING 'c:subr-result))))

(mapc '(vector fillvctor refferd-vector mapv mapvector)
   (function (lambda (x) (putprop x 'VECTOR 'c:subr-result))))

(mapc '(reference)
   (function (lambda (x) (putprop x 'REFERENCE 'c:subr-result))))

(defun c:subr-result-type (x)
   (and (consp x)
	(symbolp (car x))
	(not (get (car x) 'redefine))
	(get (car x) 'c:subr-result)))

(defun c:subr-result (fn type) (c:type-match (get fn 'c:subr-result) type))

(defun c:typep (x type) (c:type-match (c:type x) type))

(defun c:type-match (t1 t2)
    (cond ((null t1) nil)
	  ((eq t1 t2))
	  ((eq t2 'INTEGER) (memq t1 '(FIXNUM BIGNUM)))
	  ((eq t2 'NUMBER) (memq t1 '(FIXNUM FLONUM BIGNUM)))
	  ((eq t2 'ATOM) (neq t1 'CONS))
	  (t nil))))

(defun c:type (expr (again))
   (cond ((fixp expr) 'FIXNUM)
         ((floatp expr) 'FLONUM)
         ((bigp expr) 'BIGNUM)
         ((stringp expr) 'STRING)
	 ((symbolp expr) 'SYMBOL)
	 ((vectorp expr) 'VECTOR)
	 ((streamp expr) 'STREAM)
	 ((referencep expr) 'REFERENCE)
	 ((codep expr) 'CODE)
	 (again 'CONS)
         ((and (consp expr) (eq (car expr) 'quote)) (c:type (second expr) t))
         ((consp expr) (get (car expr) 'c:subr-result))))

(defun c:type-pred (args loc jmp type (otype))
   (match args
    ((x) 
     (cond ((c:constantp x) (c:symbol (c:typep x type) loc jmp))
	   ((c:subr-result-type x)
	      `(begin
	         ,(c:form x nil nil)
	         ,(c:symbol (c:subr-result (car x) type) loc jmp)))
	   (t (lets ((a (c:get-cvar)))
	       `(begin ,(c:form x a nil)
		       (if ,(c:make-type-cond a type otype)
			   ,(c:symbol t loc jmp)
			   ,(c:symbol nil loc jmp)))))))
    (t (c:argerr args))))

(defun c:make-type-cond (a type otype)
   (selectq type
    (ATOM `(atom ,a))
    (INTEGER `(|| (== (tag ,a) FIXNUM)
		  (&& (== (tag ,a) OTHERS) (== (otag ,a) BIGNUM))))
    (NUMBER `(|| (== (tag ,a) FIXNUM)
                 (&& (== (tag ,a) OTHERS)
		     (|| (== (otag ,a) FLONUM) (== (otag ,a) BIGNUM)))))
    (REFERENCE `(&& (== (tag ,a) OTHERS) (== (tag3 (otag ,a)) OTAGS)))
    (OTHERS `(&& (== (tag ,a) OTHERS) (== (otag ,a) ,otype)))
    (t `(== (tag ,a) ,type))))

(defnprop symbolp
   (lambda (args loc jmp) (c:type-pred args loc jmp 'SYMBOL))
   inline)

(defnprop consp
   (lambda (args loc jmp) (c:type-pred args loc jmp 'CONS))
   inline)

(alias listp consp)

(defnprop fixp
   (lambda (args loc jmp) (c:type-pred args loc jmp 'FIXNUM))
   inline)

(defnprop floatp
   (lambda (args loc jmp) (c:type-pred args loc jmp 'OTHERS 'FLONUM))
   inline)

(defnprop bigp
   (lambda (args loc jmp) (c:type-pred args loc jmp 'OTHERS 'BIGNUM))
   inline)

(defnprop integerp
   (lambda (args loc jmp) (c:type-pred args loc jmp 'INTEGER))
   inline)

(defnprop numberp
   (lambda (args loc jmp) (c:type-pred args loc jmp 'NUMBER))
   inline)

(defnprop stringp
   (lambda (args loc jmp) (c:type-pred args loc jmp 'OTHERS 'STRING))
   inline)

(defnprop vectorp
   (lambda (args loc jmp) (c:type-pred args loc jmp 'OTHERS 'VECTOR))
   inline)

(defnprop referencep
   (lambda (args loc jmp) (c:type-pred args loc jmp 'REFERENCE))
   inline)

(defnprop streamp
   (lambda (args loc jmp) (c:type-pred args loc jmp 'OTHERS 'STREAM))
   inline)

(defnprop codep
   (lambda (args loc jmp) (c:type-pred args loc jmp 'OTHERS 'CODE))
   inline)

(defnprop atom
   (lambda (args loc jmp) (c:type-pred args loc jmp 'ATOM))
   inline)

(defnprop null
   (lambda (args loc jmp)
    (match args
     ((x)
      (cond ((c:constantp x)
	     (c:symbol (null (c:constant-value x)) loc jmp))
	    ((and (c:subr-result-type x) (neq (c:subr-result-type x) 'SYMBOL))
	     `(begin ,(c:form x nil nil) ,(c:symbol nil loc jmp)))
	    (t (lets ((a (c:get-cvar)) (codes))
		(c:gen (c:form x a nil)
		       `(if (== ,a nil)
			    ,(c:symbol t loc jmp) 
			    ,(c:symbol nil loc jmp)))
		(c:make-begin codes)))))
      (t (c:argerr args))))
   inline)

(alias not null)

(defun c:compare (args loc jmp comp)
   (match args
    ((x y)
     (lets ((sp sp) (r1) (r2) (nsp))
      `(begin ,(c:eval-twoargs x y)
	      (if (,comp ,(c:value r1) ,(c:value r2))
                  ,(c:symbol t loc jmp)
		  ,(c:symbol nil loc jmp)))))
    (t (c:argerr args))))

(defnprop eq (lambda (args loc jmp) (c:compare args loc jmp '==)) inline)

(defnprop neq (lambda (args loc jmp) (c:compare args loc jmp '!=)) inline)

(defnprop equal (lambda (args loc jmp) (c:compare args loc jmp 'equal)) inline)
