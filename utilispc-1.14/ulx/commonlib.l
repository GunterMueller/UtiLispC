;;
;; experimental common lisp fake functions/macros.
;;
;; these definitions are to be included in common.l
;;

(defun ceiling (val (divisor 1))
  (// (+ val (1- divisor)) divisor))

(defmacro case (keyform . clauses)
  `(selectq ,keyform
     . ,(mapcar clauses
		(function
		 (lambda (x)
		   (match x
		     (('otherwise . form) `(t . ,form))
		     ((key . form) x)
		     (sym (err:argument-type sym 'case))))))))

;;; keys ha error wo dasu tokini tukaou to omotta ga mendou nanode yameta.
(defmacro ecase (keyform . clauses)
  (let ((keys nil))
    (mapcar clauses
	    (function
	     (lambda (x)
	       (cond ((atom x) (err:argument-type x 'ecase))
		     ((or (eq (car x) 'otherwise)
			  (eq (car x) t))
		      (format "Error: No /s allowed for ecase/n" (car x))
		      (break))
		     ((atom (car x))
		      (setq keys (append keys (ncons (car x)))))
		     (t (setq keys (append keys x)))))))
    `(case ,keyform
	   . ,(append clauses
		      `((t (err:argument-type ,keyform 'ecase)))))))

(macro unwind-protect (form) `(prog1 . ,form))

;;; 2^28 = 268435456
(defun integer-length (x)
  (do ((count 0 (1+ count))
       (v (or (and (0> x) (- x)) x) (logshift v -1)))
      ((0= v) count)))

(defmacro list* (arg . rest)
  (lets ((l (reverse rest))
	 (t (car l)))
    `(append (list ,arg . ,(nreverse (cdr l))) ,t)))

(defun elt (o i)
  (cond ((consp o) (nth i o))
	((vectorp o) (vref o i))
	((stringp o) (sref o i))
	(t (funcall 'err:argument-type o 'elt))))
(defun set-elt (o i v)
  (cond ((consp o) (rplaca (nthcdr i o) v))
	((vectorp o) (vset o i v))
	((stringp o) (sset o i v))
	(t (funcall 'err:argument-type o 'set-elt))))
(and (definedp 'defsetf) (defsetf elt set-elt))

(defun sequence-length (seq)
  (cond ((null seq) 0)
	((consp seq) (length seq))
	((vectorp seq) (vector-length seq))
	((stringp seq) (string-length seq))
	(t (err:argument-type seq 'sequence-length))))
;; onajimono
(defun cl:length (seq)
  (cond ((null seq) 0)
	((consp seq) (length seq))
	((vectorp seq) (vector-length seq))
	((stringp seq) (string-length seq))
	(t (err:argument-type seq 'cl:length))))
(defmacro cl:assoc (val alist . keys)
  (lets ((test (memq ':test keys)))
    (cond (test `(ass ,(second test) ,val ,alist))
	  (t `(assq ,val ,alist)))))
(defmacro defconstant (var val . doc)
  `(progn
     ;;(defconst ,var ,val)
     (setq ,var ,val)
     (declare (,var) special)))
(defmacro defparameter (var val . doc)
  `(progn
     (setq ,var ,val)
     (declare (,var) special)))

(defmacro defvar (var val . doc)
  `(progn
     (setq ,var ,val)
     (declare (,var) special)))

;(defmacro dolist ((var l . res) . body)
;  (let ((x (gensym)))
;    `(do* ((,x ,l (cdr ,x))
;	   (,var (and (consp ,x) (car ,x)) (and (consp ,x) (car ,x))))
;	  ((atom ,x) . ,res)
;	  . ,body)))
;
;(defmacro dolist ((var l . res) . body)
;  (let ((x (gensym)))
;    `(do ((,x ,l (cdr ,x)) (,var))
;	  ((atom ,x) (setq ,var nil) . ,res)
;          (setq ,var (car ,x))
;	  . ,body)))

(defmacro dolist ((var l . res) . body)
  `(lets ((,var))
     (loop
       (mapc ,l (function (lambda (,var) . ,body)))
       (exit . ,res))))

;;(defmacro boole (op int1 int2)
;;  `(funcall ,op ,int1 ,int2))

;;(setq boole-clr #'(lambda (x y) 0))
;;(setq boole-set #'(lambda (x y) 1))
;;(setq boole-1 #'(lambda (x y) x))
;;(setq boole-2 #'(lambda (x y) y))
;;(setq boole-c1 #'(lambda (x y) (- 1 x)))
;;(setq boole-c2 #'(lambda (x y) (- 1 y)))
;;(setq boole-and #'(lambda (x y) (logand x y)))
;;(setq boole-ior #'(lambda (x y) (logor x y)))
;;(setq boole-xor #'(lambda (x y) (logand 1 (+ x y))))
;;(setq boole-eqv #'(lambda (x y) (- 1 (logand 1 (+ x y)))))
;;(setq boole-nand #'(lambda (x y) (- 1 (logand x y))))
;;(setq boole-nor  #'(lambda (x y) (- 1 (logor x y))))
;;(setq boole-andc1 #'(lambda (x y) (logand (- 1 x) y)))
;;(setq boole-andc2 #'(lambda (x y) (logand x (- 1 y))))
;;(setq boole-orc1 #'(lambda (x y) (logor (- 1 x) y)))
;;(setq boole-orc2 #'(lambda (x y) (logor x (- 1 y))))

(declare (boole-clr boole-and boole-andc2 boole-1
	  boole-andc1 boole-2 boole-xor boole-ior
	  boole-nor boole-eqv boole-c2 boole-orc2
	  boole-c1 boole-orc1 boole-nand boole-set) special)

(setq boole-clr 0
      boole-and 1
      boole-andc2 2
      boole-1 3
      boole-andc1 4
      boole-2 5
      boole-xor 6
      boole-ior 7
      boole-nor 8
      boole-eqv 9
      boole-c2 10
      boole-orc2 11
      boole-c1 12
      boole-orc1 13
      boole-nand 14
      boole-set 15)

(defun vector-replace (v1 v2)
  (do ((end (min (vector-length v1) (vector-length v2)))
       (i 0 (1+ i)))
      ((>= i end) v1)
      (vset v1 i (vref v2 i))))

(defun sequence-p (s) (or (vectorp s) (consp s)))

(defun keywordp (s) (and (symbolp s) (= (sref s 0) (character ":"))))

(defun string-downcase (s) (translate s lower-case))

(defmacro multiple-value-setq (vars form)
  (or (consp vars) (err:argument-type vars 'multiple-value-setq))
  (let ((xxx (gensym)))
    `(let ((,xxx ,form))
       . ,(mapcar vars (function (lambda (x) `(setq ,x (and ,xxx (pop ,xxx)))))))))

(defmacro progv (symbols values . forms)
  (let ((sym (eval symbols))
	(val (eval values)))
    (cond ((or (null sym) (null val))
	   `(progn . ,forms))
	  (t (lets ((s (copy sym))
		    (v (copy val))
		    (len (min (length s) (length v))))
	       (setl (cdr (nthcdr (1- len) s)) nil) ;chop off the symbols and values.
	       (setl (cdr (nthcdr (1- len) v)) nil)
	       `(apply (function (lambda ,s . ,forms)) ',v))))))
	
(defmacro progv (symbols values . forms)
  `(apply `(lambda ,,symbols . ,',forms) ,values))

(macro values (l) `(list . ,l))

(macro eql (l) `(eq . ,l))
(macro endp (l) `(atom . ,l))

(defmacro find (val seq . key)
  (lets ((key (memq ':key key )))
    `(_find ,val ,seq ,(and key (second key)))))
(defun _find (val seq key-func)
  (or key-func (setq key-func 'cr))
  (cond ((vectorp seq)
	 (do ((i 0 (1+ i)))
	     ((>= i (vector-length seq)))
	     (and (eq val (funcall key-func (vref seq i)))
		  (exit (vref seq i)))))
	(t
	 (do ((e seq (cdr e)))
	     ((atom e))
	     (and (eq val (funcall key-func (car e)))
		  (exit (car e)))))))

(defun make-sequence (result-type nitems)
  (match result-type
    ('string (make-strnig nitems))
    ('vector (vector nitems))
    ('list (do ((result nil)
		(i nitems (1- i)))
	       ((= i 0) result)
	       (setq result (cons nil result))))
    (('array 'card8 x) (make-string nitems))
    (('array x y) (vector nitems))
    (t (format "Unsupported type /s for make-sequence/n" result-type)
       (err:argument-type 'make-sequence result-type))))
    

(defun functionp (x)
  (or (and (atom x) (definedp x))
      (and (consp x) (eq (car x) 'lambda))))

(defmacro logbitp (pos val) `(not (0= (logand (logshift 1 ,pos) ,val))))
(defmacro lognot (val) `(logxor -1 ,val))

(defconstant most-positive-fixnum 134217727)
(defconstant most-negative-fixnum -134217728)

;; new multiple-value-bind
;; (to avoid using lambda notation)
(defmacro multiple-value-bind (vars values-form . form)
  (or (consp vars) (funcall 'err:argument-type vars 'multiple-value-bind))
  (let ((xxx (gensym)))
    `(lets ((,xxx ,values-form)
	    . ,(mapcar vars
		       (function (lambda (x)
				   `(,x (and ,xxx (pop ,xxx)))))))
       . ,form)))

(defun typecase:type-predicate (type obj)
  (cond ((atom type)
	 (or (eq type 't)
	     (selectq type
	       (string `(stringp ,obj))
	       (list `(or (consp ,obj) (null ,obj)))
	       ((array simple-array) `(vectorp ,obj))
	       (number `(or (fixp ,obj) (bigp ,obj)))
	       (t nil))
	     ))
	(t
	 (selectq (car type)
	   ((array simple-array) `(vectorp ,obj))
	   ;; mapcar wo tukauto compile ji ni obj ga special ni
	   ;; natte simau.
	   (or `(or . ,(do ((t (cdr type) (cdr t))
			    (res nil))
			   ((atom t) res)
			   (push (typecase:type-predicate (car t) obj) res))))
	   (t nil)
	   ))))
    
(defmacro typecase (obj . clauses)
  `(cond . ,(mapcar clauses
		    (function (lambda (c)
				(cons
				 (or (typecase:type-predicate (first c) obj)
				     (err:argument-type 'type-case (first c)))
				 (cdr c)))))))

(defmacro cl:member (item list . keys)
  (lets ((keyfunc (memq ':key keys))
	 (test (memq ':test keys))
	 (var (gensym)))
    (cond ((or keyfunc test)
	   (setq test (or (and test (second test)) '(function eq)))
	   `(do ((,var ,list (cdr ,var)))
		((atom ,var))
		(and (funcall ,test ,item
			      ,(or (and keyfunc
					`(funcall ,(cadr keyfunc) (car ,var)))
				   `(car ,var)))
		     (exit ,var))))
	  (t `(memq ,item ,list)))))

(declare (break) special)
(defun error (message a1 (a2) (a3) (a4) (a5))
  (format message a1 a2 a3 a4 a5)
  (funcall break))

(macro initialized-vector (x)
  `(vector ,(length x) ',x))

(defmacro cl:push (val loc)
  (cond ((atom loc) `(push ,val ,loc))
	(t `(setf ,loc (cons ,val ,loc)))))
