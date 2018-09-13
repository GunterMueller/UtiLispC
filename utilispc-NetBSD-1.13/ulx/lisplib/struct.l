;;;
;;; Common Lisp defstruct Emulation
;;;
;;;
;;; $Revision: 1.1.1.1 $
;;; $Log: struct.l,v $
;;; Revision 1.1.1.1  2000/09/26 00:51:09  ktanaka
;;;
;;;
;;; Revision 1.7  1991/10/18  09:46:53  ken
;;; print:vector-or-struct ga ? vector wo kaesu younisita. (? no atai wo
;;; chanto settei suru tame.)
;;;
;;; Revision 1.6  1991/10/07  12:42:31  ken
;;; struct no print function no teigi wo ireta.
;;;
;;; Revision 1.5  1991/02/25  18:41:48  ken
;;; defstruct no kekka no constructor macro chuuni vector wo umekomanai
;;; youni henkou.  koreha compile no tameno henkoudearu.
;;;
;;; Revision 1.4  1991/02/09  06:54:25  ken
;;; predicate option no tuika.
;;;
;;; Revision 1.3  90/12/10  16:54:17  ken
;;; chotto machigai shuusei
;;; 
;;; Revision 1.2  90/12/10  16:51:40  ken
;;; struct-p wo kuwae, print-struct de slot ga struct datta baai struct dearu
;;; kotowo meiji suru youni sita.
;;;
;;; Revision 1.1  90/12/08  16:54:04  ken
;;; Initial revision
;;;
;;;
(defun unkeyword (x) (intern (substring x 1)))

(defun intern-name  (s1 s2) (intern (string-append s1 s2)))

(defmacro defstruct (name . slots-spec)
  (lets ((struct-name (cond ((atom name) name) (t (pop name))))
	 (options (and (consp name) name))
	 (constructor (let ((a (assq ':constructor options)))
			(cond (a (second a))
			      (t (intern-name "make-" struct-name)))))
	 (include (assq ':include options))
	 (conc-name (assq ':conc-name options))
	 (predicate (let ((a (assq ':predicate options)))
		      (cond (a (second a))
			    (t (intern-name struct-name "-p")))))
	 (print-function (let ((a (assq ':print-function options)))
			   (and a (second a))))
	 (accessor-prefix
	  (cond ((null conc-name) (string-append struct-name "-"))
		((second conc-name) (string (second conc-name)))
		(t "")))
	 (slot-names (and include (cdr (get (second include) 'slot-names))))
	 (default-vals (and include (cdr (get (second include) 'default-init)))))
    ;; collect slot names and default values.
    (setq default-vals
	  `(',struct-name
	    . ,(append
		default-vals
		(mapcar slots-spec
			(function (lambda (x) (and (consp x) (consp (cdr x))
						   (second x))))))))
    (setq slot-names
	  `(struct-name
	    . ,(append
		slot-names
		(mapcar slots-spec
			(function (lambda (x)
				    (cond ((consp x) (car x)) (t x))))))))
    ;; generate macro body.
    `(prog1
	 ',struct-name
       ;; put default value list and struct-name list.
       (putprop ',struct-name ',slot-names 'slot-names)
       (putprop ',struct-name ',default-vals 'default-init)
       (putprop ',struct-name t 'structure-p)
       ,(and include
	     `(putprop ',struct-name ',(second include) 'structure-include))
       ,(and constructor
	    `(macro ,constructor (init)
	       `(lets ((l (mapcar (get ',',struct-name 'default-init) 'eval))
		       (v (vector (length l) l)))
		  . ,(do ((slot init (cddr slot))
			  (res '(v)))
			 ((or (atom slot) (atom (cdr slot))) res)
			 (push `(,(intern-name ,accessor-prefix (unkeyword (first slot)))
				 v ,(second slot))
			       res)))))
       ,(and predicate
	    `(defun ,predicate (v)
	       (and (struct-p v)
		    (do ((name (vref v 0) (get name 'structure-include)))
			((not name) nil)
		      (and (eq name ',struct-name) (exit t))))))
       ,(and predicate
	     ;; see deftype.l
	     `(putprop ',struct-name ',predicate 'sys::type-predicate))
       ,(and print-function
	    `(putprop ',struct-name ',print-function 'print-function))
       . ,(let ((index 0))
	    (mapcar
	     (cdr slot-names)
	     (function
	      (lambda (name)
		(setq index (1+ index))
		`(defmacro ,(intern-name accessor-prefix name) (obj . val)
		   (cond (val `(vset ,obj ,,index ,(car val)))
			 (t `(vref ,obj ,,index)))))))))))

(defun struct-p (v)
  (lets ((name (and (vectorp v) (0< (vector-length v)) (vref v 0))))
    (not (null (and name (symbolp name)
	 (get name 'slot-names))))))

(defun print-struct (v (tarse nil))
  (lets ((name (vref v 0))
	 (slot-names (get name 'slot-names))
	 (print-function (get name 'print-function)))
    (or slot-names (progn (format "This is not a struct/n") (err:argument-type)))
    (cond (tarse (princ v) (terpri))
	  (t (or (and print-function
		      *print-pretty*
		      (funcall print-function v terminal-output 1))
		 (do ((i 0 (/1+ i))
		      (name slot-names (cdr name)))
		     ((<= (vector-length v) i))
		     (let ((slotval (vref v i)))
		       (format " /s: /s = " i (car name))
		       (cond ((struct-p slotval) (print-struct slotval t))
			     (t (format "/s/n"
					(or (and (stringp slotval)
						 (> (string-length slotval) 20)
						 (string-append
						  (substring slotval 0 20) "..."))
					    slotval)))))))))))

(defun print:vector-or-struct (vec (slash))
  (cond ((struct-p vec)
	 (princ "S#<") (princ (vref vec 0)) (princ " ")
	 (princ (+ 3 (address vec)))
	 (princ ">"))
	(t (print:vector vec slash)))
  vec))

(setq print:vector (function print:vector-or-struct))
(setq *print-pretty* nil)
