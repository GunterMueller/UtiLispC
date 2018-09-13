(setq defsetf-table nil)

(defmacro defsetf (acfn upfn) `(push `(,',acfn . ,',upfn) defsetf-table))

(defmacro setf (place value)
   (lets ((x))
    (cond ((atom place) `(setq ,place ,value))
	  ((setq x (assq (car place) defsetf-table))
	   `(,(cdr x) ,@(cdr place) ,value))
	  (t `(setl ,place ,value)))))


;;; ken
(defmacro defsetf (access-fn update-fn . rest)
  (cond ((symbolp update-fn)
	 (and rest (err:argument-type rest 'defsetf))
	 `(progn (putprop 'defsetf ',update-fn ',access-fn) ',access-fn))
	((atom update-fn) (err:argument-type update-fn 'defsetf))
	;; complex defsetf
	(t (match rest
	     (((store-var) . body)
	      `(progn (putprop 'defsetf ',(list update-fn store-var body) ',access-fn)
		      ',access-fn))
	      
	     (t (err:argument-type 'rest 'defsetf))))))

;;; defsetf no lambda-list wo function fuu ni suru baai.
(defun setf-sitauke (place value)
  (lets ((x))
    (cond ((atom place) `(setq ,place ,value))
	  ((setq x (get 'defsetf (car place)))
	   (cond ((atom x) ;; simple form
		  `(,x . ,(append (cdr place) `(,value))))
		 (t ;; complex form
		  (apply `(lambda ,(cons (second x) (first x))
			    . ,(third x))
			 (cons value (cdr place))))))
	  (t `(setl ,place ,value)))))

;;; defsetf no lambda-list wo defmacro fuu ni suru baai.
;(defun setf-sitauke (place value)
;  (lets ((x))
;    (cond ((atom place) `(setq ,place ,value))
;	  ((setq x (get 'defsetf (car place)))
;	   (cond ((atom x) ;; simple form
;		  `(,x . ,(append (cdr place) `(,value))))
;		 (t ;; complex form
;		  (eval `(match (cdr place)
;			   (,(first x) (let ((,(second x) value)) . ,(third x)))
;			   (xx (err:argument-type (cdr place) (car place))))))))
;	  (t `(setl ,place ,value)))))

(defun setf-sitauke (place value)
  (lets ((x))
    (cond ((atom place) `(setq ,place ,value))
	  ((setq x (get 'defsetf (car place)))
	   (cond ((atom x) ;; simple form
		  `(,x . ,(append (cdr place) `(,value))))
		 (t ;; complex form
		  (funcall `(lambda (,(second x) arg)
			      (match arg
				(,(first x) (progn . ,(third x)))
				(xx (err:argument-type arg ',(car place)))))
			   value (cdr place)))))
	  (t `(setl ,place ,value)))))

(macro setf (spec)
  (do ((ss spec (cddr ss))
       (n 0 (1+ n))
       (res (list 'progn)))
      ((or (atom ss) (atom (cdr ss)))
       (and (consp ss) (err:number-of-arguments spec 'setf))
       (cond ((consp (cddr res)) (nreverse res))
	     (t (car res))))
      (push (setf-sitauke (first ss) (second ss)) res)))

;(setf (access-fn . args) value)
;-> (match ,args
;     (lambda-list (let ((store-vars ,value) . form))
;     (t (err:argument-type args ',access-fn)))
