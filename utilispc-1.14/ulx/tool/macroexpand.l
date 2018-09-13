;;;
;;; macroexpand.l
;;; $Revision: 1.1.1.1 $
;;; $Log: macroexpand.l,v $
;;; Revision 1.1.1.1  2000/09/26 00:51:09  ktanaka
;;;
;;;
;;; Revision 1.1  1991/06/12  05:27:36  ken
;;; Initial revision
;;;
;;;

;;; main function
;;;
(defun macroexpand (m (env))
  (cond ((atom m) m)
	(t
	 (lets ((f (car m))
		(def (and (definedp f) (getd f))))
	   (cond ((consp f) (funcall 'err:illegal-function))
		 ((specialp f) (me:expand-special m))
		 (def (cond ((or (codep def) (eq 'lambda (car def)))
			     (cons f (mapcar (cdr m) 'macroexpand)))
			    ((eq 'macro (car def))
			     (macroexpand (funcall (cdr def) (cdr m))))
			    (t (funcall 'err:illegal-function))))
		 (t (cons f (mapcar (cdr m) 'macroexpand))))))))

;; helper functions for me:expand-special
;; 
(defun me:expand-bind (b)
  (cond ((atom b) b)
	(t `(,(car b) . ,(mapcar (cdr b) 'macroexpand)))))
(defun me:expand-clause (c)
  (mapcar c 'macroexpand))
(defun me:expand-match-clause (c)
  `(,(car c) . ,(mapcar (cdr c) 'macroexpand)))
(defun me:expand-setq (l)
  (do ((m l (cddr m))
	(res nil))
       ((atom m) (nreverse res))
       (push (first m) res)
       (push (macroexpand (second m)) res)))
(defun me:expand-function (mm)
  (cond ((atom mm) mm)
	(t (match mm
	     (('lambda formal . body)
	      `(lambda ,formal . ,(mapcar body 'macroexpand)))
	     (('macro 'lambda formal . body)
	      `(macro lambda ,formal . ,(mapcar body 'macroexpand)))
	     (@ @)))))

;; special expander for special forms
;;
(defun me:expand-special (m)
  (selectq (car m)
    ((prog1 prog2 progn loop or and catch)
     `(,(car m) . ,(mapcar (cdr m) 'macroexpand)))
    ((quote comment go pop) m)
    (function `(function ,(me:expand-function (second m))))
    ((let lets prog)
     `(,(car m) ,(mapcar (second m) 'me:expand-bind)
	 . ,(mapcar (cddr m) 'macroexpand)))
    (cond `(cond . ,(mapcar (cdr m) 'me:expand-clause)))
    ((selectq match)
     `(,(car m) ,(macroexpand (second m))
       . ,(mapcar (cddr m) 'me:expand-match-clause)))
    (setq `(setq . ,(me:expand-setq (cdr m))))
    (push `(push ,(macroexpand (second m)) . ,(cddr m)))
    (t (funcall 'err:illegal-function (car m)))))

;; utility function for expanding whole file.
;;
(defun macroexpand-file (infile outfile)
  (lets ((in (inopen (stream infile)))
	 (out (outopen (stream outfile)))
	 (err:end-of-file (function (lambda (a (b)) (throw 'eof)))))
    (catch 'eof
      (loop
       (let ((sexp (read in))
	     (standard-output out))
	 (prind (macroexpand sexp)))))))
	     
;; utility function for expanding a function definition.
(defun macroexpand-def (sym)
  (lets ((def (getd sym)))
    (cond ((or (eq (car def) 'lambda) (eq (car def) 'macro))
	   (second (macroexpand `(function ,def))))
	  (t def))))
