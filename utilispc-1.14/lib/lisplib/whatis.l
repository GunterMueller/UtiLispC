(defun functype (f)
   (cond ((codep f) 'subr)
         ((consp f)
          (match f
             (('lambda . x) 'expr)
             (('macro 'lambda ('@) ('match '@ x . y)) 'defmacro)
             (('macro 'lambda . x) 'macro)
             (('macro . x) 'cmacro)))))
(defun describe (fname code)
   (cond ((get fname 'function-description))
         ((/0> (maxarg code)) '(LSUBR))
         (t (lets ((temps '(t9 t8 t7 t6 t5 t4 t3 t2 t1 t0)) 
                   (n (length temps)) (i (minarg code)))
             (do ((result (nthcdr (/- n i) temps)))
                 ((>= i (maxarg code)) (reverse result))
                 (incr i 1)
                 (push (ncons (nth (/- n i) temps)) result))))))
(defun getdef (fname)
   (lets ((def (getd fname)))
    (cond ((or (codep def)
               (and (consp def) (eq (car def) 'macro) (codep (cdr def))))
           (or (get fname 'previous-definition) def))
          (t def))))
(defun whatis (fname)
   (cond ((definedp fname)
          (lets ((def (getdef fname)))
           (selectq (functype def)
              (expr `(,fname . ,(second def)))
              (subr `(,fname . ,(describe fname def)))
              (macro `(macro ,fname . ,(third def)))
              (defmacro `(defmacro ,fname . ,(first (third (fourth def)))))
              (cmacro `(macro ,fname . ,(describe fname (cdr def)))))))
         ((specialp fname) `(special ,fname))))
(exfile (string-append system_lib_path "lisplib/description"))
