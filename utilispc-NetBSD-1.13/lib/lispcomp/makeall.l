(putd 'defun '(macro lambda (_x) (list 'putd (list 'quote (car _x))
	(list 'function (cons 'lambda (cdr _x)))))) 
(defun exfile (lispfile)
   (catch 'exfile
    (lets ((standard-input (inopen (stream (alloc lispfile)))) 
           (err:end-of-file (function (lambda (stg (dum)) (throw 'exfile)))))
     (loop (eval (read))))))
(setq a '(
"comp.l"
"c-cd.l"
"c-fix.l"
"c-list.l"
"c-map.l"
"c-pred.l"
"c-spec.l"
"compile.l"
"dump.l"
"opt.l"
))
(exfile "../lispsys.main.l")
(mapc a #'exfile)
(mapc a
      #'(lambda (file)
	 (princ "==== ")
	 (princ file)
	 (princ " ====")
	 (terpri)
	 (compfile file
		  (string-append (substring file 0 (- (string-length file) 2))
				 ".o"))))
(quit)
