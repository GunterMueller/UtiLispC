(putd 'defun '(macro lambda (_x) (list 'putd (list 'quote (car _x))
	(list 'function (cons 'lambda (cdr _x)))))) 
(defun exfile (lispfile)
   (catch 'exfile
    (lets ((standard-input (inopen (stream (alloc lispfile)))) 
           (err:end-of-file (function (lambda (stg (dum)) (throw 'exfile)))))
     (loop (eval (read))))))
(exfile "../lispsys.main.l")
(setq a '(
"../lispcomp/comp.l"
"../lispcomp/c-cd.l"
"../lispcomp/c-fix.l"
"../lispcomp/c-list.l"
"../lispcomp/c-map.l"
"../lispcomp/c-pred.l"
"../lispcomp/c-spec.l"
"../lispcomp/compile.l"
"../lispcomp/dump.l"
"../lispcomp/opt.l"
))
(setq b '(
"amuse.l"
"newed.l"
"prind.l"
"sort.l"
"toplevel.l"
"trace.l"
;"common.l"
;"deftype.l"
;"defun.l"
;"struct.l"
))
(mapc a #'exfile)

(mapc b 
      #'(lambda (file)
	 (princ "==== ")
	 (princ file)
	 (princ " ====")
	 (terpri)
	 (exfile file)
	 (compfile file
		   (string-append (substring file 0 (- (string-length file) 2))
		   ".o"))))
(quit)
