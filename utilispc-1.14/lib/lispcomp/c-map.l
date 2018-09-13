(defnprop map
   (lambda (args loc jmp) (c:map-fn 'sublist nil args loc jmp))
   inline)

(defnprop maplist
   (lambda (args loc jmp) (c:map-fn 'sublist 'list args loc jmp))
   inline)

(defnprop mapcon
   (lambda (args loc jmp) (c:map-fn 'sublist 'nconc args loc jmp))
   inline)

(defnprop mapc
   (lambda (args loc jmp) (c:map-fn 'element nil args loc jmp))
   inline)

(defnprop mapcar
   (lambda (args loc jmp) (c:map-fn 'element 'list args loc jmp))
   inline)

(defnprop mapcan
   (lambda (args loc jmp) (c:map-fn 'element 'nconc args loc jmp))
   inline)

(defun c:map-fn (next resfn args loc jmp)
   (and (<> (length args) 2) (c:argerr args))
   (lets ((constfn (c:constfn (second args))) 
          (easyfn (and (neq resfn 'list) constfn (c:easyfn (second args)))) 
          (lst (c:get-cvar)) (lstsave) (res) (ressave) (ressave2) (item) (ag) 
          (fnarg) (tmp) (lab (gensym "map")) (sp sp))
    (cond (easyfn (and (eq next 'element) (setq item (c:get-cvar))))
          (t (setq item lst lstsave (incr sp 1))
             (or constfn (setq fnarg (incr sp 1)))))
    (cond (loc (setq ressave (cond (easyfn (c:get-cvar)) (t (incr sp 1))))
               (and (eq resfn 'nconc)
                    (cond (easyfn
			   (setq ressave2 (c:get-cvar)) (setq tmp ressave2))
                          (t (setq ressave2 (incr sp 1) tmp (c:get-cvar)))))))
    (and (or (eq resfn 'some) (eq resfn 'every) (and loc resfn))
	 (setq res (cond (easyfn (c:get-cvar)) (t lst))))
    (setq ag `((evaluated ,item)))
    `(begin ,(cond (constfn (c:form (first args) lst nil))
                   (t `(begin ,(c:form (first args) lstsave nil)
                              ,(c:form (second args) fnarg nil)
                              ,(c:assign lst lstsave))))
            ,(and ressave
                  (neq resfn 'some)
                  (neq resfn 'every)
                  (c:assign ressave (cond ((null resfn) lst) (t nil))))
            ,(and ressave2 (c:assign ressave2 nil))
            (while (consp ,lst)
                   ,(and lstsave (c:assign lstsave `(cdr ,lst)))
                   ,(and ressave (eq resfn 'some) (c:assign ressave lst))
                   ,(and (eq next 'element) (c:assign item `(car ,lst)))
                   ,(match (second args)
                       (('function ('lambda formals . body))
                        (and (<> (length formals) 1) (c:argerr args))
                        (c:aplambda formals ag body res nil))
                       (('quote ('lambda formals . body))
                        (and (<> (length formals) 1) (c:argerr args))
                        (c:aplambda formals ag body res nil))
                       (('function sym) (c:apsy sym ag res nil))
                       (('quote sym) (c:apsy sym ag res nil))
                       (fn (c:funcall fnarg ag res nil)))
                   ,(and ressave
                         (selectq resfn
                            ('list
                             (c:assign
                                ressave
                                `(alcons ,res ,(c:value ressave) (dis ,sp))))
                            ('nconc
                             `(if (consp ,res)
                                  (if (atom ,(c:value ressave2))
                                      (begin ,(c:assign ressave res)
                                             ,(c:assign ressave2 res))
                                      (begin ,(and (not easyfn) 
                                             (c:assign tmp ressave2))
                                             (while (consp (cdr ,tmp)) 
                                                    ,(c:assign tmp `(cdr ,tmp)))
                                             ,(c:assign `(cdr ,tmp) res) 
                                             ,(c:assign ressave2 res)))))))
                   ,(cond ((eq resfn 'some)
                           `(if (!= ,res nil)
                                (begin ,(c:final ressave loc nil)
                                       (goto ,(c:get-tlab jmp lab)))))
                          ((eq resfn 'every)
                           `(if (== ,res nil)
                                (begin ,(c:final nil loc nil)
                                       (goto ,(c:get-flab jmp lab))))))
                   ,(c:assign lst (or lstsave `(cdr ,lst))))
	    ,(selectq resfn
	       (some (c:symbol nil loc jmp))
	       (every (c:symbol t loc jmp))
	       (list (c:nreverse `(evaluated ,ressave) loc jmp))
	       (t (c:final ressave loc jmp)))
            ,(and (or (eq resfn 'some) (eq resfn 'every)) `(label ,lab)))))

(defnprop funcall
   (lambda (args loc jmp)
    (match args
     ((fn . rest)
      (cond ((and (atom fn) (not (symbolp fn))) (c:error "illegal function"))
	    (t (lets ((sp sp) (fstarg (1+ sp)) (codes))
	        (c:gen-evalargs-push args)
		(c:gen-final
		   `(funcall ,(c:var fstarg) ,(length rest) (dis ,sp)) loc jmp)
		(c:make-begin codes)))))
     (args (c:argerr args))))
   inline)

(defnprop evaluated
   (lambda (args loc jmp) (c:final (car args) loc jmp)) inline)

(defnprop some
   (lambda (args loc jmp) (c:map-fn 'element 'some args loc jmp))
   inline)

(defnprop every
   (lambda (args loc jmp) (c:map-fn 'element 'every args loc jmp))
   inline)

(comment "mapv and mapvector")
