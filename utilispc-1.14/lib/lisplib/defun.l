(comment "defun.l")

(defmacro cl:defun (name formals . body)
   (lets ((nec (formal-nec formals)) 
          (opt (formal-optkey (find-& formals '&optional))) 
          (opts (formal-supp (find-& formals '&optional))) 
          (rest (formal-rest (find-& formals '&rest))) 
          (key (formal-optkey (find-& formals '&key))) 
          (keys (formal-supp (find-& formals '&key))) 
          (aux (formal-optkey (find-& formals '&aux))) 
          (args (nconc (copy nec)
                       (mapcar opt (function car))
                       (remq nil (copy opts))
                       (and rest `(,rest))
                       (mapcar key (function car))
                       (remq nil (copy keys))
                       (mapcar aux (function car))))
          (honmono-name (honmono-name name)))
    `(progn (macro ,name (l) 
             `(let ,(let-locals ',nec l)
               ,`(lets ,(lets-locals
                           ',opt
                           ',opts
                           ',rest
                           ',key
                           ',keys
                           ',aux
                           (nthcdr ,(length nec) l))
                  ,'(,honmono-name . ,args))))
            (defun ,honmono-name ,args . ,body))))

(defun find-& (l x)
   (lets ((a (some l (function (lambda (y) (eq x y)))))) (and a (cdr a))))

(defun formal-nec (l)
   (cond ((or (null l) (&-p (car l))) nil)
         (t `(,(car l) . ,(formal-nec (cdr l))))))

(defun formal-optkey (l)
   (cond ((or (null l) (&-p (car l))) nil)
         ((or (symbolp (car l)) (atom (cdar l)))
          `((,(unkey (car l))) . ,(formal-optkey (cdr l))))
         (t `((,(unkey (caar l)) ,(cadar l)) . ,(formal-optkey (cdr l))))))

(defun formal-supp (l)
   (cond ((or (null l) (&-p (car l))) nil)
         ((<= (length (car l)) 2) `(nil . ,(formal-supp (cdr l))))
         (t `(,(caddar l) . ,(formal-supp (cdr l))))))

(defun formal-rest (l) (and l (car l)))

(defun let-locals (nec l) (locals-nec nec l))

(defun lets-locals (opt opts rest key keys aux args)
   (lets ((len (length opt)))
    (nconc (locals-opt opt opts args)
           (locals-keyrest
              key
              keys
              rest
              (and (< len (length args)) (nthcdr len args)))
           aux)))

(defun locals-nec (onec l) (pairlis onec l))

(defun locals-opt (opt opts l)
   (cond ((atom opt) nil)
         ((atom l)
          (nconc (copy opt)
                 (mapcan opts (function (lambda (x) (and x `((,x))))))))
         ((car opts)
          `((,(caar opt) ,(car l)) 
            (,(car opts) t) . 
            ,(locals-opt (cdr opt) (cdr opts) (cdr l))))
         (t `((,(caar opt) ,(car l)) . 
              ,(locals-opt (cdr opt) (cdr opts) (cdr l))))))

(defun locals-keyrest (key keys rest args)
   (lets ((res) (rr))
    (do ((l args (cddr l)) (uk))
        ((or (atom l) (atom (cdr l))))
        (setq uk (unkeyword (car l)))
        (cond (rest (push `',(car l) rr) (push uk rr)))
        (push `(,uk ,(second l)) res))
    (do ((k key (cdr k)) (ks keys (cdr ks)) (supp))
        ((atom k)
         (and rest (push `(,rest ,(cons 'list (nreverse rr))) res))
         (nreverse res))
        (setq supp (cond ((assq (caar k) res) t) (t nil)))
        (or supp (push (car k) res))
        (and (car ks) (push `(,(car ks) ,supp) res)))))

(defun &-p (s) (and (symbolp s) (= (sref (pname s) 0) (character "&"))))

(defun key-p (s) (and (symbolp s) (= (sref (pname s) 0) (character ":"))))

(defun find-keyword (k l)
   (lets ((kk (keyword k))) (some l (function (lambda (x) (eq x kk))))))

(defun keyword (s) (intern (string-append ":" s)))

(defun unkeyword (s) (intern (substring s 1)))

(defun unkey (s) (cond ((key-p s) (unkeyword s)) (t s)))

(defun pairlis (l1 l2)
   (cond ((or (atom l1) (atom l2)) nil)
         (t `((,(car l1) ,(car l2)) . ,(pairlis (cdr l1) (cdr l2))))))

(defun honmono-name (name) (intern (string-append "_" name)))

(defun utiarg (argl)
   (lets ((nec t))
    (mapcan argl
       (function
        (lambda (x)
         (cond ((or (eq x '&optional) (eq x '&rest)) (setq nec nil) nil)
               ((or nec (consp x)) `(,x))
               (t `((,x)))))))))

(or (definedp 'ut:defun) (putd 'ut:defun (getd 'defun)))

(defmacro defun (name argl . body)
   (lets ((newbody
             (cond ((and (consp body) (stringp (car body))) (cdr body))
                   (t body))))
    (cond ((or (memq '&rest argl) (memq '&key argl))
           `(cl:defun ,name ,argl . ,newbody))
          (t `(ut:defun ,name ,(utiarg argl) . ,newbody)))))

(macro cl:apply (x)
   (lets ((morearg (car (last x))) (fn (car x)) 
          (argl (nreverse (cdr (reverse (cdr x))))))
    `(apply-sub ,fn (nconc ,(cons 'list argl) ,morearg))))

(defun apply-sub (fn args)
   (cond ((macrop fn) (eval (eval `(exm ,fn . ,(macroargs args)))))
         (t (apply fn args))))

(defun macrop (fn)
   (cond ((symbolp fn)
          (and (definedp fn) (consp (getd fn)) (eq 'macro (car (getd fn)))))
         (t (eq 'macro (car fn)))))

(defun macroargs (args)
   (mapcar args
      (function
       (lambda (x) (cond ((and (symbolp x) (key-p x)) x) (t `',x))))))

(defmacro macrolet (ml . body)
   (lets ((syms (mapcar ml (function (lambda (x) (gensym)))))
	  (dummy (save-defs syms ml))
	  (dummy (local-defs ml))
	  (expanded (expand-args body))
	  (dummy (restore-defs syms ml)))
     `(progn .,expanded)))

;(defmacro macrolet (ml . body)
;   (lets ((syms (mapcar ml (function (lambda (x) (gensym))))))
;    `(lets ,(save-defs syms ml)
;       (prog2 (progn . ,(local-defs ml))
;              (progn . ,body)
;              (progn . ,(restore-defs syms ml))))))

(defun save-defs (syms ml)
   (do ((s syms (cdr s)) (m ml (cdr m)) (res))
       ((or (atom s) (atom m)))
       (putd (car s) (and (definedp (caar m)) (getd (caar m))))))

(defun local-defs (ml)
   (mapcar ml (function (lambda (x) (eval `(defmacro . ,x))))))

(defun restore-defs (syms ml)
   (do ((s syms (cdr s)) (m ml (cdr m))(def))
       ((or (atom s) (atom m)))
       (setq def (getd (car s)))
       (cond (def (putd (caar m) def))
	     (t (make-undefined (caar m))))))

(defun expand-rec (l)
  (cond ((atom l)l)
	(t
	 (lets ((sym (car l))
		(body (cdr l))
		(def))
	   (cond ((and (consp sym)
		       (eq (car sym) 'macro))
		  (expand-rec (funcall (cdr sym)(cdr l))))
		 ((and (consp sym)
		       (eq (car sym) 'lambda))
		  `((lambda ,(cadr sym) .,(expand-args (cddr sym)))
		    .,(expand-args body)))
		 ((eq sym 'lambda)
		  `(lambda ,(car body) .,(expand-args (cdr body))))
		 ((specialp sym)
		  (special-expand l))
		 ((not (definedp sym))
		  (princ "Undefined symbol : " terminal-output)
		  (princ sym terminal-output)
		  (terpri terminal-output)
		  l)
		 ((and (consp (setq def (getd sym)))
		       (eq (car def) 'macro))
		  (expand-rec (funcall (cdr def)(cdr l))))
		 (t
		  `(,sym .,(expand-args body))))))))

(defun expand-args (l)
  (mapcar l (function expand-rec)))

(defun special-expand (l)
  (match l
    (('cond . rest)
     `(cond .,(mapcar rest (function expand-args))))
    (('selectq key . rest)
     `(selectq ,(expand-rec key) 
	.,(mapcar rest (function (lambda (x) 
				   `(,(car x) .,(expand-args (cdr x))))))))
    (('prog varlist . body)
     `(prog ,(expand-varlist varlist) ., (expand-args body)))
    (('let varlist . body)
     `(let ,(expand-varlist varlist) ., (expand-args body)))
    (('lets varlist . body)
     `(lets ,(expand-varlist varlist) ., (expand-args body)))
    (('quote . body) l)
    (('go . body) l)
    (('comment . body) l)
    (('push val var)
     `(push ,(expand-rec val) ,var))
    (('pop . body) l)
    (('setq . body)
     (do ((ll body (cddr ll))
	  (ret nil))
       ((atom ll)
	`(setq .,ret))
       (setq ret (nconc ret `(,(car ll),(expand-rec (cadr ll)))))))
    (('match key . rest)
     `(match ,(expand-rec key) 
	.,(mapcar rest (function (lambda (x) 
				   `(,(car x) .,(expand-args (cdr x))))))))
    ((sym . body)
     `(,sym .,(expand-args body)))))

(defun expand-varlist (l)
  (do ((ll l (cdr ll))
       (ret nil))
    ((atom ll)(nreverse ret))
    (push `(,(caar ll) .,(expand-args (cdar ll))) ret)))

