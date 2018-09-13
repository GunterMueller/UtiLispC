(comment "%W% %G%")

(defun c:carcdr (fn args loc jmp)
   (cond ((= (length args) 1)
          (setq args (car args))
          (do* ((i (/- (string-length fn) 2) (/1- i))
                (tmp (c:get-cvar))
                (codes `(,(c:form args tmp nil))))
               ((= i 0) (c:gen (c:final tmp loc jmp)) (c:make-begin codes))
               (c:gen (c:check-cons args tmp)
                      `(= ,tmp
                          (,(cond ((= (sref fn i) (character "a")) 'car)
                                  (t 'cdr))
                           ,tmp)))))
         (t (c:argerr args))))

(defun c:set-carcdr (fn)
   (putprop fn `(lambda (args loc jmp) (c:carcdr ',fn args loc jmp)) 'inline))

(mapc '(cr car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr 
        cddar cdddr caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr 
        cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr caddddr 
        cadddddr caddddddr cdddddr cddddddr cdddddddr)
   (function c:set-carcdr))

(alias first car)

(alias second cadr)

(alias third caddr)

(alias fourth cadddr)

(alias fifth caddddr)

(alias sixth cadddddr)

(alias seventh caddddddr)

(defnprop cons
   (lambda (args loc jmp)
    (cond ((= (length args) 2)
	   (lets ((r1) (r2) (nsp))
            `(begin ,(c:eval-twoargs (first args) (second args))
                    ,(c:final
                        `(alcons ,(c:value r1) ,(c:value r2) (dis ,nsp))
                        loc
                        jmp))))
          (t (c:argerr args))))
   inline)

(defnprop ncons
   (lambda (args loc jmp)
    (lets ((a (c:get-cvar)))
     (cond ((= (length args) 1)
            `(begin ,(c:form (first args) a nil)
                    ,(c:final `(alcons ,a nil (dis ,sp)) loc jmp)))
           (t (c:argerr args)))))
   inline)

(defnprop xcons
   (lambda (args loc jmp)
    (cond ((= (length args) 2)
	   (lets ((r1) (r2) (nsp))
            `(begin ,(c:eval-twoargs (first args) (second args))
                    ,(c:final
                        `(alcons ,(c:value r2) ,(c:value r1) (dis ,nsp))
                        loc
                        jmp))))
          (t (c:argerr args))))
   inline)

(defnprop last
   (lambda (args loc jmp)
    (lets ((a (c:get-cvar)))
     (cond ((eq (length args) 1)
            `(begin ,(c:form (car args) a nil)
                    ,(c:check-cons (car args) a)
                    (for (nil (consp (cdr ,a)) ((= ,a (cdr ,a)))))
                    ,(c:final a loc jmp)))
           (t (c:argerr args)))))
   inline)

(defnprop length
   (lambda (args loc jmp)
    (lets ((a (c:get-cvar)) (i (c:get-ivar)) (codes))
     (cond ((eq (length args) 1)
            `(begin ,(c:form (car args) a nil)
                    (for (((= ,i 0)) (consp ,a) ((= ,a (cdr ,a)))) (/+= ,i 1))
                    ,(c:final `(itofix ,i) loc jmp)))
           (t (c:argerr args)))))
   inline)

(defnprop nthcdr
   (lambda (args loc jmp)
    (match args
       ((n l)
        (lets ((a (c:get-cvar)) (b (c:get-cvar)) (i (c:get-ivar)) (codes))
         (cond ((c:constantp n)
                (setq n (c:constant-value n))
                (or (and (fixp n) (<= 0 n)) (c:typerr n))))
         (cond ((and (fixp n) (<= 0 n 7))
                (funcall (get (c:nthcdr-fn n) 'inline) (cdr args) loc jmp))
               (t (or (fixp n) (c:gen (c:form n b nil) (c:check-posfix n b)))
                  (c:gen (c:form l a nil)
                         `(for (((= ,i ,(cond ((fixp n) n) (t `(fixtoi ,b)))))
                                (< 0 ,i)
                                ((= ,i (/- ,i 1))))
                               ,(c:check-cons nil a)
                               (= ,a (cdr ,a)))
                         (c:final a loc jmp))
                  (c:make-begin codes)))))
       (t (c:argerr args))))
   inline)

(defun c:nthcdr-fn (n)
   (nth n '(cr cdr cddr cdddr cddddr cdddddr cddddddr cdddddddr)))

(defnprop nth
   (lambda (args loc jmp)
    (funcall (get 'car 'inline) `((nthcdr . ,args)) loc jmp))
   inline)

(defnprop list
   (lambda (args loc jmp)
    (do ((l args (cdr l)) (n 0 (/1+ n)) (sp sp) (codes))
        ((atom l)
         (c:gen (c:final `(allist ,n (dis ,sp) nil) loc jmp))
         (c:make-begin codes))
        (c:gen-form (car l) (incr sp 1) nil)))
   inline)

(defnprop reverse
   (lambda (args loc jmp)
    (lets ((sp sp) (a (c:get-cvar)) (d (c:get-cvar)) (lsave (incr sp 1)))
     (cond ((= (length args) 1)
            `(begin ,(c:form (car args) a nil)
                    (if (consp ,a)
                        (begin (= ,d nil)
                               (dowhile
                                  (begin ,(c:assign lsave `(cdr ,a))
                                         (= ,d (alcons (car ,a) ,d (dis ,sp)))
                                         ,(c:assign a lsave))
                                  (consp ,a))
                               ,(c:final d loc jmp))
                        ,(c:final a loc jmp))))
           (t (c:argerr args)))))
   inline)

(defnprop nreverse
   (lambda (args loc jmp)
    (cond ((= (length args) 1) (c:nreverse (car args) loc jmp))
          (t (c:argerr args))))
   inline)

(defun c:nreverse (arg loc jmp)
   (lets ((a (c:get-cvar)) (d (c:get-cvar)) (dd (c:get-cvar)))
     `(begin ,(c:form arg a nil)
             (= ,d ,a)
             (= ,dd nil)
             (while (consp ,a)
                    (= ,d ,a)
                    (= ,a (cdr ,a))
                    (= (cdr ,d) ,dd)
                    (= ,dd ,d))
             ,(c:final d loc jmp))))

(defun c:replace-cons (ad args loc jmp)
   (lets ((sp sp))
    (match args
       ((l x)
	(lets ((r1) (r2) (nsp))
         `(begin ,(c:eval-twoargs l x)
                 ,(c:check-cons l (c:value r1))
                 (= (,ad ,(c:value r1)) ,(c:value r2))
                 ,(c:final r1 loc jmp))))
       (x (c:argerr args)))))

(defnprop rplaca
   (lambda (args loc jmp) (c:replace-cons 'car args loc jmp))
   inline)

(defnprop rplacd
   (lambda (args loc jmp) (c:replace-cons 'cdr args loc jmp))
   inline)

(comment defnprop subst nil inline)

(defun c:list-search (elmtype relfn a1 a2 loc jmp)
   (lets ((constfn (c:constfn relfn)) (easyfn (and constfn (c:easyfn relfn))) 
          (item) (lst) (lstsave) (elm) (fnarg) (ag) (sp sp) 
          (spsave sp) (r1) (r2) (nsp) (foundlab (gensym "found")) 
          (j (ncons foundlab)) (nillab (gensym "notfound")) (codes))
    (cond (easyfn (c:gen (c:eval-twoargs a1 a2))
                  (cond ((= sp nsp) (setq item r1))
                        (t (setq item (c:get-cvar)) (c:gen-assign item r1)))
                  (setq lst r2 elm (c:get-cvar) lstsave r2))
          (t (c:gen-form a1 (/1+ sp) nil)
             (setq lst (c:get-cvar) item (incr sp 1))
             (c:gen-form a2 (/1+ sp) nil)
             (setq lstsave (incr sp 1) elm lst)
             (cond ((not constfn)
                    (c:gen-form relfn (/1+ sp) nil)
                    (setq fnarg (incr sp 1))))
             (c:gen-assign lst lstsave)))
    (setq ag `((evaluated ,item) (evaluated ,elm)))
    (c:gen `(for (nil (consp ,lst) nil)
                 ,(c:assign elm `(car ,lst))
                 ,(and (eq elmtype 'alist)
                       `(begin ,(c:check-cons nil elm) (= ,elm (car ,elm))))
                 ,(match relfn
                     (('function ('lambda formals . body))
                      (c:aplambda formals ag body nil j))
                     (('quote ('lambda formals . body))
                      (c:aplambda formals ag body nil j))
                     (('function sym)
                      (cond ((or (eq sym 'eq) (eq sym 'equal))
                             (and (eq sym 'eq) (setq sym '==))
                             `(if (,sym ,item ,elm) (goto ,foundlab)))
                            (t (c:apsy sym ag nil j))))
                     (('quote sym)
                      (cond ((or (eq sym 'eq) (eq sym 'equal))
                             (and (eq sym 'eq) (setq sym '==))
                             `(if (,sym ,item ,elm) (goto ,foundlab)))
                            (t (c:apsy sym ag nil j))))
                     (fn (c:funcall fnarg ag nil j)))
                 ,(cond (easyfn `(= ,lst (cdr ,lst)))
                        (t `(begin ,(c:assign lst lstsave)
                                   ,(c:assign lst `(cdr ,lst))
                                   ,(c:assign lstsave lst)))))
           (c:symbol nil loc nil)
           `(goto ,(c:get-flab jmp nillab))
           `(label ,foundlab)
           (c:item-found elmtype lstsave loc (and jmp (ncons (car jmp))))
           `(label ,nillab))
    (c:make-begin codes)))

(defun c:item-found (elmtype lst loc jmp)
   (c:final-const
      (cond ((eq elmtype 'alist) `(car ,(c:value lst))) (t (c:value lst)))
      loc
      jmp))

(defnprop memq
   (lambda (args loc jmp)
    (match args
       ((x ('quote y))
        (cond ((atom y)
	       `(begin ,(and (not (c:nosidep x)) (c:form x nil nil))
		       ,(c:symbol nil loc jmp)))
	      ((and (null loc) (<= (length y) 8))
               (lets ((a (c:get-cvar)))
	        `(begin ,(c:form x a nil)
		        (if ,(c:selq-cond a y)
		            ,(c:symbol t loc jmp)
		            ,(c:symbol nil loc jmp)))))
              (t (c:list-search 'element '(function eq) x `',y loc jmp))))
       ((x y) (c:list-search 'element '(function eq) x y loc jmp))
       (x (c:argerr args))))
   inline)

(defnprop member
   (lambda (args loc jmp)
    (match args
       ((x y) (c:list-search 'element '(function equal) x y loc jmp))
       (x (c:argerr args))))
   inline)

(defnprop mem
   (lambda (args loc jmp)
    (match args
       ((fn x y) (c:list-search 'element fn x y loc jmp))
       (x (c:argerr args))))
   inline)

(defnprop assq
   (lambda (args loc jmp)
    (match args
       ((x y) (c:list-search 'alist '(function eq) x y loc jmp))
       (x (c:argerr args))))
   inline)

(defnprop assoc
   (lambda (args loc jmp)
    (match args
       ((x y) (c:list-search 'alist '(function equal) x y loc jmp))
       (x (c:argerr args))))
   inline)

(defnprop ass
   (lambda (args loc jmp)
    (match args
       ((x y fn) (c:list-search 'alist fn x y loc jmp))
       (x (c:argerr args))))
   inline)

(comment defnprop delq nil inline)

(comment defnprop remq nil inline)

