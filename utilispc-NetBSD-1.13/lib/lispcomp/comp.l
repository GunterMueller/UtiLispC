(comment "%W% %G%")

(declare
   (cstream hstream compiled-funcs compfile-optimize
    compiling-funcs c-chartable ncodepieces nliterals literals
    current-fn current-cfn
    codes sp special-vars local-vars current-subr cvars ivars
    root-symbols minarg maxarg r1 r2 nsp
    loop-exit-label loop-special-vars loop-value
    prog-exit-label prog-special-vars prog-value prog-tags
    catch-tags
    typecheck tcheck-var
    fentry-label ftop-label return-value
    compile-debug-mode compile-readtable fix-flag
   )
   special)

(defun a ((flag)) (prind (c:test 'soko flag)))

(defun goyo nil (catch 'goyo (my-break)))

(defun riki nil (throw 'goyo))

(defun c:test (fn flag)
   (lets ((def (getd fn))
	  (hstream standard-output)
	  (cstream standard-output)
	  (compiled-funcs)
	  (compiling-funcs)
	  (ncodepieces 1)
	  (nliterals 1) (literals `((,fn . 0)))
	  (minarg 0) (maxarg 0))
    (setq zzz (c:lambda (second def) (cddr def) fn))
    (and flag (setq zzz (c:optimize zzz)))
    zzz))

(comment "kore wa inchiki")

(defun c:get-cvar nil (lets ((v (gensym "cvar"))) (push v cvars) v))

(defun c:get-ivar nil (lets ((v (gensym "ivar"))) (push v ivars) v))

(defun c:int-ivar (vs) (mapcar vs (function (lambda (v) `(int ,v)))))

(defun c:in-cvar (x) nil)

(defun c:in-root (x) (memq x root-symbols))

(setq root-symbols '(nil t))

(comment "kokokara hajimaru")

(comment "generate codes")

(macro c:gen (x) `(c:gen* (list . ,x)))

(defun c:gen* (x)
   (mapc x (function (lambda (c) (and (consp c) (push c codes))))))

(macro c:gen-form (x) `(c:gen (c:form . ,x)))

(macro c:gen-progn (x) `(c:gen (c:progn . ,x)))

(macro c:gen-final (x) `(c:gen (c:final . ,x)))

(macro c:gen-assign (x) `(c:gen (c:assign . ,x)))

(comment "lambda")

(defun c:lambda (formals body fn)
   (lets ((codes) (special-vars) (local-vars) (current-fn fn)
	  (type-list) (sp 0) (regs)  
	  (cvars) (ivars)
	  (prog-tags) (catch-tags)
	  (tcheck-var (and typecheck (c:get-cvar)))
	  (return-value (c:get-cvar))
	  (fentry-label (gensym "entry")) (ftop-label (gensym "top")))
    (c:gen `(label ,fentry-label)
	   (c:formals formals)
	   `(label ,ftop-label)
	   (c:progn body return-value nil)
           (and special-vars (c:undo-special nil))
           `(return ,return-value))
    (c:make-block `(*sp . ,(nconc cvars (c:int-ivar ivars))) codes)))

(defun c:formals (formals)
   (do ((l formals (cdr l))
        (n 0)
        (flag)
        (sym)
        (form)
        (codes)
        (len (length formals))
        (nopts (apply (function /+)
                    (mapcar formals
                       (function (lambda (y) (cond ((consp y) 1) (t 0))))))))
       ((atom l)
        (setq minarg (- len nopts))
        (setq maxarg len)
        `(begin 
          (if ,(cond ((0= nopts) `(!= na ,n))
		     (t `(|| (< na ,(- n nopts)) (< ,n na))))
	      (parerr)) .
          ,(nreverse codes)))
       (setq sym (car l))
       (cond ((symbolp sym)
              (and flag (c:error "illegal lambda list" formals)))
             ((or (atom sym) (not (symbolp (car sym))))
              (c:error "illega lambda var" sym))
             (t (setq sym (car sym) form (cdr sym) flag t)
                (c:gen `(if (== na ,n)
                            (begin ,(c:progn form '*--fp nil)
                                   (= na (/+ na 1)))))))
       (incr n 1)
       (c:shift-local 1)
       (cond ((c:specialp sym)
              (c:gen (c:bind-special
                      sym
                      (cond ((/0= nopts) (/- n len)) (t `(ag (/- na ,n)))))))
	     (t (c:push-local sym 0)))))

(comment "local variable manipulation")

(defun c:check-var (x)
   (and (not (symbolp x)) (c:error "illegal variable" x)))

(defmacro c:push-local (sym n) `(push (cons ,sym ,n) local-vars))

(defmacro c:local-sym (sym) `(assq ,sym local-vars))

(defun c:shift-local (n)
   (mapc local-vars (function (lambda (x) (rplacd x (- (cdr x) n))))))

(comment "special variable manipulation")

(defun c:declare (sym prop (val t)) (putprop sym val prop))

(defun c:specialp (sym) (get sym 'special))

(defun c:bind-special (sym val)
   (lets ((a (c:get-cvar)))
    (push sym special-vars)
    `(begin (pushb (= ,a ,(c:special-sym sym)))
            (pushb (value ,a))
            (= (value ,a) ,(c:value val))))))

(defun c:undo-special (to)
   (do ((l special-vars (cdr l)) (i 0 (1+ i)) (a (c:get-cvar)) (codes))
       ((eq l to) (c:gen `(+= bstack ,i)) (c:make-begin codes))
       (c:gen `(= ,a (star (/+ bstack ,i)))
              `(= (value (star (/+ bstack ,(incr i 1)))) ,a))))

(defun c:special-sym (sym)
   (lets ((p (assq sym literals)))
    (cond (p (c:literal (cdr p))) (t (c:push-literal sym)))))

(defun c:special-value (sym) `(value ,(c:special-sym sym)))

(defun c:special-def (sym) `(definition ,(c:special-sym sym)))

(defun c:push-literal (x)
   (push `(,x . ,nliterals) literals)
   (prog1 (c:literal nliterals) (incr nliterals 1)))

(defun c:set-value (sym val)
   (lets ((x (c:local-sym sym)))
    (cond (x (c:assign (cdr x) val))
          (t (c:assign (c:special-value sym) val)))))

(comment "shitauke")

(defun c:progn (body loc jmp)
   (cond ((atom body) (c:symbol nil loc jmp))
         (t (do ((l body (cdr l)) (codes))
                ((atom (cdr l))
		 (c:gen-form (car l) loc jmp)
		 (c:make-begin codes))
                (c:gen-form (car l) nil nil)))))

(defun c:form (form loc jmp)
   (cond ((and (null loc) (null jmp) (c:nosidep form)) nil) 
         ((atom form) (and (or loc jmp) (c:atom form loc jmp)))
	 ((macrop (car form))
	  (c:form (macroexpand (car form) (cdr form)) loc jmp))
	 ((symbolp (car form)) (c:apsy (car form) (cdr form) loc jmp))
	 ((consp (car form)) (c:apcons (car form) (cdr form) loc jmp))
	 (t (c:error "illegal function call" form))))

(defun c:atom (x loc jmp)
   (cond ((numberp x) (c:number x loc jmp))
         ((stringp x) (c:string x loc jmp))
         ((symbolp x) (c:symbol x loc jmp))
	 ((vectorp x) (c:vector x loc jmp))
         (t (c:warn "cannot compile un-rereadable atom" x)
            (c:final (c:special-sym x) loc jmp))))

(defmacro c:others (x loc jmp) `(c:final (c:push-literal ,x) ,loc ,jmp))

(defun c:number (num loc jmp)
   (cond ((fixp num) (c:final `(itofix ,num) loc jmp))
	 (t (c:others num loc jmp))))

(putd 'c:string (getd 'c:others))

(putd 'c:cons (getd 'c:others))

(putd 'c:vector (getd 'c:others))

(defun c:symbol (sym loc jmp)
   (lets ((x (c:in-cvar sym)) (p))
    (cond (x (c:final x loc jmp))
	  ((c:in-root sym) (c:final sym loc jmp))
	  (t (setq x (c:local-sym sym))
	     (cond ((null x)
		    (cond ((not (c:specialp sym))
			   (c:warn "assumed special" sym)
			   (c:declare sym 'special)))
		    (c:final (c:special-value sym) loc jmp))
		   (t (c:final (cdr x) loc jmp)))))))

(defun c:atom-literal (x)
   (cond ((fixp x) `(itofix ,x))
	 ((symbolp x) (cond ((c:in-root x) x) (t (c:special-sym x))))
	 (t (c:push-literal x))))

(defun c:apcons (fn args loc jmp)
   (match fn
    (('lambda formals . body) (c:aplambda formals args body loc jmp))
    (t (c:error "illegal function" fn))))

(defun c:apsy (sym args loc jmp)
   (cond ((eq current-fn sym)
	  (cond ((and (eq loc return-value) (null special-vars))
		 (c:tail-recursion args))
                (t (c:call-cfn sym args loc jmp))))
         ((and (get sym 'inline) (not (c:redefinedp sym)))
          (lets ((current-subr sym)) (funcall (get sym 'inline) args loc jmp)))
         ((and (predefinedp sym) (not (c:redefinedp sym)))
          (c:call-predef sym args loc jmp))
         ((and compfile-optimize (assq sym compiling-funcs))
          (c:call-cfn sym args loc jmp))
         (t (c:call-sy sym args loc jmp))))

(defun c:tail-recursion (args)
   (lets ((len (length args)) (sp sp) (spsave sp) (codes))
    (and (not (<= minarg len maxarg))
         (c:argerr args current-fn))
    (c:gen-evalargs-push args)
    (do ((n 1 (/1+ n)) (ssp (1+ spsave) (1+ ssp)))
        ((< len n))
        (c:gen `(= ,(c:var (/- n len)) ,(c:var ssp))))
    (cond ((= maxarg len) (c:gen `(goto ,ftop-label)))
          (t (c:gen `(= na ,len) `(goto ,fentry-label))))
    (c:make-begin codes)))

(defun c:call-predef (sym args loc jmp)
   (lets ((codes) (sp sp) (max (maxarg (getd sym))))
    (and (= -1 max) (setq max 32768))
    (or (<= (minarg (getd sym)) (length args) max)
	(c:error "mismatched number of args" `(,sym . ,args)))
    (c:gen-evalargs-push args)
    (c:gen-final `(predef-call ,sym ,(length args) (dis ,sp)) loc jmp)
    (c:make-begin codes)))

(defun c:call-cfn (sym args loc jmp)
   (lets ((a) (sp sp) (codes))
    (c:gen-evalargs-push args)
    (cond ((eq sym current-fn)
           (c:gen
	     (c:final `(cfunc-call ,sym ,(length args) (dis ,sp)) loc jmp)))
	  (t (setq a (c:get-cvar))
	     (c:gen `(= ,a ,(c:special-sym sym))
		    `(pushcb (definition ,a))
		    `(= ,a (cfunc-call ,sym ,(length args) (dis ,sp)))
		    '(discardcb)
	            (c:final a loc jmp))))
    (c:make-begin codes)))

(defun c:call-sy (sym args loc jmp)
   (lets ((a (c:get-cvar)) (sp sp) (codes))
    (or (definedp sym) (c:warn "assumed non-macro" sym))
    (c:gen-evalargs-push args)
    (c:gen-final
      `(funcall ,(c:special-sym sym) ,(length args) (dis ,sp)) loc jmp)
    (c:make-begin codes)))

(defun c:aplambda (formals actuals body loc jmp)
   (lets ((local-vars local-vars) (special-vars special-vars)
	  (special-save special-vars)
	  (sp sp) (ssp sp) (ssave sp) (var) (n 0) (codes))
    (c:gen-evalargs-push actuals)
    (do ((lf formals (cdr lf)) (la actuals (cdr la))
	 (ssp (1+ ssave) (1+ ssp)) (var))
	((or (atom lf) (atom la))
	 (or (atom la) (c:error "too many actual parameters" actuals))
	 (do ((lf lf (cdr lf)) (a (c:get-cvar)))
	     ((atom lf)
	      (cond ((eq special-vars special-save) (c:gen-progn body loc jmp))
		    (t (c:gen (c:progn body a nil)
			      (c:undo-special special-save)
			      (c:final a loc jmp))))
	      (c:make-begin codes))
	     (setq var (car lf))
	     (and (atom var) (c:error "too few actual parameters" actuals))
	     (c:gen-progn (cdr var) a nil)
	     (c:check-var (setq var (car var)))
	     (cond ((c:specialp var) (c:gen (c:bind-special var a)))
		   (t (c:push-local var (incr sp 1))
		      (c:gen-assign sp a)))))
	(and (consp (setq var (car lf))) (setq var (car var)))
	(c:check-var var)
	(cond ((c:specialp var) (c:gen (c:bind-special var ssp)))
	      (t (c:push-local var ssp))))))

(defun c:funcall (fn args loc jmp)
   (lets ((sp sp) (codes))
    (c:gen-evalargs-push args)
    (c:gen-final `(funcall ,(c:value fn) ,(length args) (dis ,sp)) loc jmp)
    (c:make-begin codes)))

(defun c:gen-evalargs-push (args)
   (do ((l args (cdr l)))
       ((atom l))
       (c:gen-form (car l) (1+ sp) nil)
       (incr sp 1)))

(defun c:eval-twoargs (x1 x2)
   (cond ((c:easyp x2) 
	  (setq r1 (c:get-cvar) r2 (c:get-cvar) nsp sp)
          `(begin ,(c:form x1 r1 nil) ,(c:form x2 r2 nil)))
         ((c:exchangep x1 x2)
	  (setq r1 (c:get-cvar) r2 (c:get-cvar) nsp sp)
	  `(begin ,(c:form x2 r2 nil) ,(c:form x1 r1 nil)))
	 (t (lets ((sp sp))
	     (setq r1 (1+ sp) r2 (c:get-cvar) nsp (1+ sp))
	     `(begin ,(c:form x1 r1 nil)
		     ,(prog2 (incr sp 1) (c:form x2 r2 nil)))))))

(defun c:final (val loc jmp)
   (lets ((loc (c:value loc)) (val (c:value val)))
    (cond ((null jmp) (c:assign loc val))
	  ((or (null val) (c:true-val val)) (c:final-const val loc jmp)) 
	  (t (setq val (c:assign loc val))
	     (match jmp
	       ((nil . fl) `(if (== ,val nil) (goto ,fl)))
	       ((tl . nil) `(if (!= ,val nil) (goto ,tl)))
	       ((tl . fl) `(if (== ,val nil) (goto ,fl) (goto ,tl))))))))

(defun c:final-const (val loc jmp)
   (cond ((null loc) (c:final-const-simple val jmp))
	 (t (c:final-const-block val loc jmp))))

(defun c:final-const-simple (val jmp)
   (cond ((null val) jmp (and (cdr jmp) `(goto ,(cdr jmp))))
	 (t (and jmp (car jmp) `(goto ,(car jmp))))))

(defun c:final-const-block (val loc jmp)
   `(begin ,(c:assign loc val) ,(c:final-const-simple val jmp)))

(defun c:assign (loc val)
   (cond (loc `(= ,(c:value loc) ,(c:value val))) (t val)))

(defun c:true-val (x)
   (cond ((symbolp x) (eq x t))
	 ((atom x) t)
	 (t (memq (car x) '(itofix lit)))))

(defun c:make-block (f c)
   (cond ((and (null f) (= (length c) 1)) (car c))
	 ((null f) `(begin . ,(nreverse c)))
	 (t `(block ,f . ,(nreverse c)))))

(defun c:make-begin (c) (c:make-block nil c))

(defun c:redefinedp (sym) (get sym 'redefine))

(comment "location and value manipulation")

(defun c:value (x) (cond ((fixp x) (c:var x)) (t x)))

(defun c:var (n) (cond ((<= n 0) `(ag ,(/- n))) (t `(l ,(/1- n)))))

(defun c:literal (n) `(lit ,n))

(defun c:stack (n) `(l ,(- nsp n)))

(defmacro alias (f1 f2) `(putprop ',f1 (get ',f2 'inline) 'inline))

(comment "type check")

(defun c:check-fix (x a (b tcheck-var))
   (and typecheck (not (c:fix-result x)) `(checkfix ,b ,a)))

(defun c:check-cons (x a (b tcheck-var))
   (and typecheck (not (c:cons-result x)) `(checkcons ,b ,a)))

(defun c:check-sym (x a (b tcheck-var))
   (and typecheck (not (c:symbol-result x)) `(checksym ,b ,a)))

(defun c:check-str (x a (b tcheck-var))
   (and typecheck
	(cond ((c:string-result x) nil)
	      ((c:symbol-result x) `(= ,b (prname ,a)))
	      (t `(checkstr ,b ,a)))))

(defun c:check-vec (x a (b tcheck-var))
   (and typecheck (not (c:vector-result x)) `(checkvec ,b ,a)))

(defun c:check-posfix (x a) (c:check-fix x a))

(defun c:fix-result (x)
   (or (fixp x)
       (and (consp x)
	    (symbolp (setq x (car x)))
	    (cond ((predefinedp x) (eq (get x 'c:subr-result) 'FIXNUM))
		  (t (get x 'fix-result))))))

(defun c:cons-result (x)
   (and (consp x)
        (symbolp (car x))
	(cond ((eq (car x) 'quote) (consp (second x)))
	      ((predefinedp (setq x (car x)))
	       (eq (get x 'subr-result) 'CONS))
	      (t (get x 'cons-result)))))

(defun c:symbol-result (x)
   (and (consp x)
        (symbolp (car x))
	(cond ((eq (car x) 'quote) (symbolp (second x)))
	      ((predefinedp (setq x (car x)))
	       (eq (get x 'subr-result) 'SYMBOL))
	      (t (get x 'symbol-result)))))

(defun c:string-result (x)
   (and (consp x)
        (symbolp (car x))
	(cond ((eq (car x) 'quote) (stringp (second x)))
	      ((predefinedp (setq x (car x)))
	       (eq (get x 'subr-result) 'STRING))
	      (t (get x 'string-result)))))

(defun c:vector-result (x)
   (and (consp x)
        (symbolp (car x))
	(cond ((eq (car x) 'quote) (vectorp (second x)))
	      ((predefinedp (setq x (car x)))
	       (eq (get x 'subr-result) 'VECTOR))
	      (t (get x 'vector-result)))))

(comment "subr no zokusei nado")

(defun c:constantp (x)
   (cond ((symbolp x) (memq x '(nil t)))
	 ((atom x) t)
	 (t (memq (car x) '(quote function)))))

(defun c:constant-value (x) (cond ((atom x) x) (t (second x))))

(defun c:nosidep (form)
   (or (atom form)
       (memq (car form) '(function quote))
       (and (symbolp (car form))
            (get (car form) 'noside)
            (not (get (car form) 'redefine))
            (every (cdr form) (function c:nosidep)))))

(mapc '(atom eq neq null not symbolp consp numberp fixp floatp stringp 
        vectorp referencep streamp codep equal and or or progn prog1 prog2 
        and cr car cdr caar cadr cdar cddr caaar caadr cadar caddr 
        cdaar cdadr cddar cdddr caaaar caaadr caadar caaddr cadaar cadadr 
        caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr 
        first second third fourth fifth sixth seventh /+ /- * // \ /1+ /1- < 
        <= => > <$ <=$ =>$ >$ greaterp lessp /0< /0= /0> /0<$ /0=$ /0>$ plusp 
        zerop minusp logand logor logxor assq assoc memq member get getd vref 
        sref deref cons xcons ncons list append reverse subst last length nth 
        nthcdr)
   (function (lambda (x) (putprop x t 'noside))))

(defun c:easyp (x)
   (or (atom x)
       (eq (car x) 'quote)
       (eq (car x) 'function)
       (and (symbolp (car x))
            (get (car x) 'easy)
            (not (get (car x) 'redefine))
            (every (cdr x) (function c:easyp)))))

(mapc '(atom eq neq null not symbolp consp numberp fixp floatp stringp 
        vectorp referencep streamp codep equal and or progn prog1 prog2 
        loop exit cr car cdr caar cadr cdar cddr caaar caadr cadar caddr 
        cdaar cdadr cddar cdddr caaaar caaadr caadar caaddr cadaar cadadr 
        caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr 
        first second third fourth fifth sixth seventh /+ /- * // \ /1+ /1- < 
        <= => > <$ <=$ =>$ >$ greaterp lessp /0< /0= /0> /0<$ /0=$ /0>$ plusp 
        zerop minusp logand logor logxor assq assoc memq member get getd vref 
        sref deref last length nth nthcdr)
   (function (lambda (x) (putprop x t 'easy))))

(defun c:exchangep (x1 x2)
   (and (c:easyp x1) (c:nosidep x1) (or (c:constantp x1) (c:nosidep x2))))

(defun macrop (fn)
   (cond ((symbolp fn)
	  (and (definedp fn) (consp (getd fn)) (eq 'macro (car (getd fn)))))
         (t (eq 'macro (car fn)))))

(defun macroexpand (m args)
   (and (symbolp m) (setq m (getd m)))
   (funcall (cdr m) args))

(defun c:constfn (fn) (and (consp fn) (eq (car fn) 'function)))

(defun c:easyfn (fn)
   "kore wa inchiki mo hanahadashii"
   (match fn
    (('function ('lambda formals . body)) nil)
    (('function sym) (and (not (get sym 'redefine)) (get sym 'easy)))
    (t nil)))

(comment "warning and error")

(defun c:warn (msg x)
   (format "warning: /c --- /s (in /s)/n" msg x current-fn))

(defun c:error (msg x)
   (format "error: /c --- /s (in /s)/n" msg x current-fn)
   (funcall break))

(defun c:argerr (x (fn current-subr))
   (format "error: mismatched number of args, /s --- /s (in /s)"
          x fn current-fn)
   (funcall break))
  
(defun c:typerr (x (fn current-subr))
   (format "error: illegal argument type, /s --- /s (in /s)"
          x fn current-fn)
   (funcall break))

(comment "iroiro")

(defmacro c:post-incr (n) `(prog1 ,n (setq ,n (1+ ,n))))
