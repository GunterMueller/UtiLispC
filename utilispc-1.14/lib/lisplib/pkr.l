 (comment : "Prolog/KR Interpreter  (by H. Nakashima)")
 (defun p:version nil (format "Version /c-/c/n" "C" "16.4"))
 (comment : "ASSERT(A/Z) copies outer definition -- Vers. 12.0" 
  "EQ is fixed -- Vers. 12.1" "READ revised -- Vers. 12.2" 
  "ASSERT(A/Z) knows (DEFINE p NIL) -- Vers. 12.4" 
  "TERPRI may take one argument (stream) -- Vers. 12.8" 
  "P:FETCHVALUE does not remove ' -- Vers. 12.9" 
  "HELP installed -- Vers. 12.9")
 (declare
    (:variable-prefix :trace :free :database :daemon :numdata :last-result)
    special)
 (defun prolog nil (p:init) (p:version) (pgo))
 (defun p:init nil
    (setq :prolog-world '(standard-world))
    (setq :definition-stack nil)
    (setq :printlevel 7)
    (setq :variable-prefix `(,(character "*"),(character ">"),(character "<")))
    (setq :trace nil)
    (setq :traceall nil)
    (setq :step nil)
    (setq :steplevel 99999)
    (setq :selectstep nil)
    (setq :debug1 nil)
    (setq :free nil)
    (setq :database (vector 1000))
    (setq :daemon (vector 1000))
    (do ((i 0 (add1 i))) ((eq i 1000)) (push i :free))
    (setq :numdata nil)
    (p:initcode)
    (pr:init 256)
    (setq :toplevel (ncons toplevel)))
 (defun p:error (mes (at ""))
    (r:push-cue (ncons `(error ,mes ,at)) :old-subst))
 (declare
    (:debug :debug1 :step :selectstep :steplevel :traceall :tracelevel 
     :printlevel)
    special)
 (defun pgo nil (push-toplevel 'prolog-toplevel) (toplevel))
 (declare (:toplevel) special)
 (defun push-toplevel (top) (push toplevel :toplevel) (setq toplevel top))
 (defun pop-toplevel nil
    (cond ((null :toplevel) nil) (t (setq toplevel (pop :toplevel)))))
 (defun prolog-toplevel nil
    (lets ((attention-handler
              (function (lambda nil (rind (cond (:debug1 "D:") (t ":"))))))
           (err:read
              (function
               (lambda (stream (x nil))
                (format "ILLEGAL OBJECT READ : /c/n" (readline stream))
                (throw ':prologloop nil)))))
     (catch ':top
        (let ((input (catch ':prologloop (rind (cond (:debug1 "D:") (t ":")))))
              (:backtrace nil))
         (cond ((null input))
               ((symbolp input) (p:os (string-append input (readline))))
               (t (p:execute input)
                  (and (eq standard-input terminal-input)
                       (grind (p:fetchvalue
                                 (car :last-result)
                                 (cdr :last-result)
                                 :printlevel)))))))))
 (declare (:command) special)
 (defun p:os (:command)
    (catch ':command
       (let ((attention-handler (function (lambda nil (throw ':command nil)))))
        (call :command))))
 (declare (:undolist :definition-stack :item) special)
 (defun p:execute (:item)
    (let ((:debug (or :traceall :trace :step :debug1)))
     (catch ':prologloop
        (lets ((attention-handler (function p:attention)) 
               (break (function p:break)) ($subst (p:newsubst)) 
               (:undolist nil) (result (refute :item $subst)))
         (setq :last-result `(,result . ,$subst))))
     (putprop 'last-input (ncons (ncons (ncons :item))) (car :prolog-world))))
 (defun p:assert (clause)
    (putprop (caar clause)
       (assert:pos (r:getdef (car clause)) `(,(cdar clause) . ,(cdr clause)))
       (car :prolog-world))
    (p:push-world (car :prolog-world) (caar clause)))
 (defun assert:pos (olddef newdef)
    (cond ((null olddef) (ncons newdef))
          ((null (car olddef)) `(,newdef . ,olddef))
          ((and (assert:unit newdef) (not (assert:unit (car olddef))))
           `(,newdef . ,olddef))
          ((and (assert:unit (car olddef)) (not (assert:unit newdef)))
           `(,(car olddef) . ,(assert:pos (cdr olddef) newdef)))
          ((assert:specificp (car newdef) (caar olddef)) `(,newdef . ,olddef))
          (t `(,(car olddef) . ,(assert:pos (cdr olddef) newdef)))))
 (defun assert:specificp (p1 p2)
    (cond ((p:varp p1) nil)
          ((p:varp p2) t)
          ((atom p1) (not (atom p2)))
          ((atom p2) nil)
          (t (do ((x (pop p1) (pop p1)) (y (pop p2) (pop p2)))
                 (nil)
                 (cond ((assert:specificp x y) (exit t))
                       ((and (atom y) (or (listp x) (p:varp x))) (exit nil))
                       ((p:varp p1) (exit))
                       ((p:varp p2) (exit t))
                       ((atom p1) (exit (not (atom p2))))
                       ((atom p2) (exit)))))))
 (defun assert:unit (def) (null (cdr def)))
 (defun p:assertz (clause)
    (putprop (caar clause)
       (assert:last `(,(cdar clause) . ,(cdr clause)) (r:getdef (car clause)))
       (car :prolog-world))
    (p:push-world (car :prolog-world) (caar clause)))
 (defun p:asserta (clause)
    (putprop (caar clause)
       `((,(cdar clause) . ,(cdr clause)) . ,(r:getdef (car clause)))
       (car :prolog-world))
    (p:push-world (car :prolog-world) (caar clause)))
 (defun *assert (x indent close)
    (prin1 (pop x))
    (princ " ")
    (*prind (pop x) (cursor) 0)
    (cond ((printable x (/- (colleft) close 2))
           (princ "  ")
           (*block x (/+ indent 5) close))
          (t (tab (/+ indent 5)) (*block x (/+ indent 5) close))))
 (putprop 'assert (function *assert) 'prind)
 (putprop 'assertz (function *assert) 'prind)
 (putprop 'asserta (function *assert) 'prind)
 (defun p:attention nil
    (refute '(attention) :old-subst :level :cue :stack)
    (throw 'refute t))
 (defun p:break nil
    (let ((standard-input terminal-input)
	  (standard-output terminal-output) (:step t)) (p:step)))
 (defun p:push-world (world pred)
    (let ((defs (get world ':world)))
     (cond ((memq pred defs)) (t (putprop world `(,pred . ,defs) ':world)))))
 (defun assert:last (clause defs)
    (cond ((null defs) (ncons clause))
          ((null (car defs)) `(,clause . ,defs))
          (t `(,(car defs) . ,(assert:last clause (cdr defs))))))
 (declare (:fetched-value :fetched-subst :prolog-world) special)
 (defun unify (x xsubst y ysubst)
    (and (p:varp y)
         (p:assigned y ysubst)
         (setq y (p:fetch (cadr :fetched-value) (cddr :fetched-value)))
         (setq ysubst :fetched-subst))
    (cond ((or (eq x '?) (eq y '?)))
          ((listp x)
           (cond ((and (symbolp (car x)) (eq (sref (car x) 0) (character "!")))
                  (!:refute (substring (car x) 1) (cdr x) xsubst y ysubst))
                 ((and (eq (car x) 'quote) (eq (length x) 2))
                  (cond ((atom y) (equal (second x) y))
                        ((and (eq (car y) 'quote) (eq (length y) 2))
                         (equal (second x) (second y)))
                        (t (equal (second x) y))))
                 ((p:varp y) (p:link y ysubst x xsubst))
                 ((atom y) nil)
                 ((and (eq (car y) 'quote) (eq (length y) 2))
                  (equal x (second y)))
                 ((and (symbolp (car y)) (eq (sref (car y) 0) (character "!")))
                  (!:refute (substring (car y) 1) (cdr y) ysubst x xsubst))
                 ((unify (car x) xsubst (car y) ysubst)
                  (unify (cdr x) xsubst (cdr y) ysubst))))
          ((listp y)
           (cond ((and (symbolp (car y)) (eq (sref (car y) 0) (character "!")))
                  (!:refute (substring (car y) 1) (cdr y) ysubst x xsubst))
                 ((and (eq (car y) 'quote) (eq (length y) 2))
                  (equal x (second y)))
                 ((p:varp x)
                  (cond ((p:assigned x xsubst)
                         (unify (cadr :fetched-value)
                                (cddr :fetched-value)
                                y
                                ysubst))
                        (t (p:link x xsubst y ysubst))))))
          ((p:varp x)
           (cond ((p:assigned x xsubst)
                  (unify (cadr :fetched-value) (cddr :fetched-value) y ysubst))
                 (t (p:link x xsubst y ysubst))))
          ((p:varp y) (p:link y ysubst x xsubst))
          (t (equal x y))))
 (defun s:unify (formal fsubst actual asubst)
    (and (p:varp actual)
         (p:assigned actual asubst)
         (setq actual (p:fetch (cadr :fetched-value) (cddr :fetched-value)))
         (setq asubst :fetched-subst))
    (cond ((eq formal '?))
          ((listp actual)
           (cond ((and (symbolp (car actual))
                       (eq (sref (car actual) 0) (character "!")))
                  (r:lambda-bind
                     asubst
                     (let ((var (intern (string (car :variable-prefix)))))
                      (r:lambda-link var asubst (gensym var) asubst)
                      (and (refute (cdr actual) asubst (add1 :level))
                           (s:unify formal fsubst var asubst)))))
                 ((and (symbolp formal) (eq (sref formal 0) (character "*")))
                  (p:link formal fsubst actual asubst))
                 ((atom formal) nil)
                 ((s:unify (car formal) fsubst (car actual) asubst)
                  (s:unify (cdr formal) fsubst (cdr actual) asubst))))
          ((and (symbolp formal) (eq (sref formal 0) (character "*")))
           (p:link formal fsubst actual asubst))
          ((p:varp actual) (p:link actual asubst formal fsubst))
          (t (equal formal actual))))
 (macro p:varp (x)
    `(and (symbolp ,(car x)) (memq (sref ,(car x) 0) :variable-prefix)))
 (macro p:newsubst (sb)
    (cond (sb `(ncons ,(car sb))) (t '(ncons (gensym "_")))))
 (macro p:assigned (x)
    `(setq :fetched-value (assq ,(first x) (cdr ,(second x)))))
 (defun p:link (x xsubst y ysubst)
    (or (and (eq xsubst ysubst) (eq x y))
        (push `(,(rplacd xsubst `((,x ,y . ,ysubst) . ,(cdr xsubst))) . 
                ,(second xsubst))
              :undolist)))
 (defun p:fetch (x s)
    (cond ((p:assigned x s)
           (p:fetch (cadr :fetched-value) (cddr :fetched-value)))
          (t (setq :fetched-subst s) x)))
 (defun p:undo (up)
    (loop (and (eq :undolist up) (exit))
          (delq (cdar :undolist) (caar :undolist))
          (pop :undolist)))
 (defun p:fetchvalue (x ($subst nil) (fetch-level 0))
    (cond ((p:varp x)
           (cond ($subst (cond ((p:assigned x $subst)
                                (p:fetchvalue
                                   (cadr :fetched-value)
                                   (cddr :fetched-value)
                                   fetch-level))
                               (t (intern (string-append x (car $subst))))))
                 (t x)))
          ((atom x) x)
          ((zerop (sub1 fetch-level)) '?)
          (t `(,(p:fetchvalue (car x) $subst (sub1 fetch-level)) . 
               ,(p:fetchvalue (cdr x) $subst (sub1 fetch-level))))))
 (defun p:trace:r (result $form $subst $level)
    (prin1 '=)
    (prin1 :level)
    (grind (p:fetchvalue (and result $form) $subst :printlevel)))
 (defun p:trace ($form $subst $level)
    (prin1 $level)
    (grind (p:fetchvalue $form $subst :printlevel)))
 (declare (:backtrace) special)
 (defun p:step nil
    (cond ((and (or :step (memq (car :form) :selectstep))
                (>= :steplevel :level))
           (setq :steplevel 1000)
           (p:trace :form :old-subst :level)
           (do ((com (rind "S:") (rind "S:")))
               (nil)
               (cond ((listp com)
                      (lets ((:selectstep nil) (:step nil))
                       (p:execute com)
                       (grind (p:fetchvalue
                                 (car :last-result)
                                 (cdr :last-result)
                                 :printlevel))))
                     (t (selectq com
                           (c (exit))
                           (f (exit (setq :step nil)))
                           (g (exit (setq :steplevel :level)))
                           (bt (p:backtrace :printlevel))
                           (backtrace (p:backtrace 99))
                           ((q z)
                            (setq :last-result (ncons nil))
                            (throw ':prologloop nil))
                           (pp (grind (p:fetchvalue :form :old-subst)))
                           (s (setq :step nil)
                              (push (rind "FUNCTION:") :selectstep))
                           (all (exit (setq :step t)))
                           (level (setq :printlevel (rind "LEVEL:")))
                           (x (lets ((:selectstep nil) (:step nil))
                               (p:execute (rind "Form:"))
                               (grind (p:fetchvalue (car :last-result) 
                                       (cdr :last-result) :printlevel))))
                           ((? help)
                            (format "Available commands are:/n")
                            (princ "All, ")
                            (princ "BackTrace, ")
                            (princ "Continue, ")
                            (princ "Finish, ")
                            (princ "Go, ")
                            (princ "Level, ")
                            (princ "PrettyPrint, ")
                            (princ "Quit, ")
                            (princ "eXecute, ")
                            (princ "Z, ")
                            (format "/nand normal predicate calls./n"))
                           (t (format "Undefined command : /s/nTry HELP/n" 
                               com)))))))))
 (defun r:succeed nil
    (cond ((null :cue) (throw 'refute :goal))
          (t (let ((cue (pop :cue)))
              (setq :clause (vref cue 0))
              (setq :old-subst (vref cue 1))
              (setq :level (vref cue 2))
              (setq :form (pop :clause))
              (setq :definitions (r:getdef :form))
              (setq :undo-point :undolist)
              (setq :new-subst (p:newsubst))
              (setq :father (vref cue 3))))))
 (defun r:push-cue ($clause $subst ($level :level) ($stack :stack))
    (let ((cue (vector 4)))
     (vset cue 0 $clause)
     (vset cue 1 $subst)
     (vset cue 2 $level)
     (vset cue 3 :father)
     (push cue :cue)))
 (defun r:fail nil
    (cond (:stack (r:pop) (p:undo :undo-point))
          (t (p:undo :final-undo-point) (throw 'refute nil))))
 (comment
    (defun r:push ($form $definitions ($subst :old-subst))
       (setq :stack `(,$form 
                      ,$definitions 
                      ,$subst 
                      ,:level 
                      ,:cue 
                      ,:undo-point 
                      ,:final-undo-point . 
                      ,:stack))))
 (defun r:push ($form $definitions ($subst :old-subst))
    (let ((v (vector 8)))
     (vset v 0 $form)
     (vset v 1 $definitions)
     (vset v 2 $subst)
     (vset v 3 :level)
     (vset v 4 :cue)
     (vset v 5 :undo-point)
     (vset v 6 :final-undo-point)
     (vset v 7 :father)
     (push v :stack)))
 (comment
    (defun r:pop nil
       (setq :form (pop :stack))
       (setq :clause nil)
       (setq :definitions (pop :stack))
       (setq :old-subst (pop :stack))
       (setq :level (pop :stack))
       (setq :cue (pop :stack))
       (setq :undo-point (pop :stack))
       (setq :final-undo-point (pop :stack))))
 (defun r:pop nil
    (let ((v (pop :stack)))
     (setq :form (vref v 0))
     (setq :clause nil)
     (setq :definitions (vref v 1))
     (setq :old-subst (vref v 2))
     (setq :level (vref v 3))
     (setq :cue (vref v 4))
     (setq :undo-point (vref v 5))
     (setq :final-undo-point (vref v 6))
     (setq :father (vref v 7))))
 (defun pop:check (cgoal)
    (do ((d (r:getdef :form)))
        ((null d) (r:push :form :definitions))
        (and (memq cgoal (pop d)) (exit))))
 (defun p:backtrace (level)
    (grind (p:fetchvalue
              (mapcar :backtrace
                 (function (lambda (x) (p:fetchvalue (car x) (cdr x) level))))
              (p:newsubst "")
              (add1 level))))
 (defun r:getdef ($form)
    (and (listp $form) (r:get-pred-value (car $form) :prolog-world)))
 (defun r:clear nil (setq :cue nil :stack nil) t)
 (defun r:pause nil
    (push `((,:level ,:form) . ,:old-subst) :backtrace)
    (p:step)
    (and (or :traceall (memq (car :form) :trace))
         (p:trace :form :old-subst :level)))
 (defun r:pause2 (result)
    (or result
        (push `(,:level fail ,(car (p:fetchvalue :form :old-subst 2)))
              :backtrace))
    (and (or :traceall
             (memq (car :form) :trace)
             (and (or :step (memq (car :form) :selectstep))
                  (>= :steplevel :level)))
         (p:trace:r result :form :old-subst :level)))
 (declare
    (:form :old-subst :level :cue :stack :father :goal :clause :definitions 
     :undo-point :new-subst :final-undo-point)
    special)
 (defun refute (:form (:old-subst (p:newsubst)) (:level 0) (:cue nil) 
                (:stack nil))
    (lets ((:new-subst (p:newsubst)) (:goal :form) (:clause nil) 
           (:definitions (r:getdef :form)) (:undo-point :undolist) 
           (:final-undo-point :undolist) (:father `(,:stack . ,:cue)))
     (catch 'refute (loop (refute:one)))))
 (defun refute:one nil
    (and :clause (r:push-cue :clause :old-subst))
    (loop (cond ((listp :form)
                 (and (p:varp (car :form))
                      (setq :form `(,(p:fetchvalue
                                        (car :form)
                                        `("" . ,(cdr :old-subst))) . 
                                    ,(cdr :form)))
                      (setq :definitions (r:getdef :form)))
                 (exit))
                ((p:varp :form)
                 (setq :form (p:fetch :form :old-subst))
                 (or (listp :form) (exit (p:error "ILLEGAL FORM" :form)))
                 (setq :old-subst :fetched-subst
                       :definitions (r:getdef :form)))
                (t (exit (p:error "ILLEGAL FORM" :form)))))
    (and :debug (r:pause))
    (cond ((null :definitions)
           (cond ((p:system (car :form) (cdr :form) :old-subst) (r:succeed))
                 (t (and :debug (r:pause2 nil)) (r:fail))))
          (t (setq :father `(,:stack . ,:cue))
             (loop (cond ((null :definitions)
                          (and :debug (r:pause2 nil))
                          (exit (r:fail) nil))
                         ((null (car :definitions)) (exit (r:fail) nil))
                         ((unify (caar :definitions)
                                 :new-subst
                                 (cdr :form)
                                 :old-subst)
                          (cond ((cdr :definitions)
                                 (r:push :form (cdr :definitions)))
                                (:debug (r:push `(fail: ,:form) nil)))
                          (exit (and :debug (r:pause2 t))
                                (cond ((setq :clause (cdr (car :definitions)))
                                       (setq :form (pop :clause))
                                       (setq :level (add1 :level))
                                       (setq :undo-point :undolist)
                                       (setq :old-subst :new-subst)
                                       (setq :new-subst (p:newsubst))
                                       (setq :definitions (r:getdef :form))
                                       t)
                                      ((r:succeed)))))
                         (t (p:undo :undo-point) (pop :definitions)))))))
 (defun refute:n (:clause (:old-subst (p:newsubst)) (:level :level) (:cue nil) 
                  (:stack nil))
    (lets ((:form (pop :clause)) (:goal :form) (:definitions (r:getdef :form)) 
           (:undo-point :undolist) (:new-subst (p:newsubst)) 
           (:final-undo-point :undolist))
     (catch 'refute (loop (refute:one)))))
 (declare (:name :args :subst) special)
 (defun p:system (:name :args $subst)
    (let ((code (r:get-pred-value :name ':code)))
     (cond (code (let ((:subst (p:newsubst "")))
                  (cond ((s:unify (pop code) :subst :args $subst)
                         (funcall code))
                        (t (p:error
                              "ILLEGAL FORM"
                              (p:fetchvalue `(,:name . ,:args) $subst))))))
           ((listp :name) (comment lambda form) (r:lambda :name :args $subst))
           ((definedp :name)
            (p:lispfunction
               :name
               :args
               $subst
               (r:get-pred-value :name ':lisp-predicate)
               (and (listp (getd :name)) (eq (car (getd :name)) 'macro))))
           ((and (p:varp :name) (p:assigned :name $subst))
            (setq :form `(,(p:fetch
                              (cadr :fetched-value)
                              (cddr :fetched-value)) . 
                          ,:args))
            (refute:one))
           (t (p:error "UNDEFINED PREDICATE" :name)))))
 (defun r:lambda (lambda-form $args $subst)
    (r:lambda-bind
       $subst
       (refute (third lambda-form)
               (r:lambda-link (second lambda-form) $subst $args $subst)
               (add1 :level))))
 (defun s:convert (x $subst eliminate-last?)
    (cond ((p:varp x)
           (cond ((p:assigned x $subst)
                  (s:convert
                     (cadr :fetched-value)
                     (cddr :fetched-value)
                     eliminate-last?))
                 (t x)))
          ((atom x) x)
          ((and eliminate-last? (null (cdr x))) nil)
          ((and (symbolp (car x)) (eq (sref (car x) 0) (character "!")))
           (r:lambda-bind
              $subst
              (lets ((var (intern (string (car :variable-prefix)))))
               (r:lambda-link var $subst (gensym var) $subst)
               (and (refute (cdr x) $subst) (s:convert var $subst nil)))))
          (t `(,(s:convert (car x) $subst nil) . 
               ,(s:convert (cdr x) $subst eliminate-last?)))))
 (declare (:bind) special)
 (macro r:lambda-bind (x)
    `(let ((:bind nil)) (prog1 ,(second x) (delq :bind ,(first x)))))
 (defun r:lambda-link (var var-subst args a-subst)
    (prog1 (rplacd var-subst
              `(,(setq :bind `(,var ,args . ,a-subst)) . ,(cdr var-subst)))
           (or (eq (string-length var) 1) (setq :bind nil))))
 (defun !:refute (name body bsubst oppornent osubst)
    (r:lambda-bind
       bsubst
       (refute body
               (r:lambda-link
                  (intern (string-append (string (car :variable-prefix)) name))
                  bsubst
                  oppornent
                  osubst)
               (add1 :level))))
 (defun r:get-pred-value (pred ind)
    (cond ((symbolp pred)
           (loop (cond ((atom ind) (exit (get pred ind)))
                       ((get pred (car ind)) (exit (get pred (car ind)))))
                 (pop ind)))
          ((listp pred)
           (cond ((and (symbolp (car pred))
                       (eq (sref (car pred) 0) (character "!")))
                  (!:refute
                     (substring (car pred) 1)
                     (cdr pred)
                     :old-subst
                     '*:
                     :new-subst)
                  (r:get-pred-value (p:fetchvalue '*: :new-subst) ind))
                 ((eq (car pred) 'lambda) nil)
                 (t (p:error "Illegal Predicate" pred) (r:succeed) nil)))
          (t (p:error "ILLEGAL PREDICATE" pred) (r:succeed) nil)))
 (defun p:lispfunction ($name $args $subst do-not-need-value? macro?)
    (let ((args (s:convert $args $subst (not do-not-need-value?))))
     (cond (macro? (cond (do-not-need-value? (eval `(,$name . ,args)))
                         (t (unify (eval `(,$name . ,args))
                                   (ncons "")
                                   (cond ($args (car (last $args))) (t nil))
                                   $subst))))
           (do-not-need-value? (apply $name args))
           (t (unify (apply $name args)
                     (ncons "")
                     (cond ($args (car (last $args))))
                     $subst)))))
 (comment
    (defun r:pop-cue nil
       (cond ((null :cue) nil)
             (t (let ((cue (pop :cue)))
                 (setq :clause (vref cue 0))
                 (setq :old-subst (vref cue 1))
                 (setq :level (vref cue 2))
                 (setq :form (pop :clause))
                 (setq :definitions (r:getdef :form))
                 (setq :undo-point :undolist)
                 (setq :new-subst (p:newsubst))
                 (setq :stack (vref cue 3))
                 (setq :father (vref cue 4)))
                t))))
 (declare (:args :subst :level loaded :file :def-list) special)
 (macro p:define (cp)
    (cond ((symbolp (car cp))
           `(push `(,',(car cp) . 
                    ,`(,',(second cp) . 
                       ,(function (lambda nil (and . ,(cddr cp))))))
                  :def-list))
          ((listp (car cp))
           `(progn . 
             ,(mapcar (car cp)
                 (function (lambda (x) `(p:define ,x . ,(cdr cp)))))))))
 (defun p:initcode nil
    (let ((:def-list nil))
     (init:def)
     (init:control)
     (init:io)
     (init:db)
     (init:error)
     (init:misc)
     (mapc :def-list (function (lambda (x) (putprop (car x) (cdr x) ':code))))
     (mapc '(atom bset call close comment defun defcs delcs exfile fixp floatp 
             format free greaterp inopen listp lessp listp memq minusp numberp 
             oddp outopen plusp princ referencep setref skipline string-lessp 
             sset streamp string-equal string-lessp stringp symbolp tyo 
             vectorp vset zerop /0= /0=$ /0> /0>$ /0< /0<$ > >$ < <$ >= >=$ <= 
             <=$ <> <>$ # #$ =$)
        (function (lambda (x) (putprop x t ':lisp-predicate))))
     (mapc '(ec:b ec:bi ec:bo ec:c ec:d ec:e ec:f ec:fn ec:i ec:ib ec:in ec:it 
             ec:k ec:l ec:last ec:level ec:li ec:n ec:o ec:p ec:pop ec:pp ec:q 
             ec:r ec:ra ec:ri ec:s ec:sc ec:st ec:stack ec:top ec:u ec:v 
             ec:var ec:x ec:z e:move e:defcom)
        (function (lambda (x) (putprop x t ':lisp-predicate))))))
 (defun init:def nil
    (comment ********** definitions)
    (p:define
       (assert as)
       (*pred . *body)
       (p:assert (p:fetchvalue :args `("" . ,(cdr :old-subst))))
       t)
    (p:define
       (asserta aa)
       (*pred . *body)
       (p:asserta (p:fetchvalue :args `("" . ,(cdr :old-subst))))
       t)
    (p:define
       (assertz az)
       (*pred . *body)
       (p:assertz (p:fetchvalue :args `("" . ,(cdr :old-subst))))
       t)
    (p:define
       define
       (*name . *def)
       (prog1 t
              (putprop (p:fetchvalue '*name :subst)
                 (p:fetchvalue (cdr :args) `("" . ,(cdr :old-subst)))
                 (car :prolog-world))
              (p:push-world (car :prolog-world) (p:fetchvalue '*name :subst))))
    (p:define
       definition
       (*name *def)
       (unify (get (p:fetchvalue '*name :subst) (car :prolog-world))
              (ncons "")
              '*def
              :subst))
    (p:define (listing lis) *args (p:listing (p:fetchvalue '*args :subst)))
    (p:define
       retract
       (*name)
       (p:retract (p:fetch '*name :subst) :fetched-subst))
    (p:define
       set
       (*name . *value)
       (putprop (p:fetchvalue '*name :subst)
          (ncons (ncons (p:fetchvalue (cdr :args) `("" . ,(cdr :old-subst)))))
          (car :prolog-world))))
 (defun init:db nil
    (comment ********** internal data base)
    (p:define
       erase
       (*pattern)
       (loop (or (p:erase (first :args) :old-subst) (exit nil))
             (and (p:continue) (exit (r:clear) t))))
    (p:define
       record
       (*pattern)
       (let ((loc (p:record (p:fetchvalue '*pattern :subst))))
        (cond (loc (cond ((p:continue)) (t (rec:erasel loc) nil))) (t nil)))
       (r:clear))
    (p:define
       recorded
       (*pattern)
       (p:recorded (first :args) :old-subst :cue)
       (r:clear))
    (p:define
       recording
       (*pattern *pred)
       (lets ((daemonbody (p:fetchvalue '*pred :subst)) 
              (loc (p:record (p:fetchvalue '*pattern :subst) t daemonbody)))
        (cond (loc (vset :daemon loc daemonbody))))))
 (defun init:io nil
    (comment ********** inputs and outputs)
    (p:define
       add
       (*file . *names)
       (let ((fn (intern (p:fetchvalue '*file :subst))) 
             (x (p:fetchvalue '*names :subst)))
        (print (putprop fn (union x (get fn 'loaded)) 'loaded))))
    (p:define
       close
       (*file)
       (let ((file (p:fetchvalue '*file :subst)))
        (cond ((streamp file) (close file))
              (t (p:error "ILLEGAL ARGUMENT TO CLOSE" file)))))
    (p:define
       del
       (*file . *names)
       (let ((fn (intern (p:fetchvalue '*file :subst))) 
             (names (p:fetchvalue '*names :subst)))
        (print (putprop fn
                  (let ((loaded (get fn 'loaded)))
                   (mapc names
                      (function (lambda (x) (setq loaded (delq x loaded)))))
                   loaded)
                  'loaded))))
    (p:define
       dump
       (*file *names)
       (p:dump (p:fetchvalue (p:fetch '*file :subst) :fetched-subst)
               (p:fetchvalue (p:fetch '*names :subst) :fetched-subst)))
    (p:define grind (*x) (prog1 t (grind (p:fetchvalue '*x :subst))))
    (p:define
       inopen
       (*file *stream)
       (let ((file (p:fetchvalue '*file :subst)))
        (unify (cond ((streamp file) (inopen file))
                     ((or (symbolp file) (stringp file))
                      (inopen (stream (alloc file))))
                     (t (p:error "ILLEGAL ARGUMENT TO INOPEN" file)))
               :subst
               '*stream
               :subst)))
    (p:define load (*file) (p:load (p:fetchvalue '*file :subst)))
    (p:define
       load-world
       (*file *world)
       (let ((:prolog-world
                `(,(p:fetchvalue '*world :subst) . ,:prolog-world)))
        (p:load (p:fetchvalue '*file :subst))))
    (p:define
       loaded
       (*file *loaded)
       (unify '*loaded
              :subst
              (get (intern (p:fetchvalue '*file :subst)) 'loaded)
              :subst))
    (p:define
       new-file
       (*name)
       (stringp (p:alloc-new-file (p:fetchvalue '*name :subst))))
    (p:define
       outopen
       (*file *stream)
       (let ((file (p:fetchvalue '*file :subst)))
        (unify (cond ((streamp file) (outopen file))
                     ((or (symbolp file) (stringp file))
                      (outopen (stream (alloc file))))
                     (t (p:error "ILLEGAL ARGUMENT TO INOPEN" file)))
               :subst
               '*stream
               :subst)))
    (p:define
       print
       (*x . *y)
       (lets ((args (p:fetchvalue '*y :subst)) (a2 (and args (first args))) 
              (a3 (and (eq (length args) 2) (second args))) 
              (standard-output
                 (cond ((streamp a2) a2)
                       ((streamp a3) a3)
                       (t (p:standard-output))))
              ($printlevel
                 (cond ((numberp a2) a2) ((numberp a3) a3) (t :printlevel))))
        (or (and (null a2) (null a3))
            (streamp a2)
            (streamp a3)
            (numberp a2)
            (numberp a3)
            (p:error "ILLEGAL ARGUMENT TO PRINT" `(,a2 ,a3)))
        (grind (p:fetchvalue '*x :subst $printlevel))
        t))
    (p:define
       prin1
       (*x . *y)
       (lets ((args (p:fetchvalue '*y :subst)) (a2 (and args (first args))) 
              (a3 (and (eq (length args) 2) (second args))) 
              (standard-output
                 (cond ((streamp a2) a2)
                       ((streamp a3) a3)
                       (t (p:standard-output))))
              ($printlevel
                 (cond ((numberp a2) a2) ((numberp a3) a3) (t :printlevel))))
        (or (and (null a2) (null a3))
            (streamp a2)
            (streamp a3)
            (numberp a2)
            (numberp a3)
            (p:error "ILLEGAL ARGUMENT TO PRIN1" `(,a2 ,a3)))
        (prin1 (p:fetchvalue '*x :subst $printlevel))
        t))
    (p:define
       princ
       (*x . *y)
       (lets ((args (p:fetchvalue '*y :subst)) (a2 (and args (first args))) 
              (a3 (and (eq (length args) 2) (second args))) 
              (standard-output
                 (cond ((streamp a2) a2)
                       ((streamp a3) a3)
                       (t (p:standard-output))))
              ($printlevel
                 (cond ((numberp a2) a2) ((numberp a3) a3) (t :printlevel))))
        (or (and (null a2) (null a3))
            (streamp a2)
            (streamp a3)
            (numberp a2)
            (numberp a3)
            (p:error "ILLEGAL ARGUMENT TO PRINC" `(,a2 ,a3)))
        (princ (p:fetchvalue '*x :subst $printlevel))
        t))
    (p:define
       print-level
       (*level)
       (or (unify (p:fetch '*level :subst)
                  :fetched-subst
                  :printlevel
                  :fetched-subst)
           (let ((l (p:fetchvalue '*level :subst)))
            (cond ((numberp l) (setq :printlevel l))
                  (t (p:error "ILLEGAL ARGUMENT TO PRINT-LEVEL" l))))))
    (p:define prompt ("-:"))
    (p:define
       read
       (*x . *stream)
       (catch ':read
          (unify '*x
                 :subst
                 (let ((prompt (cond ((get 'prompt (car :prolog-world))
                                      (caaar (get 'prompt 
                                              (car :prolog-world))))
                                     (t (caar (get 'prompt ':code)))))
                       (err:end-of-file
                          (function (lambda (s) (throw ':read nil))))
                       (standard-input
                          (let ((s (p:fetchvalue '*stream :subst)))
                           (cond (s (first s)) (t (p:standard-input))))))
                  (rind prompt))
                 (p:newsubst ""))))
    (p:define
       standard-input
       (*stream)
       (let ((stream (p:fetchvalue '*stream :subst)))
        (cond ((eq stream 'terminal-input)
               (setq standard-input terminal-input))
              ((streamp stream) (setq standard-input stream))
              ((p:varp stream) (unify '*stream :subst standard-input :subst))
              (t (p:error "ILLEGAL ARGUMENT TO STANDARD-INPUT" stream)))))
    (p:define
       standard-output
       (*stream)
       (let ((stream (p:fetchvalue '*stream :subst)))
        (cond ((eq stream 'terminal-output)
               (setq standard-output terminal-output))
              ((streamp stream) (setq standard-output stream))
              ((p:varp stream) (unify '*stream :subst standard-output :subst))
              (t (p:error "ILLEGAL ARGUMENT TO STANDARD-OUTPUT" stream)))))
    (p:define
       store
       (*file)
       (p:store (p:fetchvalue (p:fetch '*file :subst) :fetched-subst)))
    (p:define
       tab
       (*tab . *r)
       (lets ((n (p:fetchvalue '*tab :subst)) (rest (p:fetchvalue '*r :subst)) 
              (a2 (and rest (first rest))) 
              (standard-output
                 (cond ((streamp a2) a2) (t (p:standard-output)))))
        (or (null a2) (streamp a2) (p:error "ILLEGAL ARGUMENT TO TAB" rest))
        (tab n)
        t))
    (p:define
       terminal-input
       (*stream)
       (unify '*stream :subst terminal-input :subst))
    (p:define
       terminal-output
       (*stream)
       (unify '*stream :subst terminal-output :subst))
    (p:define
       terpri
       *x
       (lets ((args (p:fetchvalue '*x :subst)) (a2 (and args (first args))) 
              (standard-output
                 (cond ((streamp a2) a2) (t (p:standard-output)))))
        (or (null a2) (streamp a2) (p:error "ILLEGAL ARGUMENT TO TERPRI" args))
        (terpri)
        t)))
 (defun init:control nil
    (comment ********** controls)
    (p:define
       accumulate
       (? ? ?)
       (p:accumulate (first :args) (second :args) (third :args) :old-subst))
    (p:define
       and
       *args
       (let ((args (p:fetch '*args :subst)))
        (and args (r:push-cue args :fetched-subst))
        t))
    (p:define candidates ? (p:candidates (car :args) (cdr :args) :old-subst))
    (p:define
       catch
       (*pred)
       (r:push (list ':catch '*pred) nil :subst)
       (r:push-cue (ncons (p:fetch '*pred :subst)) :fetched-subst))
    (p:define :catch (*any) nil)
    (p:define
       (co-begin par)
       *args
       (p:par (p:fetch '*args :subst) :fetched-subst (add1 :level))
       (r:clear))
    (p:define
       cond
       *args
       (lets ((a (p:fetch '*args :subst)) ($subst :fetched-subst))
        (do ((w))
            ((null a))
            (cond ((atom (setq w (pop a)))
                   (p:error "ILLEGAL ARGUMENT TO COND" w))
                  ((refute (car w) $subst :level)
                   (exit (cond ((cdr w) (r:push-cue (cdr w) $subst)) (t))))))))
    (p:define
       create
       (*form *name)
       (unify '*name
              :subst
              (p:create (p:fetch '*form :subst) :fetched-subst)
              :subst))
    (p:define cut nil (prog1 t (setq :stack (car :father))))
    (p:define
       dand
       ?
       (do ((a :args (cdr a)))
           ((null a) (exit t))
           (cond ((refute (car a) :old-subst :level)) (t (exit nil)))))
    (p:define do ? (p:do :args :old-subst))
    (p:define
       dor
       *args
       (do ((a :args (cdr a)))
           ((null a))
           (and (refute (car a) :old-subst :level nil) (exit t))))
    (p:define fail nil (p:fail))
    (p:define fail: (*form) nil)
    (p:define false nil nil)
    (p:define
       for-all
       (*pred . *body)
       (p:for-all
          (p:fetch '*pred :subst)
          (p:fetch '*body :subst)
          :fetched-subst))
    (p:define
       if
       (*if *then . *else)
       (cond ((refute (p:fetch '*if :subst) :fetched-subst :level nil)
              (r:push-cue (ncons (p:fetch '*then :subst)) :fetched-subst))
             ((p:fetch '*else :subst)
              (r:push-cue (p:fetch '*else :subst) :fetched-subst))
             (t)))
    (p:define
       initiate
       (*form *name)
       (unify '*name
              :subst
              (p:initiate (p:fetch '*form :subst) :fetched-subst)
              :subst))
    (p:define loop ? (catch ':loop (p:loop :args :old-subst)))
    (p:define
       exit
       nil
       (let ((err:catch
                (function (lambda (x) (p:error "NOT IN LOOP" '(exit))))))
        (throw ':loop t)))
    (p:define
       next
       (*name . *args)
       (p:next (p:fetchvalue '*name :subst)
               (p:fetch '*args :subst)
               :fetched-subst))
    (p:define
       not
       ?
       (not (refute:n
               (p:fetch :args :old-subst)
               :fetched-subst
               (add1 :level))))
    (comment
       (p:define
          not
          (*pred)
          (let ((:level (add1 :level)))
           (r:push '(true) nil)
           (r:push-cue `(,(p:fetch '*pred :subst) (fail)) :fetched-subst))))
    (p:define
       onbt
       (*pred)
       (let ((:father `(,:stack . ,:cue)))
        (r:push `(and ,(p:fetch '*pred :subst) (fail)) nil :fetched-subst)))
    (p:define
       or
       *args
       (lets (($args (p:fetch '*args :subst)) ($subst :fetched-subst))
        (cond ($args (and (cdr $args) (r:push `(or . ,(cdr $args)) nil))
                     (r:push-cue (ncons (first $args)) $subst)
                     t)
              (t nil))))
    (p:define
       por
       *args
       (p:por (p:fetch '*args :subst) :fetched-subst (add1 :level))
       (r:clear))
    (p:define
       receive
       (*pred)
       (p:receive (p:fetch '*pred :subst) :fetched-subst))
    (p:define return *args (p:return (p:fetch '*args :subst) :fetched-subst))
    (p:define
       send
       (*name . *args)
       (p:send (p:fetchvalue '*name :subst)
               (p:fetch '*args :subst)
               :fetched-subst))
    (p:define throw (*pattern) (p:throw '*pattern :subst))
    (p:define true nil)
    (p:define wait (*pred) (p:wait (p:fetch '*pred :subst) :fetched-subst)))
 (defun init:misc nil
    (comment ********** miscellaneous)
    (p:define
       atom
       (*x)
       (let ((v (p:fetchvalue '*x :subst))) (and (atom v) (not (p:varp v)))))
    (p:define
       case
       (*case)
       (let ((case (p:fetchvalue '*case :subst)))
        (cond ((eq case 'lower) (setq use-lower t))
              ((eq case 'upper) (setq use-lower nil) t)
              (t (unify '*case
                        :subst
                        (cond (use-lower 'lower) (t 'upper))
                        :subst)))))
    (p:define
       create-world
       (*world . *defs)
       (p:world (p:fetchvalue '*world :subst) (p:fetchvalue '*defs :subst)))
    
    (p:define
       peek-world
       (*name *list)
       (unify (get (p:fetchvalue '*name :subst) ':world) :subst '*list :subst))
    (p:define
       world-name
       (*name)
       (unify '*name :subst (car :prolog-world) :subst))
    (p:define
       eq
       (*x *y)
       (lets ((x (p:fetch (p:fetch '*x :subst) :fetched-subst)) 
              (xs :fetched-subst) 
              (y (p:fetch (p:fetch '*y :subst) :fetched-subst)) 
              (ys :fetched-subst))
        (cond ((p:varp x) (and (eq x y) (eq xs ys))) (t (eq x y)))))
    (p:define erase-world (*name) (p:erase-world (p:fetchvalue '*name :subst)))
    (p:define (quit end epilog) nil (print 'epilog) (quit))
    (p:define
       format
       (*format . *args)
       (prog1 t
              (eval `(format 
                      ,(p:fetchvalue '*format :subst) . 
                      ,(mapcar (p:fetchvalue '*args :subst)
                          (function (lambda (x) `',x)))))))
    (p:define
       gt
       (*x *y)
       (greaterp (p:fetchvalue '*x :subst) (p:fetchvalue '*y :subst)))
    (p:define
       lt
       (*x *y)
       (lessp (p:fetchvalue '*x :subst) (p:fetchvalue '*y :subst)))
    (p:define
       last-result
       (*r)
       (unify '*r :subst (car :last-result) (cdr :last-result)))
    (p:define
       linesize
       (*l)
       (cond ((unify '*l :subst (linesize) :subst))
             (t (let ((l (p:fetchvalue '*l :subst)))
                 (cond ((numberp l) (linesize l))
                       (t (p:error "ILLEGAL ARGUMENT TO LINESIZE" l)))))))
    (p:define lisp nil (pop-toplevel) (toplevel))
    (p:define
       (match =)
       (? ?)
       (unify (first :args) :old-subst (second :args) :old-subst))
    (p:define
       pred
       (*x *y)
       (unify '*y :subst (sub1 (p:fetchvalue '*x :subst)) (ncons nil)))
    (p:define
       select
       (? . ?)
       (do ((pat (first :args))
            (select (cdr :args) (cdr select))
            (u :undolist))
           ((null select) nil)
           (cond ((unify pat :old-subst (caar select) :old-subst)
                  (r:push-cue (cdar select) :old-subst)
                  (exit t))
                 (t (p:undo u)))))
    (p:define
       succ
       (*x *y)
       (unify '*y :subst (add1 (p:fetchvalue '*x :subst)) (ncons nil)))
    
    (p:define
       time
       (*pred *time)
       (lets ((tm (time)) 
              (rs (refute (p:fetch '*pred :subst) :fetched-subst :level nil)))
        (prog1 (unify '*time :subst (/- (time) tm) (ncons nil))
               (and (eq (car :goal) 'time)
                    (setq :goal `(time ,rs ,(p:fetch '*time :subst)))))))
    (p:define
       prefix
       *args
       (setq :variable-prefix (mapcar (p:fetchvalue '*args :subst)
                                 'character)))
    (p:define
       var
       (*x)
       (let ((x (p:fetch '*x :subst)))
        (and (p:varp x)
             (cond ((p:assigned x :fetched-subst)
                    (and (p:varp (cadr :fetched-value))
                         (not (p:assigned
                                 (cadr :fetched-value)
                                 (cddr :fetched-value)))))
                   (t)))))
    (p:define version nil (p:version))
    (p:define
       with
       (*def . *pred)
       (p:with (p:fetchvalue '*def :subst)
               (p:fetch '*pred :subst)
               :fetched-subst))
    (p:define x *args (setq :goal (eval (p:fetchvalue '*args :subst)))))
 (declare (:prologhelp) special)
 (defun init:error nil
    (p:define
       (attention break)
       nil
       (let ((standard-output terminal-output) (:step t)) (p:step)))
    (p:define backtrace (*bt) (unify '*bt :subst :backtrace :subst))
    (p:define
       debug
       (*flag)
       (selectq (p:fetchvalue '*flag :subst)
          (on (setq :debug1 t))
          (off (setq :debug1 nil) t)
          (t (unify '*flag :subst (cond (:debug1 'on) (t 'off)) :subst))))
    (p:assert '((error *mes *at) (standard-error-handler *mes *at)))
    (p:define prolog nil (pgo))
    (p:define standard-error-handler (*statement *at) (p:break))
    (p:define
       step
       (*pred . *select)
       (let ((:debug t) (:debug1 t) (:traceall nil) (:trace nil) 
             (:selectstep nil) (:steplevel 1000))
        (cond ((p:fetch '*select :subst)
               (setq :step nil)
               (setq :selectstep (p:fetch '*select :subst))))
        (let ((pred (p:fetch '*pred :subst)))
         (selectq pred
            (on (setq :step t))
            (off (setq :step nil))
            (t (let ((:step t)) (refute pred :fetched-subst)))))))
    (p:define top nil (throw ':top t))
    (p:define
       toplevel
       (*name)
       (p:toplevel (p:fetch '*name :subst) :fetched-subst))
    (p:define
       trace
       *args
       (setq :trace (append (p:fetchvalue '*args :subst) :trace)))
    (p:define trace-all nil (setq :traceall t))
    (p:define
       untrace
       *args
       (prog1 t
              (mapc (p:fetchvalue '*args :subst)
                 (function (lambda (x) (setq :trace (delq x :trace)))))))
    (p:define
       untrace-all
       nil
       (prog1 t
              (setq :traceall nil)
              (setq :trace nil)
              (setq :tracelevel nil))))
 (defprop member
    (((*element (*element . *rest)))
     ((*element (*top . *rest)) (member *element *rest)))
    standard-world)
 (defun p:standard-input nil
    (let ((si (r:getdef '(standard-input))))
     (cond (si (caaar si)) (t standard-input))))
 (defun p:standard-output nil
    (let ((so (r:getdef '(standard-output))))
     (cond (so (caaar so)) (t standard-output))))
 (defun p:continue nil
    (cond ((null :cue) t)
          (t (let ((cue (car :cue)))
              (refute:n (vref cue 0) (vref cue 1) (vref cue 2) (cdr :cue))))))
 (declare
    (edit-name edit-command editor-chain editor-stack editor-variable-prefix 
     find editor-get-definition editor-restore-definition editor-execute 
     loaded edit-file)
    special)
 (defun p:record (data (newdaemon nil) (daemonbody nil))
    (or (listp data) (p:error "ILLEGAL ARGUMENT TO RECORD" data))
    (let ((ind (car data)) (pat (cdr data)) (loc nil) (subst (p:newsubst)))
     (cond ((not (symbolp ind)) (p:error "ILLEGAL ARGUMENT TO RECORD" data))
           ((setq loc (p:recorded data subst nil t))
            (let ((daemon (vref :daemon loc)))
             (cond (daemon (cond (newdaemon nil)
                                 (t (vset :daemon loc nil)
                                    (refute daemon subst)
                                    loc)))
                   (newdaemon (refute daemonbody subst) nil))))
           (t (let ((loc (rec:alloc data)) (l (get ind (length pat))))
               (cond ((null l) (putprop ind (ncons loc) (length pat)))
                     (t (nconc l (ncons loc))))
               (p:record2 ind pat 1 loc)
               loc)))))
 (defun p:record2 (ind pat pos loc)
    (cond ((null pat) loc)
          ((symbolp (car pat))
           (let ((l (get (car pat) ind)) (l1 nil))
            (cond ((null l) (putprop (car pat) (ncons `(,pos ,loc)) ind))
                  ((null (setq l1 (assq pos l)))
                   (nconc l (ncons `(,pos ,loc))))
                  (t (rec:merge loc l1))))
           (p:record2 ind (cdr pat) (add1 pos) loc))
          ((numberp (car pat))
           (cond (:numdata
                    (let ((l (assq (car pat) :numdata)) (l1 nil))
                     (cond ((null l)
                            (nconc :numdata
                                   (ncons `(,(car pat) (,ind (,pos ,loc))))))
                           ((null (setq l1 (assq ind l)))
                            (nconc l (ncons `(,ind (,pos ,loc)))))
                           ((null (setq l (assq pos l1)))
                            (nconc l1 (ncons `(,pos ,loc))))
                           (t (rec:merge loc l)))))
                 (t (setq :numdata (ncons `(,(car pat) (,ind (,pos ,loc)))))))
           (p:record2 ind (cdr pat) (add1 pos) loc))
          (t (p:record2 ind (cdr pat) (add1 pos) loc))))
 (declare (:pattern :patvalue :newdaemon) special)
 (defun p:recorded (:pattern (:subst (ncons "")) (:cue nil) (:newdaemon nil))
    (let ((:patvalue (p:fetchvalue :pattern :subst)))
     (and (listp :patvalue) (catch 'recorded (rec:search)))))
 (defun rec:search nil
    (lets ((ind (car :patvalue)) (pat (cdr :patvalue)) 
           (n (do ((w pat (cdr w)) (n 0))
                  ((null w) n)
                  (and (or (numberp (car w)) (symbolp (car w)))
                       (not (p:varp (car w)))
                       (setq n (add1 n)))))
           (v (vector n)))
     (or (and (symbolp ind) (not (p:varp ind)))
         (p:error "ILLEGAL PATTERN" :patvalue))
     (cond ((zerop n)
            (throw 'recorded
               (do ((:u :undolist) (cand (get ind (length pat)) (cdr cand)))
                   ((null cand))
                   (and (or :newdaemon (null (vref :daemon (car cand))))
                        (unify :pattern
                               :subst
                               (vref :database (car cand))
                               (ncons ""))
                        (p:continue)
                        (exit (car cand)))
                   (p:undo :u)))))
     (do ((i 0) (pos 1 (add1 pos)) (x nil))
         ((>= i n))
         (setq x (pop pat))
         (cond ((p:varp x))
               ((or (numberp x) (symbolp x))
                (vset v
                      i
                      (let ((p (assq pos
                                  (cond ((symbolp x) (get x ind))
                                        ((numberp x) 
                                         (assq ind (assq x :numdata)))))))
                       (cond (p (cdr p)) ((throw 'recorded nil)))))
                (setq i (add1 i)))))
     (do ((i 0) (n1 (sub1 n)) (x nil) (y nil))
         (nil)
         (do ((:u :undolist) (cand (vref v 0)))
             ((< i n1) (vset v 0 cand))
             (cond ((null cand) (throw 'recorded nil))
                   ((and (or :newdaemon (null (vref :daemon (car cand))))
                         (unify :pattern
                                :subst
                                (vref :database (car cand))
                                (ncons ""))
                         (p:continue))
                    (throw 'recorded (car cand)))
                   (t (p:undo :u) (pop cand) (setq i 0))))
         (setq x (vref v i))
         (setq y (vref v (add1 i)))
         (and (null x) (throw 'recorded nil))
         (loop (cond ((null y) (throw 'recorded nil))
                     ((greaterp (car x) (car y)) (pop y))
                     ((exit (vset v (add1 i) y)))))
         (loop (cond ((null x) (throw 'recorded nil))
                     ((lessp (car x) (car y)) (pop x))
                     ((exit (vset v i x)))))
         (cond ((eq (car x) (car y)) (setq i (add1 i)))
               ((zerop i))
               (t (setq i (sub1 i)) (setq y x) (setq x (vref v i)))))))
 (defun rec:alloc (pat)
    (cond ((null :free) (p:error "NO MORE DATA AREA"))
          (t (vset :database (car :free) pat) (pop :free))))
 (defun rec:merge (a l)
    (cond ((null (cdr l)) (nconc l (ncons a)))
          ((eq a (second l)))
          ((lessp a (second l)) (rplacd l `(,a . ,(cdr l))))
          (t (rec:merge a (cdr l)))))
 (defun p:erase (pat $subst)
    (let ((loc (p:recorded pat $subst)))
     (cond ((null loc) nil) (t (rec:erasel loc) t))))
 (defun rec:erasel (loc)
    (lets ((:pattern (vref :database loc)) (ind (car :pattern)) 
           (pat (cdr :pattern)) (n (length pat)) (pos 1))
     (push loc :free)
     (vset :daemon loc nil)
     (putprop ind (delq loc (get ind n)) n)
     (mapc pat
        (function
         (lambda (pat)
          (delq loc
             (assq pos
                (cond ((symbolp pat) (get pat ind))
                      ((numberp pat) (assq ind (assq pat :numdata))))))
          (setq pos (add1 pos)))))))
 (defun p:with (defs body $subst (defsptr :definition-stack))
    (prog1 (let ((:prolog-world
                    (cond ((atom defs) `(,defs . ,:prolog-world))
                          (t :prolog-world))))
            (mapc defs
               (function
                (lambda (d)
                 (cond ((and (listp d) (symbolp (car d)))
                        (push `(,(car d) . ,(r:getdef d)) :definition-stack)
                        (putprop (car d) (cdr d) (car :prolog-world)))
                       (t (p:error "ILLEGAL ARGUMENT TO WITH" d))))))
            (refute:n body $subst :level))
           (p:reset :definition-stack defsptr)))
 (defun p:reset (defs until)
    (do ((x defs (cdr x)))
        ((eq x until))
        (putprop (caar x) (cdar x) (car :prolog-world))))
 (declare (for-pred do-pred) special)
 (defun p:for-all (for-pred do-pred :subst) (catch ':for-all (for-all:loop)))
 (defun for-all:loop nil
    (do ((:form for-pred)
         (:clause nil)
         (:definitions (r:getdef for-pred))
         (:old-subst :subst)
         (:new-subst (p:newsubst))
         (:undo-point :undolist)
         (:final-undo-point :undolist)
         (:stack nil)
         (:cue nil))
        ((not (catch 'refute
                 (and (catch 'refute (loop (refute:one)))
                      (cond ((refute:n do-pred :subst :level) (r:fail) t)
                            (t (throw ':for-all nil))))))
         t)))
 (comment
    (defun p:fail (level)
       (do ((flevel (/+ :level
                        (cond (level (p:fetchvalue (first level) :subst))
                              (t 1))
                        -1))
            (:level 8388607))
           ((> flevel :level) (r:push :form :definitions) nil)
           (cond (:stack (r:pop)) (t (exit nil))))))
 (defun p:fail nil (setq :stack (car :father)) nil)
 (defun p:return ($form $subst)
    (prog1 t
           (setq :stack (car :father))
           (setq :cue (cdr :father))
           (cond (:cue (setq :father (vref (car :cue) 3)))
                 (t (setq :father `(nil))))
           (and $form (r:push-cue $form $subst))))
 (declare (:result) special)
 (declare (:variable) special)
 (defun p:candidates (:variable :clause :subst (:form (pop :clause)))
    (cond ((p:varp :variable)
           (do ((:definitions (r:getdef :form))
                (:old-subst :subst)
                (:new-subst (p:newsubst))
                (:undo-point :undolist)
                (:final-undo-point :undolist)
                (:stack nil)
                (:cue nil)
                (:result nil))
               ((not (catch 'refute
                        (and (catch 'refute (loop (refute:one)))
                             (progn (push (p:fetchvalue :variable :subst)
                                          :result)
                                    (r:fail)
                                    t))))
                (unify :variable :subst (nreverse :result) :subst)
                t)))
          (t (p:error "ILLEGAL ARGUMENT TO CANDIDATES" :variable))))
 (declare (:structure) special)
 (defun p:accumulate (:structure :form variable :subst)
    (do ((:clause nil)
         (:definitions (r:getdef :form))
         (:old-subst :subst)
         (:new-subst (p:newsubst))
         (:undo-point :undolist)
         (:final-undo-point :undolist)
         (:stack nil)
         (:cue nil)
         (:result nil))
        ((not (catch 'refute
                 (and (catch 'refute (loop (refute:one)))
                      (progn (push (p:fetchvalue :structure :subst) :result)
                             (r:fail)
                             t))))
         (unify variable :subst (nreverse :result) :subst)
         t)))
 (defun union (x y)
    (cond ((null x) y)
          ((null y) x)
          ((member (car x) y) (union (cdr x) y))
          (t `(,(car x) . ,(union (cdr x) y)))))
 (defun p:toplevel (name subst)
    (let ((pred (p:fetchvalue name subst)))
     (cond ((p:varp pred)
            (unify name
                   subst
                   (cond ((atom toplevel) toplevel)
                         ((eq (car toplevel) 'lambda)
                          (second (second (third toplevel))))
                         (t nil))
                   subst))
           ((listp pred) (setq toplevel `(lambda nil (p:execute ',pred))))
           ((eq pred 'reset) (setq toplevel 'prolog-toplevel) (toplevel))
           (t (format "TRY ""(TOPLEVEL RESET)"" TO RESET./n")))))
 (defun p:throw ($form $subst)
    (do (($stack :stack))
        ((and (listp :form)
              (eq (car :form) ':catch)
              (unify $form $subst (second :form) :old-subst))
         t)
        (cond (:stack (r:pop))
              (t (setq :stack $stack)
                 (exit (p:error
                          "NO CATCHING STRUCTURE"
                          (p:fetchvalue $form $subst)))))))
 (defun p:loop (body $subst)
    (let ((u :undolist)) (loop (refute:n body $subst :level) (p:undo u))))
 (defun p:do (body $subst)
    (do ((:caluse nil nil)
         (:cue nil nil)
         (:stack nil nil)
         (:form (car body) (car body))
         (:definitions (r:getdef (car body)) (r:getdef (car body)))
         (:undo-point :undolist :undolist)
         (:new-subst (p:newsubst) (p:newsubst))
         (:final-undo-point :undolist :undolist))
        (nil)
        (catch 'refute (loop (refute:one)))
        (pop body)
        (cond ((null body) (exit t)) ((atom body) (exit t)))))
 (defun p:load (:file)
    (lets ((standard-input (inopen (stream (alloc :file)))) (loaded nil) 
           (err:end-of-file
              (function
               (lambda (s)
                (throw 'load
                   (putprop (intern :file) (nreverse loaded) 'loaded)))))
           (attention-handler
              (function
               (lambda nil
                (throw 'load
                   (putprop (intern :file) (nreverse loaded) 'loaded)))))
           (err:read
              (function
               (lambda (stream (x nil))
                (format "ILLEGAL OBJECT READ : /c/n" (readline stream))
                (throw 'load nil)))))
     (catch 'load
        (do ((x (read) (read)) (:backtrace nil nil))
            (nil)
            (p:execute x)
            (cond ((eq (car x) 'define) (push (second x) loaded))
                  ((memq (car x) '(assert as assertz az asserta))
                   (setq loaded (union (ncons (car (second x))) loaded)))
                  (t (push x loaded)))))))
 (defun p:dump (filename names)
    (prog (flist (standard-output standard-output) def file)
       (setq flist (cond ((symbolp names) (get names ':world))
                         ((listp names) names)
                         (t nil)))
       (cond (flist (putprop (intern filename) flist 'loaded))
             (t (p:error "NOTHING TO BE DUMPED" names) (return t)))
       (setq standard-output (cond ((stringp (setq file (alloc filename)))
                                    (format "FILE ALREADY EXISTS./n")
                                    (cond ((memq 
                                            (rind "WANNA OVER WRITE? (Y/N)") 
                                            '(y yes))
                                           (setq file (outopen (stream file))))
                                          (t (format "USE STORE INSTEAD./n") 
                                           (return nil))))
                                   (t (setq file 
                                       (outopen 
                                        (stream (p:alloc-new-file filename)) 
                                        255 2560)))))
       (cond ((atom flist)
              (p:error "ILLEGAL ARGUMENT TO DUMP" flist)
              (return t))
             (t (let ((:prolog-world
                         (cond ((symbolp names) `(,names . ,:prolog-world))
                               (t :prolog-world))))
                 (mapc flist
                    (function
                     (lambda (name)
                      (cond ((symbolp name)
                             (cond ((setq def (get name (car :prolog-world)))
                                    (grind `(define ,name . ,def)))
                                   (t (let ((standard-output terminal-output)) 
                                       (princ "UNDEFINED PREDICATE :") 
                                       (grind name) (terpri)))))
                            (t (grind name)))))))))
       (close file)
       (return t)))
 (defun p:store (filename)
    (prog (flist (standard-output standard-output) def file)
       (setq flist (get (intern filename) 'loaded))
       (cond (flist) (t (p:error "NOT LOADED YET" filename) (return t)))
       (setq standard-output (cond ((stringp (setq file (alloc filename)))
                                    (setq file (outopen (stream file))))
                                   (t (setq file 
                                       (outopen 
                                        (stream (p:alloc-new-file filename)) 
                                        255 2560)))))
       (cond ((atom flist)
              (p:error "ILLEGAL ARGUMENT TO DUMP" flist)
              (return t))
             (t (mapc flist
                   (function
                    (lambda (name)
                     (cond ((symbolp name)
                            (cond ((setq def (get name (car :prolog-world)))
                                   (grind `(define ,name . ,def)))
                                  (t (let ((standard-output terminal-output)) 
                                      (princ "UNDEFINED PREDICATE :") 
                                      (grind name) (terpri)))))
                           (t (grind name))))))))
       (close file)
       (return t)))
 (defun p:next (:name :args :subst)
    (catch ':next
       (p:next1
          :name
          :args
          (get :name ':context)
          (get :name ':count)
          (get :name ':original-subst))))
 (defun p:next1 (name args context count original-subst)
    (cond (context
             (lets ((:form (pop context)) (:clause (pop context)) 
                    (:definitions (pop context)) (:old-subst (pop context)) 
                    (:new-subst (pop context)) (:undolist (pop context)) 
                    (:undo-point nil) (:final-undo-point nil) 
                    (:stack (pop context)) (:cue (pop context)))
              (or (zerop count)
                  (catch 'refute (prog1 t (r:fail)))
                  (throw ':next nil))
              (cond ((catch 'refute (loop (refute:one))))
                    (t (throw ':next nil)))
              (prog1 (unify:send
                        args
                        :subst
                        (p:fetchvalue (get name 'args) original-subst)
                        original-subst)
                     (putprop name
                        `(,:form
                          ,:clause
                          ,:definitions
                          ,:old-subst
                          ,:new-subst
                          ,:undolist
                          ,:stack
                          ,:cue)
                        ':context)
                     (putprop name (/1+ count) ':count))))
          (t (p:error "ILLEGAL ARGUMENT TO NEXT" name))))
 (defun p:send (:name :args :subst)
    (p:wake :name)
    (catch 'refute (p:send1 :name :args (get :name ':context))))
 (defun p:wake (name) (putprop name (p:wake1 (get name ':wait)) ':wait))
 (defun p:wake1 (wait-list)
    (cond ((null wait-list) nil)
          ((refute (first (car wait-list))
                   (second (car wait-list))
                   (add1 :level))
           (push (third (car wait-list)) :active-process)
           (setq :wait-process (delq (third (car wait-list)) wait-list))
           (p:wake1 (cdr wait-list)))
          (t `(,(car wait-list) . ,(p:wake1 (cdr wait-list))))))
 (declare (:process-name) special)
 (defun p:send1 (:process-name args context)
    (cond (context
             (lets ((:form (pop context)) (:clause (pop context)) 
                    (:definitions (pop context)) (:old-subst (pop context)) 
                    (:new-subst (pop context)) (:undolist nil) 
                    (:final-undo-point nil) (:stack nil) (:cue nil) 
                    (:prolog-world (get :process-name ':world)))
              (and (unify:send args :subst (cdr :form) :old-subst)
                   (let ((:undo-point :undolist))
                    (catch ':send (loop (refute:one)))))))
          (t (p:error "ILLEGAL ARGUMENT TO SEND" :process-name))))
 (defun p:receive (pred $subst)
    (cond ((boundp ':process-name)
           (putprop :process-name
              `(,pred
                nil
                ,(r:getdef pred)
                ,:old-subst
                ,:new-subst
                ,:undolist
                nil
                ,:cue)
              ':context))
          (t (p:error "RECEIVE USED IN ILLEGAL CONTEXT" pred)))
    (throw ':send t))
 (defun p:wait (pred subst)
    (cond ((boundp ':process-name)
           (cond ((refute pred subst (add1 :level)) t)
                 ((boundp ':process-number)
                  (push :process-number :wait-process)
                  (setq :active-process (delq :process-number :active-process))
                  (putprop :process-name
                     `((,pred ,subst ,:process-number) . 
                       ,(get :process-name ':wait))
                     ':wait))
                 (t (p:error "WAIT USED IN ILLEGAL CONTEXT" 
                     "(OUTSIDE PAND)"))))
          (t (p:error "WAIT USED IN ILLEGAL CONTEXT" "(OUTSIDE PROCESS)"))))
 (defun p:initiate (form subst)
    (let ((name (gensym (car form))))
     (putprop name
        `(,form nil ,(r:getdef form) ,subst ,(p:newsubst) nil nil nil)
        ':context)
     (putprop name (cdr form) 'args)
     (putprop name 0 ':count)
     (putprop name subst ':original-subst)
     name))
 (defun p:create (form subst)
    (let ((name (gensym (car form))))
     (putprop name
        `(,form nil ,(r:getdef form) ,subst ,(p:newsubst))
        ':context)
     (putprop name :prolog-world ':world)
     name))
 (defun p:retract (p p-subst)
    (cond ((p:varp p)
           (cond ((p:assigned p p-subst)
                  (p:retract (cadr :fetched-value) (cddr :fetched-value)))
                 (t (p:error "UNDEFINED VARIABLE" p))))
          ((symbolp p) (remprop p (car :prolog-world)) t)
          ((p:varp (car p))
           (cond ((p:assigned (car p) p-subst)
                  (p:retract
                     `(,(p:fetchvalue (car p) p-subst) . ,(cdr p))
                     p-subst))
                 (t (p:error "ILLEGAL ARGUMENT TO RETRACT" p))))
          ((listp p)
           (lets ((def (r:getdef p)) (name (pop p)))
            (do ((d def (cdr d)))
                ((or (null d) (null (car d))))
                (cond ((unify p p-subst (caar d) (p:newsubst ""))
                       (exit (putprop name
                                (delete (car d) def)
                                (car :prolog-world))
                             t))))))
          (t (p:error "ILLEGAL ARGUMENT TO RETRACT" p))))
 (defun p:world (name defs)
    (cond ((atom name)
           (let ((nl nil))
            (mapc defs
               (function
                (lambda (d)
                 (cond ((and (listp d) (symbolp (car d)))
                        (push (car d) nl)
                        (putprop (car d) (cdr d) name))
                       (t (p:error "ILLEGAL ARGUMENT TO WORLD" d))))))
            (putprop name nl ':world)))
          (t (p:error "ILLEGAL ARGUMENT TO WORLD" name))))
 (defun p:erase-world (name)
    (mapc (get name ':world) (function (lambda (x) (remprop x name))))
    (remprop name ':world))
 (defun unify:send (send-pattern send-subst receive-pattern receive-subst)
    (cond ((null send-pattern) t)
          ((unify (car send-pattern)
                  send-subst
                  (car receive-pattern)
                  receive-subst)
           (unify:send
              (cdr send-pattern)
              send-subst
              (cdr receive-pattern)
              receive-subst))))
 (defun p:listing (name-list)
    (mapc name-list
       (function
        (lambda (:name)
         (cond ((symbolp :name)
                (mapc (get :name (car :prolog-world))
                   (function
                    (lambda (d)
                     (cond ((atom d) (print d))
                           (t (grind `(assert 
                                       (,:name . ,(car d)) . 
                                       ,(cdr d)))))))))
               (t (p:error "ILLEGAL ARGUMENT TO LISTING" :name)))))))
 (defun p:alloc-new-file (name)
    (close (outopen (stream (alloc name)))))
 (defun delete (e l)
    (cond ((atom l) l)
          ((eq e (car l)) (cdr l))
          (t `(,(car l) . ,(delete e (cdr l))))))
 (declare
    (:clause-length :clause-vector :undo-vector :stack-vector :subst-vector 
     :hope :current-processes :saved-processes :max-processes)
    special)
 (defun pr:init (size)
    (setq :clause-vector (vector size))
    (setq :subst-vector (vector size))
    (setq :undo-vector (vector size))
    (setq :stack-vector (vector size))
    (setq :active-process nil)
    (setq :wait-process nil)
    (setq :current-processes 0)
    (setq :max-processes (sub1 size)))
 (defun p:por (clauses :old-subst :level)
    (lets ((:goal ':succeed:) (:clause nil) (:definitions nil) (:undolist nil) 
           (:undo-point nil) (:final-undo-point nil) (:wait-process nil) 
           (:saved-processes :current-processes) 
           (:active-process (para:setup clauses)))
     (prog1 (para:refute (function por:refute))
            (setq :current-processes :saved-processes))))
 (declare (:toplevel-function) special)
 (defun para:refute (:toplevel-function)
    (cond ((zerop :saved-processes)
           (comment : toplevel parallel routine)
           (catch 'para (loop (para:call :toplevel-function))))
          (t (catch 'para (para:call :toplevel-function))
             (r:push-cue
                (ncons `(para:reentry
                           ,:toplevel-function
                           ,:active-process
                           ,:wait-process
                           ,:current-processes
                           ,:saved-processes
                           *))
                :old-subst)
             t)))
 (defun para:call (toplevel-function)
    (let ((:hope nil))
     (funcall toplevel-function)
     (cond ((null :hope) (throw 'para nil))
           (:active-process)
           (:wait-process (throw 'para (p:error "DEAD LOCK")))
           (t (throw 'para t)))))
 (defun para:setup (clauses)
    (do ((active-process nil)
         (n :current-processes (add1 n))
         (:stack nil nil)
         (:cue nil)
         (:new-subst (p:newsubst) (p:newsubst)))
        ((null clauses) (setq :current-processes n) (nreverse active-process))
        (and (> n :max-processes) (exit (p:error "TOO MANY PROCESSES")))
        (r:push (car clauses) (r:getdef (pop clauses)))
        (pr:save n)
        (push n active-process)))
 (defun para:reentry (:toplevel-function :active-process :wait-process 
                      :current-processes :saved-processes)
    (let ((:goal ':succeed:) (:old-subst nil) (:level (add1 :level)) 
          (:clause nil) (:definitions nil) (:cue nil) (:undolist nil) 
          (:undo-point nil) (:final-undo-point nil) (:stack nil))
     (catch 'para (funcall :toplevel-function))
     (r:push `(para:reentry
                 ,:toplevel-function
                 ,:active-process
                 ,:wait-process
                 ,:current-processes
                 ,:saved-processes
                 *)
             nil
             :old-subst)))
 (defun p:par (clauses :old-subst :level)
    (lets ((:goal ':succeed:) (:form nil) (:clause nil) (:definitions nil) 
           (:undolist nil) (:undo-point nil) (:final-undo-point nil) 
           (:wait-process nil) (:saved-processes :current-processes) 
           (:active-process (para:setup clauses)))
     (prog1 (para:refute (function par:refute))
            (setq :current-processes :saved-processes))))
 (defun por:refute nil
    (do ((active :active-process (cdr active)))
        ((null active))
        (lets ((:process-number (car active)) 
               (:undolist (vref :undo-vector :process-number)) 
               (:stack (vref :stack-vector :process-number)) (:cue nil) 
               (:new-subst (vref :subst-vector :process-number)) (:form nil))
         (cond (:stack (r:pop)
                       (setq :clause (vref :clause-vector :process-number))
                       (pr:switch:in :undolist)
                       (and (eq (para-ser:interface) ':succeed:)
                            (throw 'para t))
                       (pr:switch:out :undolist)
                       (pr:save :process-number))))))
 (declare (:process-number :active-process :wait-process) special)
 (defun par:refute nil
    (do ((active :active-process (cdr active)))
        ((null active))
        (lets ((:process-number (car active)) 
               (:undolist (vref :undo-vector :process-number)) 
               (:stack (vref :stack-vector :process-number)) (:cue nil) 
               (:new-subst (vref :subst-vector :process-number)) (:form nil))
         (cond (:stack (r:pop)
                       (setq :clause (vref :clause-vector :process-number))
                       (cond ((eq (para-ser:interface) ':succeed:)
                              (setq :active-process (delq :process-number 
                                                     :active-process))
                              (setq :hope t))))))))
 (defun para-ser:interface nil
    (lets ((:undo-point :undolist))
     (catch 'refute
        (progn (refute:one)
               (cond ((r:getdef :form)
                      (and :definitions (r:push :form :definitions)))
                     (t (r:push :form nil)))
               (pr:save :process-number)
               (setq :hope t)))))
 (defun pr:switch:out (u)
    (do ((w u (cdr w))) ((null w)) (delq (cdar w) (caar w))))
 (defun pr:switch:in (u)
    (do ((w u (cdr w)))
        ((null w))
        (rplacd (caar w) `(,(cdar w) . ,(cdaar w)))))
 (defun pr:save (n)
    (vset :undo-vector n :undolist)
    (vset :stack-vector n :stack)
    (vset :subst-vector n :new-subst)
    (vset :clause-vector n :clause))
