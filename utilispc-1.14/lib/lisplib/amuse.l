(comment : amuse)
(format "; Loading AMUSE (Version 82-08-09)/n")
(macro edit (l) `(l:edit ',(car l) ',(cdr l)))
(declare
   (edit-name edit-command entrance-command editor-chain editor-stack 
    editor-variable-prefix find editor-get-definition 
    editor-restore-definition editor-execute edit-load edit-file interactive?)
   special)
(defun amuse (edit-name entrance-command editor-get-definition 
              editor-restore-definition editor-execute)
   (catch ':edit
      (let ((editor-chain nil) (editor-stack nil) (find nil) (printlevel 7) 
            (editor-variable-prefix (character "&")))
       (e:getdef edit-name)
       (catch ':editloop
          (do ((interactive? nil))
              (nil)
              (cond (entrance-command (e:interpret (pop entrance-command)))
                    (t (exit)))))
       (let ((interactive? t)) (e:loop nil)))))
(defun l:edit (edit-name com)
   (amuse edit-name
          com
          (function
           (lambda (name)
            (cond ((symbolp name) (and (definedp name) (copy (getd name))))
                  ((stringp name) (e:getfile name)))))
          (function
           (lambda (def)
            (cond ((symbolp edit-name) (putd edit-name (copy def)))
                  ((stringp edit-name) (e:putfile edit-name def)))))
          (function (lambda (form) (eval form)))))
(declare (flag prmpt) special)
(defun e:loop (flag (prmpt "E:") 
               (attention-handler '(lambda nil (throw ':editloop nil))))
   (loop (catch ':editloop (catch flag (e:interpret (rind prmpt))))))
(defun e:interpret (edit-command)
   (cond ((symbolp edit-command)
          (let ((forms (get edit-command ':edit-form)))
           (cond (forms (e:exlis forms)) (t (e:err "UNDEFINED COMMAND")))))
         ((numberp edit-command) (e:move edit-command))
         ((listp edit-command)
          (cond ((numberp (car edit-command))
                 (do ((i (car edit-command) (sub1 i)))
                     ((<= i 0))
                     (mapc (cdr edit-command) (function e:interpret))))
                (t (ec:x edit-command))))
         (t (push edit-command editor-stack) (e:err "ILLEGAL COMMAND"))))
(defun e:exlis (forms)
   (cond ((null forms) t)
         ((funcall editor-execute (car forms)) (e:exlis (cdr forms)))
         (t nil)))
(macro e:defcom (x)
   (cond ((listp (car x))
          `(mapc ',(car x)
              (function
               (lambda (x) (eval `(e:defcom . ,`(,x . ,',(cdr x))))))))
         (t `(prog2 (putprop ',(car x) ',(cdr x) ':edit-form) ',(car x)))))
(defun e:init nil
   (e:defcom b (ec:b))
   (e:defcom bi (:bi))
   (e:defcom bo (ec:bo))
   (e:defcom c (ec:c))
   (e:defcom d (ec:k) (ec:pop))
   (e:defcom e (:e))
   (e:defcom f (:f))
   (e:defcom fs (:fs))
   (e:defcom fn (ec:fn))
   (e:defcom ib (:ib))
   (e:defcom (i in) (:in))
   (e:defcom it (:it))
   (e:defcom k (ec:k))
   (e:defcom (last l) (ec:l))
   (e:defcom level (:level))
   (e:defcom li (ec:li))
   (e:defcom ld (ec:ld))
   (e:defcom n (ec:n))
   (e:defcom (o pop) (ec:pop))
   (e:defcom p (ec:p))
   (e:defcom pp (ec:pp))
   (e:defcom q (ec:s) (ec:z))
   (e:defcom r (:r))
   (e:defcom ri (ec:ri))
   (e:defcom ra (:ra))
   (e:defcom r1 (:r1))
   (e:defcom r2 (:r2))
   (e:defcom r3 (:r3))
   (e:defcom s (ec:s))
   (e:defcom sc (:sc))
   (e:defcom st (ec:st))
   (e:defcom stack (ec:stack))
   (e:defcom top (ec:top))
   (e:defcom u (:u))
   (e:defcom v (ec:v))
   (e:defcom var (:var))
   (e:defcom x (:x))
   (e:defcom z (ec:z))
   (e:defcom ? (ec:?)))
(defun :bi nil (ec:bi (rind "From:") (rind "To:")))
(defun :e nil (ec:e (e:read "Name:")))
(defun :f nil (ec:f (e:read "Find:")))
(defun :fs nil (ec:fs (e:read "String:")))
(defun :ib nil (ec:ib (e:read "Insert:")))
(defun :in nil (ec:in (e:read "Insert:")))
(defun :it nil (ec:it (e:read "Insert:")))
(defun :level nil (ec:level (rind "Number:")))
(defun :r nil (ec:r (e:read "To:")))
(defun :ra nil (ec:ra (e:read "From:") (e:read "To:") 0))
(defun :r1 nil (ec:ra (e:read "From:") (e:read "To:") 1))
(defun :r2 nil (ec:ra (e:read "From:") (e:read "To:") 2))
(defun :r3 nil (ec:ra (e:read "From:") (e:read "To:") 3))
(defun :sc nil (ec:sc (e:read "Name:")))
(defun :u nil (ec:u (e:read "Upto:")))
(defun :var nil (ec:var (e:read "Prefix:")))
(defun :x nil (ec:x (e:read "Form:")))
(defun ec:? nil (format "EDITING /S/N" edit-name) t)
(defun ec:x (form)
   (let ((interactive? nil)) (print (funcall editor-execute form))))
(defun ec:ld nil (mapcar (caar (last editor-chain)) editor-execute))
(defun e:scope nil (caar editor-chain))
(defun e:stack nil editor-stack)
(defun e:name nil edit-name)
(defun ec:level (level) (setq printlevel level))
(defun ec:top nil (setq editor-chain (last editor-chain)) t)
(defun ec:st nil (grind (e:fetchvalue editor-stack '(nil) printlevel)) t)
(defun ec:stack nil (grind editor-stack) t)
(defun ec:s nil (e:restore (caar (last editor-chain))) t)
(defun ec:sc (edit-name) (e:restore (caar (last editor-chain))) t)
(defun ec:z nil (throw ':edit t))
(defun ec:var (pr) (setq editor-variable-prefix (character pr)))
(defun ec:c nil (push (copy (caar editor-chain)) editor-stack) t)
(defun ec:e (name) (e:getdef name))
(defun ec:it (el) (and (e:move 1) (ec:ib el) (e:move 0)))
(defun ec:p nil
   (grind (e:fetchvalue (caar editor-chain) '(nil) printlevel))
   t)
(defun ec:pp nil (grind (caar editor-chain)) t)
(defun e:getdef (nam1)
   (setq editor-chain (ncons (ncons (funcall editor-get-definition nam1))))
   (setq edit-name nam1))
(defun e:read ((prmpt "ARG:"))
   (let ((com (cond ((listp entrance-command) (pop entrance-command))
                    (t (rind prmpt)))))
    (cond ((numberp com)
           (cond ((< com 0) (e:err "ILLEGAL STACK POSITION"))
                 ((= com 0) (copy (caar editor-chain)))
                 ((>= (length editor-stack) com)
                  (e:fetchvalue (nth (sub1 com) editor-stack)))
                 (t (e:err "STACK TOO SHORT"))))
          ((and (listp com)
                (eq (car com) 'quote)
                (cdr com)
                (numberp (second com)))
           (second com))
          (t com))))
(defun e:err (mes)
   (format "/S : /C/N" edit-command mes)
   (cond (interactive? (readline) (throw ':editloop nil)) (t nil)))
(defun e:fatal (mes)
   (format "/S : /C/N" edit-command mes)
   (cond (interactive? (readline)) (t nil))
   (throw ':editloop nil))
(defun e:warn (mes)
   (cond (interactive?
            (format "/S : /C/N" edit-command mes)
            (readline)
            (throw ':editloop nil))
         (t nil)))
(defun ec:n nil
   (let ((hc (pop editor-chain)))
    (cond ((null editor-chain) (push hc editor-chain) (e:err "TOP LEVEL"))
          ((numberp (cdr hc))
           (let ((length (string-length (caar editor-chain))))
            (cond ((< (/+ (cdr hc) (string-length (car hc))) length)
                   (e:move (/+ (cdr hc) (string-length (car hc)) 1))
                   t)
                  (t (push hc editor-chain) (e:err "LAST CHARACTER")))))
          ((null (cddr hc)) (push hc editor-chain) (e:err "LAST ELEMENT"))
          (t (push `(,(second (cdr hc)) . ,(cdr (cdr hc))) editor-chain) t))))
(defun ec:b nil
   (let ((hc (pop editor-chain)))
    (cond ((null editor-chain) (push hc editor-chain) (e:err "TOP LEVEL"))
          ((numberp (cdr hc))
           (cond ((< 0 (cdr hc))
                  (push `(,(string (sref (caar editor-chain) (sub1 (cdr hc))))
                          . ,(sub1 (cdr hc)))
                        editor-chain)
                  t)
                 (t (push hc editor-chain) (e:err "FIRST CHARACTER"))))
          ((eq (caar editor-chain) (cdr hc))
           (push hc editor-chain)
           (e:err "FIRST ELEMENT"))
          (t (do ((w (caar editor-chain) (cdr w)))
                 ((eq (cdr w) (cdr hc))
                  (push `(,(car w) . ,w) editor-chain)
                  t))))))
(defun ec:in (item)
   (let ((hc (pop editor-chain)))
    (cond (editor-chain
             (cond ((numberp (cdr hc))
                    (ec:r (funcall (cond ((stringp (caar editor-chain)) 'cr)
                                         ((symbolp (caar editor-chain)) 
                                          'intern)
                                         (t (e:fatal "IMPLEMENTATION ERROR")))
                             (string-append
                                (substring
                                   (caar editor-chain)
                                   0
                                   (/+ (cdr hc) (string-length (car hc))))
                                item
                                (substring
                                   (caar editor-chain)
                                   (/+ (cdr hc) (string-length (car hc)))))))
                    (push `(,item . ,(/+ (cdr hc) (string-length (car hc))))
                          editor-chain))
                   (t (rplacd (cdr hc) `(,item . ,(cddr hc)))
                      (push `(,item . ,(cddr hc)) editor-chain)))
             t)
          (t (push hc editor-chain)
             (push item editor-stack)
             (e:err "TOP LEVEL")))))
(defun ec:k nil
   (let ((hc (pop editor-chain)))
    (cond (editor-chain
             (push (pop hc) editor-stack)
             (cond ((numberp hc)
                    (ec:r (funcall (cond ((stringp (caar editor-chain)) 'cr)
                                         ((symbolp (caar editor-chain)) 
                                          'intern)
                                         (t (e:fatal "IMPLEMENTATION ERROR")))
                             (string-append
                                (substring (caar editor-chain) 0 hc)
                                (substring
                                   (caar editor-chain)
                                   (/+ (string-length (car editor-stack)) 
                                    hc)))))
                    (cond ((>= hc (string-length (caar editor-chain))))
                          (t (e:move (add1 hc)))))
                   ((cdr hc)
                    (rplaca hc (second hc))
                    (rplacd hc (cddr hc))
                    (push `(,(car hc) . ,hc) editor-chain))
                   ((eq hc (caar editor-chain))
                    (comment one element list)
                    (rplaca editor-chain
                       `(nil . 
                         ,(cond ((cdar editor-chain)
                                 (rplaca (cdar editor-chain) nil))
                                (t nil)))))
                   (t (do ((w (caar editor-chain) (cdr w)))
                          ((eq (cdr w) hc) (rplacd w (cddr w))))))
             t)
          (t (push hc editor-chain) (e:err "TOP LEVEL")))))
(defun e:move (n (head (caar editor-chain)))
   (cond ((zerop n)
          (cond ((cdr editor-chain) (pop editor-chain) t)
                (t (e:warn "TOP LEVEL"))))
         ((listp head)
          (setq n (e:range n))
          (cond ((or (> n (length head)) (<= n 0)) (e:warn "LIST TOO SHORT"))
                (t (push `(,(nth (sub1 n) head) . ,(nthcdr (sub1 n) head))
                         editor-chain)
                   t)))
         ((or (stringp head) (symbolp head))
          (setq n (e:srange n))
          (push `(,(string (sref head n)) . ,n) editor-chain)
          t)
         (t (e:warn "ATOMIC SCOPE"))))
(defun e:srange (n)
   (lets ((head (caar editor-chain)) (length (string-length head)))
    (cond ((and (> n 0) (<= n (add1 length))) (sub1 n))
          ((< n 0) (e:srange (/+ length n 1)))
          ((= n 0) 0)
          (t (sub1 length)))))
(defun ec:ib (item)
   (let ((hc (pop editor-chain)))
    (cond (editor-chain
             (cond ((numberp (cdr hc))
                    (ec:r (funcall (cond ((stringp (caar editor-chain)) 'cr)
                                         ((symbolp (caar editor-chain)) 
                                          'intern)
                                         (t (e:fatal "IMPLEMENTATION ERROR")))
                             (string-append
                                (substring (caar editor-chain) 0 (cdr hc))
                                item
                                (substring (caar editor-chain) (cdr hc)))))
                    (push `(,item . ,(cdr hc)) editor-chain))
                   (t (rplacd (cdr hc) `(,(cadr hc) . ,(cddr hc)))
                      (rplaca (cdr hc) item)
                      (push `(,(car hc) . ,(cddr hc)) editor-chain)
                      t)))
          (t (push hc editor-chain)
             (push item editor-stack)
             (e:err "TOPLEVEL")))))
(defun e:restore (defs)
   (funcall editor-restore-definition defs)
   (and interactive? (format "/S : RESTORED/N" edit-name))
   t)
(defun ec:r (item)
   (let ((hc (pop editor-chain)))
    (cond (editor-chain
             (cond ((numberp (cdr hc))
                    (ec:r (funcall (cond ((stringp (caar editor-chain)) 'cr)
                                         ((symbolp (caar editor-chain)) 
                                          'intern)
                                         (t (e:fatal "IMPLEMENTATION ERROR")))
                             (string-append
                                (substring (caar editor-chain) 0 (cdr hc))
                                (string item)
                                (substring
                                   (caar editor-chain)
                                   (/+ (cdr hc) (string-length (car hc)))))))
                    (push `(,(string item) . ,(cdr hc)) editor-chain))
                   (t (pop hc)
                      (rplaca hc item)
                      (push `(,item . ,hc) editor-chain))))
          (t (push (car hc) editor-stack)
             (setq editor-chain (ncons (ncons item)))))
    t))
(declare (from to repmax repc :undolist) special)
(defun ec:ra (from to (repmax 0))
   (let ((repc 0) (:undolist nil) (s (ncons nil)))
    (cond ((e:unify from s (caar editor-chain))
           (cond ((cdr editor-chain)
                  (rplaca (cdar editor-chain)
                     (car (rplaca (car editor-chain) (e:fetchvalue to s)))))
                 (t (rplaca (car editor-chain) (e:fetchvalue to s))))
           (catch 'replaceall (rep:count)))
          (t (catch 'replaceall (e:repall (caar editor-chain)))))
    (cond (interactive? (format "/s replaced/n" repc))
          ((zerop repc) nil)
          (t repc))))
(defun e:repall (l (s (ncons nil)))
   (cond ((atom l))
         (t (cond ((e:unify from s (car l))
                   (rplaca l (e:fetchvalue to s))
                   (rep:count))
                  (t (e:repall (car l))))
            (cond ((and (atom (cdr l))
                        (e:unify from (setq s (ncons nil)) (cdr l)))
                   (rplacd l (e:fetchvalue to s))
                   (rep:count))
                  (t (e:repall (cdr l)))))))
(defun rep:count nil
   (cond ((eq (setq repc (add1 repc)) repmax) (throw 'replaceall nil))))
(defun ec:pop nil
   (cond ((null editor-stack) (e:err "NOTHING SAVED"))
         (t (pop editor-stack))))
(defun ec:f (item (:undolist nil))
   (setq find item)
   (or (e:find item) (e:warn "NOT FOUND")))
(defun ec:fs (item) (setq find item) (or (e:finds item) (e:warn "NOT FOUND")))
(defun e:find-substring (item (from 0))
   (cond ((or (stringp item) (symbolp item))
          (let ((n (string-search item (caar editor-chain) from)))
           (and n (push `(,(string item) . ,n) editor-chain))))
         (t (e:err "INCOMPATIBLE TYPE"))))
(defun e:find (item)
   (lets ((x (caar editor-chain)))
    (cond ((e:unify item (ncons nil) x) t)
          ((atom x) nil)
          (t (do ((length (length x)) (n 1 (add1 n)))
                 ((> n length))
                 (e:move n)
                 (cond ((e:find item) (exit t)) (t (pop editor-chain))))))))
(defun e:finds (item)
   (lets ((x (caar editor-chain)))
    (cond ((or (stringp x) (symbolp x))
           (cond ((e:find-substring item) t) (t nil)))
          ((atom x) nil)
          (t (do ((length (length x)) (n 1 (add1 n)))
                 ((> n length))
                 (e:move n)
                 (cond ((e:finds item) (exit t)) (t (pop editor-chain))))))))
(defun ec:fn nil
   (let ((oldchain editor-chain))
    (cond ((numberp (cdar editor-chain))
           (cond ((e:find-substring
                     find
                     (/+ (cdar editor-chain)
                         (string-length (car (pop editor-chain)))))
                  t)
                 (t (loop (cond ((null (cdr editor-chain))
                                 (setq editor-chain oldchain)
                                 (exit (e:warn "NOT FOUND")))
                                (t (lets ((x (pop editor-chain)) (y (pop x))) 
                                    (cond 
                                     ((atom (cdr x)) (comment last element)) 
                                     (t 
                                      (push `(,(second x) . ,(cdr x))
                                            editor-chain)
                                      (and (e:finds find) (exit t)))))))))))
          (t (loop (cond ((null (cdr editor-chain))
                          (setq editor-chain oldchain)
                          (exit (e:warn "NOT FOUND")))
                         (t (lets ((x (pop editor-chain)) (y (pop x)))
                             (cond ((atom (cdr x)) (comment last element))
                                   (t 
                                    (push `(,(second x) . ,(cdr x))
                                          editor-chain)
                                    (and (e:find find) (exit t))))))))))))
(defun ec:l nil (e:move -1))
(defun ec:last nil (e:move (length (caar editor-chain))))
(defun ec:bi (from to (head (caar editor-chain)))
   (cond ((atom head) (e:err "ATOMIC SCOPE"))
         (t (lets ((pos1 (e:range from)) (pos2 (e:range to)))
             (cond ((> pos1 pos2) (e:err "FIRST OPERAND EXCEEDS SECOND"))
                   ((or (<= pos1 0) (> pos2 (length head)))
                    (e:err "ILLEGAL POSITIONING"))
                   (t (let ((el1 (nthcdr (/1- pos1) head)) 
                            (el2 (nthcdr (/1- pos2) head)))
                       (rplacd (rplaca el1 `(,(car el1) . ,(cdr el1)))
                          (cdr el2))
                       (rplacd (cond ((eq el1 el2) (car el2)) (t el2)) nil)
                       t)))))))
(defun ec:ri nil
   (lets ((hc (pop editor-chain)) 
          (up (cond (editor-chain (caar editor-chain))
                    (t (push hc editor-chain) (e:err "TOP LEVEL"))))
          (w (cdr hc)))
    (rplacd (rplaca up `(,(car up) . ,(cdr up))) (cdr w))
    (rplacd (cond ((eq w up) (car w)) (t w)) nil)
    (push `(,(car up) . ,up) editor-chain)
    (push `(,(car (cond ((eq w up) (car w)) (t w))) . 
            ,(cond ((eq w up) (car w)) (t w)))
          editor-chain))
   t)
(defun ec:li nil
   (lets ((hc (pop editor-chain)) 
          (w (cond (editor-chain (cdr hc))
                   (t (push hc editor-chain) (e:err "TOP LEVEL")))))
    (rplacd (rplaca w `(,(car w) . ,(cdr w))) nil)
    (push `(,(car w) . ,(cdr hc)) editor-chain)
    (push `(,(car hc) . ,(car w)) editor-chain))
   t)
(defun ec:bo nil
   (lets ((hc (cond ((atom (caar editor-chain)) (e:err "ATOMIC ITEM"))
                    (t (pop editor-chain))))
          (w (cdr hc)))
    (cond (editor-chain
             (rplaca (rplacd w (nconc (cdar w) (cdr w))) (caar w))
             t)
          (t (push hc editor-chain) (e:err "TOP LEVEL")))))
(defun e:range (n (head (caar editor-chain)))
   (and (eq n 'l) (setq n -1))
   (cond ((not (numberp n)) (e:fatal "NUMBER EXPECTED"))
         ((minusp n)
          (cond ((atom head) (e:warn "ATOMIC SCOPE"))
                (t (/+ (length head) n 1))))
         (t n)))
(defun e:putfile (file x)
   (do ((standard-output (outopen (stream (alloc file)))))
       ((null x) (close standard-output))
       (grind (pop x))))
(defun e:getfile (edit-file)
   (catch 'getfile
      (lets ((ddname (alloc edit-file)) 
             (standard-input (inopen (stream ddname))) 
             (err:end-of-file
                '(lambda (x) (throw 'getfile (nreverse edit-load))))
             (edit-load nil))
       (loop (push (read) edit-load)))))
(defun ec:v nil
   (let ((hc (car editor-chain)))
    (cond ((numberp (cdr hc))
           (print (string-append
                     (substring (car (second editor-chain)) 0 (cdr hc))
                     "$$$"
                     (substring
                        (car (second editor-chain))
                        (/+ (cdr hc) (string-length (car hc))))))
           t)
          ((cdr editor-chain)
           (grind (e:fetchvalue
                     (e:maplist
                        (car (second editor-chain))
                        (function
                         (lambda (x)
                          (cond ((eq x (cdar editor-chain)) '$$$)
                                ((atom x) x)
                                (t (car x))))))
                     '(nil)
                     printlevel))
           t)
          (t (e:err "Top level")))))
(defun e:maplist (l f)
   (cond ((atom l) (funcall f l))
         (t `(,(funcall f l) . ,(e:maplist (cdr l) f)))))
(defun e:unify (x subst y)
   (cond ((e:varp x)
          (cond ((e:assigned x subst) (equal (e:fetch x subst) y))
                (t (e:link x subst y))))
         ((atom y) (equal x y))
         ((atom x) nil)
         ((e:unify (car x) subst (car y)) (e:unify (cdr x) subst (cdr y)))))
(defun e:varp (x) (and (symbolp x) (eq (character x) editor-variable-prefix)))
(defun e:assigned (x subst) (assq x (cdr subst)))
(defun e:link (x xsubst y) (rplacd xsubst `((,x . ,y) . ,(cdr xsubst))))
(defun e:fetch (x subst (value (assq x (cdr subst))))
   (cond (value (cdr value)) (t x)))
(defun e:fetchvalue (x (subst '(nil)) (level 0))
   (cond ((e:varp x)
          (cond ((e:assigned x subst)
                 (e:fetchvalue (e:fetch x subst) '(nil) level))
                (t x)))
         ((stringp x) (substring x 0))
         ((atom x) x)
         ((zerop (sub1 level)) (cond ((null (cdr x)) '(?)) (t '?)))
         ((vectorp x)
          (mapvector x '(lambda (y) (e:fetchvalue y subst (sub1 level)))))
         (t `(,(e:fetchvalue (car x) subst (sub1 level)) . 
              ,(e:fetchvalue (cdr x) subst (sub1 level))))))
(defun vte:edit nil (exfile "#a4154.lisplib.comp(vt100)") (funcall 'vte:edit))
(defun ec:u (upto)
   (do ((hc (cdr editor-chain) (cdr hc)))
       ((null hc) (e:err "No such upper structure"))
       (cond ((and (listp (caar hc)) (equal (caaar hc) upto))
              (exit (setq editor-chain hc) t)))))
(defun e:current-state nil editor-chain)
(defun e:restore-state (state) (setq editor-chain state))
(macro execm (c)
   `(catch ':editloop
       (cond ((boundp 'editor-chain)
              (do ((entrance-command ',c))
                  ((null entrance-command) t)
                  (e:interpret (pop entrance-command))))
             (t (format "EXECM used outside the editor./n") nil))))
(e:init)