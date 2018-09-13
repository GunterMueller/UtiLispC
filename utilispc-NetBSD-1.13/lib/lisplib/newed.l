 (declare
    (pattern hc count cmnd deleted plen plev before-jump contents stream)
    special)
 (defmacro ed (x) (list '&eddef (list 'quote x)))
 (defun &eddef (x)
    (loop (and (symbolp x) (definedp x) (exit))
          (format "/s IS NOT FUNC OR MACRO NAME/n" x)
          (setq x (lets ((prompt "ENTER FUNC NAME>")) (read))))
    (let ((temp (&ed (getd x)))) (and temp (putd x (car temp)))))
 (defmacro edv (x) (list '&edval (list 'quote x)))
 (defun &edval (x)
    (loop (and (symbolp x) (boundp x) (exit))
          (format "/s IS NOT BOUND SYMBOL/n" x)
          (setq x (lets ((prompt "ENTER VARIABLE NAME>")) (read))))
    (let ((temp (&ed (eval x)))) (and temp (set x (car temp)))))
 (defmacro edp (x) (list '&edprop (list 'quote x)))
 (defun &edprop (x)
    (loop (and (symbolp x) (exit))
          (format "/s IS NOT SYMBOL/n" x)
          (setq x (lets ((prompt "ENTER SYMBOL>")) (read))))
    (let ((temp (&ed (plist x)))) (and temp (setplist x (car temp)))))
 (defun edf (x)
    (lets ((stream (inopen (cond ((stringp x) (stream (alloc x)))
                                 (t (funcall err:argument-type x 'edf)))))
           (contents))
     (catch 'edf
        (do ((err:end-of-file (function (lambda (stream) (throw 'edf)))))
            (nil)
            (push (read stream) contents)))
     (lets ((temp (&ed (nreverse contents))))
      (and temp
           (lets ((standard-output (outopen stream)))
            (mapc (car temp) (function (lambda (x) (terpri) (prind x))))
            (close standard-output))))))
 (defun &ed (x)
    (lets ((hc (list (list (copy x)))) (prompt "?") (plen 4) (plev 3) 
           (before-jump hc) (deleted) (pattern) 
           (attention-handler (function (lambda nil (throw '&ed)))))
     (catch '&edend
        (loop (catch '&ed
                 (do ((cmnd (read) (read)))
                     (nil)
                     (cond ((fixp cmnd) (&move cmnd))
                           ((symbolp cmnd)
                            (cond ((get cmnd '&ed) (funcall (get cmnd '&ed)))
                                  (t (&err "INVALID COMMAND"))))
                           (t (lets ((break (function 
                                             (lambda nil (throw '&ed))))
                                     (? (caar hc)))
                               (print (setq deleted (eval cmnd))))))))))))
 (defun &err (msg) (format "### /c - /s/n" msg cmnd) (readline) (throw '&ed))
 (defun &match (x (y pattern))
    (cond ((atom y) (or (eq y '?) (equal x y)))
          ((atom x) nil)
          ((eq (car y) '???)
           (pop y)
           (or (null y)
               (eq x y)
               (loop (cond ((&match x y) (exit t))
                           ((atom x) (exit nil))
                           (t (pop x))))))
          ((&match (car x) (car y)) (&match (cdr x) (cdr y)))))
 (defun &fi nil
    (or (&match (caar hc))
        (do ((l (caar hc) (cdr l)))
            ((atom l) (and (not (null l)) (equal l pattern)))
            (push l hc)
            (and (&fi) (exit t))
            (pop hc))))
 (defun &ff nil
    (and (listp hc)
         (do ((l (cdr (pop hc)) (cdr l)))
             ((atom l) (&ff))
             (push l hc)
             (and (&fi) (exit t))
             (pop hc))))
 (defun &fb nil
    (and (listp hc)
         (do ((l (&pred (pop hc)) (&pred l)))
             ((null l) (&fb))
             (push l hc)
             (and (&fib) (exit t))
             (pop hc))))
 (defun &fib nil
    (or (and (listp (caar hc))
             (lets ((x (last (caar hc))))
              (or (and (not (null (cdr x))) (equal (cdr x) pattern))
                  (do ((l x (&pred l)))
                      ((null l) nil)
                      (push l hc)
                      (and (&fib) (exit t))
                      (pop hc)))))
        (&match (caar hc))))
 (defun &pred (x)
    (cond ((eq x (caar hc)) nil)
          (t (do ((l (caar hc) (cdr l))) ((eq x (cdr l)) l)))))
 (defun &read-pattern nil
    (lets ((temp (rind "PATTERN?"))) (or (eq temp '@) (setq pattern temp))))
 (defun &move (n)
    (cond ((/0= n) (and (null (cdr hc)) (&err "TOP LEVEL")) (pop hc))
          ((atom (caar hc)) (&err "ATOMIC SCOPE"))
          (t (push (nthcdr (/1- (&range n)) (caar hc)) hc))))
 (defun &range (n)
    (or (fixp n) (&err "NUMBER EXPECTED"))
    (and (/0> n) (setq n (/+ (length (caar hc)) n 1)))
    (or (<= 0 n (length (caar hc))) (&err "LIST TOO SHORT"))
    n)
 (defun &readnum nil
    (lets ((temp (rind "NUMBER?")))
     (or (fixp temp) (&err "NUMBER EXPECTED"))
     temp))
 (defun &op (p fn)
    (and (atom (caar hc)) (&err "ATOMIC SCOPE"))
    (cond ((fixp p) (funcall fn (&range p)))
          (t (and (listp p)
                  (eq (car p) 'quote)
                  (listp (cdr p))
                  (null (cddr p))
                  (cond ((fixp (cadr p)) (setq p (cadr p)))
                        ((eq (cadr p) '@) (setq p pattern))))
             (setq pattern p)
             (or (&opp fn) (&err "NOT FOUND")))))
 (defun &opp (fn)
    (and (listp (caar hc))
         (do ((x (caar hc) (cdr x)) (n 1 (/1+ n)))
             ((atom x) nil)
             (and (&match (car x)) (exit (funcall fn n) t))
             (push x hc)
             (and (&opp fn) (pop hc) (exit t))
             (pop hc))))
 (defprop in
    (lambda nil
     (and (null (cdr hc)) (&err "TOP LEVEL"))
     (lets ((l (memq (caar hc) (car (second hc)))))
      (rplacd l (cons (rind "SEXPR?") (cdr l)))))
    &ed)
 (defprop a
    (lambda nil
     (cond ((atom (caar hc)) (rplaca (car hc) (rind "SEXPR?")))
           (t (rplacd (last (caar hc)) (rind "SEXPR?")))))
    &ed)
 (defprop p (lambda nil (prind (caar hc) (linesize) nil plev plen)) &ed)
 (defprop v
    (lambda nil
     (print (cond ((null (cdr hc)) (caar hc))
                  (t (subst '# (caar hc) (caadr hc))))))
    &ed)
 (defprop pp (lambda nil (prind (caar hc))) &ed)
 (defprop q (lambda nil (throw '&edend (car (last hc)))) &ed)
 (defprop k (lambda nil (throw '&edend)) &ed)
 (defprop n
    (lambda nil
     (and (atom (cdar hc)) (&err "LIST TAIL"))
     (push (cdr (pop hc)) hc))
    &ed)
 (defprop l
    (lambda nil
     (and (atom (cdr hc)) (&err "TOP LEVEL"))
     (and (eq (car hc) (caadr hc)) (&err "LIST TOP"))
     (push (&pred (pop hc)) hc))
    &ed)
 (defprop ra
    (lambda nil
     (and (atom (caar hc)) (&err "ATOMIC SCOPE"))
     (lets ((count 0))
      (&read-pattern)
      (&repall (caar hc) (rind "SEXPR?"))
      (format "/s OCCURENCES REPLACED/n" count)))
    &ed)
 (defun &repall (x y)
    (loop (cond ((&match (car x)) (rplaca x (copy y)) (incr count 1))
                ((listp (car x)) (&repall (car x) y)))
          (cond ((atom (cdr x))
                 (and (not (null (cdr x)))
                      (equal (cdr x) pattern)
                      (progn (rplacd x (copy y)) (incr count 1)))
                 (exit)))
          (pop x)))
 (defprop i
    (lambda nil
     (&op (rind "??")
          (function
           (lambda (n)
            (cond ((/0= n) (rplaca (car hc) (cons (rind "??") (caar hc))))
                  (t (lets ((l (nthcdr (/1- n) (caar hc))))
                      (rplacd l (cons (rind "??") (cdr l))))))))))
    &ed)
 (defprop d
    (lambda nil
     (&op (rind "??")
          (function
           (lambda (n)
            (cond ((/0= n) (&err "INVALID ARGUMENT"))
                  ((eq n 1)
                   (setq deleted (caaar hc))
                   (rplaca (car hc) (cdaar hc)))
                  (t (lets ((l (nthcdr (/- n 2) (caar hc))))
                      (setq deleted (cadr l))
                      (rplacd l (cddr l)))))))))
    &ed)
 (defprop r
    (lambda nil
     (&op (rind "??")
          (function
           (lambda (n)
            (and (/0= n) (&err "INVALID ARGUMENT"))
            (setq deleted (nth (/1- n) (caar hc)))
            (rplaca (nthcdr (/1- n) (caar hc)) (rind "??"))))))
    &ed)
 (defprop y
    (lambda nil
     (&op (rind "??")
          (function
           (lambda (n)
            (cond ((/0= n) (rplaca (car hc) (cons (copy deleted) (caar hc))))
                  (t (lets ((l (nthcdr (/1- n) (caar hc))))
                      (rplacd l (cons (copy deleted) (cdr l))))))))))
    &ed)
 (defprop f
    (lambda nil
     (lets ((save hc))
      (&read-pattern)
      (cond ((&fi) (setq before-jump save)) (t (&err "NOT FOUND")))))
    &ed)
 (defprop ff
    (lambda nil
     (lets ((save hc))
      (&read-pattern)
      (cond ((&ff) (setq before-jump save))
            (t (setq hc save) (&err "NOT FOUND")))))
    &ed)
 (defprop fn
    (lambda nil
     (lets ((save hc))
      (cond ((&ff) (setq before-jump save))
            (t (setq hc save) (&err "NOT FOUND")))))
    &ed)
 (defprop fb
    (lambda nil
     (lets ((save hc))
      (&read-pattern)
      (cond ((&fb) (setq before-jump save))
            (t (setq hc save) (&err "NOT FOUND")))))
    &ed)
 (defprop x
    (lambda nil
     (or (&memtree (car before-jump) (car (last hc))) (&err "ALREADY DELETED"))
     (setq hc before-jump before-jump hc))
    &ed)
 (defun &memtree (x y)
    (or (eq x y)
        (and (listp (car y)) (&memtree x (car y)))
        (and (listp (cdr y)) (&memtree x (cdr y)))))
 (defprop bi
    (lambda nil
     (lets ((m (&range (rind "POSITION?"))) (n (&range (rind "POSITION?"))) 
            (w (nthcdr (/1- m) (caar hc))) (ww (nthcdr (/1- n) (caar hc))))
      (cond ((> m n) (&err "FIRST OPERAND EXCEEDS SECOND"))
            (t (rplacd (rplaca w (cons (car w) (cdr w))) (cdr ww))
               (rplacd (cond ((eq w ww) (car ww)) (t ww)) nil)))))
    &ed)
 (defprop bo
    (lambda nil
     (lets ((n (&range (rind "POSITION?"))) (w (nthcdr (/1- n) (caar hc))))
      (cond ((atom (car w)) (&err "SPECIFIED ITEM IS AN ATOM"))
            (t (rplaca (rplacd w (nconc (cdar w) (cdr w))) (caar w))))))
    &ed)
 (defprop li
    (lambda nil
     (lets ((n (&range (rind "POSITION?"))) 
            (w (nthcdr (cond ((/0= n) (&err "ILLEGAL OPERAND")) (t (/1- n)))
                       (caar hc))))
      (rplacd (rplaca w (cons (car w) (cdr w))) nil)))
    &ed)
 (defprop lo
    (lambda nil
     (lets ((n (&range (rind "POSITION?"))) (w (nthcdr (/1- n) (caar hc))))
      (and (atom (car w)) (&err "SPECIFIED ITEM IS AN ATOM"))
      (rplacd (rplaca (caar hc) (cons (caaar hc) (cdaar hc))) (cdr w))
      (rplaca (rplacd w (cdar w)) (caar w))))
    &ed)
 (defprop ri
    (lambda nil
     (lets ((n (&range (rind "POSITION?"))) 
            (w (nthcdr (cond ((/0= n) (&err "ILLEGAL OPERAND")) (t (/1- n)))
                       (caar hc))))
      (rplacd (rplaca (caar hc) (cons (caaar hc) (cdaar hc))) (cdr w))
      (rplacd (cond ((eq w (caar hc)) (car w)) (t w)) nil)))
    &ed)
 (defprop ro
    (lambda nil
     (lets ((n (&range (rind "POSITION?"))) (w (nthcdr (/1- n) (caar hc))))
      (and (atom (car w)) (&err "SPECIFIED ITEM IS AN ATOM"))
      (rplacd (last (car w)) (cdr w))
      (rplacd w nil)))
    &ed)
 (defprop top (lambda nil (setq hc (last hc))) &ed)
 (defprop u
    (lambda nil
     (lets ((n (&readnum)))
      (or (< n (length hc)) (&err "NOT IN SO DEEP LEVEL"))
      (setq hc (nthcdr n hc))))
    &ed)
 (defprop level (lambda nil (setq plev (&readnum))) &ed)
 (defprop length (lambda nil (setq plen (&readnum))) &ed)
 (defprop w
    (lambda nil
     (do ((l (reverse hc) (cdr l)))
         ((null (cdr l)))
         (do ((x (caar l) (cdr x)) (n 1 (/1+ n)))
             ((eq (car x) (caadr l)) (princ ".") (princ n))))
     (terpri))
    &ed)
 (defprop e
    (lambda nil
     (lets ((? (caar hc)) (break (function (lambda nil (throw '&ed)))))
      (setq deleted (print (eval (rind "FORM?"))))))
    &ed)

 (defun linesize () 80)

