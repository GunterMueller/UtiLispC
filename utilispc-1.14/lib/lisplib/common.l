;(macro cl:apply (x)
;   (lets ((morearg (car (last x))) (fn (car x)) 
;          (argl (nreverse (cdr (nreverse (cdr x))))))
;    `(apply ,fn (nconc ,(cons 'list argl) ,morearg))))

(macro aref (x) `(vref . ,x))

(macro case (x) `(selectq . ,x))

(defun constantp (x)
   (or (numberp x)
       (and (symbolp x) (or (boundp x) (key-p x)))
       (and (consp x) (eq (car x) 'quote))))

(macro decf (x)
   `(setf ,(car x) (/- ,(car x) ,(cond ((cdr x) (second x)) (t 1)))))

(defmacro defconstant (var val . doc) `(defconst ,var ,val))

(defmacro defparameter (var val . doc) `(setq ,var ,val))

(defmacro deftype (type-name . rest) `',type-name)

(defmacro defvar (var val . doc) `(setq ,var ,val))

(defmacro dolist ((var l . res) . body)
   `(lets ((,var))
      (mapc ,l (function (lambda (,var) . ,body)))
      (progn . ,res)))

(defmacro dotimes ((var count . res) . body)
   `(do ((,var 0 (/1+ ,var))) ((= ,var ,count) . ,res) . ,body))

(defmacro eval-when (situation . forms) `(progn . ,forms))

(macro export (x) ''export)

(defun find (key vector)
   (do ((i 0 (/1+ i)))
       ((or (>= i (vector-length vector)) (eq key (vref vector i)))
        (and (< i (vector-length vector)) (vref vector i)))))

(defun floor (x (d 1)) (// x d))

(defun getf (place ind (default))
   (cond ((atom place) default)
         ((atom (cdr place)) default)
         ((eq (car place) ind) (second place))
         (t (getf (cddr place) ind default))))

(defun gethash (key hash (defl))
   (lets ((x (ass (car hash) key (cdr hash)))) (cond (x (cdr x)) (t defl))))

(defmacro if (test then . else) `(cond (,test ,then) (t . ,else)))

(macro incf (x)
   `(setf ,(car x) (/+ ,(car x) ,(cond ((cdr x) (second x)) (t 1)))))

(macro in-package (body) ''in-package)

(macro let* (forms) `(lets . ,forms))

(defun macroexpand (form (env))
   (cond ((atom form) form)
         ((and (symbolp (car form))
               (definedp (car form))
               (consp (getd (car form)))
               (eq (car (getd (car form))) 'macro))
          (macroexpand (funcall (cdr (getd (car form))) (cdr form)) env))
         ((and (consp (car form)) (eq (caar form) 'macro))
          (macroexpand (funcall (cdar form) (cdr form)) env))
         (t form)))

(macro make-array (x)
   (lets ((con (memq ':initial-element x)))
    `(vector ,(car x) ,(and con (second con)))))

(macro make-hash-table (x)
   (lets ((test (memq ':test x)))
    ``(,,(cond (test (second test)) (t (function eq))))))

(defmacro cl:mapc (func list) `(mapc ,list ,func))

(defmacro cl:mapcar (func list) `(mapcar ,list ,func))

(defmacro cl:mapcan (func list) `(mapcan ,list ,func))

(defmacro cl:mapcon (func list) `(mapcon ,list ,func))

(defmacro cl:maplist (func list) `(maplist ,list ,func))

(defmacro multiple-value-bind (vars values-form . form)
   `(apply (function (lambda ,vars . ,form)) ,values-form))

(defun position (item v)
  (cond ((consp v)
	 (let ((q (memq item v)))
	   (and q (- (length v) (length q)))))
	(t (let ((i 0) (vlen (vector-length v)))
	     (loop (and (<= vlen i) (exit nil))
		   (and (eq (vref v i) item) (exit i))
		   (setq i (/1+ i)))))))

(macro proclaim (body) ''proclaim)

(defun puthash (key hash val)
   (lets ((x (ass (car hash) key (cdr hash))))
    (cond (x (rplacd x val)) (t (nconc hash `((,key . ,val)))))
    val))

(defmacro pushnew (item loc)
   `(or (and (memq ,item ,loc) ,loc) (push ,item ,loc)))

(comment "kono remhash wa OOUSO de aru ")

(defun remhash (key hash)
   (lets ((x (ass (car hash) key (cdr hash))))
    (cond (x (remq x hash) t) (t nil))))

(macro svref (x) `(vref . ,x))

(defmacro the (type form) form)

(defmacro unless (cond . body) `(cond ((not ,cond) . ,body)))

;(putd 'values (getd 'list))
(macro values (x) `(list .,x))

;(putd 'values-list (getd 'cr))
(macro values-list (x) `(cr .,x))

(defmacro when (test . form) `(and ,test (progn . ,form)))

(defmacro while (cond . body) `(loop (or ,cond (exit)) . body))

(setq  nil)

(defun ubv-handler (x (y))
   (cond ((= (sref x 0) (character ":")) (set x x))
         (t (err:unbound-variable x y))))

(setq err:unbound-variable (function ubv-handler))
