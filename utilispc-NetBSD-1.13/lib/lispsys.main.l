(putd 'defun '(macro lambda (_x) (list 'putd (list 'quote (car _x))
	(list 'function (cons 'lambda (cdr _x)))))) 
(putd 'macro '(macro lambda (_x) (list 'putd (list 'quote (car _x))
	(list 'function (cons 'macro (cons 'lambda (cdr _x)))))))
(defun expt (x y)
  (cond ((zerop y) 1)
	((and (integerp y)(not (minusp y)))
	  (let ((z 1)) 
             (loop (and (oddp y) (setq z (times z x)))
		   (setq y (quotient y 2))
		   (and (zerop y) (exit))
		   (setq x (times x x)))
	     z))
	 (t (^$ (float x) (float y)))))
(defun readmacro (char fn (rt readtable)(mt macrotable))
  (let ((c (character char)))
    (vset rt c (logor (vref rt c) 512))
    (vset mt c fn)))
(macro defmacro (l)
   (list 'macro
         (car l)
         '(@)
         (list 'match
               '@
               (cdr l)
               (list '@
                     (list 'funcall
                           'err:argument-type
                           '@
                           (list 'quote (car l)))))))
(defmacro declare (l type)
   (or (memq type
             '(special redefine fix-result fix-value nogc
	       string-result fixnum vector string lppeval))
       (funcall err:argument-type type 'declare))
   (list 'lets
         (list (list 'type (list 'quote type)))
         (list 'mapc
               (list 'quote l)
               '(function (lambda (x) (putprop x t type))))))
(declare (system_lib_path lispfile print-result package-obvector) special)
(declare (print-result package-obvector) special)
(declare (standard-input standard-output macrotable) special)
(declare (err:argument-type err:number-of-arguments) special)
(declare (toplevel prompt) special)
(defun exfile (lispfile (print-result) (readtable default-readtable) 
               (macrotable default-macrotable))
   (catch 'exfile (exfile* lispfile)))
(defun exfile* (lispfile)
   (lets ((standard-input (inopen (stream (alloc lispfile)))) 
          (err:end-of-file (function (lambda (stg (dum)) (throw 'exfile)))))
    (cond (print-result (loop (print (eval (read)))))
          (t (loop (eval (read)))))))
(putd 'include-file (getd 'exfile))
(defmacro package-load (lispfile . externals)
   (list 'package-load* lispfile (list 'quote externals)))
(defun package-load* (lispfile externals)
   (lets ((package-obvector (vector 1023)) 
          (intern (function
                   (lambda (str)
                    (car (or (intern-soft str obvector)
                             (ncons (intern str package-obvector))))))))
    (loadfile lispfile)))
(defun oblist ((v obvector))
   (do ((i 0 (/1+ i))
        (result nil (append (vref v i) result))
        (l (vector-length v)))
       ((eq i l) result)))
(defun remob (x (v obvector))
   (lets ((n (\ (hash x) (vector-length v))))
    (vset v n (delq x (vref v n)))
    nil))
; copy from readmacro.l 1991/3/28
(defun readsharp nil
  (let ((c (tyipeek)))
    (cond ((= c (character "'")) (tyi) (list 'function (read)))
	  ((= c (character "(")) (setq c (read)) (vector (length c) c))
	  ((= c (character "!"))(readline)(read))
	  ((or (= c (character "x")) (= c (character "X"))) (tyi) (readfix 16))
	  ((or (= c (character "o")) (= c (character "O"))) (tyi) (readfix 8))
	  ((or (= c (character "b")) (= c (character "B"))) (tyi) (readfix 2))
	  ((= c (character "\")) (tyi) (readchar))
	  ((0< (logand (vref readtable c) 32768)) '#)
	  ((symbolp (setq c (read))) (intern (string-append "#" c)))
	  (t c)))))

(defun readfix (base)
 (lets ((res 0) (c (tyipeek)))
  (cond ((/0< (logand (vref readtable c) 32768)) 0))
  (setq res (readfix1 c base))
  (tyi)
  (cond ((>= res (// base 2)) (setq res (/- res base))))
  (do ((c (tyipeek) (tyipeek)))
      ((/0< (logand (vref readtable c) 32768)) res)
      (tyi)
      (setq res (plus (times res base) (readfix1 c base))))))

(defun readfix1 (c base)
 (cond ((<= 0 (/- c (character "0")) (min (/1- base) 9))
        (/- c (character "0")))
       ((and (<= (character "a") c (character "z")) (= base 16))
        (/+ c 10 (/- (character "a"))))
       ((and (<= (character "A") c (character "Z")) (= base 16))
        (/+ c 10 (/- (character "A"))))
       (t 0)))

;hex-imageを(negative) bignum対応にしたもの. 

;いままでのhex-imageをhex-image-subにし, hex-image を新設. 第1引数はimageをとる
;数. 第2引数は non nullなら8桁ずつに空白をいれる.   
;ex:
;> (hex-image (expt 2 100))
;"000010000000000000000000000000"
;> (hex-image (expt 2 100) t)
;"10 00000000 00000000 00000000"

(defun hex-image (n (format)) 
 (lets ((d 16777216) (s0 -8388608) (s1 8388608) (str0 "") (str1
  (cond ((minusp n)
         (cond ((not (lessp n s0)) (hex-image-sub n))
                         (t (string-append
                               (hex-image (sub1 (quotient (add1 n) d)))
                               (hex-image-sub
                                  (plus (sub1 (remainder (add1 n) d)) d))))))
        (t (cond ((lessp n s1) (hex-image-sub n))
                           (t (string-append
                                 (hex-image (quotient n d))
                                 (hex-image-sub (remainder n d))))))))
        (leng0 (string-length str1)) (leng1))
  (cond (format (cond ((<= (sref str1 0) 56)
         (setq leng1 (string-search-not-char "0" str1))
         (and (null leng1) (setq leng1 5))
         (and (string-search-char (string (sref str1 leng1)) "89abcdef")
              (setq leng1 (/1- leng1))))
        (t (setq leng1 (string-search-not-char "f" str1))
           (and (string-search-char (string (sref str1 leng1)) "01234567")
                (setq leng1 (/1- leng1)))))
  (setq leng0 (/- leng0 leng1))
  (setq str1 (substring str1 leng1))
  (loop (and (<= leng0 8) (exit (string-append str1 str0)))
        (setq leng0 (/- leng0 8))
        (setq str0 (string-append " " (substring str1 leng0) str0))
        (setq str1 (substring str1 0 leng0))))
  (t str1))))

(defun hex-image-char (n) (sref "0123456789abcdef" n))
(defun hex-image-sub (n)
   (lets ((s (make-string 6 "0")))
    (do ((i 5 (/- i 1)) (n n (logshift n -4)))
        ((eq i -1) s)
        (sset s i (hex-image-char (logand n 15))))))

;このdeclareはいるだろうか. (hex-valueもbignum対応にした. (readfix 16)とは違い,
;先頭の文字が8以上でも負数にはならない. 
;ex:
;> (hex-value "ffffffffffff")
;281474976710655
;> (readfix 16)ffffffffffff
;-1

(declare (lower-case) special)  ;必要か. hex-value内の16進10進変換は変更した. 

(defun hex-value (s)
   (lets ((x 0) (n (string-length s)))
    (do ((i 0 (/1+ i)))
        ((eq i n) x)
        (setq x (plus (times x 16)
                      (// (string-search-char
                          (sref s i)
                          "00112233445566778899aAbBcCdDeEfF") 2))))))

(defun readchar nil
  (lets ((x (read)))
    (cond ((not (symbolp x)) x)
	  ((= (string-length x) 1) (character (pname x)))
	  ((eq x 'Backspace) 8)
	  ((eq x 'Tab) 9)
	  ((eq x 'Linefeed) 10)
	  ((eq x 'Page) 12)
	  ((eq x 'Return) 13)
	  ((eq x 'Space) 32)
	  ((eq x 'Rubout) 127))))

(readmacro (character "#") (function readsharp))

(defun convert-back-quote (x)
   (cond ((null x) nil)
         ((atom x) (list 'quote x))
         ((eq (car x) '/,) (cdr x))
         ((eq (car x) '/,@) x)
         (t (do ((const t)
		 (splice nil)
                 (l x (cdr l))
                 (res nil
                      (cond (const
			     (match (convert-back-quote (car l))
                               ('nil (cons 'nil res))
                               (('quote x) (cons x res))
                               (('/,@ . x)
                                (setq const nil splice t)
                                (cons x 
				      (mapcar res 
					(function 
					 (lambda (x) (list 'quote (ncons x)))))))
                               (x (setq const nil) 
                                  (cons x 
                                        (mapcar res 
					  (function 
					   (lambda (x) (list 'quote x))))))))
			    (splice
			     (match (convert-back-quote (car l))
			       ('nil (cons (ncons nil) res))
			       (('quote x) (cons (list 'quote (ncons x)) res))
			       (('/,@ . x) (cons x res))
			       (x (cons (list 'list x) res))))
                            (t
			     (match (convert-back-quote (car l))
			       (('/,@ . x)
				(setq splice t)
				(cons x
				      (mapcar res 
					(function 
					 (lambda (x) 
					   (cond ((and (consp x) 
						       (eq (car x) 'quote))
						  (list 'quote (ncons (second x))))
						 (t (list 'list x))))))))
			       (x (cons x res)))))))
	      ((or (atom l) (eq (car l) '/,))
	       (cond ((null l)
		      (cond (const (list 'quote (nreverse res)))
			    (splice (cons 'append (nreverse res)))
			    (t (cons 'list (nreverse res)))))
		     (const (match (convert-back-quote l)
			      (('quote x)
			       (list 'quote (nconc (nreverse res) x)))
			      (x (do 
                                     ((l res (cdr l))
                                      (res x 
					   (list 'cons (list 'quote (car l)) 
						 res)))
				   ((null l) res)))))
		     (splice (cons 'append
				   (nreverse
				    (cons (convert-back-quote l) res))))
		     (t (do ((l res (cdr l))
			     (res (convert-back-quote l)
				  (cond ((and (consp res) 
					      (eq (car res) 'quote) 
					      (null (cddr res)) (consp (car l)) 
					      (eq (caar l) 'quote) 
					      (null (cddr (car l))))
					 (list 'quote 
					       (cons (second (car l)) 
						     (second res))))
					(t (list 'cons (car l) res)))))
			  ((null l) res)))))))))

(defun set-backquote-character (char)
  (readmacro
   (character char)
   (function (lambda nil (convert-back-quote (read))))))

(set-backquote-character "`")

(defun set-unquote-character (char)
  (readmacro
   (character char)
   (function
    (lambda ((ch (tyipeek)))
      (cons (cond ((= ch (character "@")) (tyi) '/,@) (t '/,)) (read))))))

(set-unquote-character ",")
;
(macro format (l)
   (lets ((str (car l)) (args (cdr l)) (leng (string-length str)) (obj))
    (do ((n 0 (/+ m 2))
         (m (string-search-char "/" str 0)
            (string-search-char "/" str (/+ m 2))))
        ((null m)
         (cons 'progn
               (nreverse
                  (cons 'nil
                        (cond ((eq n leng) obj)
                              (t (cons (list 'princ (substring str n)) 
                                  obj)))))))
        (or (eq m n) (push (list 'princ (substring str n m)) obj))
        (push (selectq (getchar str (/1+ m))
                 ((n N) '(terpri))
                 ((s S)
                  (list 'prin1
                        (cond ((atom args)
                               (funcall err:number-of-arguments nil 'format))
                              (t (pop args)))))
                 ((c C)
                  (list 'princ
                        (cond ((atom args)
                               (funcall err:number-of-arguments nil 'format))
                              (t (pop args)))))
                 ((i g I G)
                  (list 'prind
                        (cond ((atom args)
                               (funcall err:number-of-arguments nil 'format))
                              (t (pop args)))))
                 (// '(princ "/"))
                 ((t T)
                  (list 'tab
                        (cond ((atom args)
                               (funcall err:number-of-arguments nil 'format))
                              (t (pop args)))))
                 ((b B) (list 'tyo (pop args)))
                 (t (funcall err:argument-type (getchar str n) 'format)
                    (exit)))
              obj))))
(macro do (l)
   (list 'let
         (mapcar (car l)
            (function
             (lambda (x) (list (first x) (and (listp (cdr x)) (second x))))))
         (list 'loop
               (cons 'and
                     (and (listp (cadr l))
                          (list (caadr l) (cons 'exit (cdadr l)))))
               (cons 'progn (cddr l))
               (cons 'setq
                     (mapcan (car l)
                        (function
                         (lambda (x)
                          (and (listp (cdr x))
                               (listp (cddr x))
                               (list (first x) (third x))))))))))
(macro do* (l)
   `(lets ,(mapcar (car l)
              (function
               (lambda (x) `(,(first x) ,(and (listp (cdr x)) (second x))))))
     (loop (and . ,(and (listp (cadr l)) `(,(caadr l) (exit . ,(cdadr l)))))
            . ,(append (cddr l)
           (mapcan (car l)
               (function
                (lambda (x)
                 (and (listp (cdr x))
                      (listp (cddr x))
                      `((setq ,(first x) ,(third x)))))))))))
(putd 'dos (getd 'do*))
(defmacro defprop (x y z)
   (list 'putprop (list 'quote x) (list 'quote y) (list 'quote z)))
(defmacro defnprop (x y z)
   (list 'putprop (list 'quote x) (list 'function y) (list 'quote z)))
(defmacro incr (x n) (list 'setq x (list '/+ x n)))
(defmacro decr (x n) (list 'setq x (list '/- x n)))
(defmacro setl (where what)
   (match where
      ((fn x)
       (selectq fn
          ((car caar cadr caaar caadr cadar caddr first second third fourth)
           (list 'rplaca
                 (list (cdr (assq fn
                               '((car . cr) (caar . car) (cadr . cdr) 
                                 (caaar . caar) (caadr . cadr) 
                                 (cadar . cdar) (caddr . cddr) (first . cr) 
                                 (second . cdr) (third . cddr) 
                                 (fourth . cdddr))))
                       x)
                 what))
          ((cdr cddr cdar cdddr cddar cdadr cdaar)
           (list 'rplacd
                 (list (cdr (assq fn
                               '((cdr . cr) (cddr . cdr) (cdar . car) 
                                 (cdddr . cddr) (cddar . cdar) 
                                 (cdadr . cadr) (cdaar . caar))))
                       x)
                 what))
          ((deref plist getd)
           (list (cdr (assq fn
                         '((deref . setref) (plist . setplist) 
                           (getd . putd))))
                 x
                 what))
          (t (cond ((and (symbolp fn)
                         (definedp fn)
                         (consp (getd fn))
                         (eq (car (getd fn)) 'macro))
                    (list 'setl (funcall (cdr (getd fn)) (list x)) what))
                   (t (funcall err:argument-type where 'setl))))))
      ((fn x y)
       (selectq fn
          ((vref sref bref)
           (list (cdr (assq fn
                         '((vref . vset) (sref . sset) (bref . bset))))
                 x
                 y
                 what))
          (nth (list 'rplaca (list 'nthcdr x y) what))
          (get (list 'putprop x what y))
          (t (cond ((and (symbolp fn)
                         (definedp fn)
                         (consp (getd fn))
                         (eq (car (getd fn)) 'macro))
                    (list 'setl (funcall (cdr (getd fn)) (list x y)) what))
                   (t (funcall err:argument-type where 'setl))))))
      ((fn . args) 
       (cond ((and (symbolp fn)
                   (definedp fn)
                   (consp (getd fn))
                   (eq (car (getd fn)) 'macro))
              (list 'setl (funcall (cdr (getd fn)) args) what))
             (t (funcall err:argument-type where 'setl))))
      (where (cond ((symbolp where) (list 'setq where what))
                   (t (funcall err:argument-type where 'setl))))))
(putd 'setf (getd 'setl))
(setq usebq nil)
(macro exm (l) (list (cdr (getd (first l))) (list 'quote (cdr l))))
(macro macro-expand (l) (list (cdr (getd (first l))) (list 'quote (cdr l))))
(putd 're (getd 'string-match))
(declare (pred) special)
(defun filter (l pred)
   (mapcan l
      (function (lambda (x) (cond ((funcall pred x) (list x)) (t nil))))))
(defun symbol-lessp (x y) (string-lessp (pname x) (pname y)))
(defun symbol-greaterp (x y) (string-lessp (pname y) (pname x)))
(defun string-hex-image (str)
   (lets ((size (string-length str)) (result (make-string (* 2 size) 0)))
    (do ((i 0 (add1 i)) (j 0 (add1 j)))
        ((= i size) result)
        (sset result j (hex-image-char (quotient (sref str i) 16)))
        (sset result
              (incr j 1)
              (hex-image-char (remainder (sref str i) 16))))))
(defun string-hex-value (s)
   (lets ((n (string-length s)) (r (make-string (// n 2) 0)))
    (do ((i 0 (/+ i 2)) (j 0 (/1+ j)))
        ((eq i n) r)
        (sset r j (hex-value (substring s i (/+ i 2)))))))
(defun string-change (text pat1 pat2 (count (string-length text)))
   (lets ((s 0) (result) (i) (pat1-length (string-length pat1)))
    (loop (cond ((<= count 0) (push (substring text s) result) (exit)))
          (setq i (string-search pat1 text s))
          (cond ((null i) (push (substring text s) result) (exit)))
          (push (substring text s i) result)
          (push pat2 result)
          (setq s (/+ i pat1-length))
          (setq count (/1- count)))
    (eval (cons 'string-append (reverse result)))))
(defun string-trim (str (chars " "))
   (string-right-trim (string-left-trim str chars) chars))
(defun string-both-trim (str (chars " "))
   (string-right-trim (string-left-trim str chars) chars))
(defun string-right-trim (str (chars " "))
   (lets ((i (string-search-not-char chars (setq str (string-reverse str)))))
    (cond ((null i) (string-append ""))
          (t (string-reverse (substring str i))))))
(defun string-left-trim (str (chars " "))
   (lets ((i (string-search-not-char chars (setq str str))))
    (cond ((null i) (string-append "")) (t (substring str i)))))
(defun left-adjust (s n (pud " "))
   (substring (string-append s (make-string n pud)) 0 n))
(defun right-adjust (s n (pud " "))
   (lets ((str (string-append (make-string n pud) s)) 
          (size (string-length str)))
    (substring str (/- size n))))
(defun number-image (n)
 (lets ((sign (cond ((minusp n) (setq n (minus n)) "-") (t ""))) (result))
  (loop (push (string (/+ (character "0") (remainder n 10))) result)
        (cond ((zerop (setq n (quotient n 10)))
               (exit (apply 'string-append (cons sign result))))))))
(defun number-value (s)
 (lets ((sign (cond ((and (/0< (string-length s)) (= (sref s 0) 45))
                     (setq s (substring s 1))
                     t)
                    (t nil)))
        (size (string-length s)) (x 0) (i 0))
  (loop (and (eq i size) (exit (cond (sign (minus x)) (t x))))
        (or (<= (character "0") (sref s i) (character "9")) (exit))
        (setq x (plus (times x 10) (/- (sref s i) (character "0"))))
        (incr i 1))))
(defun push-toplevel (fn)
   (putprop 'toplevel (cons toplevel (get 'toplevel ':toplevel)) ':toplevel)
   (setq toplevel fn))
(defun pop-toplevel nil
   (lets ((s (get 'toplevel ':toplevel)))
    (cond (s (setq toplevel (first s)) (putprop 'toplevel (cdr s) ':toplevel))
          (t (setq toplevel 'utilisp)))
    toplevel))
(remob ':toplevel)
(defmacro autoload (funcs file load . externals)
   (cons 'progn
         (mapcar funcs
            (function
             (lambda (func)
              (list 'macro
                    func
                    '(l)
                    (cons load (cons file externals))
                    (list 'cons (list 'quote func) 'l)))))))
(defun prind (x (leng (linelength)) (asblock) (plev) (plen))
   (package-load (string-append system_lib_path "lisplib/prind")
		  *prind printable *block)
   (funcall 'prind x leng asblock plev plen))
(putd 'grind (getd 'prind))
(defmacro pp (fn)
   (and (definedp fn)
        (lets ((f (getd fn)))
         (cond ((consp f)
                (match f
                   (('lambda . x) `(prind '(defun ,fn . ,x)))
                   (('macro 'lambda ('@) ('match '@ x . y))
                    `(prind '(defmacro ,fn . ,x)))
                   (('macro 'lambda . x) `(prind '(macro ,fn . ,x)))
		   (('macro . x) `(and (codep ',x) (prind ',f)))))
               ((codep f) `(prind ',f))))))
(defun rind ((prompt prompt))
   (package-load (string-append system_lib_path "lisplib/rind"))
   (funcall 'rind prompt))
(defun sort (table <)
   (package-load (string-append system_lib_path "lisplib/sort"))
   (funcall 'sort table <))
(defun whatis (fname)
   (package-load (string-append system_lib_path "lisplib/whatis")
		  previous-definition function-description)
   (funcall 'whatis fname))
(autoload (trace trace-when untrace)
	(string-append system_lib_path "lisplib/trace") package-load)
(autoload (program-load)
	  (string-append system_lib_path "lisplib/progload") loadfile)
(autoload (compile compfile revert reset-compilation-flags c:lambda make-code)
   (string-append system_lib_path "lispcomp/comprc")
   package-load
   typecheck ubvcheck indexcheck udfcheck low-level-optimize
   previous-definition compiled-code function-description
   eof readfile)
(autoload (ed edf edv edp) (string-append system_lib_path "lisplib/newed")
			 loadfile)
(autoload (edit) (string-append system_lib_path "lisplib/amuse") loadfile)
(autoload (stingy) (string-append system_lib_path "lisplib/stingy")
			package-load)
(autoload (prolog) (string-append system_lib_path "lisplib/pkr") loadfile)
(defun call (str) (systemcall str))
(defun cd ((str (getenv "HOME"))) (systemchdir str))
(defun getenv (str) (systemgetenv str))
(defun putenv (str val) (systemputenv str val))
(defun userid () (getenv "USER"))
(defun date-time nil
   (lets ((vec (localtime (vector 9))))
    (string-append
       (date-time* (vref vec 5))
       (date-time* (/1+ (vref vec 4)))
       (date-time* (vref vec 3))
       (date-time* (vref vec 2))
       (date-time* (vref vec 1))
       (date-time* (vref vec 0)))))
(defun date-time* (n)
   (lets ((result (make-string 2)))
    (sset result 0 (/+ (character "0") (\ (// n 10) 10)))
    (sset result 1 (/+ (character "0") (\ n 10)))
    result))
(defun make-character-conversion-table (from to disp)
   (do ((table (make-string 256)) (i 0 (/1+ i)))
       ((>= i 256) table)
       (sset table i (cond ((>= to i from) (/+ i disp)) (t i)))))
(setq lower-case
   (make-character-conversion-table
      (character "A")
      (character "Z")
      (/- (character "a") (character "A"))))
(setq upper-case
   (make-character-conversion-table
      (character "a")
      (character "z")
      (/- (character "A") (character "a"))))
(defmacro defconst (var val)
  `(progn (putprop ',var ,val 'constant) (setq ,var ,val)))

; Process command options of utilisp.
;	Before entering this file (/usr/lib/lispsys.main.l),
;	the following options are processed by the interpreter.
;
;	-h (heapsize)
;	-f (compiled code area size)
(declare (argv extendheap-ratio) special)
(setq argv (argv))
(setq extendheap-ratio 90)
(setq gc-hook 
      (function 
       (lambda (size)
	 (lets ((hs (heapsizeK)) 
		(hu (- (+ hs (// size 1024) 1) (heapfreeK)))
		(hs100 (// hs 100)))
	   (and (< (* hs100 extendheap-ratio) hu)
		(loop 
		 (setq hs (* hs 2) hs100 (* hs100 2))
		 (and (>= (* hs100 extendheap-ratio) hu)
		      (princ "Heap extended to " terminal-output)
		      (print hs terminal-output)
		      (extendheapK hs)
                      (exit nil))))))))

(defun exstr (lispfile (readtable default-readtable) 
               (macrotable default-macrotable))
   (catch 'exstr (exstr* lispfile)))
(defun exstr* (lispfile)
   (lets ((standard-input (inopen (string-stream lispfile))) 
          (err:end-of-file (function (lambda (stg (dum)) (throw 'exstr)))))
    (loop (eval (read)))))
; "/tmp/soko.o" -> "soko"
(declare (c-chartable) special)
(setq c-chartable "________________________________________________0123456789_______ABCDEFGHIJKLMNOPQRSTUVWXYZ______abcdefghijklmnopqrstuvwxyz_____________________________________________________________________________________________________________________________________")
(defun make-base-name (name)
  (lets ((revstr (string-reverse name))
	 (len (string-length name))
	 (i (string-search-char "." revstr))
	 (to (cond (i (- len i 1))(t len)))
	 (j (string-search-char "/" revstr))
	 (from (cond (j (- len j))(t 0))))
    (substring name from to)))
; (code-load files)
;(defmacro code-load (x . y)
;  `(let ((xx ,x))
;     (cond ((consp xx)
;	    (lets ((codes (init-code-files xx))
;		   (ofile (car codes)))
;	      (apply 'system-code-load (nconc codes ',y))
;	      (call (string-append "rm " ofile))))
;	   (t (apply 'system-code-load (cons xx ',y))))))
;
;(defun init-code-files (files (initfunc))
;  (lets ((suffix (number-image (getpid)))
;	 (base (string-append "/tmp/uti" suffix))
;	 (cfile (string-append base ".c"))
;	 (cstream (outopen (stream cfile)))
;	 (ofile (string-append base ".o"))
;	 (ret nil))
;    (or initfunc (setq initfunc (string-append "init_code_uti" suffix)))
;    (princ initfunc cstream)
;    (princ "(fp){" cstream)
;    (do ((l files (cdr l)))
;      ((atom l))
;      (princ "init_code_" cstream)
;      (princ (translate (make-base-name (car l)) c-chartable) cstream)
;      (princ "(fp);" cstream)
;      (push (string (car l)) ret))
;    (princ "}" cstream)
;    (terpri cstream)
;    (close cstream)
;    (call (string-append "gcc -c -o " ofile " " cfile))
;    (cons ofile (nreverse ret))))
(defmacro code-load (x . y)
  `(let ((xx ,x))
     (cond ((consp xx)
	    (apply 'system-code-load (nconc (copy xx) ',y)))
	   (t (apply 'system-code-load (cons xx ',y))))))
;;
(defmacro make-a.out (aout x . y)
  (lets ((aa (eval aout))
	 (xx (append (init-code-files (eval x) "init_codes")
		     y 
		     `(,(string-append system_lib_path "libutilisp.a") "-lm")))
	 (filestr ""))
    (do ((l xx (cdr l)))
      ((atom l)
       `(call (string-append "gcc -static -o " ,aa " " ,filestr)))
     (setq filestr (string-append filestr (car l) " ")))))
; 97/10/31 Iwasaki
(defun inopenp (f)
   (lets ((err:open-close #'(lambda (x (y)) (throw 'inopenp nil)))
	  (s (stream f)))
    (cond ((catch 'inopenp (inopen s)) (close s) t)
	  (t nil))))
(defun loadfile (pre)
   (lets ((ofile (string-append pre ".o")))
    (cond ((inopenp ofile) (system-code-load ofile) nil)
	  (t (lets ((lfile (string-append pre ".l")))
	      (cond ((inopenp lfile) (exfile lfile) nil)
		    (t (funcall err:open-close (stream lfile) 'loadfile))))))))
;
(cond ((not (string-equal "utilisp" (car argv)))
       (setq utilisp_path (car argv))))
; Option "-n" : don't execute .lisprc
(or (member "-n" argv)
    (lets ((err:open-close (function (lambda (x (w)) (throw 'lisprc)))) 
           (lisprc (string-append (getenv "HOME") "/.utilisprc")))
     (catch 'lisprc (exfile lisprc))))

;
(lets ((f (member "-p" argv)))
  (and f (cdr f)
       (setq extendheap-ratio (number-value (cadr f)))))
; Option "-E" : execute string
(lets ((f (member "-E" argv)))
 (and f
      (cdr f)
      (lets ((file (cadr f))(argc (length (cddr f)))(argv (cddr f)))
       (exstr file)
	(flush standard-output)
	(quit))))
; Option "-F" : execute one initial file
; options list
; -n -M -N -F
; -p V -E V -h V -ls V -cs V -bs V -es V -m V -d V -gctype X

;
(setq argv (cdr argv))
(loop
 (and (atom argv)(exit nil))
 (lets 
    ((option1s '("n" "M" "N" "F"))
     (option2s '("p" "E" "h" "ls" "cs" "bs" "es" "m" "d" "gctype"))
     (arg (car argv)))
    (cond 
     ((and (0< (string-length arg))(equal (substring arg 0 1) "-"))
      (cond 
       ((member (substring arg 1)option1s)
	(setq argv (cdr argv)))
       ((member (substring arg 1)option2s)
	(cond ((consp (cdr argv))
	       (setq argv (cddr argv)))
	      (t (format "arg /c needs at least 1 argument/n" arg)(quit))))
       (t (format "unknown option /c/n" arg)
	  (quit))))
     (t
      (lets ((err:open-close 
	      (function (lambda (x (w)) 
			  (format "file /c is not found/n" arg)
			  (quit)))))
	    (setq argv (cdr argv))
	    (catch 'lisprc 
	      (lets ((err:end-of-file 
		      (function (lambda (x (w))(quit))))
		     (script-input (inopen (stream (alloc arg)))))
		    (loop (eval (read script-input))))))))))
