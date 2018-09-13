(declare (bnchtbl list10 list30 list50 list100 list1000 list10000
	  minbig xlist ylist zlist dum) special)

(setq bnchtbl
      '((1-1	(bench11 10))
	(1-2	(bench12 1)) 
	(1-3	(and (codep (getd 'tarai))(bench13 1)))
	(1-4	(bench14 1))
	(2-1	(bench21 1))
	(2-2	(bench22 100))
	(2-3	(bench23 10))
	(2-4	(bench24 100))
	(2-5	(bench25 100))
	(2-6	(bench26 10000))
	(2-7	(bench27 10000))
	(2-8	(bench28 10000))
	(2-9	(bench29 1000))
	(2-10	(bench210 100))
	(2-11	(bench211 10))
	(2-12	(bench212 10000))
	(2-13	(bench213 10000))
	(2-14	(bench214 1000))
	(2-15	(bench215 100))
	(3-1	(bench31 10))
	(4-1	(bench41 10))
	(4-2	(bench42 10))
	(5-1	(bench51 10))
	(6-1	(bench61 10))
	(7-1	(bench71 100))
	(7-2	(bench72 100))
	(7-3	(bench73 1000))
	(7-4	(bench74 100))
	(8-1	(bench81 100))
	(8-2	(bench82 1000))
	(8-3	(bench83 100))
	(8-4	(bench84 10))
	(8-5	(bench85 1))
	(8-6	(bench86 10000))
	(8-7	(bench87 1000))
	(8-8	(bench88 100))
	(8-9	(bench89 10))
	(9-1	(bench91 1))
	(9-2	(bench92 1))
	(9-3	(bench93 1))
	(9-4	(bench94 1))
	(9-5	(bench95 10))
	(9-6	(bench96 1))
	(9-7	(bench97 1))
	(9-8	(bench98 1))
	(9-9	(bench99 1))
	(10	(prolog 'prol))
	(11-1	(bench111 100))
	(11-2	(bench112 1))
	(12-1	(bench121 1))
	))

(defun	bench (x) (eval (cadr(assq x bnchtbl))))

(defun	mypr (x) (princ (fix x))
		 (princ	".")
		 (princ (fill (fix(times 100000 (difference x (fix x)))))))

(defun fill (x) (do ((i 0 (1+ i))
		     (result "" (string-append (string (+ 48(\ y 10)))result))
		     (y x (// y 10)))
		    ((eq i 5)result)))

(defmacro benchmark (n . body)
  `(lets((time1) (time2) (time3) (gctime1) (gctime2) (gctime3) (gc) (run))
     (setq time1 (time))
     (setq gctime1 (gctime))
     (do ((i 1 (1+ i)))
	 ((> i ,n))
         . ,body )
     (setq time2 (time))
     (setq gctime2 (gctime))
     (do ((i 1 (1+ i))) ((> i ,n)))
     (setq time3 (time))
     (setq gctime3 (gctime))
     (setq gc (difference gctime2 gctime1))
     (setq run (difference(plus time2 time2 gctime3)time1 time3 gctime2))
     (princ "total = ")
     (princ run)
     (princ ",  runtime = ")
     (princ (difference run gc))
     (princ ", gc = ")
     (princ gc)
     (princ ", for ")
     (princ ,n)
     (princ " iterations.")
     (terpri)
     (princ "average=")
     (mypr  (quotient (difference run gc) ,n 0.06))
     (princ " ms")
     (terpri)
     ))

(defmacro benchmark2 (n com . body)
  `(lets((time1) (time2) (time3) (gctime1) (gctime2) (gctime3) (gc) (run))
     (setq time1 (time))
     (setq gctime1 (gctime))
     (do ((i 1 (1+ i)))
	 ((> i ,n))
         ,com . ,body )
     (setq time2 (time))
     (setq gctime2 (gctime))
     (do ((i 1 (1+ i))) ((> i ,n)) ,com)
     (setq time3 (time))
     (setq gctime3 (gctime))
     (setq gc (difference gctime2 gctime1))
     (setq run (difference(plus time2 time2 gctime3)time1 time3 gctime2))
     (princ "total = ")
     (princ run)
     (princ ",  runtime = ")
     (princ (difference run gc))
     (princ ", gc = ")
     (princ gc)
     (princ ", for ")
     (princ ,n)
     (princ " iterations.")
     (terpri)
     (princ "average=")
     (mypr  (quotient (difference run gc) ,n 0.06))
     (princ " ms")
     (terpri)
     ))
;
; [1] function call/return
;

; **** Tarai ****
;      where number-manipulations may be replaced
;      by those restricted to integer if available

(defun tarai (x y z)
   (cond ((> x y)
          (tarai (tarai (1- x) y z)
                 (tarai (1- y) z x)
                 (tarai (1- z) x y) ))
         (t y) ))

(defun tak (x y z)
   (cond ((not (< y x)) z)
        (t (tak (tak (1- x) y z)
                (tak (1- y) z x)
                (tak (1- z) x y) ))))

; measure the following forms:
; << 1-1 >>
; (tarai 8 4 0)		; tarai is called 12605 times.
			;    with 9453 sub1's and 9454 else parts.

; << 1-2 >>
; (tarai 10 5 0)	; tarai is called 343073 times.
			;    with 257304 sub1's.
			;    with 343073 >'s  (t 85768 nil 257305)

; << 1-3 >>     **** try only the compiled code! ****
; (tarai 12 6 0)	; tarai is called 12604861 times.
			;    with 9453645 sub1's.

; << 1-4 >>
; (tak 18 12 6)		; tak is called 63609 times.
			;    in honor of usa lisp comunity


(defun bench11 (iter) (benchmark iter (tarai 8 4 0)))
(defun bench12 (iter) (benchmark iter (tarai 10 5 0)))
(defun bench13 (iter) (benchmark 1 (tarai 12 6 0)))
(defun bench14 (iter) (benchmark iter (tak 18 12 6)))

;
; [2] list manipulation
;

; **** tarai with list ****

(defun list-tarai (x y z)
  (cond ((lessp (car x) (car y))
         (list-tarai (list-tarai (copytree (cdr x)) y z)
                     (list-tarai (copytree (cdr y)) z x)
                     (list-tarai (copytree (cdr z)) x y) ))
        (t y) ))

; copytree copies all level of conses in the argument.

(defun copytree (x) (subst nil nil x))

; << 2-1 >>	measure the following form: 
;
; (list-tarai '(1 2 3 4 5 6 7 8 9 10) '(5 6 7 8 9 10) '(9 10))
;
;        analogous to (tarai 8 4 0)
;        list-tarai is called 12605 times.
;        with 9453 copy's and 25210 car's.

(defun bench21 (iter)
  (benchmark iter
	     (list-tarai '(1 2 3 4 5 6 7 8 9 10)
			 '(5 6 7 8 9 10) '(9 10)) ))

; **** slow reverse ****
;         written by hideyuki nakashima and satoru tomura

(defun srev (x)
  (cond ((null x) nil)
        (t (sapp (srev (cdr x)) (ncons (car x)))) ))

(defun sapp (x y)
  (cond ((null x) y)
        (t (sapp (srev (cdr (srev x)))
                 (cons (car (srev x)) y) ))))

; << 2-2 >>

(defun bench22 (iter) (benchmark iter (srev '(1 2 3 4 5))))

; << 2-3 >>

(defun bench23 (iter) (benchmark iter (srev '(1 2 3 4 5 6))))

; **** quicksort ****

(defun qsort (l r)
  (cond ((null l) r)
        (t ((lambda (p)
              (qsort (car p)
                     (cons (car l) (qsort (cdr p) r) )))
            (partition (cdr l) (car l)) ))))

(defun partition (l x)
  (cond ((null l) (cons nil nil))
        (t ((lambda (p)
              (cond ((lessp (car l) x)
                     (cons (cons (car l) (car p)) (cdr p)) )
                    (t (cons (car p) (cons (car l) (cdr p))))) )
            (partition (cdr l) x) ))))

; ---- common data definition ----

(defun index (start end step)
  (cond ((0= step))
	((= step  1)(pindex start end))
	(t (do ((i start (+ i step))(result nil (nconc result (list i))))
          	((or(and (0> step) (< i end))
                    (and (0< step) (> i end)))
  	        result)))))

(defun pindex (start end)
	(do ((i end (1- i))(result nil (cons i result)))
	    ((< i start)result)))
 
(setq list10 '(1 2 3 4 5 6 7 8 9 10))

(setq list30 '( 1  2  3  4  5  6  7  8  9 10
               11 12 13 14 15 16 17 18 19 20
               21 22 23 24 25 26 27 28 29 30 ))

(setq list50 '(27 74 17 33 94 18 46 83 65 2
               32 53 28 85 99 47 28 82  6 11
               55 29 39 81 90 37 10  0 66 51
                7 21 85 27 31 63 75  4 95 99
               11 28 61 74 18 92 40 53 59  8 ))

(setq list100 (index 1 100 1))

(setq list1000 (index 1 1000 1))

(setq list10000 (index 1 10000 1))

; << 2-4 >> sort a list of 50 element.
;             compare the performance of quicksort in prolog.

(defun bench24 (iter) (benchmark iter (qsort list50 ())))

; << 2-5 >> **** naive reverse to compare with prolog ****

(defun napp (x y)
  (cond ((consp x)
         (cons (car x) (napp (cdr x) y)) )
        (t y) ))

(defun nrev (x)
  (cond ((consp x)
         (napp (nrev (cdr x)) (list (car x))) )
        (t nil) ))

(defun bench25 (iter) (benchmark iter (nrev list30)))

; << 2-6 >> **** built-in (non-/destructive) reverse ****

; use the built-in destructive reverse function.
;  if it is not provided by the system, use the following function:
;
;(defun nreverse (l)
;  (cond ((null l) nil)
;        (t (do ((current (cdr l)) (previous l) next)
;               ((null current) (rplacd l nil) previous)
;             (setq next (cdr current))
;             (rplacd current previous)
;             (setq previous current current next) ))))
;
;  if built-in reverse is not available, use the following definition:
;
;(defun reverse (l)
;  (prog (x)
;   loop (cond ((null l) (return x)))
;        (setq x (cons (car l) x))
;        (setq l (cdr l))
;        (go loop) ))

(defun bench26 (iter) (benchmark iter (reverse list30)))

(defun bench27 (iter) (benchmark2 iter (setq dum (copy list30)) (nreverse dum)))

(defun bench28 (iter) (benchmark iter (reverse list10)))

(defun bench29 (iter) (benchmark iter (reverse list100)))

(defun bench210 (iter) (benchmark iter (reverse list1000)))

(defun bench211 (iter) (benchmark iter (reverse list10000)))

(defun bench212 (iter) (benchmark2 iter (setq dum (copy list10))(nreverse dum)))

(defun bench213 (iter) (benchmark2 iter (setq dum(copy list100))(nreverse dum)))

(defun bench214 (iter) (benchmark2 iter(setq dum(copy list1000))(nreverse dum)))

(defun bench215 (iter)(benchmark2 iter(setq dum(copy list10000))(nreverse dum)))

;
; [3] string manipulation
;

; **** tarai with string ****

(defun string-tarai (x y z)
  (cond ((string-lessp
	  (substring x 0 1) (substring y 0 1))
         (string-tarai (string-tarai (substring x 1) y z)
                       (string-tarai (substring y 1) z x)
                       (string-tarai (substring z 1) x y) ))
        (t y) ))

	; where substring must create new string header and body.
	;    (substring string 0 1) = (shead string)
	;    (substring string 1)   = (stail string).

; << 3-1 >>

; (string-tarai "abcdefghij" "efghij" "ij")
                ; where string-tarai is called 12605 times.
                ; stail is called 9453 times.

(defun bench31 (iter)
  (benchmark iter (string-tarai "abcdefghij" "efghij" "ij")) )

;
; [4] number consing
;

; **** tarai with number ****

(defun flo-tarai (x y z)
   (cond ((>$ x y)
          (flo-tarai (flo-tarai (-$ x 0.11) y z)
		     (flo-tarai (-$ y 0.11) z x)
		     (flo-tarai (-$ z 0.11) x y) ))
         (t y) ))

; << 4-1 >>	flonum

; (flo-tarai 3.8 3.4 3.0)

	; this complexity is equivalent to that of tarai-4.
	; confirm that flo-tarai is called 12605 times.
	; if not, change the decrement (0.11) to an appropriate value.
	; number-manipulations may be replaced
	; by those restricted to flonum if available

; << 4-2 >>	bignum

; (tarai big-x big-y big-z)

(defun big-tarai (x y z)
   (cond ((greaterp x y)
          (big-tarai (big-tarai (sub1 x) y z)
                     (big-tarai (sub1 y) z x)
                     (big-tarai (sub1 z) x y) ))
          (t y) ))

	; this complexity is equivalent to that of tarai-4.
	; where big-z should be greater than the least positive
	; bignum, big-x = big-z + 8, and big-y = big-z + 4.


(defun bench41 (iter) (benchmark iter (flo-tarai 3.8 3.4 3.0)))
(setq  minbig (expt 2 27))
(defun bench42 (iter)
  (benchmark iter (big-tarai (plus 8 minbig)(plus 4 minbig)minbig)))

;
; [5] array manipulation
;

; **** bubble sort ****
; read data into array, sort data and return a list of sorted data.

; data is an array of size 50

(defun bubblesort (ls)
  (prog(top (len(length ls))(bound(1- len))(data(fill-vector(vector len)ls))x)
   loop (setq top 0)
	(do ((j 0 (1+ j)))((= j bound))
            (cond ((> (setq x (vref data j))(vref data (1+ j)))
			(vset data j (vref data (1+ j)))
			(vset data (1+ j) x)
                        (setq top j) )))
        (cond ((zerop top)
               (return
		 (do ((j 0 (1+ j))(x nil (append x (list (vref data j)))))
		     ((= j len) x)))))
        (setq bound top)
        (go loop) ))

; << 5-1 >>	sort 50 elements by bubble sort.

(defun bench51 (iter)
  (benchmark iter (bubblesort list50)) )

;
; [6] property list
;

; **** generate sequence ****

(defun sequence (n)
  (cond ((get 'seq-table n) (get 'seq-table n))
        (t (putprop 'seq-table
              (- (sequence (sub1 n)) (sequence (- n 2)))
              n )
           (get 'seq-table (get 'seq-table n)) )))

; ---- initialization of property list of sequence ----
;
;(setplist 'seq-table '(-1 1 0 1 1  1))

; << 6-1 >> calculate 100th number

(defun bench61 (iter) (benchmark iter (setplist 'seq-table '(-1 1 0 1 1 1))
				      (sequence 100)))

;
; [7] mapping function and non-local variable access
;

; **** bita ****

(defun bita (a)
  (cond ((null (cdr a)) a)
        ((null (cddr a)) (list (cons (car a) (cons '$ (cdr a)))))
        (t (bitl (cdr a) (list (car a)))) ))

(defun bitl (x j)
  (cond ((null x) nil)
        (t (nconc
            (mapcan
	     (bita x)
               (function
                (lambda (k)
                  (mapcar
                   (bita j)
		     (function
                      (lambda (l)
                        (list l '$ k) ))))))
            (bitl (cdr x) (append j (list (car x)))) ))))

; **** bitb ****

(declare (aa) special)

(defun bitb (aa)                 ; aa is non-local.
  (cond ((null aa) nil)
        ((null (cdr aa)) aa)
        (t ((lambda (c)
              (setq aa (list (car aa) '$ (cadr aa)))
              (mapcon
               (bitb (cdr c))
	 	(function (lambda (b) (g (car b))))) )
            aa )
           )))

(defun g (b)
  (cond ((atom b) (list aa))     ; aa is defined in bitb
        (t (cons (list (car aa) '$ b)
                 (mapcar
                  (g (car b))
		  (function (lambda (aa) (cons aa (cdr b))))
                   )))))


; << 7-1 >>	bita-5

(defun bench71 (iter) (benchmark iter (bita '(a b c d e))))

; << 7-2 >>	bita-6

(defun bench72 (iter) (benchmark iter (bita '(a b c d e f))))

; << 7-3 >>	bitb-5

(defun bench73 (iter) (benchmark iter (bitb '(a b c d e))))

; << 7-4 >>	bitb-6

(defun bench74 (iter) (benchmark iter (bitb '(a b c d e f))))

;
; [8] sort
;

; **** measure the performance of a built-in function sort ****

; << 8-1 >>

(defun bench81 (iter) (benchmark iter (sort (append list50 nil)(function <))))

; << 8-2 >>

(defun bench82 (iter) (benchmark iter (sort (append list10 nil)(function <))))

; << 8-3 >>

(defun bench83 (iter) (benchmark iter (sort (append list100 nil)(function <))))

; << 8-4 >>

(defun bench84 (iter) (benchmark iter (sort (append list1000 nil)(function <))))

; << 8-5 >>

(defun bench85 (iter) (benchmark iter (sort(append list10000 nil)(function <))))

; << 8-6 >>

(defun bench86 (iter) (benchmark iter (sort (append list10 nil)(function >))))

; << 8-7 >>

(defun bench87 (iter) (benchmark iter (sort (append list100 nil)(function >))))

; << 8-8 >>

(defun bench88 (iter) (benchmark iter (sort (append list1000 nil)(function >))))

; << 8-9 >>

(defun bench89 (iter) (benchmark iter (sort(append list10000 nil)(function >))))

;
; [9] tpu
;

; *** tpu program ****
; theorem prover by unity binary resulution
;         [see]  c-l. chang  and  r.  c-t.  lee,
;                symbolic logic  and  mechanical theorem proving,
;                academic press, 1973.

(defconst xlist '(x1 x2 x3 x4 x5 x6 x7))
(defconst ylist '(y1 y2 y3 y4 y5 y6 y7))
(defconst zlist '(zz1 zz2 zz3 zz4 zz5 zz6 zz7))

(defun rename (c xy)
    (prog (var z)
      (setq z zlist)
      (setq var (cadr c))
 b1   (cond ((null var) (go b2)))
      (setq c (subst (car z) (car var) c))
      (setq z (cdr z))
      (setq var (cdr var))
      (go b1)
 b2   (setq z xy)
      (setq var (cadr c))
 b3   (cond ((null var) (return c)))
      (setq c (subst (car z) (car var) c))
      (setq z (cdr z))
      (setq var (cdr var))
      (go b3) ))

(defun inside (a e)
    (cond ((atom e) (eq a e))
          ((inside a (car e)) t)
          (t (inside a (cdr e))) ))

(defun disagree (e1 e2)
    (cond ((null e1) nil)
          ((or (atom e1) (atom e2))
           (cond ((equal e1 e2) nil) (t (list e1 e2))) )
          ((equal (car e1) (car e2)) (disagree (cdr e1) (cdr e2)))
          ((or (atom (car e1)) (atom (car e2))) (list (car e1) (car e2)))
          (t (disagree (car e1) (car e2))) ))

(defun unification (e1 e2)
    (prog (d u d1 d2)
      (cond
       ((not (equal (length e1) (length e2))) (return 'no)) )
 b1   (setq d (disagree e1 e2))
      (cond ((null d) (return (reverse u))))
      (setq d1 (car d))
      (setq d2 (cadr d))
      (cond ((or (member d1 xlist) (member d1 ylist)) (go b3)))
      (cond ((or (member d2 xlist) (member d2 ylist)) (go b4)))
 b2   (return 'no)
 b3   (cond ((inside d1 d2) (go b2)))
      (setq u (cons d u))
      (setq e1 (subst d2 d1 e1))
      (setq e2 (subst d2 d1 e2))
      (go b1)
 b4   (cond ((inside d2 d1) (go b2)))
      (setq u (cons (reverse d) u))
      (setq e1 (subst d1 d2 e1))
      (setq e2 (subst d1 d2 e2))
      (go b1) ))

(defun deletev (x y var)
    (prog (var1 tx tx1 x1)
      (setq x (append x y))
 b1   (cond ((null var) (return x)))
      (setq var1 (car var))
      (setq tx x)
      (setq x1 nil)
 b2   (cond ((null tx) (go b4)))
      (setq tx1 (car tx))
      (cond ((eq tx1 var1) (go b3)))
      (setq x1 (cons tx1 x1))
      (setq tx (cdr tx))
      (go b2)
 b3   (setq x (append x1 (cdr tx)))
 b4   (setq var (cdr var))
      (go b1) ))

(defun uresolve (c1 c2 n)
    (prog (l1 l2 vc1 vc2 x y sign unif r res var v1 v2 h hist tc2)
      (setq c1 (rename c1 xlist))
      (setq c2 (rename c2 ylist))
      (setq l1 (car c1))
      (setq l2 (car c2))
      (setq vc1 (cadr c1))
      (setq vc2 (cadr c2))
      (setq c2 (caddr c2))
      (setq x (car (caddr c1)))
      (setq sign -1)
      (cond ((eq (car x) 'not) (go b7)))
      (setq sign 1)
 b1   (cond
       ((null c2) (return (list (reverse res) (reverse hist) n))) )
      (setq y (car c2))
      (cond ((eq (car y) 'not) (go b2)))
      (go b6)
 b2   (setq unif (unification x (cdr y)))
 b3   (cond ((equal unif 'no) (go b6)))
      (setq r (append (reverse tc2) (cdr c2)))
      (cond ((null r) (return (list 'contradiction l1 l2))))
      (setq var nil)
 b4   (cond ((null unif) (go b5)))
      (setq v1 (caar unif))
      (setq v2 (cadar unif))
      (setq var (cons v1 var))
      (setq r (subst v2 v1 r))
      (setq unif (cdr unif))
      (go b4)
 b5   (setq n (add1 n))
      (setq h (list n l1 l2 (add1 (length tc2))))
      (setq r (list n (deletev vc1 vc2 var) r))
      (setq res (cons r res))
      (setq hist (cons h hist))
 b6   (setq tc2 (cons y tc2))
      (setq c2 (cdr c2))
      (cond ((equal sign 1) (go b1)))
 b7   (cond
       ((null c2) (return (list (reverse res) (reverse hist) n))) )
      (setq y (car c2))
      (cond ((eq (car y) 'not) (go b6)))
      (setq unif (unification (cdr x) y))
      (go b3) ))

(defun gunit (s1 s2 w c n)
    (prog (l s3 ss3 w1 v u res hist m x)
      (cond ((null w) (return (list res hist n))))
      (setq l (length (caddr c)))
      (setq s3 (list (list 10000 c)))
      (setq ss3 s3)
 b1   (cond ((null w) (go b7)))
      (setq w1 (car w))
 b2   (cond ((null ss3) (go b4)))
      (setq v (car ss3))
      (cond ((greaterp (car w1) (car v)) (go b3)))
      (setq u (uresolve w1 (cadr v) n))
      (cond ((null (car u)) (go b3)))
      (setq res (append res (car u)))
      (setq hist (append hist (cadr u)))
      (setq n (caddr u))
 b3   (setq ss3 (cdr ss3))
      (go b2)
 b4   (cond ((equal (sub1 l) 1) (go b6)))
      (setq m (car w1))
 b5   (cond ((null res) (go b6)))
      (setq x (cons (list m (car res)) x))
      (setq res (cdr res))
      (go b5)
 b6   (setq w (cdr w))
      (setq ss3 s3)
      (go b1)
 b7   (setq l (sub1 l))
      (cond ((equal l 1) (return (list res hist n))))
      (setq s3 x)
      (setq ss3 s3)
      (setq x nil)
      (setq w (append s1 s2))
      (go b1) ))

(defun pnsort (res)
    (prog (c pos neg)
 b1   (cond ((null res) (return (list (reverse pos) (reverse neg)))))
      (setq c (caar (cddar res)))
      (cond ((equal (car c) 'not) (go b3)))
      (setq pos (cons (car res) pos))
 b2   (setq res (cdr res))
      (go b1)
 b3   (setq neg (cons (car res) neg))
      (go b2) ))

(defun fdepth (c)
    (prog (n u)
      (setq c (car (caddr c)))
      (cond ((equal (car c) 'not) (go b1)))
      (setq c (cdr c))
      (go b2)
 b1   (setq c (cddr c))
 b2   (setq n 0)
 b3   (cond ((null c) (go b5)))
      (cond ((atom (car c)) (go b4)))
      (setq u (append (cdar c) u))
 b4   (setq c (cdr c))
      (go b3)
 b5   (cond ((null u) (return n)))
      (setq n (add1 n))
      (setq c u)
      (setq u nil)
      (go b3) ))

(defun ftest (res n4)
    (prog (c u)
 b1   (cond ((null res) (return (reverse u))))
      (setq c (car res))
      (cond ((greaterp (fdepth c) n4) (go b2)))
      (setq u (cons c u))
 b2   (setq res (cdr res))
      (go b1) ))

(defun subsume (c1 c2)
    (prog (z var u)
      (setq c1 (rename c1 xlist))
      (setq c1 (car (caddr c1)))
      (setq z zlist)
      (setq var (cadr c2))
      (setq c2 (car (caddr c2)))
 b1   (cond ((null var) (go b2)))
      (setq c2 (subst (car z) (car var) c2))
      (setq var (cdr var))
      (go b1)
 b2   (setq u (unification c1 c2))
      (cond ((equal u 'no) (return nil)))
      (return t) ))

(defun stest (u res)
    (prog (r v w x1 y z)
 b1   (cond ((null res) (go b5)))
      (setq r (car res))
      (setq z (append u v))
 b2   (cond ((null z) (go b3)))
      (cond ((subsume (car z) r) (go b4)))
      (setq z (cdr z))
      (go b2)
 b3   (setq v (cons r v))
 b4   (setq res (cdr res))
      (go b1)
 b5   (cond ((null v) (return w)))
      (setq x1 (car v))
      (setq z (cdr v))
 b6   (cond ((null z) (go b8)))
      (cond ((subsume x1 (car z)) (go b7)))
      (setq y (cons (car z) y))
 b7   (setq z (cdr z))
      (go b6)
 b8   (setq w (cons x1 w))
      (setq v (reverse y))
      (setq y nil)
      (go b5) ))

(defun contradict (u v)
    (prog (x1 y res)
 b1   (cond ((or (null u) (null v)) (return nil)))
      (setq x1 (car u))
      (setq y v)
 b2   (cond ((null y) (go b3)))
      (setq res (uresolve x1 (car y) -1))
      (cond ((equal (car res) 'contradiction) (return res)))
      (setq y (cdr y))
      (go b2)
 b3   (setq u (cdr u))
      (go b1) ))

(defun dtree (z hist n1)
    (prog (x tx x1 h m1 m2 m n)
      (setq hist (reverse hist))
      (setq x (cdr z))
      (setq z (list z))
      (cond ((greaterp (car x) (cadr x)) (go b0)))
      (setq x (reverse x))
 b0   (cond ((greaterp (cadr x) n1) (go b1)))
      (setq x (list (car x)))
 b1   (cond ((null x) (return z)))
      (setq x1 (car x))
 b2   (cond ((equal x1 (caar hist)) (go b3)))
      (setq hist (cdr hist))
      (go b2)
 b3   (setq x (cdr x))
      (setq h (car hist))
      (setq z (cons h z))
      (setq hist (cdr hist))
      (setq m1 (cadr h))
      (setq m2 (caddr h))
      (cond ((greaterp m1 n1) (go b5)))
 b4   (cond ((greaterp m2 n1) (go b6)))
      (go b1)
 b5   (setq n 1)
      (setq m m1)
      (go b7)
 b6   (setq n 2)
      (setq m m2)
 b7   (cond ((null x) (go b8)))
      (setq x1 (car x))
      (cond ((equal x1 m) (go b10)))
      (cond ((greaterp  x1 m) (go b9)))
 b8   (setq x (append (reverse tx) (cons m x)))
      (go b11)
 b9   (setq tx (cons x1 tx))
      (setq x (cdr x))
      (go b7)
 b10  (setq x (append (reverse tx) x))
 b11  (setq tx nil)
      (cond ((equal n 2) (go b1)))
      (go b4) ))

(defun tpu (s1 s2 s3 w n1 n2 n3 n4)
    (prog (s w1 ts u1 u n k ck wck v pos neg hist y x1 x)
      (setq s (append s1 s2))
      (setq s (reverse s))
 b1   (cond ((null w) (go b6)))
      (setq w1 (car w))
 b2   (setq ts s)
      (cond ((null w1) (go b5)))
 b3   (cond ((eq (car w1) (caar ts)) (go b4)))
      (setq ts (cdr ts))
      (go b3)
 b4   (setq u1 (cons (car ts) u1))
      (setq w1 (cdr w1))
      (go b2)
 b5   (setq u (cons u1 u))
      (setq w (cdr w))
      (setq u1 nil)
      (go b1)
 b6   (setq w (reverse u))
      (setq n n1)
      (setq u (contradict s1 s2))
      (cond ((not (null u)) (return u)))
      (setq k 1)
 b7   (cond ((greaterp k n2) (return '(s is not proved))))
      (setq ck (car s3))
      (setq wck (car w))
      (setq v (gunit s1 s2 wck ck n))
      (cond ((null (car v)) (go b12)))
      (setq n (caddr v))
      (setq hist (append hist (cadr v)))
      (setq v (car v))
      (cond ((lessp k n3) (go b8)))
      (setq v (ftest v n4))
 b8   (setq v (pnsort v))
      (setq pos (stest s1 (car v)))
      (setq neg (stest s2 (cadr v)))
      (cond ((null (append pos neg)) (go b12)))
      (setq u (contradict s1 neg))
      (cond ((not (null u)) (return (dtree u hist n1))))
      (setq u (contradict pos s2))
      (cond ((not (null u)) (return (dtree u hist n1))))
      (setq s1 (append s1 pos))
      (setq s2 (append s2 neg))
      (setq w (cdr w))
      (setq y (append pos neg))
 b9   (cond ((null w) (go b10)))
      (setq x1 (append y (car w)))
      (setq x (cons x1 x))
      (setq w (cdr w))
      (go b9)
 b10  (setq w (append (reverse x) (list y)))
      (setq x nil)
 b11  (setq s3 (append (cdr s3) (list ck)))
      (setq k (add1 k))
      (go b7)
 b12  (setq w (append (cdr w) (list nil)))
      (go b11) ))

; << 9-1 >>	tpu-1

(defun bench91 (iter) (benchmark iter (tpu1)))

(defun tpu1 ()
(tpu
  '((1 (x y) ((p (g x y) x y))) (2 (x y) ((p x (h x y) y))))
  '((3 (x) ((not p (k x) x (k x)))))
  '((4 (x y z u v w) ((not p x y u) (not p y z v) (not p x v w) (p u z w)))
    (5 (x y z u v w) ((not p x y u) (not p y z v) (not p u z w) (p x v w))) )
  '((3) nil)
  5 2 3 0 )
)

	; this form must return 
	; ((6 3 4 4) (11 2 6 2) (15 1 11 1) (contradiction 1 15)) .

; << 9-2 >>	tpu-2

(defun bench92 (iter) (benchmark iter (tpu2)))

(defun tpu2 ()
(tpu
  '((1 (x) ((p e x x)))
    (2 (x) ((p x e x)))
    (3 (x) ((p x x e)))
    (4 nil ((p a b c))) )
  '((5 nil ((not p b a c))))
  '((6 (x y z u v w) ((not p x y u) (not p y z v) (not p x v w) (p u z w)))
    (7 (x y z u v w) ((not p x y u) (not p y z v) (not p u z w) (p x v w))) )
  '((4) nil)
  7 4 5 0 )
)

	; this form must return
	; ((8 4 6 1) (21 3 8 1) (30 2 21 1) (32 30 7 2) (42 3 32 1) (55 1 42 1)
	;  (62 55 6 1) (112 5 62 3) (130 3 112 1) (contradiction 2 130)) .

; << 9-3 >>	tpu-3

(defun bench93 (iter) (benchmark iter (tpu3)))

(defun tpu3 ()
(tpu
  '((1 (x) ((p e x x))) (2 (x) ((p (i x) x e))))
  '((3 nil ((not p a e a))))
  '((4 (x y z u v w) ((not p x y u) (not p y z v) (not p x v w) (p u z w)))
    (5 (x y z u v w) ((not p x y u) (not p y z v) (not p u z w) (p x v w))) )
  '((3) (3))
  5 4 5 0 )
)

	; this form must return
	; ((13 3 5 4)(16 2 13 2)(17 1 16 2)(18 17 4 4)(23 2 18 3)(24 1 23 2)
	;  (30 24 5 4) (46 2 30 2) (56 2 46 1) (contradiction 1 56)) .

; << 9-4 >>	tpu-4

(defun bench94 (iter) (benchmark iter (tpu4)))

(defun tpu4 ()
(tpu
   '((1 (x) ((p e x x))) (2 (x) ((p (i x) x e))))
   '((3 (x) ((not p a x e))))
   '((4 (x y z u v w) ((not p x y u) (not p y z v) (not p x v w) (p u z w)))
     (5 (x y z u v w) ((not p x y u) (not p y z v) (not p u z w) (p x v w))) )
   '((3) (3))
   5 4 5 0 )
)

	; this form must return
	; ((6 3 4 4) (11 2 6 3) (12 1 11 2) (20 12 5 4) (42 2 20 2) (62 2 42 1)
	;  (contradiction 1 62)) .

; << 9-5 >>	tpu-5

(defun bench95 (iter) (benchmark iter (tpu5)))

(defun tpu5 ()
(tpu
   '((1 (x) ((p e x x)))
     (2 (x) ((p x w x)))
     (3 (x) ((p x (i x) e)))
     (4 (x) ((p (i x) x e)))
     (5 nil ((s a))) )
   '((6 nil ((not s e))))
   '((7 (x y z) ((not s x) (not s y) (not p x (i y) z) (s z)))
     (8 (x y z u v w) ((not p x y u) (not p y z v) (not p x v w) (p u z w)))
     (9 (x y z u v w) ((not p x y u) (not p y z v) (not p u z w) (p x v w))) )
   '((6) nil nil)
   9 4 5 0 )
)

	; this form must return
	; ((10 6 7 4) (14 5 10 2) (18 5 14 1) (condtradiction 3 18)) .

; << 9-6 >>	tpu-6

(defun bench96 (iter) (benchmark iter (tpu6)))

(defun tpu6 ()
(tpu
   '((1 (x) ((p e x x)))
     (2 (x) ((p x e x)))
     (3 (x) ((p x (i x) e)))
     (4 (x) ((p (i x) x e)))
     (5 nil ((s b))) )
   '((6 nil ((not s (i b)))))
   '((7 (x y z) ((not s x) (not s y) (not p x (i y) z) (s z)))
     (8 (x y z u v w) ((not p x y u) (not p y z v) (not p x v w) (p u z w)))
     (9 (x y z u v w) ((not p x y u) (not p y z v) (not p u z w) (p x v w))) )
   '((5 6) nil nil)
   9 4 5 0 )
)

	; this must return
	; ((11 5 7 1) (19 5 11 1) (23 3 19 1) (152 23 7 1) (169 6 152 3) 
	;  (186 5 169 1) (condtradiction 1 186)) .

; << 9-7 >>	tpu-7

(defun bench97 (iter) (benchmark iter (tpu7)))

(defun tpu7 ()
(tpu
   '((1 nil ((p a))) (2 nil ((m a (s c) (s b)))) (3 (x) ((m x x (s x)))))
   '((4 nil ((not d a b))))
   '((5 (x y z) ((not m x y z) (m y x z)))
     (6 (x y z) ((not m x y z) (d x z)))
     (7 (x y z u) ((not p x) (not m y z u) (not d x u) (d x y) (d x z))) )
   '((1 2 3 4) (1 2 3 4) (1 2 3 4))
   7 4 5 0 )
)

	; this must return
	; ((13 2 6 1) (16 13 7 3) (43 4 16 3) (66 4 43 3) (75 3 66 2)
	;  (contradiction 1 75)) .

; << 9-8 >>	tpu-8

(defun bench98 (iter) (benchmark iter (tpu8)))

(defun tpu8 ()
(tpu
   '((1 nil ((l 1 a))) (2 (x) ((d x x))))
   nil
   '((3 (x) ((p x) (d (g x) x)))
     (4 (x) ((p x) (l 1 (g x))))
     (5 (x) ((p x) (l (g x) x)))
     (6 (x) ((not p x) (not d x a)))
     (7 (x y z) ((not d x y) (not d y z) (d x z)))
     (8 (x) ((not l 1 x) (not l x a) (p (f x))))
     (9 (x) ((not l 1 x) (not l x a) (d (f x) x))) )
   '((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
   9 20 21 0 )
)

	; this must return
	; ((10 2 6 2)(15 10 3 1)(16 10 4 1)(17 10 5 1)(23 17 8 2)(25 16 23 1)
	;  (26 17 9 2) (28 16 26 1) (32 25 6 1) (33 32 7 3) (47 28 33 1)
	;  (contradiction 15 47)) .

; << 9-9 >>	tpu-9

(defun bench99 (iter) (benchmark iter (tpu9)))

(defun tpu9 ()
(tpu
   '((1 (x) ((l x (f x)))))
   '((2 (x) ((not l x x))))
   '((3 (x y) ((not l x y) (not l y x)))
     (4 (x y) ((not d x (f y)) (l y x)))
     (5 (x) ((p x) (d (h x) x)))
     (6 (x) ((p x) (p (h x))))
     (7 (x) ((p x) (l (h x) x)))
     (8 (x) ((not p x) (not l a x) (l (f a) x))) )
   '((1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
   8 20 21 0 )
)

	; this must return
	; ((14 2 8 3)(16 1 14 2)(17 16 5 1)(18 16 6 1)(19 16 7 1)(20 19 3 1)
	;  (23 17 4 1) (24 23 8 2) (28 20 24 2) (contradiction 18 28)) .

; [10] portable prolog

; portable-prolog originally written by h. nakashima
; transferred to ecl on   monday, 9 november 1981

; this program may be modified in order to speed up its performance.

(setq printlength 0 printlevel 0)

(declare (file old-subst cue new-subst undo-list *subst*)special)

(defun prolog ((file))
  (prog (result)
         (cond ((null file) (setq file standard-input))
               (t (setq file (inopen (stream file)))))
         (cond ((eq file terminal-input)
                (print '(portable prolog on lisp))
                (terpri)))
   loop (cond ((eq file standard-input) (print 'prl>)))
        (cond((eq (setq result (read-exec)) 'epilog)
              (return 'epilog))
             ((eq result 'syntax-error) nil)
             ((eq file standard-input) (print (cond (result result)
                                       (t '*failure*)))))
        (go loop)))

(defun read-exec ()
  (prog (sign)
    (setq sign (read file))
    (return (cond ((eq sign '/+)(define-clause (read-rest)))
                  ((eq sign '/-)(refute-clause (read-rest)))
                  ((memq sign '(end ok stop epilog)) 'epilog)
                  (t (read-error sign))))))

(defun read-rest ()
  (prog (form rest)
    (setq form (read file))
    (return (cond ((atom form)(read-error form))
                  ((eq (setq rest (read-sign)) 'syntax-error)
                   'syntax-error)
                  (t (cons form rest))))))

(defun read-sign ()
  (prog (sign)
    (setq sign (read file))
    (return (cond((eq sign '/-)(read-rest))
                 ((eq sign ':) nil)
                 (t (read-error sign))))))

(defun read-error (obj)
  (print (list 'syntax 'error obj)) 'syntax-error)

(defun define-clause (clause)
  (cond ((eq clause 'syntax-error) 'syntax-error)
        (t (prog (definition)
             (setq definition (get (caar clause) 'prolog))
             (cond ((null definition)
                    (putprop (caar clause)
                             (cons clause nil) 'prolog))
                   (t (nconc definition (cons clause nil))))
             (return 'defined)))))

(defun refute-clause (clause)
  (cond ((eq clause 'syntax-error) 'syntax-error)
        (t (refutes clause (cons nil nil) (cons nil nil) nil))))

(defun refutes (clause new-subst old-subst cue)
  (cond ((null clause)
        (cond ((null cue) '*proved*)
              (t (refutes (car (first cue)) (cons nil nil)
                          (cdr (first cue)) (cdr cue)))))
        (t (refute clause))))

(defun refute (clause)
  (prog (definition undo-list result)
    (setq definition (get (caar clause) 'prolog))
   refuteloop
    (or definition
        (return
         (cond ((and
                 (setq result (try-sys (car clause) old-subst))
                 (refutes (cdr clause) (cons nil nil) old-subst cue))
                result)
               (t (undo) nil))))
    (cond ((and (unify (car clause) old-subst
                       (caar definition) new-subst)
                (refutes (cdr (first definition)) (cons nil nil)
                         new-subst
                         (cons (cons (cdr clause) old-subst) cue)))
           (return '*proved*))
          (t (undo)))
    (setq definition (cdr definition))
    (go refuteloop)))

(defun undo()
  (prog()
   loop (cond ((null undo-list) (return nil))
              (t (rplacd (car undo-list) (cddar undo-list))
                 (setq undo-list (cdr undo-list))
                 (go loop)))))

(defun try-sys (form subst)
  (cond ((eq (car form) 'call)
         (apply (fetch-value (second form) subst)
                (mapcar (cddr form)
			(function(lambda (x) (fetch-value x subst)))))
         '*proved*)
        ((eq (car form) 'eval)
         (cond ((unify
                 (apply (fetch-value (car (second form)) subst)
                        (mapcar (cdr (second form))
				(function(lambda (x) (fetch-value x subst)))))
                 subst (third form) subst)
                '*proved*)
               (t nil)))
        ((eq (car form) 'end) 'epilog)
        ((eq (car form) 'delete)
         (mapcar (cdr form)
		 (function(lambda (x) (remprop (fetch-value x subst) 'prolog))))
         'deleted)
        (t nil)))

(defun unify (x x-subst y y-subst)
  (cond ((var? x)
         (cond ((assigned? x x-subst)
                (unify (fetch x x-subst) *subst* y y-subst))
               (t (link x x-subst y y-subst))))
        ((var? y) (unify y y-subst x x-subst))
        ((atom x) (eq x y))
        ((atom y) nil)
        ((unify (car x) x-subst (car y) y-subst)
         (unify (cdr x) x-subst (cdr y) y-subst))
        (t nil)))

(defun var? (x) (and (symbolp x) (eq (charact x) '*)))

(defun assigned? (x subst) (assoc x (cdr subst)))

(defun fetch (x subst)
  (setq *subst* subst)
  (cond ((var? x)
         (prog (v)
           (setq v (assoc x (cdr subst)))
           (cond ((null v) (return x)))
           (setq *subst* (cddr v))
           (return (fetch (second v) (cddr v)))))
        (t x)))

(defun fetch-value (x subst)
  (cond ((var? x)
         (prog (v)
           (setq v (assoc x (cdr subst)))
           (cond ((null v) (return x))
                 (t (return (fetch-value (second v) (cddr v)))))))
        ((atom x) x)
        (t (cons (fetch-value (car x) subst)
                 (fetch-value (cdr x) subst)))))

(defun link (x x-subst y y-subst)
  (setq undo-list
        (cons (rplacd x-subst
                      (cons (cons x (cons (fetch y y-subst)
                                          *subst*))
                            (cdr x-subst)))
              undo-list)))

;(defun character (x) (cond ((null x) nil)
;                           (t (car (explode x)))))

(defun charact (x) (cond ((null x)nil)
			   (t (getchar x 0))))

; how to use portable-prolog
;       (prolog {file-name})
;               (prolog)        input from terminal
;               (prolog {(directory)} file-name {extension})
;                       a file must be terminated by "stop".
;
; the syntax of portable-prolog
;       +(f1 {p1}...):                                  asserts a fact
;       +(f1 {p1}...)-(f2 {p2}...) ... -(fn {pn}...):   asserts a theorem 
;       -(f1 {p1}...) ... -(fn {pn}...):                        query
;       -(end):, stop, end, epilog, or end              end of prolog session
;
; convensions
;       *variable               represents a variable
;       -(call function parameter...)   apply a function to parameters
;                                       with a success of unification
;       -(eval form clause)     unifies the value of form with clause

; ---- now enter the portable-prolog ----

;
; [11] differentiation by data-driven programming
;

; main routine

(defun diff (exp x)
  (cond ((eq exp x) 1)
        ((atom exp) 0)
        ((atom (car exp))
         ((lambda (fn)
            (cond ((and fn (= (length exp) 3))
                   (apply fn (list (cdr exp) x)) )
                  (t 'break) ))
          (get (car exp) 'diff-fn) ))))

; define how to differentiate to each operator

(putprop '+ 'plus-diff 'diff-fn)
(putprop '- 'difference-diff 'diff-fn)
(putprop '* 'times-diff 'diff-fn)
(putprop '// 'quotient-diff 'diff-fn)
(putprop '** 'expt-diff 'diff-fn)

(defun plus-diff (sexp x)
  (simp-plus (diff (car sexp) x) (diff (cadr sexp) x)) )

(defun simp-plus (arg1 arg2)
  (cond ((and (numberp arg1) (numberp arg2))
         (plus arg1 arg2) )
        ((eq arg1 0) arg2)
        ((eq arg2 0) arg1)
        (t (list '+ arg1 arg2)) ))

(defun difference-diff (sexp x)
  (simp-difference (diff (car sexp) x) (diff (cadr sexp) x)) )

(defun simp-difference (arg1 arg2)
  (cond ((and (numberp arg1) (numberp arg2))
         (difference arg1 arg2) )
        ((eq arg1 0) (list '* arg2 -1))
        ((eq arg2 0) arg1)
        ((numberp arg2) (list '+ arg1 (times arg2 -1)))
        (t (list '- arg1 arg2)) ))

(defun times-diff (sexp x)
  (simp-plus (simp-times  (diff (car sexp) x) (cadr sexp))
             (simp-times (car sexp) (diff (cadr sexp) x)) ))

(defun simp-times (arg1 arg2)
  (cond ((and (numberp arg1) (numberp arg2))
         (times arg1 arg2) )
        ((or (eq arg1 0) (eq arg2 0)) 0)
        ((eq arg1 1) arg2)
        ((eq arg2 1) arg1)
        (t (list '* arg1 arg2)) ))

(defun quotient2 (sexp x)
  (diff (list '* (car sexp) (list '** (cadr sexp) -1)) x) )

(defun expt-diff (sexp x)
  (cond ((= (cadr sexp) 0) 0)
        ((= (cadr sexp) 1) (diff (car sexp) x))
        (t (diff (list '* (car sexp)
                       (simp-expt (car sexp) (sub1 (cadr sexp))) )
                 x ))))

(defun simp-expt (arg exp)
  (cond ((zerop exp) 1)
        ((= exp 1) arg)
        (t (list '** arg exp)) ))

; << 11-1 >>

(defun bench111 (iter)
  (benchmark iter
	     (diff (diff
		    '(+ (+ (+ (** x 3) (* 3 (** x 2))) (* 3 x)) 1) 'x) 'x)
	     ))

; this must return (+ (+ (+ x x) (+ (+ x x) (* x 2))) 6) .

; << 11-2 >> d(6) (x - 1)**6/dx
;      this must result in 720.

(defun bench112 (iter)
  (benchmark iter
	     (diff (diff (diff (diff (diff (diff
                          '(** (- x 1) 6) 'x) 'x) 'x) 'x) 'x) 'x)
	     ))

; [12] **** yet another theorem-prover ****
; rewrite rule base theorem-proving programs
;         written by bob boyer.

(declare (unify-subst temp-temp)special)

(defun add-lemma (term)
       (cond ((and (not (atom term))
                   (eq (car term)
                       (quote equal))
                   (not (atom (cadr term))))
              (putprop (car (cadr term))
                       (cons term (get (car (cadr term))
                                           (quote lemmas)))
                       (quote lemmas)))
             (t (print "quote add-lemma-did-not-like-term")
                (print term)(break))))
(defun add-lemma-lst (lst)
       (cond ((null lst)
              t)
             (t (add-lemma (car lst))
                (add-lemma-lst (cdr lst)))))
(defun apply-subst (alist term)
       (cond ((atom term)
              (cond ((setq temp-temp (assq term alist))
                     (cdr temp-temp))
                    (t term)))
             (t (cons (car term)
                      (apply-subst-lst alist (cdr term))))))
(defun apply-subst-lst (alist lst)
       (cond ((null lst)
              nil)
             (t (cons (apply-subst alist (car lst))
                      (apply-subst-lst alist (cdr lst))))))
(defun falsep (x lst)
       (or (equal x (quote (f)))
           (member x lst)))
(defun one-way-unify (term1 term2)
       (progn (setq unify-subst nil)
              (one-way-unify1 term1 term2)))
(defun one-way-unify1 (term1 term2)
       (cond ((atom term2)
              (cond ((setq temp-temp (assq term2 unify-subst))
                     (equal term1 (cdr temp-temp)))
                    (t (setq unify-subst (cons (cons term2 term1)
                                               unify-subst))
                       t)))
             ((atom term1)
              nil)
             ((eq (car term1)
                  (car term2))
              (one-way-unify1-lst (cdr term1)
                                  (cdr term2)))
             (t nil)))
(defun one-way-unify1-lst (lst1 lst2)
       (cond ((null lst1)
              t)
             ((one-way-unify1 (car lst1)
                              (car lst2))
              (one-way-unify1-lst (cdr lst1)
                                  (cdr lst2)))
             (t nil)))
(defun rewrite (term)
       (cond ((atom term)
              term)
             (t (rewrite-with-lemmas (cons (car term)
                                           (rewrite-args (cdr term)))
                                     (get (car term)
                                              (quote lemmas))))))
(defun rewrite-args (lst)
       (cond ((null lst)
              nil)
             (t (cons (rewrite (car lst))
                      (rewrite-args (cdr lst))))))
(defun rewrite-with-lemmas (term lst)
       (cond ((null lst)
              term)
             ((one-way-unify term (cadr (car lst)))
              (rewrite (apply-subst unify-subst (caddr (car lst)))))
             (t (rewrite-with-lemmas term (cdr lst)))))
(defun
  setup nil
  (add-lemma-lst
    (quote ((equal (compile form)
                   (reverse (codegen (optimize form)
                                     (nil))))
            (equal (eqp x y)
                   (equal (fix x)
                          (fix y)))
            (equal (greaterp x y)
                   (lessp y x))
            (equal (lesseqp x y)
                   (not (lessp y x)))
            (equal (greatereqp x y)
                   (not (lessp x y)))
            (equal (boolean x)
                   (or (equal x (t))
                       (equal x (f))))
            (equal (iff x y)
                   (and (implies x y)
                        (implies y x)))
            (equal (even1 x)
                   (if (zerop x)
                       (t)
                       (odd (sub1 x))))
            (equal (countps- l pred)
                   (countps-loop l pred (zero)))
            (equal (fact- i)
                   (fact-loop i 1))
            (equal (reverse- x)
                   (reverse-loop x (nil)))
            (equal (divides x y)
                   (zerop (remainder y x)))
            (equal (assume-true var alist)
                   (cons (cons var (t))
                         alist))
            (equal (assume-false var alist)
                   (cons (cons var (f))
                         alist))
            (equal (tautology-checker x)
                   (tautologyp (normalize x)
                               (nil)))
            (equal (falsify x)
                   (falsify1 (normalize x)
                             (nil)))
            (equal (prime x)
                   (and (not (zerop x))
                        (not (equal x (add1 (zero))))
                        (prime1 x (sub1 x))))
            (equal (and p q)
                   (if p (if q (t)
                             (f))
                       (f)))
            (equal (or p q)
                   (if p (t)
                       (if q (t)
                           (f))
                       (f)))
            (equal (not p)
                   (if p (f)
                       (t)))
            (equal (implies p q)
                   (if p (if q (t)
                             (f))
                       (t)))
            (equal (fix x)
                   (if (numberp x)
                       x
                       (zero)))
            (equal (if (if a b c)
                       d e)
                   (if a (if b d e)
                       (if c d e)))
            (equal (zerop x)
                   (or (equal x (zero))
                       (not (numberp x))))
            (equal (plus (plus x y)
                         z)
                   (plus x (plus y z)))
            (equal (equal (plus a b)
                          (zero))
                   (and (zerop a)
                        (zerop b)))
            (equal (difference x x)
                   (zero))
            (equal (equal (plus a b)
                          (plus a c))
                   (equal (fix b)
                          (fix c)))
            (equal (equal (zero)
                          (difference x y))
                   (not (lessp y x)))
            (equal (equal x (difference x y))
                   (and (numberp x)
                        (or (equal x (zero))
                            (zerop y))))
            (equal (meaning (plus-tree (append x y))
                            a)
                   (plus (meaning (plus-tree x)
                                  a)
                         (meaning (plus-tree y)
                                  a)))
            (equal (meaning (plus-tree (plus-fringe x))
                            a)
                   (fix (meaning x a)))
            (equal (append (append x y)
                           z)
                   (append x (append y z)))
            (equal (reverse (append a b))
                   (append (reverse b)
                           (reverse a)))
            (equal (times x (plus y z))
                   (plus (times x y)
                         (times x z)))
            (equal (times (times x y)
                          z)
                   (times x (times y z)))
            (equal (equal (times x y)
                          (zero))
                   (or (zerop x)
                       (zerop y)))
            (equal (exec (append x y)
                         pds envrn)
                   (exec y (exec x pds envrn)
                         envrn))
            (equal (mc-flatten x y)
                   (append (flatten x)
                           y))
            (equal (member x (append a b))
                   (or (member x a)
                       (member x b)))
            (equal (member x (reverse y))
                   (member x y))
            (equal (length (reverse x))
                   (length x))
            (equal (member a (intersect b c))
                   (and (member a b)
                        (member a c)))
            (equal (nth (zero)
                        i)
                   (zero))
            (equal (exp i (plus j k))
                   (times (exp i j)
                          (exp i k)))
            (equal (exp i (times j k))
                   (exp (exp i j)
                        k))
            (equal (reverse-loop x y)
                   (append (reverse x)
                           y))
            (equal (reverse-loop x (nil))
                   (reverse x))
            (equal (count-list z (sort-lp x y))
                   (plus (count-list z x)
                         (count-list z y)))
            (equal (equal (append a b)
                          (append a c))
                   (equal b c))
            (equal (plus (remainder x y)
                         (times y (quotient2 x y)))
                   (fix x))
            (equal (power-eval (big-plus1 l i base)
                               base)
                   (plus (power-eval l base)
                         i))
            (equal (power-eval (big-plus x y i base)
                               base)
                   (plus i (plus (power-eval x base)
                                 (power-eval y base))))
            (equal (remainder y 1)
                   (zero))
            (equal (lessp (remainder x y)
                          y)
                   (not (zerop y)))
            (equal (remainder x x)
                   (zero))
            (equal (lessp (quotient2 i j)
                          i)
                   (and (not (zerop i))
                        (or (zerop j)
                            (not (equal j 1)))))
            (equal (lessp (remainder x y)
                          x)
                   (and (not (zerop y))
                        (not (zerop x))
                        (not (lessp x y))))
            (equal (power-eval (power-rep i base)
                               base)
                   (fix i))
            (equal (power-eval (big-plus (power-rep i base)
                                         (power-rep j base)
                                         (zero)
                                         base)
                               base)
                   (plus i j))
            (equal (gcd x y)
                   (gcd y x))
            (equal (nth (append a b)
                        i)
                   (append (nth a i)
                           (nth b (difference i (length a)))))
            (equal (difference (plus x y)
                               x)
                   (fix y))
            (equal (difference (plus y x)
                               x)
                   (fix y))
            (equal (difference (plus x y)
                               (plus x z))
                   (difference y z))
            (equal (times x (difference c w))
                   (difference (times c x)
                               (times w x)))
            (equal (remainder (times x z)
                              z)
                   (zero))
            (equal (difference (plus b (plus a c))
                               a)
                   (plus b c))
            (equal (difference (add1 (plus y z))
                               z)
                   (add1 y))
            (equal (lessp (plus x y)
                          (plus x z))
                   (lessp y z))
            (equal (lessp (times x z)
                          (times y z))
                   (and (not (zerop z))
                        (lessp x y)))
            (equal (lessp y (plus x y))
                   (not (zerop x)))
            (equal (gcd (times x z)
                        (times y z))
                   (times z (gcd x y)))
            (equal (value (normalize x)
                          a)
                   (value x a))
            (equal (equal (flatten x)
                          (cons y (nil)))
                   (and (nlistp x)
                        (equal x y)))
            (equal (listp (gopher x))
                   (listp x))
            (equal (samefringe x y)
                   (equal (flatten x)
                          (flatten y)))
            (equal (equal (greatest-factor x y)
                          (zero))
                   (and (or (zerop y)
                            (equal y 1))
                        (equal x (zero))))
            (equal (equal (greatest-factor x y)
                          1)
                   (equal x 1))
            (equal (numberp (greatest-factor x y))
                   (not (and (or (zerop y)
                                 (equal y 1))
                             (not (numberp x)))))
            (equal (times-list (append x y))
                   (times (times-list x)
                          (times-list y)))
            (equal (prime-list (append x y))
                   (and (prime-list x)
                        (prime-list y)))
            (equal (equal z (times w z))
                   (and (numberp z)
                        (or (equal z (zero))
                            (equal w 1))))
            (equal (greatereqpr x y)
                   (not (lessp x y)))
            (equal (equal x (times x y))
                   (or (equal x (zero))
                       (and (numberp x)
                            (equal y 1))))
            (equal (remainder (times y x)
                              y)
                   (zero))
            (equal (equal (times a b)
                          1)
                   (and (not (equal a (zero)))
                        (not (equal b (zero)))
                        (numberp a)
                        (numberp b)
                        (equal (sub1 a)
                               (zero))
                        (equal (sub1 b)
                               (zero))))
            (equal (lessp (length (delete x l))
                          (length l))
                   (member x l))
            (equal (sort2 (delete x l))
                   (delete x (sort2 l)))
            (equal (dsort x)
                   (sort2 x))
            (equal (length (cons x1
                                 (cons x2
                                       (cons x3 (cons x4
                                                      (cons x5
                                                            (cons x6 x7)))))))
                   (plus 6 (length x7)))
            (equal (difference (add1 (add1 x))
                               2)
                   (fix x))
            (equal (quotient2 (plus x (plus x y))
                             2)
                   (plus x (quotient2 y 2)))
            (equal (sigma (zero)
                          i)
                   (quotient2 (times i (add1 i))
                             2))
            (equal (plus x (add1 y))
                   (if (numberp y)
                       (add1 (plus x y))
                       (add1 x)))
            (equal (equal (difference x y)
                          (difference z y))
                   (if (lessp x y)
                       (not (lessp y z))
                       (if (lessp z y)
                           (not (lessp y x))
                           (equal (fix x)
                                  (fix z)))))
            (equal (meaning (plus-tree (delete x y))
                            a)
                   (if (member x y)
                       (difference (meaning (plus-tree y)
                                            a)
                                   (meaning x a))
                       (meaning (plus-tree y)
                                a)))
            (equal (times x (add1 y))
                   (if (numberp y)
                       (plus x (times x y))
                       (fix x)))
            (equal (nth (nil)
                        i)
                   (if (zerop i)
                       (nil)
                       (zero)))
            (equal (last (append a b))
                   (if (listp b)
                       (last b)
                       (if (listp a)
                           (cons (car (last a))
                                 b)
                           b)))
            (equal (equal (lessp x y)
                          z)
                   (if (lessp x y)
                       (equal t z)
                       (equal f z)))
            (equal (assignment x (append a b))
                   (if (assignedp x a)
                       (assignment x a)
                       (assignment x b)))
            (equal (car (gopher x))
                   (if (listp x)
                       (car (flatten x))
                       (zero)))
            (equal (flatten (cdr (gopher x)))
                   (if (listp x)
                       (cdr (flatten x))
                       (cons (zero)
                             (nil))))
            (equal (quotient2 (times y x)
                             y)
                   (if (zerop y)
                       (zero)
                       (fix x)))
            (equal (get j (set i val mem))
                   (if (eqp j i)
                       val
                       (get j mem)))))))
(defun tautologyp (x true-lst false-lst)
       (cond ((truep x true-lst)
              t)
             ((falsep x false-lst)
              nil)
             ((atom x)
              nil)
             ((eq (car x)
                  (quote if))
              (cond ((truep (cadr x)
                            true-lst)
                     (tautologyp (caddr x)
                                 true-lst false-lst))
                    ((falsep (cadr x)
                             false-lst)
                     (tautologyp (cadddr x)
                                 true-lst false-lst))
                    (t (and (tautologyp (caddr x)
                                        (cons (cadr x)
                                              true-lst)
                                        false-lst)
                            (tautologyp (cadddr x)
                                        true-lst
                                        (cons (cadr x)
                                              false-lst))))))
             (t nil)))
(defun tautp (x)
       (tautologyp (rewrite x)
                   nil nil))
(defun test nil
       (prog (ans term)
             (setq term
                   (apply-subst
                     (quote ((x f (plus (plus a b)
                                        (plus c (zero))))
                             (y f (times (times a b)
                                         (plus c d)))
                             (z f (reverse (append (append a b)
                                                   (nil))))
                             (u equal (plus a b)
                                (difference x y))
                             (w lessp (remainder a b)
                                (member a (length b)))))
                     (quote (implies (and (implies x y)
                                          (and (implies y z)
                                               (and (implies z u)
                                                    (implies u w))))
                                     (implies x w)))))
             (setq ans (tautp term)) ))

(defun trans-of-implies (n)
       (list (quote implies)
             (trans-of-implies1 n)
             (list (quote implies)
                   0 n)))
(defun trans-of-implies1 (n)
       (cond ((equal n 1)
              (list (quote implies)
                    0 1))
             (t (list (quote and)
                      (list (quote implies)
                            (sub1 n)
                            n)
                      (trans-of-implies1 (sub1 n))))))
(defun truep (x lst)
       (or (equal x (quote (t)))
           (member x lst)))

; initialization

(setup)

; confirm that (test) must return t.

; << 12-1 >> measure the following execution time.

(defun bench121 (iter) (benchmark iter (test)))

(defun benchall ()
	(lets	((prompt "output file name:")
		 (standard-output (outopen(stream(string(read))))))
		(mapc bnchtbl 
			(function(lambda (x)(princ "[[ ")(princ(car x))
					    (princ " ]]")(terpri)(gc)
					    (eval (cadr x)))))
		(quit)))
