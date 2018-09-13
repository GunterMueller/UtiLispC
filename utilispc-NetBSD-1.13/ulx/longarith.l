;;; 32bit integer arithmetic routines.
;;;
;;;

;;; 32bit value = (cons *higher-16bit* *lower-16bit*)

(defun plus32 (a b)
  (lets ((l (+ (cdr a) (cdr b)))
	 (h (+ (car a) (car b))))
    (declare (l h) fix-value)
    (cons (logand (+ h (logshift l -16)) 65535) (logand l 65535))))

(defun inc32 (a)
  (lets ((l (1+ (cdr a)))
	 (h (cond ((= l 65536) (1+ (car a)))
		  (t (car a)))))
    (cons h (logand l 65535))))

(defun dec32 (a)
  (lets ((l (1- (cdr a)))
	 (h (cond ((= l -1) (1- (car a)))
		  (t (car a)))))
    (cons (logand h 65535) (logand l 65535))))

;;; mochiron 28bit made sika dame.
(defun fix32 (a)
  (cond ((fix32p a) a)
	((bigp a) (cons (fix (quotient a 65536)) (remainder a 65536)))
	(t (cons (logshift a -16) (logand a 65535)))))

(defun fix32p (a)
  (and (consp a) (fixp (car a)) (fixp (cdr a))))

(defun card32->big (a)
  (plus (times (car a) 65536) (cdr a)))

(defun logor32 (a b)
  (cons (logor (car a) (car b))
	(logor (cdr a) (cdr b))))

(defun logand32 (a b)
  (cons (logand (car a) (car b))
	(logand (cdr a) (cdr b))))

(defun logxor32 (a b)
  (cons (logxor (car a) (car b))
	(logxor (cdr a) (cdr b))))

(defun lognot32 (a)
  (logxor32 a '(65535 . 65535)))

(defun logshift32 (a count)
  (cond ((0= count) a)
	((0> count)
	 (let ((l (logor (logshift (cdr a) count)
			 (logshift (car a) (+ 16 count))))
	       (h (logshift (car a) count)))
	   (cons h (logand l 65535))))
	(t
	 (let ((h (logor (logshift (car a) count)
			 (logshift (cdr a) (- count 16))))
	       (l (logshift (cdr a) count)))
	   (cons (logand h 65535) (logand l 65535))))))

;;; (equal x `(0 . 0)) demo iikamo sirenai (error check ha dekinaku naruga).
(defun zerop32 (x) (and (zerop (car x))(zerop (cdr x))))

(defun oddp32 (x) (oddp (cdr x)))
;; iikagen
(defun plusp32 (x) (not (or (0= (car x)) (0= (cdr x)))))

(defun logbitp32 (pos val)
  (declare (pos) fix-value)
  (let ((p (or (and (> pos 15) (car val)) (cdr val)))
	(n (or (and (> pos 15) (- pos 16)) pos)))
    (declare (p n) fix-value)
    (not (0= (logand (logshift 1 n) p)))))

;;; mask = (high . low)
(defun byte32 (mask first) (cons mask first))

;;; logshift32 is expensive.
;;; note: common lisp no dpb to kanzen ni onaji deha nai!!
;;; int no newbyte wo umeru bubun ha 0 dearu kotowo katei site iru.
;;; chanto sitamononi suruniha int -> (logand32 (lognot32 (car bytespec)) int).
;;; to sinakereba naranaiga, douse hituyou nai.
(defun dpb32 (newbyte bytespec int)
  (logor32 (cond ((0= (cdr bytespec)) (logand32 (car bytespec) newbyte))
		  (t (logshift32 (logand32 (car bytespec) newbyte) (cdr bytespec))))
	    int))

