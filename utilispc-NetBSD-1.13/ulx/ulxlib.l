;;; library functions for developping ulx
;;;

(setq *inhibit-appending* t)

(defun print-vector (x)
  (princ "#(")
  (mapvector x (function (lambda (x) (princ x)(princ " "))))
  (princ ")")
  (terpri))

(defun print-resource-id (x)
  (princ
   (string-append
    "0x"
    (substring (hex-image (car x)) 2)
    (substring (hex-image (cdr x)) 2))))

(defun pr (x (tarse nil))
  (cond ((struct-p x) (print-struct x tarse))
	((vectorp x) (print-vector x))
	((fix32p x) (print-resource-id x)(terpri))
	(t (print x))))

(defun get-display-host ()
  (lets ((env (getenv "DISPLAY"))
	 (colon (string-search-char (character ":") env))
	 (host (or (and colon (substring env 0 colon)) env))
	 (disp nil)
	 (screen nil))
    host))

(defun pause (x) (print x)(catch x (break)))

(defun mask-string (s)
  (lets ((len (string-length s))
	 (r (make-string len)))
    (do ((i 0 (1+ i)))
	((= i len) r)
	(sset r i (logand 127 (sref s i))))))

(macro null-mask (l) '(make-string 4 0))
(defmacro set-mask (v pos) `(bset ,v (- 31 ,pos) t))
(defmacro clear-mask (v pos) `(bset ,v (- 31 ,pos) nil))
(defmacro check-mask (v pos) `(bref ,v (- 31 ,pos)))
(defmacro copy-mask (v) `(substring ,v 0))
