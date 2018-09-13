(defun num2str (n)
  (cond ((integerp n) (int-image n)) (t (flo-image n))))

(defun int-image (n)
  (lets ((result "")
	 (sign (cond ((minusp n) (setq n (minus n)) "-")
		     (t ""))))
	(loop (setq result
		    (string-append
		     (string (+ (character "0")
				(remainder n 10)))
		     result))
	      (and (zerop (setq n (quotient n 10)))
		   (exit (string-append sign result))))))

(defun flo-image (n)
  (lets ((sign (cond ((minusp n) (setq n (minus n)) "-") (t ""))))
	(string-append
	 sign
	 (int-image (fix n))
	 "."
	 (string-nreverse
	  (cut0
	   (string-amend
	    (make-string digits (character "0"))
	    (string-nreverse
	     (int-image
	      (fix (plus (times (difference n (fix n))
				(expt 10 digits))
			 0.5))))))))))

(defun cut0 (s)
  (lets ((from (string-search-not-char (character "0") s)))
	(cond ((null from) "0")
	      (t (substring s from)))))
