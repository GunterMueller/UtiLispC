(defun fact (n) (cond ((0= n) 1)
		      (t (times n (fact (1- n))))))
(setq f1 (fact 1000))
(setq f9 (fact 900))
(setq f19 (fact 19))
