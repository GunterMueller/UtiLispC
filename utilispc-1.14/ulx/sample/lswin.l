
(defun xlswins ((display))
  (unless display
    (setq display (open-display (get-display-host))))
  (let ((root (screen-root (first (display-roots display)))))
    (if display
	(lswins root))))

(defun lswins (window (indent 0))
  (let ((childs (first (query-tree window))))
    (princ (make-string (+ indent indent) #x20))
    (print-resource-id (window-id window))
    (terpri)
    (if childs
	(do ((w childs (cdr w)))
	    ((atom w))
	    (lswins (car w) (1+ indent))))))